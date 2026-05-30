module Engine exposing
    ( processNextEvent
    , advanceUntil
    , drainAll
    , drainBounded
    )

import Dict
import Event exposing (Event, EventType(..), event)
import EventTime exposing (EventTime(..), addTimes, compareTimes)
import Id exposing (JobID(..), LockID(..), NodeID(..), QueueID(..))
import Job exposing (Job, Priority(..), priorityRank, newJob)
import Lock exposing (AcquireResult(..))
import Node exposing (DispatchRule(..), NodeData, NodeKind(..), NodeState(..))
import Queue exposing (EnqueueResult(..))
import Random
import ServiceTime
import SimState exposing (SimState)
import Topology exposing (Topology)


-- ── Public API ────────────────────────────────────────────────────────────────

processNextEvent : Topology -> SimState -> SimState
processNextEvent topo state =
    case SimState.popNextEvent state of
        Nothing ->
            state

        Just ( evt, state1 ) ->
            handleEvent topo evt (SimState.logEvent evt state1)


advanceUntil : EventTime -> Topology -> SimState -> SimState
advanceUntil deadline topo state =
    case state.eventQueue of
        [] ->
            state

        next :: _ ->
            if compareTimes next.time deadline == GT then
                state
            else
                advanceUntil deadline topo (processNextEvent topo state)


-- Drain at most maxEvents events (prevents infinite loops when Source nodes
-- keep scheduling new arrivals).
drainAll : Topology -> SimState -> SimState
drainAll =
    drainBounded 100000


drainBounded : Int -> Topology -> SimState -> SimState
drainBounded maxEvents topo state =
    if maxEvents <= 0 then
        state
    else
        case state.eventQueue of
            [] ->
                state
            _ ->
                drainBounded (maxEvents - 1) topo (processNextEvent topo state)


-- ── Dispatch ──────────────────────────────────────────────────────────────────

handleEvent : Topology -> Event -> SimState -> SimState
handleEvent topo evt state =
    case evt.kind of
        JobArrived nid jid ->
            onJobArrived topo nid jid state

        ServiceComplete nid jid ->
            onServiceComplete topo nid jid state

        SignoffComplete nid lid jid ->
            onSignoffComplete topo nid lid jid state

        InterruptStarted ->
            onInterruptStarted topo state

        InterruptEnded ->
            onInterruptResumed topo state

        -- All other event types are purely informational (already logged)
        _ ->
            state


-- ── Interrupt: halt / resume workers ─────────────────────────────────────────

-- Set halted=True on every Worker; log a WorkerHalted event for each.
onInterruptStarted : Topology -> SimState -> SimState
onInterruptStarted _ state =
    Dict.foldl
        (\nid node acc ->
            case node.kind of
                Worker cfg ->
                    SimState.putNode (NodeID nid) { node | kind = Worker { cfg | halted = True } } acc
                        |> SimState.logEvent (event state.clock (WorkerHalted (NodeID nid)))

                _ ->
                    acc
        )
        state
        state.nodes


-- Clear halted=False on every Worker; log WorkerResumed; wake idle ones.
onInterruptResumed : Topology -> SimState -> SimState
onInterruptResumed topo state =
    Dict.foldl
        (\nid node acc ->
            case node.kind of
                Worker cfg ->
                    let
                        resumedNode = { node | kind = Worker { cfg | halted = False } }
                        acc1 =
                            SimState.putNode (NodeID nid) resumedNode acc
                                |> SimState.logEvent (event state.clock (WorkerResumed (NodeID nid)))
                    in
                    if node.state == Idle then
                        case Topology.nodeInput (NodeID nid) topo of
                            Just qid ->
                                tryPullFromQueue topo (NodeID nid) resumedNode qid acc1
                            Nothing ->
                                acc1
                    else
                        acc1

                _ ->
                    acc
        )
        state
        state.nodes


-- ── Source: new job arrives ───────────────────────────────────────────────────

onJobArrived : Topology -> NodeID -> JobID -> SimState -> SimState
onJobArrived topo nid jid state =
    case SimState.getNode nid state of
        Just { kind } ->
            case kind of
                Source cfg ->
                    let
                        -- Sample priority: fraction of arrivals get High priority
                        ( roll, seed0 ) =
                            Random.step (Random.float 0 1) state.seed

                        priority =
                            if roll < cfg.highPriorityFraction then High else cfg.jobPriority

                        rawJob =
                            newJob jid priority 1.0 cfg.jobLabel state.clock

                        job =
                            { rawJob | history = [ ( state.clock, JobArrived nid jid ) ] }

                        -- schedule next arrival
                        ( gap, seed1 ) =
                            sampleExp cfg.arrivalRate seed0

                        ( nextJid, state1 ) =
                            SimState.nextJobID { state | seed = seed1 }

                        state2 =
                            state1
                                |> SimState.putJob job
                                |> SimState.scheduleEvent
                                    (event (addTimes state.clock (EventTime gap))
                                        (JobArrived nid nextJid)
                                    )
                    in
                    pushToOutputs topo nid job state2

                _ ->
                    state

        Nothing ->
            state


-- ── Worker: service done ──────────────────────────────────────────────────────

onServiceComplete : Topology -> NodeID -> JobID -> SimState -> SimState
onServiceComplete topo nid jid state =
    case SimState.getNode nid state of
        Just ({ kind } as node) ->
            case kind of
                Worker cfg ->
                    -- Ignore stale completions from jobs that were preempted
                    case node.state of
                        Busy currentJid _ ->
                            if currentJid /= jid then
                                state
                            else
                                case cfg.signoff of
                                    Just lid ->
                                        startSignoffRequest nid lid jid node state

                                    Nothing ->
                                        releaseJobToOutputs topo nid jid node state

                        _ ->
                            state

                _ ->
                    state

        Nothing ->
            state


startSignoffRequest : NodeID -> LockID -> JobID -> NodeData -> SimState -> SimState
startSignoffRequest nid lid jid node state =
    case SimState.getLock lid state of
        Nothing ->
            state

        Just lock ->
            let
                state0 =
                    case SimState.getJob jid state of
                        Just job ->
                            SimState.putJob
                                { job | history = ( state.clock, SignoffRequested nid lid jid ) :: job.history }
                                state
                        Nothing ->
                            state

                updatedNode =
                    { node | state = Signoff jid lid }

                state1 =
                    SimState.putNode nid updatedNode state0
                        |> SimState.logEvent (event state.clock (SignoffRequested nid lid jid))
            in
            case Lock.acquire nid jid lock of
                Acquired lock1 ->
                    beginSignoff nid lid jid lock1 state1

                Queued lock1 ->
                    SimState.putLock lid lock1 state1


beginSignoff : NodeID -> LockID -> JobID -> Lock.LockState -> SimState -> SimState
beginSignoff nid lid jid lock state =
    let
        ( duration, seed1 ) =
            ServiceTime.sample lock.config.serviceTime 1.0 state.seed

        completionTime =
            addTimes state.clock (EventTime duration)
    in
    { state | seed = seed1 }
        |> SimState.putLock lid lock
        |> SimState.logEvent (event state.clock (SignoffStarted nid lid jid))
        |> SimState.scheduleEvent (event completionTime (SignoffComplete nid lid jid))


-- ── Sign-off complete ─────────────────────────────────────────────────────────

onSignoffComplete : Topology -> NodeID -> LockID -> JobID -> SimState -> SimState
onSignoffComplete topo nid lid jid state =
    case ( SimState.getNode nid state, SimState.getLock lid state ) of
        ( Just node, Just lock ) ->
            let
                ( lock1, maybeWaiter ) =
                    Lock.release lock

                state1 =
                    SimState.putLock lid lock1 state

                state2 =
                    case maybeWaiter of
                        Nothing ->
                            state1

                        Just ( waiterNid, waiterJid ) ->
                            beginSignoff waiterNid lid waiterJid lock1 state1

                state3 =
                    case SimState.getJob jid state2 of
                        Just job ->
                            SimState.putJob
                                { job | history = ( state.clock, SignoffComplete nid lid jid ) :: job.history }
                                state2
                        Nothing ->
                            state2
            in
            releaseJobToOutputs topo nid jid node state3

        _ ->
            state


-- ── Shared: push job to outputs, then go idle ─────────────────────────────────

releaseJobToOutputs : Topology -> NodeID -> JobID -> NodeData -> SimState -> SimState
releaseJobToOutputs topo nid jid node state =
    case SimState.getJob jid state of
        Nothing ->
            state

        Just job ->
            let
                stamped =
                    { job | history = ( state.clock, ServiceComplete nid jid ) :: job.history }
            in
            SimState.putJob stamped state
                |> pushToOutputs topo nid stamped
                |> becomeIdle topo nid node


pushToOutputs : Topology -> NodeID -> Job -> SimState -> SimState
pushToOutputs topo nid job state =
    tryQueues topo job (Topology.nodeOutputs nid topo) state


tryQueues : Topology -> Job -> List QueueID -> SimState -> SimState
tryQueues topo job qids state =
    case qids of
        [] ->
            state

        qid :: rest ->
            case SimState.getQueue qid state of
                Nothing ->
                    tryQueues topo job rest state

                Just queue ->
                    case Queue.enqueue job queue of
                        Enqueued queue1 ->
                            state
                                |> SimState.putQueue qid queue1
                                |> SimState.logEvent (event state.clock (JobEnqueued qid job.id))
                                |> wakeConsumers topo qid

                        WasBlocked ->
                            tryQueues topo job rest state

                        DroppedExisting droppedID queue1 ->
                            state
                                |> SimState.putQueue qid queue1
                                |> SimState.logEvent (event state.clock (JobDropped qid droppedID))
                                |> SimState.logEvent (event state.clock (JobEnqueued qid job.id))
                                |> wakeConsumers topo qid

                        DroppedIncoming ->
                            state
                                |> SimState.logEvent (event state.clock (JobDropped qid job.id))


-- After a job enters a queue, wake consumers:
--   • Idle Workers pull immediately (unless interrupted).
--   • Busy preemptive Workers may be displaced by a higher-priority arrival.
--   • Non-Worker nodes (Dispatcher, Sink) always pull when idle.
wakeConsumers : Topology -> QueueID -> SimState -> SimState
wakeConsumers topo qid state =
    List.foldl
        (\nid acc ->
            case SimState.getNode nid acc of
                Just node ->
                    case ( node.state, node.kind ) of
                        ( Idle, Worker cfg ) ->
                            if cfg.halted then
                                acc
                            else
                                tryPullFromQueue topo nid node qid acc

                        ( Busy busyJid _, Worker cfg ) ->
                            if cfg.preemptive && not cfg.halted then
                                tryPreempt topo nid node busyJid qid acc
                            else
                                acc

                        ( Idle, _ ) ->
                            tryPullFromQueue topo nid node qid acc

                        _ ->
                            acc

                Nothing ->
                    acc
        )
        state
        (Topology.queueConsumers qid topo)


-- Attempt to preempt a busy worker if the head of qid has strictly higher priority.
-- On preemption: dequeue the incoming job, re-enqueue the displaced job, start service.
tryPreempt : Topology -> NodeID -> NodeData -> JobID -> QueueID -> SimState -> SimState
tryPreempt topo nid workerNode busyJid qid state =
    case ( SimState.getJob busyJid state, SimState.getQueue qid state ) of
        ( Just busyJob, Just queue ) ->
            case Queue.peek queue of
                Nothing ->
                    state

                Just incomingJob ->
                    if priorityRank incomingJob.priority > priorityRank busyJob.priority then
                        case Queue.dequeue queue of
                            Nothing ->
                                state

                            Just ( dequeued, queue1 ) ->
                                let
                                    state1 =
                                        state
                                            |> SimState.putQueue qid queue1
                                            |> SimState.logEvent (event state.clock (JobDequeued qid nid dequeued.id))
                                            |> SimState.logEvent (event state.clock (ServicePreempted nid busyJid))

                                    -- Put the displaced job back in the queue
                                    state2 =
                                        case SimState.getQueue qid state1 of
                                            Just q ->
                                                case Queue.enqueue busyJob q of
                                                    Enqueued q1 ->
                                                        state1
                                                            |> SimState.putQueue qid q1
                                                            |> SimState.logEvent (event state1.clock (JobEnqueued qid busyJob.id))

                                                    _ ->
                                                        state1

                                            Nothing ->
                                                state1
                                in
                                startService topo nid { workerNode | state = Idle } dequeued state2
                    else
                        state

        _ ->
            state


becomeIdle : Topology -> NodeID -> NodeData -> SimState -> SimState
becomeIdle topo nid node state =
    let
        idleNode =
            { node | state = Idle }

        state1 =
            SimState.putNode nid idleNode state
    in
    case node.kind of
        Worker cfg ->
            -- Workers wait out an active interrupt before pulling new work
            if cfg.halted then
                state1
            else
                case Topology.nodeInput nid topo of
                    Just qid ->
                        tryPullFromQueue topo nid idleNode qid state1

                    Nothing ->
                        state1

        _ ->
            case Topology.nodeInput nid topo of
                Just qid ->
                    tryPullFromQueue topo nid idleNode qid state1

                Nothing ->
                    state1


tryPullFromQueue : Topology -> NodeID -> NodeData -> QueueID -> SimState -> SimState
tryPullFromQueue topo nid node qid state =
    case SimState.getQueue qid state of
        Nothing ->
            state

        Just queue ->
            case Queue.dequeue queue of
                Nothing ->
                    state

                Just ( queueCopy, queue1 ) ->
                    -- Use the store copy so history stamped since enqueue is included
                    let
                        job =
                            SimState.getJob queueCopy.id state |> Maybe.withDefault queueCopy
                    in
                    SimState.putQueue qid queue1 state
                        |> SimState.logEvent (event state.clock (JobDequeued qid nid job.id))
                        |> startService topo nid { node | state = Idle } job


startService : Topology -> NodeID -> NodeData -> Job -> SimState -> SimState
startService topo nid node job state =
    case node.kind of
        Worker cfg ->
            let
                stamped =
                    { job | history = ( state.clock, ServiceStarted nid job.id ) :: job.history }

                ( duration, seed1 ) =
                    ServiceTime.sample cfg.serviceTime stamped.size state.seed

                completionTime =
                    addTimes state.clock (EventTime duration)

                busyNode =
                    { node | state = Busy stamped.id completionTime }
            in
            { state | seed = seed1 }
                |> SimState.putJob stamped
                |> SimState.putNode nid busyNode
                |> SimState.logEvent (event state.clock (ServiceStarted nid stamped.id))
                |> SimState.scheduleEvent (event completionTime (ServiceComplete nid stamped.id))

        Dispatcher cfg ->
            -- Route job, persist updated config (RoundRobin index), then pull next
            let
                outputs =
                    Topology.nodeOutputs nid topo

                byLength qid =
                    SimState.getQueue qid state |> Maybe.map Queue.size |> Maybe.withDefault 0

                ( orderedQueues, updatedCfg, seed1 ) =
                    case cfg.rule of
                        ShortestQueue ->
                            ( List.sortBy byLength outputs, cfg, state.seed )

                        RoundRobin ->
                            let
                                n   = max 1 (List.length outputs)
                                idx = modBy n cfg.roundRobinIndex
                            in
                            ( List.drop idx outputs ++ List.take idx outputs
                            , { cfg | roundRobinIndex = cfg.roundRobinIndex + 1 }
                            , state.seed
                            )

                        RandomChoice ->
                            let
                                n                = max 1 (List.length outputs)
                                ( idx, newSeed ) = Random.step (Random.int 0 (n - 1)) state.seed
                            in
                            ( List.drop idx outputs ++ List.take idx outputs
                            , cfg
                            , newSeed
                            )

                state1 =
                    tryQueues topo job orderedQueues { state | seed = seed1 }

                idleNode =
                    { node | kind = Dispatcher updatedCfg, state = Idle }

                state2 =
                    SimState.putNode nid idleNode state1
            in
            case Topology.nodeInput nid topo of
                Just inputQid ->
                    tryPullFromQueue topo nid idleNode inputQid state2

                Nothing ->
                    state2

        Sink ->
            let
                completed =
                    { job | history = ( state.clock, JobArrived nid job.id ) :: job.history }
            in
            state
                |> SimState.completeJob completed
                |> SimState.logEvent (event state.clock (JobArrived nid job.id))

        _ ->
            state


-- ── Utility ───────────────────────────────────────────────────────────────────

-- Exponential inter-arrival sampling for Source nodes (Poisson process).
-- Service time sampling is handled by ServiceTime.sample.
sampleExp : Float -> Random.Seed -> ( Int, Random.Seed )
sampleExp rate seed =
    let
        ( u, seed1 ) =
            Random.step (Random.float 0.001 1.0) seed

        ticks =
            max 1 (round (negate (logBase e u) / max 0.001 rate))
    in
    ( ticks, seed1 )
