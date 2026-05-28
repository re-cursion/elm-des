module Engine exposing
    ( processNextEvent
    , advanceUntil
    , drainAll
    )

import Event exposing (Event, EventType(..), event)
import EventTime exposing (EventTime(..), addTimes, compareTimes)
import Id exposing (JobID(..), LockID(..), NodeID(..), QueueID(..))
import Job exposing (Job, newJob)
import Lock exposing (AcquireResult(..))
import Node exposing (NodeData, NodeKind(..), NodeState(..))
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


drainAll : Topology -> SimState -> SimState
drainAll topo state =
    case state.eventQueue of
        [] ->
            state
        _ ->
            drainAll topo (processNextEvent topo state)


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

        -- All other event types are purely informational (already logged)
        _ ->
            state


-- ── Source: new job arrives ───────────────────────────────────────────────────

onJobArrived : Topology -> NodeID -> JobID -> SimState -> SimState
onJobArrived topo nid jid state =
    case SimState.getNode nid state of
        Just { kind } ->
            case kind of
                Source cfg ->
                    let
                        job =
                            newJob jid cfg.jobPriority 1.0 cfg.jobLabel state.clock

                        -- schedule next arrival
                        ( gap, seed1 ) =
                            sampleExp cfg.arrivalRate state.seed

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
                    case cfg.signoff of
                        Just lid ->
                            startSignoffRequest nid lid jid node state

                        Nothing ->
                            releaseJobToOutputs topo nid jid node state

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
                updatedNode =
                    { node | state = Signoff jid lid }

                state1 =
                    SimState.putNode nid updatedNode state
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
            in
            releaseJobToOutputs topo nid jid node state2

        _ ->
            state


-- ── Shared: push job to outputs, then go idle ─────────────────────────────────

releaseJobToOutputs : Topology -> NodeID -> JobID -> NodeData -> SimState -> SimState
releaseJobToOutputs topo nid jid node state =
    case SimState.getJob jid state of
        Nothing ->
            state

        Just job ->
            case pushToOutputs topo nid job state of
                -- pushToOutputs returns state; we need to know if it succeeded.
                -- Use the node state after the push to detect blocking.
                state1 ->
                    -- If job still exists in state, it was blocked; mark node.
                    -- If job was consumed by a Sink it's removed; node goes idle.
                    case SimState.getJob jid state1 of
                        Just _ ->
                            -- job still around means it went into a queue (or was dropped)
                            -- node can go idle and pull next
                            becomeIdle topo nid node state1

                        Nothing ->
                            -- job was removed (only Sink does this right now,
                            -- but unreachable here since jobs go via queues)
                            becomeIdle topo nid node state1


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


-- After a job enters a queue, wake any idle node whose input is that queue
wakeConsumers : Topology -> QueueID -> SimState -> SimState
wakeConsumers topo qid state =
    List.foldl
        (\nid acc ->
            case SimState.getNode nid acc of
                Just node ->
                    if node.state == Idle then
                        tryPullFromQueue nid node qid acc
                    else
                        acc

                Nothing ->
                    acc
        )
        state
        (Topology.queueConsumers qid topo)


becomeIdle : Topology -> NodeID -> NodeData -> SimState -> SimState
becomeIdle topo nid node state =
    let
        idleNode =
            { node | state = Idle }

        state1 =
            SimState.putNode nid idleNode state
    in
    case Topology.nodeInput nid topo of
        Just qid ->
            tryPullFromQueue nid idleNode qid state1

        Nothing ->
            state1


tryPullFromQueue : NodeID -> NodeData -> QueueID -> SimState -> SimState
tryPullFromQueue nid node qid state =
    case SimState.getQueue qid state of
        Nothing ->
            state

        Just queue ->
            case Queue.dequeue queue of
                Nothing ->
                    state

                Just ( job, queue1 ) ->
                    SimState.putQueue qid queue1 state
                        |> SimState.logEvent (event state.clock (JobDequeued qid nid job.id))
                        |> startService nid { node | state = Idle } job


startService : NodeID -> NodeData -> Job -> SimState -> SimState
startService nid node job state =
    case node.kind of
        Worker cfg ->
            let
                ( duration, seed1 ) =
                    ServiceTime.sample cfg.serviceTime job.size state.seed

                completionTime =
                    addTimes state.clock (EventTime duration)

                busyNode =
                    { node | state = Busy job.id completionTime }
            in
            { state | seed = seed1 }
                |> SimState.putNode nid busyNode
                |> SimState.logEvent (event state.clock (ServiceStarted nid job.id))
                |> SimState.scheduleEvent (event completionTime (ServiceComplete nid job.id))

        Sink ->
            state
                |> SimState.removeJob job.id
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
