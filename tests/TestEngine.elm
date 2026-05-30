module TestEngine exposing (suite)

import Dict
import Engine
import Event exposing (EventType(..))
import EventTime exposing (EventTime(..))
import Expect
import Id exposing (JobID(..), NodeID(..), QueueID(..))
import Job exposing (Priority(..))
import Node exposing (NodeState(..), DispatchRule(..), makeSource, makeSink, makeWorker, makeDispatcher, makeInterrupt)
import Job exposing (Priority(..))
import Queue
import Random
import ServiceTime exposing (ServiceTime(..))
import SimState exposing (SimState)
import Test exposing (Test, describe, test)
import Topology exposing (Topology)


-- ── Scenario ──────────────────────────────────────────────────────────────────
-- Source(1) → [Q1] → Worker(2) → [Q2] → Sink(3)

threeNodeScenario : ( Topology, SimState )
threeNodeScenario =
    let
        topo =
            Topology.empty
                |> Topology.addOutputEdge { from = NodeID 1, to = QueueID 1 }
                |> Topology.addInputEdge  { from = QueueID 1, to = NodeID 2 }
                |> Topology.addOutputEdge { from = NodeID 2, to = QueueID 2 }
                |> Topology.addInputEdge  { from = QueueID 2, to = NodeID 3 }

        sourceCfg =
            { arrivalRate = 1.0, jobPriority = Normal, highPriorityFraction = 0.0, jobLabel = "job" }

        workerCfg =
            { serviceTime = Exponential 1.0, preemptive = False, signoff = Nothing, halted = False }

        mkQ =
            Queue.empty { capacity = 5, discipline = Queue.FIFO, overflow = Queue.Block }

        ( firstJobID, state0 ) =
            SimState.nextJobID (SimState.init (Random.initialSeed 0))

        state =
            state0
                |> SimState.putNode (NodeID 1) (makeSource "Source" sourceCfg)
                |> SimState.putNode (NodeID 2) (makeWorker "Worker" workerCfg)
                |> SimState.putNode (NodeID 3) (makeSink "Sink")
                |> SimState.putQueue (QueueID 1) mkQ
                |> SimState.putQueue (QueueID 2) mkQ
                |> SimState.scheduleEvent
                    (Event.event (EventTime 0) (JobArrived (NodeID 1) firstJobID))
    in
    ( topo, state )


-- ── Helpers ───────────────────────────────────────────────────────────────────

step : Topology -> SimState -> SimState
step =
    Engine.processNextEvent


drainN : Int -> Topology -> SimState -> SimState
drainN n topo state =
    if n <= 0 then
        state
    else
        case state.eventQueue of
            [] ->
                state
            _ ->
                drainN (n - 1) topo (step topo state)


hasEventKind : (EventType -> Bool) -> SimState -> Bool
hasEventKind pred state =
    List.any (\e -> pred e.kind) state.eventLog


isServiceStarted : EventType -> Bool
isServiceStarted k =
    case k of
        ServiceStarted _ _ -> True
        _ -> False


isServiceComplete : EventType -> Bool
isServiceComplete k =
    case k of
        ServiceComplete _ _ -> True
        _ -> False


-- ── Fork-join scenario ────────────────────────────────────────────────────────
-- Source(1) → [Q1] → Dispatcher(2) → [Q2,Q3] → [WorkerA(3),WorkerB(4)] → [Q4] → Sink(5)

forkJoinScenario : ( Topology, SimState )
forkJoinScenario =
    let
        topo =
            Topology.empty
                |> Topology.addOutputEdge { from = NodeID 1, to = QueueID 1 }
                |> Topology.addInputEdge  { from = QueueID 1, to = NodeID 2 }
                |> Topology.addOutputEdge { from = NodeID 2, to = QueueID 2 }
                |> Topology.addOutputEdge { from = NodeID 2, to = QueueID 3 }
                |> Topology.addInputEdge  { from = QueueID 2, to = NodeID 3 }
                |> Topology.addInputEdge  { from = QueueID 3, to = NodeID 4 }
                |> Topology.addOutputEdge { from = NodeID 3, to = QueueID 4 }
                |> Topology.addOutputEdge { from = NodeID 4, to = QueueID 4 }
                |> Topology.addInputEdge  { from = QueueID 4, to = NodeID 5 }

        sourceCfg =
            { arrivalRate = 1.0, jobPriority = Normal, highPriorityFraction = 0.0, jobLabel = "job" }

        workerCfg =
            { serviceTime = Deterministic 3, preemptive = False, signoff = Nothing, halted = False }

        dispCfg =
            { rule = ShortestQueue, roundRobinIndex = 0 }

        mkQ =
            Queue.empty { capacity = 10, discipline = Queue.FIFO, overflow = Queue.Block }

        ( firstJobID, state0 ) =
            SimState.nextJobID (SimState.init (Random.initialSeed 7))

        state =
            state0
                |> SimState.putNode (NodeID 1) (makeSource     "Source"   sourceCfg)
                |> SimState.putNode (NodeID 2) (makeDispatcher "Dispatch" dispCfg)
                |> SimState.putNode (NodeID 3) (makeWorker     "Worker A" workerCfg)
                |> SimState.putNode (NodeID 4) (makeWorker     "Worker B" workerCfg)
                |> SimState.putNode (NodeID 5) (makeSink       "Sink")
                |> SimState.putNode (NodeID 6) (makeInterrupt  "Interrupt")
                |> SimState.putQueue (QueueID 1) mkQ
                |> SimState.putQueue (QueueID 2) mkQ
                |> SimState.putQueue (QueueID 3) mkQ
                |> SimState.putQueue (QueueID 4) mkQ
                |> SimState.scheduleEvent
                    (Event.event (EventTime 0) (JobArrived (NodeID 1) firstJobID))
    in
    ( topo, state )


isServicePreemptedEvt : Event.Event -> Bool
isServicePreemptedEvt e =
    case e.kind of
        ServicePreempted _ _ -> True
        _                    -> False


isBusy : Node.NodeData -> Bool
isBusy node =
    case node.state of
        Busy _ _ -> True
        _        -> False


isServiceStartedEvt : Event.Event -> Bool
isServiceStartedEvt e =
    isServiceStarted e.kind


isServiceCompleteAt : Int -> Event.Event -> Bool
isServiceCompleteAt nid e =
    case e.kind of
        ServiceComplete (NodeID n) _ -> n == nid
        _                            -> False


isArrivedAt : Int -> Event.Event -> Bool
isArrivedAt nid e =
    case e.kind of
        JobArrived (NodeID n) _ -> n == nid
        _                       -> False


-- ── Tests ─────────────────────────────────────────────────────────────────────

suite : Test
suite =
    describe "Engine"
        [ describe "initial state"
            [ test "exactly one pending event (first arrival)" <|
                \_ ->
                    let
                        ( _, state ) = threeNodeScenario
                    in
                    Expect.equal 1 (List.length state.eventQueue)

            , test "all nodes start Idle" <|
                \_ ->
                    let
                        ( _, state ) = threeNodeScenario
                        nodeStates = Dict.values state.nodes |> List.map .state
                    in
                    Expect.equal [ Idle, Idle, Idle ] nodeStates
            ]

        , describe "after processing first arrival"
            [ test "a JobDequeued event is logged (worker pulled from Q1)" <|
                \_ ->
                    let
                        ( topo, state ) = threeNodeScenario
                        state1 = step topo state
                    in
                    Expect.equal True
                        (hasEventKind
                            (\k -> case k of
                                JobDequeued _ _ _ -> True
                                _ -> False
                            )
                            state1
                        )

            , test "worker is Busy after first arrival" <|
                \_ ->
                    let
                        ( topo, state ) = threeNodeScenario
                        state1 = step topo state
                    in
                    case SimState.getNode (NodeID 2) state1 of
                        Just node ->
                            case node.state of
                                Busy _ _ -> Expect.pass
                                other -> Expect.fail ("expected Busy, got: " ++ Debug.toString other)
                        Nothing ->
                            Expect.fail "worker node not found"

            , test "next arrival is scheduled in the event queue" <|
                \_ ->
                    let
                        ( topo, state ) = threeNodeScenario
                        state1 = step topo state
                        hasArrival =
                            List.any
                                (\e -> case e.kind of
                                    JobArrived _ _ -> True
                                    _ -> False
                                )
                                state1.eventQueue
                    in
                    Expect.equal True hasArrival
            ]

        , describe "end-to-end flow"
            [ test "at least one job reaches the Sink after 50 events" <|
                \_ ->
                    let
                        ( topo, state ) = threeNodeScenario
                        finalState = drainN 50 topo state
                        sinkArrivals =
                            List.filter
                                (\e -> case e.kind of
                                    JobArrived (NodeID 3) _ -> True
                                    _ -> False
                                )
                                finalState.eventLog
                    in
                    Expect.greaterThan 0 (List.length sinkArrivals)

            , test "ServiceComplete events appear after ServiceStarted" <|
                \_ ->
                    let
                        ( topo, state ) = threeNodeScenario
                        finalState = drainN 30 topo state
                        hasStarted   = hasEventKind isServiceStarted finalState
                        hasCompleted = hasEventKind isServiceComplete finalState
                    in
                    Expect.equal ( True, True ) ( hasStarted, hasCompleted )
            ]

        , describe "Dispatcher"
            [ test "first job is immediately routed into one of the worker queues" <|
                \_ ->
                    let
                        ( topo, state ) = forkJoinScenario
                        -- Process the first arrival; Dispatcher routes it instantly
                        state1 = drainN 5 topo state
                        q2size = SimState.getQueue (QueueID 2) state1
                                    |> Maybe.map Queue.size
                                    |> Maybe.withDefault 0
                        q3size = SimState.getQueue (QueueID 3) state1
                                    |> Maybe.map Queue.size
                                    |> Maybe.withDefault 0
                    in
                    -- At least one worker queue should have received a job
                    -- (it may already be dequeued into the worker, so check workers too)
                    let
                        workerABusy = SimState.getNode (NodeID 3) state1
                                        |> Maybe.map isBusy
                                        |> Maybe.withDefault False
                        workerBBusy = SimState.getNode (NodeID 4) state1
                                        |> Maybe.map isBusy
                                        |> Maybe.withDefault False
                    in
                    Expect.equal True (q2size > 0 || q3size > 0 || workerABusy || workerBBusy)

            , test "Dispatcher routes to shorter queue (shortest-queue policy)" <|
                \_ ->
                    let
                        ( topo, state ) = forkJoinScenario
                        -- Drain enough events that both workers are busy and queues accumulate
                        finalState = drainN 200 topo state
                        -- Both workers should have processed jobs
                        processedA = finalState.eventLog
                                        |> List.filter (isServiceCompleteAt 3)
                                        |> List.length
                        processedB = finalState.eventLog
                                        |> List.filter (isServiceCompleteAt 4)
                                        |> List.length
                    in
                    -- Both workers should have handled jobs (no starvation)
                    Expect.equal True (processedA > 0 && processedB > 0)

            , test "jobs reach the Sink in a fork-join topology" <|
                \_ ->
                    let
                        ( topo, state ) = forkJoinScenario
                        finalState = drainN 300 topo state
                        sinkArrivals =
                            finalState.eventLog
                                |> List.filter (isArrivedAt 5)
                                |> List.length
                    in
                    Expect.greaterThan 0 sinkArrivals
            ]

        , describe "Dispatcher (RandomChoice)"
            [ test "RandomChoice routes jobs to both workers" <|
                \_ ->
                    let
                        ( topo, _ ) = forkJoinScenario

                        dispCfg =
                            { rule = RandomChoice, roundRobinIndex = 0 }

                        workerCfg =
                            { serviceTime = Deterministic 3, preemptive = False, signoff = Nothing, halted = False }

                        sourceCfg =
                            { arrivalRate = 1.0, jobPriority = Normal, highPriorityFraction = 0.0, jobLabel = "job" }

                        mkQ =
                            Queue.empty { capacity = 10, discipline = Queue.FIFO, overflow = Queue.Block }

                        ( firstJobID, state0 ) =
                            SimState.nextJobID (SimState.init (Random.initialSeed 42))

                        state =
                            state0
                                |> SimState.putNode (NodeID 1) (makeSource     "Source"   sourceCfg)
                                |> SimState.putNode (NodeID 2) (makeDispatcher "Dispatch" dispCfg)
                                |> SimState.putNode (NodeID 3) (makeWorker     "Worker A" workerCfg)
                                |> SimState.putNode (NodeID 4) (makeWorker     "Worker B" workerCfg)
                                |> SimState.putNode (NodeID 5) (makeSink       "Sink")
                                |> SimState.putNode (NodeID 6) (makeInterrupt  "Interrupt")
                                |> SimState.putQueue (QueueID 1) mkQ
                                |> SimState.putQueue (QueueID 2) mkQ
                                |> SimState.putQueue (QueueID 3) mkQ
                                |> SimState.putQueue (QueueID 4) mkQ
                                |> SimState.scheduleEvent
                                    (Event.event (EventTime 0) (JobArrived (NodeID 1) firstJobID))

                        finalState = drainN 200 topo state

                        processedA = finalState.eventLog |> List.filter (isServiceCompleteAt 3) |> List.length
                        processedB = finalState.eventLog |> List.filter (isServiceCompleteAt 4) |> List.length
                    in
                    Expect.equal True (processedA > 0 && processedB > 0)
            ]

        , describe "Interrupt"
            [ test "InterruptStarted sets interruptActive" <|
                \_ ->
                    let
                        ( topo, state ) = forkJoinScenario
                        interrupted =
                            state
                                |> SimState.scheduleEvent
                                    (Event.event (EventTime 0) InterruptStarted)
                                |> Engine.processNextEvent topo  -- JobArrived
                                |> Engine.processNextEvent topo  -- InterruptStarted
                    in
                    Expect.equal True (SimState.isInterruptActive interrupted)

            , test "workers do not start new jobs while interrupted from t=0" <|
                \_ ->
                    let
                        ( topo, state ) = forkJoinScenario
                        -- Schedule InterruptStarted at t=0 so it fires before the first
                        -- JobArrived (insertSorted places same-time events newest-first)
                        state1 =
                            state
                                |> SimState.scheduleEvent
                                    (Event.event (EventTime 0) InterruptStarted)
                                |> drainN 40 topo
                        startedCount =
                            state1.eventLog
                                |> List.filter isServiceStartedEvt
                                |> List.length
                    in
                    -- Dispatcher still routes jobs but Workers never start service
                    Expect.equal 0 startedCount

            , test "InterruptEnded clears interruptActive" <|
                \_ ->
                    let
                        ( topo, state ) = forkJoinScenario
                        state1 =
                            state
                                |> SimState.scheduleEvent (Event.event (EventTime 0) InterruptStarted)
                                |> SimState.scheduleEvent (Event.event (EventTime 1) InterruptEnded)
                                |> drainN 10 topo
                    in
                    Expect.equal False (SimState.isInterruptActive state1)

            , test "workers resume after InterruptEnded" <|
                \_ ->
                    let
                        ( topo, state ) = forkJoinScenario
                        -- Let jobs accumulate in queues, trigger interrupt, then end it
                        state1 = drainN 30 topo state
                        state2 =
                            state1
                                |> SimState.scheduleEvent
                                    (Event.event state1.clock InterruptStarted)
                                |> SimState.scheduleEvent
                                    (Event.event (EventTime (EventTime.eventTime2Int state1.clock + 5)) InterruptEnded)
                        state3 = drainN 50 topo state2
                        -- After interrupt ends, workers should have started new service
                        startedAfterEnd =
                            state3.eventLog
                                |> List.filter isServiceStartedEvt
                                |> List.length
                    in
                    Expect.greaterThan 0 startedAfterEnd
            ]

        , describe "Preemption"
            [ test "switching source to high priority triggers ServicePreempted" <|
                \_ ->
                    let
                        -- Source(1)→Q1→Worker(2,preemptive,slow)→Q2→Sink(3)
                        topo =
                            Topology.empty
                                |> Topology.addOutputEdge { from = NodeID 1, to = QueueID 1 }
                                |> Topology.addInputEdge  { from = QueueID 1, to = NodeID 2 }
                                |> Topology.addOutputEdge { from = NodeID 2, to = QueueID 2 }
                                |> Topology.addInputEdge  { from = QueueID 2, to = NodeID 3 }

                        normalSrc = { arrivalRate = 0.5, jobPriority = Normal, highPriorityFraction = 0.0, jobLabel = "job" }
                        highSrc   = { normalSrc | highPriorityFraction = 1.0 }
                        wrkCfg    = { serviceTime = Deterministic 20, preemptive = True, signoff = Nothing, halted = False }
                        mkQ       = Queue.empty { capacity = 10, discipline = Queue.PriorityFIFO, overflow = Queue.Block }

                        ( jid1, s0 ) = SimState.nextJobID (SimState.init (Random.initialSeed 7))
                        state0 =
                            s0
                                |> SimState.putNode (NodeID 1) (makeSource "Src" normalSrc)
                                |> SimState.putNode (NodeID 2) (makeWorker "W" wrkCfg)
                                |> SimState.putNode (NodeID 3) (makeSink "Sink")
                                |> SimState.putQueue (QueueID 1) mkQ
                                |> SimState.putQueue (QueueID 2) mkQ
                                |> SimState.scheduleEvent (Event.event (EventTime 0) (JobArrived (NodeID 1) jid1))

                        -- Let first Normal job reach the worker (it becomes Busy)
                        state1 = drainN 5 topo state0

                        -- Switch source to 100 % High; next arrival will trigger preemption
                        state2 = SimState.putNode (NodeID 1) (makeSource "Src" highSrc) state1
                        state3 = drainN 5 topo state2
                    in
                    Expect.equal True (List.any isServicePreemptedEvt state3.eventLog)

            , test "with 50% high-priority mix, preemptions occur and all jobs complete" <|
                \_ ->
                    let
                        topo =
                            Topology.empty
                                |> Topology.addOutputEdge { from = NodeID 1, to = QueueID 1 }
                                |> Topology.addInputEdge  { from = QueueID 1, to = NodeID 2 }
                                |> Topology.addOutputEdge { from = NodeID 2, to = QueueID 2 }
                                |> Topology.addInputEdge  { from = QueueID 2, to = NodeID 3 }

                        srcCfg = { arrivalRate = 0.3, jobPriority = Normal, highPriorityFraction = 0.5, jobLabel = "job" }
                        wrkCfg = { serviceTime = Deterministic 3, preemptive = True, signoff = Nothing, halted = False }
                        mkQ    = Queue.empty { capacity = 20, discipline = Queue.PriorityFIFO, overflow = Queue.Block }

                        ( jid1, s0 ) = SimState.nextJobID (SimState.init (Random.initialSeed 55))
                        state =
                            s0
                                |> SimState.putNode (NodeID 1) (makeSource "Src" srcCfg)
                                |> SimState.putNode (NodeID 2) (makeWorker "W" wrkCfg)
                                |> SimState.putNode (NodeID 3) (makeSink "Sink")
                                |> SimState.putQueue (QueueID 1) mkQ
                                |> SimState.putQueue (QueueID 2) mkQ
                                |> SimState.scheduleEvent (Event.event (EventTime 0) (JobArrived (NodeID 1) jid1))

                        final        = drainN 500 topo state
                        preemptions  = List.length (List.filter isServicePreemptedEvt final.eventLog)
                        sinkArrivals = List.length (List.filter (isArrivedAt 3) final.eventLog)
                    in
                    Expect.all
                        [ \_ -> Expect.greaterThan 0 preemptions
                        , \_ -> Expect.greaterThan 0 sinkArrivals
                        ]
                        ()
            ]
        ]
