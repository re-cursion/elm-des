module TestMetrics exposing (suite)

import Dict
import Engine
import EventTime exposing (EventTime(..))
import Expect
import Id exposing (JobID(..), NodeID(..), QueueID(..))
import Job exposing (Priority(..))
import Metrics
import Node exposing (NodeState(..), makeSource, makeSink, makeWorker)
import Queue
import Random
import ServiceTime exposing (ServiceTime(..))
import SimState exposing (SimState)
import Test exposing (Test, describe, test)
import Topology exposing (Topology)
import Event exposing (EventType(..))


-- ── Scenario ──────────────────────────────────────────────────────────────────
-- Source(1) → [Q1] → Worker(2) → [Q2] → Sink(3)

scenario : ( Topology, SimState )
scenario =
    let
        topo =
            Topology.empty
                |> Topology.addOutputEdge { from = NodeID 1, to = QueueID 1 }
                |> Topology.addInputEdge  { from = QueueID 1, to = NodeID 2 }
                |> Topology.addOutputEdge { from = NodeID 2, to = QueueID 2 }
                |> Topology.addInputEdge  { from = QueueID 2, to = NodeID 3 }

        sourceCfg =
            { arrivalRate = 0.5, jobPriority = Normal, highPriorityFraction = 0.0, jobLabel = "job" }

        workerCfg =
            { serviceTime = Deterministic 3, preemptive = False, signoff = Nothing, halted = False }

        mkQ =
            Queue.empty { capacity = 10, discipline = Queue.FIFO, overflow = Queue.Block }

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


runFor : Int -> SimState
runFor duration =
    let
        ( topo, init ) = scenario
    in
    Engine.advanceUntil (EventTime duration) topo init


-- ── Tests ─────────────────────────────────────────────────────────────────────

suite : Test
suite =
    describe "Metrics"
        [ describe "basic sanity"
            [ test "totalTicks is > 0 and does not exceed the deadline" <|
                \_ ->
                    let
                        m = Metrics.compute (runFor 500)
                    in
                    Expect.all
                        [ Expect.greaterThan 0
                        , Expect.atMost 500
                        ]
                        m.totalTicks

            , test "throughput is non-negative" <|
                \_ ->
                    let
                        m = Metrics.compute (runFor 500)
                    in
                    Expect.atLeast 0.0 m.throughput

            , test "at least one job completes after 500 ticks" <|
                \_ ->
                    let
                        m = Metrics.compute (runFor 500)
                    in
                    Expect.greaterThan 0 (List.length m.cycleTimes)
            ]

        , describe "cycle times"
            [ test "all cycle times are positive" <|
                \_ ->
                    let
                        m = Metrics.compute (runFor 500)
                    in
                    m.cycleTimes
                        |> List.all (\t -> t > 0)
                        |> Expect.equal True

            , test "avgCycleTime is consistent with cycleTimes list" <|
                \_ ->
                    let
                        m = Metrics.compute (runFor 500)
                        n = List.length m.cycleTimes
                        expected =
                            if n > 0 then
                                toFloat (List.sum m.cycleTimes) / toFloat n
                            else
                                0.0
                    in
                    Expect.within (Expect.Absolute 0.01) m.avgCycleTime expected

            , test "p95CycleTime >= p50CycleTime" <|
                \_ ->
                    let
                        m = Metrics.compute (runFor 500)
                    in
                    Expect.atLeast m.p50CycleTime m.p95CycleTime
            ]

        , describe "node metrics"
            [ test "worker utilisation is between 0 and 1" <|
                \_ ->
                    let
                        m = Metrics.compute (runFor 500)
                        util =
                            Dict.get 2 m.nodes
                                |> Maybe.map .utilisation
                                |> Maybe.withDefault -1.0
                    in
                    Expect.all
                        [ Expect.atLeast 0.0
                        , Expect.atMost 1.0
                        ]
                        util

            , test "worker has processed at least one job after 500 ticks" <|
                \_ ->
                    let
                        m = Metrics.compute (runFor 500)
                        processed =
                            Dict.get 2 m.nodes
                                |> Maybe.map .jobsProcessed
                                |> Maybe.withDefault 0
                    in
                    Expect.greaterThan 0 processed

            , test "source and sink nodes have zero utilisation" <|
                \_ ->
                    let
                        m = Metrics.compute (runFor 500)
                        sourceUtil =
                            Dict.get 1 m.nodes |> Maybe.map .utilisation |> Maybe.withDefault -1.0

                        sinkUtil =
                            Dict.get 3 m.nodes |> Maybe.map .utilisation |> Maybe.withDefault -1.0
                    in
                    Expect.equal ( 0.0, 0.0 ) ( sourceUtil, sinkUtil )
            ]

        , describe "interrupt-aware utilisation"
            [ test "halted ticks are excluded from the utilisation denominator" <|
                \_ ->
                    -- Hand-crafted event log: worker busy t=0..5 and t=15..20, halted t=5..15
                    -- busyTicks=10, haltedTicks=10, denom=20-10=10 → utilisation = 10/10 = 1.0
                    let
                        workerData =
                            Node.makeWorker "W"
                                { serviceTime = Deterministic 5
                                , preemptive  = False
                                , signoff     = Nothing
                                , halted      = False
                                }

                        base =
                            SimState.init (Random.initialSeed 0)
                                |> SimState.putNode (NodeID 2) workerData

                        addEvt t kind s =
                            SimState.logEvent (Event.event (EventTime t) kind) s

                        state =
                            { base | clock = EventTime 20 }
                                |> addEvt 0  (ServiceStarted  (NodeID 2) (JobID 1))
                                |> addEvt 5  (ServiceComplete  (NodeID 2) (JobID 1))
                                |> addEvt 5  (WorkerHalted     (NodeID 2))
                                |> addEvt 15 (WorkerResumed    (NodeID 2))
                                |> addEvt 15 (ServiceStarted   (NodeID 2) (JobID 2))
                                |> addEvt 20 (ServiceComplete   (NodeID 2) (JobID 2))

                        util =
                            Metrics.compute state
                                |> .nodes
                                |> Dict.get 2
                                |> Maybe.map .utilisation
                                |> Maybe.withDefault -1.0
                    in
                    Expect.within (Expect.Absolute 0.001) 1.0 util

            , test "worker utilisation stays in [0,1] with an interrupt window" <|
                \_ ->
                    let
                        ( topo, base ) = scenario

                        s =
                            base
                                |> SimState.scheduleEvent (Event.event (EventTime 50)  InterruptStarted)
                                |> SimState.scheduleEvent (Event.event (EventTime 100) InterruptEnded)

                        m = Metrics.compute (Engine.advanceUntil (EventTime 300) topo s)

                        util =
                            Dict.get 2 m.nodes
                                |> Maybe.map .utilisation
                                |> Maybe.withDefault -1.0
                    in
                    Expect.all
                        [ Expect.atLeast 0.0
                        , Expect.atMost 1.0
                        ]
                        util
            ]

        , describe "queue metrics"
            [ test "queue avgLength is non-negative" <|
                \_ ->
                    let
                        m = Metrics.compute (runFor 500)
                        q1Avg =
                            Dict.get 1 m.queues |> Maybe.map .avgLength |> Maybe.withDefault -1.0
                    in
                    Expect.atLeast 0.0 q1Avg

            , test "queue dropCount is zero when capacity is large" <|
                \_ ->
                    let
                        m = Metrics.compute (runFor 500)
                        drops =
                            Dict.get 1 m.queues |> Maybe.map .dropCount |> Maybe.withDefault -1
                    in
                    Expect.equal 0 drops
            ]
        ]
