module TestEnsemble exposing (suite)

import Dict
import Ensemble exposing (Distribution)
import EventTime exposing (EventTime(..))
import Expect
import Id exposing (NodeID(..), QueueID(..))
import Job exposing (Priority(..))
import Node exposing (makeSource, makeSink, makeWorker)
import Queue
import Random
import ServiceTime exposing (ServiceTime(..))
import SimState exposing (SimState)
import Test exposing (Test, describe, test)
import Topology exposing (Topology)
import Event exposing (EventType(..))


-- ── Scenario ──────────────────────────────────────────────────────────────────

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
            { arrivalRate = 0.3, jobPriority = Job.Normal, jobLabel = "job" }

        workerCfg =
            { serviceTime = Exponential 0.5, preemptive = False, signoff = Nothing }

        mkQ =
            Queue.empty { capacity = 10, discipline = Queue.FIFO, overflow = Queue.Block }

        ( firstJobID, state0 ) =
            SimState.nextJobID (SimState.init (Random.initialSeed 1))

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


runEnsemble : Int -> Int -> Ensemble.EnsembleStats
runEnsemble replicas duration =
    let
        ( topo, init ) = scenario
    in
    Ensemble.run { replicas = replicas, duration = duration } topo init


-- ── Distribution helpers ──────────────────────────────────────────────────────

isValidDist : Distribution -> Bool
isValidDist d =
    d.mean >= 0
        && d.stdDev >= 0
        && d.min <= d.p05
        && d.p05 <= d.p50
        && d.p50 <= d.p95
        && d.p95 <= d.max


-- ── Tests ─────────────────────────────────────────────────────────────────────

suite : Test
suite =
    describe "Ensemble"
        [ describe "run metadata"
            [ test "n equals the requested replica count" <|
                \_ ->
                    let s = runEnsemble 10 200
                    in Expect.equal 10 s.n

            , test "duration is stored correctly" <|
                \_ ->
                    let s = runEnsemble 5 300
                    in Expect.equal 300 s.duration
            ]

        , describe "distribution invariants"
            [ test "throughput distribution is ordered: min ≤ p05 ≤ p50 ≤ p95 ≤ max" <|
                \_ ->
                    let
                        s = runEnsemble 20 500
                    in
                    Expect.equal True (isValidDist s.throughput)

            , test "avgCycleTime distribution is ordered" <|
                \_ ->
                    let
                        s = runEnsemble 20 500
                    in
                    Expect.equal True (isValidDist s.avgCycleTime)

            , test "p95CycleTime distribution is ordered" <|
                \_ ->
                    let
                        s = runEnsemble 20 500
                    in
                    Expect.equal True (isValidDist s.p95CycleTime)

            , test "utilisation distributions for all nodes are valid" <|
                \_ ->
                    let
                        s = runEnsemble 20 500
                    in
                    s.utilisation
                        |> Dict.values
                        |> List.all isValidDist
                        |> Expect.equal True

            , test "avgQueueLen distributions are valid" <|
                \_ ->
                    let
                        s = runEnsemble 20 500
                    in
                    s.avgQueueLen
                        |> Dict.values
                        |> List.all isValidDist
                        |> Expect.equal True
            ]

        , describe "worker utilisation"
            [ test "worker utilisation mean is between 0 and 1" <|
                \_ ->
                    let
                        s = runEnsemble 20 500
                        util =
                            Dict.get 2 s.utilisation
                                |> Maybe.map .mean
                                |> Maybe.withDefault -1.0
                    in
                    Expect.all
                        [ Expect.atLeast 0.0
                        , Expect.atMost 1.0
                        ]
                        util

            , test "worker utilisation p95 >= p50" <|
                \_ ->
                    let
                        s = runEnsemble 20 500
                        d =
                            Dict.get 2 s.utilisation
                                |> Maybe.withDefault
                                    { mean = 0, stdDev = 0, min = 0, p05 = 0, p50 = 0, p95 = 0, max = 0 }
                    in
                    Expect.atLeast d.p50 d.p95
            ]

        , describe "single replica"
            [ test "running 1 replica still produces valid distributions" <|
                \_ ->
                    let
                        s = runEnsemble 1 300
                    in
                    Expect.equal True (isValidDist s.throughput)
            ]
        ]
