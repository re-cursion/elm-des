module TestEngine exposing (suite)

import Dict
import Engine
import Event exposing (EventType(..))
import EventTime exposing (EventTime(..))
import Expect
import Id exposing (JobID(..), NodeID(..), QueueID(..))
import Job exposing (Priority(..))
import Node exposing (NodeState(..), makeSource, makeSink, makeWorker)
import Queue
import Random
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
            { arrivalRate = 1.0, jobPriority = Normal, jobLabel = "job" }

        workerCfg =
            { serviceRate = 1.0, preemptive = False, signoff = Nothing }

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
        ]
