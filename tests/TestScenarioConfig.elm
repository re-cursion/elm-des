module TestScenarioConfig exposing (suite)

import Dict
import Engine
import EventTime exposing (EventTime(..))
import Expect
import Id exposing (NodeID(..), QueueID(..))
import Metrics
import ScenarioConfig exposing (NodeSpecKind(..))
import Test exposing (Test, describe, test)


-- ── Minimal single-worker JSON ─────────────────────────────────────────────

singleWorkerJson : String
singleWorkerJson =
    """
    {
      "meta": { "id": "test", "title": "Test", "theme": "plain", "seed": 1, "speed": 10 },
      "nodes": [
        { "id": 1, "kind": "source", "label": "Source", "x": 0,   "y": 0,
          "arrivalRate": 0.5, "priority": "normal" },
        { "id": 2, "kind": "worker", "label": "Worker", "x": 200, "y": 0,
          "serviceTime": { "kind": "deterministic", "ticks": 4 } },
        { "id": 3, "kind": "sink",   "label": "Sink",   "x": 400, "y": 0 }
      ],
      "queues": [
        { "id": 1, "label": "Q1", "x": 100, "y": 0, "capacity": 5,
          "discipline": "fifo", "overflow": "block" },
        { "id": 2, "label": "Q2", "x": 300, "y": 0, "capacity": 5,
          "discipline": "fifo", "overflow": "block" }
      ],
      "edges": [
        { "from": "node:1",  "to": "queue:1" },
        { "from": "queue:1", "to": "node:2"  },
        { "from": "node:2",  "to": "queue:2" },
        { "from": "queue:2", "to": "node:3"  }
      ]
    }
    """


-- ── Parallel workers JSON ──────────────────────────────────────────────────

parallelJson : String
parallelJson =
    """
    {
      "meta": { "id": "parallel", "title": "Parallel", "theme": "plain", "seed": 42, "speed": 5 },
      "nodes": [
        { "id": 1, "kind": "source",     "label": "Source",     "arrivalRate": 0.4, "x": 0,   "y": 100 },
        { "id": 2, "kind": "dispatcher", "label": "Dispatch",   "rule": "shortest-queue",
          "x": 200, "y": 100 },
        { "id": 3, "kind": "worker",     "label": "Worker A",
          "serviceTime": { "kind": "log-normal", "mu": 1.5, "sigma": 0.3 },
          "preemptive": true, "x": 400, "y": 50 },
        { "id": 4, "kind": "worker",     "label": "Worker B",
          "serviceTime": { "kind": "log-normal", "mu": 1.5, "sigma": 0.3 },
          "signoff": "inspector", "x": 400, "y": 150 },
        { "id": 5, "kind": "sink",       "label": "Sink",        "x": 600, "y": 100 }
      ],
      "queues": [
        { "id": 1, "label": "Q1", "capacity": 8, "x": 100, "y": 100, "discipline": "fifo",          "overflow": "block" },
        { "id": 2, "label": "Q2", "capacity": 4, "x": 300, "y": 50,  "discipline": "fifo",          "overflow": "block" },
        { "id": 3, "label": "Q3", "capacity": 4, "x": 300, "y": 150, "discipline": "priority-fifo", "overflow": "block" },
        { "id": 4, "label": "Q4", "capacity": 6, "x": 500, "y": 100, "discipline": "fifo",          "overflow": "block" }
      ],
      "edges": [
        { "from": "node:1",  "to": "queue:1" },
        { "from": "queue:1", "to": "node:2"  },
        { "from": "node:2",  "to": "queue:2" },
        { "from": "node:2",  "to": "queue:3" },
        { "from": "queue:2", "to": "node:3"  },
        { "from": "queue:3", "to": "node:4"  },
        { "from": "node:3",  "to": "queue:4" },
        { "from": "node:4",  "to": "queue:4" },
        { "from": "queue:4", "to": "node:5"  }
      ],
      "locks": [
        { "id": "inspector", "label": "Inspector", "capacity": 1,
          "serviceTime": { "kind": "deterministic", "ticks": 3 } }
      ],
      "scheduledEvents": [
        { "at": 50, "kind": "boss-meeting", "duration": 10, "label": "Stand-up" }
      ]
    }
    """


-- ── Tests ─────────────────────────────────────────────────────────────────────

suite : Test
suite =
    describe "ScenarioConfig"
        [ describe "JSON decode"
            [ test "decodes a minimal single-worker scenario" <|
                \_ ->
                    ScenarioConfig.decode singleWorkerJson
                        |> Expect.ok

            , test "meta fields are correct" <|
                \_ ->
                    case ScenarioConfig.decode singleWorkerJson of
                        Err e ->
                            Expect.fail (Debug.toString e)

                        Ok cfg ->
                            Expect.all
                                [ \c -> Expect.equal "test" c.meta.id
                                , \c -> Expect.equal 1 c.meta.seed
                                , \c -> Expect.equal 10.0 c.meta.speed
                                ]
                                cfg

            , test "decodes three nodes" <|
                \_ ->
                    case ScenarioConfig.decode singleWorkerJson of
                        Err e ->
                            Expect.fail (Debug.toString e)

                        Ok cfg ->
                            Expect.equal 3 (List.length cfg.nodes)

            , test "decodes source node kind" <|
                \_ ->
                    case ScenarioConfig.decode singleWorkerJson of
                        Err e ->
                            Expect.fail (Debug.toString e)

                        Ok cfg ->
                            let
                                firstNode = List.head cfg.nodes
                            in
                            case firstNode of
                                Nothing ->
                                    Expect.fail "no nodes"

                                Just n ->
                                    case n.kind of
                                        SourceSpec c ->
                                            Expect.within (Expect.Absolute 0.001) 0.5 c.arrivalRate

                                        _ ->
                                            Expect.fail "expected SourceSpec"

            , test "decodes parallel scenario with 5 nodes and 4 queues" <|
                \_ ->
                    case ScenarioConfig.decode parallelJson of
                        Err e ->
                            Expect.fail (Debug.toString e)

                        Ok cfg ->
                            Expect.all
                                [ \c -> Expect.equal 5 (List.length c.nodes)
                                , \c -> Expect.equal 4 (List.length c.queues)
                                , \c -> Expect.equal 9 (List.length c.edges)
                                , \c -> Expect.equal 1 (List.length c.locks)
                                , \c -> Expect.equal 1 (List.length c.scheduledEvents)
                                ]
                                cfg

            , test "decodes dispatcher with shortest-queue rule" <|
                \_ ->
                    case ScenarioConfig.decode parallelJson of
                        Err e ->
                            Expect.fail (Debug.toString e)

                        Ok cfg ->
                            let
                                dispNode = cfg.nodes |> List.filter (\n -> n.id == 2) |> List.head
                            in
                            case dispNode of
                                Nothing ->
                                    Expect.fail "no dispatcher node"

                                Just n ->
                                    case n.kind of
                                        DispatcherSpec c ->
                                            Expect.equal "ShortestQueue" (Debug.toString c.rule)

                                        _ ->
                                            Expect.fail "expected DispatcherSpec"

            , test "decodes worker signoff field" <|
                \_ ->
                    case ScenarioConfig.decode parallelJson of
                        Err e ->
                            Expect.fail (Debug.toString e)

                        Ok cfg ->
                            let
                                wrkB = cfg.nodes |> List.filter (\n -> n.id == 4) |> List.head
                            in
                            case wrkB of
                                Nothing ->
                                    Expect.fail "no worker B node"

                                Just n ->
                                    case n.kind of
                                        WorkerSpec c ->
                                            Expect.equal (Just "inspector") c.signoff

                                        _ ->
                                            Expect.fail "expected WorkerSpec"

            , test "decode fails on invalid JSON" <|
                \_ ->
                    ScenarioConfig.decode "{bad json"
                        |> (\r -> case r of
                                Err _ -> Expect.pass
                                Ok _  -> Expect.fail "expected error")

            , test "decode fails on unknown node kind" <|
                \_ ->
                    let
                        json =
                            """{"meta":{"id":"x","title":"x","theme":"x"},
                               "nodes":[{"id":1,"kind":"unknownkind","label":"X"}],
                               "queues":[],"edges":[]}"""
                    in
                    ScenarioConfig.decode json
                        |> (\r -> case r of
                                Err _ -> Expect.pass
                                Ok _  -> Expect.fail "expected error")
            ]

        , describe "toTopologyAndState"
            [ test "builds valid topology from single-worker JSON" <|
                \_ ->
                    case ScenarioConfig.decode singleWorkerJson of
                        Err e ->
                            Expect.fail (Debug.toString e)

                        Ok cfg ->
                            ScenarioConfig.toTopologyAndState cfg
                                |> Expect.ok

            , test "topology contains correct node and queue count" <|
                \_ ->
                    case ScenarioConfig.decode singleWorkerJson |> Result.andThen ScenarioConfig.toTopologyAndState of
                        Err e ->
                            Expect.fail e

                        Ok ( _, state ) ->
                            Expect.all
                                [ \s -> Expect.equal 3 (Dict.size s.nodes)
                                , \s -> Expect.equal 2 (Dict.size s.queues)
                                ]
                                state

            , test "simulation runs without crashing after decode" <|
                \_ ->
                    case ScenarioConfig.decode singleWorkerJson |> Result.andThen ScenarioConfig.toTopologyAndState of
                        Err e ->
                            Expect.fail e

                        Ok ( topo, initState ) ->
                            let
                                finalState = Engine.advanceUntil (EventTime 200) topo initState
                                m = Metrics.compute finalState
                            in
                            Expect.greaterThan 0 (List.length m.cycleTimes)

            , test "parallel scenario builds valid topology and runs" <|
                \_ ->
                    case ScenarioConfig.decode parallelJson |> Result.andThen ScenarioConfig.toTopologyAndState of
                        Err e ->
                            Expect.fail e

                        Ok ( topo, initState ) ->
                            let
                                finalState = Engine.advanceUntil (EventTime 300) topo initState
                                m = Metrics.compute finalState
                            in
                            Expect.all
                                [ \metrics -> Expect.greaterThan 0 (List.length metrics.cycleTimes)
                                , \metrics ->
                                    Dict.get 3 metrics.nodes
                                        |> Maybe.map .utilisation
                                        |> Maybe.withDefault -1.0
                                        |> (\u -> Expect.all [ Expect.atLeast 0.0, Expect.atMost 1.0 ] u)
                                ]
                                m

            , test "interrupt is scheduled from scheduledEvents" <|
                \_ ->
                    case ScenarioConfig.decode parallelJson |> Result.andThen ScenarioConfig.toTopologyAndState of
                        Err e ->
                            Expect.fail e

                        Ok ( _, initState ) ->
                            let
                                hasInterruptAt50 =
                                    initState.eventQueue
                                        |> List.any
                                            (\evt ->
                                                evt.time == EventTime 50
                                            )
                            in
                            Expect.equal True hasInterruptAt50
            ]
        ]
