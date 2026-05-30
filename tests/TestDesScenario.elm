module TestDesScenario exposing (suite)

import Des.Scenario as Scenario
import Dict
import EventTime exposing (EventTime(..))
import Expect
import Metrics
import ScenarioConfig
import Scenarios
import Test exposing (Test, describe, test)


decodeOrFail : String -> ScenarioConfig.ScenarioConfig
decodeOrFail json =
    case ScenarioConfig.decode json of
        Ok cfg -> cfg
        Err e  -> Debug.todo ("Scenario decode failed in test: " ++ e)


-- ── Tests ─────────────────────────────────────────────────────────────────────

suite : Test
suite =
    describe "Des.Scenario"
        [ describe "init"
            [ test "single-worker scenario initialises without error" <|
                \_ ->
                    let
                        cfg         = decodeOrFail Scenarios.singleWorker
                        ( model, _ ) = Scenario.init cfg
                    in
                    Expect.equal 4 (Dict.size model.simState.nodes)

            , test "two-parallel scenario initialises with 6 nodes" <|
                \_ ->
                    let
                        cfg         = decodeOrFail Scenarios.twoParallel
                        ( model, _ ) = Scenario.init cfg
                    in
                    Expect.equal 6 (Dict.size model.simState.nodes)

            , test "pipeline scenario initialises with 6 nodes" <|
                \_ ->
                    let
                        cfg         = decodeOrFail Scenarios.threePipeline
                        ( model, _ ) = Scenario.init cfg
                    in
                    Expect.equal 6 (Dict.size model.simState.nodes)

            , test "layout positions are populated from config" <|
                \_ ->
                    let
                        cfg         = decodeOrFail Scenarios.singleWorker
                        ( model, _ ) = Scenario.init cfg
                    in
                    Expect.all
                        [ \m -> Expect.equal 4 (Dict.size m.layout.nodes)
                        , \m -> Expect.equal 2 (Dict.size m.layout.queues)
                        , \m -> Expect.greaterThan 0 (round m.layout.svgW)
                        , \m -> Expect.greaterThan 0 (round m.layout.svgH)
                        ]
                        model

            , test "interrupt events are pre-scheduled" <|
                \_ ->
                    let
                        cfg         = decodeOrFail Scenarios.singleWorker
                        ( model, _ ) = Scenario.init cfg
                    in
                    let
                        hasAt150 =
                            model.simState.eventQueue
                                |> List.any (\e -> e.time == EventTime 150)
                    in
                    Expect.equal True hasAt150
            ]

        , describe "update Step"
            [ test "Step processes an event (event log grows)" <|
                \_ ->
                    let
                        cfg            = decodeOrFail Scenarios.singleWorker
                        ( model0, _ )  = Scenario.init cfg
                        ( model1, _ )  = Scenario.update Scenario.Step model0
                    in
                    Expect.greaterThan 0 (List.length model1.simState.eventLog)

            , test "RunToEnd completes and creates checkpoints" <|
                \_ ->
                    let
                        cfg            = decodeOrFail Scenarios.singleWorker
                        ( model0, _ )  = Scenario.init cfg
                        ( model1, _ )  = Scenario.update Scenario.RunToEnd model0
                    in
                    Expect.greaterThan 0 (List.length model1.checkpoints)

            , test "Reset brings clock back to 0" <|
                \_ ->
                    let
                        cfg           = decodeOrFail Scenarios.singleWorker
                        ( m0, _ )     = Scenario.init cfg
                        ( m1, _ )     = Scenario.update Scenario.RunToEnd m0
                        ( m2, _ )     = Scenario.update Scenario.Reset m1
                    in
                    Expect.equal (EventTime 0) m2.simState.clock
            ]

        , describe "simulation quality"
            [ test "single-worker produces completed jobs after RunToEnd" <|
                \_ ->
                    let
                        cfg           = decodeOrFail Scenarios.singleWorker
                        ( m0, _ )     = Scenario.init cfg
                        ( m1, _ )     = Scenario.update Scenario.RunToEnd m0
                        metrics       = Metrics.compute m1.simState
                    in
                    Expect.greaterThan 0 (List.length metrics.cycleTimes)

            , test "two-parallel worker utilisations are in [0,1]" <|
                \_ ->
                    let
                        cfg           = decodeOrFail Scenarios.twoParallel
                        ( m0, _ )     = Scenario.init cfg
                        ( m1, _ )     = Scenario.update Scenario.RunToEnd m0
                        metrics       = Metrics.compute m1.simState
                        utils         =
                            Dict.values metrics.nodes
                                |> List.map .utilisation
                    in
                    utils
                        |> List.all (\u -> u >= 0.0 && u <= 1.0)
                        |> Expect.equal True

            , test "pipeline scenario produces positive cycle times" <|
                \_ ->
                    let
                        cfg           = decodeOrFail Scenarios.threePipeline
                        ( m0, _ )     = Scenario.init cfg
                        ( m1, _ )     = Scenario.update Scenario.RunToEnd m0
                        metrics       = Metrics.compute m1.simState
                    in
                    metrics.cycleTimes
                        |> List.all (\ct -> ct > 0)
                        |> Expect.equal True
            ]
        ]
