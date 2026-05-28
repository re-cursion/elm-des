module Ensemble exposing
    ( Distribution
    , EnsembleStats
    , run
    )

import Dict exposing (Dict)
import Engine
import EventTime exposing (EventTime(..))
import Metrics exposing (SystemMetrics)
import Random
import SimState exposing (SimState)
import Topology exposing (Topology)


-- ── Types ─────────────────────────────────────────────────────────────────────

type alias Distribution =
    { mean   : Float
    , stdDev : Float
    , min    : Float
    , p05    : Float
    , p50    : Float
    , p95    : Float
    , max    : Float
    }


type alias EnsembleStats =
    { n            : Int
    , duration     : Int               -- simulated time per replica
    , throughput   : Distribution
    , avgCycleTime : Distribution
    , p95CycleTime : Distribution
    , utilisation  : Dict Int Distribution   -- keyed by NodeID int
    , avgQueueLen  : Dict Int Distribution   -- keyed by QueueID int
    , dropCount    : Dict Int Distribution
    }


-- ── Runner ────────────────────────────────────────────────────────────────────

-- Run `n` independent replicas, each simulated for `duration` ticks.
-- `initState` must be the initial (pre-run) SimState; each replica gets a
-- distinct seed derived from it.
run : { replicas : Int, duration : Int } -> Topology -> SimState -> EnsembleStats
run cfg topo initState =
    let
        seeds =
            generateSeeds cfg.replicas initState.seed

        metrics =
            List.map
                (\seed ->
                    { initState | seed = seed }
                        |> Engine.advanceUntil (EventTime cfg.duration) topo
                        |> Metrics.compute
                )
                seeds
    in
    aggregate cfg metrics


-- ── Aggregation ───────────────────────────────────────────────────────────────

aggregate : { replicas : Int, duration : Int } -> List SystemMetrics -> EnsembleStats
aggregate cfg metrics =
    let
        nodeIds =
            metrics
                |> List.head
                |> Maybe.map (.nodes >> Dict.keys)
                |> Maybe.withDefault []

        queueIds =
            metrics
                |> List.head
                |> Maybe.map (.queues >> Dict.keys)
                |> Maybe.withDefault []
    in
    { n          = cfg.replicas
    , duration   = cfg.duration
    , throughput =
        dist (List.map .throughput metrics)
    , avgCycleTime =
        dist (List.map .avgCycleTime metrics)
    , p95CycleTime =
        dist (List.map .p95CycleTime metrics)
    , utilisation =
        nodeIds
            |> List.map (\nid ->
                ( nid
                , dist
                    (List.filterMap
                        (.nodes >> Dict.get nid >> Maybe.map .utilisation)
                        metrics
                    )
                )
            )
            |> Dict.fromList
    , avgQueueLen =
        queueIds
            |> List.map (\qid ->
                ( qid
                , dist
                    (List.filterMap
                        (.queues >> Dict.get qid >> Maybe.map .avgLength)
                        metrics
                    )
                )
            )
            |> Dict.fromList
    , dropCount =
        queueIds
            |> List.map (\qid ->
                ( qid
                , dist
                    (List.filterMap
                        (.queues >> Dict.get qid >> Maybe.map (toFloat << .dropCount))
                        metrics
                    )
                )
            )
            |> Dict.fromList
    }


-- Compute summary statistics for a list of floats.
dist : List Float -> Distribution
dist values =
    case values of
        [] ->
            { mean = 0, stdDev = 0, min = 0, p05 = 0, p50 = 0, p95 = 0, max = 0 }

        _ ->
            let
                n =
                    toFloat (List.length values)

                mean =
                    List.sum values / n

                variance =
                    List.map (\v -> (v - mean) ^ 2) values
                        |> List.sum
                        |> (\s -> s / n)

                sorted =
                    List.sort values
            in
            { mean   = mean
            , stdDev = sqrt variance
            , min    = List.minimum sorted |> Maybe.withDefault 0
            , p05    = floatPercentile 5  sorted
            , p50    = floatPercentile 50 sorted
            , p95    = floatPercentile 95 sorted
            , max    = List.maximum sorted |> Maybe.withDefault 0
            }


floatPercentile : Int -> List Float -> Float
floatPercentile p sorted =
    let
        n =
            List.length sorted
    in
    if n == 0 then
        0.0
    else
        let
            idx =
                clamp 0 (n - 1) (round (toFloat p / 100.0 * toFloat (n - 1)))
        in
        sorted
            |> List.drop idx
            |> List.head
            |> Maybe.withDefault 0.0


-- ── Seed generation ───────────────────────────────────────────────────────────

-- Derive n independent seeds from a base seed by advancing it n times.
generateSeeds : Int -> Random.Seed -> List Random.Seed
generateSeeds n baseSeed =
    List.foldl
        (\_ ( acc, s ) ->
            let
                ( _, s1 ) =
                    Random.step (Random.int 0 Random.maxInt) s
            in
            ( s1 :: acc, s1 )
        )
        ( [], baseSeed )
        (List.repeat n ())
        |> Tuple.first
        |> List.reverse
