module Metrics exposing
    ( NodeMetrics
    , QueueMetrics
    , SystemMetrics
    , compute
    )

import Dict exposing (Dict)
import Event exposing (EventType(..))
import EventTime exposing (eventTime2Int)
import Id exposing (JobID(..), NodeID(..), QueueID(..))
import Node exposing (NodeKind(..))
import SimState exposing (SimState)


type alias NodeMetrics =
    { utilisation    : Float   -- fraction of elapsed time the node was busy
    , jobsProcessed  : Int
    , avgServiceTime : Float   -- mean ticks per job in service
    }


type alias QueueMetrics =
    { avgLength : Float
    , maxLength : Int
    , dropCount : Int
    }


type alias SystemMetrics =
    { totalTicks   : Int
    , throughput   : Float     -- completed jobs per tick
    , cycleTimes   : List Int  -- raw per-job cycle times (for histogram / percentiles)
    , avgCycleTime : Float
    , p50CycleTime : Float
    , p95CycleTime : Float
    , nodes        : Dict Int NodeMetrics
    , queues       : Dict Int QueueMetrics
    }


-- Compute metrics from a completed SimState.
-- The event log is scanned in chronological order (it is stored newest-first).
compute : SimState -> SystemMetrics
compute state =
    let
        totalTicks =
            eventTime2Int state.clock

        sourceIds =
            state.nodes
                |> Dict.filter (\_ nd -> case nd.kind of
                    Source _ -> True
                    _ -> False)
                |> Dict.keys

        sinkIds =
            state.nodes
                |> Dict.filter (\_ nd -> case nd.kind of
                    Sink -> True
                    _ -> False)
                |> Dict.keys

        chronological =
            List.reverse state.eventLog

        acc =
            List.foldl (step sourceIds sinkIds) emptyAcc chronological

        -- node metrics
        nodeMetrics =
            Dict.map
                (\nid _ ->
                    let
                        busy =
                            Dict.get nid acc.busyTicks |> Maybe.withDefault 0

                        processed =
                            Dict.get nid acc.jobsProcessed |> Maybe.withDefault 0
                    in
                    { utilisation =
                        if totalTicks > 0 then
                            toFloat busy / toFloat totalTicks
                        else
                            0.0
                    , jobsProcessed = processed
                    , avgServiceTime =
                        if processed > 0 then
                            toFloat busy / toFloat processed
                        else
                            0.0
                    }
                )
                state.nodes

        -- queue metrics
        queueMetrics =
            Dict.map
                (\qid _ ->
                    { avgLength =
                        if totalTicks > 0 then
                            (Dict.get qid acc.queueTimeSum |> Maybe.withDefault 0.0)
                                / toFloat totalTicks
                        else
                            0.0
                    , maxLength =
                        Dict.get qid acc.queueMaxLen |> Maybe.withDefault 0
                    , dropCount =
                        Dict.get qid acc.queueDrops |> Maybe.withDefault 0
                    }
                )
                state.queues

        -- cycle time statistics
        sorted =
            List.sort acc.cycleTimes

        n =
            List.length sorted

        avgCT =
            if n > 0 then
                toFloat (List.sum acc.cycleTimes) / toFloat n
            else
                0.0
    in
    { totalTicks   = totalTicks
    , throughput   = if totalTicks > 0 then toFloat n / toFloat totalTicks else 0.0
    , cycleTimes   = acc.cycleTimes
    , avgCycleTime = avgCT
    , p50CycleTime = intPercentile 50 sorted
    , p95CycleTime = intPercentile 95 sorted
    , nodes        = nodeMetrics
    , queues       = queueMetrics
    }


-- ── Accumulator ───────────────────────────────────────────────────────────────

type alias Acc =
    { jobCreatedAt   : Dict Int Int    -- jid → tick when job entered the system
    , cycleTimes     : List Int
    , serviceStartAt : Dict Int Int    -- jid → tick when service started
    , busyTicks      : Dict Int Int    -- nid → cumulative busy ticks
    , jobsProcessed  : Dict Int Int    -- nid → completed job count
    , queueLen       : Dict Int Int    -- qid → current occupancy
    , queueMaxLen    : Dict Int Int    -- qid → peak occupancy
    , queueDrops     : Dict Int Int    -- qid → drop count
    , queueTimeSum   : Dict Int Float  -- qid → Σ(length × dt) for time-average
    , queueLastTick  : Dict Int Int    -- qid → tick of last length change
    }


emptyAcc : Acc
emptyAcc =
    { jobCreatedAt   = Dict.empty
    , cycleTimes     = []
    , serviceStartAt = Dict.empty
    , busyTicks      = Dict.empty
    , jobsProcessed  = Dict.empty
    , queueLen       = Dict.empty
    , queueMaxLen    = Dict.empty
    , queueDrops     = Dict.empty
    , queueTimeSum   = Dict.empty
    , queueLastTick  = Dict.empty
    }


step : List Int -> List Int -> Event.Event -> Acc -> Acc
step sourceIds sinkIds evt acc =
    let
        t =
            eventTime2Int evt.time
    in
    case evt.kind of
        JobArrived (NodeID nid) (JobID jid) ->
            if List.member nid sourceIds then
                { acc | jobCreatedAt = Dict.insert jid t acc.jobCreatedAt }

            else if List.member nid sinkIds then
                let
                    ct =
                        t - (Dict.get jid acc.jobCreatedAt |> Maybe.withDefault t)
                in
                { acc | cycleTimes = ct :: acc.cycleTimes }

            else
                acc

        ServiceStarted (NodeID _) (JobID jid) ->
            { acc | serviceStartAt = Dict.insert jid t acc.serviceStartAt }

        ServiceComplete (NodeID nid) (JobID jid) ->
            let
                started =
                    Dict.get jid acc.serviceStartAt |> Maybe.withDefault t

                duration =
                    t - started
            in
            { acc
                | busyTicks =
                    Dict.update nid (Just << (+) duration << Maybe.withDefault 0) acc.busyTicks
                , jobsProcessed =
                    Dict.update nid (Just << (+) 1 << Maybe.withDefault 0) acc.jobsProcessed
                , serviceStartAt =
                    Dict.remove jid acc.serviceStartAt
            }

        JobEnqueued (QueueID qid) _ ->
            updateQueue qid 1 t acc

        JobDequeued (QueueID qid) _ _ ->
            updateQueue qid -1 t acc

        JobDropped (QueueID qid) _ ->
            { acc
                | queueDrops =
                    Dict.update qid (Just << (+) 1 << Maybe.withDefault 0) acc.queueDrops
            }

        _ ->
            acc


-- Update queue length by `delta`, recording time-weighted sum for average.
updateQueue : Int -> Int -> Int -> Acc -> Acc
updateQueue qid delta t acc =
    let
        prevLen =
            Dict.get qid acc.queueLen |> Maybe.withDefault 0

        prevTick =
            Dict.get qid acc.queueLastTick |> Maybe.withDefault 0

        dt =
            t - prevTick

        newLen =
            max 0 (prevLen + delta)

        newTimeSum =
            (Dict.get qid acc.queueTimeSum |> Maybe.withDefault 0.0)
                + toFloat prevLen * toFloat dt

        newMax =
            max newLen (Dict.get qid acc.queueMaxLen |> Maybe.withDefault 0)
    in
    { acc
        | queueLen      = Dict.insert qid newLen acc.queueLen
        , queueMaxLen   = Dict.insert qid newMax acc.queueMaxLen
        , queueTimeSum  = Dict.insert qid newTimeSum acc.queueTimeSum
        , queueLastTick = Dict.insert qid t acc.queueLastTick
    }


-- ── Statistics helpers ────────────────────────────────────────────────────────

intPercentile : Int -> List Int -> Float
intPercentile p sorted =
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
            |> Maybe.withDefault 0
            |> toFloat
