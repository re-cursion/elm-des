module Metrics exposing
    ( NodeMetrics
    , QueueMetrics
    , JobMetrics
    , SystemMetrics
    , compute
    , BusySegment
    , QueueStep
    , Timelines
    , computeTimelines
    )

import Dict exposing (Dict)
import Event exposing (EventType(..))
import EventTime exposing (EventTime, eventTime2Int)
import Id exposing (JobID(..), NodeID(..), QueueID(..))
import Job exposing (Job)
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


type alias JobMetrics =
    { jobId       : JobID
    , cycleTime   : Int   -- ticks from source arrival to sink arrival
    , serviceTime : Int   -- sum of all service windows
    , signoffTime : Int   -- sum of all signoff windows
    , waitTime    : Int   -- cycleTime - serviceTime - signoffTime
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
    , jobs         : List JobMetrics
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

        -- close any halted windows still open at end of run
        closedHaltedTicks =
            Dict.foldl
                (\nid startT hticks ->
                    Dict.update nid
                        (Just << (+) (totalTicks - startT) << Maybe.withDefault 0)
                        hticks
                )
                acc.haltedTicks
                acc.haltedStart

        -- node metrics
        nodeMetrics =
            Dict.map
                (\nid _ ->
                    let
                        busy =
                            Dict.get nid acc.busyTicks |> Maybe.withDefault 0

                        halted =
                            Dict.get nid closedHaltedTicks |> Maybe.withDefault 0

                        denom =
                            totalTicks - halted

                        processed =
                            Dict.get nid acc.jobsProcessed |> Maybe.withDefault 0
                    in
                    { utilisation =
                        if denom > 0 then
                            toFloat busy / toFloat denom
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

        jobMetrics =
            Dict.values state.completedJobs
                |> List.map jobMetricsFromJob
    in
    { totalTicks   = totalTicks
    , throughput   = if totalTicks > 0 then toFloat n / toFloat totalTicks else 0.0
    , cycleTimes   = acc.cycleTimes
    , avgCycleTime = avgCT
    , p50CycleTime = intPercentile 50 sorted
    , p95CycleTime = intPercentile 95 sorted
    , nodes        = nodeMetrics
    , queues       = queueMetrics
    , jobs         = jobMetrics
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
    , haltedStart    : Dict Int Int    -- nid → tick of most recent WorkerHalted
    , haltedTicks    : Dict Int Int    -- nid → cumulative halted ticks
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
    , haltedStart    = Dict.empty
    , haltedTicks    = Dict.empty
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

        WorkerHalted (NodeID nid) ->
            { acc | haltedStart = Dict.insert nid t acc.haltedStart }

        WorkerResumed (NodeID nid) ->
            case Dict.get nid acc.haltedStart of
                Nothing ->
                    acc

                Just startT ->
                    { acc
                        | haltedStart = Dict.remove nid acc.haltedStart
                        , haltedTicks =
                            Dict.update nid
                                (Just << (+) (t - startT) << Maybe.withDefault 0)
                                acc.haltedTicks
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


-- ── Timeline types & computation ─────────────────────────────────────────────

type alias BusySegment =
    { from : Int
    , to   : Int
    }


type alias QueueStep =
    { t   : Int
    , len : Int
    }


type alias Timelines =
    { totalTicks  : Int
    , nodeBusy    : Dict Int (List BusySegment)
    , queueLength : Dict Int (List QueueStep)
    }


computeTimelines : SimState -> Timelines
computeTimelines state =
    let
        totalTicks =
            eventTime2Int state.clock

        chronological =
            List.reverse state.eventLog

        acc0 : TlAcc
        acc0 =
            { svcStart  = Dict.empty
            , busySegs  = Dict.empty
            , queueLen  = Dict.empty
            , queueHist = Dict.empty
            }

        acc =
            List.foldl tlStep acc0 chronological

        -- Close any segment that is still open (worker busy at end of run)
        closedBusy =
            Dict.foldl
                (\nid startT segs ->
                    Dict.update nid
                        (Just << (::) { from = startT, to = totalTicks } << Maybe.withDefault [])
                        segs
                )
                acc.busySegs
                acc.svcStart
    in
    { totalTicks  = totalTicks
    , nodeBusy    = Dict.map (\_ segs -> List.reverse segs) closedBusy
    , queueLength = Dict.map (\_ steps -> List.reverse steps) acc.queueHist
    }


type alias TlAcc =
    { svcStart  : Dict Int Int
    , busySegs  : Dict Int (List BusySegment)
    , queueLen  : Dict Int Int
    , queueHist : Dict Int (List QueueStep)
    }


tlStep : Event.Event -> TlAcc -> TlAcc
tlStep evt acc =
    let
        t = eventTime2Int evt.time
    in
    case evt.kind of
        ServiceStarted (NodeID nid) _ ->
            { acc | svcStart = Dict.insert nid t acc.svcStart }

        ServiceComplete (NodeID nid) _ ->
            case Dict.get nid acc.svcStart of
                Nothing ->
                    acc

                Just startT ->
                    { acc
                        | svcStart = Dict.remove nid acc.svcStart
                        , busySegs =
                            Dict.update nid
                                (Just << (::) { from = startT, to = t } << Maybe.withDefault [])
                                acc.busySegs
                    }

        JobEnqueued (QueueID qid) _ ->
            tlUpdateQueue qid 1 t acc

        JobDequeued (QueueID qid) _ _ ->
            tlUpdateQueue qid -1 t acc

        _ ->
            acc


tlUpdateQueue : Int -> Int -> Int -> TlAcc -> TlAcc
tlUpdateQueue qid delta t acc =
    let
        newLen =
            max 0 ((Dict.get qid acc.queueLen |> Maybe.withDefault 0) + delta)
    in
    { acc
        | queueLen  = Dict.insert qid newLen acc.queueLen
        , queueHist =
            Dict.update qid
                (Just << (::) { t = t, len = newLen } << Maybe.withDefault [])
                acc.queueHist
    }


-- ── Per-job metrics ───────────────────────────────────────────────────────────

jobMetricsFromJob : Job -> JobMetrics
jobMetricsFromJob job =
    let
        arrivedTick =
            eventTime2Int job.arrivedAt

        -- history is newest-first; find the sink arrival (last JobArrived = first in list that matches)
        sinkTick =
            job.history
                |> List.filterMap
                    (\( t, kind ) ->
                        case kind of
                            JobArrived _ _ -> Just (eventTime2Int t)
                            _              -> Nothing
                    )
                |> List.head
                |> Maybe.withDefault arrivedTick

        cycleTime =
            sinkTick - arrivedTick

        -- sum all matched ServiceStarted/ServiceComplete pairs by scanning chronologically
        chronoHistory =
            List.reverse job.history

        serviceTime =
            computeWindowSum
                (\k -> case k of
                    ServiceStarted _ _ -> True
                    _ -> False
                )
                (\k -> case k of
                    ServiceComplete _ _ -> True
                    _ -> False
                )
                chronoHistory

        signoffTime =
            computeWindowSum
                (\k -> case k of
                    SignoffRequested _ _ _ -> True
                    _ -> False
                )
                (\k -> case k of
                    SignoffComplete _ _ _ -> True
                    _ -> False
                )
                chronoHistory
    in
    { jobId       = job.id
    , cycleTime   = cycleTime
    , serviceTime = serviceTime
    , signoffTime = signoffTime
    , waitTime    = max 0 (cycleTime - serviceTime - signoffTime)
    }


-- Sum durations of matched open/close event pairs.
computeWindowSum : (EventType -> Bool) -> (EventType -> Bool) -> List ( EventTime, EventType ) -> Int
computeWindowSum isOpen isClose history =
    let
        go events openAt total =
            case events of
                [] ->
                    total

                ( t, kind ) :: rest ->
                    if isOpen kind then
                        go rest (Just (eventTime2Int t)) total

                    else if isClose kind then
                        case openAt of
                            Just startT ->
                                go rest Nothing (total + eventTime2Int t - startT)

                            Nothing ->
                                go rest Nothing total

                    else
                        go rest openAt total
    in
    go history Nothing 0


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
