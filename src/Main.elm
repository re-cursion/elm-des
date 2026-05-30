module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Engine
import Ensemble exposing (EnsembleStats)
import Event exposing (EventType(..))
import EventTime exposing (EventTime(..), addTimes, eventTime2Int)
import Html exposing (Html, button, div, h2, h3, input, li, option, p, select, span, table, td, text, th, tr, ul)
import Html.Attributes as HA exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Id exposing (JobID(..), LockID(..), NodeID(..), QueueID(..))
import Lock
import Job exposing (Priority(..))
import Metrics
import Node exposing (NodeKind(..), NodeState(..), DispatchRule(..), makeSource, makeSink, makeWorker, makeDispatcher, makeInterrupt)
import Queue
import Random
import ServiceTime exposing (ServiceTime(..))
import SimState exposing (SimState)
import Svg
import Svg.Attributes as SA
import Topology exposing (Topology)


-- ── Scenario ──────────────────────────────────────────────────────────────────
-- Source(1) → [Q1] → Worker(2) → [Q2] → Sink(3)

buildScenario : TopologyPreset -> Float -> ServiceTime -> Int -> Float -> DispatchRule -> ( Topology, SimState )
buildScenario preset arrivalRate serviceTime queueCap pctHigh dispatchRule =
    case preset of
        SingleWorker  -> buildSingle   arrivalRate serviceTime queueCap pctHigh
        TwoParallel   -> buildParallel arrivalRate serviceTime queueCap pctHigh dispatchRule
        ThreePipeline -> buildPipeline arrivalRate serviceTime queueCap pctHigh


-- Shared: schedule the first arrival and two pre-planned interrupt windows.
scheduleStandardEvents : JobID -> SimState -> SimState
scheduleStandardEvents jid state =
    state
        |> SimState.scheduleEvent (Event.event (EventTime 0)   (JobArrived (NodeID 1) jid))
        |> SimState.scheduleEvent (Event.event (EventTime 150) InterruptStarted)
        |> SimState.scheduleEvent (Event.event (EventTime 170) InterruptEnded)
        |> SimState.scheduleEvent (Event.event (EventTime 350) InterruptStarted)
        |> SimState.scheduleEvent (Event.event (EventTime 370) InterruptEnded)


-- ── M/M/1: Source(1) → Q1 → Worker(2) → Q2 → Sink(3), Interrupt(6) ──────────
buildSingle : Float -> ServiceTime -> Int -> Float -> ( Topology, SimState )
buildSingle arrivalRate serviceTime queueCap pctHigh =
    let
        topo =
            Topology.empty
                |> Topology.addOutputEdge { from = NodeID 1, to = QueueID 1 }
                |> Topology.addInputEdge  { from = QueueID 1, to = NodeID 2 }
                |> Topology.addOutputEdge { from = NodeID 2, to = QueueID 2 }
                |> Topology.addInputEdge  { from = QueueID 2, to = NodeID 3 }

        q   = Queue.empty { capacity = queueCap, discipline = Queue.FIFO, overflow = Queue.Block }
        src = { arrivalRate = arrivalRate, jobPriority = Normal, highPriorityFraction = pctHigh, jobLabel = "Job" }
        wrk = { serviceTime = serviceTime, preemptive = True, signoff = Nothing, halted = False }

        ( jid, s0 ) = SimState.nextJobID (SimState.init (Random.initialSeed 42))
        state =
            s0
                |> SimState.putNode (NodeID 1) (makeSource    "Source"    src)
                |> SimState.putNode (NodeID 2) (makeWorker    "Worker"    wrk)
                |> SimState.putNode (NodeID 3) (makeSink      "Sink")
                |> SimState.putNode (NodeID 6) (makeInterrupt "Interrupt")
                |> SimState.putQueue (QueueID 1) q
                |> SimState.putQueue (QueueID 2) q
                |> scheduleStandardEvents jid
    in
    ( topo, state )


-- ── M/M/2: Source(1) → Q1 → Dispatcher(2) → [Q2,Q3] → [WA(3),WB(4)] → Q4 → Sink(5) ─
buildParallel : Float -> ServiceTime -> Int -> Float -> DispatchRule -> ( Topology, SimState )
buildParallel arrivalRate serviceTime queueCap pctHigh dispatchRule =
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

        q    = Queue.empty { capacity = queueCap, discipline = Queue.FIFO, overflow = Queue.Block }
        src  = { arrivalRate = arrivalRate, jobPriority = Normal, highPriorityFraction = pctHigh, jobLabel = "Job" }
        wrkA = { serviceTime = serviceTime, preemptive = False, signoff = Nothing,                      halted = False }
        wrkB = { serviceTime = serviceTime, preemptive = False, signoff = Just (LockID "inspector"), halted = False }
        disp = { rule = dispatchRule, roundRobinIndex = 0 }

        inspector =
            Lock.newLock
                { id          = LockID "inspector"
                , label       = "Inspector"
                , capacity    = 1
                , serviceTime = Deterministic 5
                }

        ( jid, s0 ) = SimState.nextJobID (SimState.init (Random.initialSeed 42))
        state =
            s0
                |> SimState.putNode (NodeID 1) (makeSource     "Source"    src)
                |> SimState.putNode (NodeID 2) (makeDispatcher "Dispatch"  disp)
                |> SimState.putNode (NodeID 3) (makeWorker     "Worker A"  wrkA)
                |> SimState.putNode (NodeID 4) (makeWorker     "Worker B"  wrkB)
                |> SimState.putNode (NodeID 5) (makeSink       "Sink")
                |> SimState.putNode (NodeID 6) (makeInterrupt  "Interrupt")
                |> SimState.putLock (LockID "inspector") inspector
                |> SimState.putQueue (QueueID 1) q
                |> SimState.putQueue (QueueID 2) q
                |> SimState.putQueue (QueueID 3) q
                |> SimState.putQueue (QueueID 4) q
                |> scheduleStandardEvents jid
    in
    ( topo, state )


-- ── Pipeline: Source(1) → Q1 → WA(2) → Q2 → WB(3) → Q3 → WC(4) → Q4 → Sink(5) ─
buildPipeline : Float -> ServiceTime -> Int -> Float -> ( Topology, SimState )
buildPipeline arrivalRate serviceTime queueCap pctHigh =
    let
        topo =
            Topology.empty
                |> Topology.addOutputEdge { from = NodeID 1, to = QueueID 1 }
                |> Topology.addInputEdge  { from = QueueID 1, to = NodeID 2 }
                |> Topology.addOutputEdge { from = NodeID 2, to = QueueID 2 }
                |> Topology.addInputEdge  { from = QueueID 2, to = NodeID 3 }
                |> Topology.addOutputEdge { from = NodeID 3, to = QueueID 3 }
                |> Topology.addInputEdge  { from = QueueID 3, to = NodeID 4 }
                |> Topology.addOutputEdge { from = NodeID 4, to = QueueID 4 }
                |> Topology.addInputEdge  { from = QueueID 4, to = NodeID 5 }

        q   = Queue.empty { capacity = queueCap, discipline = Queue.FIFO, overflow = Queue.Block }
        src = { arrivalRate = arrivalRate, jobPriority = Normal, highPriorityFraction = pctHigh, jobLabel = "Job" }
        wrk = { serviceTime = serviceTime, preemptive = True, signoff = Nothing, halted = False }

        ( jid, s0 ) = SimState.nextJobID (SimState.init (Random.initialSeed 42))
        state =
            s0
                |> SimState.putNode (NodeID 1) (makeSource    "Source"    src)
                |> SimState.putNode (NodeID 2) (makeWorker    "Stage 1"   wrk)
                |> SimState.putNode (NodeID 3) (makeWorker    "Stage 2"   wrk)
                |> SimState.putNode (NodeID 4) (makeWorker    "Stage 3"   wrk)
                |> SimState.putNode (NodeID 5) (makeSink      "Sink")
                |> SimState.putNode (NodeID 6) (makeInterrupt "Interrupt")
                |> SimState.putQueue (QueueID 1) q
                |> SimState.putQueue (QueueID 2) q
                |> SimState.putQueue (QueueID 3) q
                |> SimState.putQueue (QueueID 4) q
                |> scheduleStandardEvents jid
    in
    ( topo, state )


-- ── Topology presets ──────────────────────────────────────────────────────────

type TopologyPreset
    = SingleWorker    -- Source → Q → Worker → Q → Sink  (M/M/1)
    | TwoParallel     -- Source → Q → Dispatcher → [Q,Q] → [WA,WB] → Q → Sink  (M/M/2)
    | ThreePipeline   -- Source → Q → WA → Q → WB → Q → WC → Q → Sink


presetLabel : TopologyPreset -> String
presetLabel p =
    case p of
        SingleWorker  -> "M/M/1  (single worker)"
        TwoParallel   -> "M/M/2  (parallel, shortest-queue)"
        ThreePipeline -> "Pipeline  (3 sequential stages)"


parseDispatchRule : String -> DispatchRule
parseDispatchRule s =
    if s == "roundrobin" then RoundRobin else ShortestQueue


parseServiceTime : String -> String -> String -> Maybe ServiceTime
parseServiceTime stype p1 p2 =
    case stype of
        "Exponential"   -> String.toFloat p1 |> Maybe.map Exponential
        "LogNormal"     -> Maybe.map2 LogNormal (String.toFloat p1) (String.toFloat p2)
        "Erlang"        -> Maybe.map2 Erlang (String.toInt p1) (String.toFloat p2)
        "Deterministic" -> String.toInt p1 |> Maybe.map Deterministic
        "Uniform"       -> Maybe.map2 Uniform (String.toInt p1) (String.toInt p2)
        _               -> Nothing


-- ── Playback ──────────────────────────────────────────────────────────────────

type PlaybackMode
    = Stopped
    | Playing Float   -- simulated time-units per real second


-- Above this speed, drain all pending events in one frame instead of pacing.
maxAnimatedSpeed : Float
maxAnimatedSpeed =
    500.0


speeds : List ( String, PlaybackMode )
speeds =
    [ ( "Pause",  Stopped )
    , ( "×1",     Playing 1 )
    , ( "×10",    Playing 10 )
    , ( "×100",   Playing 100 )
    , ( "Max",    Playing maxAnimatedSpeed )
    ]


-- ── Animated job transit ──────────────────────────────────────────────────────

type alias JobAnim =
    { x        : Float
    , y        : Float
    , tx       : Float
    , ty       : Float
    , priority : Priority
    }


-- ── Model ─────────────────────────────────────────────────────────────────────

type alias Model =
    { topo             : Topology
    , simState         : SimState
    , playback         : PlaybackMode
    , accumulator      : Float
    , checkpoints      : List SimState
    , jobAnims         : Dict Int JobAnim   -- visual transit dots, keyed by JobID int
    -- Scenario config (live UI state; applied on ApplyScenario)
    , topologyPreset      : TopologyPreset
    , cfgArrivalRate      : String
    , cfgSTType           : String
    , cfgParam1           : String
    , cfgParam2           : String
    , cfgQueueCap         : String
    , cfgPctHighPriority  : String   -- fraction of arrivals upgraded to High priority
    , cfgDispatchRule     : String   -- "shortest" | "roundrobin" (TwoParallel only)
    , interruptDuration   : Int      -- ticks for a manually triggered interrupt
    -- Ensemble
    , ensembleReplicas : Int
    , ensembleDuration : Int
    , ensembleResult   : Maybe EnsembleStats
    }


defaultArrivalRate : Float
defaultArrivalRate = 0.3

defaultServiceTime : ServiceTime
defaultServiceTime = LogNormal 1.2 0.4

defaultQueueCap : Int
defaultQueueCap = 5


defaultPreset : TopologyPreset
defaultPreset = TwoParallel

defaultPctHighPriority : Float
defaultPctHighPriority = 0.15

defaultInterruptDuration : Int
defaultInterruptDuration = 20


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( topo, simState ) =
            buildScenario defaultPreset defaultArrivalRate defaultServiceTime defaultQueueCap defaultPctHighPriority ShortestQueue
    in
    ( { topo                = topo
      , simState            = simState
      , playback            = Stopped
      , accumulator         = 0
      , checkpoints         = []
      , jobAnims            = Dict.empty
      , topologyPreset      = defaultPreset
      , cfgArrivalRate      = "0.3"
      , cfgSTType           = "LogNormal"
      , cfgParam1           = "1.2"
      , cfgParam2           = "0.4"
      , cfgQueueCap         = "5"
      , cfgPctHighPriority  = "0.15"
      , cfgDispatchRule     = "shortest"
      , interruptDuration   = defaultInterruptDuration
      , ensembleReplicas    = 100
      , ensembleDuration    = 1000
      , ensembleResult      = Nothing
      }
    , Cmd.none
    )


-- ── Update ────────────────────────────────────────────────────────────────────

type Msg
    = Step
    | RunToEnd
    | Reset
    | SetPlayback PlaybackMode
    | Tick
    | ScrubTo String
    | TriggerInterrupt
    | SetInterruptDuration String
    | SetTopologyPreset TopologyPreset
    | SetCfgArrivalRate String
    | SetCfgSTType String
    | SetCfgParam1 String
    | SetCfgParam2 String
    | SetCfgQueueCap String
    | SetCfgPctHighPriority String
    | SetCfgDispatchRule    String
    | ApplyScenario
    | SetReplicas String
    | SetDuration String
    | RunEnsemble


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step ->
            ( { model
                | simState = Engine.processNextEvent model.topo model.simState
                , playback = Stopped
              }
            , Cmd.none
            )

        RunToEnd ->
            let
                ( finalState, cps ) =
                    buildCheckpoints model.topo model.simState
            in
            ( { model
                | simState    = finalState
                , checkpoints = cps
                , playback    = Stopped
                , jobAnims    = Dict.empty
              }
            , Cmd.none
            )

        Reset ->
            let
                st  = parseServiceTime model.cfgSTType model.cfgParam1 model.cfgParam2
                        |> Maybe.withDefault defaultServiceTime
                ar  = String.toFloat model.cfgArrivalRate |> Maybe.withDefault defaultArrivalRate
                qc  = String.toInt   model.cfgQueueCap   |> Maybe.withDefault defaultQueueCap
                ph  = String.toFloat model.cfgPctHighPriority |> Maybe.withDefault defaultPctHighPriority |> clamp 0 1
                dr  = parseDispatchRule model.cfgDispatchRule
                ( topo, simState ) =
                    buildScenario model.topologyPreset (max 0.001 ar) st (max 1 qc) ph dr
            in
            ( { model
                | topo        = topo
                , simState    = simState
                , checkpoints = []
                , playback    = Stopped
                , accumulator = 0
                , jobAnims    = Dict.empty
              }
            , Cmd.none
            )

        TriggerInterrupt ->
            if SimState.isInterruptActive model.simState then
                ( model, Cmd.none )
            else
                let
                    t  = model.simState.clock
                    t2 = addTimes t (EventTime model.interruptDuration)
                    newState =
                        model.simState
                            |> SimState.scheduleEvent (Event.event t  InterruptStarted)
                            |> SimState.scheduleEvent (Event.event t2 InterruptEnded)
                in
                ( { model | simState = newState }, Cmd.none )

        SetInterruptDuration s ->
            case String.toInt s of
                Just n  -> ( { model | interruptDuration = max 1 n }, Cmd.none )
                Nothing -> ( model, Cmd.none )

        SetTopologyPreset p ->
            let
                st = parseServiceTime model.cfgSTType model.cfgParam1 model.cfgParam2
                        |> Maybe.withDefault defaultServiceTime
                ar = String.toFloat model.cfgArrivalRate |> Maybe.withDefault defaultArrivalRate
                qc = String.toInt   model.cfgQueueCap   |> Maybe.withDefault defaultQueueCap
                ph = String.toFloat model.cfgPctHighPriority |> Maybe.withDefault defaultPctHighPriority |> clamp 0 1
                dr = parseDispatchRule model.cfgDispatchRule
                ( topo, simState ) = buildScenario p (max 0.001 ar) st (max 1 qc) ph dr
            in
            ( { model
                | topologyPreset = p
                , topo        = topo
                , simState    = simState
                , checkpoints = []
                , playback    = Stopped
                , accumulator = 0
                , jobAnims    = Dict.empty
              }
            , Cmd.none
            )

        SetCfgArrivalRate     s -> ( { model | cfgArrivalRate     = s }, Cmd.none )
        SetCfgSTType          s -> ( { model | cfgSTType          = s }, Cmd.none )
        SetCfgParam1          s -> ( { model | cfgParam1          = s }, Cmd.none )
        SetCfgParam2          s -> ( { model | cfgParam2          = s }, Cmd.none )
        SetCfgQueueCap        s -> ( { model | cfgQueueCap        = s }, Cmd.none )
        SetCfgPctHighPriority s -> ( { model | cfgPctHighPriority = s }, Cmd.none )
        SetCfgDispatchRule    s -> ( { model | cfgDispatchRule    = s }, Cmd.none )

        ApplyScenario ->
            case ( parseServiceTime model.cfgSTType model.cfgParam1 model.cfgParam2
                 , String.toFloat model.cfgArrivalRate
                 , String.toInt   model.cfgQueueCap
                 ) of
                ( Just st, Just ar, Just qc ) ->
                    let
                        ph = String.toFloat model.cfgPctHighPriority |> Maybe.withDefault defaultPctHighPriority |> clamp 0 1
                        dr = parseDispatchRule model.cfgDispatchRule
                        ( topo, simState ) =
                            buildScenario model.topologyPreset (max 0.001 ar) st (max 1 qc) ph dr
                    in
                    ( { model
                        | topo        = topo
                        , simState    = simState
                        , checkpoints = []
                        , playback    = Stopped
                        , accumulator = 0
                        , jobAnims    = Dict.empty
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ScrubTo s ->
            case String.toInt s of
                Nothing ->
                    ( model, Cmd.none )

                Just t ->
                    let
                        nearest =
                            model.checkpoints
                                |> List.filter (\cp -> eventTime2Int cp.clock <= t)
                                |> List.reverse
                                |> List.head
                                |> Maybe.withDefault model.simState

                        newState =
                            Engine.advanceUntil (EventTime t) model.topo nearest
                    in
                    ( { model | simState = newState, playback = Stopped, jobAnims = Dict.empty }, Cmd.none )

        SetPlayback mode ->
            ( { model | playback = mode, accumulator = 0 }, Cmd.none )

        Tick ->
            case model.playback of
                Stopped ->
                    ( model, Cmd.none )

                Playing n ->
                    if n >= maxAnimatedSpeed then
                        ( { model
                            | simState = Engine.drainAll model.topo model.simState
                            , jobAnims = Dict.empty
                          }
                        , Cmd.none
                        )

                    else
                        let
                            oldLogLen =
                                List.length model.simState.eventLog

                            newAcc =
                                model.accumulator + n / 60.0

                            simUnits =
                                floor newAcc

                            newState =
                                if simUnits > 0 then
                                    Engine.advanceUntil
                                        (addTimes model.simState.clock (EventTime simUnits))
                                        model.topo
                                        model.simState
                                else
                                    model.simState

                            -- Events fired this frame, oldest-first
                            newEventsChron =
                                if simUnits > 0 then
                                    List.take
                                        (List.length newState.eventLog - oldLogLen)
                                        newState.eventLog
                                        |> List.reverse
                                else
                                    []

                            newAnims =
                                stepJobAnims (1.0 / 60.0) model.topologyPreset newState newEventsChron model.jobAnims
                        in
                        ( { model
                            | simState    = newState
                            , accumulator = newAcc - toFloat simUnits
                            , jobAnims    = newAnims
                          }
                        , Cmd.none
                        )

        SetReplicas s ->
            case String.toInt s of
                Just n -> ( { model | ensembleReplicas = max 1 n }, Cmd.none )
                Nothing -> ( model, Cmd.none )

        SetDuration s ->
            case String.toInt s of
                Just n -> ( { model | ensembleDuration = max 1 n }, Cmd.none )
                Nothing -> ( model, Cmd.none )

        RunEnsemble ->
            let
                st =
                    parseServiceTime model.cfgSTType model.cfgParam1 model.cfgParam2
                        |> Maybe.withDefault defaultServiceTime

                ar =
                    String.toFloat model.cfgArrivalRate
                        |> Maybe.withDefault defaultArrivalRate

                qc =
                    String.toInt model.cfgQueueCap
                        |> Maybe.withDefault defaultQueueCap

                ph =
                    String.toFloat model.cfgPctHighPriority |> Maybe.withDefault defaultPctHighPriority |> clamp 0 1

                dr =
                    parseDispatchRule model.cfgDispatchRule

                ( topo, initState ) =
                    buildScenario model.topologyPreset (max 0.001 ar) st (max 1 qc) ph dr

                result =
                    Ensemble.run
                        { replicas = model.ensembleReplicas
                        , duration = model.ensembleDuration
                        }
                        topo
                        initState
            in
            ( { model | ensembleResult = Just result, playback = Stopped }, Cmd.none )


-- Save a checkpoint every 100 events while draining up to 100 000 events.
buildCheckpoints : Topology -> SimState -> ( SimState, List SimState )
buildCheckpoints topo initState =
    let
        go n state cps =
            if n >= 100000 then
                ( state, List.reverse cps )
            else
                case state.eventQueue of
                    [] ->
                        ( state, List.reverse cps )

                    _ ->
                        let
                            s1 = Engine.processNextEvent topo state
                            newCps =
                                if modBy 100 (n + 1) == 0 then s1 :: cps
                                else cps
                        in
                        go (n + 1) s1 newCps
    in
    go 0 initState [ initState ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.playback of
        Playing _ ->
            Browser.Events.onAnimationFrame (\_ -> Tick)

        Stopped ->
            Sub.none


-- ── View ──────────────────────────────────────────────────────────────────────

view : Model -> Html Msg
view model =
    div [ style "font-family" "monospace", style "padding" "1rem" ]
        [ h2 [] [ text "elm-des — Discrete Event Simulation" ]
        , viewScenarioPanel model
        , viewPlaybackControls model
        , viewClock model.simState
        , viewCanvas model.topologyPreset model.simState model.jobAnims
        , viewScrubber model
        , viewLiveMetrics model.topologyPreset model.simState
        , viewHistory model
        , viewEventLog model.simState
        , viewEnsemblePanel model
        ]


viewPlaybackControls : Model -> Html Msg
viewPlaybackControls model =
    div [ style "margin-bottom" "0.5rem" ]
        (List.map (speedBtn model.playback) speeds
            ++ [ separator
               , btn Step     "Step"
               , btn RunToEnd "Run to end"
               , btn Reset    "Reset"
               , separator
               , viewInterruptControl model
               ]
        )


viewInterruptControl : Model -> Html Msg
viewInterruptControl model =
    if SimState.isInterruptActive model.simState then
        span [ style "color" "#d63031", style "font-size" "0.85em", style "font-weight" "bold" ]
            [ text "⚡ Workers interrupted" ]

    else
        span []
            [ button
                [ onClick TriggerInterrupt
                , style "padding" "0.25rem 0.75rem"
                , style "border" "1px solid #999"
                , style "cursor" "pointer"
                , style "margin-right" "0.25rem"
                ]
                [ text "Interrupt" ]
            , input
                [ type_ "number"
                , value (String.fromInt model.interruptDuration)
                , onInput SetInterruptDuration
                , style "width" "3.5rem"
                , style "font-family" "monospace"
                , style "margin-right" "0.25rem"
                ]
                []
            , span [ style "font-size" "0.8em", style "color" "#636e72" ] [ text "ticks" ]
            ]


speedBtn : PlaybackMode -> ( String, PlaybackMode ) -> Html Msg
speedBtn current ( label, mode ) =
    let
        isActive =
            current == mode

        bg =
            if isActive then "#333" else "#eee"

        fg =
            if isActive then "#fff" else "#333"
    in
    button
        [ onClick (SetPlayback mode)
        , style "margin-right" "0.25rem"
        , style "padding" "0.25rem 0.75rem"
        , style "background" bg
        , style "color" fg
        , style "border" "1px solid #999"
        , style "cursor" "pointer"
        ]
        [ text label ]


btn : Msg -> String -> Html Msg
btn msg label =
    button
        [ onClick msg
        , style "margin-right" "0.5rem"
        , style "padding" "0.25rem 0.75rem"
        , style "border" "1px solid #999"
        , style "cursor" "pointer"
        ]
        [ text label ]


separator : Html msg
separator =
    span [ style "margin-right" "0.75rem" ] [ text "|" ]


viewClock : SimState -> Html msg
viewClock state =
    p [] [ text ("Clock: " ++ String.fromInt (eventTime2Int state.clock)) ]


-- ── Animation helpers ─────────────────────────────────────────────────────────
-- Dots travel at a fixed real-time speed (canvas units/second) regardless of
-- playback rate, so they look smooth at ×1 and fluid at ×10.

animUnitsPerSec : Float
animUnitsPerSec = 400.0


canvasNodeCenter : TopologyPreset -> Int -> ( Float, Float )
canvasNodeCenter preset nid =
    case preset of
        SingleWorker ->
            case nid of
                1 -> ( 164, 90 )   -- Source
                2 -> ( 399, 90 )   -- Worker
                3 -> ( 634, 90 )   -- Sink
                _ -> ( 399, 90 )
        TwoParallel ->
            case nid of
                1 -> ( 18,  125 )  -- Source
                2 -> ( 192, 125 )  -- Dispatcher
                3 -> ( 384, 82  )  -- Worker A
                4 -> ( 384, 168 )  -- Worker B
                5 -> ( 630, 125 )  -- Sink
                _ -> ( 380, 125 )
        ThreePipeline ->
            case nid of
                1 -> ( 69,  90 )   -- Source
                2 -> ( 215, 90 )   -- Stage 1
                3 -> ( 387, 90 )   -- Stage 2
                4 -> ( 559, 90 )   -- Stage 3
                5 -> ( 691, 90 )   -- Sink
                _ -> ( 380, 90 )


canvasQueueCenter : TopologyPreset -> Int -> ( Float, Float )
canvasQueueCenter preset qid =
    case preset of
        SingleWorker ->
            case qid of
                1 -> ( 254, 90 )   -- Q1
                2 -> ( 544, 90 )   -- Q2
                _ -> ( 399, 90 )
        TwoParallel ->
            case qid of
                1 -> ( 105, 125 )  -- Q1
                2 -> ( 282, 82  )  -- Q2
                3 -> ( 282, 168 )  -- Q3
                4 -> ( 490, 125 )  -- Q4
                _ -> ( 380, 125 )
        ThreePipeline ->
            case qid of
                1 -> ( 129, 90 )   -- Q1
                2 -> ( 301, 90 )   -- Q2
                3 -> ( 473, 90 )   -- Q3
                4 -> ( 645, 90 )   -- Q4
                _ -> ( 380, 90 )


sinkNodeId : TopologyPreset -> Int
sinkNodeId preset =
    case preset of
        SingleWorker  -> 3
        TwoParallel   -> 5
        ThreePipeline -> 5


-- Update all job animations: apply new-event targets, then advance positions.
stepJobAnims : Float -> TopologyPreset -> SimState -> List Event.Event -> Dict Int JobAnim -> Dict Int JobAnim
stepJobAnims dt preset state newEventsChron anims =
    let
        withTargets =
            List.foldl (applyEventToAnim preset state) anims newEventsChron

        step =
            animUnitsPerSec * dt
    in
    Dict.map
        (\_ a ->
            let
                dx   = a.tx - a.x
                dy   = a.ty - a.y
                dist = sqrt (dx * dx + dy * dy)
            in
            if dist <= step then
                { a | x = a.tx, y = a.ty }
            else
                { a | x = a.x + dx / dist * step, y = a.y + dy / dist * step }
        )
        withTargets


applyEventToAnim : TopologyPreset -> SimState -> Event.Event -> Dict Int JobAnim -> Dict Int JobAnim
applyEventToAnim preset state evt anims =
    case evt.kind of
        JobArrived (NodeID nid) (JobID jid) ->
            if nid == 1 then
                let
                    prio     = SimState.getJob (JobID jid) state |> Maybe.map .priority |> Maybe.withDefault Normal
                    ( x, y ) = canvasNodeCenter preset 1
                in
                Dict.insert jid { x = x, y = y, tx = x, ty = y, priority = prio } anims

            else if nid == sinkNodeId preset then
                Dict.remove jid anims

            else
                anims

        JobEnqueued (QueueID qid) (JobID jid) ->
            let
                ( tx, ty ) = canvasQueueCenter preset qid
            in
            Dict.update jid (Maybe.map (\a -> { a | tx = tx, ty = ty })) anims

        JobDequeued (QueueID _) (NodeID nid) (JobID jid) ->
            let
                ( tx, ty ) = canvasNodeCenter preset nid
            in
            Dict.update jid (Maybe.map (\a -> { a | tx = tx, ty = ty })) anims

        _ ->
            anims


viewTransitDots : Dict Int JobAnim -> List (Svg.Svg msg)
viewTransitDots anims =
    Dict.values anims
        |> List.filterMap
            (\a ->
                let
                    dx = a.tx - a.x
                    dy = a.ty - a.y
                in
                if dx * dx + dy * dy > 4.0 then
                    Just
                        (Svg.circle
                            [ SA.cx (String.fromFloat a.x)
                            , SA.cy (String.fromFloat a.y)
                            , SA.r "7"
                            , SA.fill (priorityColor a.priority)
                            , SA.stroke "#fff"
                            , SA.strokeWidth "1.5"
                            , SA.opacity "0.92"
                            ]
                            []
                        )
                else
                    Nothing
            )


-- ── 2D SVG Canvas ─────────────────────────────────────────────────────────────
-- Layout: Source(1)→[Q1]→Dispatcher(2)→[Q2,Q3]→[WorkerA(3),WorkerB(4)]→[Q4]→Sink(5)
-- Canvas 760 × 230.  All coords in SVG user units.
--
-- Top lane (cy=82):   Q2, Worker A
-- Middle (cy=125):    Source, Q1, Dispatcher, Q4, Sink
-- Bottom lane (cy=168): Q3, Worker B

viewCanvas : TopologyPreset -> SimState -> Dict Int JobAnim -> Html msg
viewCanvas preset state anims =
    case preset of
        SingleWorker  -> viewCanvasSingle   state anims
        TwoParallel   -> viewCanvasParallel state anims
        ThreePipeline -> viewCanvasPipeline state anims


-- ── M/M/1 canvas: Source → Q1 → Worker → Q2 → Sink (760×180) ────────────────
viewCanvasSingle : SimState -> Dict Int JobAnim -> Html msg
viewCanvasSingle state anims =
    let
        metrics = Metrics.compute state
        cy = 90.0
        qh = 44.0
    in
    div [ style "margin-bottom" "1rem" ]
        [ Svg.svg
            [ SA.viewBox "0 0 760 180", SA.width "760", SA.height "180"
            , style "border" "1px solid #ccc", style "border-radius" "4px"
            , style "background" "#f8f9fa", style "display" "block"
            ]
            ([ svgDefs
            , svgEdge 186 cy 194 cy                     -- Source → Q1
            , svgEdge 314 cy 334 cy                     -- Q1 → Worker
            , svgEdge 464 cy 484 cy                     -- Worker → Q2
            , svgEdge 604 cy 612 cy                     -- Q2 → Sink
            , svgQueueNode 1 state metrics 194 (cy - qh/2) 120 qh
            , svgQueueNode 2 state metrics 484 (cy - qh/2) 120 qh
            , svgCircleNode 1 "Source" "#74b9ff" "#0984e3" state 164 cy 22
            , svgWorkerNode 2 334 (cy - qh/2) 130 qh state metrics
            , svgCircleNode 3 "Sink"   "#55efc4" "#00b894" state 634 cy 22
            , svgInterruptNode state
            ] ++ viewTransitDots anims)
        ]


-- ── M/M/2 canvas: Source → Q1 → Dispatcher → [Q2,Q3] → [WA,WB] → Q4 → Sink ─
viewCanvasParallel : SimState -> Dict Int JobAnim -> Html msg
viewCanvasParallel state anims =
    let
        metrics = Metrics.compute state
        cyM = 125.0
        cyT = 82.0
        cyB = 168.0
        qh  = 44.0
    in
    div [ style "margin-bottom" "1rem" ]
        [ Svg.svg
            [ SA.viewBox "0 0 760 230", SA.width "760", SA.height "230"
            , style "border" "1px solid #ccc", style "border-radius" "4px"
            , style "background" "#f8f9fa", style "display" "block"
            ]
            ([ svgDefs
            , svgEdge 36 cyM 62 cyM
            , svgEdge 148 cyM 156 cyM
            , svgEdge 228 (cyM - 8) 238 cyT
            , svgEdge 228 (cyM + 8) 238 cyB
            , svgEdge 326 cyT 334 cyT
            , svgEdge 326 cyB 334 cyB
            , svgEdge 434 cyT 446 (cyM - 8)
            , svgEdge 434 cyB 446 (cyM + 8)
            , svgEdge 534 cyM 612 cyM
            , svgQueueNode 1 state metrics 62  (cyM - qh/2) 86 qh
            , svgQueueNode 2 state metrics 238 (cyT - qh/2) 88 qh
            , svgQueueNode 3 state metrics 238 (cyB - qh/2) 88 qh
            , svgQueueNode 4 state metrics 446 (cyM - qh/2) 88 qh
            , svgCircleNode 1 "Source" "#74b9ff" "#0984e3" state 18 cyM 18
            , svgCircleNode 5 "Sink"   "#55efc4" "#00b894" state 630 cyM 18
            , svgDispatcherNode state 156 (cyM - 22) 72 44
            , svgWorkerNode 3 334 (cyT - 22) 100 44 state metrics
            , svgWorkerNode 4 334 (cyB - 22) 100 44 state metrics
            , svgLockBadge  state 334 (cyB + 23) 100
            , svgInterruptNode state
            ] ++ viewTransitDots anims)
        ]


-- ── Pipeline canvas: Source → Q1 → W1 → Q2 → W2 → Q3 → W3 → Q4 → Sink (760×180)
viewCanvasPipeline : SimState -> Dict Int JobAnim -> Html msg
viewCanvasPipeline state anims =
    let
        metrics = Metrics.compute state
        cy = 90.0
        qh = 40.0
        ww = 80.0
        qw = 60.0
        -- layout: source(r=14) gap q1(60) gap w1(80) gap q2(60) gap w2(80) gap q3(60) gap w3(80) gap q4(60) gap sink(r=14)
        -- horizontally centered, starting at x=69 for source center
        srcR = 14.0
        srcX = 69.0
        q1x  = srcX + srcR + 16
        w1x  = q1x + qw + 16
        q2x  = w1x + ww + 16
        w2x  = q2x + qw + 16
        q3x  = w2x + ww + 16
        w3x  = q3x + qw + 16
        q4x  = w3x + ww + 16
        snkX = q4x + qw + 16 + srcR
    in
    div [ style "margin-bottom" "1rem" ]
        [ Svg.svg
            [ SA.viewBox "0 0 760 180", SA.width "760", SA.height "180"
            , style "border" "1px solid #ccc", style "border-radius" "4px"
            , style "background" "#f8f9fa", style "display" "block"
            ]
            ([ svgDefs
            , svgEdge (srcX + srcR) cy q1x cy
            , svgEdge (q1x + qw) cy w1x cy
            , svgEdge (w1x + ww) cy q2x cy
            , svgEdge (q2x + qw) cy w2x cy
            , svgEdge (w2x + ww) cy q3x cy
            , svgEdge (q3x + qw) cy w3x cy
            , svgEdge (w3x + ww) cy q4x cy
            , svgEdge (q4x + qw) cy (snkX - srcR) cy
            , svgQueueNode 1 state metrics q1x (cy - qh/2) qw qh
            , svgQueueNode 2 state metrics q2x (cy - qh/2) qw qh
            , svgQueueNode 3 state metrics q3x (cy - qh/2) qw qh
            , svgQueueNode 4 state metrics q4x (cy - qh/2) qw qh
            , svgCircleNode 1 "Source" "#74b9ff" "#0984e3" state srcX cy srcR
            , svgCircleNode 5 "Sink"   "#55efc4" "#00b894" state snkX cy srcR
            , svgWorkerNode 2 w1x (cy - qh/2) ww qh state metrics
            , svgWorkerNode 3 w2x (cy - qh/2) ww qh state metrics
            , svgWorkerNode 4 w3x (cy - qh/2) ww qh state metrics
            , svgInterruptNode state
            ] ++ viewTransitDots anims)
        ]


svgDefs : Svg.Svg msg
svgDefs =
    Svg.defs []
        [ Svg.marker
            [ SA.id "arr"
            , SA.markerWidth "8"
            , SA.markerHeight "8"
            , SA.refX "7"
            , SA.refY "3"
            , SA.orient "auto"
            ]
            [ Svg.path [ SA.d "M0,0 L0,6 L8,3 z", SA.fill "#888" ] [] ]
        ]


svgEdge : Float -> Float -> Float -> Float -> Svg.Svg msg
svgEdge x1 y1 x2 y2 =
    Svg.line
        [ SA.x1 (String.fromFloat x1)
        , SA.y1 (String.fromFloat y1)
        , SA.x2 (String.fromFloat x2)
        , SA.y2 (String.fromFloat y2)
        , SA.stroke "#888"
        , SA.strokeWidth "2"
        , SA.markerEnd "url(#arr)"
        ]
        []


svgCircleNode : Int -> String -> String -> String -> SimState -> Float -> Float -> Float -> Svg.Svg msg
svgCircleNode nid fallbackLabel fill stroke state cx cy r =
    let
        lbl =
            SimState.getNode (NodeID nid) state
                |> Maybe.map .label
                |> Maybe.withDefault fallbackLabel
    in
    Svg.g []
        [ Svg.circle
            [ SA.cx (String.fromFloat cx)
            , SA.cy (String.fromFloat cy)
            , SA.r  (String.fromFloat r)
            , SA.fill fill
            , SA.stroke stroke
            , SA.strokeWidth "2"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat cx)
            , SA.y (String.fromFloat (cy + 5))
            , SA.textAnchor "middle"
            , SA.fontSize "11"
            , SA.fill "#2d3436"
            ]
            [ text lbl ]
        ]


svgWorkerNode : Int -> Float -> Float -> Float -> Float -> SimState -> Metrics.SystemMetrics -> Svg.Svg msg
svgWorkerNode nid nx ny nw nh state metrics =
    let
        bh  = 8   -- utilisation bar height at bottom

        mNode  = SimState.getNode (NodeID nid) state
        lbl    = mNode |> Maybe.map .label |> Maybe.withDefault "Worker"
        ns     = mNode |> Maybe.map .state |> Maybe.withDefault Idle
        halted =
            case mNode of
                Nothing -> False
                Just n  ->
                    case n.kind of
                        Worker cfg -> cfg.halted
                        _          -> False

        bodyColor =
            if halted then "#b2bec3"
            else case ns of
                Idle          -> "#dfe6e9"
                Busy _ _      -> "#fdcb6e"
                Blocked _     -> "#d63031"
                Signoff _ _   -> "#a29bfe"
                Preempted _ _ -> "#fd79a8"
                Paused _      -> "#b2bec3"

        stateStr =
            if halted then "halted"
            else case ns of
                Idle          -> "idle"
                Busy _ _      -> "busy"
                Blocked _     -> "blocked"
                Signoff _ _   -> "signoff"
                Preempted _ _ -> "preempted"
                Paused _      -> "paused"

        util =
            Dict.get nid metrics.nodes
                |> Maybe.map .utilisation
                |> Maybe.withDefault 0.0

        utilW = util * nw
        bodyH = nh - bh

        -- Dot for the current job (top-right corner of body)
        jobDot =
            case ns of
                Busy jid _ ->
                    case SimState.getJob jid state of
                        Just job ->
                            [ Svg.circle
                                [ SA.cx (String.fromFloat (nx + nw - 11))
                                , SA.cy (String.fromFloat (ny + 11))
                                , SA.r "8"
                                , SA.fill (priorityColor job.priority)
                                , SA.stroke "#fff"
                                , SA.strokeWidth "1.5"
                                ]
                                []
                            ]
                        Nothing ->
                            []
                _ ->
                    []
    in
    Svg.g []
        ([ Svg.rect
            [ SA.x (String.fromFloat nx)
            , SA.y (String.fromFloat ny)
            , SA.width (String.fromFloat nw)
            , SA.height (String.fromFloat bodyH)
            , SA.rx "5"
            , SA.fill bodyColor
            , SA.stroke "#636e72"
            , SA.strokeWidth "1.5"
            ]
            []
        , Svg.rect
            [ SA.x (String.fromFloat nx)
            , SA.y (String.fromFloat (ny + bodyH))
            , SA.width (String.fromFloat nw)
            , SA.height (String.fromFloat bh)
            , SA.rx "5"
            , SA.fill "#b2bec3"
            ]
            []
        , Svg.rect
            [ SA.x (String.fromFloat nx)
            , SA.y (String.fromFloat (ny + bodyH))
            , SA.width (String.fromFloat utilW)
            , SA.height (String.fromFloat bh)
            , SA.fill "#00b894"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat (nx + nw / 2))
            , SA.y (String.fromFloat (ny + bodyH / 2 - 4))
            , SA.textAnchor "middle"
            , SA.fontSize "12"
            , SA.fontWeight "bold"
            , SA.fill "#2d3436"
            ]
            [ text lbl ]
        , Svg.text_
            [ SA.x (String.fromFloat (nx + nw / 2))
            , SA.y (String.fromFloat (ny + bodyH / 2 + 10))
            , SA.textAnchor "middle"
            , SA.fontSize "10"
            , SA.fill "#636e72"
            ]
            [ text stateStr ]
        , Svg.text_
            [ SA.x (String.fromFloat (nx + nw / 2))
            , SA.y (String.fromFloat (ny + nh + 14))
            , SA.textAnchor "middle"
            , SA.fontSize "10"
            , SA.fill "#00b894"
            ]
            [ text (fmtPct util) ]
        ]
            ++ jobDot
        )


svgQueueNode : Int -> SimState -> Metrics.SystemMetrics -> Float -> Float -> Float -> Float -> Svg.Svg msg
svgQueueNode qid state metrics qx qy qw qh =
    let
        mQ   = SimState.getQueue (QueueID qid) state
        sz   = mQ |> Maybe.map Queue.size     |> Maybe.withDefault 0
        cap  = mQ |> Maybe.map Queue.capacity |> Maybe.withDefault 1
        jobs = mQ |> Maybe.map Queue.toList   |> Maybe.withDefault []

        frac      = if cap > 0 then toFloat sz / toFloat cap else 0.0
        fillColor = if frac >= 1.0 then "#d63031" else "#74b9ff"

        drops =
            Dict.get qid metrics.queues
                |> Maybe.map .dropCount
                |> Maybe.withDefault 0

        -- Job slot dots (up to 10 shown)
        displayCap  = min cap 10
        slotSpacing = qw / toFloat (displayCap + 1)
        slotR       = min 7.0 (slotSpacing / 2.5)
        slotY       = qy + qh * 0.62

        slots =
            List.range 0 (displayCap - 1)
                |> List.map
                    (\i ->
                        let
                            cx   = qx + slotSpacing * toFloat (i + 1)
                            mJob = List.drop i jobs |> List.head
                        in
                        case mJob of
                            Just job ->
                                Svg.circle
                                    [ SA.cx (String.fromFloat cx)
                                    , SA.cy (String.fromFloat slotY)
                                    , SA.r  (String.fromFloat slotR)
                                    , SA.fill (priorityColor job.priority)
                                    , SA.stroke "#fff"
                                    , SA.strokeWidth "1"
                                    ]
                                    []

                            Nothing ->
                                Svg.circle
                                    [ SA.cx (String.fromFloat cx)
                                    , SA.cy (String.fromFloat slotY)
                                    , SA.r  (String.fromFloat slotR)
                                    , SA.fill "none"
                                    , SA.stroke "#ccc"
                                    , SA.strokeWidth "1"
                                    ]
                                    []
                    )

        dropBadge =
            if drops > 0 then
                [ Svg.text_
                    [ SA.x (String.fromFloat (qx + qw - 3))
                    , SA.y (String.fromFloat (qy + 11))
                    , SA.textAnchor "end"
                    , SA.fontSize "9"
                    , SA.fill "#d63031"
                    ]
                    [ text ("▼" ++ String.fromInt drops) ]
                ]
            else
                []
    in
    Svg.g []
        ([ Svg.rect
            [ SA.x (String.fromFloat qx)
            , SA.y (String.fromFloat qy)
            , SA.width (String.fromFloat qw)
            , SA.height (String.fromFloat qh)
            , SA.rx "4"
            , SA.fill "#dfe6e9"
            , SA.stroke "#b2bec3"
            , SA.strokeWidth "1"
            ]
            []
         , Svg.rect
            [ SA.x (String.fromFloat qx)
            , SA.y (String.fromFloat qy)
            , SA.width (String.fromFloat (frac * qw))
            , SA.height (String.fromFloat qh)
            , SA.rx "4"
            , SA.fill fillColor
            , SA.opacity "0.18"
            ]
            []
         , Svg.text_
            [ SA.x (String.fromFloat (qx + qw / 2))
            , SA.y (String.fromFloat (qy + 13))
            , SA.textAnchor "middle"
            , SA.fontSize "10"
            , SA.fontWeight "bold"
            , SA.fill "#636e72"
            ]
            [ text ("Q" ++ String.fromInt qid) ]
         ]
            ++ slots
            ++ dropBadge
        )


-- Dispatcher: a chevron-style parallelogram to signal routing
svgDispatcherNode : SimState -> Float -> Float -> Float -> Float -> Svg.Svg msg
svgDispatcherNode state dx dy dw dh =
    let
        lbl = SimState.getNode (NodeID 2) state
                |> Maybe.map .label
                |> Maybe.withDefault "Dispatch"
        skew = 10.0
        pts =
            String.join " "
                [ String.fromFloat (dx + skew) ++ "," ++ String.fromFloat dy
                , String.fromFloat (dx + dw)   ++ "," ++ String.fromFloat dy
                , String.fromFloat (dx + dw - skew) ++ "," ++ String.fromFloat (dy + dh)
                , String.fromFloat dx           ++ "," ++ String.fromFloat (dy + dh)
                ]
    in
    Svg.g []
        [ Svg.polygon
            [ SA.points pts
            , SA.fill "#a29bfe"
            , SA.stroke "#6c5ce7"
            , SA.strokeWidth "1.5"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat (dx + dw / 2))
            , SA.y (String.fromFloat (dy + dh / 2 + 4))
            , SA.textAnchor "middle"
            , SA.fontSize "10"
            , SA.fontWeight "bold"
            , SA.fill "#2d3436"
            ]
            [ text lbl ]
        ]


-- Inspector lock badge: shows sign-off lock state (capacity=1) below Worker B
svgLockBadge : SimState -> Float -> Float -> Float -> Svg.Svg msg
svgLockBadge state bx by bw =
    let
        mLock    = SimState.getLock (LockID "inspector") state
        inUse    = mLock |> Maybe.map .inUse                          |> Maybe.withDefault 0
        nWaiters = mLock |> Maybe.map (List.length << .waiters)       |> Maybe.withDefault 0
        bh       = 16.0
        bgColor  = if inUse > 0 then "#a29bfe" else "#dfe6e9"
        fgColor  = if inUse > 0 then "#2d3436" else "#636e72"
        status   =
            if inUse == 0 then "free"
            else if nWaiters > 0 then "busy +" ++ String.fromInt nWaiters ++ " wait"
            else "busy"
    in
    Svg.g []
        [ Svg.rect
            [ SA.x (String.fromFloat bx)
            , SA.y (String.fromFloat by)
            , SA.width (String.fromFloat bw)
            , SA.height (String.fromFloat bh)
            , SA.rx "3"
            , SA.fill bgColor
            , SA.stroke (if inUse > 0 then "#6c5ce7" else "#b2bec3")
            , SA.strokeWidth "1"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat (bx + bw / 2))
            , SA.y (String.fromFloat (by + 11))
            , SA.textAnchor "middle"
            , SA.fontSize "9"
            , SA.fill fgColor
            ]
            [ text ("Inspector: " ++ status) ]
        ]


-- Interrupt node indicator: small badge in top-right corner
svgInterruptNode : SimState -> Svg.Svg msg
svgInterruptNode state =
    let
        active = SimState.isInterruptActive state
        bx = 650.0
        by = 8.0
        bw = 104.0
        bh = 22.0
        bgColor = if active then "#d63031" else "#dfe6e9"
        fgColor = if active then "#fff"     else "#636e72"
        lbl = if active then "⚡ INTERRUPT" else "Interrupt (idle)"
    in
    Svg.g []
        [ Svg.rect
            [ SA.x (String.fromFloat bx)
            , SA.y (String.fromFloat by)
            , SA.width (String.fromFloat bw)
            , SA.height (String.fromFloat bh)
            , SA.rx "4"
            , SA.fill bgColor
            , SA.stroke (if active then "#c0392b" else "#b2bec3")
            , SA.strokeWidth "1"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat (bx + bw / 2))
            , SA.y (String.fromFloat (by + 14))
            , SA.textAnchor "middle"
            , SA.fontSize "9"
            , SA.fontWeight "bold"
            , SA.fill fgColor
            ]
            [ text lbl ]
        ]


priorityColor : Priority -> String
priorityColor p =
    case p of
        Low      -> "#b2bec3"
        Normal   -> "#74b9ff"
        High     -> "#fdcb6e"
        Critical -> "#d63031"


-- ── Live metrics strip ────────────────────────────────────────────────────────

workerIdsForPreset : TopologyPreset -> List ( String, Int )
workerIdsForPreset preset =
    case preset of
        SingleWorker  -> [ ( "Worker", 2 ) ]
        TwoParallel   -> [ ( "Worker A", 3 ), ( "Worker B", 4 ) ]
        ThreePipeline -> [ ( "Stage 1", 2 ), ( "Stage 2", 3 ), ( "Stage 3", 4 ) ]


workerQueueIdsForPreset : TopologyPreset -> List ( String, Int )
workerQueueIdsForPreset preset =
    case preset of
        SingleWorker  -> [ ( "Q1", 1 ) ]
        TwoParallel   -> [ ( "Q2 (→ Worker A)", 2 ), ( "Q3 (→ Worker B)", 3 ) ]
        ThreePipeline -> [ ( "Q1", 1 ), ( "Q2", 2 ), ( "Q3", 3 ) ]


viewLiveMetrics : TopologyPreset -> SimState -> Html msg
viewLiveMetrics preset state =
    let
        m = Metrics.compute state
        n = List.length m.cycleTimes
        utilCells =
            workerIdsForPreset preset
                |> List.map
                    (\( lbl, nid ) ->
                        let util = Dict.get nid m.nodes |> Maybe.map .utilisation |> Maybe.withDefault 0.0
                        in metricCell lbl (fmtPct util)
                    )
    in
    div
        [ style "display" "flex"
        , style "gap" "2rem"
        , style "padding" "0.4rem 0"
        , style "font-size" "0.85em"
        , style "border-bottom" "1px solid #eee"
        , style "margin-bottom" "1rem"
        ]
        ([ metricCell "Throughput" (fmt m.throughput ++ " /tick")
         , metricCell "Avg cycle"  (fmt m.avgCycleTime ++ " ticks")
         , metricCell "p95 cycle"  (fmt m.p95CycleTime ++ " ticks")
         ]
         ++ utilCells
         ++ [ metricCell "Completed" (String.fromInt n ++ " jobs") ]
        )


metricCell : String -> String -> Html msg
metricCell label val =
    div []
        [ div [ style "color" "#888", style "font-size" "0.8em" ] [ text label ]
        , div [ style "font-weight" "bold" ] [ text val ]
        ]


fmtPct : Float -> String
fmtPct f =
    String.fromInt (round (f * 100)) ++ "%"


-- ── Time-travel scrubber ──────────────────────────────────────────────────────

viewScrubber : Model -> Html Msg
viewScrubber model =
    case model.checkpoints of
        [] ->
            p [ style "color" "#aaa", style "font-size" "0.8em", style "margin" "0.25rem 0 0.75rem" ]
                [ text "Run to end to enable time-travel scrubbing." ]

        _ ->
            let
                maxTick =
                    model.checkpoints
                        |> List.reverse
                        |> List.head
                        |> Maybe.map (\cp -> eventTime2Int cp.clock)
                        |> Maybe.withDefault 0

                currentTick =
                    eventTime2Int model.simState.clock
            in
            div [ style "margin-bottom" "0.75rem" ]
                [ div [ style "font-size" "0.8em", style "color" "#636e72", style "margin-bottom" "0.2rem" ]
                    [ text ("t = " ++ String.fromInt currentTick ++ " / " ++ String.fromInt maxTick) ]
                , input
                    [ type_ "range"
                    , HA.min "0"
                    , HA.max (String.fromInt maxTick)
                    , value (String.fromInt currentTick)
                    , onInput ScrubTo
                    , style "width" "760px"
                    , style "display" "block"
                    , style "cursor" "pointer"
                    ]
                    []
                ]


-- ── Sparkline history ─────────────────────────────────────────────────────────
-- Hidden during Playing to avoid scanning the full event log at 60 fps.

viewHistory : Model -> Html msg
viewHistory model =
    case model.playback of
        Playing _ ->
            text ""

        _ ->
            let
                tl = Metrics.computeTimelines model.simState
                m  = Metrics.compute model.simState
            in
            if tl.totalTicks == 0 then
                text ""
            else
                let
                    workerStrips =
                        workerIdsForPreset model.topologyPreset
                            |> List.concatMap
                                (\( lbl, nid ) ->
                                    [ sparkLabel (lbl ++ " busy / idle")
                                    , busyStrip nid tl
                                    ]
                                )

                    queueCharts =
                        workerQueueIdsForPreset model.topologyPreset
                            |> List.concatMap
                                (\( lbl, qid ) ->
                                    [ sparkLabel ("Q" ++ String.fromInt qid ++ " length" ++
                                        (if lbl == "Q" ++ String.fromInt qid then "" else "  " ++ lbl))
                                    , queueChart qid model.simState tl
                                    ]
                                )
                in
                div [ style "margin-bottom" "1rem" ]
                    ([ h2 [] [ text "History" ] ]
                     ++ workerStrips
                     ++ queueCharts
                     ++ [ sparkLabel "Cycle time distribution  (p50 = green  p95 = orange)"
                        , cycleHistogram m
                        ]
                    )


sparkLabel : String -> Html msg
sparkLabel s =
    div [ style "font-size" "0.75em", style "color" "#636e72", style "margin-top" "0.4rem" ]
        [ text s ]


busyStrip : Int -> Metrics.Timelines -> Html msg
busyStrip nid tl =
    let
        w = 760.0
        h = 18.0

        segs =
            Dict.get nid tl.nodeBusy |> Maybe.withDefault []

        sx t =
            toFloat t / toFloat (max 1 tl.totalTicks) * w

        busyRects =
            List.map
                (\s ->
                    Svg.rect
                        [ SA.x      (String.fromFloat (sx s.from))
                        , SA.y      "0"
                        , SA.width  (String.fromFloat (max 1 (sx s.to - sx s.from)))
                        , SA.height (String.fromFloat h)
                        , SA.fill   "#00b894"
                        , SA.opacity "0.85"
                        ]
                        []
                )
                segs
    in
    Svg.svg
        [ SA.viewBox ("0 0 " ++ String.fromFloat w ++ " " ++ String.fromFloat h)
        , SA.width (String.fromFloat w)
        , SA.height (String.fromFloat h)
        , style "display" "block"
        , style "border" "1px solid #e0e0e0"
        ]
        (Svg.rect
            [ SA.x "0", SA.y "0"
            , SA.width (String.fromFloat w), SA.height (String.fromFloat h)
            , SA.fill "#f0f0f0"
            ]
            []
            :: busyRects
        )


queueChart : Int -> SimState -> Metrics.Timelines -> Html msg
queueChart qid state tl =
    let
        w = 760.0
        h = 36.0

        cap =
            SimState.getQueue (QueueID qid) state
                |> Maybe.map Queue.capacity
                |> Maybe.withDefault 1

        steps =
            Dict.get qid tl.queueLength |> Maybe.withDefault []

        pts =
            stepPolyline w h tl.totalTicks cap steps
    in
    Svg.svg
        [ SA.viewBox ("0 0 " ++ String.fromFloat w ++ " " ++ String.fromFloat h)
        , SA.width (String.fromFloat w)
        , SA.height (String.fromFloat h)
        , style "display" "block"
        , style "border" "1px solid #e0e0e0"
        ]
        [ Svg.rect
            [ SA.x "0", SA.y "0"
            , SA.width (String.fromFloat w), SA.height (String.fromFloat h)
            , SA.fill "#f8f9fa"
            ]
            []
        , Svg.polyline
            [ SA.points pts
            , SA.fill "#74b9ff"
            , SA.fillOpacity "0.35"
            , SA.stroke "#0984e3"
            , SA.strokeWidth "1.5"
            , SA.strokeLinejoin "miter"
            ]
            []
        ]


-- Build a closed step-function polyline (suitable for filled area rendering).
-- Points run: bottom-left → step function top → bottom-right → close.
stepPolyline : Float -> Float -> Int -> Int -> List Metrics.QueueStep -> String
stepPolyline w h totalTicks cap steps =
    let
        sx t   = toFloat t / toFloat (max 1 totalTicks) * w
        sy len = h - toFloat len / toFloat (max 1 cap) * h

        ( _, innerPts ) =
            List.foldl
                (\step ( prevLen, acc ) ->
                    ( step.len
                    , acc
                        ++ [ ( sx step.t, sy prevLen )
                           , ( sx step.t, sy step.len )
                           ]
                    )
                )
                ( 0, [] )
                steps

        lastLen =
            List.reverse steps |> List.head |> Maybe.map .len |> Maybe.withDefault 0

        allPts =
            [ ( 0, h ), ( 0, sy 0 ) ]
                ++ innerPts
                ++ [ ( w, sy lastLen ), ( w, h ) ]
    in
    allPts
        |> List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat y)
        |> String.join " "


cycleHistogram : Metrics.SystemMetrics -> Html msg
cycleHistogram m =
    if List.isEmpty m.cycleTimes then
        p [ style "color" "#aaa", style "font-size" "0.8em" ] [ text "No completed jobs yet." ]

    else
        let
            cts    = m.cycleTimes
            minCT  = List.minimum cts |> Maybe.withDefault 0
            maxCT  = List.maximum cts |> Maybe.withDefault 1
            range  = max 1 (maxCT - minCT)
            nBins  = min 30 (List.length cts) |> max 1
            binW   = max 1 (ceiling (toFloat range / toFloat nBins))
            nBins2 = ceiling (toFloat range / toFloat binW) + 1

            bins =
                List.foldl
                    (\ct acc ->
                        let
                            i = min (nBins2 - 1) ((ct - minCT) // binW)
                        in
                        Dict.update i (Just << (+) 1 << Maybe.withDefault 0) acc
                    )
                    Dict.empty
                    cts

            maxCount = Dict.values bins |> List.maximum |> Maybe.withDefault 1

            svgW = 760.0
            svgH = 80.0
            bw   = svgW / toFloat nBins2

            bars =
                List.range 0 (nBins2 - 1)
                    |> List.map
                        (\i ->
                            let
                                count = Dict.get i bins |> Maybe.withDefault 0
                                bh    = toFloat count / toFloat maxCount * svgH
                            in
                            Svg.rect
                                [ SA.x      (String.fromFloat (toFloat i * bw + 0.5))
                                , SA.y      (String.fromFloat (svgH - bh))
                                , SA.width  (String.fromFloat (max 1 (bw - 1)))
                                , SA.height (String.fromFloat (max 0 bh))
                                , SA.fill "#74b9ff"
                                , SA.rx "1"
                                ]
                                []
                        )

            pMarker val colour lbl =
                let
                    x = clamp 0 svgW ((val - toFloat minCT) / toFloat range * svgW)
                in
                [ Svg.line
                    [ SA.x1 (String.fromFloat x), SA.y1 "0"
                    , SA.x2 (String.fromFloat x), SA.y2 (String.fromFloat svgH)
                    , SA.stroke colour, SA.strokeWidth "1.5"
                    , SA.strokeDasharray "4,3"
                    ]
                    []
                , Svg.text_
                    [ SA.x (String.fromFloat (x + 3)), SA.y "11"
                    , SA.fontSize "9", SA.fill colour
                    ]
                    [ text lbl ]
                ]

            xTick t anchor =
                Svg.text_
                    [ SA.x (String.fromFloat (clamp 0 svgW (toFloat (t - minCT) / toFloat range * svgW)))
                    , SA.y (String.fromFloat (svgH + 12))
                    , SA.textAnchor anchor, SA.fontSize "9", SA.fill "#636e72"
                    ]
                    [ text (String.fromInt t ++ " t") ]
        in
        Svg.svg
            [ SA.viewBox ("0 0 760 " ++ String.fromFloat (svgH + 15))
            , SA.width "760"
            , SA.height (String.fromFloat (svgH + 15))
            , style "display" "block"
            , style "border" "1px solid #e0e0e0"
            , style "background" "#f8f9fa"
            ]
            (bars
                ++ pMarker m.p50CycleTime "#00b894" "p50"
                ++ pMarker m.p95CycleTime "#e17055" "p95"
                ++ [ xTick minCT "start", xTick maxCT "end" ]
            )


viewEventLog : SimState -> Html msg
viewEventLog state =
    div []
        [ h2 [] [ text "Event log (newest first)" ]
        , ul
            [ style "max-height" "300px"
            , style "overflow-y" "auto"
            , style "border" "1px solid #ccc"
            , style "padding" "0.5rem"
            ]
            (state.eventLog |> List.map viewEvent)
        , h2 [] [ text "Pending events" ]
        , ul [] (state.eventQueue |> List.map viewEvent)
        ]


viewEvent : Event.Event -> Html msg
viewEvent evt =
    li []
        [ span [ style "color" "#888" ]
            [ text ("t=" ++ String.fromInt (eventTime2Int evt.time) ++ " ") ]
        , text (describeEvent evt.kind)
        ]


describeEvent : EventType -> String
describeEvent kind =
    case kind of
        JobArrived (NodeID nid) jid ->
            "JobArrived  node=" ++ String.fromInt nid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        ServiceStarted (NodeID nid) jid ->
            "ServiceStarted  node=" ++ String.fromInt nid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        ServiceComplete (NodeID nid) jid ->
            "ServiceComplete  node=" ++ String.fromInt nid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        JobEnqueued (QueueID qid) jid ->
            "JobEnqueued  queue=" ++ String.fromInt qid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        JobDequeued (QueueID qid) (NodeID nid) jid ->
            "JobDequeued  queue=" ++ String.fromInt qid ++ " → node=" ++ String.fromInt nid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        JobBlocked (QueueID qid) jid ->
            "JobBlocked  queue=" ++ String.fromInt qid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        JobDropped (QueueID qid) jid ->
            "JobDropped  queue=" ++ String.fromInt qid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        SignoffRequested (NodeID nid) _ jid ->
            "SignoffRequested  node=" ++ String.fromInt nid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        SignoffStarted (NodeID nid) _ jid ->
            "SignoffStarted  node=" ++ String.fromInt nid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        SignoffComplete (NodeID nid) _ jid ->
            "SignoffComplete  node=" ++ String.fromInt nid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        ServicePreempted (NodeID nid) jid ->
            "ServicePreempted  node=" ++ String.fromInt nid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        InterruptStarted ->
            "InterruptStarted"

        InterruptEnded ->
            "InterruptEnded"

        WorkerHalted (NodeID nid) ->
            "WorkerHalted  node=" ++ String.fromInt nid

        WorkerResumed (NodeID nid) ->
            "WorkerResumed  node=" ++ String.fromInt nid


-- ── Scenario parameter panel ──────────────────────────────────────────────────

viewScenarioPanel : Model -> Html Msg
viewScenarioPanel model =
    div
        [ style "border" "1px solid #ccc"
        , style "border-radius" "4px"
        , style "padding" "0.6rem 1rem"
        , style "margin-bottom" "1rem"
        , style "background" "#fafafa"
        ]
        [ div [ style "font-size" "0.8em", style "color" "#636e72", style "margin-bottom" "0.4rem" ]
            [ text "Scenario (applied on Reset or Apply)" ]
        , div [ style "display" "flex", style "align-items" "center", style "gap" "1rem", style "flex-wrap" "wrap"
              , style "margin-bottom" "0.4rem" ]
            [ span [] [ text "Topology:" ]
            , select
                [ value (presetToString model.topologyPreset)
                , onInput (\s -> SetTopologyPreset (presetFromString s))
                , style "font-family" "monospace"
                ]
                [ option [ value "single"   ] [ text (presetLabel SingleWorker)  ]
                , option [ value "parallel" ] [ text (presetLabel TwoParallel)   ]
                , option [ value "pipeline" ] [ text (presetLabel ThreePipeline) ]
                ]
            ]
        , div [ style "display" "flex", style "align-items" "center", style "gap" "1rem", style "flex-wrap" "wrap" ]
            [ labelledInput "Arrival rate"    model.cfgArrivalRate      SetCfgArrivalRate
            , viewSTSelector model
            , labelledInput "Q capacity"      model.cfgQueueCap         SetCfgQueueCap
            , labelledInput "% high priority" model.cfgPctHighPriority   SetCfgPctHighPriority
            , if model.topologyPreset == TwoParallel then viewDispatchRuleSelector model else text ""
            , btn ApplyScenario "Apply"
            ]
        ]


presetToString : TopologyPreset -> String
presetToString p =
    case p of
        SingleWorker  -> "single"
        TwoParallel   -> "parallel"
        ThreePipeline -> "pipeline"


presetFromString : String -> TopologyPreset
presetFromString s =
    case s of
        "single"   -> SingleWorker
        "parallel" -> TwoParallel
        _          -> ThreePipeline


viewDispatchRuleSelector : Model -> Html Msg
viewDispatchRuleSelector model =
    span [ style "display" "inline-flex", style "align-items" "center", style "gap" "0.4rem" ]
        [ span [] [ text "Dispatch:" ]
        , select
            [ value model.cfgDispatchRule
            , onInput SetCfgDispatchRule
            , style "font-family" "monospace"
            ]
            [ option [ value "shortest"   ] [ text "Shortest queue" ]
            , option [ value "roundrobin" ] [ text "Round robin" ]
            ]
        ]


viewSTSelector : Model -> Html Msg
viewSTSelector model =
    let
        stTypes =
            [ "Exponential", "LogNormal", "Erlang", "Deterministic", "Uniform" ]

        p1Lbl = stParam1Label model.cfgSTType
        p2Lbl = stParam2Label model.cfgSTType
        hasP2 = stHasParam2 model.cfgSTType
    in
    span [ style "display" "inline-flex", style "align-items" "center", style "gap" "0.4rem" ]
        ([ span [] [ text "Service time:" ]
         , select
            [ value model.cfgSTType
            , onInput SetCfgSTType
            , style "font-family" "monospace"
            ]
            (List.map
                (\t -> option [ value t, HA.selected (t == model.cfgSTType) ] [ text t ])
                stTypes
            )
         , span [] [ text (p1Lbl ++ ":") ]
         , input [ type_ "text", value model.cfgParam1, onInput SetCfgParam1, style "width" "4rem", style "font-family" "monospace" ] []
         ]
            ++ (if hasP2 then
                    [ span [] [ text (p2Lbl ++ ":") ]
                    , input [ type_ "text", value model.cfgParam2, onInput SetCfgParam2, style "width" "4rem", style "font-family" "monospace" ] []
                    ]
                else
                    []
               )
        )


stParam1Label : String -> String
stParam1Label stype =
    case stype of
        "Exponential"   -> "λ"
        "LogNormal"     -> "μ"
        "Erlang"        -> "k"
        "Deterministic" -> "ticks"
        "Uniform"       -> "min"
        _               -> "p1"


stParam2Label : String -> String
stParam2Label stype =
    case stype of
        "LogNormal" -> "σ"
        "Erlang"    -> "λ"
        "Uniform"   -> "max"
        _           -> "p2"


stHasParam2 : String -> Bool
stHasParam2 stype =
    List.member stype [ "LogNormal", "Erlang", "Uniform" ]


-- ── Ensemble panel ────────────────────────────────────────────────────────────

viewEnsemblePanel : Model -> Html Msg
viewEnsemblePanel model =
    div [ style "margin-top" "2rem", style "border-top" "1px solid #ccc", style "padding-top" "1rem" ]
        [ h2 [] [ text "Ensemble / Monte Carlo" ]
        , div [ style "margin-bottom" "0.75rem" ]
            [ labelledInput "Replicas" (String.fromInt model.ensembleReplicas) SetReplicas
            , labelledInput "Duration (ticks)" (String.fromInt model.ensembleDuration) SetDuration
            , btn RunEnsemble ("Run " ++ String.fromInt model.ensembleReplicas ++ " replicas")
            ]
        , case model.ensembleResult of
            Nothing ->
                p [ style "color" "#888" ] [ text "No ensemble run yet." ]

            Just stats ->
                viewEnsembleStats stats
        ]


labelledInput : String -> String -> (String -> Msg) -> Html Msg
labelledInput label val toMsg =
    span [ style "margin-right" "1rem" ]
        [ span [ style "margin-right" "0.25rem" ] [ text (label ++ ":") ]
        , input
            [ type_ "number"
            , value val
            , onInput toMsg
            , style "width" "5rem"
            , style "font-family" "monospace"
            ]
            []
        ]


viewEnsembleStats : EnsembleStats -> Html msg
viewEnsembleStats stats =
    div []
        [ p [] [ text (String.fromInt stats.n ++ " replicas × " ++ String.fromInt stats.duration ++ " ticks each") ]
        , h3 [] [ text "System" ]
        , distTable
            [ ( "Throughput (jobs/tick)", stats.throughput )
            , ( "Avg cycle time (ticks)", stats.avgCycleTime )
            , ( "p95 cycle time (ticks)", stats.p95CycleTime )
            ]
        , h3 [] [ text "Worker utilisation" ]
        , distTable
            (stats.utilisation
                |> Dict.toList
                |> List.map (\( nid, d ) -> ( "Node " ++ String.fromInt nid, d ))
            )
        , h3 [] [ text "Queue avg length" ]
        , distTable
            (stats.avgQueueLen
                |> Dict.toList
                |> List.map (\( qid, d ) -> ( "Q" ++ String.fromInt qid, d ))
            )
        , h3 [] [ text "Queue drop count" ]
        , distTable
            (stats.dropCount
                |> Dict.toList
                |> List.map (\( qid, d ) -> ( "Q" ++ String.fromInt qid, d ))
            )
        ]


distTable : List ( String, Ensemble.Distribution ) -> Html msg
distTable rows =
    table
        [ style "border-collapse" "collapse"
        , style "margin-bottom" "1rem"
        , style "font-size" "0.9em"
        ]
        (tableHeader :: List.map distRow rows)


tableHeader : Html msg
tableHeader =
    tr []
        [ th [ cellStyle, style "text-align" "left" ] [ text "Metric" ]
        , th [ cellStyle ] [ text "mean" ]
        , th [ cellStyle ] [ text "std dev" ]
        , th [ cellStyle ] [ text "p05" ]
        , th [ cellStyle ] [ text "p50" ]
        , th [ cellStyle ] [ text "p95" ]
        , th [ cellStyle ] [ text "min" ]
        , th [ cellStyle ] [ text "max" ]
        ]


distRow : ( String, Ensemble.Distribution ) -> Html msg
distRow ( label, d ) =
    tr []
        [ td [ cellStyle, style "text-align" "left" ] [ text label ]
        , td [ cellStyle ] [ text (fmt d.mean) ]
        , td [ cellStyle ] [ text (fmt d.stdDev) ]
        , td [ cellStyle ] [ text (fmt d.p05) ]
        , td [ cellStyle ] [ text (fmt d.p50) ]
        , td [ cellStyle ] [ text (fmt d.p95) ]
        , td [ cellStyle ] [ text (fmt d.min) ]
        , td [ cellStyle ] [ text (fmt d.max) ]
        ]


cellStyle : Html.Attribute msg
cellStyle =
    style "padding" "0.2rem 0.6rem"


fmt : Float -> String
fmt f =
    String.fromFloat (toFloat (round (f * 100)) / 100)


-- ── Entry point ───────────────────────────────────────────────────────────────

main : Program () Model Msg
main =
    Browser.element
        { init          = init
        , update        = update
        , view          = view
        , subscriptions = subscriptions
        }
