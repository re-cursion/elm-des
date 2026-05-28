module Main exposing (main)

import Browser
import Browser.Events
import Dict
import Engine
import Ensemble exposing (EnsembleStats)
import Event exposing (EventType(..))
import EventTime exposing (EventTime(..), addTimes, eventTime2Int)
import Html exposing (Html, button, div, h2, h3, input, li, p, span, table, td, text, th, tr, ul)
import Html.Attributes as HA exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Id exposing (NodeID(..), QueueID(..))
import Job exposing (Priority(..))
import Metrics
import Node exposing (NodeKind(..), NodeState(..), makeSource, makeSink, makeWorker)
import Queue
import Random
import ServiceTime exposing (ServiceTime(..))
import SimState exposing (SimState)
import Svg
import Svg.Attributes as SA
import Topology exposing (Topology)


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
            { arrivalRate = 0.3, jobPriority = Normal, jobLabel = "Job" }

        workerCfg =
            { serviceTime = LogNormal 1.2 0.4, preemptive = False, signoff = Nothing }

        q =
            Queue.empty { capacity = 5, discipline = Queue.FIFO, overflow = Queue.Block }

        ( firstJobID, state0 ) =
            SimState.nextJobID (SimState.init (Random.initialSeed 42))

        state =
            state0
                |> SimState.putNode (NodeID 1) (makeSource "Source" sourceCfg)
                |> SimState.putNode (NodeID 2) (makeWorker "Worker" workerCfg)
                |> SimState.putNode (NodeID 3) (makeSink "Sink")
                |> SimState.putQueue (QueueID 1) q
                |> SimState.putQueue (QueueID 2) q
                |> SimState.scheduleEvent
                    (Event.event (EventTime 0) (JobArrived (NodeID 1) firstJobID))
    in
    ( topo, state )


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


-- ── Model ─────────────────────────────────────────────────────────────────────

type alias Model =
    { topo             : Topology
    , simState         : SimState
    , playback         : PlaybackMode
    , accumulator      : Float
    , checkpoints      : List SimState    -- oldest-first; populated by RunToEnd
    , ensembleReplicas : Int
    , ensembleDuration : Int
    , ensembleResult   : Maybe EnsembleStats
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( topo, simState ) =
            scenario
    in
    ( { topo             = topo
      , simState         = simState
      , playback         = Stopped
      , accumulator      = 0
      , checkpoints      = []
      , ensembleReplicas = 100
      , ensembleDuration = 1000
      , ensembleResult   = Nothing
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
              }
            , Cmd.none
            )

        Reset ->
            let
                ( topo, simState ) =
                    scenario
            in
            ( { model
                | topo        = topo
                , simState    = simState
                , checkpoints = []
                , playback    = Stopped
                , accumulator = 0
              }
            , Cmd.none
            )

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
                    ( { model | simState = newState, playback = Stopped }, Cmd.none )

        SetPlayback mode ->
            ( { model | playback = mode, accumulator = 0 }, Cmd.none )

        Tick ->
            case model.playback of
                Stopped ->
                    ( model, Cmd.none )

                Playing n ->
                    if n >= maxAnimatedSpeed then
                        ( { model | simState = Engine.drainAll model.topo model.simState }
                        , Cmd.none
                        )

                    else
                        let
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
                        in
                        ( { model
                            | simState    = newState
                            , accumulator = newAcc - toFloat simUnits
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
                ( topo, initState ) =
                    scenario

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
        , viewPlaybackControls model.playback
        , viewClock model.simState
        , viewCanvas model.simState
        , viewScrubber model
        , viewLiveMetrics model.simState
        , viewHistory model
        , viewEventLog model.simState
        , viewEnsemblePanel model
        ]


viewPlaybackControls : PlaybackMode -> Html Msg
viewPlaybackControls current =
    div [ style "margin-bottom" "1rem" ]
        (List.map (speedBtn current) speeds
            ++ [ separator
               , btn Step     "Step"
               , btn RunToEnd "Run to end"
               , btn Reset    "Reset"
               ]
        )


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


-- ── 2D SVG Canvas ─────────────────────────────────────────────────────────────
-- Hardcoded layout: Source(1) → [Q1] → Worker(2) → [Q2] → Sink(3)
-- Canvas 760 × 220.  All coords in SVG user units.

viewCanvas : SimState -> Html msg
viewCanvas state =
    let
        metrics =
            Metrics.compute state
    in
    div [ style "margin-bottom" "1rem" ]
        [ Svg.svg
            [ SA.viewBox "0 0 760 220"
            , SA.width "760"
            , SA.height "220"
            , style "border" "1px solid #ccc"
            , style "border-radius" "4px"
            , style "background" "#f8f9fa"
            , style "display" "block"
            ]
            [ svgDefs
            , svgEdge 91 110 108 110
            , svgEdge 268 110 285 110
            , svgEdge 415 110 432 110
            , svgEdge 592 110 609 110
            , svgQueueNode 1 state metrics 108 86 160 48
            , svgQueueNode 2 state metrics 432 86 160 48
            , svgCircleNode 1 "Source" "#74b9ff" "#0984e3" state 65 110 26
            , svgWorkerNode state metrics
            , svgCircleNode 3 "Sink"   "#55efc4" "#00b894" state 635 110 26
            ]
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


svgWorkerNode : SimState -> Metrics.SystemMetrics -> Svg.Svg msg
svgWorkerNode state metrics =
    let
        nx  = 285
        ny  = 76
        nw  = 130
        nh  = 68
        bh  = 10   -- utilisation bar height at bottom

        mNode = SimState.getNode (NodeID 2) state
        lbl   = mNode |> Maybe.map .label |> Maybe.withDefault "Worker"
        ns    = mNode |> Maybe.map .state |> Maybe.withDefault Idle

        bodyColor =
            case ns of
                Idle          -> "#dfe6e9"
                Busy _ _      -> "#fdcb6e"
                Blocked _     -> "#d63031"
                Signoff _ _   -> "#a29bfe"
                Preempted _ _ -> "#fd79a8"
                Paused _      -> "#b2bec3"

        stateStr =
            case ns of
                Idle          -> "idle"
                Busy _ _      -> "busy"
                Blocked _     -> "blocked"
                Signoff _ _   -> "signoff"
                Preempted _ _ -> "preempted"
                Paused _      -> "paused"

        util =
            Dict.get 2 metrics.nodes
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


priorityColor : Priority -> String
priorityColor p =
    case p of
        Low      -> "#b2bec3"
        Normal   -> "#74b9ff"
        High     -> "#fdcb6e"
        Critical -> "#d63031"


-- ── Live metrics strip ────────────────────────────────────────────────────────

viewLiveMetrics : SimState -> Html msg
viewLiveMetrics state =
    let
        m = Metrics.compute state
        n = List.length m.cycleTimes
        workerUtil =
            Dict.get 2 m.nodes |> Maybe.map .utilisation |> Maybe.withDefault 0.0
    in
    div
        [ style "display" "flex"
        , style "gap" "2rem"
        , style "padding" "0.4rem 0"
        , style "font-size" "0.85em"
        , style "border-bottom" "1px solid #eee"
        , style "margin-bottom" "1rem"
        ]
        [ metricCell "Throughput"  (fmt m.throughput ++ " /tick")
        , metricCell "Avg cycle"   (fmt m.avgCycleTime ++ " ticks")
        , metricCell "p95 cycle"   (fmt m.p95CycleTime ++ " ticks")
        , metricCell "Worker util" (fmtPct workerUtil)
        , metricCell "Completed"   (String.fromInt n ++ " jobs")
        ]


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
                div [ style "margin-bottom" "1rem" ]
                    [ h2 [] [ text "History" ]
                    , sparkLabel "Worker busy / idle"
                    , busyStrip tl
                    , sparkLabel "Q1 length"
                    , queueChart 1 model.simState tl
                    , sparkLabel "Q2 length"
                    , queueChart 2 model.simState tl
                    , sparkLabel "Cycle time distribution  (p50 = green  p95 = orange)"
                    , cycleHistogram m
                    ]


sparkLabel : String -> Html msg
sparkLabel s =
    div [ style "font-size" "0.75em", style "color" "#636e72", style "margin-top" "0.4rem" ]
        [ text s ]


busyStrip : Metrics.Timelines -> Html msg
busyStrip tl =
    let
        w = 760.0
        h = 18.0

        segs =
            Dict.get 2 tl.nodeBusy |> Maybe.withDefault []

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

        MeetingStarted ->
            "MeetingStarted"

        MeetingEnded ->
            "MeetingEnded"


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
