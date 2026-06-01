module Des.Scenario exposing
    ( Model
    , Msg(..)
    , PlaybackMode(..)
    , RenderMode(..)
    , init
    , update
    , view
    , subscriptions
    )

{-| Self-contained simulation component.

elm-presentation wires it like any Elm sub-component:

    type alias PresentationModel =
        { scenarios : Dict SlideId Des.Scenario.Model }

    Des.Scenario.init cfg               -- ( Model, Cmd Msg )
    Des.Scenario.update msg model       -- ( Model, Cmd Msg )
    Html.map ScenarioMsg (Des.Scenario.view model)
    Des.Scenario.subscriptions model    -- Sub Msg
-}

import Browser.Events
import Camera exposing (Camera)
import IsoRenderer
import Random
import Dict exposing (Dict)
import Engine
import Event exposing (EventType(..))
import EventTime exposing (EventTime(..), addTimes, eventTime2Int)
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes as HA exposing (style, type_, value)
import Html.Events exposing (onClick, onInput, on, preventDefaultOn)
import Json.Decode as JD
import Id exposing (JobID(..), NodeID(..), QueueID(..))
import Job exposing (Priority(..))
import Metrics
import Node exposing (NodeKind(..), NodeState(..))
import Queue
import ScenarioConfig exposing (ScenarioConfig, NodeSpecKind(..))
import SimState exposing (SimState)
import Svg
import Svg.Attributes as SA
import Topology exposing (Topology)


-- ── Types ─────────────────────────────────────────────────────────────────────

type PlaybackMode
    = Stopped
    | Playing Float   -- simulated time-units per real second


type RenderMode
    = Flat2D
    | Isometric


type alias JobAnim =
    { x         : Float
    , y         : Float
    , tx        : Float
    , ty        : Float
    , waypoints : List ( Float, Float )
    , priority  : Priority
    }


type alias Layout =
    { nodes  : Dict Int ( Float, Float )
    , queues : Dict Int ( Float, Float )
    , svgW   : Float
    , svgH   : Float
    }


type alias Model =
    { config      : ScenarioConfig
    , layout      : Layout
    , topo        : Topology
    , simState    : SimState
    , playback    : PlaybackMode
    , renderMode  : RenderMode
    , camera      : Camera
    , drag        : Maybe { x : Float, y : Float }
    , accumulator : Float
    , checkpoints : List SimState
    , jobAnims    : Dict Int JobAnim
    }


-- ── Public Messages ───────────────────────────────────────────────────────────

type Msg
    = Step
    | RunToEnd
    | Reset
    | SetSpeed Float
    | TriggerInterrupt
    | SetInterruptDuration String
    | ScrubTo String
    | Tick
    | ToggleRenderMode
    | SpinCamera Float
    | TiltCamera Float
    | ZoomCamera Float
    | ResetCamera
    | DragStart Float Float
    | DragMove Float Float
    | DragEnd
    | WheelZoom Float


-- ── Init ──────────────────────────────────────────────────────────────────────

init : ScenarioConfig -> ( Model, Cmd Msg )
init cfg =
    let
        ( topo, simState ) =
            ScenarioConfig.toTopologyAndState cfg
                |> Result.withDefault
                    ( Topology.empty, SimState.init (Random.initialSeed cfg.meta.seed) )
    in
    ( { config      = cfg
      , layout      = buildLayout cfg
      , topo        = topo
      , simState    = simState
      , playback    = Stopped
      , renderMode  = Flat2D
      , camera      = isoCamera (buildLayout cfg)
      , drag        = Nothing
      , accumulator = 0
      , checkpoints = []
      , jobAnims    = Dict.empty
      }
    , Cmd.none
    )


isoCamera : Layout -> Camera
isoCamera layout =
    { spinAngle = 0
    , tiltAngle = pi / 6
    , scale     = 48.0
    , origin    = ( layout.svgW / 2, layout.svgH / 2 )
    }


buildLayout : ScenarioConfig -> Layout
buildLayout cfg =
    let
        nodePos =
            cfg.nodes
                |> List.map (\n -> ( n.id, ( n.x, n.y ) ))
                |> Dict.fromList

        queuePos =
            cfg.queues
                |> List.map (\q -> ( q.id, ( q.x, q.y ) ))
                |> Dict.fromList

        allX =
            List.map .x cfg.nodes ++ List.map .x cfg.queues

        allY =
            List.map .y cfg.nodes ++ List.map .y cfg.queues

        maxX =
            List.maximum allX |> Maybe.withDefault 600

        maxY =
            List.maximum allY |> Maybe.withDefault 200
    in
    { nodes  = nodePos
    , queues = queuePos
    , svgW   = maxX + 80
    , svgH   = maxY + 80
    }


-- ── Update ────────────────────────────────────────────────────────────────────

maxAnimatedSpeed : Float
maxAnimatedSpeed = 500.0


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
                ( topo, simState ) =
                    ScenarioConfig.toTopologyAndState model.config
                        |> Result.withDefault ( model.topo, model.simState )

                layout =
                    model.layout
            in
            ( { model
                | topo        = topo
                , simState    = simState
                , checkpoints = []
                , playback    = Stopped
                , renderMode  = Flat2D
                , camera      = isoCamera layout
                , drag        = Nothing
                , accumulator = 0
                , jobAnims    = Dict.empty
              }
            , Cmd.none
            )

        SetSpeed n ->
            ( { model
                | playback    = if n <= 0 then Stopped else Playing n
                , accumulator = 0
              }
            , Cmd.none
            )

        TriggerInterrupt ->
            if SimState.isInterruptActive model.simState then
                ( model, Cmd.none )

            else
                let
                    t  = model.simState.clock
                    t2 = addTimes t (EventTime 20)
                    newState =
                        model.simState
                            |> SimState.scheduleEvent (Event.event t  InterruptStarted)
                            |> SimState.scheduleEvent (Event.event t2 InterruptEnded)
                in
                ( { model | simState = newState }, Cmd.none )

        SetInterruptDuration _ ->
            ( model, Cmd.none )

        ToggleRenderMode ->
            let
                next = case model.renderMode of
                    Flat2D    -> Isometric
                    Isometric -> Flat2D
            in
            ( { model | renderMode = next }, Cmd.none )

        SpinCamera delta ->
            ( { model | camera = Camera.spin delta model.camera }, Cmd.none )

        TiltCamera delta ->
            ( { model | camera = Camera.tilt delta model.camera }, Cmd.none )

        ZoomCamera factor ->
            ( { model | camera = Camera.zoom factor model.camera }, Cmd.none )

        ResetCamera ->
            ( { model | camera = isoCamera model.layout }, Cmd.none )

        DragStart x y ->
            ( { model | drag = Just { x = x, y = y } }, Cmd.none )

        DragMove x y ->
            case model.drag of
                Nothing ->
                    ( model, Cmd.none )

                Just last ->
                    let
                        dx = x - last.x
                        dy = y - last.y
                    in
                    ( { model
                        | camera =
                            model.camera
                                |> Camera.spin (dx * (pi / 300))
                                |> Camera.tilt (-dy * (pi / 500))
                        , drag = Just { x = x, y = y }
                      }
                    , Cmd.none
                    )

        DragEnd ->
            ( { model | drag = Nothing }, Cmd.none )

        WheelZoom deltaY ->
            let
                factor =
                    clamp 0.9 1.1 (1.0 - deltaY * 0.001)
            in
            ( { model | camera = Camera.zoom factor model.camera }, Cmd.none )

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
                    ( { model | simState = newState, playback = Stopped, jobAnims = Dict.empty }
                    , Cmd.none
                    )

        Tick ->
            case model.playback of
                Stopped ->
                    ( model, Cmd.none )

                Playing n ->
                    if n >= maxAnimatedSpeed then
                        -- Advance a large but bounded window per frame so the JS
                        -- thread is never blocked long enough to swallow input events.
                        let
                            fastDeadline =
                                addTimes model.simState.clock (EventTime 5000)

                            fastState =
                                Engine.advanceUntil fastDeadline model.topo model.simState
                        in
                        ( { model
                            | simState    = fastState
                            , accumulator = 0
                            , jobAnims    = Dict.empty
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

                            -- Subtract only the clock ticks actually consumed, not the
                            -- requested simUnits. If no event fell within the deadline the
                            -- clock stays put; keeping the remainder lets the accumulator
                            -- grow across frames until it covers the gap to the next event.
                            clockDelta =
                                eventTime2Int newState.clock - eventTime2Int model.simState.clock

                            newEventsChron =
                                if clockDelta > 0 then
                                    List.take
                                        (List.length newState.eventLog - oldLogLen)
                                        newState.eventLog
                                        |> List.reverse
                                else
                                    []

                            newAnims =
                                stepJobAnims (1.0 / 60.0) n model.layout newState newEventsChron model.jobAnims
                        in
                        ( { model
                            | simState    = newState
                            , accumulator = newAcc - toFloat clockDelta
                            , jobAnims    = newAnims
                          }
                        , Cmd.none
                        )


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


-- ── Subscriptions ─────────────────────────────────────────────────────────────

clientXY : (Float -> Float -> Msg) -> JD.Decoder Msg
clientXY toMsg =
    JD.map2 toMsg
        (JD.field "clientX" JD.float)
        (JD.field "clientY" JD.float)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.playback of
            Playing _ -> Browser.Events.onAnimationFrame (\_ -> Tick)
            Stopped   -> Sub.none
        , case model.drag of
            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove (clientXY DragMove)
                    , Browser.Events.onMouseUp   (JD.succeed DragEnd)
                    ]
            Nothing ->
                Sub.none
        ]


-- ── View ──────────────────────────────────────────────────────────────────────

view : Model -> Html Msg
view model =
    div [ style "font-family" "monospace" ]
        [ viewControls model
        , viewCanvas model
        , viewMetrics model
        , viewScrubber model
        ]


{-| Compact system dashboard: completed jobs, throughput, cycle-time
percentiles, total drops, and per-worker utilisation. Pure function of the
metrics (which are a single-pass scan of the event log). -}
viewMetrics : Model -> Html Msg
viewMetrics model =
    let
        m = Metrics.compute model.simState
        isExp = model.config.meta.theme == "expanse"

        completed = List.length m.jobs
        dropped = Dict.foldl (\_ q acc -> acc + q.dropCount) 0 m.queues

        chipBg     = if isExp then "#161b22" else "#f0f0f0"
        chipBorder = if isExp then "#30363d" else "#ccc"
        valColor   = if isExp then "#e6edf3" else "#222"
        lblColor   = if isExp then "#8b949e" else "#666"

        roundTo d x =
            let f = toFloat (10 ^ d)
            in String.fromFloat (toFloat (round (x * f)) / f)

        chip lbl val =
            div
                [ style "padding" "0.3rem 0.6rem"
                , style "background" chipBg
                , style "border" ("1px solid " ++ chipBorder)
                , style "border-radius" "4px"
                , style "min-width" "62px"
                ]
                [ div [ style "font-size" "0.62rem", style "color" lblColor, style "text-transform" "uppercase", style "letter-spacing" "0.5px" ] [ text lbl ]
                , div [ style "font-size" "0.95rem", style "font-weight" "bold", style "color" valColor ] [ text val ]
                ]

        utilChips =
            model.config.nodes
                |> List.filterMap
                    (\n ->
                        case n.kind of
                            WorkerSpec _ ->
                                let u = Dict.get n.id m.nodes |> Maybe.map .utilisation |> Maybe.withDefault 0.0
                                in Just (chip n.label (String.fromInt (round (u * 100)) ++ "%"))

                            _ ->
                                Nothing
                    )
    in
    div [ style "display" "flex", style "gap" "0.4rem", style "flex-wrap" "wrap", style "margin-top" "0.5rem" ]
        ([ chip "Completed" (String.fromInt completed)
         , chip "Thrpt/t" (roundTo 3 m.throughput)
         , chip "Avg cyc" (roundTo 1 m.avgCycleTime)
         , chip "p95 cyc" (roundTo 1 m.p95CycleTime)
         , chip "Dropped" (String.fromInt dropped)
         ]
            ++ utilChips
        )


viewControls : Model -> Html Msg
viewControls model =
    let
        speeds =
            [ ( "Pause", 0 )
            , ( "×1",    1 )
            , ( "×10",   10 )
            , ( "×100",  100 )
            , ( "Max",   maxAnimatedSpeed )
            ]

        currentN =
            case model.playback of
                Stopped   -> 0
                Playing n -> n

        speedBtn ( lbl, n ) =
            let
                active = currentN == n
            in
            button
                [ onClick (SetSpeed n)
                , style "margin-right" "0.25rem"
                , style "padding" "0.25rem 0.75rem"
                , style "background" (if active then "#333" else "#eee")
                , style "color" (if active then "#fff" else "#333")
                , style "border" "1px solid #999"
                , style "cursor" "pointer"
                ]
                [ text lbl ]

        interruptCtrl =
            if SimState.isInterruptActive model.simState then
                span [ style "color" "#d63031", style "font-weight" "bold", style "font-size" "0.85em" ]
                    [ text "⚡ Workers interrupted" ]

            else
                button
                    [ onClick TriggerInterrupt
                    , style "padding" "0.25rem 0.75rem"
                    , style "border" "1px solid #999"
                    , style "cursor" "pointer"
                    , style "margin-right" "0.25rem"
                    ]
                    [ text "Interrupt" ]

        isoActive = model.renderMode == Isometric
    in
    div [ style "margin-bottom" "0.5rem" ]
        (List.map speedBtn speeds
            ++ [ span [ style "margin" "0 0.5rem" ] [ text "|" ]
               , button
                    [ onClick Step,     style "margin-right" "0.25rem", style "padding" "0.25rem 0.75rem", style "border" "1px solid #999", style "cursor" "pointer" ]
                    [ text "Step" ]
               , button
                    [ onClick RunToEnd, style "margin-right" "0.25rem", style "padding" "0.25rem 0.75rem", style "border" "1px solid #999", style "cursor" "pointer" ]
                    [ text "Run to end" ]
               , button
                    [ onClick Reset,    style "margin-right" "0.5rem",  style "padding" "0.25rem 0.75rem", style "border" "1px solid #999", style "cursor" "pointer" ]
                    [ text "Reset" ]
               , span [ style "margin" "0 0.5rem" ] [ text "|" ]
               , interruptCtrl
               , span
                    [ style "margin-left" "1rem", style "color" "#888", style "font-size" "0.85em" ]
                    [ text ("t = " ++ String.fromInt (eventTime2Int model.simState.clock)) ]
               , span [ style "margin" "0 0.5rem" ] [ text "|" ]
               , button
                    [ onClick ToggleRenderMode
                    , style "padding" "0.25rem 0.75rem"
                    , style "border" "1px solid #999"
                    , style "cursor" "pointer"
                    , style "background" (if isoActive then "#333" else "#eee")
                    , style "color" (if isoActive then "#fff" else "#333")
                    ]
                    [ text (if isoActive then "Iso" else "2D") ]
               ]
        )


viewScrubber : Model -> Html Msg
viewScrubber model =
    case model.checkpoints of
        [] ->
            text ""

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
            div [ style "margin-top" "0.4rem" ]
                [ input
                    [ type_ "range"
                    , HA.min "0"
                    , HA.max (String.fromInt maxTick)
                    , value (String.fromInt currentTick)
                    , onInput ScrubTo
                    , style "width" (String.fromFloat model.layout.svgW ++ "px")
                    , style "display" "block"
                    , style "cursor" "pointer"
                    ]
                    []
                ]


-- ── SVG canvas ────────────────────────────────────────────────────────────────

viewCanvas : Model -> Html Msg
viewCanvas model =
    let
        metrics = Metrics.compute model.simState
        w = model.layout.svgW
        h = model.layout.svgH
    in
    case model.renderMode of
        Flat2D ->
            let
                theme2d  = model.config.meta.theme
                isExp    = theme2d == "expanse"
                bgColor  = if isExp then "#0d1117" else "#f8f9fa"
                bdrColor = if isExp then "#1a2634" else "#ccc"
            in
            div [ style "margin-bottom" "0.5rem" ]
                [ Svg.svg
                    [ SA.viewBox ("0 0 " ++ String.fromFloat w ++ " " ++ String.fromFloat h)
                    , SA.width (String.fromFloat w)
                    , SA.height (String.fromFloat h)
                    , style "border" ("1px solid " ++ bdrColor)
                    , style "border-radius" "4px"
                    , style "background" bgColor
                    , style "display" "block"
                    ]
                    (svgDefs
                        :: svgEdges theme2d model.config model.layout
                        ++ svgQueues theme2d model.config model.simState metrics model.layout
                        ++ svgNodes theme2d model.config model.simState metrics model.layout
                        ++ viewTransitDots model.jobAnims
                    )
                ]

        Isometric ->
            let
                isoW = w * 2
                isoH = h * 2
                baseCam = model.camera
                cam0 = { baseCam | origin = ( 0, 0 ) }
                ( projCx, projCy ) =
                    Camera.project cam0
                        { x = (w / 2) / 48.0, y = 0, z = (h / 2) / 48.0 }
                cam = { baseCam | origin = ( isoW / 2 - projCx, isoH / 2 - projCy ) }
                jobPositions =
                    Dict.map
                        (\_ a ->
                            let
                                dx = a.tx - a.x
                                dy = a.ty - a.y
                            in
                            { x        = a.x
                            , y        = a.y
                            , tx       = a.tx
                            , ty       = a.ty
                            , inTransit = dx * dx + dy * dy > 4.0 || not (List.isEmpty a.waypoints)
                            , priority  = a.priority
                            }
                        )
                        model.jobAnims
            in
            div [ style "margin-bottom" "0.5rem" ]
                [ viewCameraControls model.camera
                , Svg.svg
                    [ SA.viewBox ("0 0 " ++ String.fromFloat isoW ++ " " ++ String.fromFloat isoH)
                    , SA.width (String.fromFloat isoW)
                    , SA.height (String.fromFloat isoH)
                    , style "border" "1px solid #444"
                    , style "border-radius" "4px"
                    , style "background" "#0f0f1a"
                    , style "display" "block"
                    , style "cursor" (if model.drag /= Nothing then "grabbing" else "grab")
                    , style "user-select" "none"
                    , on "mousedown" (clientXY DragStart)
                    , on "dblclick" (JD.succeed ResetCamera)
                    , preventDefaultOn "wheel"
                        (JD.map (\d -> ( WheelZoom d, True ))
                            (JD.field "deltaY" JD.float)
                        )
                    ]
                    [ IsoRenderer.viewStarfield cam isoW isoH
                    , IsoRenderer.viewScene cam model.config model.simState metrics jobPositions
                    , IsoRenderer.viewAlert (SimState.isInterruptActive model.simState) isoW
                    ]
                ]


viewCameraControls : Camera -> Html Msg
viewCameraControls cam =
    let
        btn msg lbl =
            button
                [ onClick msg
                , style "margin-right" "0.25rem"
                , style "padding" "0.2rem 0.6rem"
                , style "border" "1px solid #999"
                , style "cursor" "pointer"
                , style "background" "#eee"
                ]
                [ text lbl ]
    in
    div [ style "margin-bottom" "0.25rem", style "font-size" "0.8em" ]
        [ btn (SpinCamera (-pi / 8)) "◀ spin"
        , btn (SpinCamera  (pi / 8)) "spin ▶"
        , span [ style "margin" "0 0.4rem" ] [ text "|" ]
        , btn (TiltCamera  0.05) "tilt ▲"
        , btn (TiltCamera -0.05) "tilt ▼"
        , span [ style "margin" "0 0.4rem" ] [ text "|" ]
        , btn (ZoomCamera 1.25) "zoom +"
        , btn (ZoomCamera 0.8)  "zoom −"
        , span [ style "margin" "0 0.4rem" ] [ text "|" ]
        , btn ResetCamera "reset"
        , span
            [ style "margin-left" "0.75rem", style "color" "#888" ]
            [ text
                ("spin=" ++ String.fromInt (round (cam.spinAngle * 180 / pi))
                ++ "° tilt=" ++ String.fromInt (round (cam.tiltAngle * 180 / pi))
                ++ "° zoom=" ++ String.fromInt (round cam.scale)
                )
            ]
        ]


svgDefs : Svg.Svg msg
svgDefs =
    Svg.defs []
        [ Svg.marker
            [ SA.id "des-arr"
            , SA.markerWidth "8"
            , SA.markerHeight "8"
            , SA.refX "7"
            , SA.refY "3"
            , SA.orient "auto"
            ]
            [ Svg.path [ SA.d "M0,0 L0,6 L8,3 z", SA.fill "#888" ] [] ]
        ]


-- ── Edges ─────────────────────────────────────────────────────────────────────

svgEdges : String -> ScenarioConfig -> Layout -> List (Svg.Svg msg)
svgEdges theme cfg layout =
    List.filterMap (svgEdge theme layout) cfg.edges


svgEdge : String -> Layout -> ScenarioConfig.EdgeSpec -> Maybe (Svg.Svg msg)
svgEdge theme layout edge =
    let
        lookupPos s =
            case String.split ":" s of
                [ "node",  n ] -> String.toInt n |> Maybe.andThen (\i -> Dict.get i layout.nodes)
                [ "queue", n ] -> String.toInt n |> Maybe.andThen (\i -> Dict.get i layout.queues)
                _              -> Nothing
        edgeColor = if theme == "expanse" then "#546E7A" else "#888"
    in
    Maybe.map2
        (\( x1, y1 ) ( x2, y2 ) ->
            Svg.line
                [ SA.x1 (String.fromFloat x1)
                , SA.y1 (String.fromFloat y1)
                , SA.x2 (String.fromFloat x2)
                , SA.y2 (String.fromFloat y2)
                , SA.stroke edgeColor
                , SA.strokeWidth "2"
                , SA.markerEnd "url(#des-arr)"
                ]
                []
        )
        (lookupPos edge.from)
        (lookupPos edge.to)


-- ── Queue nodes ───────────────────────────────────────────────────────────────

svgQueues : String -> ScenarioConfig -> SimState -> Metrics.SystemMetrics -> Layout -> List (Svg.Svg msg)
svgQueues theme cfg state metrics layout =
    List.filterMap (svgQueue theme state metrics layout) cfg.queues


svgQueue : String -> SimState -> Metrics.SystemMetrics -> Layout -> ScenarioConfig.QueueSpec -> Maybe (Svg.Svg msg)
svgQueue theme state metrics layout qspec =
    Dict.get qspec.id layout.queues
        |> Maybe.map
            (\( cx, cy ) ->
                let
                    qw = 80.0
                    qh = 36.0
                    qx = cx - qw / 2
                    qy = cy - qh / 2

                    mQ   = SimState.getQueue (QueueID qspec.id) state
                    sz   = mQ |> Maybe.map Queue.size     |> Maybe.withDefault 0
                    cap  = mQ |> Maybe.map Queue.capacity |> Maybe.withDefault 1
                    jobs = mQ |> Maybe.map Queue.toList   |> Maybe.withDefault []

                    frac      = if cap > 0 then toFloat sz / toFloat cap else 0.0
                    fillColor = if frac >= 1.0 then "#d63031"
                                else if theme == "expanse" then "#00838F"
                                else "#74b9ff"

                    drops =
                        Dict.get qspec.id metrics.queues
                            |> Maybe.map .dropCount
                            |> Maybe.withDefault 0

                    displayCap  = min cap 8
                    slotSpacing = qw / toFloat (displayCap + 1)
                    slotR       = min 6.0 (slotSpacing / 2.5)
                    slotY       = qy + qh * 0.65

                    slots =
                        List.take displayCap jobs
                            |> List.indexedMap
                                (\i job ->
                                    let
                                        slotCx = qx + slotSpacing * toFloat (i + 1)
                                    in
                                    Svg.circle
                                        [ SA.cx (String.fromFloat slotCx)
                                        , SA.cy (String.fromFloat slotY)
                                        , SA.r  (String.fromFloat slotR)
                                        , SA.fill (priorityColor job.priority)
                                        , SA.stroke "#fff"
                                        , SA.strokeWidth "1"
                                        ]
                                        []
                                )

                    dropBadge =
                        if drops > 0 then
                            [ Svg.text_
                                [ SA.x (String.fromFloat (qx + qw - 3))
                                , SA.y (String.fromFloat (qy + 10))
                                , SA.textAnchor "end"
                                , SA.fontSize "9"
                                , SA.fill "#d63031"
                                ]
                                [ text ("▼" ++ String.fromInt drops) ]
                            ]
                        else
                            []
                in
                let
                    isExp   = theme == "expanse"
                    qBg     = if isExp then "#1E2D3D" else "#dfe6e9"
                    qBorder = if isExp then "#263238" else "#b2bec3"
                    qTxt    = if isExp then "#90A4AE" else "#636e72"
                in
                Svg.g []
                    ([ Svg.rect
                        [ SA.x (String.fromFloat qx)
                        , SA.y (String.fromFloat qy)
                        , SA.width  (String.fromFloat qw)
                        , SA.height (String.fromFloat qh)
                        , SA.rx "4"
                        , SA.fill qBg
                        , SA.stroke qBorder
                        , SA.strokeWidth "1"
                        ]
                        []
                     , Svg.rect
                        [ SA.x (String.fromFloat qx)
                        , SA.y (String.fromFloat qy)
                        , SA.width  (String.fromFloat (frac * qw))
                        , SA.height (String.fromFloat qh)
                        , SA.rx "4"
                        , SA.fill fillColor
                        , SA.opacity "0.25"
                        ]
                        []
                     , Svg.text_
                        [ SA.x (String.fromFloat cx)
                        , SA.y (String.fromFloat (qy + 12))
                        , SA.textAnchor "middle"
                        , SA.fontSize "9"
                        , SA.fontWeight "bold"
                        , SA.fill qTxt
                        ]
                        [ text qspec.label ]
                     ]
                        ++ slots
                        ++ dropBadge
                    )
            )


-- ── Sim nodes ─────────────────────────────────────────────────────────────────

svgNodes : String -> ScenarioConfig -> SimState -> Metrics.SystemMetrics -> Layout -> List (Svg.Svg msg)
svgNodes theme cfg state metrics layout =
    List.filterMap (svgSimNode theme state metrics layout) cfg.nodes


svgSimNode : String -> SimState -> Metrics.SystemMetrics -> Layout -> ScenarioConfig.NodeSpec -> Maybe (Svg.Svg msg)
svgSimNode theme state metrics layout nspec =
    Dict.get nspec.id layout.nodes
        |> Maybe.map
            (\( cx, cy ) ->
                let isExp = theme == "expanse"
                in
                case nspec.kind of
                    SourceSpec _ ->
                        if isExp then
                            svgCircle cx cy 22 "#006064" "#00838F" "#E0F7FA" nspec.label
                        else
                            svgCircle cx cy 22 "#74b9ff" "#0984e3" "#2d3436" nspec.label

                    SinkSpec ->
                        if isExp then
                            svgCircle cx cy 22 "#4A148C" "#6A1B9A" "#EDE7F6" nspec.label
                        else
                            svgCircle cx cy 22 "#55efc4" "#00b894" "#2d3436" nspec.label

                    InterruptSpec ->
                        svgInterruptBadge theme nspec.label cx cy (SimState.isInterruptActive state)

                    DispatcherSpec _ ->
                        svgDispatcher theme cx cy nspec.label

                    WorkerSpec _ ->
                        svgWorker theme nspec.id cx cy state metrics
            )


svgCircle : Float -> Float -> Float -> String -> String -> String -> String -> Svg.Svg msg
svgCircle cx cy r fill stroke txtFill lbl =
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
            , SA.y (String.fromFloat (cy + 4))
            , SA.textAnchor "middle"
            , SA.fontSize "10"
            , SA.fill txtFill
            ]
            [ text lbl ]
        ]


svgDispatcher : String -> Float -> Float -> String -> Svg.Svg msg
svgDispatcher theme cx cy lbl =
    let
        dw   = 72.0
        dh   = 36.0
        dx   = cx - dw / 2
        dy   = cy - dh / 2
        skew = 8.0
        pts  =
            String.join " "
                [ String.fromFloat (dx + skew) ++ "," ++ String.fromFloat dy
                , String.fromFloat (dx + dw)   ++ "," ++ String.fromFloat dy
                , String.fromFloat (dx + dw - skew) ++ "," ++ String.fromFloat (dy + dh)
                , String.fromFloat dx           ++ "," ++ String.fromFloat (dy + dh)
                ]
        isExp = theme == "expanse"
        dFill   = if isExp then "#E65100" else "#a29bfe"
        dStroke = if isExp then "#BF360C" else "#6c5ce7"
        dText   = if isExp then "#FFF8E1" else "#2d3436"
    in
    Svg.g []
        [ Svg.polygon
            [ SA.points pts
            , SA.fill dFill
            , SA.stroke dStroke
            , SA.strokeWidth "1.5"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat cx)
            , SA.y (String.fromFloat (cy + 4))
            , SA.textAnchor "middle"
            , SA.fontSize "10"
            , SA.fontWeight "bold"
            , SA.fill dText
            ]
            [ text lbl ]
        ]


svgWorker : String -> Int -> Float -> Float -> SimState -> Metrics.SystemMetrics -> Svg.Svg msg
svgWorker theme nid cx cy state metrics =
    let
        nw = 100.0
        nh = 44.0
        bh = 7.0
        nx = cx - nw / 2
        ny = cy - nh / 2

        mNode = SimState.getNode (NodeID nid) state
        lbl   = mNode |> Maybe.map .label |> Maybe.withDefault "Worker"
        ns    = mNode |> Maybe.map .state |> Maybe.withDefault Idle

        halted =
            case mNode of
                Nothing -> False
                Just n  ->
                    case n.kind of
                        Worker cfg -> cfg.halted
                        _          -> False

        isExp = theme == "expanse"

        bodyColor =
            if halted then (if isExp then "#263238" else "#b2bec3")
            else case ns of
                Idle          -> if isExp then "#1E2D3D" else "#dfe6e9"
                Busy _ _      -> if isExp then "#BF360C" else "#fdcb6e"
                Blocked _     -> if isExp then "#7F0000" else "#d63031"
                Signoff _ _   -> if isExp then "#4A148C" else "#a29bfe"
                Preempted _ _ -> if isExp then "#880E4F" else "#fd79a8"
                Paused _      -> if isExp then "#263238" else "#b2bec3"

        wBorder  = if isExp then "#37474F" else "#636e72"
        wText    = if isExp then "#CFD8DC" else "#2d3436"
        wSubText = if isExp then "#78909C" else "#636e72"
        wUtil    = if isExp then "#00838F" else "#00b894"

        stateStr =
            if halted then "halted"
            else case ns of
                Idle          -> "idle"
                Busy _ _      -> "busy"
                Blocked _     -> "blocked"
                Signoff _ _   -> "signoff"
                Preempted _ _ -> "preempted"
                Paused _      -> "paused"

        util  = Dict.get nid metrics.nodes |> Maybe.map .utilisation |> Maybe.withDefault 0.0
        utilW = util * nw
        bodyH = nh - bh

        jobDot =
            case ns of
                Busy jid _ ->
                    case SimState.getJob jid state of
                        Just job ->
                            [ Svg.circle
                                [ SA.cx (String.fromFloat (nx + nw - 10))
                                , SA.cy (String.fromFloat (ny + 10))
                                , SA.r "7"
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
            , SA.width  (String.fromFloat nw)
            , SA.height (String.fromFloat bodyH)
            , SA.rx "5"
            , SA.fill bodyColor
            , SA.stroke wBorder
            , SA.strokeWidth "1.5"
            ]
            []
         , Svg.rect
            [ SA.x (String.fromFloat nx)
            , SA.y (String.fromFloat (ny + bodyH))
            , SA.width  (String.fromFloat nw)
            , SA.height (String.fromFloat bh)
            , SA.rx "5"
            , SA.fill wBorder
            ]
            []
         , Svg.rect
            [ SA.x (String.fromFloat nx)
            , SA.y (String.fromFloat (ny + bodyH))
            , SA.width  (String.fromFloat utilW)
            , SA.height (String.fromFloat bh)
            , SA.fill wUtil
            ]
            []
         , Svg.text_
            [ SA.x (String.fromFloat cx)
            , SA.y (String.fromFloat (ny + bodyH / 2 - 3))
            , SA.textAnchor "middle"
            , SA.fontSize "11"
            , SA.fontWeight "bold"
            , SA.fill wText
            ]
            [ text lbl ]
         , Svg.text_
            [ SA.x (String.fromFloat cx)
            , SA.y (String.fromFloat (ny + bodyH / 2 + 10))
            , SA.textAnchor "middle"
            , SA.fontSize "9"
            , SA.fill wSubText
            ]
            [ text stateStr ]
         , Svg.text_
            [ SA.x (String.fromFloat cx)
            , SA.y (String.fromFloat (ny + nh + 13))
            , SA.textAnchor "middle"
            , SA.fontSize "9"
            , SA.fill wUtil
            ]
            [ text (String.fromInt (round (util * 100)) ++ "%") ]
         ]
            ++ jobDot
        )


svgInterruptBadge : String -> String -> Float -> Float -> Bool -> Svg.Svg msg
svgInterruptBadge theme nodeLbl cx cy active =
    let
        bw      = 80.0
        bh      = 20.0
        bx      = cx - bw / 2
        by      = cy - bh / 2
        isExp   = theme == "expanse"
        bgColor = if active then (if isExp then "#B71C1C" else "#d63031")
                  else (if isExp then "#1E2D3D" else "#dfe6e9")
        fgColor = if active then "#fff"
                  else (if isExp then "#546E7A" else "#636e72")
        lbl     = if active then ("⚡ " ++ String.toUpper nodeLbl) else nodeLbl
        bdrCol  = if isExp then (if active then "#7F0000" else "#263238") else (if active then "#c0392b" else "#b2bec3")
    in
    Svg.g []
        [ Svg.rect
            [ SA.x (String.fromFloat bx)
            , SA.y (String.fromFloat by)
            , SA.width  (String.fromFloat bw)
            , SA.height (String.fromFloat bh)
            , SA.rx "4"
            , SA.fill bgColor
            , SA.stroke bdrCol
            , SA.strokeWidth "1"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat cx)
            , SA.y (String.fromFloat (cy + 4))
            , SA.textAnchor "middle"
            , SA.fontSize "9"
            , SA.fontWeight "bold"
            , SA.fill fgColor
            ]
            [ text lbl ]
        ]


-- ── Job transit dots ──────────────────────────────────────────────────────────

animUnitsPerSec : Float
animUnitsPerSec = 400.0


stepJobAnims : Float -> Float -> Layout -> SimState -> List Event.Event -> Dict Int JobAnim -> Dict Int JobAnim
stepJobAnims dt speed layout state newEventsChron anims =
    let
        withTargets =
            List.foldl (applyEventToAnim layout state) anims newEventsChron

        budget =
            animUnitsPerSec * dt * max 1.0 speed
    in
    Dict.map (\_ a -> advanceAnim budget a) withTargets


advanceAnim : Float -> JobAnim -> JobAnim
advanceAnim budget a =
    let
        dx   = a.tx - a.x
        dy   = a.ty - a.y
        dist = sqrt (dx * dx + dy * dy)
    in
    if dist <= budget then
        case a.waypoints of
            [] ->
                { a | x = a.tx, y = a.ty }

            ( nx, ny ) :: rest ->
                advanceAnim (budget - dist)
                    { a | x = a.tx, y = a.ty, tx = nx, ty = ny, waypoints = rest }

    else
        { a | x = a.x + dx / dist * budget, y = a.y + dy / dist * budget }


applyEventToAnim : Layout -> SimState -> Event.Event -> Dict Int JobAnim -> Dict Int JobAnim
applyEventToAnim layout state evt anims =
    let
        nodePos nid =
            Dict.get nid layout.nodes |> Maybe.withDefault ( 0, 0 )

        queuePos qid =
            Dict.get qid layout.queues |> Maybe.withDefault ( 0, 0 )

        firstNodeId =
            Dict.keys layout.nodes |> List.minimum |> Maybe.withDefault 1

        sinkNodeId =
            Dict.keys layout.nodes |> List.maximum |> Maybe.withDefault 99
    in
    case evt.kind of
        JobArrived (NodeID nid) (JobID jid) ->
            if nid == firstNodeId then
                let
                    prio     = SimState.getJob (JobID jid) state |> Maybe.map .priority |> Maybe.withDefault Normal
                    ( x, y ) = nodePos nid
                in
                Dict.insert jid { x = x, y = y, tx = x, ty = y, waypoints = [], priority = prio } anims

            else if nid == sinkNodeId then
                Dict.remove jid anims

            else
                anims

        JobEnqueued (QueueID qid) (JobID jid) ->
            let
                wp = queuePos qid
            in
            Dict.update jid (Maybe.map (\a -> { a | waypoints = a.waypoints ++ [ wp ] })) anims

        JobDequeued (QueueID _) (NodeID nid) (JobID jid) ->
            let
                wp = nodePos nid
            in
            Dict.update jid (Maybe.map (\a -> { a | waypoints = a.waypoints ++ [ wp ] })) anims

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
                if dx * dx + dy * dy > 4.0 || not (List.isEmpty a.waypoints) then
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


priorityColor : Priority -> String
priorityColor p =
    case p of
        Low      -> "#b2bec3"
        Normal   -> "#74b9ff"
        High     -> "#fdcb6e"
        Critical -> "#d63031"
