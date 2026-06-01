module IsoRenderer exposing
    ( viewScene
    , viewStarfield
    , viewAlert
    , JobIsoData
    , nodeToObjects
    , queueToObjects
    , edgeToObjects
    )

{-| Converts simulation state + ScenarioConfig into SceneObjects, then renders
them via SceneObject.renderAll. Wired into Des.Scenario when isometric
rendering is requested.
-}

import Camera exposing (Camera)
import Dict
import Id exposing (NodeID(..), QueueID(..))
import Job exposing (Priority(..))
import Metrics exposing (NodeMetrics, QueueMetrics, SystemMetrics)
import Node exposing (NodeState(..))
import Queue
import ScenarioConfig exposing (EdgeSpec, NodeSpec, QueueSpec, ScenarioConfig)
import SceneObject exposing (BoxStyle, SceneObject, SceneShape(..), renderAll)
import SimState exposing (SimState)
import Svg exposing (Svg)
import Svg.Attributes as SA


-- ── Types ─────────────────────────────────────────────────────────────────────

type alias JobIsoData =
    { x        : Float
    , y        : Float
    , tx       : Float
    , ty       : Float
    , inTransit : Bool
    , priority  : Priority
    }


-- ── Scene entry point ─────────────────────────────────────────────────────────

viewScene
    : Camera
    -> ScenarioConfig
    -> SimState
    -> SystemMetrics
    -> Dict.Dict Int JobIsoData
    -> Svg msg
viewScene cam cfg state metrics jobPositions =
    let
        theme = cfg.meta.theme

        nodeObjects =
            cfg.nodes
                |> List.concatMap
                    (\spec ->
                        let
                            nm = Dict.get spec.id metrics.nodes |> Maybe.withDefault emptyNodeMetrics
                            ns = SimState.getNode (NodeID spec.id) state |> Maybe.map .state
                        in
                        nodeToObjects theme spec nm ns
                    )

        queueObjects =
            cfg.queues
                |> List.concatMap
                    (\spec ->
                        let
                            qm    = Dict.get spec.id metrics.queues |> Maybe.withDefault emptyQueueMetrics
                            prios = SimState.getQueue (QueueID spec.id) state
                                        |> Maybe.map (\q -> List.map .priority (Queue.toList q))
                                        |> Maybe.withDefault []
                        in
                        queueToObjects theme spec prios qm
                    )

        edgeObjects =
            cfg.edges |> List.concatMap (edgeToObjects cfg)

        jobDots =
            Dict.values jobPositions
                |> List.concatMap
                    (\job ->
                        let
                            wx   = job.x / 48.0
                            wz   = job.y / 48.0
                            dotY = if job.inTransit then 1.2 else 0.6
                            ( sx, sy )   = Camera.project cam { x = wx, y = dotY, z = wz }
                            ( shx, shy ) = Camera.project cam { x = wx, y = 0.02, z = wz }
                            -- Heading: project a look-ahead point (the current
                            -- target) and take the screen-space bearing so the
                            -- ship banks toward where it is actually flying.
                            ( tsx, tsy ) = Camera.project cam { x = job.tx / 48.0, y = dotY, z = job.ty / 48.0 }
                            dsx = tsx - sx
                            dsy = tsy - sy
                            heading =
                                if dsx * dsx + dsy * dsy > 1.0 then
                                    atan2 dsy dsx * 180.0 / pi + 90.0
                                else
                                    0.0
                        in
                        [ Svg.ellipse
                            [ SA.cx (String.fromFloat shx)
                            , SA.cy (String.fromFloat shy)
                            , SA.rx "7"
                            , SA.ry "3"
                            , SA.fill "#000"
                            , SA.opacity "0.22"
                            ]
                            []
                        , jobDot theme sx sy heading job.priority
                        ]
                    )

        allObjects =
            edgeObjects ++ nodeObjects ++ queueObjects

        nodeLabels =
            cfg.nodes
                |> List.map (\spec ->
                    let nm = Dict.get spec.id metrics.nodes |> Maybe.withDefault emptyNodeMetrics
                    in nodeLabel cam spec nm state
                )

        queueLabels =
            cfg.queues
                |> List.map (\spec ->
                    let
                        len = SimState.getQueue (QueueID spec.id) state
                                |> Maybe.map Queue.size
                                |> Maybe.withDefault 0
                        drops = Dict.get spec.id metrics.queues
                                |> Maybe.map .dropCount
                                |> Maybe.withDefault 0
                    in
                    queueLabel cam spec len drops
                )
    in
    Svg.g []
        ( renderAll cam [ floorSlab cfg ]
            ++ renderAll cam (floorTiles cfg)
            ++ renderAll cam allObjects
            ++ jobDots
            ++ nodeLabels
            ++ queueLabels
        )


-- ── Floor ─────────────────────────────────────────────────────────────────────

{-| Extent of the floor in integer world-tile coordinates, padded by a margin
around the scene's nodes and queues. -}
floorBounds : ScenarioConfig -> { x0 : Int, z0 : Int, x1 : Int, z1 : Int }
floorBounds cfg =
    let
        allX = List.map .x cfg.nodes ++ List.map .x cfg.queues
        allZ = List.map .y cfg.nodes ++ List.map .y cfg.queues

        minWX = (List.minimum allX |> Maybe.withDefault 0)  / 48.0
        maxWX = (List.maximum allX |> Maybe.withDefault 480) / 48.0
        minWZ = (List.minimum allZ |> Maybe.withDefault 0)  / 48.0
        maxWZ = (List.maximum allZ |> Maybe.withDefault 240) / 48.0
    in
    { x0 = floor (minWX - 1.5)
    , z0 = floor (minWZ - 1.5)
    , x1 = ceiling (maxWX + 1.5)
    , z1 = ceiling (maxWZ + 1.5)
    }


{-| A single thick slab spanning the whole floor; gives the ground visible
edge thickness without drawing four walls per tile. -}
floorSlab : ScenarioConfig -> SceneObject msg
floorSlab cfg =
    let
        b  = floorBounds cfg
        w  = toFloat (b.x1 - b.x0)
        d  = toFloat (b.z1 - b.z0)
        cx = (toFloat b.x0 + toFloat b.x1) / 2
        cz = (toFloat b.z0 + toFloat b.z1) / 2
        isExpanse = cfg.meta.theme == "expanse"
        style =
            if isExpanse then
                { topColour = "#0A1422", leftColour = "#060D16", rightColour = "#04090F", w = w, d = d }
            else
                { topColour = "#2B3A42", leftColour = "#1C2930", rightColour = "#141E24", w = w, d = d }
    in
    { pos = { x = cx, y = -0.15, z = cz }, height = 0.15, shape = Box style }


{-| Flat checkerboard quads on the slab's top surface. Each is one polygon
(no walls), so the whole grid is cheap. -}
floorTiles : ScenarioConfig -> List (SceneObject msg)
floorTiles cfg =
    let
        b = floorBounds cfg
        cols = List.range b.x0 (b.x1 - 1)
        rows = List.range b.z0 (b.z1 - 1)
        isExpanse = cfg.meta.theme == "expanse"
        ( fillA, fillB ) =
            if isExpanse then ( "#0D1B2A", "#111F2E" ) else ( "#37474F", "#2E3C44" )
        seam =
            if isExpanse then "#060E18" else "#26333A"
    in
    List.concatMap
        (\col ->
            List.map
                (\row ->
                    { pos    = { x = toFloat col + 0.5, y = 0.001, z = toFloat row + 0.5 }
                    , height = 0.0
                    , shape  = FlatTile
                        { w = 0.96, d = 0.96
                        , fill = if modBy 2 (col + row) == 0 then fillA else fillB
                        , stroke = seam
                        }
                    }
                )
                rows
        )
        cols


-- ── Node → SceneObjects ───────────────────────────────────────────────────────

nodeToObjects : String -> NodeSpec -> NodeMetrics -> Maybe NodeState -> List (SceneObject msg)
nodeToObjects theme spec metrics mState =
    let
        pos = { x = spec.x / 48.0, y = 0.0, z = spec.y / 48.0 }
        h   = spec.h
        u   = metrics.utilisation

        baseStyle = nodeBoxStyle theme spec u
        nodeW = baseStyle.w

        -- During a bosmang all-hands the worker downs tools: render the box
        -- dark and lifeless so the interrupt is unmistakable in the iso view.
        isPaused =
            case mState of
                Just (Paused _) -> True
                _               -> False

        style =
            if isPaused then
                { topColour = "#263238", leftColour = "#1A2429", rightColour = "#121A1E"
                , w = baseStyle.w, d = baseStyle.d
                }
            else
                baseStyle

        node =
            { pos    = pos
            , height = h
            , shape  = Box style
            }

        -- Dock berths get a landing pad that reads free (green) vs occupied
        -- (amber), so "wait until a dock is free" is visible at a glance.
        isDock =
            case spec.kind of
                ScenarioConfig.WorkerSpec _ -> True
                _                           -> False

        pad =
            if isDock then
                let
                    ( padColour, padRim ) =
                        case mState of
                            Just Idle       -> ( "#1B5E20", "#43A047" )   -- free: green
                            Nothing         -> ( "#1B5E20", "#43A047" )
                            Just (Paused _) -> ( "#3E2723", "#5D4037" )   -- meeting: dim
                            _               -> ( "#B26A00", "#FFB300" )   -- occupied: amber
                in
                [ { pos    = { x = pos.x, y = 0.004, z = pos.z }
                  , height = 0.0
                  , shape  = FlatTile { w = 1.4, d = 1.4, fill = padColour, stroke = padRim }
                  }
                ]
            else
                []

        -- A small beacon mast on the back corner whose colour signals live
        -- state (busy / blocked / signoff / preempted / paused).
        beacon =
            case Maybe.andThen beaconColour mState of
                Just c ->
                    [ { pos    = { x = pos.x + nodeW / 2.0 - 0.12, y = h, z = pos.z - nodeW / 2.0 + 0.12 }
                      , height = 0.3
                      , shape  = Box
                            { topColour = c, leftColour = c, rightColour = c
                            , w = 0.1, d = 0.1
                            }
                      }
                    ]

                Nothing ->
                    []

        -- Left-anchored gauge: the fill grows rightward from the node's left
        -- edge (pos.x - nodeW/2), so it reads as a 0→100% bar rather than a
        -- slab that expands symmetrically from the centre.
        fillW = max 0.04 (u * nodeW)

        utilBar =
            { pos    = { x = pos.x - nodeW / 2.0 + fillW / 2.0, y = h + 0.02, z = pos.z }
            , height = 0.06
            , shape  = Box
                { topColour   = utilColour u
                , leftColour  = utilColour u
                , rightColour = utilColour u
                , w           = fillW
                , d           = 0.2
                }
            }
    in
    pad ++ [ node, utilBar ] ++ beacon


{-| State-signalling beacon colour for a worker node. `Idle` (and non-worker
states) get no beacon. -}
beaconColour : NodeState -> Maybe String
beaconColour st =
    case st of
        Busy _ _      -> Just "#FFD54F"   -- working: warm amber
        Blocked _     -> Just "#EF5350"   -- output blocked: red
        Signoff _ _   -> Just "#AB47BC"   -- awaiting sign-off: purple
        Preempted _ _ -> Just "#FF7043"   -- preempted: orange
        Paused _      -> Just "#B71C1C"   -- bosmang meeting: deep red alert
        Idle          -> Nothing


nodeBoxStyle : String -> NodeSpec -> Float -> BoxStyle
nodeBoxStyle theme spec u =
    case spec.kind of
        ScenarioConfig.SourceSpec _ ->
            if theme == "expanse" then
                { topColour = "#00838F", leftColour = "#006064", rightColour = "#004D40", w = 1.0, d = 1.0 }
            else
                { topColour = "#4CAF50", leftColour = "#388E3C", rightColour = "#2E7D32", w = 1.0, d = 1.0 }

        ScenarioConfig.SinkSpec ->
            if theme == "expanse" then
                { topColour = "#6A1B9A", leftColour = "#4A148C", rightColour = "#2E0064", w = 1.0, d = 1.0 }
            else
                { topColour = "#9E9E9E", leftColour = "#757575", rightColour = "#616161", w = 1.0, d = 1.0 }

        ScenarioConfig.WorkerSpec _ ->
            if theme == "expanse" then
                let
                    ( top, left, right ) =
                        if u > 0.8 then ( "#FF7043", "#E64A19", "#BF360C" )
                        else if u > 0.5 then ( "#78909C", "#546E7A", "#37474F" )
                        else ( "#546E7A", "#37474F", "#263238" )
                in
                { topColour = top, leftColour = left, rightColour = right, w = 1.0, d = 1.0 }
            else
                let top = if u > 0.8 then "#EF5350" else if u > 0.5 then "#FFA726" else "#42A5F5"
                in { topColour = top, leftColour = darken top, rightColour = darken2 top, w = 1.0, d = 1.0 }

        ScenarioConfig.DispatcherSpec _ ->
            if theme == "expanse" then
                { topColour = "#FF8F00", leftColour = "#E65100", rightColour = "#BF360C", w = 1.0, d = 1.0 }
            else
                { topColour = "#AB47BC", leftColour = "#7B1FA2", rightColour = "#6A1B9A", w = 1.0, d = 1.0 }

        ScenarioConfig.InterruptSpec ->
            if theme == "expanse" then
                { topColour = "#B71C1C", leftColour = "#7F0000", rightColour = "#5D0000", w = 0.8, d = 0.8 }
            else
                { topColour = "#F44336", leftColour = "#C62828", rightColour = "#B71C1C", w = 0.6, d = 0.6 }


-- ── Queue → SceneObjects ──────────────────────────────────────────────────────

queueToObjects : String -> QueueSpec -> List Priority -> QueueMetrics -> List (SceneObject msg)
queueToObjects theme spec prios _ =
    let
        pos  = { x = spec.x / 48.0, y = 0.0, z = spec.y / 48.0 }
        barW = toFloat spec.capacity * 0.4

        ( ptTop, ptLeft, ptRight ) =
            if theme == "expanse" then
                ( "#8D4004", "#6D2E02", "#4E1E01" )
            else
                ( "#607D8B", "#455A64", "#37474F" )

        platform =
            { pos    = pos
            , height = spec.h
            , shape  = Box
                { topColour   = ptTop
                , leftColour  = ptLeft
                , rightColour = ptRight
                , w = barW
                , d = 0.6
                }
            }

        -- Slots sit in a row along the platform's x-axis. The platform is
        -- centred on pos, so the first cell starts at the left edge and each
        -- slot is centred within its 0.4-wide cell. A gap (cell 0.4 vs cube
        -- 0.25) separates adjacent jobs so the queue reads slot-by-slot.
        cell     = 0.4
        leftEdge = pos.x - barW / 2
        slotX i  = leftEdge + (toFloat i + 0.5) * cell

        ( skTop, skLeft, skRight ) =
            if theme == "expanse" then
                ( "#05202B", "#03161E", "#020E14" )
            else
                ( "#2F3E46", "#26333A", "#1C262C" )

        -- One recessed socket per capacity slot, so empty capacity is visible
        -- at a glance (a "3/6 full" buffer reads without the label).
        slotSockets =
            List.range 0 (spec.capacity - 1)
                |> List.map
                    (\i ->
                        { pos    = { x = slotX i, y = spec.h, z = pos.z }
                        , height = 0.05
                        , shape  = Box
                            { topColour = skTop, leftColour = skLeft, rightColour = skRight
                            , w = 0.3, d = 0.4
                            }
                        }
                    )

        -- Filled slots: bright priority-coloured cubes seated in their socket.
        jobCubes =
            List.indexedMap
                (\i prio ->
                    { pos    = { x = slotX i, y = spec.h + 0.05, z = pos.z }
                    , height = 0.28
                    , shape  = Box (jobBoxStyle prio)
                    }
                )
                prios
    in
    platform :: (slotSockets ++ jobCubes)


jobBoxStyle : Priority -> BoxStyle
jobBoxStyle prio =
    case prio of
        Low      -> { topColour = "#b2bec3", leftColour = "#8e9aa3", rightColour = "#747e86", w = 0.25, d = 0.35 }
        Normal   -> { topColour = "#74b9ff", leftColour = "#4a9de0", rightColour = "#2e7fc4", w = 0.25, d = 0.35 }
        High     -> { topColour = "#fdcb6e", leftColour = "#e0b050", rightColour = "#c49030", w = 0.25, d = 0.35 }
        Critical -> { topColour = "#d63031", leftColour = "#b52021", rightColour = "#961010", w = 0.25, d = 0.35 }


-- ── Edge → SceneObject ────────────────────────────────────────────────────────

{-| An edge is sampled into several short segments along its quadratic arc, each
emitted as its own SceneObject positioned at the segment midpoint. This lets the
painter's-algorithm depth sort interleave edge segments with the boxes they pass
in front of / behind, instead of sorting the whole edge by a single endpoint. -}
edgeToObjects : ScenarioConfig -> EdgeSpec -> List (SceneObject msg)
edgeToObjects cfg edge =
    case ( resolveEndpoint cfg edge.from, resolveEndpoint cfg edge.to ) of
        ( Just fp, Just tp ) ->
            arcSamples fp tp 10
                |> segmentPairs
                |> List.map
                    (\( a, b ) ->
                        { pos    = midpoint a b
                        , height = 0.0
                        , shape  =
                            Path3D
                                { points = [ a, b ]
                                , stroke = "#90A4AE"
                                , width  = 2.0
                                }
                        }
                    )

        _ ->
            []


{-| Sample a quadratic Bézier (from → arcMid → to) into n+1 points. -}
arcSamples :
    { x : Float, y : Float, z : Float }
    -> { x : Float, y : Float, z : Float }
    -> Int
    -> List { x : Float, y : Float, z : Float }
arcSamples a b n =
    let
        m = arcMid a b
        at t =
            let
                u = 1.0 - t
                q c0 c1 c2 = u * u * c0 + 2.0 * u * t * c1 + t * t * c2
            in
            { x = q a.x m.x b.x
            , y = q a.y m.y b.y
            , z = q a.z m.z b.z
            }
    in
    List.range 0 n |> List.map (\i -> at (toFloat i / toFloat n))


segmentPairs : List a -> List ( a, a )
segmentPairs xs =
    case xs of
        p :: ((q :: _) as rest) ->
            ( p, q ) :: segmentPairs rest

        _ ->
            []


midpoint :
    { x : Float, y : Float, z : Float }
    -> { x : Float, y : Float, z : Float }
    -> { x : Float, y : Float, z : Float }
midpoint a b =
    { x = (a.x + b.x) / 2.0
    , y = (a.y + b.y) / 2.0
    , z = (a.z + b.z) / 2.0
    }


resolveEndpoint : ScenarioConfig -> String -> Maybe { x : Float, y : Float, z : Float }
resolveEndpoint cfg ep =
    case String.split ":" ep of
        [ "node", n ] ->
            String.toInt n
                |> Maybe.andThen
                    (\id ->
                        cfg.nodes
                            |> List.filter (\s -> s.id == id)
                            |> List.head
                            |> Maybe.map (\s -> { x = s.x / 48.0, y = s.h / 2.0, z = s.y / 48.0 })
                    )

        [ "queue", n ] ->
            String.toInt n
                |> Maybe.andThen
                    (\id ->
                        cfg.queues
                            |> List.filter (\s -> s.id == id)
                            |> List.head
                            |> Maybe.map (\s -> { x = s.x / 48.0, y = s.h / 2.0, z = s.y / 48.0 })
                    )

        _ ->
            Nothing


arcMid : { x : Float, y : Float, z : Float } -> { x : Float, y : Float, z : Float } -> { x : Float, y : Float, z : Float }
arcMid a b =
    { x = (a.x + b.x) / 2.0
    , y = max a.y b.y + 0.2
    , z = (a.z + b.z) / 2.0
    }


-- ── Labels ────────────────────────────────────────────────────────────────────

nodeLabel : Camera -> NodeSpec -> NodeMetrics -> SimState -> Svg msg
nodeLabel cam spec nm state =
    let
        pos    = { x = spec.x / 48.0, y = spec.h + 0.25, z = spec.y / 48.0 }
        ( sx, sy ) = Camera.project cam pos

        mNode  = SimState.getNode (NodeID spec.id) state
        stateStr =
            case mNode |> Maybe.map .state of
                Just (Busy _ _)      -> "busy"
                Just (Blocked _)     -> "blocked"
                Just (Signoff _ _)   -> "signoff"
                Just (Preempted _ _) -> "preempt"
                Just (Paused _)      -> "paused"
                Just Idle            -> "idle"
                Nothing              -> ""

        utilPct = String.fromInt (round (nm.utilisation * 100)) ++ "%"
        subLine =
            if stateStr == "" then ""
            else stateStr ++ " " ++ utilPct
    in
    Svg.g []
        [ isoText sx (sy - 4)  "#fff"  "11" "bold"   spec.label
        , isoText sx (sy + 10) "#aaa"  "9"  "normal" subLine
        ]


queueLabel : Camera -> QueueSpec -> Int -> Int -> Svg msg
queueLabel cam spec len drops =
    let
        pos    = { x = spec.x / 48.0, y = spec.h + 0.25, z = spec.y / 48.0 }
        ( sx, sy ) = Camera.project cam pos
        fillStr = String.fromInt len ++ "/" ++ String.fromInt spec.capacity
        dropLine =
            if drops > 0 then
                [ isoText sx (sy + 19) "#ff6b6b" "9" "bold" ("⚠ " ++ String.fromInt drops ++ " dropped") ]
            else
                []
    in
    Svg.g []
        ([ isoText sx (sy - 4)  "#fff" "10" "bold"   spec.label
         , isoText sx (sy + 8)  "#aaa" "9"  "normal" fillStr
         ]
            ++ dropLine
        )


isoText : Float -> Float -> String -> String -> String -> String -> Svg msg
isoText sx sy colour size weight content =
    Svg.text_
        [ SA.x          (String.fromFloat sx)
        , SA.y          (String.fromFloat sy)
        , SA.textAnchor "middle"
        , SA.fontSize   size
        , SA.fontWeight weight
        , SA.fontFamily "monospace"
        , SA.fill       colour
        ]
        [ Svg.text content ]


-- ── Job dot ───────────────────────────────────────────────────────────────────

jobDot : String -> Float -> Float -> Float -> Priority -> Svg msg
jobDot theme sx sy heading prio =
    if theme == "expanse" then
        shipMarker sx sy heading prio
    else
        Svg.circle
            [ SA.cx   (String.fromFloat sx)
            , SA.cy   (String.fromFloat sy)
            , SA.r    "5"
            , SA.fill (priorityColor prio)
            ]
            []


{-| A small angular hull silhouette (bow up) used as the job marker in the
expanse theme — an asset-free vector ship in place of a plain dot. Critical
jobs get a brighter outline so warships stand out in transit. -}
shipMarker : Float -> Float -> Float -> Priority -> Svg msg
shipMarker sx sy heading prio =
    let
        pt dx dy =
            String.fromFloat (sx + dx) ++ "," ++ String.fromFloat (sy + dy)

        -- Hull authored bow-up; rotated about (sx,sy) to face the heading.
        hull =
            String.join " "
                [ pt 0 -7, pt 4 -1, pt 3 5, pt -3 5, pt -4 -1 ]

        stroke =
            case prio of
                Critical -> "#ffe08a"
                _        -> "#0b1b26"

        rot =
            "rotate(" ++ String.fromFloat heading
                ++ " " ++ String.fromFloat sx
                ++ " " ++ String.fromFloat sy ++ ")"
    in
    Svg.g [ SA.transform rot ]
        [ Svg.polygon
            [ SA.points hull
            , SA.fill   (priorityColor prio)
            , SA.stroke stroke
            , SA.strokeWidth (if prio == Critical then "1.4" else "0.8")
            , SA.strokeLinejoin "round"
            ]
            []
        , Svg.circle
            [ SA.cx (String.fromFloat sx)
            , SA.cy (String.fromFloat (sy + 5.5))
            , SA.r  "1.4"
            , SA.fill (if prio == Critical then "#ff5b3a" else "#56d2ff")
            , SA.opacity "0.9"
            ]
            []
        ]


priorityColor : Priority -> String
priorityColor p =
    case p of
        Low      -> "#b2bec3"
        Normal   -> "#74b9ff"
        High     -> "#fdcb6e"
        Critical -> "#d63031"


-- ── Interrupt alert overlay ───────────────────────────────────────────────────

{-| Screen-space banner shown across the top of the iso canvas while a bosmang
all-hands (interrupt) is active. Drawn last so it sits over the scene. -}
viewAlert : Bool -> Float -> Svg msg
viewAlert active canvasW =
    if not active then
        Svg.g [] []
    else
        Svg.g []
            [ Svg.rect
                [ SA.x "0", SA.y "0"
                , SA.width (String.fromFloat canvasW)
                , SA.height "26"
                , SA.fill "#B71C1C"
                , SA.opacity "0.88"
                ]
                []
            , Svg.text_
                [ SA.x (String.fromFloat (canvasW / 2.0))
                , SA.y "18"
                , SA.textAnchor "middle"
                , SA.fontSize "13"
                , SA.fontWeight "bold"
                , SA.fontFamily "monospace"
                , SA.fill "#fff"
                , SA.letterSpacing "1"
                ]
                [ Svg.text "⚡ BOSMANG ALL-HANDS — WORKERS DOWN TOOLS ⚡" ]
            ]


-- ── Starfield ─────────────────────────────────────────────────────────────────

{-| Parallax starfield for the isometric background.
Three depth layers scroll at different rates as the camera spins.
Stars are deterministic (no random seed needed in the model). -}
viewStarfield : Camera -> Float -> Float -> Svg msg
viewStarfield cam canvasW canvasH =
    let
        layers =
            [ { count = 70, parallax = 0.04, minR = 0.4, maxR = 0.9, opacity = 0.35 }
            , { count = 40, parallax = 0.10, minR = 0.7, maxR = 1.3, opacity = 0.55 }
            , { count = 20, parallax = 0.20, minR = 1.0, maxR = 2.0, opacity = 0.80 }
            ]

        -- Simple deterministic LCG: avoids importing Random just for static positions
        lcg n = modBy 99991 (n * 48271 + 12345)

        renderStar layerIdx { parallax, minR, maxR, opacity } i =
            let
                s1 = lcg (i * 31337 + layerIdx * 99991)
                s2 = lcg (s1 + 7919)
                s3 = lcg (s2 + 4567)
                x0 = toFloat s1 / 99991.0
                y0 = toFloat s2 / 99991.0
                sz = toFloat s3 / 99991.0
                -- Parallax: one full camera revolution shifts a star by parallax*canvasW
                shift    = cam.spinAngle / (2 * pi) * parallax * canvasW
                rawX     = x0 * canvasW + shift
                n        = floor (rawX / canvasW)
                wrappedX = rawX - toFloat n * canvasW
                sy_      = y0 * canvasH
                r        = minR + sz * (maxR - minR)
            in
            Svg.circle
                [ SA.cx      (String.fromFloat wrappedX)
                , SA.cy      (String.fromFloat sy_)
                , SA.r       (String.fromFloat r)
                , SA.fill    "#ffffff"
                , SA.opacity (String.fromFloat opacity)
                ]
                []
    in
    Svg.g []
        (List.concat
            (List.indexedMap
                (\li layer ->
                    List.range 0 (layer.count - 1)
                        |> List.map (renderStar li layer)
                )
                layers
            )
        )


-- ── Colour helpers ────────────────────────────────────────────────────────────

utilColour : Float -> String
utilColour u =
    if u > 0.9 then "#EF5350"
    else if u > 0.7 then "#FFA726"
    else "#66BB6A"



darken : String -> String
darken c =
    case c of
        "#EF5350" -> "#C62828"
        "#FFA726" -> "#E65100"
        "#42A5F5" -> "#1565C0"
        _         -> "#333"


darken2 : String -> String
darken2 c =
    case c of
        "#EF5350" -> "#B71C1C"
        "#FFA726" -> "#BF360C"
        "#42A5F5" -> "#0D47A1"
        _         -> "#222"


-- ── Empty defaults ────────────────────────────────────────────────────────────

emptyNodeMetrics : NodeMetrics
emptyNodeMetrics =
    { utilisation    = 0.0
    , jobsProcessed  = 0
    , avgServiceTime = 0.0
    }


emptyQueueMetrics : QueueMetrics
emptyQueueMetrics =
    { avgLength = 0.0
    , maxLength = 0
    , dropCount = 0
    }
