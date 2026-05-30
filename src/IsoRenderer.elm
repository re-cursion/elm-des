module IsoRenderer exposing
    ( viewScene
    , nodeToObjects
    , queueToObjects
    , edgeToObject
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
import Queue
import ScenarioConfig exposing (EdgeSpec, NodeSpec, QueueSpec, ScenarioConfig)
import SceneObject exposing (BoxStyle, SceneObject, SceneShape(..), renderAll)
import SimState exposing (SimState)
import Svg exposing (Svg)
import Svg.Attributes as SA


-- ── Scene entry point ─────────────────────────────────────────────────────────

viewScene
    : Camera
    -> ScenarioConfig
    -> SimState
    -> SystemMetrics
    -> Dict.Dict Int ( Float, Float )
    -> Svg msg
viewScene cam cfg state metrics jobPositions =
    let
        nodeObjects =
            cfg.nodes
                |> List.concatMap
                    (\spec ->
                        let nm = Dict.get spec.id metrics.nodes |> Maybe.withDefault emptyNodeMetrics
                        in nodeToObjects spec nm
                    )

        queueObjects =
            cfg.queues
                |> List.concatMap
                    (\spec ->
                        let
                            qm  = Dict.get spec.id metrics.queues |> Maybe.withDefault emptyQueueMetrics
                            len = SimState.getQueue (QueueID spec.id) state
                                    |> Maybe.map Queue.size
                                    |> Maybe.withDefault 0
                        in
                        queueToObjects spec len qm
                    )

        edgeObjects =
            cfg.edges |> List.filterMap (edgeToObject cfg)

        jobDots =
            Dict.values jobPositions
                |> List.map (\( sx, sy ) -> jobDot sx sy)

        allObjects =
            edgeObjects ++ nodeObjects ++ queueObjects
    in
    Svg.g [] (renderAll cam allObjects ++ jobDots)


-- ── Node → SceneObjects ───────────────────────────────────────────────────────

nodeToObjects : NodeSpec -> NodeMetrics -> List (SceneObject msg)
nodeToObjects spec metrics =
    let
        pos = { x = spec.x / 48.0, y = 0.0, z = spec.y / 48.0 }
        h   = spec.h
        u   = metrics.utilisation

        node =
            { pos    = pos
            , height = h
            , shape  = Box (nodeBoxStyle spec u)
            }

        utilBar =
            { pos    = { pos | y = h }
            , height = 0.05
            , shape  = Box
                { topColour   = utilColour u
                , leftColour  = utilColour u
                , rightColour = utilColour u
                , w           = u * 1.0
                , d           = 0.2
                }
            }
    in
    [ node, utilBar ]


nodeBoxStyle : NodeSpec -> Float -> BoxStyle
nodeBoxStyle spec u =
    case spec.kind of
        ScenarioConfig.SourceSpec _ ->
            { topColour = "#4CAF50", leftColour = "#388E3C", rightColour = "#2E7D32", w = 1.0, d = 1.0 }

        ScenarioConfig.SinkSpec ->
            { topColour = "#9E9E9E", leftColour = "#757575", rightColour = "#616161", w = 1.0, d = 1.0 }

        ScenarioConfig.WorkerSpec _ ->
            let
                top = if u > 0.8 then "#EF5350" else if u > 0.5 then "#FFA726" else "#42A5F5"
            in
            { topColour = top, leftColour = darken top, rightColour = darken2 top, w = 1.0, d = 1.0 }

        ScenarioConfig.DispatcherSpec _ ->
            { topColour = "#AB47BC", leftColour = "#7B1FA2", rightColour = "#6A1B9A", w = 1.0, d = 1.0 }

        ScenarioConfig.InterruptSpec ->
            { topColour = "#F44336", leftColour = "#C62828", rightColour = "#B71C1C", w = 0.6, d = 0.6 }


-- ── Queue → SceneObjects ──────────────────────────────────────────────────────

queueToObjects : QueueSpec -> Int -> QueueMetrics -> List (SceneObject msg)
queueToObjects spec len qm =
    let
        pos          = { x = spec.x / 48.0, y = 0.0, z = spec.y / 48.0 }
        cap          = spec.capacity
        fillFraction = if cap > 0 then toFloat len / toFloat cap else 0.0
        barW         = toFloat cap * 0.4

        platform =
            { pos    = pos
            , height = spec.h
            , shape  = Box
                { topColour   = "#607D8B"
                , leftColour  = "#455A64"
                , rightColour = "#37474F"
                , w = barW
                , d = 0.6
                }
            }

        fillBar =
            { pos    = { pos | y = spec.h }
            , height = 0.05
            , shape  = Box
                { topColour   = fillColour fillFraction
                , leftColour  = fillColour fillFraction
                , rightColour = fillColour fillFraction
                , w = fillFraction * barW
                , d = 0.6
                }
            }
    in
    [ platform, fillBar ]


-- ── Edge → SceneObject ────────────────────────────────────────────────────────

edgeToObject : ScenarioConfig -> EdgeSpec -> Maybe (SceneObject msg)
edgeToObject cfg edge =
    let
        fromPos = resolveEndpoint cfg edge.from
        toPos   = resolveEndpoint cfg edge.to
    in
    case ( fromPos, toPos ) of
        ( Just fp, Just tp ) ->
            Just
                { pos    = fp
                , height = 0.0
                , shape  =
                    Path3D
                        { points = [ fp, arcMid fp tp, tp ]
                        , stroke = "#78909C"
                        , width  = 0.5
                        }
                }

        _ ->
            Nothing


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


-- ── Job dot ───────────────────────────────────────────────────────────────────

jobDot : Float -> Float -> Svg msg
jobDot sx sy =
    Svg.circle
        [ SA.cx   (String.fromFloat sx)
        , SA.cy   (String.fromFloat sy)
        , SA.r    "5"
        , SA.fill "#FFD54F"
        ]
        []


-- ── Colour helpers ────────────────────────────────────────────────────────────

utilColour : Float -> String
utilColour u =
    if u > 0.9 then "#EF5350"
    else if u > 0.7 then "#FFA726"
    else "#66BB6A"


fillColour : Float -> String
fillColour f =
    if f > 0.9 then "#EF5350"
    else if f > 0.6 then "#FFA726"
    else "#42A5F5"


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
