module SceneObject exposing
    ( SceneObject
    , SceneShape(..)
    , BoxStyle
    , sortByDepth
    , renderAll
    )

{-| Scene graph for the Phase 5 isometric renderer.

Every visible element is a SceneObject. The renderer:
  1. Computes each object's depthOf key with the current Camera
  2. Sorts farthest-first (painter's algorithm)
  3. Calls the shape's SVG renderer in order
-}

import Camera exposing (Camera)
import Theme exposing (SpriteSource)
import Svg exposing (Svg)
import Svg.Attributes as SA


-- ── Types ─────────────────────────────────────────────────────────────────────

type alias SceneObject msg =
    { pos    : { x : Float, y : Float, z : Float }
    , height : Float     -- vertical extent (world units); used by Box faces
    , shape  : SceneShape msg
    }


type SceneShape msg
    = Box BoxStyle
    | FlatTile { w : Float, d : Float, fill : String }
    | BillboardSprite { source : SpriteSource, w : Float, h : Float }
    | DirectionalSprite { source : SpriteSource, directions : Int, w : Float, h : Float }
    | Path3D { points : List { x : Float, y : Float, z : Float }, stroke : String, width : Float }
    | RawSvg (( Float, Float ) -> Svg msg)   -- escape hatch: caller gets screen pos


type alias BoxStyle =
    { topColour   : String
    , leftColour  : String
    , rightColour : String
    , w : Float   -- width  (x axis)
    , d : Float   -- depth  (z axis)
    }


-- ── Sorting ───────────────────────────────────────────────────────────────────

sortByDepth : Camera -> List (SceneObject msg) -> List (SceneObject msg)
sortByDepth cam objects =
    objects
        |> List.sortBy (\obj -> Camera.depthOf cam obj.pos)
        |> List.reverse


-- ── Rendering ─────────────────────────────────────────────────────────────────

renderAll : Camera -> List (SceneObject msg) -> List (Svg msg)
renderAll cam objects =
    objects
        |> sortByDepth cam
        |> List.concatMap (renderOne cam)


renderOne : Camera -> SceneObject msg -> List (Svg msg)
renderOne cam obj =
    let
        base = Camera.project cam obj.pos
    in
    case obj.shape of
        Box style ->
            renderBox cam obj.pos obj.height style

        FlatTile { w, d, fill } ->
            [ renderFlatTile cam obj.pos w d fill ]

        BillboardSprite { w, h } ->
            [ renderBillboard base w h ]

        DirectionalSprite { directions, w, h } ->
            [ renderDirectional cam base directions w h ]

        Path3D { points, stroke, width } ->
            [ renderPath3D cam points stroke width ]

        RawSvg fn ->
            [ fn base ]


-- ── Box ───────────────────────────────────────────────────────────────────────

{-| Draw three faces of an isometric box: top, left, right.
The box floor corner is at `pos`; it rises `height` units in the Y direction. -}
renderBox : Camera -> { x : Float, y : Float, z : Float } -> Float -> BoxStyle -> List (Svg msg)
renderBox cam pos height style =
    let
        w  = style.w
        d  = style.d
        -- Eight corners of the box in world space
        -- Floor: pos (origin), +x, +z, +x+z
        -- Ceiling: same + height in y
        p  = pos
        px = { pos | x = pos.x + w }
        pz = { pos | z = pos.z + d }
        pxz = { pos | x = pos.x + w, z = pos.z + d }
        py  = { pos | y = pos.y + height }
        pxy = { pos | x = pos.x + w, y = pos.y + height }
        pzy = { pos | z = pos.z + d, y = pos.y + height }
        pxzy = { pos | x = pos.x + w, z = pos.z + d, y = pos.y + height }

        sp    = Camera.project cam p
        spx   = Camera.project cam px
        spz   = Camera.project cam pz
        spxz  = Camera.project cam pxz
        spy   = Camera.project cam py
        spxy  = Camera.project cam pxy
        spzy  = Camera.project cam pzy
        spxzy = Camera.project cam pxzy

        top   = polygon4 spy  spxy spxzy spzy  style.topColour
        left  = polygon4 sp   spy  spzy  spz   style.leftColour
        right = polygon4 spx  spxz spxzy spxy  style.rightColour
    in
    [ left, right, top ]


-- ── FlatTile ──────────────────────────────────────────────────────────────────

renderFlatTile : Camera -> { x : Float, y : Float, z : Float } -> Float -> Float -> String -> Svg msg
renderFlatTile cam pos w d fill =
    let
        p   = pos
        px  = { pos | x = pos.x + w }
        pz  = { pos | z = pos.z + d }
        pxz = { pos | x = pos.x + w, z = pos.z + d }
    in
    polygon4
        (Camera.project cam p)
        (Camera.project cam px)
        (Camera.project cam pxz)
        (Camera.project cam pz)
        fill


-- ── Billboard ─────────────────────────────────────────────────────────────────

renderBillboard : ( Float, Float ) -> Float -> Float -> Svg msg
renderBillboard ( sx, sy ) w h =
    Svg.rect
        [ SA.x      (String.fromFloat (sx - w / 2))
        , SA.y      (String.fromFloat (sy - h))
        , SA.width  (String.fromFloat w)
        , SA.height (String.fromFloat h)
        , SA.fill   "#888"
        , SA.opacity "0.7"
        ]
        []


renderDirectional : Camera -> ( Float, Float ) -> Int -> Float -> Float -> Svg msg
renderDirectional _ ( sx, sy ) _ w h =
    -- Placeholder — real impl stamps a <use> with the right sprite-sheet frame
    renderBillboard ( sx, sy ) w h


-- ── Path3D ────────────────────────────────────────────────────────────────────

renderPath3D : Camera -> List { x : Float, y : Float, z : Float } -> String -> Float -> Svg msg
renderPath3D cam points stroke width =
    let
        screenPts = List.map (Camera.project cam) points
        d =
            case screenPts of
                [] -> ""
                ( hx, hy ) :: rest ->
                    "M " ++ String.fromFloat hx ++ " " ++ String.fromFloat hy
                    ++ String.concat
                        (List.map (\( px, py ) ->
                            " L " ++ String.fromFloat px ++ " " ++ String.fromFloat py)
                            rest)
    in
    Svg.path
        [ SA.d            d
        , SA.stroke       stroke
        , SA.strokeWidth  (String.fromFloat width)
        , SA.fill         "none"
        , SA.strokeLinecap "round"
        ]
        []


-- ── Helpers ───────────────────────────────────────────────────────────────────

polygon4 : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> String -> Svg msg
polygon4 ( ax, ay ) ( bx, by ) ( cx, cy ) ( dx, dy ) colour =
    let
        pts =
            [ String.fromFloat ax, ",", String.fromFloat ay, " "
            , String.fromFloat bx, ",", String.fromFloat by, " "
            , String.fromFloat cx, ",", String.fromFloat cy, " "
            , String.fromFloat dx, ",", String.fromFloat dy
            ]
    in
    Svg.polygon
        [ SA.points (String.concat pts)
        , SA.fill   colour
        , SA.stroke colour
        , SA.strokeWidth "0.5"
        ]
        []
