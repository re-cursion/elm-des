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
import Theme exposing (SpriteSource(..))
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

        BillboardSprite { source, w, h } ->
            [ renderBillboard cam base source w h ]

        DirectionalSprite { source, directions, w, h } ->
            [ renderDirectional cam base source directions w h ]

        Path3D { points, stroke, width } ->
            [ renderPath3D cam points stroke width ]

        RawSvg fn ->
            [ fn base ]


-- ── Box ───────────────────────────────────────────────────────────────────────

{-| Draw a solid isometric box. `pos` is the CENTRE of the footprint at floor
level; the box extends ±w/2 in x, ±d/2 in z, and rises `height` in y.

All four side walls are drawn, depth-sorted farthest-first, then the top face
last. This keeps the box solid (never see-through) at any `spinAngle`. The two
x-facing walls use `leftColour`, the two z-facing walls use `rightColour`, so a
given physical face keeps its shade as the camera rotates. -}
renderBox : Camera -> { x : Float, y : Float, z : Float } -> Float -> BoxStyle -> List (Svg msg)
renderBox cam pos height style =
    let
        hw = style.w / 2
        hd = style.d / 2
        x0 = pos.x - hw
        x1 = pos.x + hw
        z0 = pos.z - hd
        z1 = pos.z + hd
        y0 = pos.y
        y1 = pos.y + height

        -- 8 corners: cXYZ where X/Y/Z ∈ {0,1} select low/high on that axis
        c000 = { x = x0, y = y0, z = z0 }
        c100 = { x = x1, y = y0, z = z0 }
        c001 = { x = x0, y = y0, z = z1 }
        c101 = { x = x1, y = y0, z = z1 }
        c010 = { x = x0, y = y1, z = z0 }
        c110 = { x = x1, y = y1, z = z0 }
        c011 = { x = x0, y = y1, z = z1 }
        c111 = { x = x1, y = y1, z = z1 }

        prj = Camera.project cam

        faceDepth corners =
            List.sum (List.map (Camera.depthOf cam) corners) / 4

        -- Four side walls: (depth-key, rendered polygon)
        walls =
            [ ( faceDepth [ c000, c100, c110, c010 ]
              , polygon4 (prj c000) (prj c100) (prj c110) (prj c010) style.rightColour )  -- −z wall
            , ( faceDepth [ c001, c101, c111, c011 ]
              , polygon4 (prj c001) (prj c101) (prj c111) (prj c011) style.rightColour )  -- +z wall
            , ( faceDepth [ c000, c001, c011, c010 ]
              , polygon4 (prj c000) (prj c001) (prj c011) (prj c010) style.leftColour )   -- −x wall
            , ( faceDepth [ c100, c101, c111, c110 ]
              , polygon4 (prj c100) (prj c101) (prj c111) (prj c110) style.leftColour )   -- +x wall
            ]

        sortedWalls =
            walls
                |> List.sortBy Tuple.first
                |> List.reverse
                |> List.map Tuple.second

        top = polygon4 (prj c010) (prj c110) (prj c111) (prj c011) style.topColour
    in
    sortedWalls ++ [ top ]


-- ── FlatTile ──────────────────────────────────────────────────────────────────

{-| Flat ground quad centred on `pos`, spanning ±w/2 in x and ±d/2 in z. -}
renderFlatTile : Camera -> { x : Float, y : Float, z : Float } -> Float -> Float -> String -> Svg msg
renderFlatTile cam pos w d fill =
    let
        hw  = w / 2
        hd  = d / 2
        p   = { pos | x = pos.x - hw, z = pos.z - hd }
        px  = { pos | x = pos.x + hw, z = pos.z - hd }
        pz  = { pos | x = pos.x - hw, z = pos.z + hd }
        pxz = { pos | x = pos.x + hw, z = pos.z + hd }
    in
    polygon4
        (Camera.project cam p)
        (Camera.project cam px)
        (Camera.project cam pxz)
        (Camera.project cam pz)
        fill


-- ── Billboard ─────────────────────────────────────────────────────────────────

renderBillboard : Camera -> ( Float, Float ) -> SpriteSource -> Float -> Float -> Svg msg
renderBillboard cam ( sx, sy ) source w h =
    let
        pxW = w * cam.scale
        pxH = h * cam.scale
    in
    case source of
        VectorSymbol id ->
            Svg.node "use"
                [ SA.xlinkHref ("#" ++ id)
                , SA.x      (String.fromFloat (sx - pxW / 2))
                , SA.y      (String.fromFloat (sy - pxH))
                , SA.width  (String.fromFloat pxW)
                , SA.height (String.fromFloat pxH)
                ]
                []

        RasterImage url ->
            Svg.image
                [ SA.xlinkHref url
                , SA.x      (String.fromFloat (sx - pxW / 2))
                , SA.y      (String.fromFloat (sy - pxH))
                , SA.width  (String.fromFloat pxW)
                , SA.height (String.fromFloat pxH)
                , SA.preserveAspectRatio "xMidYMid meet"
                ]
                []

        SpriteSheet spec ->
            -- Nested SVG with a viewBox acts as a clip: shows only the frame region.
            Svg.svg
                [ SA.x       (String.fromFloat (sx - pxW / 2))
                , SA.y       (String.fromFloat (sy - pxH))
                , SA.width   (String.fromFloat pxW)
                , SA.height  (String.fromFloat pxH)
                , SA.viewBox (String.join " "
                    [ String.fromInt spec.x, String.fromInt spec.y
                    , String.fromInt spec.w, String.fromInt spec.h
                    ])
                ]
                [ Svg.image
                    [ SA.xlinkHref spec.url
                    , SA.x "0", SA.y "0"
                    , SA.width  (String.fromInt (spec.x + spec.w + 9999))
                    , SA.height (String.fromInt (spec.y + spec.h + 9999))
                    ]
                    []
                ]


renderDirectional : Camera -> ( Float, Float ) -> SpriteSource -> Int -> Float -> Float -> Svg msg
renderDirectional cam ( sx, sy ) source directions w h =
    let
        pxW = w * cam.scale
        pxH = h * cam.scale
        -- Map spin angle (unbounded) → frame index in [0, directions)
        norm     = cam.spinAngle - (2 * pi * toFloat (floor (cam.spinAngle / (2 * pi))))
        frameIdx = modBy (max 1 directions) (round (norm / (2 * pi) * toFloat directions))
    in
    case source of
        VectorSymbol id ->
            Svg.node "use"
                [ SA.xlinkHref ("#" ++ id)
                , SA.x      (String.fromFloat (sx - pxW / 2))
                , SA.y      (String.fromFloat (sy - pxH))
                , SA.width  (String.fromFloat pxW)
                , SA.height (String.fromFloat pxH)
                ]
                []

        RasterImage url ->
            Svg.image
                [ SA.xlinkHref url
                , SA.x      (String.fromFloat (sx - pxW / 2))
                , SA.y      (String.fromFloat (sy - pxH))
                , SA.width  (String.fromFloat pxW)
                , SA.height (String.fromFloat pxH)
                , SA.preserveAspectRatio "xMidYMid meet"
                ]
                []

        SpriteSheet spec ->
            -- Frames arranged horizontally: frame N starts at x = spec.x + N * spec.w
            let frameX = spec.x + frameIdx * spec.w
            in
            Svg.svg
                [ SA.x       (String.fromFloat (sx - pxW / 2))
                , SA.y       (String.fromFloat (sy - pxH))
                , SA.width   (String.fromFloat pxW)
                , SA.height  (String.fromFloat pxH)
                , SA.viewBox (String.join " "
                    [ String.fromInt frameX, String.fromInt spec.y
                    , String.fromInt spec.w,  String.fromInt spec.h
                    ])
                ]
                [ Svg.image
                    [ SA.xlinkHref spec.url
                    , SA.x "0", SA.y "0"
                    , SA.width  (String.fromInt (spec.x + directions * spec.w + 9999))
                    , SA.height (String.fromInt (spec.y + spec.h + 9999))
                    ]
                    []
                ]


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
