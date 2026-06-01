module TestSceneObject exposing (suite)

import Camera exposing (defaultCamera)
import Expect
import SceneObject exposing (SceneObject, SceneShape(..), sortByDepth)
import Test exposing (Test, describe, test)


{-| A trivial flat tile at a world position; height/shape are irrelevant to the
depth sort, which keys only on `pos`. -}
tile : Float -> Float -> Float -> SceneObject ()
tile x y z =
    { pos = { x = x, y = y, z = z }
    , height = 0.0
    , shape = FlatTile { w = 1.0, d = 1.0, fill = "#000" }
    }


suite : Test
suite =
    describe "SceneObject.sortByDepth (painter's algorithm)"
        -- With spinAngle = 0 the projection reduces depth to x + z, so the
        -- expected ordering is easy to reason about by hand.
        [ test "farthest object (largest x+z) is drawn first" <|
            \_ ->
                let
                    cam = { defaultCamera | spinAngle = 0 }
                    near = tile 0 0 0     -- depth 0
                    mid  = tile 1 0 0     -- depth 1
                    far  = tile 5 0 5     -- depth 10
                    xs =
                        sortByDepth cam [ near, far, mid ]
                            |> List.map (\o -> o.pos.x)
                in
                Expect.equal [ 5, 1, 0 ] xs

        , test "vertical height (y) does not affect depth ordering" <|
            \_ ->
                let
                    cam = { defaultCamera | spinAngle = 0 }
                    low  = tile 2 0 0
                    high = tile 2 10 0
                    xs =
                        sortByDepth cam [ low, high ]
                            |> List.map (\o -> o.pos.x)
                in
                Expect.equal [ 2, 2 ] xs

        , test "ordering is stable under a full 2π spin" <|
            \_ ->
                let
                    cam  = { defaultCamera | spinAngle = 0 }
                    cam2 = { defaultCamera | spinAngle = 2 * pi }
                    objs = [ tile 0 0 0, tile 3 0 1, tile 1 0 4 ]
                    order c =
                        sortByDepth c objs |> List.map (\o -> ( o.pos.x, o.pos.z ))
                in
                Expect.equal (order cam) (order cam2)
        ]
