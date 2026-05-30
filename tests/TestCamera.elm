module TestCamera exposing (suite)

import Camera exposing (defaultCamera, depthOf, project, spin, tilt, zoom)
import Expect
import Test exposing (Test, describe, test)


approx : Float -> Float -> Expect.Expectation
approx expected actual =
    let
        diff = abs (actual - expected)
    in
    if diff < 0.001 then
        Expect.pass
    else
        Expect.fail
            ("Expected ~" ++ String.fromFloat expected ++ " but got " ++ String.fromFloat actual)


suite : Test
suite =
    describe "Camera"
        [ describe "project"
            [ test "origin maps to screen origin" <|
                \_ ->
                    let
                        cam = { defaultCamera | origin = ( 400, 300 ), scale = 48, spinAngle = 0, tiltAngle = pi / 6 }
                        ( sx, sy ) = project cam { x = 0, y = 0, z = 0 }
                    in
                    Expect.all
                        [ \_ -> approx 400 sx
                        , \_ -> approx 300 sy
                        ]
                        ()

            , test "positive X moves right on screen (spinAngle=0)" <|
                \_ ->
                    let
                        cam = { defaultCamera | spinAngle = 0, tiltAngle = pi / 6, scale = 48, origin = ( 0, 0 ) }
                        ( sx, _ ) = project cam { x = 1, y = 0, z = 0 }
                    in
                    Expect.greaterThan 0 (round sx)

            , test "positive Y moves up on screen" <|
                \_ ->
                    let
                        cam = { defaultCamera | spinAngle = 0, tiltAngle = pi / 6, scale = 48, origin = ( 0, 0 ) }
                        ( _, sy ) = project cam { x = 0, y = 1, z = 0 }
                    in
                    Expect.lessThan 0 (round sy)

            , test "spin by 2π returns close to original position" <|
                \_ ->
                    let
                        cam  = { defaultCamera | spinAngle = 0, scale = 48, origin = ( 0, 0 ) }
                        cam2 = { cam | spinAngle = 2 * pi }
                        pt   = { x = 3, y = 0, z = 1 }
                        ( x1, y1 ) = project cam  pt
                        ( x2, y2 ) = project cam2 pt
                    in
                    Expect.all
                        [ \_ -> approx x1 x2
                        , \_ -> approx y1 y2
                        ]
                        ()

            , test "spin 90° swaps x and z roles" <|
                \_ ->
                    let
                        cam0 = { defaultCamera | spinAngle = 0,      scale = 48, origin = ( 0, 0 ), tiltAngle = pi / 6 }
                        cam1 = { defaultCamera | spinAngle = pi / 2, scale = 48, origin = ( 0, 0 ), tiltAngle = pi / 6 }
                        ptX  = { x = 1, y = 0, z = 0 }
                        ptZ  = { x = 0, y = 0, z = 1 }
                        ( sx0, _ ) = project cam0 ptX
                        ( sz1, _ ) = project cam1 ptZ
                    in
                    -- +X at spin=0 should project same as +Z at spin=90°
                    approx sx0 sz1
            ]

        , describe "depthOf"
            [ test "farther object (larger x+z) has larger depth" <|
                \_ ->
                    let
                        cam  = defaultCamera
                        near = { x = 0, y = 0, z = 0 }
                        far  = { x = 5, y = 0, z = 5 }
                    in
                    Expect.greaterThan (depthOf cam near) (depthOf cam far)

            , test "depth is symmetric after full spin" <|
                \_ ->
                    let
                        cam  = { defaultCamera | spinAngle = 0 }
                        cam2 = { defaultCamera | spinAngle = 2 * pi }
                        pt   = { x = 3, y = 1, z = 2 }
                    in
                    approx (depthOf cam pt) (depthOf cam2 pt)
            ]

        , describe "controls"
            [ test "spin adds to spinAngle" <|
                \_ ->
                    let cam = spin 0.5 defaultCamera
                    in approx (defaultCamera.spinAngle + 0.5) cam.spinAngle

            , test "tilt clamps at lower bound" <|
                \_ ->
                    let cam = tilt -100 defaultCamera
                    in Expect.greaterThan 0 (round (cam.tiltAngle * 100))

            , test "tilt clamps at upper bound" <|
                \_ ->
                    let cam = tilt 100 defaultCamera
                    in Expect.atMost (pi / 2.5) cam.tiltAngle

            , test "zoom scales up" <|
                \_ ->
                    let cam = zoom 2.0 defaultCamera
                    in Expect.greaterThan (round defaultCamera.scale) (round cam.scale)

            , test "zoom clamps at minimum" <|
                \_ ->
                    let cam = zoom 0.00001 defaultCamera
                    in Expect.greaterThan 0 (round cam.scale)
            ]
        ]
