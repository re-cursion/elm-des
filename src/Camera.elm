module Camera exposing
    ( Camera
    , defaultCamera
    , project
    , depthOf
    , spin
    , tilt
    , zoom
    )

{-| Rotatable isometric camera for Phase 5 renderer.

World coordinates: x = right, y = up, z = toward viewer.
The camera rotates the scene around the Y axis (spinAngle) then
applies a fixed isometric projection.
-}


type alias Camera =
    { spinAngle : Float          -- Y-axis rotation, radians 0..2π
    , tiltAngle : Float          -- tilt from horizontal (default: pi/6 = 30°)
    , scale     : Float          -- pixels per world unit
    , origin    : ( Float, Float )  -- screen-space centre point
    }


defaultCamera : Camera
defaultCamera =
    { spinAngle = pi / 4
    , tiltAngle = pi / 6
    , scale     = 48.0
    , origin    = ( 400.0, 300.0 )
    }


{-| Project a 3-D world point to SVG screen coordinates. -}
project : Camera -> { x : Float, y : Float, z : Float } -> ( Float, Float )
project cam { x, y, z } =
    let
        -- 1. Rotate around Y axis by spinAngle
        xr =  x * cos cam.spinAngle + z * sin cam.spinAngle
        zr = -x * sin cam.spinAngle + z * cos cam.spinAngle

        -- 2. Isometric projection; negate y so world-up = screen-up
        sx = (xr - zr) * cos cam.tiltAngle * cam.scale
        sy = (-(xr + zr) * sin cam.tiltAngle - y) * cam.scale
    in
    ( Tuple.first  cam.origin + sx
    , Tuple.second cam.origin + sy
    )


{-| Painter's-sort key: larger = farther from viewer = drawn first. -}
depthOf : Camera -> { x : Float, y : Float, z : Float } -> Float
depthOf cam { x, z } =
    let
        xr =  x * cos cam.spinAngle + z * sin cam.spinAngle
        zr = -x * sin cam.spinAngle + z * cos cam.spinAngle
    in
    xr + zr


spin : Float -> Camera -> Camera
spin delta cam =
    { cam | spinAngle = cam.spinAngle + delta }


tilt : Float -> Camera -> Camera
tilt delta cam =
    let
        newTilt = clamp 0.05 (pi / 2.5) (cam.tiltAngle + delta)
    in
    { cam | tiltAngle = newTilt }


zoom : Float -> Camera -> Camera
zoom factor cam =
    { cam | scale = max 8.0 (min 200.0 (cam.scale * factor)) }
