module ServiceTime exposing
    ( ServiceTime(..)
    , sample
    )

import Random


type ServiceTime
    = Exponential Float       -- mean = 1/rate; memoryless, peak at zero
    | LogNormal Float Float   -- mu sigma; median = exp(mu), right-skewed tail
    | Erlang Int Float        -- k phases at rate r; mean = k/r
    | Deterministic Int       -- always exactly N ticks
    | Uniform Int Int         -- uniform in [lo, hi] ticks


-- Sample a duration in ticks. `size` scales the result (job.size, default 1.0).
-- Always returns at least 1 tick.
sample : ServiceTime -> Float -> Random.Seed -> ( Int, Random.Seed )
sample st size seed =
    case st of
        Exponential rate ->
            let
                ( u, seed1 ) =
                    Random.step (Random.float 0.001 1.0) seed

                raw =
                    negate (logBase e u) / max 0.001 rate
            in
            ( max 1 (round (raw * size)), seed1 )

        LogNormal mu sigma ->
            let
                ( z, seed1 ) =
                    boxMuller seed

                raw =
                    e ^ (mu + sigma * z)
            in
            ( max 1 (round (raw * size)), seed1 )

        Erlang k rate ->
            sampleErlang k rate size seed

        Deterministic n ->
            ( max 1 (round (toFloat n * size)), seed )

        Uniform lo hi ->
            let
                ( n, seed1 ) =
                    Random.step (Random.int lo hi) seed
            in
            ( max 1 (round (toFloat n * size)), seed1 )


-- Box-Muller transform: two uniform samples → one standard normal sample.
boxMuller : Random.Seed -> ( Float, Random.Seed )
boxMuller seed =
    let
        ( u1, seed1 ) =
            Random.step (Random.float 0.001 1.0) seed

        ( u2, seed2 ) =
            Random.step (Random.float 0.0 1.0) seed1

        z =
            sqrt (negate 2.0 * logBase e u1) * cos (2.0 * pi * u2)
    in
    ( z, seed2 )


sampleErlang : Int -> Float -> Float -> Random.Seed -> ( Int, Random.Seed )
sampleErlang k rate size seed =
    let
        ( total, finalSeed ) =
            List.foldl
                (\_ ( acc, s ) ->
                    let
                        ( u, s1 ) =
                            Random.step (Random.float 0.001 1.0) s

                        phase =
                            negate (logBase e u) / max 0.001 rate
                    in
                    ( acc + phase, s1 )
                )
                ( 0.0, seed )
                (List.repeat k ())
    in
    ( max 1 (round (total * size)), finalSeed )
