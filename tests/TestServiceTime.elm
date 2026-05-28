module TestServiceTime exposing (suite)

import Expect
import Fuzz exposing (floatRange, intRange)
import Random
import ServiceTime exposing (ServiceTime(..), sample)
import Test exposing (Test, describe, fuzz, fuzz2, test)


seed0 : Random.Seed
seed0 =
    Random.initialSeed 42


-- Draw N samples and return the list of tick values.
samples : Int -> ServiceTime -> Float -> List Int
samples n st size =
    List.foldl
        (\_ ( acc, s ) ->
            let
                ( t, s1 ) =
                    sample st size s
            in
            ( acc ++ [ t ], s1 )
        )
        ( [], seed0 )
        (List.repeat n ())
        |> Tuple.first


suite : Test
suite =
    describe "ServiceTime"
        [ describe "all variants return at least 1 tick"
            [ test "Exponential" <|
                \_ ->
                    samples 200 (Exponential 1.0) 1.0
                        |> List.all (\t -> t >= 1)
                        |> Expect.equal True

            , test "LogNormal" <|
                \_ ->
                    samples 200 (LogNormal 1.0 0.5) 1.0
                        |> List.all (\t -> t >= 1)
                        |> Expect.equal True

            , test "Erlang" <|
                \_ ->
                    samples 200 (Erlang 3 1.0) 1.0
                        |> List.all (\t -> t >= 1)
                        |> Expect.equal True

            , test "Deterministic" <|
                \_ ->
                    samples 20 (Deterministic 5) 1.0
                        |> List.all (\t -> t >= 1)
                        |> Expect.equal True

            , test "Uniform" <|
                \_ ->
                    samples 200 (Uniform 2 8) 1.0
                        |> List.all (\t -> t >= 1)
                        |> Expect.equal True
            ]

        , describe "Deterministic always returns the same value"
            [ test "size=1.0" <|
                \_ ->
                    samples 20 (Deterministic 7) 1.0
                        |> List.all (\t -> t == 7)
                        |> Expect.equal True

            , test "size=2.0 doubles the ticks" <|
                \_ ->
                    samples 20 (Deterministic 5) 2.0
                        |> List.all (\t -> t == 10)
                        |> Expect.equal True
            ]

        , describe "Uniform stays within [lo, hi] (scaled by size=1.0)"
            [ test "all samples in [2, 8]" <|
                \_ ->
                    samples 300 (Uniform 2 8) 1.0
                        |> List.all (\t -> t >= 2 && t <= 8)
                        |> Expect.equal True
            ]

        , describe "size scaling"
            [ test "doubling size roughly doubles Exponential mean" <|
                \_ ->
                    let
                        n =
                            500

                        mean ts =
                            toFloat (List.sum ts) / toFloat (List.length ts)

                        m1 =
                            mean (samples n (Exponential 0.5) 1.0)

                        m2 =
                            mean (samples n (Exponential 0.5) 2.0)
                    in
                    -- m2 should be roughly 2×m1; allow ±40% for randomness
                    Expect.within (Expect.Absolute (m1 * 0.4 + 1.0)) m2 (m1 * 2.0)

            , test "size=0.5 halves Deterministic" <|
                \_ ->
                    samples 10 (Deterministic 10) 0.5
                        |> List.all (\t -> t == 5)
                        |> Expect.equal True

            , fuzz (floatRange 0.1 5.0) "Deterministic 10 * size rounds correctly" <|
                \sz ->
                    let
                        expected =
                            max 1 (round (10.0 * sz))
                    in
                    samples 1 (Deterministic 10) sz
                        |> List.head
                        |> Maybe.withDefault 0
                        |> Expect.equal expected
            ]

        , describe "LogNormal samples are always positive"
            [ test "200 samples all > 0" <|
                \_ ->
                    samples 200 (LogNormal 0.0 1.0) 1.0
                        |> List.all (\t -> t > 0)
                        |> Expect.equal True
            ]

        , describe "Erlang mean grows with k"
            [ test "Erlang 4 has higher mean than Erlang 1 at same rate" <|
                \_ ->
                    let
                        n =
                            400

                        mean ts =
                            toFloat (List.sum ts) / toFloat (List.length ts)

                        m1 =
                            mean (samples n (Erlang 1 1.0) 1.0)

                        m4 =
                            mean (samples n (Erlang 4 1.0) 1.0)
                    in
                    Expect.greaterThan m1 m4
            ]
        ]
