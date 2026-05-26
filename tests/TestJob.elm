module TestJob exposing (suite)

import Expect
import Fuzz exposing (oneOfValues)
import Job exposing (Priority(..), comparePriority, priorityRank)
import Test exposing (Test, describe, fuzz2, test)


suite : Test
suite =
    describe "Job / Priority"
        [ describe "priorityRank"
            [ test "Low < Normal" <|
                \_ -> Expect.lessThan (priorityRank Normal) (priorityRank Low)

            , test "Normal < High" <|
                \_ -> Expect.lessThan (priorityRank High) (priorityRank Normal)

            , test "High < Critical" <|
                \_ -> Expect.lessThan (priorityRank Critical) (priorityRank High)

            , test "all ranks distinct" <|
                \_ ->
                    let
                        ranks =
                            List.map priorityRank [ Low, Normal, High, Critical ]
                    in
                    Expect.equal (List.length ranks) (List.length (dedupe ranks))
            ]

        , describe "comparePriority"
            [ test "same priority is EQ" <|
                \_ -> Expect.equal EQ (comparePriority Normal Normal)

            , test "Low vs Critical is LT" <|
                \_ -> Expect.equal LT (comparePriority Low Critical)

            , test "Critical vs Low is GT" <|
                \_ -> Expect.equal GT (comparePriority Critical Low)

            , fuzz2 anyPriority anyPriority "antisymmetric" <|
                \a b ->
                    let
                        ab =
                            comparePriority a b

                        ba =
                            comparePriority b a
                    in
                    case ( ab, ba ) of
                        ( LT, GT ) ->
                            Expect.pass

                        ( GT, LT ) ->
                            Expect.pass

                        ( EQ, EQ ) ->
                            Expect.pass

                        _ ->
                            Expect.fail
                                ("comparePriority should be antisymmetric, got "
                                    ++ Debug.toString ab
                                    ++ " and "
                                    ++ Debug.toString ba
                                )
            ]
        ]


anyPriority : Fuzz.Fuzzer Priority
anyPriority =
    oneOfValues [ Low, Normal, High, Critical ]


dedupe : List Int -> List Int
dedupe =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc
            else
                acc ++ [ x ]
        )
        []
