module TestResource exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import EventTime exposing (EventTime(..))
import Html exposing (a)
import Tuple exposing (..)
import Resource exposing (createResource, QueueID(..), input, output)



expectAll : List Expectation -> Expectation
expectAll expectations =
    Expect.all (List.map always expectations) ()


resourceTestSuite : Test
resourceTestSuite = 
    describe "Resource module"
        [ describe "creating a resource"
            [ test "create " <|
                \_ ->
                    let
                        maybeQueueId = Just (QueueID 7)
                        resource = createResource maybeQueueId [(QueueID 3), (QueueID 7)]
                    in
                    expectAll
                        [ Expect.equal (input resource) maybeQueueId
                        , Expect.equal ((output resource) |> List.length) 2 ]
            ]
        ]
--    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"


