module TestInteractions exposing (..)

import Queue exposing (..)
import Resource exposing (..)
import EventTime exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Html exposing (a)
import Tuple exposing (..)
import Resource exposing (createResource, QueueID(..), input, output, resourceWork)
import Interactions exposing (..)
import Work exposing (..)
import Dict exposing (Dict) 
import Event exposing (Event(..), eventTime)


expectAll : List Expectation -> Expectation
expectAll expectations =
    Expect.all (List.map always expectations) ()


workN : Int -> Work
workN n =
    Work (WorkID n)
        (Dict.fromList
            [ ( 1, EventTime 11 )
            , ( 2, EventTime 22 )
            , ( 3, EventTime 33 )
            , ( 4, EventTime 44 )
            ]
        )


interactionTestSuite : Test
interactionTestSuite = 
    describe "Interactions module"
        [ describe "move work from queue to resource"
            [ test "queue -> resource " <|
                \_ ->
                    let
                        work = workN 1
                        queue = Queue {size = 5, behaviour = Block} [work]
                        maybeQueueId = Just (QueueID 1)
                        resource = createResource maybeQueueId [(QueueID 3), (QueueID 7)]
                        -- take work from queue and put it into the resource
                        -- queue2Resource queue (resid, resource) eventtime = 
                        result = queue2Resource queue ((ResourceID 1), resource) (EventTime 7)
                        (queue_, resource_, events_) = case result of
                            InteractionResult q r e ->
                                (q, r, e)
                    in 
                    expectAll
                        [ Expect.equal (queue_ |> tasks |> List.length) 0
                        , Expect.equal (resource_ |> resourceWork) (Just work)
                        , Expect.equal (events_ |> List.head |> Maybe.andThen (\evt -> (Just (eventTime evt)))) (Just (EventTime 18))]
            ]
        ]
--    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"

