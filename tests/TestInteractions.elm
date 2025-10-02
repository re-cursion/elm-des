module TestInteractions exposing (..)

import Dict exposing (Dict)
import Event exposing (Event(..), eventTime)
import EventTime exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html exposing (a)
import Interactions exposing (..)
import Queue exposing (..)
import Resource exposing (..)
import Test exposing (..)
import Tuple exposing (..)
import Work exposing (..)


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
                        work =
                            workN 1

                        queue =
                            Queue { size = 5, behaviour = Block } [ work ]

                        maybeQueueId =
                            Just (QueueID 1)

                        resource =
                            createResource maybeQueueId [ QueueID 3, QueueID 7 ]

                        -- take work from queue and put it into the resource
                        result =
                            queue2Resource (EventTime 7) queue ( ResourceID 2, resource )

                        ( queue_, resource_, events_ ) =
                            case result of
                                InteractionResult q r e ->
                                    ( q, r, e )
                    in
                    expectAll
                        [ Expect.equal (queue_ |> tasks |> List.length) 0
                        , Expect.equal (resource_ |> Resource.work) (Just work)
                        , Expect.equal (events_ |> List.head |> Maybe.andThen (\evt -> Just (eventTime evt))) (Just (EventTime 29))
                        ]
            , test "empty queue -> resource " <|
                \_ ->
                    let
                        queue =
                            Queue { size = 5, behaviour = Block } []

                        maybeQueueId =
                            Just (QueueID 1)

                        resource =
                            createResource maybeQueueId [ QueueID 3, QueueID 7 ]

                        -- take work from queue and put it into the resource
                        result =
                            queue2Resource (EventTime 7) queue ( ResourceID 2, resource )

                        ( queue_, resource_, events_ ) =
                            case result of
                                InteractionResult q r e ->
                                    ( q, r, e )
                    in
                    expectAll
                        [ Expect.equal (queue_ |> tasks |> List.length) 0
                        , Expect.equal (resource_ |> Resource.work) Nothing
                        , Expect.equal (events_ |> List.head) Nothing
                        ]
            ]
        ]



--    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
