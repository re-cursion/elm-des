module TestQueue exposing (..)

import Dict exposing (Dict)
import EventTime exposing (EventTime(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html exposing (a)
import Queue exposing (Behaviour(..), PutResult(..), Queue(..), config, put, putQueue, putResult, take, tasks)
import Test exposing (..)
import Tuple exposing (..)
import Types exposing (..)
import Work exposing (Work(..), WorkID(..))


expectAll : List Expectation -> Expectation
expectAll expectations =
    Expect.all (List.map always expectations) ()


lastElement : List a -> Maybe a
lastElement =
    List.foldl (Just >> always) Nothing



-- helper function for tests: create 'Work' easily


workN : Int -> Work
workN n =
    Work (WorkID n)
        (Dict.fromList
            [ ( 1, EventTime 1 )
            , ( 2, EventTime 2 )
            ]
        )



-- Work WorkID (Dict Int EventTime)


queueTestSuite : Test
queueTestSuite =
    describe "Queue module"
        [ describe "putting and testing queue"
            [ test "create " <|
                \_ ->
                    let
                        queue =
                            Queue { size = 5, behaviour = DropFirst } []
                    in
                    Expect.equal (.size (config queue)) 5
            , test "put work onto the queue" <|
                \_ ->
                    let
                        pr =
                            Queue { size = 5, behaviour = DropFirst } []
                                |> put (workN 1)
                                |> putQueue
                                |> put (workN 2)
                                |> putQueue
                                |> put (workN 3)
                                |> putQueue
                                |> put (workN 4)
                                |> putQueue
                                |> put (workN 5)

                        queue =
                            pr |> putQueue

                        result =
                            pr |> putResult
                    in
                    expectAll
                        [ Expect.equal (queue |> tasks |> List.length) 5
                        , Expect.equal (queue |> tasks |> lastElement) (Just (workN 5))
                        , Expect.equal result "Ok"
                        ]
            , test "put more work onto the queue than allowed, DropFirst" <|
                \_ ->
                    let
                        pr =
                            Queue { size = 5, behaviour = DropFirst } []
                                |> put (workN 1)
                                |> putQueue
                                |> put (workN 2)
                                |> putQueue
                                |> put (workN 3)
                                |> putQueue
                                |> put (workN 4)
                                |> putQueue
                                |> put (workN 5)

                        element =
                            workN 6

                        pr2 =
                            put element (putQueue pr)

                        updatedQueue =
                            pr2 |> putQueue

                        result =
                            pr2 |> putResult
                    in
                    expectAll
                        [ Expect.equal (updatedQueue |> tasks |> List.length) 5
                        , Expect.equal result "FirstDropped"
                        , Expect.equal (tasks updatedQueue |> lastElement) (Just element) |> Expect.onFail (Debug.toString updatedQueue)
                        ]
            , test "put more work onto the queue than allowed, DropLast" <|
                \_ ->
                    let
                        element =
                            workN 1

                        pr =
                            Queue { size = 5, behaviour = DropLast } []
                                |> put element
                                |> putQueue
                                |> put (workN 2)
                                |> putQueue
                                |> put (workN 3)
                                |> putQueue
                                |> put (workN 4)
                                |> putQueue
                                |> put (workN 5)

                        pr2 =
                            put (workN 6) (putQueue pr)
                    in
                    expectAll
                        [ Expect.equal (pr2 |> putQueue |> tasks |> List.length) 5
                        , Expect.equal (pr2 |> putResult) "LastDropped"
                        , Expect.equal (tasks (pr2 |> putQueue) |> List.head) (Just (workN 1)) |> Expect.onFail (Debug.toString (pr2 |> putQueue))
                        ]
            , test "put more work onto the queue than allowed, Block" <|
                \_ ->
                    let
                        element =
                            workN 1

                        pr =
                            Queue { size = 5, behaviour = Block } []
                                |> put element
                                |> putQueue
                                |> put (workN 2)
                                |> putQueue
                                |> put (workN 3)
                                |> putQueue
                                |> put (workN 4)
                                |> putQueue
                                |> put (workN 5)

                        pr2 =
                            put (workN 6) (pr |> putQueue)
                    in
                    expectAll
                        [ Expect.equal (pr2 |> putQueue |> tasks |> List.length) 5
                        , Expect.equal (pr2 |> putResult) "Blocked"
                        , Expect.equal (tasks (pr2 |> putQueue) |> List.head) (Just (workN 1)) |> Expect.onFail (Debug.toString (pr2 |> putQueue))
                        , Expect.equal (tasks (pr2 |> putQueue) |> lastElement) (Just (workN 5)) |> Expect.onFail (Debug.toString (pr2 |> putQueue))
                        ]
            , test "take work from the queue" <|
                \_ ->
                    let
                        queue =
                            Queue { size = 5, behaviour = Block } []
                                |> put (workN 1)
                                |> putQueue
                                |> put (workN 2)
                                |> putQueue
                                |> put (workN 3)
                                |> putQueue
                                |> put (workN 4)
                                |> putQueue
                                |> put (workN 5)
                                |> putQueue

                        ( maybeWork, remaining ) =
                            take queue
                    in
                    expectAll
                        [ Expect.equal (remaining |> tasks |> List.length) 4 |> Expect.onFail (Debug.toString remaining)
                        , Expect.equal maybeWork (Just (workN 1)) |> Expect.onFail (Debug.toString maybeWork)
                        ]
            , test "take all the work from the queue and one more" <|
                \_ ->
                    let
                        queue =
                            Queue { size = 5, behaviour = Block } []
                                |> put (workN 1)
                                |> putQueue
                                |> put (workN 2)
                                |> putQueue
                                |> put (workN 3)
                                |> putQueue
                                |> put (workN 4)
                                |> putQueue
                                |> put (workN 5)
                                |> putQueue

                        ( maybeWork, remaining ) =
                            take queue |> second |> take |> second |> take |> second |> take |> second |> take |> second |> take
                    in
                    expectAll
                        [ Expect.equal (remaining |> tasks |> List.length) 0 |> Expect.onFail (Debug.toString remaining)
                        , Expect.equal maybeWork Nothing |> Expect.onFail (Debug.toString maybeWork)
                        ]
            ]
        ]



--    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
