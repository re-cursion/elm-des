module TestQueue exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Types exposing (..)
import Queue exposing (Behaviour(..), Queue(..), config, put, take, tasks, PutResult(..))
import Work exposing (WorkID(..), Work)
import EventTime exposing (EventTime(..))
import Html exposing (a)
import Tuple exposing (..)



expectAll : List Expectation -> Expectation
expectAll expectations =
    Expect.all (List.map always expectations) ()

lastElement : List a -> Maybe a
lastElement = List.foldl (Just >> always) Nothing

-- helper function for tests: create 'Work' easily
workN : Int -> Work
workN n = Work (WorkID n) (EventTime 10)

suite : Test
suite = 
    describe "Queue module"
        [ describe "putting and testing queue"
            [ test "create " <|
                \_ ->
                    let
                        queue = Queue { size = 5, behaviour = DropFirst } []
                    in
                        Expect.equal (.size (config queue)) 5
            , test "put work onto the queue" <|
                \_ ->
                    let
                        (queue, result) = Queue { size = 5, behaviour = DropFirst } []
                            |> put (workN 1) |> first |> put (workN 2) |> first |> put (workN 3) |> first |> put (workN 4) |> first |> put (workN 5)
                    in
                        expectAll
                                [ Expect.equal (queue |> tasks |> List.length) 5
                                , Expect.equal (queue |> tasks |> lastElement) (Just (workN 5))
                                , Expect.equal result Queue.Ok ]
            , test "put more work onto the queue than allowed, DropFirst" <|
                \_ ->
                    let 
                        (queue, _) = Queue { size = 5, behaviour = DropFirst } []
                            |> put (workN 1) |> first |> put (workN 2) |> first |> put (workN 3) |> first |> put (workN 4) |> first |> put (workN 5)
                        element = workN 6
                        (updatedQueue, result) = put element queue 
                    in
                        expectAll
                            [ (Expect.equal (updatedQueue |> tasks |> List.length) 5)
                            , (Expect.equal result FirstDropped)
                            , (Expect.equal (tasks updatedQueue |> lastElement) (Just element)) |> Expect.onFail (Debug.toString queue) ]
            , test "put more work onto the queue than allowed, DropLast" <|
                \_ ->
                    let 
                        element = workN 1
                        (queue, _) = Queue { size = 5, behaviour = DropLast } []
                            |> put element |> first |> put (workN 2) |> first |> put (workN 3) |> first |> put (workN 4) |> first |> put (workN 5)
                        (updatedQueue, result) = put (workN 6) queue 
                    in
                        expectAll
                            [ (Expect.equal (updatedQueue |> tasks |> List.length) 5)
                            , (Expect.equal result LastDropped)
                            , (Expect.equal (tasks updatedQueue |> List.head) (Just (workN 1))) |> Expect.onFail (Debug.toString queue) ]
            , test "put more work onto the queue than allowed, Block" <|
                \_ ->
                    let 
                        element = workN 1
                        (queue, _) = Queue { size = 5, behaviour = Block } []
                            |> put element |> first |> put (workN 2) |> first |> put (workN 3) |> first |> put (workN 4) |> first |> put (workN 5)
                        (updatedQueue, result) = put (workN 6) queue 
                    in
                        expectAll
                            [ (Expect.equal (updatedQueue |> tasks |> List.length) 5)
                            , (Expect.equal result Blocked)
                            , (Expect.equal (tasks updatedQueue |> List.head) (Just (workN 1))) |> Expect.onFail (Debug.toString queue) 
                            , (Expect.equal (tasks updatedQueue |> lastElement) (Just (workN 5))) |> Expect.onFail (Debug.toString queue) ]
            , test "take work from the queue" <|
                \_ ->
                    let 
                        (queue, _) = Queue { size = 5, behaviour = Block } []
                            |> put (workN 1) |> first |> put (workN 2) |> first |> put (workN 3) |> first |> put (workN 4) |> first |> put (workN 5)
                        (maybeWork, remaining) = take queue
                    in
                        expectAll
                            [ (Expect.equal (remaining |> tasks |> List.length) 4 |> Expect.onFail (Debug.toString remaining))
                            , (Expect.equal maybeWork (Just (workN 1)))  |> Expect.onFail (Debug.toString maybeWork) ]
            , test "take all the work from the queue and one more" <|
                \_ ->
                    let 
                        (queue, _) = Queue { size = 5, behaviour = Block } []
                            |> put (workN 1) |> first |> put (workN 2) |> first |> put (workN 3) |> first |> put (workN 4) |> first |> put (workN 5)
                        (maybeWork, remaining) = take queue |> second |> take |> second |> take |> second |> take |> second |> take |>second |> take
                    in
                        expectAll
                            [ (Expect.equal (remaining |> tasks |> List.length) 0 |> Expect.onFail (Debug.toString remaining))
                            , (Expect.equal maybeWork Nothing)  |> Expect.onFail (Debug.toString maybeWork) ]
            ]
        ]
--    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"


