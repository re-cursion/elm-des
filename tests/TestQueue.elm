module TestQueue exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Types exposing (..)
import Queue exposing (Behaviour(..), Queue(..), config, put, take, tasks, PutResult (..))

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
                        queue = Queue { size = 5, behaviour = DropFirst } []
                        (updatedQueue, result) = put queue (Work (WorkID 1) (Time 5))
                    in
                        Expect.equal (updatedQueue |> tasks |> List.length) 1
            ]
        ]
--    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"


