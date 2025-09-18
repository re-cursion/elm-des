module TestResource exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Types exposing (..)
import Queue exposing (Behaviour(..), Queue(..), config, put, take, tasks, PutResult(..))
import Work exposing (WorkID(..), Work)
import EventTime exposing (EventTime(..))
import Html exposing (a)
import Tuple exposing (..)



suite : Test
suite = 
    describe "Queue module"
        [ describe "putting and testing queue"
            [
                todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
            ]
        ]
--    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"


