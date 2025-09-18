module Types exposing (Model, ResourceViewInfo)

import Dict exposing (Dict)
import Process exposing (Id)
import Work exposing (Work)
import EventTime exposing (EventTime, compareTimes) 
import Event exposing (..)
import Resource exposing (..)



type alias ResourceViewInfo =
    { x : Float
    , y : Float
    }






type alias Model =
    { resources : Dict Int Resource
  --  , queues : Dict Int Queue
    , events : List Event
    , currentTime : EventTime
    }













