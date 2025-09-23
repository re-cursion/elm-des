module Interactions exposing (..)

import Resource exposing (Resource(..), QueueID, ResourceState(..), state, ResourceID(..))
import Event exposing (Event(..))
import EventTime exposing (EventTime(..), addTimes, eventTime2Int)
import Work exposing (Work(..))
import Event exposing (Event(..), EventType(..))

import Queue exposing (Queue(..), put, take, PutResult, Behaviour)
import Resource exposing (ResourceState(..), putWork2Resource)
import Html exposing (i)
import Dict



type InteractionResult
    = InteractionResult Queue Resource (List Event)


work2EventTime : Work -> ResourceID -> EventTime
work2EventTime (Work _ times) (ResourceID resid) =
    let
        match = Dict.get resid times
    in
    case match of
        Just m ->
            EventTime (eventTime2Int m)
        Nothing ->
            EventTime 0


queue2Resource : Queue -> (ResourceID, Resource) -> EventTime -> InteractionResult
queue2Resource queue (resid, resource) eventtime = 
    case (state resource) of
        Busy ->
            InteractionResult queue resource []
        Idle ->
            let
                (maybework, queue_) = take queue
                serviceCompleteTime = 
                    case maybework of
                        Just work ->
                            addTimes eventtime (work2EventTime work resid)
                        Nothing ->
                            EventTime 0
                serviceCompleteEvent = Event serviceCompleteTime resid ServiceComplete
            in
            InteractionResult queue_ (putWork2Resource maybework resource) [serviceCompleteEvent]


-- resource2Queue : (ResourceID, Resource) -> Queue -> EventTime -> InteractionResult
-- resource2Queue (resid, resource) queue eventtime =
