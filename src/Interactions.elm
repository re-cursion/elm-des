module Interactions exposing (..)

import Resource exposing (Resource(..), QueueID, ResourceState(..), state, ResourceID(..), work)
import Event exposing (Event(..))
import Dict
import Event exposing (Event(..), EventType(..))
import EventTime exposing (EventTime(..), addTimes, eventTime2Int)
import Html exposing (i)
import Queue exposing (Behaviour, PutResult(..), Queue(..), put, take, putQueue, putResult)
import Resource exposing (QueueID, Resource(..), ResourceID(..), ResourceState(..), putWork2Resource, state)
import Work exposing (Work(..))
import Event exposing (Event(..), EventType(..))

import Queue exposing (Queue(..), put, take, PutResult(..), Behaviour(..))
import Resource exposing (ResourceState(..), putWork2Resource)
import Html exposing (i)
import Dict



type InteractionResult
    = InteractionResult Queue Resource (List Event)


work2EventTime : Work -> ResourceID -> EventTime
work2EventTime (Work _ times) (ResourceID resid) =
    let
        match =
            Dict.get resid times
    in
    case match of
        Just m ->
            EventTime (eventTime2Int m)

        Nothing ->
            EventTime 0


queue2Resource : EventTime -> Queue -> (ResourceID, Resource) -> InteractionResult
queue2Resource eventtime queue (resid, resource) = 
    case (state resource) of
        Busy ->
            InteractionResult queue resource []

        Idle ->
            let
                ( maybework, queue_ ) =
                    take queue

                serviceCompleteTime =
                    maybework
                        |> Maybe.map
                            (\work ->
                                addTimes eventtime (work2EventTime work resid)
                            )
            in
            case serviceCompleteTime of
                Just svCmpltTime ->
                    InteractionResult queue_ (putWork2Resource maybework resource) [ Event svCmpltTime (ServiceComplete resid) ]

                Nothing ->
                    InteractionResult queue resource []




justElse : (a -> b) -> b -> Maybe a -> b
justElse fncAB dflt maybeA = 
    case maybeA of
        Just val ->
            fncAB val
        Nothing ->
            dflt



resource2Queue : (ResourceID, Resource) -> (QueueID, Queue) -> EventTime -> InteractionResult
resource2Queue (resid, resource) (qid, queue) eventtime =
    let
        work = Resource.work resource
        pr = work |> justElse 
                        (\w -> Queue.put w queue)
                        (NoWork queue)
    in
    case pr of
        Queue.NoWork q ->
            InteractionResult q resource []
        Queue.Ok q ->
            InteractionResult (putQueue pr) (putWork2Resource work resource) [Event eventtime (R2Q resid qid) ]
        Queue.Err bhvr q ->
            case bhvr of
                Queue.Block ->
                    InteractionResult q resource []
                Queue.DropFirst ->
                    InteractionResult q resource [Event Queue.Drop qid (work |> Maybe.withDefault )]
                Queue.DropLast ->
                    InteractionResult q resource []



