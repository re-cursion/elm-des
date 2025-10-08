module Interactions exposing (..)

import Resource exposing (Resource(..), QueueID, ResourceState(..), state, ResourceID(..), work)
import Event exposing (Event(..))
import Dict
import Event exposing (Event(..), EventType(..))
import EventTime exposing (EventTime(..), addTimes, eventTime2Int)
import Html exposing (i)
import Queue exposing (Behaviour, PutResult(..), Queue(..), put, take, putQueue, putResult)
import Resource exposing (QueueID, Resource(..), ResourceID(..), ResourceState(..), putWork2Resource, state)
import Work exposing (Work(..), WorkID(..), workIDValue, workID)
import Event exposing (Event(..), EventType(..))

import Queue exposing (Queue(..), put, take, PutResult(..), Behaviour(..))
import Resource exposing (ResourceState(..), putWork2Resource)
import Html exposing (i)
import Dict



type InteractionResult
    = InteractionResult Queue Resource (List Event)
    | NoInteraction


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
            NoInteraction

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
                    NoInteraction




justOrElse : b -> (a -> b) -> Maybe a -> b
justOrElse dflt fncAB maybeA = 
    case maybeA of
        Just val ->
            fncAB val
        Nothing ->
            dflt





resourceWork2Queue_ : (ResourceID, Resource, Work) -> (QueueID, Queue) -> EventTime -> InteractionResult
resourceWork2Queue_ (resid, resource, work) (qid, queue) eventtime =
    let            
        pr = Queue.put work queue -- move the work onto the queue
    in
    case pr of
        Queue.Ok q enqueuedWork ->
            InteractionResult q (putWork2Resource Nothing resource) {-remove work from resource-} [Event eventtime (R2Q resid qid (enqueuedWork |> workID))]
        Queue.Err bhvr q droppedOrBlockedWork ->
            let
                wid = workID work
            in
            case bhvr of
                Queue.Block ->
                    InteractionResult q resource [Event eventtime (Event.Block qid wid)]
                _ -> -- DropFirst | DropLast
                    InteractionResult q resource [Event eventtime (Event.R2Q resid qid wid) -- moved work
                                                ,Event eventtime (Event.Drop qid (workID droppedOrBlockedWork))] -- work dropped from queue




resource2Queue : (ResourceID, Resource) -> (QueueID, Queue) -> EventTime -> InteractionResult
resource2Queue (resid, resource) (qid, queue) eventtime =
    let
        work = Resource.work resource
    in
    case work of
        Nothing ->
            NoInteraction
        Just justWork ->
            resourceWork2Queue_ (resid, resource, justWork) (qid, queue) eventtime



