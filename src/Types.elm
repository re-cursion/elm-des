module Types exposing (Event(..), EventType(..), Model, Resource, ResourceID(..), cmpResourceID, ResourceViewInfo, QueueID(..), fetchResourceID, fetchQueueID, eventTime, compareEventTimes, eventResourceID, eventType)

import Dict exposing (Dict)
import Process exposing (Id)
import Work exposing (Work)
import EventTime exposing (EventTime, compareTimes) 
import Event exposing (..)



type alias ResourceViewInfo =
    { x : Float
    , y : Float
    }


type EventType
    = ServiceComplete
    | FetchTask



--    | Interrupt






type Event =
    Event EventTime ResourceID EventType


eventTime : Event -> EventTime
eventTime (Event time _ _) = 
    time

eventType : Event -> EventType
eventType (Event _ _ tp) = 
    tp


eventResourceID : Event -> ResourceID
eventResourceID (Event _ nid _) = 
    nid





compareEventTimes : Event -> Event -> Order
compareEventTimes evt0 evt1 = 
    (compareTimes (evt0 |> eventTime) (evt1 |> eventTime))


type alias Model =
    { resources : Dict Int Resource
  --  , queues : Dict Int Queue
    , events : List Event
    , currentTime : EventTime
    }




type QueueID
    = QueueID Int


type ResourceID
    = ResourceID Int

cmpResourceID : ResourceID -> ResourceID -> Order
cmpResourceID lhs rhs = Basics.compare (fetchResourceID lhs) (fetchResourceID rhs)




fetchQueueID : QueueID -> Int
fetchQueueID (QueueID qid) =
    qid


fetchResourceID : ResourceID -> Int
fetchResourceID (ResourceID nid) =
    nid










type alias Resource =
    { inputQueue : Maybe QueueID
    , outputQueues : List QueueID
    , view : ResourceViewInfo
    , busy : Bool
    , currentTask : Maybe Work
    }

