module Types exposing (Time(..), Event(..), EventType(..), Model, Resource, ResourceID(..), cmpResourceID, ResourceViewInfo, QueueID(..), Work, WorkID(..), fetchResourceID, fetchQueueID, fetchWorkID, eventTime, fetchTime, compareEventTimes, compareTimes, eventResourceID, addTimes, eventType)

import Dict exposing (Dict)
import Process exposing (Id)


type alias ResourceViewInfo =
    { x : Float
    , y : Float
    }


type EventType
    = ServiceComplete
    | FetchTask



--    | Interrupt


type Time 
    = Time Int

fetchTime : Time -> Int
fetchTime (Time time)
    = time


addTimes : Time -> Time -> Time
addTimes t0 t1 = 
    (Time ((t0 |> fetchTime) + (t1 |> fetchTime)))


type Event =
    Event Time ResourceID EventType


eventTime : Event -> Time
eventTime (Event time _ _) = 
    time

eventType : Event -> EventType
eventType (Event _ _ tp) = 
    tp


eventResourceID : Event -> ResourceID
eventResourceID (Event _ nid _) = 
    nid



compareTimes : Time -> Time -> Order
compareTimes tim0 tim1 = 
    (compare (tim0 |> fetchTime) (tim1 |> fetchTime))


compareEventTimes : Event -> Event -> Order
compareEventTimes evt0 evt1 = 
    (compareTimes (evt0 |> eventTime) (evt1 |> eventTime))


type alias Model =
    { resources : Dict Int Resource
  --  , queues : Dict Int Queue
    , events : List Event
    , currentTime : Time
    }


type WorkID
    = WorkID Int


type QueueID
    = QueueID Int


type ResourceID
    = ResourceID Int

cmpResourceID : ResourceID -> ResourceID -> Order
cmpResourceID lhs rhs = Basics.compare (fetchResourceID lhs) (fetchResourceID rhs)

fetchWorkID : WorkID -> Int
fetchWorkID (WorkID tid) =
    tid


fetchQueueID : QueueID -> Int
fetchQueueID (QueueID qid) =
    qid


fetchResourceID : ResourceID -> Int
fetchResourceID (ResourceID nid) =
    nid




type alias Work =
    { id : WorkID
    , serviceTime : Time
    }






type alias Resource =
    { inputQueue : Maybe QueueID
    , outputQueues : List QueueID
    , view : ResourceViewInfo
    , busy : Bool
    , currentTask : Maybe Work
    }

