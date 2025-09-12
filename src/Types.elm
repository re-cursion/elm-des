module Types exposing (Time(..), Event(..), EventType(..), Model, Node, NodeID(..), NodeViewInfo, Queue, QueueID(..), Work, WorkID(..), fetchNodeID, fetchQueueID, fetchWorkID, eventTime, fetchTime, compareEventTimes, compareTimes, eventNodeID, addTimes, eventType)

import Process exposing (Id)


type alias NodeViewInfo =
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
    Event Time NodeID EventType


eventTime : Event -> Time
eventTime (Event time _ _) = 
    time

eventType : Event -> EventType
eventType (Event _ _ tp) = 
    tp


eventNodeID : Event -> NodeID
eventNodeID (Event _ nid _) = 
    nid



compareTimes : Time -> Time -> Order
compareTimes tim0 tim1 = 
    (compare (tim0 |> fetchTime) (tim1 |> fetchTime))


compareEventTimes : Event -> Event -> Order
compareEventTimes evt0 evt1 = 
    (compareTimes (evt0 |> eventTime) (evt1 |> eventTime))


type alias Model =
    { nodes : List Node
    , queues : List Queue
    , events : List Event
    , currentTime : Time
    }


type WorkID
    = WorkID Int


type QueueID
    = QueueID Int


type NodeID
    = NodeID Int


fetchWorkID : WorkID -> Int
fetchWorkID (WorkID tid) =
    tid


fetchQueueID : QueueID -> Int
fetchQueueID (QueueID qid) =
    qid


fetchNodeID : NodeID -> Int
fetchNodeID (NodeID nid) =
    nid


type alias Work =
    { id : WorkID
    , serviceTime : Time
    }


type alias Queue =
    { id : QueueID, tasks : List Work }


type alias Node =
    { id : NodeID
    , inputQueue : Maybe QueueID
    , outputQueues : List QueueID
    , view : NodeViewInfo
    , busy : Bool
    , currentTask : Maybe Work
    }
