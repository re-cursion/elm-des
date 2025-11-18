module Event exposing (..)

import EventTime exposing (..)
import Resource exposing (ResourceID, QueueID)
import Queue exposing (..) 
import Work exposing (..) 



type EventType
    = FetchTask ResourceID
    | ServiceComplete ResourceID
    | Q2R QueueID ResourceID WorkID
    | R2Q ResourceID QueueID WorkID
    | Drop QueueID WorkID
    | Block QueueID WorkID
--    | Interrupt


type Event
    = Event EventTime EventType


eventTime : Event -> EventTime
eventTime (Event time _) =
    time


eventType : Event -> EventType
eventType (Event _ tp) =
    tp

isEventType : Event -> Bool
isEventType (Event _ tp) =
    case tp of
        (EventType type) -> type


compareEventTimes : Event -> Event -> Order
compareEventTimes evt0 evt1 =
    compareTimes (evt0 |> eventTime) (evt1 |> eventTime)
