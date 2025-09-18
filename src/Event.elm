module Event exposing (..)

import EventTime exposing (..)
import Resource exposing (ResourceID)

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