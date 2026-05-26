module Event exposing
    ( Event
    , EventType(..)
    , event
    , eventTime
    , eventType
    , compareByTime
    )

import EventTime exposing (EventTime, compareTimes)
import Id exposing (JobID, LockID, NodeID, QueueID)


type EventType
    = JobArrived       NodeID JobID
    | ServiceStarted   NodeID JobID
    | ServiceComplete  NodeID JobID
    | JobEnqueued      QueueID JobID
    | JobDequeued      QueueID NodeID JobID
    | JobBlocked       QueueID JobID
    | JobDropped       QueueID JobID
    | SignoffRequested NodeID LockID JobID
    | SignoffStarted   NodeID LockID JobID
    | SignoffComplete  NodeID LockID JobID
    | MeetingStarted
    | MeetingEnded


type alias Event =
    { time : EventTime
    , kind : EventType
    }


event : EventTime -> EventType -> Event
event t k =
    { time = t, kind = k }


eventTime : Event -> EventTime
eventTime e =
    e.time


eventType : Event -> EventType
eventType e =
    e.kind


compareByTime : Event -> Event -> Order
compareByTime a b =
    compareTimes a.time b.time
