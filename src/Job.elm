module Job exposing
    ( Job
    , Priority(..)
    , newJob
    , priorityRank
    , comparePriority
    )

import EventTime exposing (EventTime)
import Id exposing (JobID, NodeID, QueueID)


type Priority
    = Low
    | Normal
    | High
    | Critical


type alias Job =
    { id        : JobID
    , priority  : Priority
    , label     : String
    , arrivedAt : EventTime
    }


newJob : JobID -> Priority -> String -> EventTime -> Job
newJob id_ pri lbl time =
    { id = id_, priority = pri, label = lbl, arrivedAt = time }


priorityRank : Priority -> Int
priorityRank p =
    case p of
        Low      -> 0
        Normal   -> 1
        High     -> 2
        Critical -> 3


comparePriority : Priority -> Priority -> Order
comparePriority a b =
    compare (priorityRank a) (priorityRank b)
