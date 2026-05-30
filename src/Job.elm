module Job exposing
    ( Job
    , Priority(..)
    , newJob
    , priorityRank
    , comparePriority
    )

import Event exposing (EventType)
import EventTime exposing (EventTime)
import Id exposing (JobID)


type Priority
    = Low
    | Normal
    | High
    | Critical


type alias Job =
    { id        : JobID
    , priority  : Priority
    , size      : Float      -- scales service duration (1.0 = baseline)
    , label     : String
    , arrivedAt : EventTime
    , history   : List ( EventTime, EventType )  -- stamped at each major transition
    }


newJob : JobID -> Priority -> Float -> String -> EventTime -> Job
newJob id_ pri sz lbl time =
    { id = id_, priority = pri, size = sz, label = lbl, arrivedAt = time, history = [] }


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
