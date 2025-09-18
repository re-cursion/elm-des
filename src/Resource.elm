module Resource exposing (..)

import Work exposing (Work)



type QueueID
    = QueueID Int

type ResourceID
    = ResourceID Int 


fetchResourceID : ResourceID -> Int
fetchResourceID (ResourceID nid) =
    nid


fetchQueueID : QueueID -> Int
fetchQueueID (QueueID qid) =
    qid




cmpResourceID : ResourceID -> ResourceID -> Order
cmpResourceID lhs rhs = Basics.compare (fetchResourceID lhs) (fetchResourceID rhs)



type alias Resource =
    { inputQueue : Maybe QueueID
    , outputQueues : List QueueID
   -- , view : ResourceViewInfo
    , busy : Bool
    , currentTask : Maybe Work
    }