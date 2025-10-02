module Resource exposing (..)

import Work exposing (Work(..))


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
cmpResourceID lhs rhs =
    Basics.compare (fetchResourceID lhs) (fetchResourceID rhs)


type ResourceState
    = Idle
    | Busy



--    | Interrupted


type Resource
    = Resource (Maybe QueueID) (List QueueID) ResourceState (Maybe Work)



--  | Generator QueueID
--  | Interruptor (List QueueID)


createResource : Maybe QueueID -> List QueueID -> Resource
createResource inp out =
    Resource inp out Idle Nothing


input : Resource -> Maybe QueueID
input (Resource inp _ _ _) =
    inp


output : Resource -> List QueueID
output (Resource _ out _ _) =
    out


state : Resource -> ResourceState
state (Resource _ _ st _) =
    st


work : Resource -> Maybe Work
work (Resource _ _ _ w) =
    w


putWork2Resource : Maybe Work -> Resource -> Resource
putWork2Resource maybework (Resource inp out st _) =
    case maybework of
        Just wrk ->
            Resource inp out Busy (Just wrk)

        Nothing ->
            Resource inp out st Nothing
