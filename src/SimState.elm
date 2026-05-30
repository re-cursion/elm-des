module SimState exposing
    ( SimState
    , init
    , isInterruptActive
    , nextJobID
    , getNode
    , getQueue
    , getLock
    , getJob
    , putNode
    , putQueue
    , putLock
    , putJob
    , removeJob
    , logEvent
    , scheduleEvent
    , popNextEvent
    )

import Dict exposing (Dict)
import Event exposing (Event)
import EventTime exposing (EventTime(..))
import Id exposing (JobID(..), LockID, NodeID(..), QueueID(..))
import Job exposing (Job)
import Lock exposing (LockState)
import Node exposing (NodeData, NodeKind(..))
import Queue exposing (Queue)
import Random


type alias SimState =
    { nodes      : Dict Int NodeData
    , queues     : Dict Int Queue
    , locks      : Dict String LockState
    , jobs       : Dict Int Job      -- all active jobs keyed by JobID int
    , seed       : Random.Seed
    , clock      : EventTime
    , eventQueue : List Event        -- sorted ascending by time
    , eventLog   : List Event        -- full history, newest first
    , nextID     : Int               -- counter for JobID generation
    }


init : Random.Seed -> SimState
init seed =
    { nodes      = Dict.empty
    , queues     = Dict.empty
    , locks      = Dict.empty
    , jobs       = Dict.empty
    , seed       = seed
    , clock      = EventTime 0
    , eventQueue = []
    , eventLog   = []
    , nextID     = 1
    }


isInterruptActive : SimState -> Bool
isInterruptActive state =
    Dict.values state.nodes
        |> List.any
            (\node ->
                case node.kind of
                    Worker cfg -> cfg.halted
                    _          -> False
            )


nextJobID : SimState -> ( JobID, SimState )
nextJobID state =
    ( JobID state.nextID, { state | nextID = state.nextID + 1 } )


getNode : NodeID -> SimState -> Maybe NodeData
getNode (NodeID nid) state =
    Dict.get nid state.nodes


getQueue : QueueID -> SimState -> Maybe Queue
getQueue (QueueID qid) state =
    Dict.get qid state.queues


getLock : LockID -> SimState -> Maybe LockState
getLock (Id.LockID lid) state =
    Dict.get lid state.locks


putNode : NodeID -> NodeData -> SimState -> SimState
putNode (NodeID nid) node state =
    { state | nodes = Dict.insert nid node state.nodes }


putQueue : QueueID -> Queue -> SimState -> SimState
putQueue (QueueID qid) queue state =
    { state | queues = Dict.insert qid queue state.queues }


putLock : LockID -> LockState -> SimState -> SimState
putLock (Id.LockID lid) lock state =
    { state | locks = Dict.insert lid lock state.locks }


getJob : JobID -> SimState -> Maybe Job
getJob (JobID jid) state =
    Dict.get jid state.jobs


putJob : Job -> SimState -> SimState
putJob job state =
    let
        (JobID jid) = job.id
    in
    { state | jobs = Dict.insert jid job state.jobs }


removeJob : JobID -> SimState -> SimState
removeJob (JobID jid) state =
    { state | jobs = Dict.remove jid state.jobs }


logEvent : Event -> SimState -> SimState
logEvent evt state =
    { state | eventLog = evt :: state.eventLog }


scheduleEvent : Event -> SimState -> SimState
scheduleEvent evt state =
    { state | eventQueue = insertSorted evt state.eventQueue }


popNextEvent : SimState -> Maybe ( Event, SimState )
popNextEvent state =
    case state.eventQueue of
        [] ->
            Nothing

        next :: rest ->
            Just ( next, { state | eventQueue = rest, clock = next.time } )


insertSorted : Event -> List Event -> List Event
insertSorted evt events =
    case events of
        [] ->
            [ evt ]

        head :: tail ->
            case Event.compareByTime evt head of
                GT ->
                    head :: insertSorted evt tail

                _ ->
                    evt :: head :: tail
