module Node exposing
    ( NodeKind(..)
    , NodeState(..)
    , SourceConfig
    , WorkerConfig
    , DispatcherConfig
    , DispatchRule(..)
    , NodeData
    , makeSource
    , makeWorker
    , makeSink
    , isIdle
    )

import EventTime exposing (EventTime)
import Id exposing (JobID, LockID, QueueID)
import Job exposing (Priority(..))
import ServiceTime exposing (ServiceTime)


type DispatchRule
    = RoundRobin
    | ShortestQueue
    | RandomChoice


type alias SourceConfig =
    { arrivalRate : Float     -- mean jobs per time unit
    , jobPriority : Priority
    , jobLabel    : String
    }


type alias WorkerConfig =
    { serviceTime : ServiceTime
    , preemptive  : Bool
    , signoff     : Maybe LockID
    }


type alias DispatcherConfig =
    { rule         : DispatchRule
    , dispatchTime : Float
    , roundRobinIndex : Int   -- mutable; updated each dispatch
    }


type NodeKind
    = Source SourceConfig
    | Worker WorkerConfig
    | Dispatcher DispatcherConfig
    | Sink


type NodeState
    = Idle
    | Busy     JobID EventTime    -- job being processed, scheduled completion
    | Signoff  JobID LockID       -- waiting for approver
    | Blocked  JobID              -- service done but all output queues full
    | Preempted JobID EventTime   -- interrupted; remaining service time saved
    | Paused   (Maybe NodeState)  -- halted by boss node


type alias NodeData =
    { kind  : NodeKind
    , state : NodeState
    , label : String
    }


makeSource : String -> SourceConfig -> NodeData
makeSource lbl cfg =
    { kind = Source cfg, state = Idle, label = lbl }


makeWorker : String -> WorkerConfig -> NodeData
makeWorker lbl cfg =
    { kind = Worker cfg, state = Idle, label = lbl }


makeSink : String -> NodeData
makeSink lbl =
    { kind = Sink, state = Idle, label = lbl }


isIdle : NodeData -> Bool
isIdle node =
    node.state == Idle
