module Queue exposing (Queue(..), put, take, Behaviour, PutResult, QueueConfig)

import Types exposing (Work)


type Behaviour
    = DropFirst
    | DropLast
    | Block

type PutResult
    = Ok
    | FirstDropped
    | LastDropped
    | Blocked

type alias QueueConfig
    = { size : Int, behaviour : Behaviour }


type Queue =
    Queue QueueConfig (List Work)



tasks : Queue -> List Work
tasks (Queue _ lw) = lw


config : Queue -> QueueConfig
config (Queue cfg _) = cfg



put : Queue -> Work -> (Queue, PutResult)
put queue work = 
    let
        cfg = config queue
        max = cfg.size
        tsks = tasks queue
        count = List.length tsks
    in
    if count < max then
        (Queue cfg (List.append (tasks queue) [work]), Ok) -- append new work at the end of the list of work (tasks)
    else
        case cfg.behaviour of
            Block -> 
                (Queue cfg (tasks queue), Blocked)
            DropFirst ->
                (Queue cfg (tasks queue |> List.drop 1 |> List.append [work]), FirstDropped)
            DropLast ->
                (Queue cfg (tasks queue |> List.take (max - 1) |> List.append [work]), LastDropped)



take : Queue -> (Maybe Work, Queue)
take queue = 
    (List.head (tasks queue), Queue (config queue) (Maybe.withDefault [] (List.tail (tasks queue))))


