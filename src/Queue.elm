module Queue exposing (Queue(..), put, take, tasks, Behaviour(..), PutResult(..), QueueConfig, config)

import Work exposing (..)



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



put : Work -> Queue -> (Queue, PutResult)
put work queue = 
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
                (Queue cfg ([work] |> List.append (tasks queue |> List.drop 1)), FirstDropped)
            DropLast ->
                (Queue cfg ([work] |> List.append (tasks queue |> List.take (max - 1))), LastDropped)



take : Queue -> (Maybe Work, Queue)
take queue = 
    (List.head (tasks queue), Queue (config queue) (Maybe.withDefault [] (List.tail (tasks queue))))


