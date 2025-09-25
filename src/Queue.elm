module Queue exposing (Behaviour(..), PutResult(..), Queue(..), QueueConfig, config, put, putQueue, putResult, take, tasks)

import Work exposing (Work)


type Behaviour
    = DropFirst
    | DropLast
    | Block





type PutResult
    = Ok Queue
    | Err Behaviour Queue 


putQueue : PutResult -> Queue
putQueue result =
    case result of
        Ok queue ->
            queue

        Err DropFirst queue ->
            queue

        Err DropLast queue ->
            queue

        Err Block queue ->
            queue


putResult : PutResult -> String
putResult result =
    case result of
        Ok _ ->
            "Ok"

        Err DropFirst _ ->
            "FirstDropped"

        Err DropLast _ ->
            "LastDropped"

        Err Block _ ->
            "Blocked"


type alias QueueConfig =
    { size : Int, behaviour : Behaviour }


type Queue
    = Queue QueueConfig (List Work)


tasks : Queue -> List Work
tasks (Queue _ lw) =
    lw


    


config : Queue -> QueueConfig
config (Queue cfg _) =
    cfg


put : Work -> Queue -> PutResult
put work queue =
    let
        cfg =
            config queue

        max =
            cfg.size

        tsks =
            tasks queue

        count =
            List.length tsks
    in
    if count < max then
        Ok (Queue cfg (List.append (tasks queue) [ work ]))
        -- append new work at the end of the list of work (tasks)

    else
        case cfg.behaviour of
            Block ->
                Err Block (Queue cfg (tasks queue))

            DropFirst ->
                Err DropFirst (Queue cfg ([ work ] |> List.append (tasks queue |> List.drop 1)))

            DropLast ->
                Err DropLast (Queue cfg ([ work ] |> List.append (tasks queue |> List.take (max - 1))))


take : Queue -> ( Maybe Work, Queue )
take queue =
    ( List.head (tasks queue), Queue (config queue) (Maybe.withDefault [] (List.tail (tasks queue))) )
