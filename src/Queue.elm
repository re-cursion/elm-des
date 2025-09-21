module Queue exposing (Behaviour(..), PutResult(..), Queue(..), QueueConfig, config, put, putQueue, putResult, take, tasks)

import Work exposing (Work)


type Behaviour
    = DropFirst
    | DropLast
    | Block



-- type PutResultType
--     = Ok
--     | FirstDropped
--     | LastDropped
--     | Blocked
-- type PutResult
--     = PutResultType Queue
-- putQueue : PutResult -> Queue
-- putQueue (PutResultType queue) =
--     queue
-- putResult : PutResult -> PutResultType
-- putResult result =
--     case result of
--         Ok _ ->
--             Ok
--         FirstDropped _ ->
--             FirstDropped
--         LastDropped _ ->
--             LastDropped
--         Blocked _ ->
--             Blocked


type PutResult
    = Ok Queue
    | FirstDropped Queue
    | LastDropped Queue
    | Blocked Queue


putQueue : PutResult -> Queue
putQueue result =
    case result of
        Ok queue ->
            queue

        FirstDropped queue ->
            queue

        LastDropped queue ->
            queue

        Blocked queue ->
            queue


putResult : PutResult -> String
putResult result =
    case result of
        Ok _ ->
            "Ok"

        FirstDropped _ ->
            "FirstDropped"

        LastDropped _ ->
            "LastDropped"

        Blocked _ ->
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
                Blocked (Queue cfg (tasks queue))

            DropFirst ->
                FirstDropped (Queue cfg ([ work ] |> List.append (tasks queue |> List.drop 1)))

            DropLast ->
                LastDropped (Queue cfg ([ work ] |> List.append (tasks queue |> List.take (max - 1))))


take : Queue -> ( Maybe Work, Queue )
take queue =
    ( List.head (tasks queue), Queue (config queue) (Maybe.withDefault [] (List.tail (tasks queue))) )
