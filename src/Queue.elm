module Queue exposing (Behaviour(..), PutResult(..), Queue(..), QueueConfig, config, put, putQueue, putResult, take, tasks)

import Work exposing (Work(..), WorkID(..))
import Dict


type Behaviour
    = DropFirst
    | DropLast
    | Block





type PutResult
    = Ok Queue Work
    | Err Behaviour Queue Work
    | NoWork Queue



putQueue : PutResult -> Queue
putQueue result =
    case result of
        Ok queue _ ->
            queue

        NoWork queue ->
            queue

        Err _ queue _ ->
            queue


putResult : PutResult -> String
putResult result =
    case result of
        Ok _ _->
            "Ok"

        NoWork _ ->
            "NoWork"

        Err DropFirst _ _ ->
            "FirstDropped"

        Err DropLast _ _ ->
            "LastDropped"

        Err Block _ _ ->
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
        Ok (Queue cfg (List.append (tasks queue) [ work ])) work
        -- append new work at the end of the list of work (tasks)

    else
        case cfg.behaviour of
            Block ->
                Err Block (Queue cfg (tasks queue)) work

            DropFirst ->
                let
                    (droppedWork, remainingQueue) = case (tasks queue) of
                        x :: xs ->
                            (x, xs)
                        [] ->
                            (Work (WorkID -1) (Dict.fromList []), tasks queue) -- empty work
                in
                Err DropFirst (Queue cfg ([ work ] |> List.append remainingQueue)) droppedWork

            DropLast ->
                let
                    (droppedWork, remainingQueue) = case (tasks queue |> List.reverse) of
                        x :: xs ->
                            (x, xs |> List.reverse)
                        [] ->
                            (Work (WorkID -1) (Dict.fromList []), tasks queue) -- empty work

                in
                Err DropLast (Queue cfg ([ work ] |> List.append remainingQueue)) droppedWork


take : Queue -> ( Maybe Work, Queue )
take queue =
    ( List.head (tasks queue), Queue (config queue) (Maybe.withDefault [] (List.tail (tasks queue))) )
