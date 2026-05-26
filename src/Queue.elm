module Queue exposing
    ( Discipline(..)
    , Overflow(..)
    , Queue
    , EnqueueResult(..)
    , empty
    , enqueue
    , dequeue
    , peek
    , size
    , isEmpty
    , toList
    )

import Id exposing (JobID)
import Job exposing (Job, priorityRank)


type Discipline
    = FIFO
    | PriorityFIFO
    | LIFO


type Overflow
    = Block
    | DropFirst
    | DropLast
    | DropLowestPriority


type alias QueueConfig =
    { capacity   : Int
    , discipline : Discipline
    , overflow   : Overflow
    }


type Queue
    = Queue QueueConfig (List Job)


type EnqueueResult
    = Enqueued Queue
    | WasBlocked
    | DroppedExisting JobID Queue
    | DroppedIncoming


empty : QueueConfig -> Queue
empty cfg =
    Queue cfg []


size : Queue -> Int
size (Queue _ jobs) =
    List.length jobs


isEmpty : Queue -> Bool
isEmpty q =
    size q == 0


peek : Queue -> Maybe Job
peek (Queue _ jobs) =
    List.head jobs


toList : Queue -> List Job
toList (Queue _ jobs) =
    jobs


enqueue : Job -> Queue -> EnqueueResult
enqueue job (Queue cfg jobs) =
    if List.length jobs < cfg.capacity then
        Enqueued (Queue cfg (insert cfg.discipline job jobs))

    else
        case cfg.overflow of
            Block ->
                WasBlocked

            DropFirst ->
                case jobs of
                    [] ->
                        Enqueued (Queue cfg [ job ])

                    dropped :: rest ->
                        DroppedExisting dropped.id
                            (Queue cfg (insert cfg.discipline job rest))

            DropLast ->
                let
                    reversed =
                        List.reverse jobs
                in
                case reversed of
                    [] ->
                        Enqueued (Queue cfg [ job ])

                    dropped :: rest ->
                        DroppedExisting dropped.id
                            (Queue cfg (insert cfg.discipline job (List.reverse rest)))

            DropLowestPriority ->
                dropLowestPriority job cfg jobs


dropLowestPriority : Job -> QueueConfig -> List Job -> EnqueueResult
dropLowestPriority incoming cfg jobs =
    let
        lowestInQueue =
            List.foldl
                (\j acc ->
                    case acc of
                        Nothing ->
                            Just j

                        Just current ->
                            if priorityRank j.priority < priorityRank current.priority then
                                Just j
                            else
                                acc
                )
                Nothing
                jobs
    in
    case lowestInQueue of
        Nothing ->
            Enqueued (Queue cfg [ incoming ])

        Just lowest ->
            if priorityRank incoming.priority > priorityRank lowest.priority then
                let
                    withoutLowest =
                        removeFirst (\j -> j.id == lowest.id) jobs
                in
                DroppedExisting lowest.id
                    (Queue cfg (insert cfg.discipline incoming withoutLowest))

            else
                DroppedIncoming


dequeue : Queue -> Maybe ( Job, Queue )
dequeue (Queue cfg jobs) =
    case jobs of
        [] ->
            Nothing

        first :: rest ->
            Just ( first, Queue cfg rest )


insert : Discipline -> Job -> List Job -> List Job
insert discipline job jobs =
    case discipline of
        FIFO ->
            jobs ++ [ job ]

        PriorityFIFO ->
            insertByPriority job jobs

        LIFO ->
            job :: jobs


insertByPriority : Job -> List Job -> List Job
insertByPriority job jobs =
    case jobs of
        [] ->
            [ job ]

        first :: rest ->
            -- higher priority goes earlier; ties preserve arrival order (FIFO among equals)
            if priorityRank job.priority > priorityRank first.priority then
                job :: first :: rest

            else
                first :: insertByPriority job rest


removeFirst : (a -> Bool) -> List a -> List a
removeFirst pred xs =
    case xs of
        [] ->
            []

        x :: rest ->
            if pred x then
                rest

            else
                x :: removeFirst pred rest
