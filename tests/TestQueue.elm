module TestQueue exposing (suite)

import EventTime exposing (EventTime(..))
import Expect
import Fuzz exposing (intRange)
import Id exposing (JobID(..))
import Job exposing (Job, Priority(..), newJob)
import Queue exposing (..)
import Test exposing (Test, describe, fuzz, test)


-- ── Helpers ───────────────────────────────────────────────────────────────────

fifo : Int -> Queue
fifo cap =
    empty { capacity = cap, discipline = FIFO, overflow = Block }


pfifo : Int -> Queue
pfifo cap =
    empty { capacity = cap, discipline = PriorityFIFO, overflow = Block }


job : Int -> Priority -> Job
job n pri =
    newJob (JobID n) pri "test" (EventTime 0)


fill : List Job -> Queue -> Queue
fill jobs q =
    List.foldl
        (\j acc ->
            case enqueue j acc of
                Enqueued q1 ->
                    q1

                DroppedExisting _ q1 ->
                    q1

                _ ->
                    acc
        )
        q
        jobs


dequeueAll : Queue -> List Job
dequeueAll q =
    case dequeue q of
        Nothing ->
            []

        Just ( j, q1 ) ->
            j :: dequeueAll q1


-- ── Tests ─────────────────────────────────────────────────────────────────────

suite : Test
suite =
    describe "Queue"
        [ describe "FIFO / Block"
            [ test "starts empty" <|
                \_ -> Expect.equal 0 (size (fifo 3))

            , test "enqueue increases size" <|
                \_ ->
                    case enqueue (job 1 Normal) (fifo 3) of
                        Enqueued q ->
                            Expect.equal 1 (size q)

                        _ ->
                            Expect.fail "expected Enqueued"

            , test "fills to capacity" <|
                \_ ->
                    let
                        q =
                            fill [ job 1 Normal, job 2 Normal, job 3 Normal ] (fifo 3)
                    in
                    Expect.equal 3 (size q)

            , test "blocks when full" <|
                \_ ->
                    let
                        q =
                            fill [ job 1 Normal, job 2 Normal ] (fifo 2)
                    in
                    Expect.equal WasBlocked (enqueue (job 3 Normal) q)

            , test "FIFO dequeue order" <|
                \_ ->
                    let
                        q =
                            fill [ job 1 Normal, job 2 Normal, job 3 Normal ] (fifo 3)

                        ids =
                            List.map .id (dequeueAll q)
                    in
                    Expect.equal [ JobID 1, JobID 2, JobID 3 ] ids

            , test "dequeue empty returns Nothing" <|
                \_ ->
                    Expect.equal Nothing (dequeue (fifo 3))

            , test "capacity 1 blocks after first job" <|
                \_ ->
                    let
                        q =
                            fill [ job 1 Normal ] (fifo 1)
                    in
                    Expect.equal WasBlocked (enqueue (job 2 Normal) q)
            ]

        , describe "PriorityFIFO"
            [ test "critical served before normal" <|
                \_ ->
                    let
                        q =
                            fill [ job 1 Normal, job 2 Critical ] (pfifo 4)
                    in
                    case dequeue q of
                        Just ( j, _ ) ->
                            Expect.equal (JobID 2) j.id

                        Nothing ->
                            Expect.fail "expected a job"

            , test "ties preserve arrival order" <|
                \_ ->
                    let
                        q =
                            fill [ job 1 High, job 2 High ] (pfifo 4)
                    in
                    case dequeue q of
                        Just ( j, _ ) ->
                            Expect.equal (JobID 1) j.id

                        Nothing ->
                            Expect.fail "expected a job"

            , test "full priority order Critical > High > Normal > Low" <|
                \_ ->
                    let
                        q =
                            fill
                                [ job 1 Low, job 2 Normal, job 3 High, job 4 Critical ]
                                (pfifo 4)

                        ids =
                            List.map .id (dequeueAll q)
                    in
                    Expect.equal [ JobID 4, JobID 3, JobID 2, JobID 1 ] ids
            ]

        , describe "DropFirst"
            [ test "oldest dropped when full" <|
                \_ ->
                    let
                        q =
                            fill [ job 1 Normal, job 2 Normal ]
                                (empty { capacity = 2, discipline = FIFO, overflow = DropFirst })
                    in
                    case enqueue (job 3 Normal) q of
                        DroppedExisting dropped _ ->
                            Expect.equal (JobID 1) dropped

                        _ ->
                            Expect.fail "expected DroppedExisting"

            , test "size stays at capacity after drop" <|
                \_ ->
                    let
                        q =
                            fill [ job 1 Normal, job 2 Normal ]
                                (empty { capacity = 2, discipline = FIFO, overflow = DropFirst })
                    in
                    case enqueue (job 3 Normal) q of
                        DroppedExisting _ q1 ->
                            Expect.equal 2 (size q1)

                        _ ->
                            Expect.fail "expected DroppedExisting"
            ]

        , describe "DropLast"
            [ test "newest dropped when full" <|
                \_ ->
                    let
                        q =
                            fill [ job 1 Normal, job 2 Normal ]
                                (empty { capacity = 2, discipline = FIFO, overflow = DropLast })
                    in
                    case enqueue (job 3 Normal) q of
                        DroppedExisting dropped _ ->
                            Expect.equal (JobID 2) dropped

                        _ ->
                            Expect.fail "expected DroppedExisting"
            ]

        , describe "DropLowestPriority"
            [ test "incoming high bumps existing low" <|
                \_ ->
                    let
                        q =
                            fill [ job 1 Low, job 2 Normal ]
                                (empty { capacity = 2, discipline = PriorityFIFO, overflow = DropLowestPriority })
                    in
                    case enqueue (job 3 High) q of
                        DroppedExisting dropped _ ->
                            Expect.equal (JobID 1) dropped

                        _ ->
                            Expect.fail "expected low job to be dropped"

            , test "incoming low dropped when all existing are higher priority" <|
                \_ ->
                    let
                        q =
                            fill [ job 1 High, job 2 Critical ]
                                (empty { capacity = 2, discipline = PriorityFIFO, overflow = DropLowestPriority })
                    in
                    Expect.equal DroppedIncoming (enqueue (job 3 Low) q)
            ]

        , describe "Invariants"
            [ fuzz (intRange 1 10) "size never exceeds capacity (DropFirst)" <|
                \cap ->
                    let
                        cfg =
                            { capacity = cap, discipline = FIFO, overflow = DropFirst }

                        q =
                            List.range 1 (cap + 5)
                                |> List.map (\n -> job n Normal)
                                |> List.foldl
                                    (\j acc ->
                                        case enqueue j acc of
                                            Enqueued q1 ->
                                                q1

                                            DroppedExisting _ q1 ->
                                                q1

                                            _ ->
                                                acc
                                    )
                                    (empty cfg)
                    in
                    Expect.atMost cap (size q)
            ]
        ]
