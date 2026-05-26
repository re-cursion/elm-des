module Id exposing
    ( JobID(..)
    , NodeID(..)
    , QueueID(..)
    , LockID(..)
    , jobIDInt
    , nodeIDInt
    , queueIDInt
    )


type JobID
    = JobID Int


type NodeID
    = NodeID Int


type QueueID
    = QueueID Int


type LockID
    = LockID String


jobIDInt : JobID -> Int
jobIDInt (JobID n) =
    n


nodeIDInt : NodeID -> Int
nodeIDInt (NodeID n) =
    n


queueIDInt : QueueID -> Int
queueIDInt (QueueID n) =
    n
