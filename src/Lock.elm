module Lock exposing
    ( LockConfig
    , LockState
    , newLock
    , acquire
    , release
    , AcquireResult(..)
    )

import Id exposing (JobID, LockID, NodeID)
import ServiceTime exposing (ServiceTime)


type alias LockConfig =
    { id          : LockID
    , label       : String
    , capacity    : Int
    , serviceTime : ServiceTime
    }


type alias LockState =
    { config  : LockConfig
    , inUse   : Int
    , waiters : List ( NodeID, JobID )    -- FIFO queue of waiting workers
    }


type AcquireResult
    = Acquired LockState          -- slot was free; caller may start sign-off
    | Queued   LockState          -- all slots busy; caller added to waiters


newLock : LockConfig -> LockState
newLock cfg =
    { config = cfg, inUse = 0, waiters = [] }


acquire : NodeID -> JobID -> LockState -> AcquireResult
acquire nid jid lock =
    if lock.inUse < lock.config.capacity then
        Acquired { lock | inUse = lock.inUse + 1 }

    else
        Queued { lock | waiters = lock.waiters ++ [ ( nid, jid ) ] }


-- Returns the updated lock and optionally the next waiter to notify
release : LockState -> ( LockState, Maybe ( NodeID, JobID ) )
release lock =
    case lock.waiters of
        [] ->
            ( { lock | inUse = max 0 (lock.inUse - 1) }, Nothing )

        next :: rest ->
            -- slot stays occupied; hand it straight to the next waiter
            ( { lock | waiters = rest }, Just next )
