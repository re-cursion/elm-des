
module Work exposing (WorkID(..), Work, fetchWorkID)

import EventTime exposing (EventTime)

type WorkID 
    = WorkID Int


fetchWorkID : WorkID -> Int
fetchWorkID (WorkID tid) =
    tid


type alias Work =
    { id : WorkID
    , serviceTime : EventTime
    }



