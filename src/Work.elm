module Work exposing (Work(..), WorkID(..), fetchWorkID)

import Dict exposing (Dict)
import EventTime exposing (EventTime)


type WorkID
    = WorkID Int


fetchWorkID : WorkID -> Int
fetchWorkID (WorkID tid) =
    tid


type Work
    = Work WorkID (Dict Int EventTime)
