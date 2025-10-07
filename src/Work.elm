module Work exposing (Work(..), WorkID(..), workIDValue, workID)

import Dict exposing (Dict)
import EventTime exposing (EventTime)



type WorkID
    = WorkID Int


workIDValue : WorkID -> Int
workIDValue (WorkID tid) =
    tid

workID : Work -> WorkID
workID (Work wid _) = 
    wid


type Work
    = Work WorkID (Dict Int EventTime)



