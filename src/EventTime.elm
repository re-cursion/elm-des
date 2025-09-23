module EventTime exposing (EventTime(..), eventTime2Int, addTimes, compareTimes)



type EventTime 
    = EventTime Int


eventTime2Int : EventTime -> Int
eventTime2Int (EventTime time)
    = time

addTimes : EventTime -> EventTime -> EventTime
addTimes t0 t1 = 
    (EventTime ((t0 |> eventTime2Int) + (t1 |> eventTime2Int)))



compareTimes : EventTime -> EventTime -> Order
compareTimes tim0 tim1 = 
    (compare (tim0 |> eventTime2Int) (tim1 |> eventTime2Int))