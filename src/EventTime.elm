module EventTime exposing (EventTime(..), fetchTime, addTimes, compareTimes)



type EventTime 
    = EventTime Int


fetchTime : EventTime -> Int
fetchTime (EventTime time)
    = time

addTimes : EventTime -> EventTime -> EventTime
addTimes t0 t1 = 
    (EventTime ((t0 |> fetchTime) + (t1 |> fetchTime)))



compareTimes : EventTime -> EventTime -> Order
compareTimes tim0 tim1 = 
    (compare (tim0 |> fetchTime) (tim1 |> fetchTime))