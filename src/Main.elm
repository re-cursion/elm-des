module Main exposing (..)

import Browser
import Html exposing (Html, button, div, li, ul)
import Html.Events exposing (onClick)
import NodeView exposing (drawArrowWithQueue, drawNode)
import Svg exposing (svg)
import Svg.Attributes exposing (..)
import Types exposing (Time(..), Event(..), EventType(..), Model, Node, NodeID(..), Queue, QueueID(..), Work, WorkID(..), fetchNodeID, fetchQueueID, fetchWorkID, eventTime, fetchTime, compareEventTimes, compareTimes, eventNodeID, addTimes, eventType)


init =
    { nodes =
        [ { id = NodeID 1, inputQueue = Just (QueueID 3), outputQueues = [ (QueueID 1) ], view = { x = 100, y = 100 }, busy = False, currentTask = Nothing } -- initial node
        , { id = NodeID 2, inputQueue = Just (QueueID 1), outputQueues = [ (QueueID 2) ], view = { x = 300, y = 110 }, busy = False, currentTask = Nothing }
        , { id = NodeID 3, inputQueue = Just (QueueID 2), outputQueues = [ (QueueID 3) ], view = { x = 310, y = 200 }, busy = False, currentTask = Nothing }
        ]
    , queues =
        [ { id = QueueID 1, tasks = [ Work (WorkID 1) (Time 5), Work (WorkID 2) (Time 7), Work (WorkID 3) (Time 2) ] }
        , { id = QueueID 2, tasks = [] }
        , { id = QueueID 3, tasks = [] }
        ]
    , events =
        []
    , currentTime = Time 0
    }


type Msg
    = StepSimulation


update : Msg -> Model -> Model
update msg model =
    case msg of
        StepSimulation ->
            processNextStep model


processNextStep : Model -> Model
processNextStep model =
    -- handle all nodes: service idle nodes. Create new events for all idle nodes to check the queues
    let
        idleNodes =
            List.filter (\n -> n.inputQueue /= Nothing && not n.busy) model.nodes

        updatedEvents =
            model.events ++ List.map (\n -> (Event model.currentTime n.id FetchTask) ) idleNodes

        -- sort events by time
        sortedEvents =
            List.sortWith compareEventTimes updatedEvents

        ( currentEvents, laterEvents ) =
            List.partition (\e -> (e |> eventTime |> fetchTime) <= (model.currentTime |> fetchTime)) sortedEvents

        stepPlusOneModel =
            executeOnCurrentEvents model currentEvents laterEvents
    in
    { stepPlusOneModel
        | currentTime = (Time ((stepPlusOneModel.currentTime |> fetchTime) + 1))
        , events = stepPlusOneModel.events
    }


partitionNodeMatchingEvent : Event -> List Node -> ( Maybe Node, List Node )
partitionNodeMatchingEvent event nodes =
    let
        ( matching, remaining ) =
            List.partition (\n -> n.id == (eventNodeID event)) nodes
    in
    case matching of
        n :: _ ->
            ( Just n, remaining )

        [] ->
            ( Nothing, nodes )


partitionInputQueueMatchingNode : Maybe Node -> List Queue -> ( Maybe Queue, List Queue )
partitionInputQueueMatchingNode maybeNode queues =
    case maybeNode of
        Just n ->
            let
                ( matchingQueues, remainingQueues ) =
                    List.partition (\q -> Maybe.withDefault (QueueID -1) n.inputQueue == q.id) queues
            in
            case matchingQueues of
                [] ->
                    ( Nothing, queues )

                q :: qtail ->
                    -- take the first input queue
                    ( Just q, qtail ++ remainingQueues )

        Nothing ->
            ( Nothing, queues )


compareTaskLength : Queue -> Queue -> Order
compareTaskLength q1 q2 =
    compare (List.length q1.tasks) (List.length q2.tasks)


partitionOutputQueueMatchingNode : Maybe Node -> List Queue -> ( Maybe Queue, List Queue )
partitionOutputQueueMatchingNode maybeNode queues =
    case maybeNode of
        Just n ->
            let
                ( matchingQueues, _ ) =
                    List.partition (\q -> List.member q.id n.outputQueues) queues
            in
            case matchingQueues of
                [] ->
                    ( Nothing, queues )

                q :: _ ->
                    -- take the first output queue
                    let
                        leastBusiestQueue =
                            matchingQueues |> List.sortWith compareTaskLength |> List.head
                    in
                    case leastBusiestQueue of
                        Just lbq ->
                            ( Just lbq, (queues |> List.filter (\x -> x.id /= lbq.id)) )

                        Nothing ->
                            ( Nothing, queues )

        Nothing ->
            ( Nothing, queues )


fetchTaskFromNode : Maybe Node -> Maybe Work
fetchTaskFromNode maybeNode =
    maybeNode |> Maybe.andThen .currentTask



-- case maybeNode of
--     Just n ->
--         n.currentTask
--     Nothing ->
--         Nothing


fetchTaskFromQueue : Time -> List Event -> ( List Node, List Queue ) -> ( List Node, List Queue, List Event )
fetchTaskFromQueue currentTime fetchTaskEvents ( nodes, queues ) =
    case fetchTaskEvents of
        [] ->
            ( nodes, queues, [] )

        -- no more fetch events to process
        event :: remainingEvents ->
            let
                ( node, remainingNodes ) =
                    partitionNodeMatchingEvent event nodes

                -- select node based on event
                ( queue, remainingQueues ) =
                    partitionInputQueueMatchingNode node queues

                ( updatedNode, updatedQueue, eventToBeScheduled ) =
                    case ( node, queue ) of
                        ( Just n, Just q ) ->
                            case q.tasks of
                                -- no task to fetch
                                [] ->
                                    ( Nothing, Nothing, [] )

                                hd :: tl ->
                                    ( Just { n | busy = True, currentTask = Just hd }
                                    , Just { q | tasks = tl }
                                    , [ (Event (addTimes currentTime hd.serviceTime) n.id ServiceComplete)]
                                      -- schedule service complete event
                                    )

                        _ ->
                            ( Nothing, Nothing, [] )

                -- no updates if node or queue not found
                ( updatedNodes, updatedQueues ) =
                    case ( updatedNode, updatedQueue ) of
                        ( Just un, Just uq ) ->
                            ( List.map
                                (\n ->
                                    if n.id == un.id then
                                        un

                                    else
                                        n
                                )
                                nodes
                            , List.map
                                (\q ->
                                    if q.id == uq.id then
                                        uq

                                    else
                                        q
                                )
                                queues
                            )

                        _ ->
                            ( nodes, queues )

                -- if there are more fetch events, process them (recursively)
                ( upNodes, upQueues, upEvents ) =
                    fetchTaskFromQueue currentTime remainingEvents ( updatedNodes, updatedQueues )

            in
            ( upNodes, upQueues, eventToBeScheduled ++ upEvents )



-- accumulate scheduled events


pushTaskToQueue : List Event -> ( List Node, List Queue ) -> ( List Node, List Queue )
pushTaskToQueue serviceCompleteEvents ( nodes, queues ) =
    case serviceCompleteEvents of
        [] ->
            ( nodes, queues )

        -- no service complete events to process
        event :: remainingEvents ->
            let
                ( node, remainingNodes ) =
                    partitionNodeMatchingEvent event nodes

                -- select node based on event
                ( queue, remainingQueues ) =
                    partitionOutputQueueMatchingNode node queues

                task =
                    fetchTaskFromNode node

                ( updatedNodes, updatedQueues ) =
                    case ( node, queue, task ) of
                        ( Just n, Just q, Just t ) ->
                            ( { n | busy = False, currentTask = Nothing } :: remainingNodes
                            , { q | tasks = q.tasks ++ [ t ] } :: remainingQueues
                            )

                        _ ->
                            ( nodes, queues )
            in
            pushTaskToQueue remainingEvents ( updatedNodes, updatedQueues )



-- if there are more service complete events, process them (recursively)


isServiceCompleteEvent : Event -> Bool
isServiceCompleteEvent e =
    case (e |> eventType) of
        ServiceComplete ->
            True

        _ ->
            False


isFetchTaskEvent : Event -> Bool
isFetchTaskEvent e =
    case (e |> eventType) of
        FetchTask ->
            True

        _ ->
            False


executeOnCurrentEvents : Model -> List Event -> List Event -> Model
executeOnCurrentEvents model currentEvents laterEvents =
    let
        serviceCompleteEvents =
            List.filter isServiceCompleteEvent currentEvents

        fetchTaskEvents =
            List.filter isFetchTaskEvent currentEvents

        ( changedNodes, changedQueues, newEvents ) =
            ( model.nodes, model.queues )
                |> pushTaskToQueue serviceCompleteEvents
                |> fetchTaskFromQueue model.currentTime fetchTaskEvents

        nextModelEvents =
            laterEvents ++ newEvents
    in
    { model | nodes = changedNodes, queues = changedQueues, events = nextModelEvents }


view : Model -> Html Msg
view model =
    div []
        [ Html.text "Minimal Node Simulation"
        , svgView model
        , button [ onClick StepSimulation ] [ Html.text "Step Simulation" ]
        , Html.text ("Current time: " ++ String.fromInt (model.currentTime |> fetchTime))
        , Html.text "\nQueues:"
        , ul [] (List.map (\q -> li [] [ Html.text ("Queue " ++ String.fromInt (fetchQueueID q.id) ++ ": " ++ String.join ", " (List.map (\t -> String.fromInt (fetchWorkID t.id)) q.tasks)) ]) model.queues)
        , Html.text "\nEvents:"
        , ul [] (List.map (\e -> li [] [ Html.text (eventToString e) ]) (List.sortWith compareEventTimes model.events))
        ]


eventToString : Event -> String
eventToString e =
    let
        nid = e |> eventNodeID |> fetchNodeID |> String.fromInt
        time = e |> eventTime |> fetchTime |> String.fromInt
        etype =
            case e |> eventType of
                ServiceComplete ->
                    "ServiceComplete at node " ++ nid

                FetchTask ->
                    "FetchTask at node " ++ nid
    in
    time ++ ": " ++ etype


svgView : Model -> Html msg
svgView model =
    let
        queueArrows =
            List.concatMap
                (\q ->
                    let
                        fromNodes =
                            List.filter (\n -> List.member q.id n.outputQueues) model.nodes

                        toNode =
                            List.filter (\n -> n.inputQueue == Just q.id) model.nodes |> List.head
                    in
                    case ( fromNodes, toNode ) of
                        ( fromN :: _, Just toN ) ->
                            [ drawArrowWithQueue fromN toN q ]

                        _ ->
                            []
                )
                model.queues
    in
    svg [ width "600", height "500", Svg.Attributes.style "border:1px solid #ccc; background:#fafafa;" ]
        (queueArrows ++ List.map drawNode model.nodes)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
