module Main exposing (..)

-- import ResourceView exposing (drawArrowWithQueue, drawResource)

import Browser
import Dict exposing (Dict)
import EventTime exposing (EventTime(..))
import Html exposing (Html, button, div, li, ul)
import Html.Events exposing (onClick)
import Queue exposing (..)
import Resource exposing (..)
import Svg exposing (svg)
import Svg.Attributes exposing (..)
import Types exposing (..)
import Work exposing (Work(..), WorkID(..))
import Interactions exposing (..)
import Event exposing (..)


init =
    { resources =
        Dict.fromList
            [ ( 1, Resource (Just (QueueID 3)) [ QueueID 1 ] Idle Nothing )
            , ( 2, Resource (Just (QueueID 1)) [ QueueID 2 ] Idle Nothing )
            , ( 3, Resource (Just (QueueID 2)) [ QueueID 3 ] Idle Nothing )
            ]
    , queues =
        Dict.fromList
            [ ( 1
              , { tasks =
                    [ Work (WorkID 1) (Dict.fromList [ ( 1, EventTime 5 ) ])
                    , Work (WorkID 2) (Dict.fromList [ ( 2, EventTime 5 ) ])
                    , Work (WorkID 3) (Dict.fromList [ ( 3, EventTime 5 ) ])
                    ]
                }
              )
            , ( 2, { tasks = [] } )
            , ( 3, { tasks = [] } )
            ]
    , events =
        []
    , currentTime = EventTime 0
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
    -- handle all resources: service idle resources. Create new events for all idle resources to check the queues
    let
        idleResources =
            Dict.filter (\_ resource -> (Resource.input resource) /= Nothing && (Resource.state resource == Idle)) model.resources

        updatedEvents =
            model.events ++ (idleResources |> Dict.keys |> List.map (\id -> Event model.currentTime (ResourceID id) FetchTask))

        -- sort events by time
        sortedEvents =
            List.sortWith compareEventTimes updatedEvents

        ( currentEvents, laterEvents ) =
            List.partition (\e -> (e |> eventTime |> fetchTime) <= (model.currentTime |> fetchTime)) sortedEvents

        stepPlusOneModel =
            executeOnCurrentEvents model currentEvents laterEvents
    in
    { stepPlusOneModel
        | currentTime = Time ((stepPlusOneModel.currentTime |> fetchTime) + 1)
        , events = stepPlusOneModel.events
    }




executeOnCurrentEvents : Model -> List Event -> List Event -> Model
executeOnCurrentEvents model currentEvents laterEvents =
    let
        serviceCompleteEvents =
            List.filter isServiceCompleteEvent currentEvents

        fetchTaskEvents =
            List.filter isFetchTaskEvent currentEvents

        ( changedResources, changedQueues, newEvents ) =
            ( model.resources, model.queues )
                |> pushTaskToQueue serviceCompleteEvents
                |> fetchTaskFromQueue model.currentTime fetchTaskEvents

        nextModelEvents =
            laterEvents ++ newEvents
    in
    { model | resources = changedResources, queues = changedQueues, events = nextModelEvents }


view : Model -> Html Msg
view model =
    div []
        [ Html.text "Minimal Resource Simulation"
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
        nid =
            e |> eventResourceID |> fetchResourceID |> String.fromInt

        time =
            e |> eventTime |> fetchTime |> String.fromInt

        etype =
            case e |> eventType of
                ServiceComplete ->
                    "ServiceComplete at resource " ++ nid

                FetchTask ->
                    "FetchTask at resource " ++ nid
    in
    time ++ ": " ++ etype


svgView : Model -> Html msg
svgView model =
    let
        queueArrows =
            List.concatMap
                (\q ->
                    let
                        fromResources =
                            List.filter (\n -> List.member q.id n.outputQueues) model.resources

                        toResource =
                            List.filter (\n -> n.inputQueue == Just q.id) model.resources |> List.head
                    in
                    case ( fromResources, toResource ) of
                        ( fromN :: _, Just toN ) ->
                            [ drawArrowWithQueue fromN toN q ]

                        _ ->
                            []
                )
                model.queues
    in
    svg [ width "600", height "500", Svg.Attributes.style "border:1px solid #ccc; background:#fafafa;" ]
        (queueArrows ++ List.map drawResource model.resources)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
