module Main exposing (main)

import Browser
import Dict
import Engine
import Event exposing (EventType(..))
import EventTime exposing (EventTime(..), eventTime2Int)
import Html exposing (Html, button, div, h2, li, p, span, text, ul)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Id exposing (NodeID(..), QueueID(..))
import Job exposing (Priority(..))
import Node exposing (NodeKind(..), NodeState(..), makeSource, makeSink, makeWorker)
import Queue
import Random
import SimState exposing (SimState)
import Topology exposing (Topology)


-- ── Scenario wiring ───────────────────────────────────────────────────────────
-- Simple 3-node chain: Source → [Queue 1] → Worker → [Queue 2] → Sink

scenario : ( Topology, SimState )
scenario =
    let
        topo =
            Topology.empty
                |> Topology.addOutputEdge { from = NodeID 1, to = QueueID 1 }
                |> Topology.addInputEdge  { from = QueueID 1, to = NodeID 2 }
                |> Topology.addOutputEdge { from = NodeID 2, to = QueueID 2 }
                |> Topology.addInputEdge  { from = QueueID 2, to = NodeID 3 }

        sourceCfg =
            { arrivalRate = 0.3, jobPriority = Normal, jobLabel = "Job" }

        workerCfg =
            { serviceRate = 0.5, preemptive = False, signoff = Nothing }

        q1 =
            Queue.empty { capacity = 5, discipline = Queue.FIFO, overflow = Queue.Block }

        q2 =
            Queue.empty { capacity = 5, discipline = Queue.FIFO, overflow = Queue.Block }

        seed =
            Random.initialSeed 42

        ( firstJobID, state0 ) =
            SimState.nextJobID (SimState.init seed)

        state1 =
            state0
                |> SimState.putNode (NodeID 1) (makeSource "Source" sourceCfg)
                |> SimState.putNode (NodeID 2) (makeWorker "Worker" workerCfg)
                |> SimState.putNode (NodeID 3) (makeSink "Sink")
                |> SimState.putQueue (QueueID 1) q1
                |> SimState.putQueue (QueueID 2) q2
                -- kick off first arrival
                |> SimState.scheduleEvent
                    (Event.event (EventTime 0) (JobArrived (NodeID 1) firstJobID))
    in
    ( topo, state1 )


-- ── Model ─────────────────────────────────────────────────────────────────────

type alias Model =
    { topo     : Topology
    , simState : SimState
    , paused   : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( topo, simState ) =
            scenario
    in
    ( { topo = topo, simState = simState, paused = True }, Cmd.none )


-- ── Update ────────────────────────────────────────────────────────────────────

type Msg
    = Step
    | RunToEnd
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step ->
            ( { model | simState = Engine.processNextEvent model.topo model.simState }
            , Cmd.none
            )

        RunToEnd ->
            ( { model | simState = Engine.drainAll model.topo model.simState }
            , Cmd.none
            )

        Reset ->
            let
                ( topo, simState ) =
                    scenario
            in
            ( { model | topo = topo, simState = simState }, Cmd.none )


-- ── View ──────────────────────────────────────────────────────────────────────

view : Model -> Html Msg
view model =
    div [ style "font-family" "monospace", style "padding" "1rem" ]
        [ h2 [] [ text "elm-des — Discrete Event Simulation" ]
        , viewControls
        , viewClock model.simState
        , viewQueues model.simState
        , viewNodes model.simState
        , viewEventLog model.simState
        ]


viewControls : Html Msg
viewControls =
    div [ style "margin-bottom" "1rem" ]
        [ btn Step    "Step"
        , btn RunToEnd "Run to end"
        , btn Reset   "Reset"
        ]


btn : Msg -> String -> Html Msg
btn msg label =
    button
        [ onClick msg
        , style "margin-right" "0.5rem"
        , style "padding" "0.25rem 0.75rem"
        ]
        [ text label ]


viewClock : SimState -> Html msg
viewClock state =
    p [] [ text ("Clock: " ++ String.fromInt (eventTime2Int state.clock)) ]


viewQueues : SimState -> Html msg
viewQueues state =
    div []
        [ h2 [] [ text "Queues" ]
        , ul [] (Dict.toList state.queues |> List.map viewQueue)
        ]


viewQueue : ( Int, Queue.Queue ) -> Html msg
viewQueue ( qid, queue ) =
    let
        jobs =
            Queue.toList queue

        jobLabels =
            if List.isEmpty jobs then
                "(empty)"
            else
                jobs
                    |> List.map (\j -> j.label ++ "#" ++ String.fromInt (Id.jobIDInt j.id))
                    |> String.join ", "
    in
    li []
        [ text
            ("Q"
                ++ String.fromInt qid
                ++ " ["
                ++ String.fromInt (Queue.size queue)
                ++ "]: "
                ++ jobLabels
            )
        ]


viewNodes : SimState -> Html msg
viewNodes state =
    div []
        [ h2 [] [ text "Nodes" ]
        , ul [] (Dict.toList state.nodes |> List.map viewNode)
        ]


viewNode : ( Int, Node.NodeData ) -> Html msg
viewNode ( nid, node ) =
    let
        stateStr =
            case node.state of
                Idle ->
                    "idle"

                Busy jid (EventTime t) ->
                    "busy (job #"
                        ++ String.fromInt (Id.jobIDInt jid)
                        ++ ", done @"
                        ++ String.fromInt t
                        ++ ")"

                Blocked jid ->
                    "blocked (job #" ++ String.fromInt (Id.jobIDInt jid) ++ ")"

                Signoff jid _ ->
                    "awaiting signoff (job #" ++ String.fromInt (Id.jobIDInt jid) ++ ")"

                Preempted jid _ ->
                    "preempted (job #" ++ String.fromInt (Id.jobIDInt jid) ++ ")"

                Paused _ ->
                    "paused"
    in
    li [] [ text (node.label ++ " (N" ++ String.fromInt nid ++ "): " ++ stateStr) ]


viewEventLog : SimState -> Html msg
viewEventLog state =
    div []
        [ h2 [] [ text "Event log (newest first)" ]
        , ul
            [ style "max-height" "300px"
            , style "overflow-y" "auto"
            , style "border" "1px solid #ccc"
            , style "padding" "0.5rem"
            ]
            (state.eventLog |> List.map viewEvent)
        , h2 [] [ text "Pending events" ]
        , ul [] (state.eventQueue |> List.map viewEvent)
        ]


viewEvent : Event.Event -> Html msg
viewEvent evt =
    li []
        [ span [ style "color" "#888" ]
            [ text ("t=" ++ String.fromInt (eventTime2Int evt.time) ++ " ") ]
        , text (describeEvent evt.kind)
        ]


describeEvent : EventType -> String
describeEvent kind =
    case kind of
        JobArrived (NodeID nid) jid ->
            "JobArrived  node=" ++ String.fromInt nid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        ServiceStarted (NodeID nid) jid ->
            "ServiceStarted  node=" ++ String.fromInt nid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        ServiceComplete (NodeID nid) jid ->
            "ServiceComplete  node=" ++ String.fromInt nid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        JobEnqueued (QueueID qid) jid ->
            "JobEnqueued  queue=" ++ String.fromInt qid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        JobDequeued (QueueID qid) (NodeID nid) jid ->
            "JobDequeued  queue=" ++ String.fromInt qid ++ " → node=" ++ String.fromInt nid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        JobBlocked (QueueID qid) jid ->
            "JobBlocked  queue=" ++ String.fromInt qid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        JobDropped (QueueID qid) jid ->
            "JobDropped  queue=" ++ String.fromInt qid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        SignoffRequested (NodeID nid) _ jid ->
            "SignoffRequested  node=" ++ String.fromInt nid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        SignoffStarted (NodeID nid) _ jid ->
            "SignoffStarted  node=" ++ String.fromInt nid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        SignoffComplete (NodeID nid) _ jid ->
            "SignoffComplete  node=" ++ String.fromInt nid ++ " job=#" ++ String.fromInt (Id.jobIDInt jid)

        MeetingStarted ->
            "MeetingStarted"

        MeetingEnded ->
            "MeetingEnded"


-- ── Entry point ───────────────────────────────────────────────────────────────

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
