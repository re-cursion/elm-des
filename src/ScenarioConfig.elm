module ScenarioConfig exposing
    ( ScenarioConfig
    , MetaConfig
    , NodeSpec
    , NodeSpecKind(..)
    , QueueSpec
    , EdgeSpec
    , LockSpec
    , ScheduledEventSpec
    , decode
    , toTopologyAndState
    )

import Event exposing (EventType(..))
import EventTime exposing (EventTime(..))
import Id exposing (JobID(..), LockID(..), NodeID(..), QueueID(..))
import Job exposing (Priority(..))
import Json.Decode as D exposing (Decoder)
import Lock
import Node exposing (DispatchRule(..), NodeData, makeDispatcher, makeInterrupt, makeSink, makeSource, makeWorker)
import Queue
import Random
import ServiceTime exposing (ServiceTime(..))
import SimState exposing (SimState)
import Topology exposing (Topology)


-- ── Types ─────────────────────────────────────────────────────────────────────

type alias ScenarioConfig =
    { meta            : MetaConfig
    , nodes           : List NodeSpec
    , queues          : List QueueSpec
    , edges           : List EdgeSpec
    , locks           : List LockSpec
    , scheduledEvents : List ScheduledEventSpec
    }


type alias MetaConfig =
    { id    : String
    , title : String
    , theme : String
    , seed  : Int
    , speed : Float
    }


type alias NodeSpec =
    { id    : Int
    , kind  : NodeSpecKind
    , label : String
    , x     : Float
    , y     : Float
    , z     : Float   -- isometric depth (ignored by 2D renderer)
    , h     : Float   -- visual height in world units (1.0 = standard)
    }


type NodeSpecKind
    = SourceSpec
        { arrivalRate          : Float
        , priority             : Priority
        , highPriorityFraction : Float
        }
    | WorkerSpec
        { serviceTime : ServiceTime
        , preemptive  : Bool
        , signoff     : Maybe String
        }
    | DispatcherSpec
        { rule        : DispatchRule
        , serviceTime : Maybe ServiceTime
        }
    | SinkSpec
    | InterruptSpec


type alias QueueSpec =
    { id         : Int
    , label      : String
    , x          : Float
    , y          : Float
    , z          : Float   -- isometric depth (ignored by 2D renderer)
    , h          : Float   -- visual height in world units (0.3 = flat platform)
    , capacity   : Int
    , discipline : Queue.Discipline
    , overflow   : Queue.Overflow
    }


type alias EdgeSpec =
    { from : String
    , to   : String
    }


type alias LockSpec =
    { id          : String
    , label       : String
    , capacity    : Int
    , serviceTime : ServiceTime
    }


type alias ScheduledEventSpec =
    { at       : Int
    , kind     : String
    , nodeId   : Maybe Int
    , duration : Maybe Int
    , label    : Maybe String
    }


-- ── JSON Decoder ──────────────────────────────────────────────────────────────

decode : String -> Result String ScenarioConfig
decode json =
    D.decodeString scenarioDecoder json
        |> Result.mapError D.errorToString


scenarioDecoder : Decoder ScenarioConfig
scenarioDecoder =
    D.map6 ScenarioConfig
        (D.field "meta"  metaDecoder)
        (D.field "nodes" (D.list nodeDecoder))
        (D.field "queues" (D.list queueDecoder))
        (D.field "edges" (D.list edgeDecoder))
        (D.oneOf [ D.field "locks" (D.list lockDecoder), D.succeed [] ])
        (D.oneOf [ D.field "scheduledEvents" (D.list scheduledEventDecoder), D.succeed [] ])


metaDecoder : Decoder MetaConfig
metaDecoder =
    D.map5 MetaConfig
        (D.field "id"    D.string)
        (D.field "title" D.string)
        (D.field "theme" D.string)
        (D.oneOf [ D.field "seed"  D.int,   D.succeed 42 ])
        (D.oneOf [ D.field "speed" D.float, D.succeed 10.0 ])


nodeDecoder : Decoder NodeSpec
nodeDecoder =
    D.field "kind" D.string
        |> D.andThen
            (\kind ->
                D.map7 NodeSpec
                    (D.field "id"    D.int)
                    (nodeKindDecoder kind)
                    (D.field "label" D.string)
                    (D.oneOf [ D.field "x" D.float, D.succeed 0.0 ])
                    (D.oneOf [ D.field "y" D.float, D.succeed 0.0 ])
                    (D.oneOf [ D.field "z" D.float, D.succeed 0.0 ])
                    (D.oneOf [ D.field "h" D.float, D.succeed 1.0 ])
            )


nodeKindDecoder : String -> Decoder NodeSpecKind
nodeKindDecoder kind =
    case kind of
        "source" ->
            D.map3
                (\ar prio hpf ->
                    SourceSpec { arrivalRate = ar, priority = prio, highPriorityFraction = hpf }
                )
                (D.oneOf [ D.field "arrivalRate" D.float, D.succeed 0.3 ])
                (D.oneOf [ D.field "priority" priorityDecoder, D.succeed Normal ])
                (D.oneOf [ D.field "highPriorityFraction" D.float, D.succeed 0.0 ])

        "worker" ->
            D.map3
                (\st pre sg ->
                    WorkerSpec { serviceTime = st, preemptive = pre, signoff = sg }
                )
                (D.field "serviceTime" serviceTimeDecoder)
                (D.oneOf [ D.field "preemptive" D.bool, D.succeed False ])
                (D.oneOf [ D.field "signoff" (D.nullable D.string), D.succeed Nothing ])

        "dispatcher" ->
            D.map2
                (\rule st -> DispatcherSpec { rule = rule, serviceTime = st })
                (D.oneOf [ D.field "rule" dispatchRuleDecoder, D.succeed ShortestQueue ])
                (D.oneOf [ D.map Just (D.field "serviceTime" serviceTimeDecoder), D.succeed Nothing ])

        "sink" ->
            D.succeed SinkSpec

        "boss" ->
            D.succeed InterruptSpec

        "interrupt" ->
            D.succeed InterruptSpec

        _ ->
            D.fail ("Unknown node kind: " ++ kind)


queueDecoder : Decoder QueueSpec
queueDecoder =
    D.map8
        (\id label x y z capacity disc ov ->
            { id         = id
            , label      = label
            , x          = x
            , y          = y
            , z          = z
            , h          = 1.0   -- default; overridden below
            , capacity   = capacity
            , discipline = disc
            , overflow   = ov
            }
        )
        (D.field "id"       D.int)
        (D.field "label"    D.string)
        (D.oneOf [ D.field "x" D.float, D.succeed 0.0 ])
        (D.oneOf [ D.field "y" D.float, D.succeed 0.0 ])
        (D.oneOf [ D.field "z" D.float, D.succeed 0.0 ])
        (D.field "capacity" D.int)
        (D.oneOf [ D.field "discipline" disciplineDecoder, D.succeed Queue.FIFO ])
        (D.oneOf [ D.field "overflow"   overflowDecoder,   D.succeed Queue.Block ])
        |> D.andThen
            (\spec ->
                D.oneOf [ D.field "h" D.float, D.succeed 0.3 ]
                    |> D.map (\h -> { spec | h = h })
            )


edgeDecoder : Decoder EdgeSpec
edgeDecoder =
    D.map2 EdgeSpec
        (D.field "from" D.string)
        (D.field "to"   D.string)


lockDecoder : Decoder LockSpec
lockDecoder =
    D.map4 LockSpec
        (D.field "id"          D.string)
        (D.field "label"       D.string)
        (D.field "capacity"    D.int)
        (D.field "serviceTime" serviceTimeDecoder)


scheduledEventDecoder : Decoder ScheduledEventSpec
scheduledEventDecoder =
    D.map5 ScheduledEventSpec
        (D.field "at"   D.int)
        (D.field "kind" D.string)
        (D.oneOf [ D.field "nodeId"   (D.map Just D.int),    D.succeed Nothing ])
        (D.oneOf [ D.field "duration" (D.map Just D.int),    D.succeed Nothing ])
        (D.oneOf [ D.field "label"    (D.map Just D.string), D.succeed Nothing ])


serviceTimeDecoder : Decoder ServiceTime
serviceTimeDecoder =
    D.field "kind" D.string
        |> D.andThen
            (\k ->
                case k of
                    "exponential" ->
                        D.map Exponential (D.field "rate" D.float)

                    "log-normal" ->
                        D.map2 LogNormal
                            (D.field "mu"    D.float)
                            (D.field "sigma" D.float)

                    "erlang" ->
                        D.map2 Erlang
                            (D.field "k"    D.int)
                            (D.field "rate" D.float)

                    "deterministic" ->
                        D.map Deterministic (D.field "ticks" D.int)

                    "uniform" ->
                        D.map2 Uniform
                            (D.field "lo" D.int)
                            (D.field "hi" D.int)

                    _ ->
                        D.fail ("Unknown service time kind: " ++ k)
            )


priorityDecoder : Decoder Priority
priorityDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "critical" -> D.succeed Critical
                    "high"     -> D.succeed High
                    "normal"   -> D.succeed Normal
                    "low"      -> D.succeed Low
                    _          -> D.fail ("Unknown priority: " ++ s)
            )


dispatchRuleDecoder : Decoder DispatchRule
dispatchRuleDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "shortest-queue" -> D.succeed ShortestQueue
                    "round-robin"    -> D.succeed RoundRobin
                    "random"         -> D.succeed RandomChoice
                    _                -> D.fail ("Unknown dispatch rule: " ++ s)
            )


disciplineDecoder : Decoder Queue.Discipline
disciplineDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "fifo"          -> D.succeed Queue.FIFO
                    "priority-fifo" -> D.succeed Queue.PriorityFIFO
                    "lifo"          -> D.succeed Queue.LIFO
                    _               -> D.fail ("Unknown discipline: " ++ s)
            )


overflowDecoder : Decoder Queue.Overflow
overflowDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "block"                -> D.succeed Queue.Block
                    "drop-first"           -> D.succeed Queue.DropFirst
                    "drop-last"            -> D.succeed Queue.DropLast
                    "drop-lowest-priority" -> D.succeed Queue.DropLowestPriority
                    _                      -> D.fail ("Unknown overflow: " ++ s)
            )


-- ── Build Topology + SimState ─────────────────────────────────────────────────

toTopologyAndState : ScenarioConfig -> Result String ( Topology, SimState )
toTopologyAndState cfg =
    case buildTopology cfg.edges of
        Err e ->
            Err e

        Ok topo ->
            let
                seed =
                    Random.initialSeed cfg.meta.seed

                ( firstJobId, s0 ) =
                    SimState.nextJobID (SimState.init seed)

                s1 =
                    List.foldl
                        (\spec state ->
                            case nodeSpecToData spec of
                                Ok nodeData ->
                                    SimState.putNode (NodeID spec.id) nodeData state

                                Err _ ->
                                    state
                        )
                        s0
                        cfg.nodes

                s2 =
                    List.foldl
                        (\spec state ->
                            SimState.putQueue
                                (QueueID spec.id)
                                (Queue.empty
                                    { capacity   = spec.capacity
                                    , discipline = spec.discipline
                                    , overflow   = spec.overflow
                                    }
                                )
                                state
                        )
                        s1
                        cfg.queues

                s3 =
                    List.foldl
                        (\spec state ->
                            SimState.putLock
                                (LockID spec.id)
                                (Lock.newLock
                                    { id          = LockID spec.id
                                    , label       = spec.label
                                    , capacity    = spec.capacity
                                    , serviceTime = spec.serviceTime
                                    }
                                )
                                state
                        )
                        s2
                        cfg.locks

                sourceNodeId =
                    cfg.nodes
                        |> List.filter
                            (\n ->
                                case n.kind of
                                    SourceSpec _ -> True
                                    _            -> False
                            )
                        |> List.head
                        |> Maybe.map .id

                s4 =
                    case sourceNodeId of
                        Just nid ->
                            SimState.scheduleEvent
                                (Event.event (EventTime 0) (JobArrived (NodeID nid) firstJobId))
                                s3

                        Nothing ->
                            s3

                s5 =
                    List.foldl
                        (\spec state ->
                            case spec.kind of
                                "boss-meeting" ->
                                    case spec.duration of
                                        Just dur ->
                                            state
                                                |> SimState.scheduleEvent
                                                    (Event.event (EventTime spec.at) InterruptStarted)
                                                |> SimState.scheduleEvent
                                                    (Event.event (EventTime (spec.at + dur)) InterruptEnded)

                                        Nothing ->
                                            state

                                _ ->
                                    state
                        )
                        s4
                        cfg.scheduledEvents
            in
            Ok ( topo, s5 )


nodeSpecToData : NodeSpec -> Result String NodeData
nodeSpecToData spec =
    case spec.kind of
        SourceSpec c ->
            Ok
                (makeSource spec.label
                    { arrivalRate          = c.arrivalRate
                    , jobPriority          = c.priority
                    , highPriorityFraction = c.highPriorityFraction
                    , jobLabel             = spec.label
                    }
                )

        WorkerSpec c ->
            Ok
                (makeWorker spec.label
                    { serviceTime = c.serviceTime
                    , preemptive  = c.preemptive
                    , signoff     = Maybe.map LockID c.signoff
                    , halted      = False
                    }
                )

        DispatcherSpec c ->
            Ok (makeDispatcher spec.label { rule = c.rule, roundRobinIndex = 0, serviceTime = c.serviceTime })

        SinkSpec ->
            Ok (makeSink spec.label)

        InterruptSpec ->
            Ok (makeInterrupt spec.label)


-- ── Edge parsing ──────────────────────────────────────────────────────────────

type Endpoint
    = NodeEp NodeID
    | QueueEp QueueID


parseEndpoint : String -> Result String Endpoint
parseEndpoint s =
    case String.split ":" s of
        [ "node", n ] ->
            case String.toInt n of
                Just i  -> Ok (NodeEp (NodeID i))
                Nothing -> Err ("Invalid node id in endpoint: " ++ s)

        [ "queue", n ] ->
            case String.toInt n of
                Just i  -> Ok (QueueEp (QueueID i))
                Nothing -> Err ("Invalid queue id in endpoint: " ++ s)

        _ ->
            Err ("Endpoint must be \"node:N\" or \"queue:N\", got: " ++ s)


buildTopology : List EdgeSpec -> Result String Topology
buildTopology edges =
    List.foldl
        (\edge acc ->
            case acc of
                Err e ->
                    Err e

                Ok topo ->
                    case ( parseEndpoint edge.from, parseEndpoint edge.to ) of
                        ( Ok (NodeEp nid), Ok (QueueEp qid) ) ->
                            Ok (Topology.addOutputEdge { from = nid, to = qid } topo)

                        ( Ok (QueueEp qid), Ok (NodeEp nid) ) ->
                            Ok (Topology.addInputEdge { from = qid, to = nid } topo)

                        ( Err e, _ ) ->
                            Err e

                        ( _, Err e ) ->
                            Err e

                        _ ->
                            Err
                                ("Edge must connect node→queue or queue→node: "
                                    ++ edge.from
                                    ++ " → "
                                    ++ edge.to
                                )
        )
        (Ok Topology.empty)
        edges
