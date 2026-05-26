module Topology exposing
    ( Topology
    , empty
    , addEdge
    , addInputEdge
    , addOutputEdge
    , nodeInput
    , nodeOutputs
    , queueConsumers
    )

import Dict exposing (Dict)
import Id exposing (NodeID(..), QueueID(..))


-- nodeInput    : which queue a node pulls from (single input per node)
-- nodeOutputs  : which queues a node pushes finished jobs into
-- queueConsumers : which nodes pull from a given queue (inverse of nodeInput)
type alias Topology =
    { nodeInput      : Dict Int QueueID
    , nodeOutputs    : Dict Int (List QueueID)
    , queueConsumers : Dict Int (List NodeID)
    }


empty : Topology
empty =
    { nodeInput = Dict.empty
    , nodeOutputs = Dict.empty
    , queueConsumers = Dict.empty
    }


-- Wire a node's single input queue: queue → node
addInputEdge : { from : QueueID, to : NodeID } -> Topology -> Topology
addInputEdge { from, to } topo =
    let
        (NodeID nid) = to
        (QueueID qid) = from
    in
    { topo
        | nodeInput =
            Dict.insert nid from topo.nodeInput
        , queueConsumers =
            Dict.update qid
                (Just << (::) to << Maybe.withDefault [])
                topo.queueConsumers
    }


-- Wire one of a node's output queues: node → queue
addOutputEdge : { from : NodeID, to : QueueID } -> Topology -> Topology
addOutputEdge { from, to } topo =
    let
        (NodeID nid) = from
    in
    { topo
        | nodeOutputs =
            Dict.update nid
                (Just << (::) to << Maybe.withDefault [])
                topo.nodeOutputs
    }


-- Convenience: add both directions at once for a queue that sits between two nodes
addEdge : { from : QueueID, to : NodeID } -> Topology -> Topology
addEdge =
    addInputEdge


nodeInput : NodeID -> Topology -> Maybe QueueID
nodeInput (NodeID nid) topo =
    Dict.get nid topo.nodeInput


nodeOutputs : NodeID -> Topology -> List QueueID
nodeOutputs (NodeID nid) topo =
    Dict.get nid topo.nodeOutputs |> Maybe.withDefault []


queueConsumers : QueueID -> Topology -> List NodeID
queueConsumers (QueueID qid) topo =
    Dict.get qid topo.queueConsumers |> Maybe.withDefault []
