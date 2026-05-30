module Scenarios exposing
    ( singleWorker
    , twoParallel
    , threePipeline
    )

{-| Bundled scenario JSON strings.

These are the same three topologies as the legacy hardcoded presets in Main.elm,
expressed as ScenarioConfig JSON so they can be loaded via ScenarioConfig.decode
or passed as browser flags from index.html.
-}


singleWorker : String
singleWorker =
    """{
  "meta": {
    "id": "single-worker",
    "title": "M/M/1 — Single Worker",
    "theme": "plain",
    "seed": 42,
    "speed": 10
  },
  "nodes": [
    { "id": 1, "kind": "source",    "label": "Source",    "x": 164, "y": 90,
      "arrivalRate": 0.3, "priority": "normal", "highPriorityFraction": 0.15 },
    { "id": 2, "kind": "worker",    "label": "Worker",    "x": 399, "y": 90,
      "serviceTime": { "kind": "log-normal", "mu": 1.2, "sigma": 0.4 },
      "preemptive": true },
    { "id": 3, "kind": "sink",      "label": "Sink",      "x": 634, "y": 90 },
    { "id": 6, "kind": "interrupt", "label": "Interrupt", "x": 702, "y": 20, "h": 0.5 }
  ],
  "queues": [
    { "id": 1, "label": "Q1", "x": 254, "y": 90, "capacity": 5,
      "discipline": "fifo", "overflow": "block" },
    { "id": 2, "label": "Q2", "x": 544, "y": 90, "capacity": 5,
      "discipline": "fifo", "overflow": "block" }
  ],
  "edges": [
    { "from": "node:1",  "to": "queue:1" },
    { "from": "queue:1", "to": "node:2"  },
    { "from": "node:2",  "to": "queue:2" },
    { "from": "queue:2", "to": "node:3"  }
  ],
  "scheduledEvents": [
    { "at": 150, "kind": "boss-meeting", "duration": 20, "label": "Interrupt 1" },
    { "at": 350, "kind": "boss-meeting", "duration": 20, "label": "Interrupt 2" }
  ]
}"""


twoParallel : String
twoParallel =
    """{
  "meta": {
    "id": "two-parallel",
    "title": "M/M/2 — Two Parallel Workers",
    "theme": "plain",
    "seed": 42,
    "speed": 10
  },
  "nodes": [
    { "id": 1, "kind": "source",     "label": "Source",    "x": 18,  "y": 125,
      "arrivalRate": 0.3, "priority": "normal", "highPriorityFraction": 0.15 },
    { "id": 2, "kind": "dispatcher", "label": "Dispatch",  "x": 192, "y": 125,
      "rule": "shortest-queue" },
    { "id": 3, "kind": "worker",     "label": "Worker A",  "x": 384, "y": 82,
      "serviceTime": { "kind": "log-normal", "mu": 1.2, "sigma": 0.4 },
      "preemptive": false },
    { "id": 4, "kind": "worker",     "label": "Worker B",  "x": 384, "y": 168,
      "serviceTime": { "kind": "log-normal", "mu": 1.2, "sigma": 0.4 },
      "preemptive": false, "signoff": "inspector" },
    { "id": 5, "kind": "sink",       "label": "Sink",      "x": 630, "y": 125 },
    { "id": 6, "kind": "interrupt",  "label": "Interrupt", "x": 702, "y": 20, "h": 0.5 }
  ],
  "queues": [
    { "id": 1, "label": "Q1", "x": 105, "y": 125, "capacity": 5, "discipline": "fifo", "overflow": "block" },
    { "id": 2, "label": "Q2", "x": 282, "y": 82,  "capacity": 5, "discipline": "fifo", "overflow": "block" },
    { "id": 3, "label": "Q3", "x": 282, "y": 168, "capacity": 5, "discipline": "fifo", "overflow": "block" },
    { "id": 4, "label": "Q4", "x": 490, "y": 125, "capacity": 5, "discipline": "fifo", "overflow": "block" }
  ],
  "edges": [
    { "from": "node:1",  "to": "queue:1" },
    { "from": "queue:1", "to": "node:2"  },
    { "from": "node:2",  "to": "queue:2" },
    { "from": "node:2",  "to": "queue:3" },
    { "from": "queue:2", "to": "node:3"  },
    { "from": "queue:3", "to": "node:4"  },
    { "from": "node:3",  "to": "queue:4" },
    { "from": "node:4",  "to": "queue:4" },
    { "from": "queue:4", "to": "node:5"  }
  ],
  "locks": [
    { "id": "inspector", "label": "Inspector", "capacity": 1,
      "serviceTime": { "kind": "deterministic", "ticks": 5 } }
  ],
  "scheduledEvents": [
    { "at": 150, "kind": "boss-meeting", "duration": 20, "label": "Interrupt 1" },
    { "at": 350, "kind": "boss-meeting", "duration": 20, "label": "Interrupt 2" }
  ]
}"""


threePipeline : String
threePipeline =
    """{
  "meta": {
    "id": "three-pipeline",
    "title": "Pipeline — 3 Sequential Stages",
    "theme": "plain",
    "seed": 42,
    "speed": 10
  },
  "nodes": [
    { "id": 1, "kind": "source",    "label": "Source",  "x": 69,  "y": 90,
      "arrivalRate": 0.3, "priority": "normal", "highPriorityFraction": 0.15 },
    { "id": 2, "kind": "worker",    "label": "Stage 1", "x": 215, "y": 90,
      "serviceTime": { "kind": "log-normal", "mu": 1.2, "sigma": 0.4 },
      "preemptive": true },
    { "id": 3, "kind": "worker",    "label": "Stage 2", "x": 387, "y": 90,
      "serviceTime": { "kind": "log-normal", "mu": 1.2, "sigma": 0.4 },
      "preemptive": true },
    { "id": 4, "kind": "worker",    "label": "Stage 3", "x": 559, "y": 90,
      "serviceTime": { "kind": "log-normal", "mu": 1.2, "sigma": 0.4 },
      "preemptive": true },
    { "id": 5, "kind": "sink",      "label": "Sink",    "x": 691, "y": 90 },
    { "id": 6, "kind": "interrupt", "label": "Interrupt", "x": 738, "y": 20, "h": 0.5 }
  ],
  "queues": [
    { "id": 1, "label": "Q1", "x": 129, "y": 90, "capacity": 5, "discipline": "fifo", "overflow": "block" },
    { "id": 2, "label": "Q2", "x": 301, "y": 90, "capacity": 5, "discipline": "fifo", "overflow": "block" },
    { "id": 3, "label": "Q3", "x": 473, "y": 90, "capacity": 5, "discipline": "fifo", "overflow": "block" },
    { "id": 4, "label": "Q4", "x": 645, "y": 90, "capacity": 5, "discipline": "fifo", "overflow": "block" }
  ],
  "edges": [
    { "from": "node:1",  "to": "queue:1" },
    { "from": "queue:1", "to": "node:2"  },
    { "from": "node:2",  "to": "queue:2" },
    { "from": "queue:2", "to": "node:3"  },
    { "from": "node:3",  "to": "queue:3" },
    { "from": "queue:3", "to": "node:4"  },
    { "from": "node:4",  "to": "queue:4" },
    { "from": "queue:4", "to": "node:5"  }
  ],
  "scheduledEvents": [
    { "at": 150, "kind": "boss-meeting", "duration": 20, "label": "Interrupt 1" },
    { "at": 350, "kind": "boss-meeting", "duration": 20, "label": "Interrupt 2" }
  ]
}"""
