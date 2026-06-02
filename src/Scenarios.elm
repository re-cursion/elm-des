module Scenarios exposing
    ( singleWorker
    , twoParallel
    , threePipeline
    , belterShipyard
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
      "rule": "shortest-queue",
      "serviceTime": { "kind": "deterministic", "ticks": 3 } },
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


belterShipyard : String
belterShipyard =
    """{
  "meta": {
    "id": "belter-shipyard",
    "title": "Tycho Station — Ship Docking",
    "theme": "expanse",
    "seed": 42,
    "speed": 10
  },
  "nodes": [
    { "id": 1, "kind": "source",
      "label": "Arrivals", "x": 70, "y": 170, "h": 1.0,
      "arrivalRate": 0.45, "priority": "normal", "highPriorityFraction": 0.25 },
    { "id": 2, "kind": "dispatcher",
      "label": "Traffic Control", "x": 290, "y": 170, "h": 1.4,
      "rule": "shortest-queue",
      "serviceTime": { "kind": "deterministic", "ticks": 2 } },
    { "id": 3, "kind": "worker",
      "label": "Dock A — Light Craft", "x": 560, "y": 60, "h": 0.9,
      "serviceTime": { "kind": "log-normal", "mu": 1.0, "sigma": 0.3 },
      "preemptive": true },
    { "id": 4, "kind": "worker",
      "label": "Dock B — Freighter", "x": 560, "y": 170, "h": 1.3,
      "serviceTime": { "kind": "log-normal", "mu": 1.8, "sigma": 0.5 },
      "preemptive": true },
    { "id": 5, "kind": "worker",
      "label": "Dock C — Heavy Refit", "x": 560, "y": 280, "h": 1.8,
      "serviceTime": { "kind": "log-normal", "mu": 2.6, "sigma": 0.6 },
      "preemptive": false },
    { "id": 6, "kind": "sink",
      "label": "Departure", "x": 790, "y": 170, "h": 1.0 },
    { "id": 7, "kind": "interrupt",
      "label": "Bosmang", "x": 290, "y": 30, "h": 0.5 }
  ],
  "queues": [
    { "id": 1, "label": "Holding Pattern", "x": 180, "y": 170, "h": 0.4, "capacity": 8, "discipline": "priority-fifo", "overflow": "block" },
    { "id": 2, "label": "Bay A", "x": 430, "y": 60,  "h": 0.4, "capacity": 1, "discipline": "fifo", "overflow": "block" },
    { "id": 3, "label": "Bay B", "x": 430, "y": 170, "h": 0.4, "capacity": 1, "discipline": "fifo", "overflow": "block" },
    { "id": 4, "label": "Bay C", "x": 430, "y": 280, "h": 0.4, "capacity": 1, "discipline": "fifo", "overflow": "block" },
    { "id": 5, "label": "Outbound", "x": 680, "y": 170, "h": 0.4, "capacity": 6, "discipline": "fifo", "overflow": "block" }
  ],
  "edges": [
    { "from": "node:1",  "to": "queue:1" },
    { "from": "queue:1", "to": "node:2"  },
    { "from": "node:2",  "to": "queue:2" },
    { "from": "node:2",  "to": "queue:3" },
    { "from": "node:2",  "to": "queue:4" },
    { "from": "queue:2", "to": "node:3"  },
    { "from": "queue:3", "to": "node:4"  },
    { "from": "queue:4", "to": "node:5"  },
    { "from": "node:3",  "to": "queue:5" },
    { "from": "node:4",  "to": "queue:5" },
    { "from": "node:5",  "to": "queue:5" },
    { "from": "queue:5", "to": "node:6"  }
  ],
  "scheduledEvents": [
    { "at": 200, "kind": "boss-meeting", "duration": 35, "label": "Bosmang All-Hands" },
    { "at": 500, "kind": "boss-meeting", "duration": 25, "label": "Safety Briefing" }
  ]
}"""
