# elm-des — Discrete Event Simulation in the Browser

A visual, interactive discrete event simulation (DES) engine written in Elm,
running entirely in the browser. The goal is to make it easy to build, run,
and observe queuing networks — from a simple assembly line to a supermarket,
a CPU pipeline, or a software team.

---

## Core Concepts

### Jobs (formerly "Work")

A **Job** is the unit of work flowing through the system. It carries:

- A unique `JobID`
- Timestamps of when it entered/left each node and queue (for cycle-time
  metrics)
- Any payload relevant to the theme (e.g. "customer", "packet", "ticket")

### Queues

A **Queue** is a bounded FIFO buffer that sits *between* nodes. It has:

- A maximum capacity
- An overflow behaviour: `Block | DropFirst | DropLast`
- No processing logic — it only holds jobs

### Nodes

A **Node** does work. Every node has exactly the same interface to the network:
it reads from **exactly one** input queue and writes to **one or more** output
queues. The *kind* of work it performs determines its subtype:

| Node kind | What it does |
|---|---|
| **Source** | Generates new jobs (Poisson inter-arrival times) |
| **Worker** | Processes one job at a time (Poisson service time) |
| **Dispatcher** | Routes jobs to one of N output queues (with delay) |
| **Sink** | Absorbs finished jobs and records completion stats |
| **Boss** *(later)* | Broadcasts a "stop" event that pauses all Workers |

### Events

Everything that happens is an **Event** — a timestamped fact about a state
change. Events are the only mechanism of causality in the simulation.

```
type EventType
    = JobArrived      NodeID JobID
    | ServiceStarted  NodeID JobID
    | ServiceComplete NodeID JobID
    | JobEnqueued     QueueID JobID
    | JobDequeued     QueueID NodeID JobID
    | JobBlocked      QueueID JobID          -- queue full, job waiting
    | JobDropped      QueueID JobID          -- job discarded (Drop behaviour)
    | SignoffRequested NodeID LockID JobID   -- worker waiting for approver
    | SignoffStarted   NodeID LockID JobID   -- approver began inspection
    | SignoffComplete  NodeID LockID JobID   -- job cleared; worker unblocked
    | MeetingStarted                         -- boss node: all workers pause
    | MeetingEnded
```

The event log is the ground truth of the simulation — metrics, replays, and
visualisations are all derived from it.

---

## Architecture

### The Bipartite Topology Constraint

The single most important structural rule:

> **Nodes only connect to Queues. Queues only connect to Nodes.**
> No node-to-node edges. No queue-to-queue edges.

This gives us a **bipartite graph**: `Node ↔ Queue ↔ Node ↔ Queue …`

Benefits:
- The system is always analysable: throughput, utilisation, and cycle time
  are always well-defined.
- Adding a dispatcher or boss node fits naturally — they are just nodes with
  different processing logic, the graph structure stays the same.
- Visualisation is straightforward: alternate columns/rows of nodes and queues.

### Separating Topology from State

The current code embeds input/output queue IDs *inside* each Resource. This
means the topology (who connects to whom) is tangled with the runtime state
(idle/busy, current job). That makes it hard to query "which nodes pull from
queue Q?" without scanning all nodes.

**Proposed split:**

```elm
-- Static — defined once, never changes during a run
type alias Topology =
    { nodeInput   : Dict NodeID QueueID          -- node's single input queue
    , nodeOutputs : Dict NodeID (List QueueID)   -- node → queues it feeds into
    , queueOutputs : Dict QueueID (List NodeID)  -- queue → nodes that pull from it
    }

-- Dynamic — changes with every event
type alias SimState =
    { nodes      : Dict NodeID  NodeState
    , queues     : Dict QueueID QueueState
    , locks      : Dict LockID  LockState   -- sign-off resource pools
    , clock      : EventTime
    , eventQueue : List Event               -- sorted by time, next event at head
    , eventLog   : List Event               -- full history for metrics / replay
    }

type alias LockState =
    { config   : Lock
    , inUse    : Int                        -- currently occupied slots
    , waiters  : List (NodeID, JobID)       -- workers waiting for a slot (FIFO)
    }
```

`Topology` is essentially two dictionaries that together form the bipartite
adjacency relation. Together they can answer both directions of traversal in O(1).

### On the "Matrix" Question

A 2-D matrix `Bool[numNodes][numQueues]` can represent the same bipartite
graph, and is intuitive as a grid. However in Elm it would be
`Array (Array Bool)` which requires index arithmetic and is harder to extend
(adding a node means resizing every row).

`Dict NodeID (List QueueID)` achieves the same adjacency lookup, reads
naturally in Elm, and handles sparse topologies (most nodes only touch 1–2
queues). The dictionary approach is recommended. If a grid-style UI for
editing connectivity is wanted later, it can be rendered from the dicts without
the simulation engine needing to use arrays internally.

### Event-Driven Simulation Engine

The simulation clock should **jump to the next event time**, not advance by
fixed increments. This is the standard DES approach and allows simulating
long time horizons instantly.

```
processNextEvent : Topology -> SimState -> SimState
processNextEvent topology state =
    case state.eventQueue of
        [] ->
            state  -- simulation complete

        (nextEvent :: rest) ->
            let
                state1 = { state | clock = timeOf nextEvent, eventQueue = rest }
                newEvents = handleEvent topology state1 nextEvent
            in
            insertAll newEvents state1
            |> appendLog nextEvent
```

`handleEvent` dispatches on `EventType` and returns a list of new events to
schedule. State transitions and event generation are always co-located in the
same handler, making causality easy to trace.

### Node State Machine

Every Worker node follows:

```
Idle ──(JobDequeued)──► Busy
Busy ──(ServiceComplete)──► Idle
```

When a Worker becomes Idle it immediately attempts to dequeue from its single
input queue. When service completes it checks whether a sign-off is required
(see below); if not, it immediately attempts to enqueue into each of its output
queues in turn until one accepts the job. If all output queues are full (Block
behaviour) the worker enters `WaitingForOutputSpace` and retries whenever a
`JobDequeued` event fires on any of its output queues.

### Sign-off Resources

Some nodes require approval from a separate, limited resource before a finished
job can leave. The approver (bosmang, quality inspector, safety officer) has
finite capacity and takes time — so workers queue up for their attention just
as jobs queue up for workers.

#### How it works

A **Lock** is a named resource with a capacity and a service time:

```elm
type LockID = LockID String

type alias Lock =
    { id          : LockID
    , label       : String
    , capacity    : Int       -- how many simultaneous sign-offs
    , serviceTime : Float     -- mean time to approve one job (Poisson)
    }
```

A Worker node may declare `signoff : Maybe LockID`. When it does, after
completing service the worker does **not** immediately push the job to its
output queue. Instead:

1. Worker enters `WaitingForSignoff JobID LockID` — it cannot start a new job.
2. If the lock has a free slot, the sign-off starts immediately; otherwise the
   worker waits until a slot opens.
3. After sign-off service time elapses a `SignoffComplete` event fires.
4. The job is pushed to the output queue(s); the lock slot is freed; the worker
   becomes `Idle`.

The worker is **blocked** for the entire sign-off duration. This models the
real constraint: the dock worker must be present while the bosmang inspects,
so the bay stays occupied.

#### Expanded Worker state machine

```
Idle
 │  JobDequeued
 ▼
Busy (JobID, completionTime)
 │  ServiceComplete
 ▼
WaitingForSignoff (JobID, LockID)  ◄── only if signoff configured
 │  SignoffComplete
 ▼
WaitingForOutputSpace (JobID)      ◄── only if all output queues full
 │  QueueSpaceAvailable
 ▼
Idle
```

If no sign-off is configured, the worker goes straight from `Busy` →
`WaitingForOutputSpace` → `Idle` (skipping the signoff step).

#### Locks vs. Boss node

| | Lock | Boss node |
|---|---|---|
| **Triggered by** | Worker finishing a job | Scheduled time or external event |
| **Who waits** | The worker that finished | All workers simultaneously |
| **Duration** | Per-job service time | Fixed meeting duration |
| **Capacity** | N simultaneous approvals | N/A (broadcast) |

They compose: a bosmang can be both a Boss node (calls all-hands meetings) *and*
a Lock resource (signs off individual jobs). A warship arriving might trigger an
early meeting *and* jump the sign-off queue, stacking multiple demo-worthy
effects at once.

#### In the Belter shipyard

The bosmang inspects every completed repair before a ship is cleared for
departure. Dock workers line up for the bosmang's attention while the airlock
queue builds behind them — a natural bottleneck that shows clearly in the
utilisation and cycle-time charts.

```json
"locks": [
  { "id": "bosmang", "label": "Bosmang inspection", "capacity": 1, "serviceTime": 4.0 }
],
"nodes": [
  { "id": 2, "kind": "worker", "label": "Hull welder", "signoff": "bosmang", ... }
]
```

---

### Job Priorities

Some jobs are more urgent than others — a warship needs to be combat-ready,
an ambulance cannot wait behind a delivery truck. Priorities affect two
independent decisions:

#### 1. Queue ordering (which job is served next)

`PriorityFIFO` keeps the queue sorted by priority, with arrival order as the
tiebreaker. When a new job is enqueued it is inserted at the correct position
rather than appended. The head of the queue is always the highest-priority
waiting job.

`FIFO` queues ignore priority entirely — useful for stages where strict arrival
order must be preserved (e.g. an assembly line conveyor).

#### 2. Overflow eviction (what happens when the queue is full)

`DropLowestPriority` is a new overflow behaviour: when the queue is at
capacity, the lowest-priority job already in the queue is dropped to make room
for the incoming job *if* the incoming job has higher priority. If the incoming
job is the lowest-priority job, it is dropped instead. This is particularly
dramatic in a demo — a warship arrives and immediately bumps a freighter out of
the queue.

#### 3. Preemption (can a new job interrupt a worker mid-task?)

This is per-worker and optional. Two modes:

| Mode | Behaviour |
|---|---|
| **Non-preemptive** | Higher-priority jobs jump the queue but cannot interrupt a job already being processed. Simpler; most real systems work this way. |
| **Preemptive** | A `Critical` job arriving at a busy worker interrupts the current job. The remaining service time is saved; the interrupted job re-enters the input queue at its original priority. |

Preemption produces striking demo moments: a warship arrives, a freighter is
immediately put down mid-weld, and the dock worker switches tasks. The
interrupted freighter's total cycle time visibly increases in the charts.

The `Preempted JobID EventTime` activity state carries the remaining time so
that if the interrupted job is picked up again it does not restart from scratch.

#### In the Belter shipyard theme

| Priority | Ship type |
|---|---|
| `Critical` | MCRN or UNN warship under combat orders |
| `High` | Medivac / emergency supply run |
| `Normal` | Civilian freighter |
| `Low` | Decommissioned hulk being stripped for parts |

When the bosmang sees a warship arrive he might also call the all-hands earlier
than scheduled — a combined Boss + Priority event that showcases both mechanics
at once.

---

### Playback Layer (Engine vs. Display)

The simulation engine and the display are deliberately decoupled:

- **Engine**: always computes at full speed — one call to `processNextEvent`
  costs only a Dict lookup and a list insert.
- **Playback layer**: decides *when* to call the engine and push the result to
  the view.

This means the engine never contains `sleep` or `Time` subscriptions.
Those live entirely in `Main.elm`'s subscription/update loop.

#### Playback modes

| Mode | Behaviour |
|---|---|
| `Paused` | Nothing advances; user must click "Step" |
| `Stepping` | One event per click; good for debugging |
| `Playing N` | `Time.every` fires at a rate derived from `N`; see below |

#### `Playing N` — wall-clock pacing

`N` is expressed as **simulated time-units per real second** (e.g. `N = 10`
means 10 ticks of simulation clock per second of real time).

On each `Time.every` tick the update loop advances the engine by *as many
events as fall within the next `1/fps` slice of simulated time*. With 60 fps
and `N = 10` that is a slice of `10/60 ≈ 0.17` simulated time-units — often
zero or one event, which keeps animation smooth.

```elm
-- milliseconds between animation frames
framePeriodMs : Float
framePeriodMs = 1000 / 60   -- 60 fps

-- simulated time-units that elapse in one frame at speed N
simTimePerFrame : Float -> Float
simTimePerFrame n = n / 60

-- in the update loop:
TickFrame now ->
    case model.playback of
        Playing n ->
            let
                deadline = model.simState.clock + simTimePerFrame n
                newState = advanceUntil deadline model.simState
            in
            ( { model | simState = newState }, Cmd.none )
        _ ->
            ( model, Cmd.none )
```

`advanceUntil deadline state` calls `processNextEvent` in a tail-recursive
loop, stopping when the next event's time would exceed `deadline` or the event
queue is empty.

#### Speed slider in the UI

A range input maps to `N`. Suggested defaults:

| Slider label | N value |
|---|---|
| ×1 (demo) | 1 |
| ×10 | 10 |
| ×100 | 100 |
| Max (instant) | `Infinity` — drains the whole event queue in one frame |

At `Infinity` the simulation finishes in a single update cycle; the UI then
shows the final state and full event log. This doubles as the "run to
completion" button without needing a separate code path.

---

## Data Model (Proposed)

```elm
-- Identifiers
type JobID   = JobID   Int
type NodeID  = NodeID  Int
type QueueID = QueueID Int

-- Time
type EventTime = EventTime Int   -- keep as-is, works well

-- Job priority
type Priority
    = Low
    | Normal
    | High
    | Critical    -- e.g. warship, ambulance, emergency packet

-- Jobs
type alias Job =
    { id        : JobID
    , priority  : Priority
    , arrivedAt : EventTime
    , history   : List (EventTime, EventType)   -- for cycle-time tracking
    }

-- Queue ordering discipline
type QueueDiscipline
    = FIFO                  -- arrival order, ignores priority
    | PriorityFIFO          -- sorted by priority; ties broken by arrival order
    | LIFO                  -- stack (useful for some buffer models)

-- Queue overflow behaviour (applied after discipline ordering)
type QueueBehaviour = Block | DropFirst | DropLast | DropLowestPriority

type alias QueueState =
    { capacity   : Int
    , discipline : QueueDiscipline
    , behaviour  : QueueBehaviour
    , jobs       : List Job          -- always kept sorted by discipline
    }

-- Node kinds and their state
type NodeState
    = SourceNode  SourceState
    | WorkerNode  WorkerState
    | Dispatcher  DispatcherState
    | SinkNode    SinkState
    | BossNode    BossState

type alias WorkerState =
    { activity     : WorkerActivity
    , serviceParam : Float          -- mean of Poisson distribution (in time units)
    , signoff      : Maybe LockID   -- lock to acquire before releasing output
    , preemptive   : Bool
    }

type WorkerActivity
    = Idle
    | Busy JobID EventTime          -- job being processed, completion time
    | WaitingForSignoff JobID LockID -- service done; awaiting approver
    | WaitingForOutput JobID        -- signoff done but output queue(s) full
    | Preempted JobID EventTime     -- higher-priority job took over; remaining time saved
    | Paused (Maybe WorkerActivity) -- halted by boss; resumes previous activity

-- Topology (static)
type alias Topology =
    { nodeOutputs  : Dict NodeID  (List QueueID)
    , queueOutputs : Dict QueueID (List NodeID)
    }

-- Playback control (separate from the engine)
type PlaybackMode
    = Paused
    | Stepping                    -- advance one event per user click
    | Playing Float               -- N simulated time-units shown per real second

-- Theme (swappable skin, no engine logic) — mirrors Theme2D above
type alias Theme =
    { defs       : Svg Msg
    , background : Int -> Int -> Svg Msg
    , nodeView   : NodeID -> NodeState -> NodeKind -> Svg Msg
    , queueView  : QueueID -> QueueState -> Svg Msg
    , jobView    : Job -> JobVisual -> Svg Msg
    , vocab      : Vocabulary
    }

type alias Vocabulary =
    { job    : String   -- "customer", "ship", "packet", "story"
    , worker : String   -- "cashier", "dock worker", "core", "developer"
    , queue  : String   -- "queue", "berth", "buffer", "backlog"
    , boss   : String   -- "manager", "bosmang", "scheduler"
    }

-- Full model
type alias Model =
    { scenario : ScenarioConfig     -- decoded from JSON; drives topology + initial state
    , topology : Topology
    , simState : SimState
    , playback : PlaybackMode
    , theme    : Theme
    }
```

---

## Scenario Configuration (JSON)

Each scenario is a single JSON file. Loading one fully defines the network
topology, the initial simulation parameters, the layout positions for the
visual editor, and which theme to apply. No Elm recompilation is needed to
add or change a scenario.

```jsonc
{
  "meta": {
    "id":    "belter-shipyard",
    "title": "Belter Shipyard — Tycho Station",
    "theme": "expanse",
    "seed":  42,
    "speed": 10          // simulated time-units per real second (default)
  },
  "nodes": [
    { "id": 1, "kind": "source",     "label": "Docking collar",    "x": 80,  "y": 200, "arrivalRate": 0.4, "priority": "normal" },
    { "id": 2, "kind": "worker",     "label": "Hull welder",        "x": 280, "y": 120, "serviceRate": 2.5, "preemptive": true },
    { "id": 3, "kind": "worker",     "label": "Systems tech",       "x": 280, "y": 280, "serviceRate": 3.0, "preemptive": false },
    { "id": 4, "kind": "dispatcher", "label": "Shift coordinator",  "x": 480, "y": 200, "rule": "shortest-queue", "dispatchTime": 0.5 },
    { "id": 5, "kind": "sink",       "label": "Airlock out",        "x": 680, "y": 200 },
    { "id": 6, "kind": "boss",       "label": "Bosmang",            "x": 200, "y": 400 }
  ],
  "queues": [
    { "id": 1, "label": "Berth queue",    "x": 180, "y": 200, "capacity": 4, "discipline": "priority-fifo",  "overflow": "drop-lowest-priority" },
    { "id": 2, "label": "Welding bay",    "x": 380, "y": 120, "capacity": 2, "discipline": "fifo",           "overflow": "block" },
    { "id": 3, "label": "Systems bay",    "x": 380, "y": 280, "capacity": 2, "discipline": "fifo",           "overflow": "block" },
    { "id": 4, "label": "Airlock queue",  "x": 580, "y": 200, "capacity": 3, "discipline": "priority-fifo",  "overflow": "block" }
  ],
  "edges": [
    { "from": "node:1",  "to": "queue:1" },
    { "from": "queue:1", "to": "node:2"  },
    { "from": "queue:1", "to": "node:3"  },
    { "from": "node:2",  "to": "queue:2" },
    { "from": "node:3",  "to": "queue:3" },
    { "from": "queue:2", "to": "node:4"  },
    { "from": "queue:3", "to": "node:4"  },
    { "from": "node:4",  "to": "queue:4" },
    { "from": "queue:4", "to": "node:5"  }
  ],
  "scheduledEvents": [
    { "at": 50,  "kind": "boss-meeting", "nodeId": 6, "duration": 15, "label": "Bosmang all-hands" },
    { "at": 120, "kind": "boss-meeting", "nodeId": 6, "duration": 10, "label": "Emergency drill" }
  ],
  "jobTypes": [
    { "priority": "critical", "label": "MCRN warship",      "weight": 0.05 },
    { "priority": "high",     "label": "Medivac freighter",  "weight": 0.15 },
    { "priority": "normal",   "label": "Civilian hauler",    "weight": 0.60 },
    { "priority": "low",      "label": "Derelict for parts", "weight": 0.20 }
  ]
}
```

### Decoding pipeline

```
JSON string
  → Json.Decode  →  ScenarioConfig   (Elm record, validated)
  → buildTopology  →  Topology
  → initSimState   →  SimState
  → resolveTheme   →  Theme           (looked up by meta.theme string)
  → Model
```

`ScenarioConfig` is the single source of truth. The topology and visual layout
are both derived from it, so they can never drift out of sync. If the JSON
fails to decode, a friendly error is shown instead of the simulation.

Scenarios can be:
- **Bundled** — shipped as Elm `flags` from `index.html` (a JSON literal in
  the page or fetched before Elm starts)
- **Loaded at runtime** — via `Http.get` from a `/scenarios/` folder, enabling
  a scenario picker without recompiling

---

## Theming (2D and 3D)

A theme is a pure mapping from simulation entities to visual representations.
The engine never knows a theme exists.

### 2D (SVG) — current target

Each theme is an Elm record of rendering functions:

```elm
type alias Theme2D =
    { defs       : Svg msg                               -- SVG <defs>: symbols, patterns, filters
    , background : Int -> Int -> Svg msg                 -- canvas width, height
    , nodeView   : NodeID -> NodeState -> NodeKind -> Svg msg
    , queueView  : QueueID -> QueueState -> Svg msg
    , jobView    : Job -> JobVisual -> Svg msg            -- themed shape at a given position/state
    , vocab      : Vocabulary
    }

type alias JobVisual =
    { x      : Float
    , y      : Float
    , scale  : Float          -- 1.0 in transit; smaller when packed in a queue
    , state  : JobVisualState
    }

type JobVisualState
    = InQueue    Int          -- slot index (0 = front); drives spacing along queue lane
    | InTransit  Float        -- 0.0 just left source → 1.0 arrived; drives animation
    | AtWorker                -- being processed; theme may add a progress ring etc.
    | AwaitingSignoff         -- worker blocked, bosmang en route
```

#### Job shapes per theme

Jobs are not dots — they are small themed silhouettes defined once in `defs`
as SVG `<symbol>` elements and stamped with `<use>` wherever a job appears.
This keeps path data out of the render loop and lets the browser cache the shapes.

| Theme | Job shape | Variant per priority |
|---|---|---|
| `expanse` | Side-profile spaceship | Angular military hull (Critical) · white-cross medivac (High) · boxy hauler (Normal) · broken derelict (Low) |
| `supermarket` | Top-down shopping trolley | Overflowing cart (High) · standard cart (Normal) · basket only (Low) |
| `software-team` | Ticket / index card | Red border (Critical) · yellow (High) · white (Normal) · grey (Low) |
| `traffic` | Top-down car | Ambulance/police (Critical) · taxi (High) · sedan (Normal) · old banger (Low) |
| `cpu` | Labelled packet square | Colour-coded by priority |

Each `jobView` call receives the `Job` (which carries its priority and type
label) and a `JobVisual` describing where and how large to render it. The
theme decides everything else — rotation, colour, glow, badge.

#### Animated transit

When a job moves between a queue and a node, the render layer creates a short
animation. The simulation engine just fires an event; the view layer catches it
and starts a tween:

```elm
-- Purely visual — lives in Model, not SimState
type alias AnimationState =
    { transitions : Dict JobID Transition }

type alias Transition =
    { from     : ( Float, Float )
    , to       : ( Float, Float )
    , progress : Float              -- 0.0 → 1.0, advanced each frame
    , easing   : Float -> Float     -- e.g. easeInOut
    }
```

On each `TickFrame` the progress values are advanced by `dt * speed`. When
`progress` reaches 1.0 the transition is removed and the job snaps to its
final position. At high playback speeds transitions are skipped entirely (jobs
teleport) to avoid visual chaos.

Priority also affects motion: a `Critical` warship arrives faster along the
edge (shorter transit animation), reinforcing urgency in the demo.

#### In the Expanse theme

```
  ╔══════════════╗          ╔══════════════╗
  ║  Docking     ║  >>>🚀  ║  Hull        ║
  ║  collar      ║──🛸────►║  welder      ║
  ║  [Source]    ║  >>>💥  ║  [Worker]    ║
  ╚══════════════╝          ╚══════════════╝
         queue: [🚀🛸💥🛸]  (berth queue, 4 slots)
         🚀 = MCRN warship (Critical)
         🛸 = civilian hauler (Normal)
         💥 = derelict (Low)
```

Workers show a welding-spark animation while `Busy`; a blinking amber light
while `AwaitingSignoff`; go dark while `Paused` (bosmang meeting).

### 3D (WebGL) — later phase

WebGL themes use the same `ScenarioConfig` and `SimState`. The render loop
replaces the SVG `view` function with a `WebGL.toHtml` call. The engine is
identical.

A 3D `expanse` theme might show:
- The station interior as a 3-D mesh
- Ships flying in along a docking corridor with engine trails
- Priority shown by hull markings and engine glow colour
- The bosmang interruption as a station-wide red-alert flash
- Shopping carts rolling between checkout lanes in the supermarket theme

Elm has `elm-explorations/webgl` in its ecosystem. The integration point is
just replacing the `view` function; everything else stays.

---

## elm-presentation Integration

`elm-presentation` is a separate Elm application for slide-based presentations.
elm-des scenarios should be embeddable as **live, running slides** — the
simulation keeps ticking while the presenter focuses on that slide.

### Integration strategy: elm-des as a library

elm-des exposes a clean component interface per scenario:

```elm
-- Des.Scenario public API
init         : ScenarioConfig -> Seed -> (Model, Cmd Msg)
update       : Msg -> Model -> (Model, Cmd Msg)
view         : Model -> Html Msg
subscriptions: Model -> Sub Msg
```

elm-presentation imports `Des.Scenario` and holds one `Model` per scenario
slide in its own model:

```elm
-- inside elm-presentation
type alias PresentationModel =
    { slides     : Array Slide
    , current    : Int
    , scenarios  : Dict SlideId Des.Scenario.Model   -- all kept alive
    }
```

When a slide is focused its scenario's `view` fills the slide area. When the
presenter moves to a different slide, the scenario's `subscriptions` can
optionally be paused (`Paused` playback mode) or left running — presenter's
choice, controlled by the slide definition.

### Scenario slide definition (in elm-presentation's JSON)

```jsonc
{
  "kind":     "des-scenario",
  "scenario": "belter-shipyard",   // matches meta.id in the scenario JSON
  "speed":    5,                   // override meta.speed for this slide
  "playback": "playing"            // "paused" | "stepping" | "playing"
}
```

### Communication between presenter and scenario

elm-presentation can send `Des.Scenario.Msg` values to control playback
(pause, step, change speed) without knowing the simulation internals. This
maps neatly onto Elm's `Html.map` / `Cmd.map` component pattern.

---

## Implementation Phases

### Testing Policy

Every module ships with a corresponding test file. Tests are the executable
specification — if it isn't tested it isn't defined.

| Module | What to test |
|---|---|
| `Queue` | enqueue/dequeue for all overflow behaviours and disciplines; boundary at capacity=1 and capacity=max |
| `Job` | priority ordering; `comparePriority` totality |
| `Engine` | single-step transitions for each event type; Source→Worker→Sink end-to-end; blocking and unblocking; sign-off flow |
| `Topology` | valid and invalid graphs (disconnected node, missing input queue) |
| `Lock` | capacity=1 and capacity=N; waiter queue ordering |

Tests use `elm-explorations/test` with `fuzz` tests for priority ordering and
queue invariants (the queue always stays ≤ capacity; discipline order is
maintained after every put).

### Phase 1 — Solid Core ✅ done (commit e90cbb3)

Goal: a working, correctly simulating engine with a plain-text / table UI.

- [x] `Id.elm` — shared ID types (JobID, NodeID, QueueID, LockID)
- [x] `Job.elm` — Job, Priority, comparePriority
- [x] `Queue.elm` — rewrite with Discipline, Overflow, EnqueueResult
- [x] `Node.elm` — NodeKind, NodeState, WorkerActivity
- [x] `Lock.elm` — Lock, LockState
- [x] `Event.elm` — full EventType union per plan
- [x] `Topology.elm` — nodeInput, nodeOutputs, queueOutputs
- [x] `SimState.elm` — SimState with seed, eventQueue, eventLog, job store
- [x] `Engine.elm` — processNextEvent, advanceUntil, drainAll; Source, Worker, Sink handlers
- [x] `Main.elm` — Browser.element, Step / Run-to-end / Reset controls, event log view
- [x] 31 tests passing: TestQueue, TestJob, TestEngine (end-to-end Source→Worker→Sink)

**What Phase 1 does not yet include** (deferred to later phases):
- Playback speed slider / `Time.every` animation loop (Phase 2+)
- Dispatcher and Boss nodes (Phase 2)
- JSON scenario loading (Phase 3)
- SVG visualisation and themed job shapes (Phase 3/4)

### Phase 2 — Dispatcher and Boss Nodes

- [ ] `Dispatcher` node: routing rule (round-robin, shortest-queue, random)
  with configurable dispatch time
- [ ] `BossNode`: fires `MeetingStarted` / `MeetingEnded` at scheduled times;
  all Workers transition to `Paused`
- [ ] Metrics updated to exclude paused time from utilisation

### Phase 3 — JSON Scenarios + 2D Visual Renderer

- [ ] `ScenarioConfig` Elm type + `Json.Decode` pipeline
- [ ] Scenario loader: bundled via flags and/or `Http.get` from `/scenarios/`
- [ ] SVG canvas driven by theme: nodes as shapes, queues as labelled connectors
- [ ] Animated job dots moving along edges during `Playing` mode
- [ ] `Theme2D` type + default "plain" theme (monochrome, generic labels)
- [ ] Real-time charts: utilisation bar, queue-length over time, cycle-time histogram
- [ ] Scenario picker UI (loads a different JSON without recompile)

### Phase 4 — Themed Scenarios + elm-presentation Bridge

Each theme provides:
- Visual skin (node shapes, colours, labels, background)
- Vocabulary (jobs called "customers", "packets", "stories", …)
- Preset topology JSON (e.g. 3-cashier supermarket, 4-stage pipeline)

| Theme | Vocabulary | Notable mechanic |
|---|---|---|
| Supermarket | Customers, cashiers, self-checkout | Rush-hour arrival bursts |
| CPU / Bus | Packets, cores, memory bus | Back-pressure / blocking |
| Street traffic | Vehicles, intersections, lanes | Traffic-light boss node |
| Software team | Stories, developers, code review | PR review as dispatcher |
| Belter shipyard *(The Expanse)* | Welding jobs, dock workers, airlocks | Bosmang all-hands interruption |

**Belter shipyard notes:**

The setting is a Belter asteroid mining station (think Tycho, Ceres, or Pallas).
Ships come in for repair and refit; dock workers process jobs through a series of
stages — hull integrity checks, welding, systems refit, and pressure testing
before the ship is cleared through the airlock back into the void.

| Simulation concept | Belter flavour |
|---|---|
| Source node | Ships arriving at the docking collar |
| Worker node | Dock worker (e.g. "Welder", "Systems tech") |
| Dispatcher node | Shift coordinator routing jobs to available workers |
| Queue | Work queue at each station; also the airlock holding area |
| Sink node | Ship cleared and departing |
| Boss node | **Bosmang** — calls an unscheduled all-hands ("*Kang da pensa, inyalowda!*"); all dock workers down tools for the duration |
| Job | A repair/refit order for one ship |

The bosmang interruption is a natural showcase for the Boss node mechanic:
utilisation drops to zero, queues back up, and cycle time spikes — exactly the
kind of effect the simulation is meant to make visible.

Theming is purely cosmetic — the engine does not change.

- [ ] `expanse` theme: hex-panel workers, airlock queues, ship-silhouette jobs
- [ ] `supermarket` theme
- [ ] `software-team` theme
- [ ] elm-presentation integration: expose `Des.Scenario` component API,
      wire up `Dict SlideId Model` in the presentation model,
      add `des-scenario` slide kind to elm-presentation JSON schema

### Phase 5 — WebGL / Advanced Visualisation (future)

- Animated particles for jobs
- Heat-map overlay for utilisation
- 3-D topology view with `elm-explorations/webgl`
- `expanse` 3D theme: station interior mesh, glowing ship models

---

## What to Keep from the Existing Code

| Component | Decision |
|---|---|
| `EventTime` | Keep as-is — clean and simple |
| `Queue` with overflow behaviours | Keep the logic, update the types |
| `Event` / `EventType` | Extend, rename variants for clarity |
| `Interactions.elm` | Absorb into per-node handlers; delete the file |
| `Resource.elm` | Replace with the new `NodeState` union type |
| `Types.elm` | Replace with the split `Topology` / `SimState` / `Model` |
| `Main.elm` fixed-increment loop | Replace with event-time-jump loop |
| `ResourceView.elm` | Delete (was mostly commented out anyway) |
| Test files | Adapt to new types; keep test discipline |

---

## Open Questions

1. **Seeded randomness** — Elm's `Random` module is pure and requires threading
   a `Seed` through state. For reproducible simulations the seed should be part
   of `SimState` and exposed in the UI so runs can be replayed exactly.

2. **Multiple input queues per node** — Should a Worker pull from the *first
   non-empty* queue, or merge all inputs? Probably configurable per node.

3. **Dispatcher routing rules** — Round-robin needs per-node mutable state
   (a counter). Shortest-queue needs to inspect queue states. Both are
   straightforward but should be decided before implementation.

4. **Time units** — Integer ticks work fine for now. A future improvement is
   floating-point time for more accurate Poisson sampling, especially when
   combining fast and slow nodes.

5. **Frame rate vs. speed slider interaction** — At very high N values many
   events may fall within one frame slice; the UI should still remain
   responsive. A cap of, say, 500 events per frame prevents jank while still
   feeling "fast". Above that cap, just switch to the `Infinity` / drain path.
