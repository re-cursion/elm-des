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
| **Worker** | Processes one job at a time (configurable service-time distribution) |
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
    | InterruptStarted                       -- interrupt node: all workers hold
    | InterruptEnded
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

### Service Time Distributions

Service time is a property of the **node** (its `WorkerConfig`) — the node
represents a capability (crane, cashier, dock worker) whose speed is stable.

A job's **size** (`job.size : Float`, default `1.0`) scales the median duration.
This models jobs that are intrinsically more or less work regardless of who
handles them — a warship takes longer than a freighter at every dock worker.

```
actual_duration  =  sample(node.serviceTime)  ×  job.size
```

#### Why not Poisson?

Poisson is a **count** distribution — it answers "how many arrivals in the next
N ticks?" It is discrete and always ≥ 0, so it is used for arrival processes,
not durations. The duration distribution that naturally pairs with a Poisson
arrival process is the **exponential** (inter-event times of a Poisson stream
are exponentially distributed). M/M/1 queueing theory uses exponential service
times exactly for this reason.

The limitation of exponential is that its peak is at zero — the most likely
service time is the shortest one. Real tasks have a mode well above zero with a
tail toward longer times. That calls for different distributions.

#### The `ServiceTime` type

```elm
type ServiceTime
    = Exponential Float          -- mean = 1/rate; memoryless; peak at zero
    | LogNormal Float Float      -- mu sigma; median = exp(mu); right-skewed tail; hard cutoff at 0
    | Erlang Int Float           -- k phases at rate r; mean = k/r; more bell-shaped than exponential
    | Deterministic Int          -- always exactly N ticks (machinery, conveyors)
    | Uniform Int Int            -- flat in [lo, hi] ticks
```

| Distribution | When it fits |
|---|---|
| `Exponential` | Truly random/bursty work (bosmang mood, support queue) |
| `LogNormal mu sigma` | Human tasks: peaked around a typical time, occasional long outliers. Standard fit for software estimates, manual labour, inspections. Small sigma → near-deterministic; large sigma → heavy tail |
| `Erlang k r` | Multi-phase service requiring k sub-steps each taking ~1/r ticks. Less variance than exponential; useful when "it always takes *at least* a few steps" |
| `Deterministic n` | Fixed mechanical time: crane cycle, conveyor belt, automated test suite |
| `Uniform lo hi` | Roughly fixed but with known slack, e.g. a 3–7 minute coffee break |

`LogNormal` is the default recommendation for human operators.
`Deterministic` + `Erlang` cover machinery and multi-step assembly.

#### In the Belter shipyard

| Node | Suggested distribution | Rationale |
|---|---|---|
| Hull welder | `LogNormal 2.0 0.5` | Skilled but variable — some welds are nastier |
| Systems tech | `LogNormal 1.8 0.4` | Similar; slightly faster |
| Bosmang sign-off | `LogNormal 1.2 0.7` | Unpredictable; sometimes very quick, sometimes lengthy |
| Airlock cycle | `Deterministic 3` | Fixed mechanical procedure |

Ship sizes: MCRN warship `size = 3.0`, medivac `size = 1.5`, civilian hauler
`size = 1.0`, derelict `size = 0.6`.

---

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
    , capacity    : Int          -- how many simultaneous sign-offs
    , serviceTime : ServiceTime  -- how long each approval takes (see ServiceTime type)
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
  { "id": "bosmang", "label": "Bosmang inspection", "capacity": 1, "serviceTime": { "kind": "log-normal", "mu": 1.2, "sigma": 0.7 } }
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
| `Stopped` | Nothing advances; user must click "Step" or pick a speed |
| `Stepping` | One event per click; good for debugging |
| `Playing N` | Animation loop fires 60×/s; advances `N/60` simulated units per frame |
| `Scrubbing T` | Sim already complete; display time `T` driven by a time slider |

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

#### Scrubbing mode — time travel through a completed run

Once a simulation has run to completion its full event log is preserved.
`Scrubbing` is a fourth playback mode in which a time slider controls which
moment in the simulation is displayed. The engine is not called; instead the
view derives the display state from the log up to the selected time.

```elm
type PlaybackMode
    = Stopped
    | Stepping
    | Playing Float
    | Scrubbing EventTime   -- display time driven by slider; sim already complete
```

Going backward is not possible by un-applying events, so the approach is
**periodic snapshots**: during a run (or at the moment `drainAll` completes)
the engine records a checkpoint `SimState` every `snapshotInterval` events.

```elm
type alias CompletedRun =
    { snapshots        : Array SimState   -- checkpoint every snapshotInterval events
    , snapshotInterval : Int              -- e.g. 100 events per checkpoint
    , finalState       : SimState         -- state at t_max
    }
```

Scrubbing to time `T`:
1. Find the largest snapshot with `snapshot.clock ≤ T`
2. Call `advanceUntil T topo snapshot` to replay forward to `T`
3. Render the resulting state plus all metrics derived from events up to `T`

The `Model` holds both the live state and the completed run:

```elm
type alias Model =
    { scenario     : ScenarioConfig
    , topology     : Topology
    , simState     : SimState          -- live state during Playing/Stepping
    , completedRun : Maybe CompletedRun  -- populated once simulation ends
    , playback     : PlaybackMode
    , camera       : Camera            -- isometric camera (Phase 5+)
    , theme        : Theme
    }
```

Switching from `Playing` to `Scrubbing` is triggered either by reaching the
end of the event queue or by clicking "Run to end". Switching back to `Playing`
from a scrub position resumes from that position (the scrub becomes the new
`simState`).

**Practical use**: run the simulation at Max speed, then scrub back to the
interesting moment — the bosmang interruption, a queue overflow, a preemption
— and watch it frame-by-frame at ×1. This is the primary demo workflow.

---

## Metrics & Observability

### The event log as ground truth

`SimState.eventLog` is a complete, ordered record of every state change. Every
metric the system could report is derivable from it by a single linear scan.
No separate counters or accumulators are needed during the simulation run —
metrics are a pure function of the log.

```elm
-- Metrics.elm  (new module, Phase 3)
computeMetrics : EventTime -> List Event -> SystemMetrics
```

The argument `EventTime` is the current display time (for scrubbing); only
events up to that time are included.

### Metric catalogue

| Metric | Derived from |
|---|---|
| **Worker utilisation** | `Σ (ServiceComplete.time − ServiceStarted.time)` ÷ elapsed time, per node |
| **Throughput** | count `JobArrived sinkId` per time window |
| **Cycle time** (per job) | `JobArrived sinkId` − `JobArrived sourceId` for the same `JobID` |
| **Queue length over time** | replay `JobEnqueued` / `JobDequeued` / `JobDropped` per queue |
| **Wait time in queue** | `JobDequeued.time − JobEnqueued.time` per (queue, job) pair |
| **Service time** (actual) | `ServiceComplete.time − ServiceStarted.time` per job |
| **Sign-off latency** | `SignoffComplete.time − SignoffRequested.time` per job |
| **Blocking time** | time a node spent in `WaitingForOutput` state |
| **Drop rate** | count `JobDropped` per queue ÷ total arrivals |
| **Interrupt overhead** | `Σ (InterruptEnded.time − InterruptStarted.time)` as % of elapsed |

Aggregate statistics (mean, p50, p95, max) are computed over each job-level
series. Cycle time, wait time, and service time are the most useful for demos.

```elm
type alias NodeMetrics =
    { utilisation    : Float                        -- 0.0..1.0
    , jobsProcessed  : Int
    , avgServiceTime : Float
    , busyIntervals  : List ( EventTime, EventTime )  -- for the timeline strip
    }

type alias QueueMetrics =
    { avgLength     : Float
    , maxLength     : Int
    , dropCount     : Int
    , lengthHistory : List ( EventTime, Int )         -- for sparklines
    }

type alias JobMetrics =
    { jobId       : JobID
    , cycleTime   : Int    -- ticks from source to sink
    , waitTime    : Int    -- ticks spent in queues
    , serviceTime : Int    -- ticks actually being processed
    , signoffTime : Int    -- ticks waiting for sign-off
    }

type alias SystemMetrics =
    { throughput    : Float           -- completed jobs per time unit
    , avgCycleTime  : Float
    , p50CycleTime  : Float
    , p95CycleTime  : Float
    , totalDropped  : Int
    , nodes         : Dict Int NodeMetrics
    , queues        : Dict Int QueueMetrics
    , jobs          : List JobMetrics
    }
```

### Visualisation — the scene is the dashboard

Metrics live close to the things they describe. There are three display layers:

#### Layer 1 — inline indicators (always visible)

Rendered directly on the node or queue shape in the scene:

| Element | Inline indicator |
|---|---|
| Worker node | Utilisation bar along one edge (0–100 % fill, colour shifts green → amber → red) |
| Queue | Slot fill level is the visualisation; a small number badge shows current length |
| Queue (overflow) | Drop counter badge in a warning colour; flashes on each drop |
| Edge / connector | Throughput label (jobs/time unit) or animated flow thickness |

In the isometric view these become overlays on the box faces — the utilisation
bar sits on the front face; queue fill is literal (job cubes occupying slots).

#### Layer 2 — sparklines attached to the scene

Small time-series charts floating just above or beside each element, in scene
coordinates so they rotate with the isometric view:

| Element | Sparkline |
|---|---|
| Worker node | Busy/idle timeline strip (green = busy, grey = idle, red = blocked) |
| Queue | Queue-length histogram over time |
| Lock | Sign-off latency distribution (small bar chart) |

Sparklines cover only the last `sparklineWindow` time units (configurable).
At high playback speeds they update in real time; in Scrubbing mode they show
the full history up to the scrub position.

#### Layer 3 — global metrics panel

A collapsible side panel showing aggregate system statistics:

- End-to-end cycle time: histogram of all completed jobs, with p50/p95 lines
- Throughput over time: rolling average
- System utilisation: average across all workers (single number + history chart)
- Queue drop events: timeline of drops per queue
- Meeting overhead: fraction of elapsed time workers spent paused

The panel is entirely derived from `SystemMetrics`; it has no simulation state
of its own.

### Metrics and time travel

When the user scrubs to time `T`, all three layers update to show metrics
computed from events `[0..T]`. The sparklines truncate at `T`. The global
panel reflects the state of the system at that moment. This makes it possible
to scrub to the instant before and after a bosmang meeting and see utilisation
drop to zero in real time.

---

## Ensemble / Monte Carlo Runner

A single simulation run is one stochastic sample. To get reliable estimates of
steady-state behaviour — and to quantify uncertainty — you run N independent
replicas of the same scenario and aggregate their outputs into distributions.

### What it does

`Ensemble.run { replicas : Int, duration : Int } topo initState` runs N replicas,
each for `duration` simulated ticks, each with a distinct random seed derived from
the base seed in `initState`. It then aggregates `Metrics.SystemMetrics` from each
replica into `EnsembleStats`, where every numeric outcome is a `Distribution`:

```elm
type alias Distribution =
    { mean   : Float
    , stdDev : Float
    , min    : Float
    , p05    : Float
    , p50    : Float   -- median
    , p95    : Float
    , max    : Float
    }

type alias EnsembleStats =
    { n            : Int
    , duration     : Int
    , throughput   : Distribution
    , avgCycleTime : Distribution
    , p95CycleTime : Distribution
    , utilisation  : Dict Int Distribution   -- by NodeID
    , avgQueueLen  : Dict Int Distribution   -- by QueueID
    , dropCount    : Dict Int Distribution   -- by QueueID
    }
```

### Seed generation

Each replica gets an independent seed by advancing the base seed once per replica:

```elm
generateSeeds : Int -> Random.Seed -> List Random.Seed
```

This keeps runs fully reproducible from the initial seed.

### UI integration

The main view has an "Ensemble / Monte Carlo" panel with:
- Replica count input (default 100)
- Duration input (default 1 000 ticks)
- "Run N replicas" button → `RunEnsemble` message
- Stats tables rendered as `distTable` rows (mean, stdDev, p05, p50, p95, min, max)

### Typical use

- **Capacity planning**: vary queue capacity or worker count, compare p95 cycle time
- **Sensitivity analysis**: change `arrivalRate` or `serviceTime` parameters, observe
  how distributions shift and widen
- **Bottleneck detection**: look for nodes with utilisation distributions near 1.0 — those
  are the constraints

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

-- Service time distributions (node property; see "Service Time Distributions" section)
type ServiceTime
    = Exponential Float          -- mean = 1/rate
    | LogNormal Float Float      -- mu sigma; median = exp(mu)
    | Erlang Int Float           -- k phases at rate r
    | Deterministic Int          -- always exactly N ticks
    | Uniform Int Int            -- uniform in [lo, hi] ticks

-- Jobs
type alias Job =
    { id        : JobID
    , priority  : Priority
    , size      : Float                          -- scales service duration (1.0 = baseline)
    , label     : String
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
    | Interrupt                    -- no config; events pre-scheduled or triggered from UI
    | SinkNode    SinkState

type alias WorkerState =
    { activity    : WorkerActivity
    , serviceTime : ServiceTime     -- distribution for how long this node takes per job
    , signoff     : Maybe LockID    -- lock to acquire before releasing output
    , preemptive  : Bool
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
-- See "Scrubbing mode" in the Playback Layer section for full semantics.
type PlaybackMode
    = Stopped
    | Stepping                    -- advance one event per user click
    | Playing Float               -- N simulated time-units shown per real second
    | Scrubbing EventTime         -- display time driven by slider; sim already complete

-- Completed run — populated once the event queue drains
type alias CompletedRun =
    { snapshots        : Array SimState   -- checkpoint every snapshotInterval events
    , snapshotInterval : Int
    , finalState       : SimState
    }

-- Theme (swappable skin, no engine logic) — mirrors Theme2D above
type alias Theme =
    { defs           : Svg Msg
    , background     : Int -> Int -> Svg Msg
    , nodeView       : NodeID -> NodeState -> NodeMetrics -> Svg Msg
    , queueView      : QueueID -> QueueState -> QueueMetrics -> Svg Msg
    , jobView        : Job -> JobVisual -> Svg Msg
    , sparklineView  : NodeID -> NodeMetrics -> Svg Msg      -- Layer 2: timeline strip
    , vocab          : Vocabulary
    }

type alias Vocabulary =
    { job    : String   -- "customer", "ship", "packet", "story"
    , worker : String   -- "cashier", "dock worker", "core", "developer"
    , queue  : String   -- "queue", "berth", "buffer", "backlog"
    , boss   : String   -- "manager", "bosmang", "scheduler"
    }

-- Full model
type alias Model =
    { scenario     : ScenarioConfig     -- decoded from JSON; drives topology + initial state
    , topology     : Topology
    , simState     : SimState           -- live state during Playing/Stepping
    , completedRun : Maybe CompletedRun -- populated once simulation ends
    , playback     : PlaybackMode
    , camera       : Camera             -- isometric camera (Phase 5+); ignored in 2D
    , theme        : Theme
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
    { "id": 2, "kind": "worker",     "label": "Hull welder",        "x": 280, "y": 120, "serviceTime": { "kind": "log-normal", "mu": 2.0, "sigma": 0.5 }, "preemptive": true },
    { "id": 3, "kind": "worker",     "label": "Systems tech",       "x": 280, "y": 280, "serviceTime": { "kind": "log-normal", "mu": 1.8, "sigma": 0.4 }, "preemptive": false },
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
    { "priority": "critical", "label": "MCRN warship",      "weight": 0.05, "size": 3.0 },
    { "priority": "high",     "label": "Medivac freighter",  "weight": 0.15, "size": 1.5 },
    { "priority": "normal",   "label": "Civilian hauler",    "weight": 0.60, "size": 1.0 },
    { "priority": "low",      "label": "Derelict for parts", "weight": 0.20, "size": 0.6 }
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

## Theming (2D, Isometric, WebGL)

A theme is a pure mapping from simulation entities to visual representations.
The engine never knows a theme exists. Each renderer tier (flat 2D SVG,
rotatable isometric SVG, full WebGL) uses the same `SpriteSource` and
`Background` types but a different scene graph.

---

### Sprites and backgrounds — shared types

```elm
-- Where a visual comes from; used by all renderer tiers
type SpriteSource
    = VectorSymbol String        -- SVG <symbol> id defined in defs; stamped with <use>
    | RasterImage  String        -- URL to PNG/WebP; rendered via SVG <image> or WebGL texture
    | SpriteSheet                -- clip one frame from a larger sheet
        { url : String
        , x   : Int, y : Int     -- top-left of the clip rect (pixels)
        , w   : Int, h : Int
        }

-- Background of the whole canvas / scene
type Background
    = SolidColour  String                  -- CSS colour
    | SVGPattern   String                  -- <pattern> id defined in defs (2D only)
    | ImageBackground                      -- single image scaled to fill
        { url     : String
        , opacity : Float                  -- 0.0..1.0; tint over the image
        }
    | ParallaxBackground (List ParallaxLayer)

type alias ParallaxLayer =
    { url     : String
    , depth   : Float  -- 0.0 = screen-fixed; 1.0 = moves 1:1 with camera
    , opacity : Float
    , tileX   : Bool
    , tileY   : Bool
    }
```

`SpriteSource` values are passed to the render layer, which knows whether to
emit an SVG `<use>`, an SVG `<image>`, or a WebGL textured quad. The theme
never calls a renderer API directly — it only declares *what* to show.

No special asset-loading pipeline is needed in the browser: SVG `<image>` and
CSS `background-image` accept URLs directly, and the browser handles caching.
For WebGL textures the same URLs are loaded via `elm-explorations/webgl`.

---

### 2D (SVG) — Phase 3/4

Each theme is an Elm record of rendering functions:

```elm
type alias Theme2D =
    { background    : Background
    , defs          : Svg msg              -- SVG <defs>: symbols, patterns, filters
    , jobSprite     : Job -> SpriteSource  -- sprite for this job type/priority
    , nodeSprite    : NodeID -> NodeState -> SpriteSource
    , nodeView      : NodeID -> NodeState -> NodeMetrics -> Svg msg
    , queueView     : QueueID -> QueueState -> QueueMetrics -> Svg msg
    , jobView       : Job -> JobVisual -> Svg msg
    , sparklineView : NodeID -> NodeMetrics -> Svg msg
    , vocab         : Vocabulary
    }

type alias JobVisual =
    { x      : Float
    , y      : Float
    , scale  : Float          -- 1.0 in transit; smaller when packed in a queue
    , state  : JobVisualState
    }

type JobVisualState
    = InQueue    Int          -- slot index (0 = front)
    | InTransit  Float        -- 0.0 just left source → 1.0 arrived
    | AtWorker                -- being processed; theme may add a progress ring
    | AwaitingSignoff         -- worker blocked, approver en route
```

#### Sprites in practice

Jobs are themed silhouettes. Vector shapes are defined once as `<symbol>`
elements in `defs` and stamped with `<use>` — path data stays out of the
render loop and the browser caches the shapes. Raster sprites are placed with
`<image>` elements. A `SpriteSheet` clips a specific frame from a larger PNG.

| Theme | Job sprite | Variant per priority |
|---|---|---|
| `expanse` | Side-profile spaceship | Angular military hull (Critical) · medivac cross (High) · boxy hauler (Normal) · broken derelict (Low) |
| `supermarket` | Top-down shopping trolley | Overflowing cart (High) · standard cart (Normal) · basket only (Low) |
| `software-team` | Ticket / index card | Red border (Critical) · yellow (High) · white (Normal) · grey (Low) |
| `traffic` | Top-down car | Ambulance/police (Critical) · taxi (High) · sedan (Normal) · old banger (Low) |
| `cpu` | Labelled packet square | Colour-coded by priority |

Node sprites follow the same pattern: one `SpriteSource` per `(NodeID, NodeState)`
pair. A worker node shows an idle sprite, a busy sprite (with a progress ring
overlay), a blocked sprite, and a paused/dark sprite during a boss meeting.

#### Background in 2D

The background is rendered as the bottom layer of the SVG before any nodes or
jobs are drawn:

- `SolidColour` → a `<rect>` filling the canvas
- `SVGPattern` → a `<rect>` with `fill="url(#patternId)"`
- `ImageBackground` → `<image href="..." width="100%" height="100%">` with an
  optional semi-transparent `<rect>` overlay for the tint
- `ParallaxBackground` → multiple `<image>` layers; each layer's `x`/`y`
  offset is driven by the camera pan position multiplied by its `depth` factor

For the Expanse 2D theme: a dark station-interior photograph at low opacity
behind the node/queue grid, giving depth without obscuring the simulation.

#### Animated transit

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

On each `TickFrame` progress values advance by `dt * speed`. At high playback
speeds transitions are skipped (jobs teleport) to avoid visual chaos.
`Critical` jobs get a shorter transit time — urgency is visible in motion.

#### In the Expanse 2D theme

```
  ╔══════════════╗          ╔══════════════╗
  ║  Docking     ║  >>>🚀  ║  Hull        ║
  ║  collar      ║──🛸────►║  welder      ║
  ║  [Source]    ║  >>>💥  ║  [Worker]    ║
  ╚══════════════╝          ╚══════════════╝
         queue: [🚀🛸💥🛸]  (berth queue, 4 slots)
         🚀 = MCRN warship (Critical) — SpriteSheet frame
         🛸 = civilian hauler (Normal) — SpriteSheet frame
         💥 = derelict (Low) — SpriteSheet frame
```

Background: a wide station-interior image (`ImageBackground`, opacity 0.35).
Workers show a welding-spark animation while `Busy`; a blinking amber light
while `AwaitingSignoff`; go dark while `Paused` (bosmang meeting).

### Rotatable Isometric View — Phase 5 target

The isometric renderer is a **SVG-only upgrade** — no WebGL, no extra
dependencies. All scene objects get `(x, y, z)` world coordinates. On each
frame the view applies a rotation matrix and a fixed isometric projection, then
emits SVG. The engine and `SimState` are entirely unchanged.

#### The camera

```elm
type alias Camera =
    { spinAngle : Float   -- Y-axis rotation, user-controlled (radians, 0..2π)
    , tiltAngle : Float   -- isometric tilt from horizontal (default: pi/6 ≈ 30°)
    , scale     : Float   -- pixels per world unit
    , origin    : ( Float, Float )   -- screen centre
    }

defaultCamera : Camera
defaultCamera =
    { spinAngle = pi / 4   -- classic 45° isometric start angle
    , tiltAngle = pi / 6   -- 30° tilt
    , scale     = 48
    , origin    = ( 600, 300 )
    }
```

#### The projection pipeline

Every world point goes through three steps:

```
world (x, y, z)
  ──[1. rotateY(spinAngle)]──►  camera-space (x', y', z')
  ──[2. isometric project]───►  screen (sx, sy)
  ──[3. painter's sort]──────►  far objects drawn first (occluded by near)
```

```elm
project : Camera -> { x : Float, y : Float, z : Float } -> ( Float, Float )
project cam { x, y, z } =
    let
        -- 1. rotate around Y axis
        xr =  x * cos cam.spinAngle + z * sin cam.spinAngle
        zr = -x * sin cam.spinAngle + z * cos cam.spinAngle

        -- 2. isometric project (fixed tilt)
        sx = (xr - zr) * cos cam.tiltAngle * cam.scale
        sy = (-(xr + zr) * sin cam.tiltAngle + y) * cam.scale
    in
    ( cam.origin |> Tuple.first  |> (+) sx
    , cam.origin |> Tuple.second |> (+) sy
    )
```

Depth for painter's sort is `xr + zr` after rotation — the larger this value,
the further from the viewer, so objects are drawn farthest-first.

#### Scene objects

Everything in the scene is a `SceneObject`:

```elm
type alias SceneObject msg =
    { pos     : { x : Float, y : Float, z : Float }
    , height  : Float          -- vertical extent (used to build box faces)
    , shape   : SceneShape msg
    , sortKey : Float          -- depth after rotation; computed each frame
    }

type SceneShape msg
    = Box               BoxFaces       -- 3 programmatic faces: top, left, right
    | FlatTile          { w : Float, d : Float }  -- floor quad (rhombus)
    | BillboardSprite   SpriteSource   -- always faces camera; projected to screen pos
    | DirectionalSprite                -- sprite sheet; frame chosen by spinAngle
        { source       : SpriteSource
        , directions   : Int           -- how many frames (4 or 8 is standard)
        }
    | Path3D            (List { x : Float, y : Float, z : Float })  -- edge connector
```

A `Box` renders three shaded faces. `BillboardSprite` is projected to a screen
point and rendered as a flat image always facing the viewer — good for people,
workers, small decorative objects. `DirectionalSprite` picks a frame from a
sprite sheet based on the current `spinAngle`, giving the impression the object
has volume and orientation as the scene rotates (classic isometric-game look).

**When to use each shape:**

| Scene element | Recommended shape |
|---|---|
| Nodes (worker bays, stations) | `Box` — programmatic, shading is automatic |
| Floor tiles | `FlatTile` |
| Queue slots / platforms | `Box` (low height) |
| Jobs in transit or in queue | `DirectionalSprite` (8 frames for ships/vehicles) or `BillboardSprite` (tickets, packets) |
| People / dock workers | `BillboardSprite` |
| Edge connectors | `Path3D` |

#### Sprites in the isometric view

**`DirectionalSprite` frame selection:**

The sprite sheet has `directions` equally spaced frames (typically 8, covering
360°). The visible frame index is:

```elm
directionFrame : Int -> Float -> Int
directionFrame directions spinAngle =
    let
        normalized = modBy directions (round (spinAngle / (2 * pi) * toFloat directions))
    in
    normalized
```

For an 8-direction ship sprite sheet, this picks the frame that shows the ship
broadside-on, bow-forward, stern-forward, etc. as the camera rotates. The
sprite sheet URL and clip rects live in `SpriteSource.SpriteSheet`.

**`BillboardSprite`** is simpler: the sprite is always drawn upright at its
projected screen position, scaled by `cam.scale * job.size`. It can have
animation frames driven by a per-job timer (e.g. a blinking light, an
exhaust glow).

#### Background in isometric

The background is a `ParallaxBackground` with two or three layers:

1. **Sky/space layer** (`depth = 0.0`) — a static space or environment image
   filling the canvas behind everything
2. **Distant environment** (`depth = 0.2`) — a faint station structure or
   cityscape that shifts slightly as the camera spins, giving depth
3. **Floor environment** (`depth = 1.0`) — moves fully with the camera; used
   for a tiled ground texture that lines up with the `FlatTile` floor objects

As `spinAngle` changes, each layer's `x` offset shifts by `depth * spinAngle *
parallaxScale`. This gives a convincing sense of a three-dimensional space even
though everything is still SVG.

#### Scene layout in the scenario JSON

Nodes and queues already have `x` and `y` in the flat 2D layout. The isometric
scene adds `z` and `h` (height):

```jsonc
"nodes": [
  { "id": 2, "label": "Hull welder", "x": 4, "y": 0, "z": 2, "h": 1.5, ... }
],
"queues": [
  { "id": 1, "label": "Berth queue", "x": 2, "y": 0, "z": 2, "h": 0.3, ... }
]
```

`y` is the vertical axis (height above floor). Nodes and queues sit on the
floor (`y = 0`). Jobs animate upward slightly while being processed.
The `h` field gives the visual box height — taller for important/large nodes.

2D and isometric renderers share the same `x`/`z` layout coordinates.
The 2D renderer simply ignores `z` and `h`.

#### Job animation in isometric

Jobs are small cubes that move along edges. Their world position is
interpolated between source and destination during transit. In queue slots they
are stacked or arranged in a row along the queue's orientation axis. While being
processed at a worker they hover slightly above the node surface (`y += 0.2`
with a gentle bob animation).

Priority is expressed visually: `Critical` jobs are larger (`size * 1.2`),
`Low` jobs are smaller. The theme supplies the top-face colour or sprite.

#### User controls

| Gesture | Effect |
|---|---|
| Drag horizontally | Adjust `spinAngle` (rotate around Y axis) |
| Drag vertically | Adjust `tiltAngle` (zoom from top-down to oblique) |
| Scroll / pinch | Adjust `scale` |
| Double-click | Reset camera to `defaultCamera` |

Controls are wired via `Browser.Events.onMouseMove` / `onMouseDown` with a
`Dragging Bool` flag in the UI model. The camera lives in `Model`, not
`SimState`, so resetting the simulation never moves the camera.

#### In the Expanse isometric theme

Background: three parallax layers — static deep-space starfield (depth 0.0),
faint asteroid/station silhouettes (depth 0.2), hex-panel floor texture
matching the `FlatTile` grid (depth 1.0).

Floor: `FlatTile` shapes with a hex-panel PNG texture, giving the station
deck a gritty industrial look.

Nodes: `Box` shapes with corrugated-metal shading on side faces; an animated
welding-spark overlay (SVG filter) while `Busy`; amber beacon while
`AwaitingSignoff`; dark and silent while `Paused`.

Ships (jobs): `DirectionalSprite` with an 8-direction sprite sheet. Four ship
types (one per priority), each with 8 angle frames = 32 sprites total per
sheet. Ships glow faintly while moving, grow dim at low priority, blaze with
running lights at `Critical`.

Workers / bosmang: `BillboardSprite` with idle / working / gesturing frames.

The bosmang's office is a taller `Box` in the corner of the scene; a red
rotating beacon activates during a meeting.

#### Relationship to the full WebGL upgrade (Phase 6)

The isometric renderer and the WebGL renderer share the same:
- Scene layout data (`x, y, z, h` per node/queue in the JSON)
- `Camera` record (spin, tilt, scale, origin)

Migrating a theme from isometric SVG to WebGL means replacing `SceneShape`
with 3D mesh objects and the SVG projection with a proper MVP matrix.
The engine, SimState, and scenario JSON all stay identical.

---

### WebGL 3D — Phase 6

In Phase 6 jobs, nodes, and queues are **real 3D objects** with geometry,
materials, and lighting — not sprites. The SVG scene graph is replaced with
a `WebGL.toHtml` call; everything else (engine, SimState, Camera, scenario
JSON) stays identical.

```elm
-- A 3D mesh asset
type alias Mesh3D =
    { vertices  : List Vec3
    , normals   : List Vec3
    , uvCoords  : List Vec2
    , faces     : List ( Int, Int, Int )   -- triangle indices
    , material  : Material3D
    }

type alias Material3D =
    { albedo    : String        -- URL to diffuse texture (or CSS colour for solid)
    , normal    : Maybe String  -- normal map URL
    , roughness : Float         -- 0.0 = mirror, 1.0 = fully diffuse
    , metalness : Float         -- 0.0 = plastic, 1.0 = metal
    }

-- Theme3D replaces SceneShape with mesh references
type alias Theme3D =
    { background  : Background       -- becomes a skybox or environment map
    , jobMesh     : Job -> Mesh3D
    , nodeMesh    : NodeID -> NodeState -> Mesh3D
    , queueMesh   : QueueID -> QueueState -> Mesh3D
    , lighting    : List Light3D
    , vocab       : Vocabulary
    }

type Light3D
    = DirectionalLight { direction : Vec3, colour : Vec3, intensity : Float }
    | AmbientLight     { colour : Vec3, intensity : Float }
    | PointLight       { position : Vec3, colour : Vec3, range : Float }
```

`Background` in 3D becomes a **skybox** (six-face cubemap) or an **HDRI
environment map** that also drives image-based lighting on surfaces.

#### In the Expanse 3D theme

- Ships are low-poly OBJ meshes with diffuse + normal maps; running lights are
  `PointLight` objects parented to each ship, moving with it
- Station floor: tiled geometry with a worn-metal PBR material
- Workers: rigged character meshes with idle/working/gesturing animations
  (skeletal animation driven by `WorkerActivity` state)
- The bosmang meeting: a station-wide red-alert point light pulses across all
  surfaces; workers' meshes switch to a "standing" idle pose
- Particle systems for welding sparks and thruster exhaust (modelled as
  `PointLight` + billboard quads, using `elm-explorations/webgl` instanced rendering)

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
| `Metrics` | utilisation of a known busy/idle sequence; cycle time from a hand-crafted event log; queue length history; scrub to mid-run matches hand-computed state |
| `ServiceTime` | each variant produces durations in expected range; `size` scaling; LogNormal samples are always positive |
| `Isometric` | `project` round-trips at known angles; `directionFrame` selects correct frame for 4- and 8-direction sheets; painter sort produces correct depth order for a known set of objects |

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
- Dispatcher and Interrupt nodes (Phase 3)
- JSON scenario loading (Phase 3)
- Flat 2D SVG visualisation and themed job shapes (Phase 3/4)
- Rotatable isometric view (Phase 5)
- Full WebGL renderer (Phase 6)

### Phase 2 — Service Time Distributions + Metrics + Ensemble Runner ✅ done

- [x] `ServiceTime.elm` — `Exponential | LogNormal | Erlang | Deterministic | Uniform`
- [x] `size : Float` field on `Job` (default `1.0`); scales actual service duration
- [x] Engine `startService` samples from `ServiceTime` and multiplies by `job.size`
- [x] Box-Muller transform helper for `LogNormal` sampling
- [x] `drainAll` bounded to 100 000 events (Source nodes create infinite event chains)
- [x] Animated playback via `Browser.Events.onAnimationFrame`; speed buttons ×1…Max
- [x] `Metrics.elm` — single-pass event-log scan → `SystemMetrics` (throughput, cycle times,
      utilisation, queue avg length, drop count)
- [x] `Ensemble.elm` — `run N replicas` → `EnsembleStats` with `Distribution` per metric
      (mean, stdDev, p05, p50, p95, min, max)
- [x] Ensemble UI panel in `Main.elm`: replica count + duration inputs, results tables
- [x] 65 tests passing: TestQueue, TestJob, TestEngine, TestServiceTime, TestMetrics, TestEnsemble

### Phase 3 — 2D Visual Renderer + Node Types + Topology Presets ✅ largely done

**Engine additions:**
- [x] `Dispatcher` node — `ShortestQueue` routing (routes to least-loaded output queue);
      `RoundRobin` implemented and selectable via UI; `RandomChoice` stub present
- [x] `Interrupt` node — fires `InterruptStarted` / `InterruptEnded`; per-worker `halted : Bool`
      in `WorkerConfig` replaces the former global `SimState.interruptActive` flag; `onInterruptStarted`
      fans out to all Workers and logs `WorkerHalted NodeID`; `onInterruptResumed` clears halted and
      wakes idle workers, logging `WorkerResumed NodeID`
- [x] `SimState.isInterruptActive` — derived helper (any Worker cfg.halted == True); replaces
      direct field access
- [x] `Engine.wakeConsumers` guards Workers against starting new jobs while `cfg.halted`
- [x] Preemptive scheduling — `ServicePreempted NodeID JobID` event; `tryPreempt` in engine;
      stale `ServiceComplete` guard; all presets use `preemptive = True` on workers
- [x] `Signoff / Lock` exercised in `TwoParallel` — Worker B requires inspector sign-off
      (capacity=1 lock, `Deterministic 5` ticks); purple `Signoff` state visible on canvas

**2D visual renderer:**
- [x] SVG canvas with three hardcoded preset layouts (not yet driven by JSON/Theme2D)
- [x] Animated job transit dots — `JobAnim` tracks current and target canvas position;
      dots move at 400 SVG-units/sec in real time, cleared at Max speed
- [x] Layer 1 inline indicators: utilisation bar on Worker nodes (per-node, not hardcoded),
      fill-level bar + slot dots on queues, drop-count badge, priority-coloured job dots,
      inspector lock badge below Worker B showing `free` / `busy` / `+N wait` state
- [x] Layer 2 sparklines: busy/idle strip per Worker, step-function queue-length chart
- [x] Layer 3 histogram: cycle-time distribution with p50 (green) and p95 (orange) markers
- [x] `Metrics.computeTimelines` — single-pass to produce `BusySegment` / `QueueStep` lists
- [x] `CompletedRun` snapshots — checkpoint every 100 events during `buildCheckpoints`
- [x] Time-travel scrubber — range slider; replays from nearest checkpoint via `Engine.advanceUntil`

**Scenario system:**
- [x] `TopologyPreset` — `SingleWorker` (M/M/1) | `TwoParallel` (M/M/2) | `ThreePipeline`;
      dropdown selector in the scenario panel
- [x] Scenario parameter panel — arrival rate, service-time distribution + params, queue
      capacity, % high-priority, dispatch rule (ShortestQueue / RoundRobin for TwoParallel)
- [x] `Interrupt` events pre-scheduled at t=150–170 and t=350–370; interactive "Interrupt"
      button in playback controls for manual one-shot interrupts
- [x] Ensemble runner uses the currently selected preset and parameters
- [x] 95 tests: TestQueue, TestJob, TestEngine (Dispatcher + Interrupt + preemption + RandomChoice),
      TestServiceTime, TestMetrics (interrupt-aware utilisation, JobMetrics), TestEnsemble,
      TestScenarioConfig (decode + toTopologyAndState)

**Still pending from Phase 3:**
- [x] `ScenarioConfig` Elm type + `Json.Decode` pipeline — `ScenarioConfig.elm` decodes the
      full JSON schema (meta, nodes, queues, edges, locks, scheduledEvents) and
      `toTopologyAndState` builds `(Topology, SimState)` from a decoded config;
      10 tests in `TestScenarioConfig.elm`
- [x] `Theme2D` type — `Theme.elm` defines `SpriteSource`, `Background`, `ParallaxLayer`,
      `JobVisual`, `JobVisualState`, `Vocabulary`, and `Theme2D msg`; canvas still
      hard-codes shapes/colours (a full default theme is Phase 4)
- [x] `Job.history : List (EventTime, EventType)` — stamped at 6 transitions (source arrival,
      ServiceStarted, ServiceComplete, SignoffRequested, SignoffComplete, sink arrival); store copy
      used in `tryPullFromQueue` so history survives queue residence; completed jobs move to
      `SimState.completedJobs` (replacing `removeJob` at Sink)
- [x] `JobMetrics` — cycleTime, serviceTime, signoffTime, waitTime computed from each completed
      job's history via `computeWindowSum`; exposed in `SystemMetrics.jobs`
- [x] Metrics: `haltedTicks` per worker tracked from `WorkerHalted`/`WorkerResumed` events;
      utilisation denominator is `totalTicks − haltedTicks[nid]` so halted windows don't deflate utilisation
- [x] `RandomChoice` routing in `Dispatcher` — seeds `Random.int 0 (n-1)`, threads updated seed
      back through `state.seed` before `tryQueues`

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

- [ ] `SpriteSource` and `Background` types in `Theme.elm`
- [ ] `expanse` theme (flat 2D): ship sprite sheet (4 priorities × frames), station-interior background image, hex-panel SVG pattern for queue slots
- [ ] `supermarket` theme (flat 2D): trolley sprite sheet, supermarket background
- [ ] `software-team` theme (flat 2D): ticket sprites, office background
- [ ] elm-presentation integration: expose `Des.Scenario` component API,
      wire up `Dict SlideId Model` in the presentation model,
      add `des-scenario` slide kind to elm-presentation JSON schema
- [ ] Add `z` and `h` layout fields to all scenario JSONs, ready for Phase 5 isometric renderer

### Phase 5 — Rotatable Isometric Renderer

- [ ] `Camera` type and `project` function (Y-axis rotation + isometric projection)
- [ ] `SceneObject` / `SceneShape` types (`Box`, `FlatTile`, `BillboardSprite`, `DirectionalSprite`, `Path3D`)
- [ ] Painter's-algorithm depth sort (`sortKey` computed after rotation each frame)
- [ ] `Box` face rendering: top + left + right faces with shading relative to `spinAngle`
- [ ] `FlatTile` floor grid with tiled texture (PNG `SpriteSource`)
- [ ] `BillboardSprite`: always-upright image at projected position; scaled by `cam.scale`
- [ ] `DirectionalSprite`: frame selection by `spinAngle`; `directionFrame` helper
- [ ] `ParallaxBackground` for isometric scenes: layer offsets driven by `spinAngle * depth`
- [ ] Camera drag controls: horizontal drag → `spinAngle`, vertical drag → `tiltAngle`, scroll → `scale`
- [ ] `z` and `h` fields added to node/queue JSON layout; 2D renderer ignores them
- [ ] Jobs animate along `Path3D` edges; hover + bob while `Busy`; teleport at high speed
- [ ] Queue slots arranged along the queue's orientation axis in 3D
- [ ] `expanse` isometric theme: hex-panel floor tiles, industrial `Box` nodes, 8-direction ship sprite sheet (32 frames), parallax starfield + station background, worker billboard sprites
- [ ] Camera reset on double-click; camera state in `Model`, never in `SimState`

### Phase 6 — WebGL 3D (future)

Jobs, nodes, and queues become **real 3D objects with geometry and materials**,
not sprites. The SVG scene graph is replaced with `WebGL.toHtml`
(`elm-explorations/webgl`). The engine, SimState, Camera, scenario JSON, and
elm-presentation bridge are all unchanged.

- [ ] `Mesh3D` and `Material3D` types; OBJ/glTF loader or hand-authored meshes
- [ ] `Theme3D` record: `jobMesh`, `nodeMesh`, `queueMesh`, `lighting`
- [ ] MVP matrix replacing the SVG `project` call
- [ ] PBR lighting: directional + ambient + point lights; `roughness` / `metalness`
- [ ] `Background` becomes a skybox (six-face cubemap) or HDRI environment map
- [ ] `expanse` 3D theme: low-poly ship meshes with diffuse + normal maps, running-light point lights
- [ ] Station floor: tiled geometry with worn-metal PBR material
- [ ] Worker character meshes: idle / working / gesturing poses driven by `WorkerActivity`
- [ ] Particle system: welding sparks, thruster exhaust (instanced billboard quads)
- [ ] Red-alert point light pulsing across all surfaces during boss meeting
- [ ] Heat-map texture on floor (utilisation baked per update into a `WebGL.Texture`)
- [ ] Free-orbit camera (full pitch + yaw, replacing Y-axis-only spin)

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

3. **Dispatcher routing rules** — `ShortestQueue` is implemented. `RoundRobin`
   needs per-node mutable state (a counter in `DispatcherConfig.roundRobinIndex`,
   already present but not updated). `RandomChoice` needs the seed threaded into
   `startService`.

4. **Time units** — Integer ticks work fine for now. A future improvement is
   floating-point time for more accurate Poisson sampling, especially when
   combining fast and slow nodes.

5. **Frame rate vs. speed slider interaction** — At very high N values many
   events may fall within one frame slice; the UI should still remain
   responsive. A cap of, say, 500 events per frame prevents jank while still
   feeling "fast". Above that cap, just switch to the `Infinity` / drain path.
