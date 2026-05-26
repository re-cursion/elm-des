# elm-des

A discrete event simulation (DES) engine for the browser, written in [Elm](https://elm-lang.org).

Build networks of queues and worker nodes, watch jobs flow through them in real time, and observe metrics like utilisation and cycle time — all without leaving the browser.

## What it models

| Concept | Description |
|---|---|
| **Job** | A unit of work flowing through the system. Carries a priority (Low / Normal / High / Critical) and a timestamp trail for cycle-time metrics. |
| **Queue** | A bounded FIFO buffer between nodes. Configurable overflow behaviour: `Block`, `DropFirst`, `DropLast`, or `DropLowestPriority`. Optional `PriorityFIFO` discipline so higher-priority jobs are served first. |
| **Node** | Does work. Reads from exactly **one** input queue, writes to **one or more** output queues. Current kinds: Source (generates jobs), Worker (processes one job at a time), Sink (absorbs finished jobs). |
| **Engine** | Pure event-time-jump loop: the clock leaps to the next event rather than ticking by fixed increments. Fully deterministic given a seed. |
| **Lock** | A named, finite-capacity sign-off resource. A Worker can require approval from a Lock before releasing a finished job — modelling quality inspection, manager sign-off, etc. |
| **Topology** | Bipartite graph: nodes only connect to queues, queues only connect to nodes. Stored separately from runtime state so topology and simulation state can never drift out of sync. |

## Project status

**Phase 1 complete.** The simulation engine, all core types, and a minimal step-through UI are working.

See [PLAN.md](PLAN.md) for the full roadmap. Coming next: Dispatcher and Boss nodes, playback speed control, JSON-driven scenarios, SVG visualisation, and themed scenarios (belter shipyard, supermarket, software team, …).

## Running locally

```sh
elm make src/Main.elm --output=index.js
# open index.html in a browser
```

Or with live reload:

```sh
npm install
npm start          # starts elm-watch or similar dev server
```

## Running tests

```sh
npx elm-test
```

31 tests across `TestQueue`, `TestJob`, and `TestEngine`. Run with `--watch` for continuous feedback during development.

## Module overview

```
src/
  Id.elm          — opaque ID types (JobID, NodeID, QueueID, LockID)
  Job.elm         — Job record, Priority type, comparePriority
  Queue.elm       — bounded queue with discipline and overflow behaviour
  Node.elm        — NodeKind union, NodeState machine, node constructors
  Lock.elm        — sign-off resource: capacity, waiter queue, acquire/release
  Event.elm       — EventType union, Event record
  EventTime.elm   — integer simulation clock
  Topology.elm    — bipartite adjacency (nodeInput, nodeOutputs, queueConsumers)
  SimState.elm    — full mutable simulation state (nodes, queues, locks, jobs, seed, event queues)
  Engine.elm      — processNextEvent, advanceUntil, drainAll
  Main.elm        — Browser.element wiring, step-through UI

tests/
  TestQueue.elm   — enqueue/dequeue for all behaviours and disciplines, fuzz invariants
  TestJob.elm     — priority ordering, comparePriority totality
  TestEngine.elm  — end-to-end Source → Worker → Sink flow
```

## Architecture in one paragraph

The simulation is a **bipartite queuing network**: jobs flow `Node → Queue → Node → Queue → …`. Nodes have a single input queue and may have multiple output queues. The engine processes one event at a time, jumping the clock forward to each event's timestamp. State changes and new event generation are co-located in each event handler, making causality easy to trace. The `Topology` (static graph) and `SimState` (dynamic runtime state) are kept separate so the graph can be inspected or edited independently of the running simulation.

## Planned themes

Each theme is a cosmetic skin — the engine never changes.

| Theme | Jobs | Nodes |
|---|---|---|
| Belter shipyard *(The Expanse)* | Ships (MCRN warship, freighter, derelict) | Dock workers, bosmang sign-off |
| Supermarket | Shopping trolleys | Cashiers, self-checkout |
| Software team | Story tickets | Developers, code review |
| CPU / Bus | Packets | Cores, memory bus |
| Street traffic | Vehicles | Intersections |
