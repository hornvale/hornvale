# The Sounding — a feasibility benchmark for the living-community engine

**Campaign:** The Sounding — the first build of the living-community program
**Frontier:** SOC-10 / UNI-30 ("the living community"), MAP-60 ("the
connection graph") — recorded on this branch in `book/src/frontier/`
**Status:** G3 draft (awaiting Nathan)

---

## 1. Summary

The living-community program (frontier §living-community) proposes to derive a
world's social history — settlements founded, raided, displaced, ruined — as a
**coarse forward-simulation baked once at genesis**, then read cheaply at
observation. The whole architecture rests on one unproven assumption: that this
is **computationally tractable** at the scale a rich world needs. The owner's
stated blocker is empirical — *"I won't believe it works until I have
benchmarks: exactly how long x takes, what it produces, with y species, z
cells, a years."*

The Sounding answers that, and only that. It is **a feasibility benchmark, not
the engine.** It builds a *representative core-loop spike* — real data
structures, real coupling topology, the real determinism constitution, but
**placeholder dynamics** over **synthetic seeded inputs** — sweeps it across
communities (Z), species (Y), history length (A epochs), and coupling density
(d̄), and reports the **feasibility frontier**: where each cost budget breaks.
The primary finding is not absolute time but **scaling** — above all whether
the inter-community coupling stays sub-quadratic, which is the one architectural
go/no-go that cannot be fixed later by optimization.

**This is the *macro-history* sounding — one of three profiles, named
honestly.** The vision's richness spans three computational profiles with
different cost shapes: the **macro history** (a city's centuries, a war's
mobilization, the ruins — scales with *world size*, the profile that can
architecturally blow up); the **fine cognition** (an individual's morning —
perception, planning, drives — *LOD-bounded*, scaling with *observation*, not
world size); and the **emergent quality** (whether the composition *reads* as
rich rather than mush — taste-checked, unbenchmarkable). The Sounding measures
the first, because it is the scariest scaling risk and the right one to retire
first. It is **necessary, not sufficient**: the fine-cognition profile needs its
own sounding (largely The Walk's territory — its pieces, The Surmise, The
Foresight, The Wanting, The Quickening, already run), and emergent quality is
cultivated and observed, never gated. A green Sounding retires the
world-scale-blowup risk; it does **not**, and must not be read to, prove the
whole vision. The companion soundings are named in the non-goals (§7).

## 2. Background

The frontier sketches the engine (frontier §living-community, §connection-graph):
a community is a persistent entity with a dated biography; a genesis-time epoch
loop reads structural pressure and resolves events (grow / found / raid→displace
/ collapse); communities couple over a sparse **connection graph** (spatial
adjacency + routes + portals); the present is the same derivable field replayed
forward from the seed. Three cost directions matter, each answering a distinct
feasibility question:

| Direction | Cost | Question | Budget (a line, not a gate) |
|---|---|---|---|
| **Genesis bake** | derive the whole baked past | "is world-creation affordable?" | generous — seconds→minutes |
| **Read** | sample a baked biography / expand a role-handle | **"does `look` stay cheap?"** | tight — sub-millisecond |
| **Present replay** | lazily derive a region forward on observation | "does the living present stay cheap?" | medium — tens of ms |

The **fidelity sweet spot** is real-structure / placeholder-dynamics: scaling
exponents and constant factors depend on the data structures, the coupling
topology, and the constitution constraints — *not* on how realistic the raids
are. So the spike can resolve trivial events (a pressure threshold flips a
coin-from-the-seed) and still yield trustworthy numbers, while being cheap to
build. Synthetic inputs (deterministic synthetic communities on a synthetic
sparse graph) decouple the measurement from real worldgen wiring — we are
measuring the loop, not the integration.

## 3. The design

### 3.1 The spike

**The input surface is deliberately tiny — six config fields; everything else
is derived.** A run is fully specified by:

```rust
struct SoundingConfig {
    seed: Seed,                 // determinism source (kernel newtype)
    communities: u32,           // Z  — synthetic communities to seed
    species: u32,               // Y  — synthetic species
    epochs: u32,                // A  — history length, in epochs
    avg_degree: f64,            // d̄  — mean connection-graph degree (edges/node)
    long_range_fraction: f64,   // portal/route edges as a fraction of all edges
}
```

The five numeric dials are the sweep axes (§3.3); the placeholder-dynamics
constants (a pressure threshold, a base growth rate) are fixed *internal
constants, not inputs*. From `(seed, config)`, deterministically, the loop
derives **four kinds of synthetic state**: a **species table**
(`Vec<{ carrying_need: f64, frequency_weight: f64 }>`, `Y` entries), a
**community table** (`Vec<{ species: u32, population: f64, node: NodeId,
biography: Vec<BioEntry> }>`, `Z` entries), a **sparse connection graph**
(`BTreeMap<NodeId, Vec<{ to: NodeId, lag: u32, kind ∈ {Adjacent, Route,
Portal} }>>`), and a per-community **capacity field**. Per epoch the dynamics
read a **four-value set** — `population`, the species' `carrying_need`, the
node's `capacity`, and the graph neighbors — which is deliberately the *same
read-shape the real engine will have* (pressure `= f(population, capacity)`,
event `= f(pressure, seed)`, coupling over `neighbors`), so the cost drivers
are faithful even though the values are synthetic. `run(config) -> World` is a
pure deterministic function; the benchmark evaluates it across the sweep.

Concretely, that spike is a minimal, deterministic core loop over synthetic
inputs:

- **Communities**: `Z` synthetic communities, each an entity with a **dated
  biography** (an append-only list of `(epoch, event, role-handle)` records), a
  population, a species tag (from `Y` synthetic species with placeholder
  trait weights), and a location node in the connection graph.
- **Connection graph**: a synthetic **sparse** graph over the community nodes —
  bounded average degree `d̄` (a handful of edges per node, the realistic
  regime), plus a tunable fraction of **long-range edges** (the portal/route
  case). Deterministic from the seed. This is the structure whose coupling cost
  the sweep must characterize.
- **The epoch loop** (`A` epochs): each epoch, for each community, compute a
  **placeholder structural pressure** (population vs a synthetic carrying number)
  and resolve a **deterministic seeded event** — grow, found (spawn a community),
  raid→displace (move population along a graph edge, appending to both
  biographies), or collapse (→ a ruin record). Displacement and a diffusive
  quantity (a stand-in for trade/culture) **propagate along graph edges**, with
  edge-specific lag — this is the coupling under test.
- **Constitution**: seeded (`hornvale_kernel::Seed`/`Stream`), `BTreeMap`/`Vec`
  only, `total_cmp` for any float ordering, no wall-clock, `quantize` on any
  serialized report float. The spike obeys the same rules the real engine must,
  so it also proves constitution-compatibility.

### 3.2 The three measured directions

- **Genesis bake** — time and peak memory to run the full `A`-epoch loop over
  `Z` communities, and the total/per-community **biography size in bytes**.
- **Read** — time to (a) sample a random community's biography and (b) expand a
  role-handle into a synthetic persona; measured as a hot-loop average over many
  reads on a baked world.
- **Present replay** — time to lazily derive one region (a graph neighborhood)
  forward `ΔA` epochs from a baked "now", incorporating a synthetic intervention
  delta — the living-present cost.

### 3.3 The sweep and the frontier

Sweep each axis over a wide range while holding the others at a midpoint:
`Z ∈ {10 … 10⁵}`, `Y ∈ {handful … hundreds}`, `A ∈ {10² … 10⁴ epochs}`,
`d̄ ∈ {sparse … denser}` and long-range-edge fraction `∈ {0 … noticeable}`.
For each point, record all §3.2 metrics. Fit **scaling exponents** per axis and
report the **feasibility frontier**: the parameter surface where each cost first
crosses its budget line (§2 table; memory ~few GB). Budgets are annotations, not
pass/fail — the deliverable is the curve, so the owner reads the ceiling and
decides.

### 3.4 The report (the artifact)

Census-style: a committed markdown summary with the scaling table, the fitted
exponents, the frontier per budget, plus a CSV of the raw sweep and **sample
biographies** (a few communities' dated histories rendered as text) — "what it
produces", so the output is inspected for quality, not just speed. Lives under
`book/src/laboratory/` (drift-checked like the census).

## 4. Architecture & files

- **The skeleton crate** — the core loop of §3.1. Because inputs are synthetic,
  it can be **kernel-only-dependent**. Its placement is the one G3 flagged
  decision (§8): a new experimental crate (`domains/`-tier, kernel-only) versus a
  module under `windows/lab`. The eventual engine is cross-domain (a
  genesis-time window/worldgen thing), so placing the skeleton now pre-commits
  where the engine grows — the owner's call.
- **The sweep harness** — a heavy/lab-tier study (`#[ignore]`d out of the commit
  gate, opt-in like the census) that runs §3.3 and emits §3.4. Follows the lab's
  "studies are data, metrics are code" (decision 0011).
- **The report** — `book/src/laboratory/generated/the-sounding/` (summary + CSV
  + sample biographies), regenerated by the harness, drift-checked in CI.
- **Determinism tests** — fast unit tests (in the commit gate) asserting the
  spike is reproducible (same seed → identical biographies) and constitution-
  clean; the *heavy sweep* stays out of the gate.

## 5. Testing & acceptance

The deliverable is **measured numbers that stand as evidence**, so acceptance is
about the measurement's integrity, not a performance pass/fail:

1. **Determinism** — same seed + same parameters → byte-identical biographies
   and identical timings-modulo-noise; reload/replay-stable. A fast gate test.
2. **The preregistered scaling hypotheses are stated before the run and then
   checked against the fit** (measure-don't-narrate; preregister-on-named-axes):
   genesis-bake `O(Z · A · d̄)` (linear in each, linear in degree — the coupling
   is bounded-local); read `O(1)` amortized (a biography sample / handle expand
   is not a function of world size); present-replay `O(region · ΔA · d̄)`. **The
   benchmark's job is to falsify or confirm these** — a super-linear coupling
   exponent is the headline finding either way.
3. **The report is legible and honest** — the frontier is plotted per budget;
   the sample biographies are inspected and read as plausible histories (a taste-
   checked gate — a human confirms the output is worth having); any measured
   super-budget region is reported plainly, never hidden.
4. **Constitution-clean** — the spike passes the workspace's `disallowed_types`
   (no HashMap), fmt, clippy; proves the real engine's constraints are
   satisfiable at the loop level.

## 6. Determinism & save-format

**Not genesis-changing. No epoch. No census regen.** The spike is a new,
isolated crate over synthetic inputs — it touches no existing world derivation,
no stream labels, no settlement placement, no committed world goldens. The
constitution nonetheless binds it (seeded, `BTreeMap`/`Vec`, `total_cmp`,
`quantize`-at-emit on the report), so the numbers reflect the real engine's
constraints and the spike de-risks constitution-compatibility. The report is a
**new** committed artifact (drift-checked), not a re-baseline of an existing
one. No carve-out is triggered.

## 7. Non-goals

- **The engine itself** — real structural pressures wired to demography/climate,
  real event dynamics, real role-handle→persona derivation, the connection graph
  derived from real currents/passes/portals. That is the **sequel** campaign,
  built on the skeleton this one proves tractable.
- **Realistic dynamics** — the events are placeholders; their realism does not
  affect the scaling this campaign measures.
- **Integration with real worldgen** — synthetic inputs only.
- **New biomes, rosters, or the ambient encounter-table layer.**
- **The fine-cognition profile** — an individual's moment-to-moment cost
  (perception, planning, drives; a goblin's or a drow-aide's *morning*). It is
  LOD-bounded (scales with observation, not world size) and largely The Walk's
  territory — its pieces (The Surmise, The Foresight, The Wanting, The
  Quickening) already run. It needs its **own** sounding, not this one.
- **Emergent quality** — whether the composed layers *read* as rich rather than
  mush. Taste-checked, cultivated and observed; no scaling benchmark answers it.
- **The institutional layer** — a mid-scale structure between community and
  persona (a court, a bureaucracy, a slave system; the drow-aide's world). An
  ontology gap for the sequel (frontier SOC-11), out of scope here.
- **A performance *target*** — we report the ceiling; we do not commit to hitting
  a number (decision #3, "sweep and find the ceiling").

## 8. Judgment calls

- **Skeleton crate placement (G3 flagged)** — kernel-only experimental crate vs a
  `windows/lab` module. Kernel-only keeps it a clean, layering-respecting unit
  the eventual engine can absorb; a lab module is lighter but couples the
  skeleton to the lab. The owner decides, because it pre-commits where the engine
  grows and touches `cli/tests/architecture.rs`'s layering + dependency
  allowlist.
- **Placeholder-dynamics fidelity** — trivial enough to build fast, structured
  enough that the coupling cost is real (displacement and a diffusive quantity
  genuinely propagate over the graph). If a reviewer judges the placeholder too
  trivial to exercise the coupling, that is a real finding to deepen the spike,
  not to wave through.
- **Budget line values** — the read/replay/genesis/memory lines are estimates of
  "interactive", "on-move", "world-creation", and "fits in RAM"; they annotate
  the plot and can be re-drawn without re-running the sweep.

## 9. Task shape (for the plan)

Roughly: (T1) the skeleton crate — communities, synthetic connection graph,
the epoch loop with placeholder pressure + events + graph-coupled displacement/
diffusion, under the constitution + determinism unit tests; (T2) the three
measured directions (genesis-bake, read, present-replay) as instrumented entry
points + peak-memory + biography-size capture; (T3) the sweep harness (heavy/lab
tier) across the four axes, scaling-exponent fit, the preregistered hypotheses
frozen first, and the report generator (summary + CSV + sample biographies);
(T4) run the sweep, write the report, the DoD (chronicle/retro; flip SOC-10/
UNI-30/MAP-60 registry rows toward the sequel; land the frontier doc) + close.
