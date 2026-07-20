# Retrospective — The Sounding

One page of process, not product. The product is chronicled; this is what the
close learned, and — because this campaign spent most of its value in the
discussion around the benchmark rather than the benchmark itself — the durable
disciplines it established.

## What went well

- **The spike found its target on the very first run.** The Sounding exists to
  detect a quadratic coupling before the engine is built on it. Its first sweep
  did exactly that: the naive `deliver` linear-scanned to find the community at
  a node, the coupling was O(Z²·A), and the top point exhausted memory. The
  method — synthetic core loop, sweep an axis, fit the log-log exponent — earned
  its keep immediately, which is the strongest possible evidence that the
  method is worth reusing.
- **The two-stage review caught what the plan shipped.** The Task-1 code came
  *verbatim from the plan* and still carried three real defects — same-epoch
  raid deliveries silently dropped, raiders never decrementing their own
  population, colliding delivery handles — that made the coupling the benchmark
  exists to measure largely inert. The task reviewer caught all three. A plan is
  not a proof; the review loop is the net, and here it was load-bearing.

## What the campaign taught mid-execution

- **A benchmark is the wall-clock exception, and its report is evidence, not a
  golden.** Timing needs `std::time::Instant`, which the constitution bans
  workspace-wide; the harness carries a scoped `#[allow(clippy::disallowed_types)]`
  with a comment, and the core loop has none. Because timings are
  machine-dependent, the committed report is *evidence regenerated on demand*,
  not a byte-identity drift-checked artifact — only the deterministic sample
  biographies are pinned. Read the spec's "drift-checked" wording accordingly.
- **Three repo-convention traps a fresh implementer hits, worth naming:**
  `Stream::range_u32(lo, hi)` is **inclusive** of `hi` (index uses must bound at
  `len - 1`); a `#[ignore = "heavy: …"]` reason must match a **canonical**
  string *verbatim* (`cli/tests/heavy_tier.rs` enforces it, not merely the
  `heavy:` token); and `f64::ln` is clippy-disallowed (cross-platform libm
  divergence, decision 0041) — transcendentals route through
  `hornvale_kernel::math`.
- **The parked-subagent failure recurred, and the controller must take over.**
  The Task-4 implementer, blocked on the runaway sweep past the 60-minute Bash
  ceiling, returned narrating "standing by" — the classic parked-agent tell. Its
  background job was dead. The controller diagnosed the O(Z²) directly, fixed it,
  and re-ran. When a dispatched agent parks on a runaway, do not resume it into
  the same runaway — take the work back.

## The durable disciplines (the campaign's real yield)

### Sound before you build — the series of adversarial soundings

The Sounding's method is the reusable asset, and the forward-looking move is to
make it **plural and adversarial**: one synthetic sounding per cost shape, run
*before* each subsystem is built, each swept deliberately to the breaking point
of its assumption. This campaign already stressed graph density to its edge
(degree → 256, long-range → 100%) and confirmed the *event* coupling is flat.
The named next soundings:

- a **diffuse-coupling sounding** — sweep graph density and watch a fixed-point
  over *all* edges (trade/culture/disease), to find where diffusion goes
  quadratic and cap density there;
- a **blast-radius axis** on replay — sweep how globally an event propagates, to
  price the world-spanning-event case (a war mobilizing the map) the local
  assumption cannot absorb;
- a **fan-out-depth sounding** — for relationships, belief hypotheses, and plans,
  to find where the combinatorial tier needs a hard depth bound.

The four **load-bearing assumptions** whose violation *is* every difficult
scenario: **graph sparsity** (diffuse coupling), **event locality** (replay),
**bounded fan-out/depth** (relationships, hypotheses, plans), and **coarse time**
(fine epochs blow up replay depth). Naming them turns "what might be intractable"
into a checklist.

### Hornvale is a deterministic, event-sourced, in-memory database

The `node_index` we hand-wrote is a secondary index; the accumulation of tables
and indexes is not drift — it is the recognition that `World = seed + ledger` is
an **event-sourced database**: the ledger is the write-ahead log, the derived
views (belief, the social graph, the indexes) are **materialized views**, and
determinism is "the database is a pure function of its log." The settled
recommendation, with reasons, so it is not relitigated blindly:

- **Own the identity; build the abstractions; do not import an engine.** A formal
  embedded database (SQLite, sled, redb) is a **determinism liability** — its own
  float handling, iteration order, and page layout are precisely the
  cross-platform byte-identity battle decisions 0033/0041 were fought to win. The
  data is *derived*, so there is nothing to persist (the irreducible core is a
  seed and a delta log). The queries are point-lookups and graph traversals, not
  SQL joins — `BTreeMap` and the room mesh already cover them. The one legitimate
  database use is **offline analytics on emitted data**, which is already exactly
  right (DuckDB over the census CSVs, never in the core).
- **Follow-ups (the abstractions worth building):** an **`Indexed<Entity>` table**
  that owns a primary store plus auto-maintained secondary indexes, so
  index-maintenance is one reviewed abstraction instead of a hand-rolled pattern
  re-implemented per site (the `node_index` bug surface); and a
  **reversible/transactional event-application** over the ledger — a mutation is a
  set of events applied atomically, and reversibility is "re-derive without those
  events," home-grown over the model that already exists, not imported MVCC.

### Benchmarking tooling

- **Build the reusable scaling-harness (home-grown, no dependency);** extract this
  campaign's `sweep_axis` / `fit_exponent` / report into a shared utility so every
  future algorithm is stress-tested at scale by construction — the series of
  soundings needs it regardless. The scaling method (sweep → exponent → push the
  assumption to its edge) is Hornvale-specific and is *not* what Criterion does.
- **Revisit Criterion only for the narrow CI-perf-regression question** — its one
  unique value our tooling lacks is "did this function get slower since last
  commit," which is a scoped, later decision, not a general adoption; it comes
  with heavy transitive dependencies against the serde-only allowlist (0004).

## Follow-ups promoted from this campaign

- The three next soundings (diffuse-coupling, blast-radius, fan-out-depth) and the
  shared scaling-harness that hosts them.
- The `Indexed<Entity>` table abstraction and the reversible/transactional
  event-application over the ledger.
- The real living-community **engine** (the sequel): real structural pressures
  wired to demography/climate, real dynamics, role-handle → persona expansion, the
  connection graph derived from real currents/passes/portals — built on the
  skeleton this campaign proved tractable, and carrying the node index the
  benchmark proved it needs.
- Two decision-revisits, each recorded here with its reasoning so it is not
  reopened without new information: *a formal database in the core (recommended
  against)* and *Criterion (deferred to the CI-regression question)*.
