# The Lookup — retrospective

**Completed:** 2026-07-23 (spec `docs/superpowers/specs/2026-07-23-the-lookup-design.md`,
plan `docs/superpowers/plans/2026-07-23-the-lookup.md`, five tasks: coord cache
→ ConnectionGraph Vec → bounded sibling grep → convention/infra → close). A
performance campaign, chartered from a flamegraph spike earlier in the same
session. Ran under campaign-autopilot.

**The profiling itself taught the sharpest lesson: profile the tree you're
about to ship, not the one you happen to be standing in.** The first
flamegraphs of the session were taken in the main checkout, which was still on
a *stale* local `main` (pre-subsurface-arc, 1393 facts/world) rather than the
`origin/main` just pushed (3514 facts). The stale profile put transcendentals
at 47% of genesis and never showed the real #1 hotspot at all. Only re-profiling
on the current tree revealed that `ConnectionGraph::add_edge` — invisible in the
stale run — was the single busiest routine at ~22%. Every headline number from a
profiler is relative to the exact binary under it; a fact count that doesn't
match (1393 vs 3514) is the tell that you are measuring the wrong thing. This
cost a full re-baseline mid-spike.

**"Formalize, don't re-derive" is the right execution mode for validated
byte-critical code.** The two changes were prototyped and proven byte-identical
in the spike *before* the campaign was chartered. Subagent-driven development's
default — a fresh implementer re-writing each task from a prose brief — would
have been actively dangerous here: a byte-identity change re-typed from prose
can drift by a `.clamp()`, an argument order, or a `.to_degrees()` and diverge
silently. So execution *formalized the existing prototype* (the controller added
regression tests and clean commits) and spent subagents where they earned their
keep: an independent determinism review of the finished diffs. The lesson
generalizes — when the correct implementation already exists and is verified,
re-implementation is a regression risk, not a quality gate.

**Byte-identity by construction beats byte-identity by test.** Both fixes were
shaped so that identity is a property of the code, not a hope checked
afterward: the cached coordinate is the *same expression on the same stored
data*, the flat graph iterates in the *same order*. The tests
(`coord_cache_bit_equals_recomputation_at_every_cell`, `graph_byte_identity`,
the 40-seed world-hash sweep, `lens_purity`) confirmed the construction rather
than carrying the guarantee. This is the only posture that scales to a kernel
change, where a single-seed green is necessary but never sufficient — the real
gate is the 1000-seed census drift.

**The scope guard did real work — it declined a genuine sibling.** The bounded
grep (ledger #4) found that terrain's `lithology.rs`/`carve.rs` recompute
`asin(position.z)` for latitude — the exact recompute shape the campaign was
about. But that value is in *radians and clamped* while the cache is in *degrees
and unclamped*, so routing it through the cache would need a unit round-trip
(`asin(z).to_degrees().to_radians()` is not bit-equal to `asin(z)`) and would
drop the clamp. The "include a sibling only if provably byte-safe" rule turned a
tempting fix into a followup instead of a determinism regression. A pattern-hunt
campaign needs that guard explicitly, or it over-reaches into the very corruption
it's trying to speed past.

**Ideonomy's periodic-grid confirmed rather than expanded — and that was
useful.** Two passes on the scope decision produced zero overturns, but the
{access-shape × key-type} grid drew the exclusion line precisely: the *dense-key*
column is this campaign; the *sparse-key* column (edge-pair caches, working
sets) genuinely warrants a map and is out of scope. A confirming pass that
sharpens a boundary is not a wasted pass.

**Scope notes.** No `open-questions.md` bet moved (no tracked perf/genesis bet).
No idea-registry row (infra, not a speculative direction). The
`[profile.profiling]` Cargo.toml profile was kept as non-invasive profiling
infra (ledger #5), the same class of dev-tooling decision as nextest.

## Follow-ups (promoted from `.superpowers/sdd/followups.md`)

- **Allocation churn / the `.collect()` pattern** — after these fixes, memory
  allocation leads the genesis profile (~17% self). Reuse scratch buffers /
  iterate lazily. Different lever; next perf pass.
- **`annual_mean_insolation` solar-geometry loop** — recomputes per-(cell,day)
  trig; hoist the per-cell-invariant part out of the day loop. A loop-hoist, not
  an index cache.
- **Terrain latitude recompute** (`lithology.rs`/`carve.rs`) — the byte-unsafe
  sibling above; needs a separate cached radian-latitude to fold safely.
- **Finer `NearestCellIndex`** — its dot-product loop dominates it in the census
  path; a finer spatial index could help but carries tie-break/determinism risk.
- **Re-run (b)/(c) profiling on current `origin/main`** — the session's census
  sweep and render/possess profiles were taken on the stale checkout; redo.
