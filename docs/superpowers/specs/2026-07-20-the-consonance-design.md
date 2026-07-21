# The Consonance — Design

**Date:** 2026-07-20
**Status:** **Approved at G3 (2026-07-20)**.
**Campaign:** LANG-48, "proportional/relational reasoning over already-
witnessed quantities" — a new mechanism spanning `domains/astronomy`,
`domains/language`, and `windows/worldgen`. Complements LANG-37 (the
causal-schema library, fully shipped by The Explanations) and LANG-44 (the
numeracy rung, shipped by Few and Many).

**Post-implementation erratum (2026-07-20, at close):** two claims below
were found wrong during implementation, independently re-verified by task
reviewers and the final whole-branch review; the shipped code is correct,
this spec's prose is what's stale. Left the body text below as the
historical planning record rather than silently rewritten.

1. **The real admitted schema set for `FactShape::CyclicEvent` is FOUR
   schemas, not three.** Every place below reading "`Agentive`,
   `PathJourney`, `Balance`" should read "`Agentive`, `PathJourney`,
   `Balance`, `CycleReturn`" — confirmed directly against
   `schema_table()` in `domains/language/src/schemas.rs`. This was a gap
   in the controller's own research before this spec was written, not a
   deviation the implementation introduced; every test built against the
   real four-member set.
2. **§3.4's proposed mechanism for `expressible_at_rung` (reusing
   `render_quantity_at_rung`'s own classification) is unsound and was not
   built as written.** `render_quantity_at_rung` renders both `1.0` and
   `2.0` as *exact* words at `Subitizing`, so "degrades to a qualitative
   bucket" never fires for a `2:1` ratio's own integers — the spec's own
   rule would have wrongly called `2:1` nameable at the floor. The
   shipped predicate instead asks directly whether a rung's grammar can
   compose two named quantities into a relationship (true at `1:1`
   everywhere — naming one quantity twice, not a relationship — false at
   `Subitizing` for any other ratio, true at `FullCounting`/`Decimals`),
   which is what §3.4's prose actually argues for even though its
   proposed IMPLEMENTATION doesn't deliver it. Confirmed correct
   (monotonic, no counterexample exists) by the Task 5 review.

---

## 1. What this is

A two-mooned world's astronomy already commits a real orbital period for
every moon. If one moon orbits in 30 days and another in 60, the ledger
already contains a clean 2:1 relationship — the same shape of thing real
archaeoastronomy shows societies noticing and using long before abstract
mathematics existed (the saros cycle, the Metonic cycle). Nothing today
computes this relationship, and nothing lets a culture's account of the
world reflect it. This campaign derives it, gates a culture's access to it
by the same sky-capability discipline every other astronomical fact
already uses, and lets it flow into the causal-schema library exactly the
way `moon-count` and `day-length-std` already do — so "two gods walk in
step, one keeping pace at half the speed of the other" becomes a real,
derived explanation a culture can reach for, not an authored one-off.

## 2. What exists (verified against `main`@`41f351eb`)

- **`domains/astronomy/src/facts.rs`'s `MOON_PERIOD_STD`** — a real
  per-moon orbital period, committed once per moon. **Verified: no
  culture can witness it today.** `windows/worldgen/src/chorus.rs`'s
  `observability_table()` has exactly five rows (`is-a`, `moon-count`,
  `star-class`, `day-length-std`, `instance-of`); `moon-period-std` has
  none. `chorus_ground()`'s `CONSTRUCTION_ORDER` — the hardcoded list that
  flattens world truth into the `Vec<GroundFact>` every account is built
  from — never includes it either. Absence from the `Observability` table
  means unconditional *non*-access (`account.rs`'s `disposition_for`
  fails closed), not unconditional access.
- **`MOON_COUNT`'s own fact shape** (`facts.rs`, verified directly) —
  `fact(subject, MOON_COUNT, Value::Number(system.moons.len() as f64))`,
  where `subject` is one shared world/anchor-level entity used across
  every astronomy fact for that world. The new fact this campaign adds
  follows this exact shape.
- **`domains/language/src/schemas.rs`'s causal-schema library** — `Schema
  { id: SchemaId, source: SourceDomain, shapes: &'static [FactShape], slot:
  SlotKind, mediation: f64 }`, a closed 12-row table
  (`schema_table()`). `select_schema(admitted: &[(SchemaId, f64)], beta:
  f64, stream: &mut Stream) -> Option<SchemaId>` is fact-agnostic — it
  operates purely over caller-supplied `(SchemaId, weight)` pairs and
  never sees a `GroundFact` at all, so it needs no change to serve a new
  fact. **Verified which schemas admit `FactShape::CyclicEvent`** (the
  shape `day-length-std` already uses, and the shape this campaign's new
  fact will use): `Agentive`, `PathJourney`, `Balance` — read directly
  from `schema_table()`. `SchemaId::LinkSympathy` ("a bond linking the
  counted things") admits only `FactShape::Count`, not `CyclicEvent` —
  despite being a thematically excellent fit for a noticed relationship,
  it does not fire for this campaign's fact as shipped; extending it is a
  real, deliberately deferred follow-up (LANG-50).
- **`explain_day`/`explain_moons`** (`chorus.rs`) — the real, only
  precedent for selecting and firing a schema onto one fact: locate the
  fact's single `account.entries` index, call `admitted(shape)` →
  `schema_prior(subsistence, sociality, admitted)` →
  `select_schema(&prior, beta, &mut stream)`, then mutate that entry's
  `Disposition` to `Explained { underlying, schema, agent, lexeme,
  manner }`. Each is single-fact, single-entry, with its own permanent
  stream label. Nothing in this pipeline looks at more than one fact at
  once today.
- **`domains/language/src/numeracy.rs`'s `NumeracyRung` /
  `render_quantity_at_rung`** — verified used in exactly two files
  (`numeracy.rs` itself and `windows/book/src/lib.rs`'s
  `comprehend_quantity`, itself `#[allow(dead_code)]` and unwired into any
  live path). No existing precedent anywhere for a rung-gated capability
  check outside that one reserved seam.
- **Moon count is drawn 0–3** (`domains/astronomy/src/moons.rs`,
  `range_u32(1,100)`: 0 at 15%, 1 at 40%, 2 at 30%, 3 at 15%) — not always
  exactly two. A 0- or 1-moon world has no pair to compare (a real,
  honest absence, not a special case to code around); a 3-moon world has
  three candidate pairs.

## 3. Architecture

### 3.1 Ratio detection (`domains/astronomy`)

A new pure function, computed alongside every other per-world astronomy
fact (no cross-domain dependency — it operates only on `Moon.period`
values astronomy already holds): for every pair of a world's moons
(naturally zero pairs at 0–1 moons, one pair at 2, three pairs at 3 — no
moon-count special-casing needed, the algorithm is just "iterate every
pair"), compute the period ratio and compare it against a small closed
set of low-order rationals (numerator/denominator bounded by a small
constant — exact bound is plan-time tuning). A pair *matches* when its
ratio falls within a fixed relative-deviation tolerance (plan-time tuning,
precedented by The Residue's own leveling-fraction constant) of its
nearest candidate rational. Across all matching pairs, **at most one
survives per world**: the best match by (deviation ascending, then
simplicity — smallest `max(numerator, denominator)` — ascending, then
moon-index ascending for full determinism). Most worlds, most of the
time, have no matching pair at all — a real "no relationship" outcome,
not a gap to be filled.

The survivor becomes one new fact, `MOON_PERIOD_RATIO` (a new constant in
`facts.rs` beside `MOON_COUNT`/`MOON_PERIOD_STD`), shape identical to
`MOON_COUNT`'s own: `fact(subject, MOON_PERIOD_RATIO,
Value::Number(ratio))`, `subject` the same shared world-level entity every
other astronomy fact already uses. Zero new `Value` variants, zero new
entity types.

### 3.2 Witnessed access (`windows/worldgen/src/chorus.rs`)

One new `Observability` row: `MOON_PERIOD_RATIO → Requirement::SkyGraded {
threshold: <higher than moon-count's 0.6> }` — noticing a precise period
relationship demands more sustained observation than a one-time count, so
the new threshold sits meaningfully above `moon-count`'s (exact value is
plan-time tuning). Added to `CONSTRUCTION_ORDER` alongside the four
existing entries, so it flows into every culture's `GroundFact` list and
through `account_of` exactly like every other astronomical fact — gated,
not granted, exactly as `moon-count` already is.

### 3.3 Explanation (`windows/worldgen/src/chorus.rs`)

A new `explain_moon_ratio` function, structurally identical to
`explain_moons`: locate the single `MOON_PERIOD_RATIO` account entry (only
present when both §3.1 found a match and §3.2's capability gate admitted
it), call `admitted(FactShape::CyclicEvent)` → `schema_prior(...)` →
`select_schema(...)`, bind whichever slot the winning schema requires
(`Agent` via `beliefs_held_by`, exactly as `explain_day`/`explain_moons`
already do — no new slot-binding machinery), and mutate the entry's
`Disposition` to `Explained`. Reuses `FactShape::CyclicEvent` — zero
changes to `schemas.rs`'s admission lists. The admitted schema set for
this shape (`Agentive`, `PathJourney`, `Balance`) is not narrowed or
pre-selected by this spec; which one fires for a given culture is the
existing weighted machinery's call, exactly as it already is for
`day-length-std`.

### 3.4 Numeracy: a tested predicate, not a rendered sentence

Per the confirmed G3 scope (§6), this campaign does not render any new
Book prose. It does add one new pure function proving the numeracy half
of the original idea is real: given the detected ratio's two small
integers and a `NumeracyRung`, determine whether that rung's vocabulary
can *name* the relationship precisely — reusing
`render_quantity_at_rung`'s own classification (if rendering either
integer at a rung degrades to a qualitative "few"/"many" rather than an
exact word, the ratio isn't nameable at that rung). Proven by property
test; not wired into `comprehend_quantity` or any live rendering path,
which stays exactly as reserved as Few and Many left it.

## 4. The laws (standing tests)

1. **Purity:** ratio detection, the survivor tie-break, and the
   expressibility predicate are pure — same inputs, byte-identical output,
   every time.
2. **Non-degeneracy:** across real seeds, some worlds produce a matching
   pair and some don't, tracking actual period values, not a coin flip — a
   property test that found every world matching, or none, would indicate
   the tolerance is miscalibrated.
3. **Witnessed access is honestly gated:** a culture below the new
   threshold never has a `MOON_PERIOD_RATIO` account entry at all (not a
   `Lost` entry — genuinely absent, matching how `moon-count`'s own gate
   behaves below 0.6); a culture above it does.
4. **Explanation reuses the shipped machinery unmodified:** `select_schema`,
   `schema_prior`, and `admitted` are exercised, not reimplemented — a
   test asserting the fired schema is always one of `{Agentive,
   PathJourney, Balance}` (the real admitted set for `CyclicEvent`) is the
   check that this held.
5. **Expressibility tracks the rung honestly:** a low-rung listener's
   predicate returns non-expressible for a ratio a high-rung listener's
   returns expressible for, and the reverse never happens (a coarser rung
   is never MORE expressive than a finer one).

## 5. Determinism and blast radius

**One new permanent stream** for the ratio-detection's own draw-free
computation (none needed — §3.1 is pure, zero new `Seed`/`Stream` use) and
**one new permanent stream** for `explain_moon_ratio`'s schema-selection
draw (`language/<species>/schema/sky/<shape>`-style, mirroring
`explain_day`/`explain_moons`'s own labels exactly). One new
`Observability` row and one new `CONSTRUCTION_ORDER` entry — both pure
additions; no existing predicate's consumption order changes, so no
existing world's genesis is affected for worlds where §3.1 finds no
match (the overwhelming majority). No change to `select_schema`,
`schema_prior`, `admitted`, `schemas.rs`'s admission lists, `MOON_COUNT`,
`MOON_PERIOD_STD`, `render_quantity_at_rung`, or `comprehend_quantity` —
this campaign only reads and reuses them. No Book, CLI, or vessel
rendering surface change (§3.4) — proof is a property test, consistent
with The Residue and Few and Many's own precedent.

## 6. Non-goals

- **No live Book rendering.** Confirmed directly with Nathan (G3-pending
  question, answered before this spec was written): mechanism +
  account-gating + schema-binding only, proven by tests. Wiring
  `comprehend_quantity` into a live sentence is a separate, later
  campaign.
- **Moon period vs. `year-length-std`, N-ary (three-body+) resonance
  chains, and an event-coincidence framing** (phase alignment over dated
  observation windows, building on the eclipse/conjunction machinery) —
  all real, all captured as LANG-50, none built here.
- **Extending `SchemaId::LinkSympathy` to admit `FactShape::CyclicEvent`**
  — a thematically strong fit, checked and confirmed not currently wired;
  captured as part of LANG-50, not touched here (this campaign's own
  admitted set for `CyclicEvent` — `Agentive`/`PathJourney`/`Balance` — is
  unchanged and sufficient).
- **Per-creature granularity** — the new `Observability` gate is per
  culture, matching every other sky fact; no finer granularity is added.

## 7. Registry and decisions

- LANG-48 flips `raw` → `shipped` at close, repointing **Where** at this
  campaign's chronicle.
- LANG-50 (the wider/harder siblings, including the `LinkSympathy`
  extension) added to the idea registry during this brainstorm.
- Full reasoning trail — including the corrected `LinkSympathy` finding
  (thematically excellent, not currently admitted) and the direct
  rendering-scope question posed to Nathan — is in the campaign's decision
  ledger, summarized at G3.

## 8. Flagged for G3

1. **Save-format additions (leads, per the autopilot policy on
   determinism-contract calls):** one new fact predicate
   (`MOON_PERIOD_RATIO`), one new `Observability` row, one new
   `CONSTRUCTION_ORDER` entry, one new permanent schema-selection stream.
   All pure additions, no existing consumption order touched. Confirm.
2. **The `Observability` threshold** (SkyGraded, above `moon-count`'s
   0.6): confirm the rationale (a precise relationship needs more
   sustained observation than a one-time count) is the right one, even
   though the exact number is plan-time tuning.
3. **Rendering scope (already answered, confirmed here for the record):**
   mechanism + gating + schema-binding only, no live Book prose, no
   `comprehend_quantity` wiring — matches your direct answer before this
   spec was drafted.
4. **The `LinkSympathy` non-extension:** confirm you're comfortable
   shipping with the real admitted set (`Agentive`/`PathJourney`/`Balance`)
   for V1 rather than also extending `LinkSympathy` to admit
   `CyclicEvent` — the thematically strongest fit stays deferred.
