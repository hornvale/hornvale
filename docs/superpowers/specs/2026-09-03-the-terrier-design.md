# The Terrier — the world's occupation register is kept once, not re-surveyed per call

**Date:** 2026-09-03 · **Registry rows:**
`TOOL-chamber-snapshot-prices-a-shadowcast` (`raw`, high — the row this
campaign was opened on, and corrects), `TOOL-tick-profile-2026-08` (its
last sentence repeats the same attribution),
`TOOL-turn-cost-is-json-and-the-spatial-channel` (the walk-band sibling,
untouched) · **Program:** The Penstock metaplan, the derived-component
layer — a world-scoped derivation moved to its one correct owner. ·
**Ledger:** `docs/superpowers/ledgers/2026-09-03-the-terrier.md`

Decision block: 0636–0645.

A terrier is an estate's register of who holds which land. Hornvale's is
`occupations_by_vertex`: every occupation the history bake committed,
grouped by the vertex it stands on. The vessel has been re-surveying the
whole estate every time it asked who holds one plot. Nathan's brief: *"The
chamber shadowcast, ~8 ms per derivation. After The Rack it is the largest
single item left in a chamber turn, and it is why enter and indoor look
still miss the 15 ms client line. The memo already keys correctly; the
lever is the derivation itself."*

The brief is right that the lever is the derivation and right that the
memo keys correctly. It is wrong, in the way The Rack's chronicle and the
registry row are wrong, about **which step** of the derivation costs the
money — and that is the first thing this campaign has to say, because a
campaign that optimised the shadowcast would have shipped a 0.01 ms
improvement with a green gate.

## 1. The problem, measured

### 1.1 The attribution was a two-point difference

The Rack measured a chamber snapshot at 8.4 ms after `look` (the sighting
memo hits) and 16.3–16.8 ms after `map` or a chamber `go` (the memo
misses, so the snapshot derives its own), and wrote: "the difference,
about 8 ms, is one shadowcast". The difference is one **`derive_sighting`**.
Nothing decomposed the derivation, and the shadowcast is the step it is
named after.

### 1.2 The derivation, decomposed

Scratch `Instant` prints inside `derive_sighting`, `chamber_plan`,
`describe_chamber_here`, `enter`, `brief_here` and `brief_of`, on
`windows/vessel/examples/move_cost.rs`'s own sequence, release profile,
seed 42's flagship, MacBookPro, **contended** (`uptime` load averages
`28.07 25.58 22.66` on the first run and `51.01 34.28 26.41` on the
second — every figure below is an upper bound, and the *split* is the
finding, not the absolute):

```text
step inside derive_sighting                       per call
  chamber_interior_here  (= one brief_here)     8.7 – 12.6 ms
  anchor_cells                                  0.086 – 0.091 ms
  shadowcast (SIGHT_RADIUS = 4, ≤ 81 cells)     0.011 – 0.013 ms
  occupancy seat + interior_of                  0.005 – 0.006 ms
  placement loop + furnishings                  0.009 – 0.013 ms

step inside chamber_plan
  fabric_here                                   0.003 – 0.004 ms
  chamber_sources        (= one brief_here)     8.8 – 17.6 ms
  light_field                                   0.060 – 0.896 ms
  plan_of (+ ambient)                           0.119 – 1.023 ms

inside brief_of                     n = 42 calls in the run
  is_built                                      mean 0.000 ms
  is_cold                                       mean 0.003 ms
  containing_vertex                             mean 0.003 ms
  occupations_by_vertex (452 vertices)          mean 11.421 ms, max 26.048 ms
```

Every millisecond in a chamber turn above the walk-band floor is
`hornvale_worldgen::occupations_by_vertex(world)`, called from
`brief::brief_of`, which reconstructs **every occupation in the world**
from the ledger — `find(IS_OCCUPATION)`, then seven-plus `value_of` reads
and string parses per record, then a founding-coordinates map and a sort
per vertex — and then `.remove(&vertex)`s the one entry it wanted and
drops the rest.

### 1.3 How many times a turn asks

`Session::brief_here` has five static call sites (`enter` from out of
doors, `named_neighbour` for `enter <named>` through an aperture,
`lattice_of`, `describe_chamber_here`, and `chamber_interior_here` — which
itself has 17 callers, `derive_sighting` and `chamber_sources` among them).
Dynamically, per turn, measured:

```text
turn                       brief_here in handle   in snapshot   total    handle      snapshot
enter (from out of doors)         4                   1           5      55.4 ms     9.6–19.8 ms
look (chamber)                    2                   1           3      18.6–22.3   9.2–10.9
map / go n e s w (chamber)        0                   2           2      0.02        18.9–25.4
snapshot after look                –                  1           1      –           9.2–10.9
```

`enter`'s 55.4 ms handle is 9.4 + 26.3 + 10.2 + 9.0 ms of brief;
`structure_at` is 0.003 ms, the lattice embedding ~0.1 ms, the chamber
prose 0.003 ms. The Rack's quieter reading of the same verb (33.7 ms
handle, 16.5 ms chamber `look`, 16.3–16.8 ms chamber snapshot) is the same
shape at lower contention.

### 1.4 The fix has been prescribed in the source since the brief was born

`windows/vessel/src/brief.rs:154-160` (`4569d883d`, 2026-07-27):

```rust
            // NOTE ON COST: this derives the whole per-vertex occupation map on
            // every call. Correct but wasteful, and `brief_of` will be called
            // per descent. If a profile shows it mattering, hoist the map to
            // the caller (the session can hold it for the possession's life) —
            // do NOT memoize inside this function, because a hidden cache in a
            // derivation path is how derived state stops being derived.
```

A profile now shows it mattering. The note also forbids the memo shape,
and this design honours that.

### 1.5 What is NOT the cost

So that the next reader does not re-open it: the shadowcast at
`SIGHT_RADIUS = 4` is 0.012 ms; `anchor_cells` is under 0.1 ms;
`light_field` runs one shadowcast per source and costs under 1 ms; `plan_of`
is 0.12 ms warm; `is_cold` and `containing_vertex` are already served by
hoisted, cached state and cost 3 µs each. The sighting memo's key is
complete and correct (The Rack, Task 4) and is not touched.

## 2. What must survive

**2.1 The ledger is the only stored truth** (Penstock §3.1). The hoisted
map is derived from `world.ledger` and never serialized; nothing derived
from it may be committed (it already is not — a `Brief` is a coordinate
read at derivation time).

**2.2 A view equals its scan.** The hoisted map must equal
`occupations_by_vertex(world)` computed fresh at every read, for every
vertex, and `brief_of` over it must equal `brief_of` over a fresh map at
every locale a script visits. The `World` is immutable for the life of a
`WorldContext`, so this is true by construction — and asserted anyway
(§4 P6), because "by construction" is the sentence a later writer to
`world.ledger` would not read.

**2.3 `WorldContext` is world-scoped and immutable after `build`** (The
Quire). The map is both. The order of `build`'s five seeded derivations is
a save-format contract; a ledger read draws nothing from any stream, and
is placed **after** the five so that even a reader who does not know that
cannot mistake it for a sixth.

**2.4 Determinism.** Same seed, same script: byte-identical session
goldens, gallery transcripts, client fixtures, world file. The map's
iteration order is `Vertex: Ord`'s; `brief_of` reads one entry.

**2.5 The layering.** `windows/vessel` already depends on
`hornvale_worldgen` and `hornvale_history`; no new edge.

## 3. Design

**3.1 The hoist.** `WorldContext` gains one field:

```rust
    /// The world's occupation register (The Terrier): every committed
    /// occupation grouped by the vertex it stands on, reconstructed from
    /// `world.ledger` ONCE here and read by every `Brief` this context's
    /// sessions ever derive. A pure function of the immutable `World`, so it
    /// is world-scoped like everything else on this type. Built AFTER the
    /// five seeded derivations above and consuming no stream draw: it is a
    /// read, not a sixth derivation, and cannot move the order the gallery
    /// transcripts guard.
    pub(crate) occupations: BTreeMap<Vertex, Vec<OccupationRecord>>,
```

built at the end of `WorldContext::build` with
`hornvale_worldgen::occupations_by_vertex(world)`. The cost moves to
`build`, once per world: ~9–26 ms contended against a ~3 s build.

**3.2 `brief_of` takes the register, not the world.** The `world: &World`
parameter exists only to feed `occupations_by_vertex`; it is replaced by
`occupations: &BTreeMap<Vertex, Vec<OccupationRecord>>`, and the body reads
`occupations.get(&vertex)` and finds the living occupation by reference,
cloning only the one record it returns axes from. The `NOTE ON COST` is
rewritten to say what happened to it — the note prescribed this and should
not be deleted as if it had never been right. `Session::brief_here` passes
`&self.wctx.occupations`. One production caller
(`session.rs:6642`); no test calls `brief_of` directly (grep, 2026-09-03).

The 2–5 `brief_here` calls per turn are **left as they are**. After the
hoist a brief costs `is_cold` (3 µs, cached) plus a map lookup; threading
one `Brief` through `enter`'s four sites would be complexity with no
measurable return. Recorded in the follow-up register with the post-hoist
per-call number so the decision is re-checkable, not re-litigated.

**3.3 The ratchet — a structural source scan, not a counter.** A test in
`windows/vessel` (in-module, beside `WorldContext`, following
`underground.rs::the_reach_seam_is_the_only_source_of_the_radius` and
`affordance.rs::no_verb_by_object_table_exists`) asserts that the
production half of every file under `windows/vessel/src` names
`occupations_by_vertex`, `occupations_at` or `occupation_records` only
inside the body of `WorldContext::build`, with a positive control that the
`build` body does name one. **Direction stated in its doc:** it forbids a
whole-world history read anywhere on a session path; it does not prove the
hoisted map is complete or current — §4 P6 does that. Witnessed **red**
against the pre-change tree (where `brief.rs` names it) before green.

Why not a `TurnWork` counter: after the fix no turn path reconstructs an
occupation, so the counter would have no writer — the permanently-green
zero The Rack argued against. Why not a wall-clock ceiling: `session_cost`
is host-gated and blunt (The Rack), and its ceilings are upper bounds that
remain valid when the reading falls; no re-pin is bundled here.

**3.4 The corrections.** The wrong attribution is in seven places, and a
correction has a blast radius (memory). Each is corrected **in place,
loudly and dated**, in the house style for a record that outlived its
subject, never by quiet edit:

- `book/src/frontier/idea-registry.md` — `TOOL-chamber-snapshot-prices-a-shadowcast`
  (body rewritten: the 8 ms was the brief's occupation map; the shadowcast
  is 0.012 ms; status `shipped` at close; the ID is permanent and is not
  renamed) and the last sentence of `TOOL-tick-profile-2026-08`.
- `book/src/chronicle/the-rack.md` — "The numbers" (the "one shadowcast"
  sentence) and "Honest limits" ("the chamber's ~8 ms shadowcast").
- `docs/retrospectives/the-rack.md` — the follow-ups bullet.
- `windows/vessel/examples/move_cost.rs` — the AFTER-Rack block's
  interpretation paragraph (the Measured numbers stand; their reading
  does not).
- `clients/game/bin/examples/move_cost.rs` — "That is the same shadowcast
  cost".

Statements that a turn "derives at most one shadowcast" (decision 0598,
`turn_budget.rs`, The Rack's plan) are **counts** and remain true; they are
not in the sweep.

**3.5 The register's own instrument.** `windows/vessel/examples/move_cost.rs`
gains a `## Measured — AFTER The Terrier` block in the same format as its
three existing blocks, and `clients/game/bin/examples/move_cost.rs` gains
its AFTER block restating P6's rule (min and max over every row, rounded
half-up to two decimals, `needs` included). The scratch decomposition in
§1.2 is recorded in the vessel block's prose as the reading that redirected
the campaign, with its load averages.

**3.6 Out of scope.** The walk-band 4.2 ms JSON/spatial-channel floor
(`TOOL-turn-cost-is-json-and-the-spatial-channel`); re-pinning
`session_cost.rs`'s Mac-keyed ceilings
(`TOOL-session-cost-has-no-canonical-basis`); making `occupation_records`
itself cheaper in `windows/worldgen` (its other callers are per-world
field derivations and lab metrics, which already call it once);
`CLIENT-cache-demography-report`'s 480 ms of startup, which sits in the
same `build` block and is the next largest thing there.

## 4. Preregistered measurement

Frozen before the code (decision 0016). Counts first, then the box.

- **P1 — the ratchet reads zero.** (Count, commit gate.) No production
  file under `windows/vessel/src` names a whole-world occupation read
  outside `WorldContext::build`. **Red before green:** the test is written
  and run against the pre-hoist tree first, where `brief.rs` names
  `occupations_by_vertex`, and the observed red message is pasted into the
  test's doc.
- **P2 — the chamber turn, native.** (`move_cost.rs`, release, seed 42,
  quiet box — all three `uptime` load averages under The Repose's 4 — or
  reported contended with the averages, never tuned.) Against The Rack's
  AFTER block: chamber `snapshot()+json` after `map`/`go` **16.3–16.8 → ≤ 3
  ms** (The Rack's own missed P1 budget for a chamber snapshot, now
  reachable because the brief was the residue); `enter` handle **33.7 →
  ≤ 3 ms**; chamber `look` handle **16.5 → ≤ 1 ms**. Expected shape: every
  chamber row within 1 ms of its post-`look` sibling, because the only
  thing that distinguished them was a brief.
- **P3 — the client feels it (P6 restated).** `clients/game/bin/examples/
  move_cost.rs`, default profile: **every indoor movement turn ≤ 15 ms**
  (`enter`, chamber `look`, `map`, `go n/e/s/w`), from 47.4 / 29.2–29.5 /
  18.7–20.0 ms. Outdoor rows are the control and must not move by more
  than noise.
- **P4 — what `start` pays.** `Session::start` (and `WorldContext::build`)
  grows by exactly one `occupations_by_vertex`: **≤ +30 ms** (three times
  the worst contended per-call reading), measured by the existing
  `Session::start` line of `move_cost.rs` before and after on the same box.
- **P5 — nothing byte-visible moves.** `make rebaseline` then the drift
  diff over `docs/generated-paths.txt`. Decision rules: only
  `docs/audits/` moved (the pub signature change drifts the type-audit
  report) → regenerate and commit in the same commit; a gallery possession
  transcript, a session golden, a client fixture, `world-seed-42.json`, an
  almanac or anything under `book/src/laboratory/` or `book/src/domesday/`
  moved → **STOP**, the hoist consumed a draw or changed a `Brief`, and
  that is a defect to diagnose, not an epoch to accept.
- **P6 — VIEW ≡ SCAN for the register.** For every vertex,
  `wctx.occupations[v] == occupations_by_vertex(world)[v]` (trivially, but
  asserted), and for every locale a script visits at seed 42 (a living
  occupation at the flagship; none one room north) and at seed 7 (a
  population that moves, per The Rack), `brief_of` over the hoisted map
  equals `brief_of` over a fresh map. Non-vacuity asserted: at least one
  visited locale has a living occupation and at least one has none, so a
  hoist that dropped the map would fail on the first and one that returned
  a stale "alive" would fail on the second.

Decision rules beyond P5: a P6 disagreement → the hoist is wrong, never the
scan; a P2/P3 miss with P1 green and P6 green → report the miss with a
decomposition (the scratch prints are ten minutes to restore) rather than
tune anything; a P4 overshoot → the map is being built more than once,
find the second builder.

## 5. Capture

- `TOOL-chamber-snapshot-prices-a-shadowcast` → body corrected per §3.4,
  `shipped`, Where → this spec and the chronicle.
- `TOOL-tick-profile-2026-08` → last sentence corrected.
- New `PROC-a-two-point-difference-names-a-step-not-a-cost` (`raw`, high,
  measured): a cost attributed by subtracting two timings is the whole
  derivation between them, and the registry carried the name of one step
  as if it were the number for five weeks; the cure was ten lines of
  scratch prints, and the check belongs before the row is written, not
  after a campaign is opened on it.
- Follow-up register: the 2–5 per-turn `brief_here` calls, with the
  post-hoist per-call number; `chamber_interior_here`'s 17 sites likewise.
- Decision 0636 — a world-scoped derivation lives on `WorldContext` and is
  never rebuilt on a session path; the source ratchet is its guard.
- Retrospective: the attribution lesson, and that the fix was in the
  source's own comments for five weeks.

## 6. Definition of Done

- §3.1–§3.3 shipped; P1 red-then-green with the red pasted; P6 green at
  both seeds with non-vacuity asserted.
- §3.4's seven corrections landed, each dated and loud.
- §3.5's two Measured blocks written from real runs, with `uptime` quoted.
- `make rebaseline` per P5's decision rules; type-audit report regenerated
  in the same commit as the signature change.
- Decision 0636 written; the registry rows in §5 moved; the follow-up
  register promoted into the retrospective.
- Chronicle `book/src/chronicle/the-terrier.md` (+ `SUMMARY.md`),
  retrospective `docs/retrospectives/the-terrier.md` (+ its README line),
  Confidence Gradient re-scored if any bet moved (none expected).
- Stage gates at each plan-stage boundary; census queued at close
  (`make sluice-census`) as ordinary work; the merge through `make sluice`
  with a `Sluice-Headline` trailer.
