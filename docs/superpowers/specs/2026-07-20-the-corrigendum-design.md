# The Corrigendum — a taught prediction that fails, and gets revised

**Status:** Approved at G3

**Campaign:** LANG-49, the falsification-and-revision slice of "the
epistemic arc" — a culture's priesthood teaches a dated prediction that
can genuinely turn out wrong, and a later knowledge-state shows the
correction. Downstream of The Diachronic Book (LANG-42, the knowledge
ladder and the Reckoning-of-Years) and The Doctrine (LANG-39, the
folk/doctrine conflict map); closes the "ceiling" both campaigns
explicitly named and left open — the Kuhnian/reform cycle.

---

## The problem

`ladder_of` (`windows/worldgen/src/chorus.rs:1670`) grants a
`Predictive`-rung culture its taught prediction by querying the true
future eclipse generator directly, filtered to whichever recurrence class
(a `(moon index, eclipse body)` pair) it has witnessed most — so the
taught day is always correct, by construction, no matter what. Nothing in
the codebase lets a culture's own reasoning be wrong and then get better.
Real eclipse timing has genuine structure a naive model wouldn't capture:
the true recurrence beats against the **eclipse year**
(`hornvale_astronomy::eclipse_events::eclipse_year`, already shipped —
the beat between the calendar year and the moon's nodal-regression
period), the exact astronomical mechanism behind the real saros cycle. A
culture that extrapolated from what it had actually witnessed, rather
than consulting the true future, would sometimes miss — and that miss,
checked and revised, is the "line of reasoning checked against a
counter-example and revised" LANG-49's own registry row names as
proto-philosophy's first buildable rung.

## The mechanism

**A naive model, replacing the omniscient lookup.** For a culture at
`Predictive` rung tracking recurrence class `C` (the same single
top-observed class `ladder_of` already selects — no widening to multiple
classes), the taught next-day is:

```
predicted_day = last_witnessed_day(C) + mean_interval(C)
mean_interval(C) = (last_witnessed_day(C) - first_witnessed_day(C))
                    / (witnessed_count(C) - 1)
```

computed purely over `observations_of`'s already-witnessed events —
`witnessed_count(C) >= K_PREDICT` (8) whenever this fires, so the mean is
never computed from fewer than 8 occurrences. This *replaces* the current
omniscient value in the Reckoning's existing taught-prediction line ("The
next darkening, it teaches, comes on day {predicted_day}") — a real,
deliberate change to already-committed Reckoning prose (seed 42's
Reckoning text may change if any placed culture there reaches `Predictive`
by day 36,525; Task 1 includes the keystone/golden refreeze this implies,
following The Consonance's own close-time precedent, not deferred).

**Hit or miss, and a tolerance.** A *retrospective* check — did a past
prediction, made from only the data available before it, land close to
what actually happened — is a pure re-derivation requiring no new stored
state: for each witnessed occurrence of `C` from the 3rd onward (the
first point a mean-of-priors is computable), compute what the naive model
*would have* predicted using only the occurrences strictly before it, and
compare against the occurrence's own actual day. A **hit** is within 5%
of that prediction's own `mean_interval` (`PREDICTION_TOLERANCE_FRACTION
= 0.05`, mirroring The Consonance's `RATIO_TOLERANCE` convention — the
same "how close counts as close enough" shape, given its own named
constant rather than reusing the unrelated ratio-detection one). Anything
else is a **miss**.

**Crisis, not anomaly.** A single miss is silently absorbed — real
revision doesn't fire on one bad prediction. A **crisis is live** for
class `C` at knowledge-state `at` exactly when the two most recent
retrospective checks (per the replay above, ending at the latest
witnessed occurrence at or before `at`) are both misses.
`CRISIS_MISS_RUN = 2`, a named constant, not a magic number.

## Task 1 — the Reckoning margin gains a correction line

**Files:** `windows/book/src/lib.rs` (`reckoning_culture_lines`,
`reckoning_epoch`), `windows/worldgen/src/chorus.rs` (`ladder_of`, a new
pure function alongside it computing hit/miss/crisis).

For a `Predictive`-rung culture:
- The main taught line switches from the omniscient value to
  `predicted_day` above (a real behavior change, covered by the keystone
  refreeze). **The existing honest-omission arm is preserved, re-based on
  the new value**: today, the taught line is omitted when the omniscient
  query finds no matching event inside `PREDICTION_HORIZON_STD_DAYS`
  (tested by `the_prediction_line_omits_honestly_beyond_the_teaching_horizon`,
  `windows/book/src/lib.rs:4628`) — with a pure arithmetic model that
  check would otherwise never fire again (`predicted_day` is always
  computable). The omission condition becomes `predicted_day > at +
  PREDICTION_HORIZON_STD_DAYS` instead, keeping the same honest-arm
  *shape* (a priesthood doesn't teach a day so far out it teaches
  nothing) while reading it off the naive value rather than an omniscient
  existence check. The existing test is updated to drive this arm via a
  sparse/wide-interval synthetic sequence rather than relying on a live
  seed exhibiting it, mirroring how `explain_moon_ratio`'s own edge cases
  were driven synthetically in The Consonance.
- When a crisis is live for `C` at `at`, the epoch's existing `margin`
  field (today only ever carrying the count-shortfall line) gains one more
  line, in the same "In truth, ..." register the margin already speaks:
  *"In truth, the {autonym}'s priesthood taught the darkening would come
  on day {last_predicted}; it came on day {last_actual} instead."* — using
  the most recent of the two missed retrospective checks. This is a pure
  render-time computation; nothing is committed to any world's ledger, no
  new predicate, no new `Observability` row, no new stream label. A world
  with no crisis anywhere renders byte-identically to today's mechanism
  except for the (also real) taught-day value change above.
- At a later knowledge-state, `reckoning_at`/`reckoning_epochs` naturally
  re-derive from more witnessed data — a corrected (possibly still
  imperfect) prediction appears with no new mechanism needed. This *is*
  the revision loop: nothing about "revision" is modeled as an event,
  because this project's own knowledge model has never needed one — every
  knowledge-state is a fresh derivation, and a fresh derivation over more
  data is what revision *is* here.

**Testing:** a pure unit-test battery over the new hit/miss/crisis
function (synthetic interval sequences — e.g. a `[30, 30, 30, 30, 45]`-day
sequence should read a hit-hit-hit-miss, not yet a crisis; a
`[30,30,30,45,60]` sequence's tail two checks should both miss and read
crisis); then a live-world search (seeds 1..=200, mirroring The
Consonance's own search discipline) for at least one seed reaching
`Predictive` rung by day 36,525 with a live crisis, to prove the mechanism
fires on a real generated sky, not only on synthetic data. If no such seed
exists in that range, the search widens (documented, not silently
dropped) rather than the campaign shipping an unexercised code path.

## Task 2 — a doctrine-voice acknowledgment (staged; only if Task 1 lands clean)

**Why not `ConflictState`/`conflict_of` directly (decision ledger #3):**
that machinery (`domains/language/src/schemas.rs`) operates over
registered, committed facts with two cultures' own dispositions of them.
Routing a falsified prediction through it for real would mean minting a
new committed fact, a new `Observability` row, and probably a new
`FactShape` — real design work, bigger than a staged second task should
absorb; captured instead as LANG-52, a deliberate non-goal here.

**What Task 2 actually does:** extends the *same* Task 1 surface — when a
culture reaching `Predictive` rung also holds an organized doctrine
(`doctrine_of(world, species).is_some()`, the existing SOC-1 gate every
other doctrine-aware render path already checks), the culture's Reckoning
lines carry one further sentence keyed off whether a crisis is currently
live, echoing — thematically, not mechanically — the verifiable ×
authority-serving framing LANG-39 already named as the generative
"Galileo cell":
- **No crisis:** *"None among the {autonym} have shown the priesthood's
  teaching false."*
- **Live crisis:** *"The {autonym}'s own priesthood taught wrongly, and
  could be shown wrong by any who kept their own count."*

Both lines are pure text, gated on already-derived values (`doctrine_of`,
the Task 1 crisis check) — no new fact, no new state, no interaction with
`conflict_of` at all. A folk-only culture (no organized doctrine) never
renders either line, matching every other doctrine-gated render path's
existing convention.

**Testing:** unit tests over the two-line selection (crisis / no-crisis ×
doctrine / no-doctrine, 4 cases) plus one live-world case reusing Task 1's
own found crisis-exhibiting seed, confirming the doctrine line appears
beside the Task 1 margin line when that seed's crisis culture also holds
a doctrine (or documenting, if none of Task 1's found seeds do, a
targeted search for one that does).

## Explicitly out of scope

- **Multiple recurrence classes at once** — stays scoped to the single
  top-observed class `ladder_of` already tracks (ledger #2).
- **A "smarter" revised model** (the real saros cycle is an integer-ratio
  discovery, not a better-fit mean) — captured as **LANG-51**, waits on
  this campaign shipping a real crisis to revise away from.
- **A `ConflictState`-native falsification** (a queryable, Lab-measurable
  conflict entry rather than book-rendered prose) — captured as
  **LANG-52**.
- **Proto-medicine and transmission-as-tradition** (LANG-49's other two
  sub-claims) — the brainstorm's own scope decomposition (ledger #1) found
  proto-medicine has zero substrate (no domain exists) and transmission
  looks already covered by existing institution mechanics; neither is this
  campaign's work.

## Determinism and save-format

No new predicate, no new `Observability` row, no new `FactShape`, no new
stream label — every new function is pure over facts the sky generator and
`observations_of` already produce. The only save-format-adjacent
consequence is the **keystone refreeze** Task 1's own taught-day value
change requires (per The Consonance's own close-time precedent), not a
genesis or ledger change. No epoch bump.

---

## Decisions (promoted from the scratch ledger)

1. **Scope: falsification & revision**, not proto-medicine (no substrate
   exists) or transmission (already covered) — Nathan's direct choice
   among three materially different campaign sizes.
2. **Both surfaces, staged** — the Reckoning-margin correction (Task 1)
   first, the doctrine-voice acknowledgment (Task 2) only if Task 1 lands
   clean — Nathan's direct choice.
3. **A naive mean-interval model with a 5% tolerance, and a
   two-consecutive-miss crisis threshold** — an ideonomy cycle-organon
   pass overturned an initial any-single-miss design in favor of a real
   crisis threshold, matching the Kuhnian cycle both upstream campaigns
   already named.
4. **Task 2 renders thematically, not through `ConflictState`** — a
   scope-conservation call keeping the staged task genuinely small; the
   fuller version is captured as LANG-52.
