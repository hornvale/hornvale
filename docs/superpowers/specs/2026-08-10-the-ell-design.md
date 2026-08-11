# The Ell — design

**Campaign:** The Ell (slug-named, decision 0026) · **Date:** 2026-08-10

An *ell* was a unit of length that meant 45 inches in England and 27 in
Flanders. Cloth measured in one and sold in the other is this campaign's
defect exactly: one name, two meanings, no marker.

## 1. Why

`Fact.day` is a bare `Option<f64>` documented as "the simulated day this fact
was observed". The history bake stamps **years** into it:

```rust
// windows/worldgen/src/history_emit.rs:140
let day = record.core.founded;          // a YEAR, from BakeConfig::start_year
commit_on(hornvale_history::OCC_FOUNDED, Value::Number(record.core.founded), day)?;
```

Nothing catches it, because until 2026-08-10 no second domain carried a
non-zero timeline to disagree with. Six domains stamp `day: Some(0.0)` at
genesis and never touch it again.

**The Particular then added the second timeline, and the collision cost it a
predicate.** Promotion computes `birth = founded − maturity.days()`, mixing a
year with a day count, then filters a death against a present in years. The
result is that `person-died` **cannot be committed by any world**: a death
would need `lifespan − maturity ≤ 5.5 yr` and the narrowest species margin is
39.4. Measured across five seeds: **587 promoted founders, zero deaths.**

It survived because every layer agreed it worked. The predicate is registered
and documented; the domain unit test hand-builds both branches and passes; the
trope probe counts the capability as vocabulary the world holds; and the
live-world test's `if let Some(died)` arm **has never once executed while
staying green**. It was found by scoring a preregistered prediction
*numerically* at the close — the only step that computed a figure the tests did
not already assert.

**The type this needed already exists and the ledger does not use it.**
`kernel/src/field.rs` defines `WorldTime { day: f64 }` — "fractional days
since world genesis" — used **445 times** by fields, phenomena and observers (re-measured on the
absorbed tree 2026-08-11; it was 435 when this spec was drafted).
The fact envelope is the one time-carrying surface in the kernel that opted
out, and it opted out deliberately: decision **0014** ratified that `Fact.day`
stays bare because wrapping it "buys no safety worth the churn."

That premise now has a worked counterexample, which is the standard 0014's own
framing requires for reopening. This campaign supersedes it.

## 2. What changes

Two independent repairs, and the campaign is worth doing only because it makes
both:

### 2a. The ledger speaks days

The history bake may reason in years — a bake naturally does — but **what
crosses into the ledger is days**. Conversion happens at the emit boundary,
which is the same discipline `quantize` already follows: convert where the
value is written, never in the compute path.

Three values move:

- occupation facts' `Fact.day` stamp,
- `occ-founded` / `occ-ended`'s **object** values,
- `history-now`.

**This is an epoch.** Every occupation fact in every saved world changes.
Blast radius measured: **45** `OCC_FOUNDED` references, **18** `history-now`
references.

### 2b. `Fact.day` becomes a time *point*, not a bare float

`Fact.day: Option<f64>` → a typed absolute time point in standard days.

**It must be signed.** The kernel's `Years` is a *duration* and rejects
negatives; a fact's day is a *point on an axis* and legitimately goes negative
— The Particular's founders are born before the history record begins, which
decision 0014's era never had to represent. Conflating duration with time point
is how a "just use `Years`" repair would fail.

Surface measured: **94** write sites (`day: Some(...)` / `day: None`), **197**
read sites.

**Resolved (Nathan, 2026-08-11): enforcing.** `WorldTime`'s field becomes
private behind a validating constructor, so a year cannot be stuffed into a
day-typed slot at all. The alternative — adopting the struct as-is with its
`pub day: f64` — documents the unit without defending it, and this campaign
exists precisely because a documented unit was not enough. The cost is
accepted knowingly: the change reaches all **445** existing `WorldTime` uses
rather than the 94 write sites alone, and that is the larger half of the work.

Two consequences the plan must carry:

- **The constructor's validation is about finiteness, not sign.** A day is a
  point on an axis and goes negative legitimately; rejecting negatives here
  would re-break The Particular's founders in a new way. `Years`'s
  non-negative rule must NOT be copied.
- **A private field means every read site needs an accessor**, so the 197
  readers change shape too. That is mechanical, but it is not small.

### 2c. `founder_handle` is widened, riding this epoch

Added 2026-08-11 at Nathan's direction. It is unrelated to the unit defect and
would not justify an epoch of its own — but this campaign is already paying for
one, so the marginal cost is zero and the window closes when The Ell lands.

**The defect.** `founder_handle` (`domains/history/src/flesh.rs`) keys on
`(people, site, founded, ended, peak_population, FOUNDER_ROLE)` and excludes
the occupation's entity. Two occupations identical in those five collide **by
construction**. The Radiation measured it reaching production — main panicked
on seed 2793 and, on its own branch, on 283 and 705 *inside the census range* —
and resolved it by **dropping** a colliding founder rather than failing
(`07117d05`, an authorized fidelity cut). The consequence changed; the key did
not. Verified on `8c61cbc5`: all three seeds now build clean.

**The repair is already written in this crate, and merely unwired.**
`record::founding_key_from` folds an occupation by `(people, site, founded)`
plus **one hop of ancestry**, deliberately excluding everything after the
founding. Its own doc says it *"feeds the founder role handle behind every
person name"* — **it does not**; its callers are `windows/worldgen/src/descent.rs`
and a test. That doc/reality gap is itself a finding.

Wiring `founder_handle` onto that discipline does three things at once:

- **the ancestry hop discriminates the colliding pairs** — every observed
  collision is a same-day founding-and-flight cascade where one record's ender
  is the other's parent, so the parent's own material facts separate them;
- **it stops a founder's name depending on how their community later died**,
  which `founding_key`'s rationale forbids and `founder_handle` currently does
  through `ended` and `peak_population`;
- **it makes the false doc true.**

**On decision 0051.** The struck paragraph in `flesh.rs` read "`EntityId`-valued
field" as a synonym for "unkeyable"; 0051 forbids keying on an id **as a value**
— a mint counter that shifts when an unrelated domain mints earlier. Folding a
*referent by its own material facts* is not that, and this crate already does it
in three places. **A fact neither 0051 nor that correction accounts for:** The
Signet made ids lineage-derived, so 0051's specific hazard no longer exists at
all. This spec still does not key on the id — the material fold is stronger and
does not depend on that being true — but the prohibition's foundation is worth
recording as gone.

**This changes every founder handle, and therefore every founder's name.** That
is the epoch it rides.

## 3. What does not change

- **`BakeConfig::start_year` / `end_year` stay years.** A history bake reasons
  in years and should. The unit boundary is the ledger, not the domain.
- **Six domains that stamp `day: Some(0.0)`** are mechanically retyped and
  otherwise untouched; zero is zero in any unit.
- **`WorldTime`'s 445 existing uses** keep their meaning — they were already
  days. This campaign makes the ledger agree with them, not the reverse.

## 4. Repairing `person-died` is a consequence, not a step

Once the ledger speaks days, promotion's arithmetic is consistent and
`person-died` becomes reachable with no change to its own logic. That is the
campaign's acceptance test, and it is the honest way to fix a preregistered
quantity after unblinding (decision 0016): the quantity is not retuned, the
unit error beneath it is removed, and the campaign says so in the open.

## 5. Preregistered measurement

Frozen before the code that would move it (decision 0016).

- **E1 — `person-died` becomes reachable.** Today: 0 facts across seeds 42, 7,
  1000, 3, 99 (587 founders). After: **> 0 on at least one seed**, and the
  count must be **less than the founder count** on every seed — a rule that
  fires for *everyone* is as wrong as one that fires for no one, and only the
  two-sided form can tell those apart.
- **E2 — no predicted value for how many die.** Following The Scaffold: a
  measurement with no predicted value still freezes the *definition*, which is
  what makes a later drift check meaningful. Report the number; do not score
  it against a guess.
- **E3 — the vacuous arm executes.** `windows/worldgen/tests/person_promotion.rs`'s
  `if let Some(died)` arm currently never runs while the test stays green.
  After this campaign it must run, and the test must be **shown to fail** if
  the death fact is withheld.
- **E4 — the epoch is contained.** Beyond the retype, the only *values* that
  move are the three named in §2a. If any other committed number changes, that
  is a channel this spec did not know about — a finding to report, not to
  re-pin over.
- **E5 — falsifiable, and the reason to preregister it.** No *non-history*
  domain's committed facts move. Six domains stamp day 0; if one of them
  drifts, the retype was not mechanical and this spec's §3 is wrong.
- **E6 — the widened handle is unique across the cast, and nothing is dropped.**
  Today a colliding founder is silently dropped. After §2c the drop count must
  be **zero** on every seed measured, and handles must be unique across the
  selected cast. Measure over **at least the census range 0-999**, not a
  handful of seeds: The Radiation found its collisions at 283 and 705, which a
  five-seed probe would have missed entirely.
- **E7 — the discrimination is not free, and the null is worth having.**
  `founding_key`'s exclusion of post-founding fields costs discrimination that
  the ancestry hop is supposed to recover — measured stem-collision 8.4/3.3/3.6%
  with ancestry against 27.7/14.8/16.2% without. Report the **founder-name**
  collision rate before and after §2c. If names collide materially more often,
  that is a real cost of the repair and ships as a finding, not as a reason to
  retune the key.

## 6. Testing

- **The unit crossing gets a test that fails when the conversion is dropped**,
  not merely one asserting the current output. Per the repo's mutation rule,
  the test must be *shown* red.
- `cli/tests/id_shift_invariance.rs` and `cli/tests/id_stability_under_insertion.rs`
  must stay green and untouched. This campaign changes when facts are stamped,
  not who they belong to.
- The full artifact regeneration, with every gallery diff read **as prose**.
- `make gate`, `make game-check`, `make vessel-check` — the last two because
  `clients/` is outside the cargo workspace and the gate cannot see it.

## 7. Flagged for the G3 stop

- **This supersedes a ratified decision (0014).** It needs a new decision
  record. The next free number is **0125** — *not* the 0119 this spec was
  drafted against: main renumbered the log while this campaign sat at its
  spec stop, and a `docs_consistency` guard now asserts the numbers form a
  contiguous run from 0001, so the number must be re-derived at the moment
  the record is written rather than carried from here.
- **The `WorldTime` question in §2b** — self-documenting versus enforcing —
  is the one design call this spec deliberately leaves open, because it is the
  difference between a 94-site change and a 445-site one.
- **The collision resolved itself.** `the-radiation` was in flight over three
  fact-committing files this campaign retypes; it has since merged, and this
  branch absorbed main (63 commits, The Radiation and The Grain) before
  planning. `the-docket` remains spec-only and low risk. The general hazard
  stands for the next in-flight campaign: this retype touches a field every
  domain writes, so it collides with almost anything.
- **`Fact.day` is a save-format contract.** The serialized shape must stay a
  bare JSON number, or the epoch is larger than §2a describes.

## 8. Definition of Done

`make gate`, `make game-check`, `make vessel-check` green; artifacts
regenerated and diffed as prose; E1–E5 scored with any falsification stated
plainly; decision 0119 written superseding 0014; chronicle in
`book/src/chronicle/`; retrospective in `docs/retrospectives/` (decision 0020);
registry rows flipped; Confidence Gradient re-scored or an explicit statement
that no bet moved (decision 0030).
