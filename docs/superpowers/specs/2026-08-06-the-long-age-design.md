# The Long Age — design

**Status:** G3 APPROVED (Nathan, 2026-08-06). Planning.
**Date:** 2026-08-06
**Campaign:** C2b of the peoples program
(`2026-08-03-the-peoples-program-design.md`). **Runs after C2-0 (The
Generalist), C2t (The Tolerance) and C2a (The Deep Realm), all merged; blocks
C2c (The Delvers) and C2d (The Radiation).**

Hornvale can say how heavy a creature is, what it eats, how it organizes, and
what weather it tolerates. It cannot say that one lives a long time. Longevity
is not a property here — it is a side effect of mass, and the only long-lived
things in the world are the heavy ones. An elf that lived seven centuries would
have to weigh nine hundred and seventy-six tonnes.

This campaign gives lifespan a way to be authored, and gives one consumer a
reason to notice. It authors no long-lived creature. That is deliberate, and
the null it buys is the result.

## 1. What was measured before anything was designed

The program spec (§2, F1) states its arithmetic and then says plainly: "It has
not been confirmed by a live call; C2b's spec owes that run before it depends
on the numbers." **That run is now made** — a probe over the whole
`biosphere_registry()` calling `hornvale_species::lifespan` directly.

Every stated value reproduces:

```
  claim                                   stated     measured
  --------------------------------------  ---------  ----------
  bugbear, 132.0 kg Endotherm             ~80.9 yr    80.8686
  white/black dragon, 2200.0 kg           ~163.4 yr  163.3962
  gnoll, 136.1 kg Endotherm               ~81.5 yr    81.4893
  mass needed for a 750-yr endotherm      ~977,000kg  976,563
```

A 60 kg endotherm gets 66.4009 yr; a 60 kg ectotherm 99.6014 yr. F1 stands as
written.

The dump also carried four things the source read did not have. Three of them
change this design; the fourth changes the program spec.

### 1.1 There are six peoples, not five

`SocialForm::Settled`, by lifespan: goblin 49.2103, kobold 68.7246, human
69.0098, hobgoblin 70.1636, bugbear 80.8686, gnoll 81.4893.

The program spec's C2b paragraph preregisters "byte-neutrality for the **five**
existing peoples." It is six — human arrived with C2-0 and gnoll with The
Vacancy. The `BiosphereTraits` doc and `LIFESPAN_THRESHOLD_YEARS`'s doc both
already say six. **The null this campaign freezes is over six.**

### 1.2 Lifespan has a second live consumer the program spec never named

The program spec frames C2b entirely around `cascade_regime_of`. It is not the
only reader:

- **`windows/worldgen/src/descent.rs::generation_length_of`** →
  `life_history().generation_length` → `hornvale_history::descent::{remove,
  kinship}` — The Namesake's kinship graph. `descent.rs`'s module doc records
  that seed 42's founding gaps run to "a median of 50 years and a maximum of
  975". Goblin's generation length is ~21.6 yr, so a 50-year gap reads as about
  two removes. A people at 750 years would have a generation length near 330,
  and **every ordinary founding gap would collapse to `Kinship::Sibling`.**
- **`windows/almanac::render_life_history_line`** → the committed gallery
  almanacs, one line per people on three pages, carrying a
  `pace_of_life`-bucketed headline plus rounded lifespan and maturity.
- **`windows/lab`** metrics `lifespan-years-*`, `age-at-maturity-years-*`,
  `generation-length-years-*`, `pace-of-life-*` — committed census columns in
  `book/src/laboratory/generated/the-census/rows.csv` and both summaries.

This matters twice. It widens the null from one function to four artifact
families (§5), and it hands the campaign a **second, continuous** proof that
the axis is visible (§6) — better evidence than a threshold, because nothing
about it can be satisfied by choosing a constant.

### 1.3 `LIFESPAN_THRESHOLD_YEARS`'s justification is stale

Its doc says 120 years is "clear of the wild `Solitary` beasts
(otyugh/xorn/rust-monster/owlbear), which top out around ~110 yr", and
characterises what clears it as "(a dragon)".

Measured: **six non-dragon `Solitary` kinds clear 120** — giant-octopus 131.08,
giant-squid 142.30, giant-scorpion 148.94, rhinoceros 165.22,
giant-constrictor-snake 169.23, giant-crocodile 201.25. The four named beasts
are indeed under 110; they are simply not the whole wild-solitary set. A
rhinoceros draws the frozen-isolate cascade regime today.

Harmless — none is a speaker, so the cascade is banked — but the function this
campaign edits carries a false statement about its own threshold. Correcting it
is a deliverable, not a follow-up.

### 1.4 `cascade_regime_of` computes a lifespan for creatures that have none

`allometry.rs`'s `pace_multiplier` carries the comment "Ametabolic never
reaches the time laws (handled in `life_history`)." True of `life_history`,
which returns `lifespan: None`. **False at the composition root**:
`cascade_regime_of` calls the bare `hornvale_species::lifespan(mass, class)`,
which has no `Ametabolic` arm and returns a number. Measured: xorn (Ametabolic,
55.0 kg) gets **64.9721 yr**.

Under the threshold, so no outcome changes today. But a heavier construct would
freeze its cascade on a lifespan the model says does not exist, and this
campaign is the one that touches both the function and the comment.

## 2. What this is not

- **Not a species.** No kind is authored long-lived. The roster is untouched.
- **Not metamorphosis.** `BIO-21` (metamorphic life-stages as distinct persons)
  is the sibling case the design must *leave room for*, not build. §3.3 states
  the shape it would take and why it is not taken.
- **Not a survivorship curve.** "Curve" is the program spec's word, and the
  honest reading of it — a probability of surviving to age `a` — has no
  consumer anywhere in the workspace. A field nothing reads cannot be seen to
  be wrong (The Hollow; The Deep Realm). Captured as a registry row instead.
- **Not a measurement about elves.** This campaign builds a capability and
  proves the capability is visible. Whether long life actually produces a
  near-frozen family topology is `LANG-53`'s question, and it needs the roster
  C2d authors. Claiming otherwise here would be authoring a result.
- **Not an epoch.** No seeded draw is added; no stream label is created,
  renamed or reused.

## 3. The design

### 3.1 Author an input, never an output

`biosphere_registry` authors **inputs** — `mass`, `metabolic_class`, `potency`,
`social_form`, `condition_niche`. Everything in `LifeHistory` is an **output**:
`lifespan`, `age_at_maturity`, `reproductive_tempo`, `generation_length`,
`pace_of_life`, `basal_metabolic_rate_w`. A `lifespan_override` would invert
the crate's authoring discipline — a stronger objection than the covariation
one the program spec gives, and the reason its instinct was right.

So the channel adds a **third input** and leaves lifespan derived.

Put in the language of clocks: a clock's rate has two independent controls, the
oscillator and the gear train. Kleiber's law is the oscillator, and it should
stay mass-set — a creature that burned slowly would be a *cold* creature, which
is not what longevity means. What is free is the gear ratio: beats per
lifetime. Biology keeps roughly one billion heartbeats across mammals, and its
violators are precisely the long-lived outliers — humans, bats, naked mole
rats. **A long-lived kind is not a slow oscillator; it is a long gear train.**
The authored quantity is therefore dimensionless, multiplies the time laws, and
leaves `basal_metabolic_rate_w` alone.

### 3.2 `LifeSchedule`, carried in a sparse component store

```rust
/// How a kind's time-law quantities are scheduled against its mass.
pub enum LifeSchedule {
    /// Pure allometry: every time-law quantity is f(mass, class). The
    /// default, and what every kind in the roster has today.
    Allometric,
    /// Allometry with an authored dimensionless pace factor. `1.0` is
    /// `Allometric`; above 1.0 is longer-lived and later-maturing at the
    /// same mass, below 1.0 shorter.
    Paced { factor: f64 },
}
```

Carried as a **seventh field on `BiosphereTraits`**, `schedule: LifeSchedule`,
authored explicitly on all thirty rows.

**Amended 2026-08-06, after G3, on evidence from reading the call sites.** The
approved spec said a sparse `life_schedule_registry()` following
`dispersion_registry()`. Three measurements overturned it:

- **`dispersion` is not in `WorldComponents` at all** — zero occurrences in
  `components.rs`. Its store is read directly at its single consumer
  (`disposition.rs`). The precedent is "a separate store for a
  one-consumer component", which does not transfer to an input with six
  consumers.
- **Every consumer of `life_history`/`lifespan` already holds the biosphere
  row.** `render_life_history_line(name, biosphere)` takes the row and nothing
  else; lab's `species_life_history` resolves the row then calls
  `life_history(bio.mass, bio.metabolic_class)`; so do `generation_length_of`
  and `cascade_regime_of`. A separate store would mean threading a second
  parameter into four functions across four crates, none of which has a
  `WorldComponents` in scope.
- **`WorldComponents::from_stores` has ten callers.** A separate store in
  `WorldComponents` adds a twelfth parameter to all ten. Keeping the schedule
  on the row means the synthetic Lab rosters (`goblin-twin`, `serpent`) inherit
  it correctly *by construction*, because they clone biosphere rows — and
  `family_daughters`' own comment explains that reading from the caller's `wc`
  rather than the canonical registry is load-bearing for exactly those kinds.

Cost of the amended shape: **33 struct-literal sites** gain one field (30
registry rows plus 3 in worldgen tests), every one enumerated by the compiler.
That repetition is a *feature* here — thirty explicit
`schedule: LifeSchedule::Allometric` lines are the auditable evidence that no
kind was accidentally made long-lived, which a sparse store's silent absence
could not give.

Cohesion argues the same way: `mass` and `metabolic_class` are the other two
inputs to this law and live on the row. Splitting one input-set across two
stores is worse cohesion, not better.

**The store ships empty.** Not one kind is authored `Paced`. That is §6's
problem to answer, not something to paper over.

`factor` gets a validating constructor (finite, strictly positive), matching
the registry's existing `Mass::new(132.0).unwrap()` idiom, and the type-audit
verdict `bare-ok(ratio: factor)`.

### 3.3 Where metamorphosis would go, and why it does not go there now

`BIO-21` breaks continuous ontogeny: a larva is a different organism, culture
transmission has a hard floor, personhood is a threshold rather than a
birthday. That is a **staged schedule** — in actuarial terms a select-and-
ultimate table, where early life follows its own curve before merging into the
general one.

An enum is the shape that admits it: `Staged { stages: &'static [Stage] }`
becomes a third *variant*, not a fourth *axis*, and no consumer signature
changes. This is the whole reason the channel is an enum rather than a bare
`f64` field.

It is not built, it has no consumer, and `BIO-21` is `raw`. Stating the seam
and declining to build it is the discipline The Deep Realm's retrospective
arrived at: a stated deferral is a different object from an unnoticed dead
branch.

### 3.4 The factor multiplies exactly what `pace_multiplier` multiplies

`pace_multiplier(class)` enters four quantities — `lifespan`,
`age_at_maturity`, `reproductive_tempo`, `pace_of_life` — and deliberately not
`basal_metabolic_rate_w`. The authored factor enters the same four and not the
fifth. One multiplier concept, one seam, four call sites already wired.

The consequence is that `pace_of_life` and `reproductive_tempo`, both clamped
to `[0, 1]`, **saturate at 1.0 for a strongly-paced kind**. This is deliberate
and must be documented at both sites.

The alternative — factor on lifespan and maturity only — was rejected because
the almanac's headline bucket reads `pace_of_life`, so a 750-year people would
render the sentence *"moderate-paced, lifespan ~750 yr"*. Feeding the factor
through fixes the prose and costs nothing, because every existing kind has a
factor of exactly 1.0.

### 3.5 `MAX_PACE_MULTIPLIER` does not move, and its doc is corrected

The constant's doc currently instructs: "if a future class needs a larger
multiplier, this constant — and the ceiling it defines — must move with it,
rather than silently exceeding 1.0 and being masked by the clamp."

Taken literally, an authored factor above 1.5 requires raising the ceiling —
which rescales `pace_of_life` for **all thirty kinds**, moving the
`pace-of-life-*` census columns and all six almanac headline buckets. That
forfeits the null this campaign exists to establish and forces a census regen.

The doc's actual requirement is that saturation not be **silent**. So: the
constant stays at 1.5, is redocumented as the ceiling of the **class**
component only, and the authored factor's saturation of `pace_of_life` is
stated explicitly at the constant, at `pace_of_life`, and at `LifeSchedule`.

This is a judgment call about a doc-stated invariant and leads §8.

### 3.6 `cascade_regime_of`'s `Settled` arm learns to read lifespan

```rust
SocialForm::Settled => {
    if lifespan_of(bio, schedule) >= LIFESPAN_THRESHOLD_YEARS {
        CascadeRegime::new(1, 2)
    } else {
        CascadeRegime::SETTLED
    }
}
```

**One threshold, both arms.** Decision 0066 states the model as a single
product — "sociality × (1 / lifespan)" — so one lifespan threshold serving both
rows is the faithful reading, and the `Settled` row ceasing to be constant in
lifespan is exactly what 0066 already claims to ship.

**Banding is forced by the type, not chosen.** `CascadeRegime { min, max }` is
a pair of integers, so the codomain is discrete however the map is written. A
"continuous" model would quantize to a step function anyway; the only real
question is where the step sits.

**`(1, 2)` is the right rung** on 0066's own transmission-count argument. A
settled long-lived people has many speakers but roughly fifteen times slower
turnover than goblin; a dragon has one speaker at a sixth of goblin's turnover.
So it belongs between `SETTLED (2,4)` and the frozen isolate `(0,1)`. It
coincides with the `Gregarious` rung, which is fine — a regime is a rate, and
two causes landing on one rate is honest.

**Byte-neutral by arithmetic**: the six peoples top out at 81.4893, clear of
120 by 38 years.

### 3.7 Two stale statements fixed in passing

- `LIFESPAN_THRESHOLD_YEARS`'s doc gains the true list of what clears it
  (§1.3). No behaviour change.
- `pace_multiplier`'s "Ametabolic never reaches the time laws" comment is
  corrected, and `cascade_regime_of` stops asking an `Ametabolic` kind for a
  lifespan it does not have (§1.4). **This is a behaviour change in principle
  and a no-op in fact** — xorn is the only `Ametabolic` kind and reads 64.97,
  under the threshold either way — so the regime it resolves to is unchanged.
  It must still be asserted, not assumed.

## 4. Blast radius

The compiler is the enumeration, not this list. Written as orientation only —
**the implementer must let `-D warnings` finish the sweep and must never
silence an exhaustiveness or arity error with a wildcard or a stub.**

- `domains/species/src/allometry.rs` — the four time laws take a schedule.
- `domains/species/src/lib.rs` — `LifeSchedule`, the seventh `BiosphereTraits`
  field, 30 registry rows, re-exports.
- `windows/worldgen/src/lib.rs` — `cascade_regime_of` (signature unchanged —
  it already takes the row), plus 3 `BiosphereTraits` literals in tests.
- `windows/worldgen/src/descent.rs` — `generation_length_of`.
- `windows/almanac/src/lib.rs` — `render_life_history_line`.
- `windows/lab/src/metrics.rs` — the six life-history extractors.
- `domains/species/tests/coverage.rs`, `windows/worldgen/tests/*`.

Two assertion sites make the old invariant explicit and must be *widened*
rather than merely kept passing:

- `cascade_regime_of_matches_the_authored_regime_map`
  (`windows/worldgen/src/lib.rs`) asserts all six peoples resolve to `SETTLED`.
  Still true — they are all under 120 — so it must keep passing untouched, and
  gain the new long-lived `Settled` case beside it.
- The golden `solitary-tongue-peoples-lexicons-seed-42.txt`'s failure message
  tells a future reader that a drift means "`cascade_regime_of` is no longer
  resolving **every** `Settled` people to `CascadeRegime::SETTLED` … this is a
  BUG — do not rebaseline." After this campaign that sentence is true only for
  peoples *under the threshold*. The message must say so, or it will one day
  send C2d's implementer to diagnose a bug that is the model working.

Signature churn is the real cost: `lifespan(mass, class)` and
`life_history(mass, class)` gain a schedule parameter, and every call site is
a compile error until it passes one. That is the desired failure mode.

**The type-audit stale-tag footgun applies directly here.**
`tools/type-audit/CLAUDE.md` records that a pub-boundary signature change makes
existing verdict tags go stale by position, that this is invisible to unit
tests *and* to the gallery/reference/laboratory drift check — because the
report lives in `docs/audits/`, which that subset excludes — and that a commit
which skipped the regeneration "briefly left `main` gate-failing". Three of the
four functions being resignatured carry `bare-ok(ratio: return)` tags. The
mitigation is already in the Definition of Done: run the full `make gate`, and
include `docs/audits/` in the diff sweep.

## 5. Preregistration — the null IS the result

Frozen 2026-08-06, before any implementation code. Decision 0016.

**P1 — no world changes.** The committed seed-42 world JSON is byte-identical.
Every gallery almanac is byte-identical, including all six life-history lines.
Both census fixtures are byte-identical (31 rows `the-census`, 3 rows
`census-of-the-meeting`) — **so no census regen is required, and none is
requested.** The stream manifest is byte-identical: no label is added, renamed
or reused.

**P2 — the six peoples' derived life-history is bit-identical**, all six
`LifeHistory` fields, compared by `f64::to_bits` and not by epsilon. Asserted
over all thirty kinds, not only the peoples.

The mechanism P2 relies on: a factor of exactly `1.0` folded into these
expressions is an IEEE-754 no-op. **Verified before writing this line**, over
the roster's thirty real masses and both class multipliers — bit-difference
`0` for both the `pace_of_life` form and the `lifespan` form. This is the
whole reason the null is cheap, so it is checked rather than assumed.

**P3 — exactly one committed artifact moves:**
`docs/audits/type-audit-report.md`, tracking the new public surface. If
anything else moves, P1 is false.

Made concrete against the report's actual format, which is aggregate counts by
class and by crate rather than a per-item listing: before this campaign it read
`bare-ok(ratio) | 538` and `species | 45 | 0 | 0 | 45`. `LifeSchedule` itself
is an enum and takes no verdict — the convention `SocialForm`'s doc already
states. **A diff wider than that one file is a signal, not a formality**: it
would mean a signature change dropped a tag.

> **Correction, made after Task 1 and stated rather than silently applied.**
> This paragraph originally predicted those two rows incrementing by **one**,
> on the reasoning that `LifeSchedule::Paced::factor` is the only new bare
> primitive. That was wrong, and wrong *within this document* — §3.2's own
> code carries three tags, not one: the field, `paced`'s `factor` parameter,
> and `factor()`'s return. Measured after Task 1: `bare-ok(ratio)` 538 → **541**
> and `species` 45 → **48**.
>
> **P3's claim is unchanged and held**: exactly one committed artifact moved,
> and it was `docs/audits/type-audit-report.md`. Only the illustrative count
> was wrong. It is corrected here in the open because revising a preregistered
> number after seeing the result is precisely the move this project forbids
> doing quietly — the claim that carries the null is "one file", never "+1".

**What the null proves, and what it does not.** It proves the channel is inert
in the absence of an occupant — which is what makes the epoch cheap for C2c and
C2d, and it is the *only* claim this campaign gets to make about worlds. It
proves nothing about longevity's effects, because nothing is long-lived yet.

**Falsification.** If P1 fails — if any world's identity, almanac, or census
fixture moves — then the program's "cheap epoch" premise is wrong, and C2c and
C2d must each budget an epoch and a census regen. That is a finding that
changes two later campaigns' scope, and it is reported as the headline rather
than repaired quietly.

## 6. The axis must be shown to be visible — rung 4, not rung 2

The program's single shared acceptance criterion (§3 of the metaplan): a green
test proves the code ran; only a **mutation** proves the axis is visible. With
zero authored occupants this campaign sits squarely in the rung-2 trap, and
answers it with two mutations rather than an occupant.

Both use a synthetic `BiosphereTraits` built in the test. `cascade_regime_of`
is pure over its arguments, so no roster change is needed.

**M1 — the discrete proof.** A `Settled` kind with a `Paced` schedule clearing
120 years resolves to `CascadeRegime::new(1, 2)`, and one at the default
resolves to `SETTLED`.
*The mutation that must redden it:* **delete the lifespan consultation from the
`Settled` arm** — restoring `Settled => CascadeRegime::SETTLED`. Not perturbing
the threshold's value, and not perturbing the derivation. A mutation proves
only what it perturbs, and the claim here is *the arm reads lifespan at all*.

**M2 — the continuous proof.** Over seed 42's median founding gap of 50 years,
an `Allometric` people reads `Kinship::Ancestor(n)` with `n ≥ 1`, and the same
people under a `Paced` schedule long enough to push generation length past 100
years reads `Kinship::Sibling`.
*The mutation that must redden it:* **drop the schedule on the path from
`generation_length_of` into `life_history`**, so the authored factor never
reaches the arithmetic.

M2 is the stronger evidence: its outcome is not a constant anyone chose, and it
demonstrates the axis reaching a consumer the program spec had not identified.

### 6.1 Amendment — M2 as specified was not achievable, and why that is itself the finding

**Recorded after Task 4 ran it and it stayed green.** M2's mutation — dropping
the schedule inside `generation_length_of` — does **not** redden any test, and
cannot.

`generation_length_of` resolves its biosphere row from
`WorldComponents::assemble()`, the canonical registry. Decision D6 ships the
channel with **zero occupants**, so every row that function can ever reach has
`schedule == Allometric`, which is bit-identical to the `ALLOMETRIC` the
mutation substitutes. The mutation is structurally unobservable.

**This is D6 and §6 in direct tension, and it generalises**: a consumer that
looks its row up *by name from the canonical registry* cannot be
mutation-proven to read an authored channel that nothing authors yet. Only a
consumer that takes a `&BiosphereTraits` **directly** can, because a test can
hand it a cloned-and-paced row. That splits this campaign's four consumers
cleanly:

```
  consumer                      takes            mutation-provable today?
  ----------------------------  ---------------  ------------------------
  cascade_regime_of             &BiosphereTraits  YES  (M1, reddened)
  render_life_history_line      &BiosphereTraits  YES  (M2, reddened)
  generation_length_of          species: &str     NO   (registry-locked)
  species_life_history          species: &str     NO   (registry-locked)
```

So M2 is **re-sited onto `render_life_history_line`**, which is a fair trade
and arguably a better one: that function writes the life-history line of every
committed gallery almanac, so reddening it proves the channel can move a
*committed artifact*, not merely an in-memory value.

What remains true of the kinship result: the arithmetic still differentiates —
at goblin's ~21.7-year generation length seed 42's median 50-year founding gap
reads `Ancestor(n≥1)`, and at eleven times that pace the same gap reads
`Sibling`. The test asserts both, and drives the short-lived half through the
real `generation_length_of`. But **that `generation_length_of` forwards
`bio.schedule` rather than a hardcoded default is a code-reading argument, not
an assertion**, and it is disclosed as such at the test. This follows The Deep
Realm's precedent, where the vessel's budget pass-through was likewise argued
rather than asserted and said so out loud.

**The first campaign to author a `Paced` kind — C2c — closes this gap for
free**, and should be told to: routing that kind through `generation_length_of`
turns a code-reading argument into an assertion, and is the cheapest way to
retire it.

**Both mutations must be run, both halves confirmed RED, then reverted and
confirmed green** — the template The Deep Realm's H3 established.

## 7. Definition of done

- All of §3, with the two stale statements of §3.7 corrected.
- §5's P1/P2/P3 asserted, with the artifact diff run and shown.
- §6's two mutations run, reddened, reverted, and their output recorded.
- `make gate` green; `make rebaseline` and a `git diff --exit-code` over
  `book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/`,
  expecting exactly the one file of P3.
- Chronicle entry, book freshness sweep, Confidence Gradient re-score check,
  retrospective. The program spec's C2b paragraph corrected from five peoples
  to six (§1.1).
- Registry rows for the captured discards (§9).

## 8. Flagged for review

1. **§3.5 — `MAX_PACE_MULTIPLIER` is deliberately not raised, against the
   literal reading of its own doc.** The doc says the ceiling "must move with"
   a larger multiplier. This spec argues the requirement is that saturation not
   be *silent*, and satisfies it by documenting rather than rescaling — because
   rescaling moves two census columns and six almanac lines for all thirty
   kinds. This is the one place the design knowingly reinterprets a
   doc-stated invariant, and it is the call most worth overruling if the
   reading is wrong.
2. **§3.6 — one threshold serves both arms.** Reusing 120 for `Settled` is
   argued from decision 0066's single-product wording, not from measurement.
   It is byte-neutral by 38 years of margin, so the risk is modelling taste
   rather than drift.
3. **§3.4 — `reproductive_tempo` and `pace_of_life` saturate.** A very
   long-lived kind and a merely long-lived one will read the same 1.0. No
   consumer distinguishes them today; C2c and C2d will have opinions.
4. **§1.4 — a behaviour change asserted to be a no-op.** Removing the
   `Ametabolic` lifespan call is a real change to what the code computes; it
   is a no-op only because xorn happens to sit under the threshold.
5. **Not a fidelity cut, and no census regen requested** — flagged as an
   absence so the carve-out is visibly considered rather than skipped.

## 9. Capture manifest

To the idea registry (`book/src/frontier/idea-registry.md`):

- **`BIO-survivorship-curve`** (new) — lifespan as a survivorship function
  rather than a maximum; the honest reading of "curve", with no consumer
  today. Rejected here for exactly that reason.
- **`BIO-senescence-onset`** (new) — the age at which decline begins, distinct
  from maximum lifespan; the pair a real life table needs.
- **`BIO-21`** (existing) — cross-link to this spec's §3.3 as the seam the
  enum leaves open.
- **`BIO-three-probes`** (existing) — the elf cell moves from *inexpressible
  and unread* to *expressible and read*; the row's status text is corrected on
  merge. Its status is self-reported, so it is corrected against measurement,
  not against its own prose.
- **`LANG-53`** (existing) — cross-link: the near-frozen family topology it
  waits on becomes reachable once a kind is authored `Paced`, which is C2d.

Recorded for C2c rather than fixed here, from The Deep Realm's handoff:

- **Per-stratum occupancy did not ship.** The program spec's C2a paragraph
  promises a cell's population keyed `(cell, stratum)`; decision 0105
  superseded that with a chamber graph, and nothing places anyone in a chamber.
  C2c is "first people to live in the Deep Realm" and has no occupancy
  mechanism to build on.
- **The C2a paragraph is stale prose** — it describes bands and a subterranean
  supply field; 0105 replaced bands with a graph, and supply stays surface-fed
  by design. Correcting it stops C2c's spec inheriting a false picture. Done as
  part of this campaign's §1.1 correction to the same document, since both are
  edits to the program spec.

## 10. Decisions

Promoted from the autopilot ledger on G3 approval (Nathan, 2026-08-06). The
scratch ledger dies with the worktree; this is the durable record.

**D1 — Author an input, never override the output.** The channel adds a third
authored input and leaves `lifespan` derived. `biosphere_registry` authors
inputs (`mass`, `metabolic_class`, `potency`, `social_form`,
`condition_niche`) and derives outputs; a `lifespan_override` would invert the
crate's own authoring discipline. *Discarded:* a scalar override; a new
`MetabolicClass` variant (a clade tag is not a longevity strategy, and it is an
enum widening with 7+ exhaustive-match sites for no gain).

**D2 — A seventh field on `BiosphereTraits`, not a separate store.**
**AMENDED after G3** (see §3.2 for the evidence). The original decision chose a
sparse `life_schedule_registry()` on the `dispersion_registry()` precedent;
reading the call sites showed that precedent does not transfer — `dispersion`
is not in `WorldComponents` at all and has one consumer, whereas the schedule
has six, every one of which already holds the biosphere row. *Discarded:* the
sparse store — it would add a twelfth parameter to `from_stores`' ten callers
and thread a second argument into four functions across four crates, and Lab's
synthetic rosters would no longer inherit the schedule by construction.

**D3 — The factor multiplies exactly what `pace_multiplier` multiplies.**
`lifespan`, `age_at_maturity`, `reproductive_tempo`, `pace_of_life`; **not**
`basal_metabolic_rate_w`. Metabolic rate is genuinely mass-set — a long-lived
kind is not a cold kind. *Discarded:* lifespan and maturity only, which leaves
the almanac emitting "moderate-paced, lifespan ~750 yr" because the headline
bucket reads `pace_of_life`.

**D4 — `MAX_PACE_MULTIPLIER` stays at 1.5 and is redocumented.** See §3.5.
Its doc's requirement is that saturation not be *silent*; documenting satisfies
that without rescaling `pace_of_life` for all thirty kinds. *Discarded:*
raising the ceiling (census regen, null forfeited); leaving the doc untouched
(a false instruction left in the code).

**D5 — One threshold serves both arms.** `Settled` + lifespan ≥
`LIFESPAN_THRESHOLD_YEARS` → `CascadeRegime::new(1, 2)`, else `SETTLED`.
Decision 0066 states the model as one product, and `CascadeRegime{min,max}` is
a pair of integers, so the codomain is discrete and banding is forced by the
type rather than chosen. *Discarded:* a second `Settled`-specific threshold; a
continuous map (it would quantize to the same step).

**D6 — No kind is authored long-lived.** `Paced` ships with zero occupants;
C2c (dwarves) is its first consumer, C2d (elves) its second. The Deep Realm's
retrospective sets the rule: name a derived thing's first consumer in the same
campaign, *or say plainly that it has none*. Authoring one here would move the
roster, the census fixtures and world identity — destroying the null that is
this campaign's result. The rung-2 trap is answered by §6's two mutations
instead.

**D7 — No new lab metric.** One metric reddens 34 tests until both census
fixtures refresh (31 `the-census` + 3 `census-of-the-meeting`), and with zero
occupants it would read a constant. Deferred to C2c, which will have something
worth measuring.
