# The Convalescence — recovery is the discriminator

**Campaign:** The Convalescence
**Date:** 2026-07-26
**Status:** draft (G3)
**Parent:** `2026-07-19-the-temperament-design.md` §8 — the spec that defines the
population health family. This campaign restores the alarm that spec specifies.
**Decisions in force:** 0016 (studies preregister their hypotheses), 0011
(studies are data, metrics are code), 0021.
**Occasioned by, but NOT downstream of:** The Action Clock, parked at G6 on this
metric's null control (`docs/superpowers/specs/2026-07-26-the-action-clock-design.md`
§10a). See §7 — this campaign must stand on its own evidence or it is
seed-shopping with extra steps.

## 1. The finding

`windows/lab/tests/health_calibration.rs`'s null control asserts

```rust
assert_eq!(a.chronicity, 0.0, "healthy world: no one chronically stuck");
```

The Temperament §8, which defines the metric, specifies the bug signal
differently — and says so twice:

> **The temporal signature disambiguates a hard world from a broken sim:** a
> spike that *recovers* (short half-life) is a novel/extreme world event (a
> frost, a drought) the creatures adapt to — legitimate; a spike that *persists*
> (no recovery, elevated chronicity) is a bug.

> an injected unsatisfiable need spikes and *persists* (elevated chronicity, no
> recovery — the bug signal).

**The bug signal is the conjunction, and recovery is the discriminator.** The
assertion checks only the first conjunct, so it fires on an episode that
recovered — which the spec explicitly calls legitimate. The test's own comment
states the conjunction correctly; only its assertion narrowed.

This drift is independent of any campaign. It misfires on *any* change that
shifts the phase of a creature's distress rhythms.

**The worked case** (seed 42, from The Action Clock's investigation): one
creature carries two independent distress rhythms — thirst producing four-tick
`Helpless` blocks, fatigue producing a periodic `Frustrated`. A one-tick phase
shift moved a `Frustrated` into the gap between two `Helpless` blocks, welding
`4 + 1 + 4` into a nine-tick run:

```
  before   seeeeeFeeeeFeFFeHHHHeHHHHeeeFeeeeeeFeeee
  after    seeeeeFeeeeeeeFeHHHHFHHHHeeeFeeeeeeeFeee
                                ^  e -> F
```

The episode **recovered** (`recovery_ticks = Some(2.586)`) and the world's
overall prevalence *fell*. Under the spec that is a hard patch, not a bug.

## 2. What is NOT wrong

**The run counter is correct as specified.** The Temperament defines chronicity
as "helpless/frustrated ≥ N ticks" — *labels*, not causes — so counting
consecutive distress regardless of which drive caused it is the specified
behaviour. A creature alternating between two distresses for nine ticks genuinely
never returned to health during them.

So this campaign does **not** make the run counter cause-aware. That would be
inventing a metric the spec does not describe, in order to make a red test green
— the exact move decision 0016 forbids. What was wrong is only *which quantity
the alarm reads*.

## 3. The hole in the obvious fix

Simply conjoining the two published numbers would be worse than the status quo:

`chronicity` is **per-creature** (the fraction of creatures with a long run).
`recovery_ticks` is a **population aggregate** (the mean length of recovered
spikes across everyone). Conjoining them at population scope is a category
mismatch, and it fails in the dangerous direction:

> One genuinely stuck creature that never recovers, plus nine that do →
> `chronicity = 0.1`, `recovery_ticks = Some(…)` → the conjunction reads
> "not a bug" → **the stuck creature is masked.**

That trades a false positive for a false negative. A bug alarm may be noisy; it
may not be silent.

## 4. The design

A distress episode has two independent properties — its **length** and its
**fate** — and the four cases are not equally meaningful:

```
                  recovered                still open at trace end
  short (<N)      a blip. normal.          CENSORED — undecidable
  long  (>=N)     a HARD PATCH.            THE BUG SIGNAL
                  legitimate per §8
```

**Split the measure and move the alarm.**

- **`chronicity`** — fraction of creatures with a run of at least
  `CHRONIC_TICKS`. Unchanged in computation, and **demoted to diagnostic**:
  reported, no longer bounded by the null control.
- **`stuck`** *(new)* — fraction of creatures with a run of at least
  `CHRONIC_TICKS` **that never ended**. This is the spec's conjunction evaluated
  **per creature**, and it is the alarm.

The null control then asserts `stuck == 0.0` and keeps `recovery_ticks.is_some()`.

`health.rs` already holds the concept — its own comment reads *"A run still open
at the end never recovered — it is chronic, not a recovered spike"* — so the
distinction exists in the code and is simply not surfaced.

**The censored cell is deliberately not an alarm.** A short run still open when
the trace ends might have recovered one tick later; that is right-censoring and
it is undecidable from the trace. Only *long and open* alarms. Naming this
prevents a future reader from "fixing" the apparent asymmetry.

### 4.1 The precedent this follows exactly

This is the second time a member of this family has been demoted from bound to
diagnostic, for the same reason and citing the same spec. The-living-community
removed the `prevalence < 0.02` bound, and the comment it left says why:

> Loosening the number to pass would be the seed-shopping ADR-0016 forbids;
> instead we anchor on the metric's actual PHILOSOPHY … Instantaneous prevalence
> is not the alarm and is no longer bounded here.

`prevalence` was never the alarm; neither is `chronicity`. This campaign finishes
the move that one began, and reaches the quantity §8 actually named.

## 5. Blast radius

Verified by grep rather than assumed: **no lab study reads the health family**,
so there is no census schema change, no census regeneration, no committed CSV,
and no CLI consumer. `HealthReport` is read only by
`windows/lab/src/synthetic.rs`, `windows/lab/tests/health_calibration.rs`, and
prose in `book/src/chronicle/the-temperament.md` and the idea registry.

**No world changes.** The health family is a *read* over affect traces — a View,
in The Temperament's own words. Adding a field to a report changes no genesis, no
ledger, no seed draw and no artifact. The possession galleries cannot move.

## 6. Acceptance

Preregistered before implementation (decision 0016):

- **The alarm still fires on a real bug.** `synthetic.rs`'s injected
  unsatisfiable-need scenario — a creature that never recovers — must read
  `stuck > 0`. If splitting the measure silences the scenario the metric exists
  to catch, the split is wrong.
- **The alarm is silent on a recovering hard patch.** A planted trace of
  `HHHH F HHHH` followed by recovery reads `chronicity > 0` **and**
  `stuck == 0` — the worked case of §1, as a unit test on planted labels rather
  than on a whole world.
- **The masking case is caught.** A population of one never-recovering creature
  plus nine recovering ones reads `stuck > 0`. This is the §3 failure mode, and
  it is the test that proves the per-creature form was necessary.
- **Censoring does not alarm.** A short run still open at the trace end reads
  `stuck == 0`.
- **The null control passes on `main` as it stands** — `stuck == 0.0` on all
  five seeds, with `chronicity` reported and unbounded.
- **The five-seed health family is re-baselined** and recorded, since
  `HealthReport` gains a field.

## 7. The trap this campaign must not fall into

This work was occasioned by a campaign that a red null control had blocked. That
makes it structurally identical to seed-shopping unless it is held to a higher
standard, so:

**The fix must be justified, implemented and validated entirely on `main`,
against the metric's own specification and its own synthetic scenarios — never
against The Action Clock's numbers.** The Action Clock is not mentioned in any
test, is not a success criterion, and its branch is not consulted. If this
campaign cannot stand on The Temperament §8 plus `synthetic.rs` alone, it is not
a fix and should not land.

That The Action Clock is unblocked afterwards is a *consequence*, verified last
and separately, not a goal.

## 8. Scope

**In:** the `stuck` field and its per-creature computation; the null control's
assertion moved to it; `chronicity` demoted to diagnostic with its comment
updated; the four acceptance tests; the re-baseline; the book prose that
describes the alarm.

**Out:**
- **Making the run counter cause-aware** (§2) — not what the spec describes.
- **Naming *which* creature is stuck.** A bug alarm that says "one in ten" is
  less useful than one that says "the rust-monster"; `by_species` is the nearest
  existing diagnostic. Genuinely useful, genuinely out of scope, and it belongs
  with whoever next needs to debug a red alarm.
- Any change to `CHRONIC_TICKS`, whose value is not implicated.

## 9. Flagged for G3

1. **[process — leads this list] This campaign is occasioned by a blocked
   campaign**, which is exactly the shape of rationalised seed-shopping. §7 is
   the mitigation: justified from The Temperament §8, validated on `main`, The
   Action Clock never consulted. Confirm that discipline is sufficient, because
   it is the whole reason to trust the result.
2. **[contract] `HealthReport` gains a public field.** Small blast radius (§5),
   no census and no world change, but it is a published metric's shape.
3. **[naming] `chronicity` keeps its name while ceasing to be the alarm.**
   The alternative is renaming it to something like `long_episodes` and freeing
   the word — clearer, but it churns the prose in two book chapters and the
   registry. Recommended: keep the name, change the meaning of the *bound*, as
   was done for `prevalence`. Confirm.
4. **[design] The censored cell does not alarm** (§4). A short-and-open run is
   undecidable from the trace; long-and-open alarms. Stated so a later reader
   does not "fix" the asymmetry.

---

## Addendum A — §8 is internally inconsistent (2026-07-26, after approval)

*Appended after this spec was approved at G3, during review of the campaign's
closing documentation. The approved text above is unchanged and stands as
written, §7 included.*

§1 says The Temperament §8 "specifies the bug signal differently — and says so
twice." That is incomplete. §8 also *labels* `chronicity` the alarm, in its
family bullet list (`2026-07-19-the-temperament-design.md` lines 189-191):

> - **chronicity** — fraction *persistently* stuck (helpless/frustrated ≥ N
>   ticks). **The bug alarm.**

Against that bullet the null control matched its mandate exactly. So §8 is
**internally inconsistent** rather than merely under-read: its bullet-list label
names one quantity the alarm, while its discriminator paragraph (lines 197-202)
and its evidence battery (lines 259-263) both state the signal as a conjunction.

This campaign resolves the contradiction toward §8's **reasoning** — the
discriminator paragraph and the battery, which give the reason recovery is what
separates a hard world from a broken sim — rather than toward its one-line label,
which gives none.

The design is unchanged by this: `stuck`, the demotion of `chronicity`, the
censored cell and the acceptance list are all as specified above. What this
addendum corrects is the account of *what was wrong*. Recorded here and in
decision 0080 rather than by editing approved text.
