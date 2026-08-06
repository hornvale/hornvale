# Retrospective — The Hollow

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-hollow.md): a cave's three
questions now read three fields instead of one, prevalence went 0.26% → 11.93%
of land, all three kinds and three depth bands occur, and the *existence/depth*
weld survives as the campaign's stated partial null.

## The dominant lesson: a field nothing reads cannot be observed to be wrong

`depth_reach_bands` was **write-only on `main`**. It was computed in the
provider, declared on a public struct, documented, and read by **no code
anywhere in the repository**. `cave.kind` was read in exactly one place — a map
palette — and a map that draws one colour is not obviously drawing one colour.

Three structural defects therefore survived a full campaign, a census metric, an
almanac line, and a committed picture. Not because review was careless, but
because the fields they corrupted had no consumer in front of which to be wrong.
Two of the three (`LavaTube` and `Fracture` unreachable; depth pinned at one
band by an expression whose range exceeded its input's ceiling) are visible by
inspection *once you look at the arithmetic*, and nothing in the ordinary
working of the project ever asked anyone to.

**What to do differently:** when a campaign ships a derived field, name its
first consumer in the same campaign or record explicitly that it has none. A
public field with no reader is not "ready for later" — it is unmeasured, and it
will stay unmeasured for exactly as long as it stays unread. The cheapest
available proxy is a readout battery that prints the field's *distribution*; the
one this campaign built took fifteen seconds and would have found all three
defects on the day the model shipped.

## An inherited diagnosis is a hypothesis

The predecessor campaign's Task 0 measured the substrate honestly and reported
four true numbers — 0.26% prevalence, 100% one kind, 100% one depth, 3/30
worlds empty. It then attributed those outcomes to two defects, and **the
dominant defect was invisible to it**: the presence gate compared a probability
against near-Gaussian noise massed at 0.5, so it operated entirely inside a left
tail and fired a nominal 0.325 at 0.011. That cause cannot be seen from the
outcomes it produces; it can only be seen by re-deriving from the code.

Re-deriving rather than trusting the handoff is what found it — and it is also
what found that the handoff's *scope* claim was false. The handoff asserted "no
consumer, so no artifact moves and no golden moves." There were **three live
consumers** (a census metric, the almanac's deep lines, a map lens colouring by
kind) and **five artifact groups**, plus a census refresh. The load-bearing half
of the ruling survived — world identity genuinely does not move, because the
model is a pure query over already-committed fields — but "free" was wrong, and
one grep established that in under a minute.

**What to do differently:** treat an inherited diagnosis as a hypothesis with a
named falsifier, and re-run the enumeration it rests on rather than the summary
of it. A handoff's *measurements* are usually reliable; its *attributions* and
its *blast radius* are the parts to re-derive.

## Every plan defect this campaign hit originated in plan text

Consistent with the standing record, and worth enumerating honestly rather than
counting.

| # | Defect | Where it was caught |
|---|---|---|
| 1 | Task 1 instructed deleting `cave_kind`, which still had a live caller until Task 4 | implementation |
| 2 | `cave_process`'s type-audit tag omitted the `return` verdict, though the return carries an `f64` | implementation |
| 3 | Task 5's "constants only" restriction was not executable — two of the formulas were structurally wrong | Task 4 readout |
| 4 | The H4 estimator used a bucket midpoint, which is wrong for an exhaustive table over a non-uniform field | Task 5 |
| 5 | Task 5's file list omitted `provider.rs`, which the fix needed for a new accessor | Task 5 |
| 6 | **The uniformity test measured one globe** | Task 2 |
| 7 | Four signature/type errors written from memory rather than from the file | plan self-review |

Defect 6 is the worst and is the one worth carrying forward. The plan's test
sampled a single world's noise field and asserted its deciles against a ±0.035
tolerance. At the gate's spatial frequency a level-5 world holds only about a
hundred independent noise blobs, so a *single* world's own mean wanders
0.4835–0.5237 across sixty-four seeds and its standard deviation 0.0662–0.0854.
Under correct constants the per-world worst decile deviation still has a median
around 0.025 and reaches 0.073 — **so that test fails for roughly a quarter of
seeds however well calibrated the transform is.** It tests the draw, not the
transform.

And it set a trap rather than merely being weak. Seed 42 passes it at 0.0332, a
five per cent margin, on luck. Fitting the constants to seed 42's own mean and
standard deviation drops that to 0.0126. An implementer following the plan's own
"re-fit and re-run" instruction would have been led directly to an over-fit
calibrated to one world, and would have shipped it **green**. The fix was to
change the population, not the threshold: twelve pooled worlds, 122,904 samples,
the tolerance untouched at ±0.035, worst deviation 0.0068.

*One world is an anecdote* and *measure the population you apply to* were both
already standing lessons when that plan was written. The plan cited neither. A
lesson that lives only in a memory file does not reach the document where the
defect is minted.

Defect 7 is the ordinary case and is listed for completeness: `strata::column`
takes seven arguments not five, `Basement::Craton` does not exist, `SoilDepth`
is a newtype not an enum, and the test module already had a fixture builder. All
four came from writing code into a plan from memory instead of from the file,
and all four were caught by reading the file at plan self-review. That step pays
for itself every time.

## Fixing a bug class does not confer immunity to it

The campaign exists to remove two specific shapes of defect: **two requirements
anti-correlated by construction**, and **a ceiling below the threshold that
reads it**. Its own fracture model committed both.

It multiplied competence by `1 - metamorphic_grade`, where metamorphic grade is
*defined* as `1 - hops/OROGEN_REACH` — a decreasing function of exactly the
distance the stress term reads as increasing. Proneness was therefore exactly
zero at `hops = 0`: the most faulted place in the world could host no fault
cave. And its maximum over all land was ~0.393 against the 0.5 threshold that
reads it, so the deepest band was not rare but structurally unreachable.

That is the campaign's own diagnosis, in new code, written by the same process
that wrote the diagnosis. The spec's risk section had named the per-kind
formulas as the likeliest source of iteration; it did not anticipate that they
would fail *in the same way as the code they replace*.

**What to do differently:** when a campaign names a bug class, add a check of
that class against the campaign's own new code as an explicit step. "Does any
new term anti-correlate with another term it multiplies?" and "what is this
quantity's maximum over the real population, against every threshold that reads
it?" are both mechanical questions, and both were answerable before the first
readout.

## Mechanism-first calibration is what kept a post-unblinding change honest

The rule adopted mid-campaign was that a formula could change after results were
seen **only** with a justification true independent of any target. That rule did
two things.

It **licensed** the fracture rewrite. "A fault-void model returning zero
proneness at the fault is wrong" and "a stress term with a floor cannot express
distance from a fault" are both true whatever the readout says, so the change is
a repair rather than a rescue — disclosed in the chronicle, as the project's
rule requires, but legitimate.

It also **declined** settings that would have hit a target. A sweep of 280-plus
parameter combinations does contain a region where Karst reaches its 45% target,
but only at a stress reach that degenerates to the contact cell alone and a
survival exponent of 5 or 6. Neither has a mechanism argument. Both were
refused, and the target was reported as missed.

The missed target then turned out to be **unreachable when it was set**: Karst's
total possible mass is 5.17% of land, and the shipped model realizes 94% of that
ceiling. Setting a target without computing its maximum is the same family as
the standing lesson about a floor with no ceiling, one level up. The mechanism
rule is what stopped the campaign from tuning its way to a number that the
physics could not have produced honestly.

## Two corrections that were corrections, not goalpost moves

Both were disclosed at the moment they were made, and both are worth
distinguishing from the thing they resemble.

**The readout instrument was bucketing on a probability the gate no longer
used.** It read the Karst term while the gate had moved to the *selected* kind's
proneness. The lowest bucket read 0.19972 against a nominal 0.025 — an eightfold
"miscalibration" that was entirely the instrument's, and that no constant could
have moved. The criterion had always been a claim about *the gate's* nominal
probability, so the instrument was wrong relative to the criterion from the
moment the live path was rewired. Fixing the instrument in its own step, before
any calibration ran, is what stopped the calibration step from burning its
attempts on an unwinnable row.

**The bucket estimator moved from midpoint to mean.** The mean is the correct
quantity — for independent Bernoulli trials `E[hits] = Σpᵢ`, so the expected
rate *is* the mean — and the threshold was untouched, but the change was made
after the numbers were seen. What made it reviewable rather than laundered is
that the failing reading was volunteered unprompted, and both readings still
print side by side.

The generalisable form: *a measurement is only as good as its parser*, third
instance in recent memory. When a model's inputs change, ask what the instrument
is keyed on before asking what the instrument says.

## Operational findings

- **The type-audit report moves mid-campaign, not at the artifact step.** Two
  intermediate steps added a `pub fn` and the pre-commit hook reddened on report
  freshness immediately. Adding a plan step for it was considered and declined:
  the gate already prints the exact regeneration command and fails in about nine
  seconds, so a duplicated step is noise that can drift. Knowing it is expected
  is the whole requirement.
- **The gate's input was gapped, not merely non-uniform**, and the baseline run
  is what showed it: the six probability buckets sum to exactly the land count
  and exactly the cave count, so no land cell fell in `[0.05, 0.20)` and none
  reached 0.45. That arithmetic was equally true of the earlier ten-world probe
  and went unnoticed there. **Checking that a bucketed table is exhaustive is a
  five-second arithmetic step that turns a table into a statement about the
  whole population.**
- **The clustering guard was the fragile claim asserted against the robust
  one**, and it is the reason the monotonicity argument is a measured property
  rather than an assertion. It would have failed the campaign rather than being
  explained away. It went up, 96.74% → 98.52%.

## Confidence Gradient

`book/src/open-questions.md` was checked against this campaign's territory.
**No bet moved — N/A.** Its terrain bet is about *coastline shape*: the six
Earth-anchored shape metrics (shoreline development, hypsometric bimodality,
shelf fraction, continent count, largest-continent share, plate-size Gini) and
the shoreline-development band that resolved by superseding its own instrument.
Nothing in that chapter stakes a claim on subsurface features, point processes,
or the calibration of a presence gate. The chapter mentions caves only in the
substring "caveat" and in a title in the influences list.

## Follow-ups

- **The existence/depth weld is the natural successor.** The deepening step
  still reads the same scalar existence is gated on, and the two want opposite
  calibrations. This is the campaign's headline null and is registered as its
  own idea. The underworld campaign will care directly: a chamber graph reads a
  depth budget.
- **The cave and commodity palettes are checked separately and drawn
  together.** Each palette has a within-itself distinctness test; neither
  cross-checks the other, and both render onto the same map. This was
  unguarded because two of the three cave colours had never rendered. Registered
  as its own idea.
- **The point-ore half of the gate defect is untouched**, deliberately, and
  masked rather than fixed by the areal bypass. Registered.
- **The cave taxonomy has no environment-keyed kinds** — no sea, glacial, or
  biogenic voids. Registered.

## Handoff to the underworld campaign (`the-deep-realm`)

Read this before resuming that plan.

1. **A stale idea-registry row will collide.** That branch carries its own copy
   of the cave-model row, with materially different prose (two defects; "no
   consumer, no artifact moves"). Both copies merge *cleanly* into one file as
   **two rows with a duplicate ID** — the uniqueness check catches it, but only
   after the fact. **Drop that branch's copy on resumption; keep `main`'s.**
   This is the class `make preflight` structurally cannot see.
2. **`windows/worldgen/tests/deep_realm_substrate.rs` will not compile.** It
   reads `cave.depth_reach_bands: u32`. The field is now `cave.deepest_band`, a
   `BandKind` (`Regolith`/`Cover`/`Basement`/`Roots`/`Underneath`). This is a
   type change, not a rename — the mapping is not mechanical.
3. **Re-run that campaign's Task 0 gate against the new substrate before its
   plan resumes.** The numbers it stopped on are all superseded: prevalence
   0.2554% → 11.9259% of land, 3/30 caveless worlds → 0/30, one kind → three,
   one depth band → three (Cover 40.03 / Basement 36.12 / Roots 23.85).
4. **Its ten-task plan and eleven-entry decision ledger stand.** Nothing in this
   campaign invalidated its design; it invalidated the reason that design was
   blocked.
