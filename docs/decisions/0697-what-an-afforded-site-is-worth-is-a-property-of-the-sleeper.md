# 0697. What an afforded site is worth is a property of the sleeper's species

**Status:** Accepted (2026-09-03) · **Decider:** Nathan (autopilot) ·
**Campaign:** The Pallet · **Relates:**
[0558](0558-sleep-is-never-gated-the-place-grades-it.md) (the grade this
refines), [0586](0586-every-authored-constant-declares-its-axis-of-variation.md)
(the ladder whose `per-species` rung this is), [0069](0069-fine-position-is-never-serialized.md)
(the ceiling the grade still sits under)

## Context

[0558](0558-sleep-is-never-gated-the-place-grades-it.md) ruled that sleep is
never gated and the place a body sleeps *grades* it. The Wicket built that as
one authored constant, `AFFORDED_REST_GAIN = 1.5`, multiplying every creature's
recovery rate identically — and tagged it `universal(a uniform multiplier on
every rest/sleep act's own rate, bounded rather than derived — not a species
property)`.

That verdict is wrong in exactly the shape
[0586](0586-every-authored-constant-declares-its-axis-of-variation.md) names:
"bounded rather than derived" answers *where the number came from* and "not a
species property" is a negation, both of them answers to a neighbouring
question. A `xorn` is ametabolic living stone and gains nothing at all from a
bed. The quantity varies by species.

## Decision

`AFFORDED_REST_GAIN` becomes `hornvale_species::sleep_grade_registry()` — a
total, ratcheted `ComponentStore<KindId, f64>` over the biosphere roster, 39
rows carrying **7 distinct values**, derived from a **two-axis model** over
traits the roster already states:

- **Insulation** — a body lying on a surface loses heat into it, and only a
  `ThermalStrategy::Endothermic` body pays that bill.
- **Fit** — a made bed is made by, and for, the body that made it, and
  `SocialForm::Settled` is this model's sole settlement-forming value.

The ladder descends: both halves (1.50, the fourteen settled peoples),
insulation only (1.35), fit only (1.30), contact alone (1.20), too large to fit
at a tonne and above (1.15), already buoyed (1.05), and no gain at all (1.00)
for the ametabolic and the sessile.

**`1.50` is the CEILING, not the midpoint.** It is the exact value the old
constant carried, and that constant's own calibration argument — half again is
the plainest reading of "prefer" that a body sleeping in the road can still live
with, where `2.0` would make a bed a necessity — is an argument about the
**most** a site may be worth. So the table descends from it and never exceeds
it: **no kind in any world gains more from a bed than it did before this table
existed**, and the peoples the calibration was actually authored for keep their
number byte for byte.

**`1.00` is the floor and there is nothing below it.** A value under 1.00 would
make an afforded room repay *less* than open ground, turning the grade from a
preference into a penalty — the inverse framing 0558's own implementation
records as rejected.

## Scope — what this decision does not decide

**The SITE stays binary.** `SiteGrade` is still `{ Bare, Afforded }`: a bed and
a heap of bracken are one value to the fold. *Which* thing a body prefers is the
`per-people` rung, and it needs kind-to-kind edges the object registry does not
have. The idiosyncratic *this one likes a sleeping bag* is the `per-individual`
rung and needs `Lineage`-derived values. Both are **declared** — in
`SiteGrade`'s own doc, because neither has a constant to hang a `plumb:` tag on
(both are the absence of a number, and inventing a constant to carry a tag is
the worse error) — and neither is built.

**Threat stays out of the grade.** Preference is innate; safety is situational.
A body knows by instinct that a bed out-rests a floor, and cannot know whether a
given site is *safe* without being there. A later campaign supplies threat as a
separate term.

## Consequences

- **The derivation rule lives in prose and nothing ties a row to the traits it
  cites.** Raising `owlbear`'s mass to 1500 kg leaves it on `INSULATION_ONLY`
  where the stated rule says `TOO_LARGE_TO_FIT`, and only an unrelated
  life-history golden objects. The coverage ratchet enforces **presence**, never
  correctness — the same shape `fatigue_rise_registry` already has, deliberately
  followed. The tonne threshold is inclusive and lands on `giant-crocodile`'s
  exact mass, 1000.0, so that row is one edit from flipping with nothing to say
  so.
- **`DEFAULT_SLEEP_GRADE` is documented as human's row and nothing pins the
  equality.** The same unpinned claim `DEFAULT_FATIGUE_RISE` carries beside it;
  re-authoring `MADE_FOR_THE_BODY` would silently falsify the doc.
- **`SleepTraits` replaced two adjacent `f64` parameters.** `rate` and
  `afforded_gain` are both bare ratios, adjacent, and each plausible in the
  other's slot, so a transposition type-checked. Named fields cannot be
  transposed, and the struct keeps `fatigue_with_pending` under
  `clippy::too_many_arguments`.
- **The affect-trace byte-golden is BLIND to this table**, and that null needed
  four controls to establish. Exaggerating the peoples' row to 4.00, the wild
  endotherms' row to 4.00, and **all seven rungs at once** to 4.00 each left the
  fixture byte-identical; changing `SiteGrade::Bare`'s own 1.0 to 2.0 reddened
  it. The fixture is fully sensitive to the site multiplier and *no body in its
  traced window ever takes an afforded bout* — `SupportsRest` is carried only by
  `the-fireside-bed`, which needs a built, cold room, and seed 42's trace is
  entirely open ground. **A green run of that golden is not evidence about
  anything on the afforded path**, and the module doc says so at the point of
  use.

## See also

`domains/species/src/lib.rs` (`sleep_grade_registry` and its own doc, which
carries every row's derivation); `windows/vessel/src/liveness.rs`
(`DEFAULT_SLEEP_GRADE`, `SiteGrade`, `SleepTraits`);
[The Pallet chronicle](../../book/src/chronicle/the-pallet.md).
