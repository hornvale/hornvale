# The Generalist — design

**Status:** spec, awaiting G3 review.
**Date:** 2026-08-03
**Program:** C2-0 of the peoples program
(`2026-08-03-the-peoples-program-design.md`).

Humans. One kind, on the surface, using no mechanism that does not already
ship.

## 1. Why this campaign, and why first

The peoples program adds twelve kinds across three families and needs two new
subsystems to do it — a subterranean realm and an authorable lifespan. Humans
need neither. They are a `Settled`, `Endotherm`, surface generalist, and every
part of the machinery that would place one has been running since The Vacancy.

That makes humans the program's pilot. The campaign exercises the six-registry
authoring seam **once, with one kind**, before C2c and C2d do it eleven times;
it pays one epoch to deliver a real preregistered result; and it delivers the
first of the three species the program was asked for without waiting on either
mechanism campaign.

It also inherits a decision that was deliberately handed to it. The Manikin
argued that humans are a *bad* anchor for perception — "Human night vision is
genuinely poor; it sits *below* much of the current bestiary" — and a *good*
one for articulation, because the phonology envelope is IPA and IPA is
human-calibrated. It then declined to act on either, filing it as "an argument
about where *humans* are authored, which is C2's decision to make against a
manikin that already exists." The manikin now exists. This is that decision.

## 2. What humans are for

The coexistence stack normalizes each cell's carrying capacity across competing
species with a `K^β` softmax at `hornvale_demography::BETA = 2.0`, tuned by a
13-seed × 10-β sweep. β is the monoculture↔oatmeal knob: too high and one
species wins every cell, too low and capacity spreads regardless of fitness.

**That knob has never met a true generalist.** Goblin is described as "the
cosmopolitan weed that fills margins/ecotones between the three specialists",
but goblin is warm-*marginal* — a wide, low-devotion curve recentred on the
land median by The Tumult's re-datum. Every other people in the roster has a
refuge, and each one is measured rather than asserted: kobold holds settleable
land above 3000 m, where it is the best-fit people on every cell (mean fit
0.130 against hobgoblin 0.041, goblin 0.049, bugbear 0.004); bugbear holds warm
wet lowland, with the sharpest lowland/highland split of the four (0.264 below
500 m against 0.0038 above 3000 m); hobgoblin the drier temperate open ground;
gnoll the arid margins. β = 2.0 has only ever been asked to arbitrate between
specialists.

A human is the case Gause's principle actually speaks to: a broad niche with no
refuge. Whether the stack survives one is the campaign's question, and it is
worth asking before C2c and C2d add ten more kinds to it.

**Post-hoc note (Task 6 readout):** the kobold and bugbear figures above
predate this campaign's own re-measurement, which disagrees with them —
bugbear's mean fit below 500 m read 0.017563 against the 0.264 quoted here,
and the whole kobold-highland comparison read an order of magnitude lower
with goblin and hobgoblin rank-swapped. The two runs' populations may differ;
neither figure has been corrected pending diagnosis. See
`BIO-generalist-remeasure` in the idea registry.

## 3. Design decisions

**D1 — Humans are one kind, and stay one kind.** No subspecies. The Gause
prediction requires a competitor with no refuge; splitting humans into
environmental variants would give each one a refuge and destroy the thing being
tested. `family_of` maps `human → "human"`, a singleton family, which means no
`family_proto` entry is required — a proto is owed only once a family label is
carried by two or more kinds. This is gnoll's shape from The Vacancy exactly.

**D2 — The biosphere row.**

```
  mass             70.0 kg      Endotherm
  niche            PLANT_FORAGE 0.55 / ANIMAL_PREY 0.45
  potency          0.0          (mundane; the peoples all carry 0)
  social_form      Settled
  condition_niche  human_condition_niche()  -- see D3
```

Mass follows the registry's convention of 5E canon (goblin 18.1, hobgoblin
74.8, bugbear 132.0). The derived life-history that falls out is ~69 yr
lifespan, ~13.8 yr maturity — which is close enough to the real thing to be
worth noting as a coincidence of the anchor, not evidence of anything: the
allometry is calibrated at 40 kg → 60 yr and humans land where the curve puts
them.

The trophic split is deliberately close to goblin's `0.50/0.50` and kobold's
`0.55/0.45`. Humans are not trophically novel and should not be authored as
though they were; **the generalism this campaign tests is on the condition
axes, not the resource axes.**

**D3 — The condition niche is the campaign's real authored object, and it must
be demonstrably not-goblin.** The generalist reading is: wide on every axis,
low devotion on every axis, centred on the land distribution rather than on a
preference. That is also roughly what goblin's niche became after The Tumult
recentred it on the land median. **If human is authored as goblin-recentred,
the campaign has added a synonym and the probe measures nothing** — rung 2 of
the program's ladder, reached by a different road.

So the niche is authored against a stated contrast, and the contrast is
*devotion*, not optimum. Goblin is a warm-marginal generalist: wide, but with a
real temperature lean. Human is authored flatter still on temperature and
moisture — the widest, least-devoted curves in the roster — while keeping a
mild low-to-mid elevation lean, because a species with no environmental refuge
is precisely one that does not out-compete kobold on a mountain.

Concrete values are proposed at plan time against a measured land distribution
over a seed family, not chosen here; §6 makes the measurement a gate. Authoring
a quantile without stating the population it came from is a documented way to
give a wrong number the authority of evidence.

**Post-hoc amendment (Task 5b, 2026-08-04) — "widest, least-devoted" was only
half true, and the fix.** The niche as first shipped (Task 2) stated the
contrast above as devotion alone, but its widths were in fact unargued:
narrower than goblin's on temperature (22.0 vs. 28.0) and elevation (2000.0
vs. 3000.0) — the opposite of "widest... curves in the roster" for those two
axes. Task 5's shape-attribution reading (see the §4 amendment below and
`windows/worldgen/tests/generalist_distinctness.rs`'s module doc comment)
measured that the vacuity gate's real-case dispersion gap was WIDTH-dominated
and pointed the opposite direction from a devotion-only reading — the
contradiction this paragraph originally disclaimed as "not the claim to test"
turned out to be carrying the claim. The owner directed a re-authoring:
`human_condition_niche()`'s widths are now each derived from a stated,
measurement-grounded floor (on each axis, response must vary by no more than
20% of its peak across the measured p5–p95 span of settleable land —
`windows/worldgen/tests/generalist_baseline.rs`'s Task 5b extension) and
verified, axis by axis, wider than every other people's width in the roster.
Full numbers, the per-axis rule, and which axes changed vs. were already
correct live in `human_condition_niche()`'s doc comment
(`domains/species/src/lib.rs`). Devotion was untouched — it was already the
argued, correct half of the contrast.

**D4 — Humans sit below the manikin on night vision, and the exact value is a
step-function decision.** This is The Manikin's deferred call, and the argument
for it is sound: the peoples' authored night vision runs goblin 0.5, hobgoblin
0.6, bugbear 0.7, gnoll 0.75, kobold 0.9, with the dragons at 0.9. Every people
but goblin is above the manikin's midpoint, and humans genuinely belong below
all of them.

But the value is not free, because `pack_depths` is a step function:

```
  hue       = 2 + ((1.0 - night_vision) * 3.0).round()
  luminance = 3 if night_vision > 0.6 else 1
```

which gives:

```
  night_vision      hue depth    who
  ---------------   ---------    -------------------------
  0.90                  2        dragons  (dark/light/red)
  0.75                  3        gnoll
  0.50 .. 0.17          4        goblin, and human at 0.25
  <= 0.166              5        nobody yet
```

So authoring humans at 0.25 puts them **tied with goblin at depth 4**, and
authoring them at 0.15 makes them the only kind in the roster at depth 5 — the
deepest hue inventory in the world. Both are defensible; they are different
claims, and the break sits almost exactly where the perceptual argument puts
humans, so the number must be chosen deliberately.

**Recommendation: 0.15, hue depth 5.** The colour ladder is Berlin & Kay's,
derived from human languages; a model whose hue hierarchy is human-derived and
which then denies humans its deepest rung is quietly incoherent. It also gives
depth 5 its first witness, which the coverage tests will want. Luminance is 1
either way — humans get the shallow luminance vocabulary, which is the correct
consequence of poor scotopic vision and is the cost side of the same trade.

**D5 — The remaining vectors.**

```
  MindVector        threat_response 0.5   (at the manikin, by authorship)
                    deliberation_latency 0.6
                    time_horizon 0.75

  SocietyVector     sociality Hierarchic
                    status_basis Knowledge
                    in_group_radius 0.8   (the widest in the roster)

  PerceptionVector  activity Diurnal
                    night_vision 0.15     (D4)
                    sky_attention 0.65
```

`in_group_radius 0.8` is the one value that carries an argument beyond
plausibility: it exceeds gnoll's 0.7 and makes humans the most expansive "us"
in the world, which is the social twin of the broad niche and is what the Gause
story predicts a no-refuge generalist looks like from the inside. `sky_attention
0.65` is above the midpoint on the strength of calendars, agriculture and
navigation, below kobold's 0.8.

`threat_response` sits *at* the manikin. That is now legal — The Manikin moved
the model to the rung where "kinds may coincide with" the reference vector — but
it is worth stating that this is authorship and not a default, because
re-welding a people to the identity element is the exact bug that campaign
removed.

**D6 — Articulation is where humans are the *good* anchor.** The phonology
envelope is IPA, so human articulation is the one vector family where a human
row is better founded than any other kind's. Humans are authored at the
envelope's neutral settings, and the spec says plainly that this is the
dimension The Manikin identified as human-well-founded — so a future reader
knows the coincidence with the reference is argued, not inherited.

## 4. Preregistration

Frozen before any authoring. The metric is human's per-cell competitive share
from the coexistence packer, measured over a seed family, against the **five**
existing peoples' shares on the same cells — goblin, kobold, hobgoblin, bugbear
and gnoll. (Several doc comments in `domains/species` still say "the four
peoples", predating gnoll. Do not inherit the count from them; the freshness
sweep in §7 exists partly to fix them.)

**H1 — the ecotone prediction.** Human takes a large share of marginal and
ecotone ground and competes hardest with goblin — the two generalists overlap
more than any other pair in the roster, measured by Pianka overlap and by
share correlation across cells.

**H2 — the refuge prediction.** Human does **not** become the best-fit people
on kobold's highland stronghold (settleable land above 3000 m), nor on
bugbear's warm wet lowland, nor displace hobgoblin or gnoll from theirs. All
four specialists retain a band of cells where each is the best-fit people.

**H3 — the falsification.** If human takes majority share across most
settleable land *including* the strongholds, then β = 2.0 is too low and the
coexistence stack has been protected only by the absence of a true generalist.
**This outcome is a finding about the knob, not a defect in humans**, and it
would be the campaign's headline. It must not be repaired by quietly retuning
human's niche until H2 passes — if H2 fails, it fails, and the retune is a
separate argued decision made after the null is recorded.

**The vacuity check, run before the readout counts.** Human's condition niche
must be shown measurably distinct from goblin's. If it is not, the campaign
has authored a synonym and H1–H3 are meaningless. This is the program's
ladder applied to its own first campaign.

**The mutation step.** A test that goes RED if human's `ConditionNiche` is
replaced by goblin's — proving the readout can tell the two apart. A green
suite proves the code ran; only the mutation proves the axis is visible.

**Post-hoc amendment (Task 5, before Task 6 unblinded anything) — the
statistic actually shipped, and why it differs from this section's original
wording.** This section originally froze the criterion as: "their per-cell
fit vectors must not be a monotone rescaling of one another across the seed
family" (i.e. a Spearman rank-correlation ceiling). Task 5 found that
wording **algebraically invalid as a gate**, not merely risky, before
building anything against it: `ConditionResponse::eval`
(`kernel/src/ecology.rs`) is a strictly monotone-decreasing function of
`|field - optimum|` for a fixed optimum, regardless of `devotion` or
`width`. Human and goblin share TWO axis optima by deliberate design —
elevation (1500.0 m) and moisture (0.50), both argued in
`human_condition_niche()`'s doc comment — so on those two axes the two
kinds' per-cell fit is rank-identical BY CONSTRUCTION, no matter what
`devotion`/`width` values are authored. Rank correlation is invariant under
a monotone transform, so a ceiling near "expected for two genuine
generalists" could not fail on half the niche's axes regardless of what was
authored there — the exact vacuity the check exists to catch, built into
the check itself.

The shipped statistic is instead each kind's coefficient of variation (CV =
population stddev / mean) of per-cell fit, compared as
`cv(human) / cv(goblin)`, gated on `|cv_ratio - 1|` clearing a floor of
`0.02`. This is a correction to this section's frozen wording, made explicit
here rather than silently rewritten — the original criterion was never
satisfied by any implementation, so no measurement made under it is being
retroactively reinterpreted.

**Second post-hoc amendment (Task 5b, 2026-08-04) — the width-vs-devotion
attribution finding, and its resolution.** At the time §3's original wording
was corrected above, the measured real gap (`0.0462`) was found
width-dominated, not devotion-dominated, and pointed the OPPOSITE direction
from a devotion-only attribution reading (`cv_ratio` 1.0462 real vs. 0.9766
width-only) — contrary to this section's and `human_condition_niche()`'s
framing of the *intended* contrast, because human's widths were narrower
than goblin's on two axes (see §3 D3's own post-hoc amendment). The owner
directed a re-authoring of `human_condition_niche()`'s widths from a stated,
measurement-grounded rule; after it, both readings agree in direction
(`cv_ratio` 0.9528 real, 0.9747 width-only — both `< 1`) and the real gap
grew to `0.0472`. Full reasoning and the measured table live in
`windows/worldgen/tests/generalist_distinctness.rs`'s module doc comment and
`human_condition_niche()`'s doc comment (`domains/species/src/lib.rs`).

## 5. Non-goals

- **Subspecies of any kind.** D1.
- **Any change to `BETA`.** If H3 fires, the retune is a later, argued decision.
  Changing the knob in the same campaign that first stresses it would destroy
  the attribution.
- **Touching the manikin.** Humans are authored *against* the reference; the
  reference does not move. Re-anchoring on humans is the repair The Manikin
  explicitly rejected.
- **Articulation beyond the neutral envelope, and any lexicon work beyond what
  a new kind owes.** LANG-53 is C2d's.
- **The other two families.** C2c and C2d.

## 6. Verification

1. `cargo test -p hornvale-species` during iteration.
2. `cargo test -p hornvale-worldgen --test exposure` — `pack_depths` gains a
   depth-5 witness under D4's recommendation.
3. The land-distribution measurement behind D3, stating its mesh level, cell
   filter and seed count in the niche's doc comment.
4. The vacuity check and the mutation test of §4, both before the readout is
   believed.
5. `make gate`. Budget `timeout: 3600000` — measured 22–37 min on this Mac,
   not the ~4 min decision 0040 budgeted.
6. `make rebaseline`, then `git diff --exit-code` over `book/src/gallery/`,
   `book/src/reference/`, `book/src/laboratory/` and `docs/audits/`. The
   type-audit report drifts on any pub-boundary change and is the commonly
   missed one.
7. `bash scripts/census-run.sh` on lefford — **carve-out, authorization
   requested at the close, not here.**
8. Checks `make gate` does not run: `make census-check`, and `shellcheck` if
   any script changes.

## 7. Definition of Done

- [ ] `biosphere_registry` row (D2) with `human_condition_niche()` and its
      measured rationale (D3).
- [ ] `family_of`: `human → "human"`. No `family_proto` entry — assert that the
      singleton rule still holds rather than assuming it.
- [ ] `KIND_CONCEPTS`: `("human-kind", "a human")`.
- [ ] `psyche_registry`, `society_registry`, `perception_registry` rows (D4/D5).
- [ ] `articulation_registry` and `lexicon_registry` rows in `hornvale_language`
      (D6).
- [ ] **`domains/species/tests/coverage.rs` tables updated.**
      `status_basis_coverage_matches_the_table` and
      `activity_cycle_coverage_matches_the_table` are authored tables that a new
      kind falsifies; they are not optional and they are easy to miss.
- [ ] The worldgen invariants still hold: `speech ⊆ perception ⊆ mind`, and
      society ⟺ minded ∧ social.
- [ ] `windows/worldgen/tests/non_void_roster.rs` passes with no allowlist —
      human must actually be placed on every seed it is tested against, or it is
      a ghost.
- [ ] **Both census fixtures refreshed** — 31 rows in `the-census`, 3 in
      `census-of-the-meeting`. A new kind reddens both, and refreshing only one
      leaves a suite that looks green for the wrong reason.
- [ ] Epoch declared **only if a derivation actually moved** (0084), and stamped
      per 0089. Adding a settling people changes the settlement-genesis roster,
      so it almost certainly did — but the check is the point, not the
      expectation.
- [ ] Preregistered readout recorded in the chronicle **whichever way it came
      out**, including H3.
- [ ] Book: the species chapter's roster and dimension tables; a freshness sweep
      of chapters that say "the four peoples" or "the five peoples" — several
      do, and the count is scoped to the paragraph that derived it.
- [ ] Chronicle (`book/src/chronicle/the-generalist.md`). **This spec cites
      registry IDs and the book may not** — `docs_consistency` permits them only
      in the Frontier part. Name the concept, never the id.
- [ ] Retrospective (`docs/retrospectives/the-generalist.md`), decision 0020.
- [ ] Idea-registry bookkeeping per the metaplan §9, at minimum the
      `BIO-three-probes` correction.

## 8. Flagged for review

1. **An epoch and a census regen**, the latter a carve-out needing explicit
   authorization at the close (0081, on lefford per 0079/0086).
2. **D4's night-vision value is a visible authoring choice**, not a tuning
   detail: 0.15 gives humans the roster's deepest hue vocabulary and 0.25 ties
   them with goblin. The recommendation is argued in D4; it is the one number in
   this spec whose consequence a reader will actually see in generated text.
3. **H3 is a live possibility, not a formality.** β = 2.0 was tuned against a
   roster with no true generalist. If it fires, this campaign's headline is a
   null about the coexistence stack, and C2c/C2d land on a knob that needs
   retuning first — which is better discovered here, with one kind, than at C2d
   with ten.
4. **`threat_response` coincides with the manikin.** Legal at the model's
   current rung and stated as authorship, but flagged because a people sitting
   on the identity element is the shape of the bug The Manikin just removed.
5. **The 69-year lifespan is a coincidence of the 40 kg anchor**, not a
   validation of the allometry. Worth stating in the chronicle before someone
   reads it as evidence the curve is right — C2b is about to change that curve.
