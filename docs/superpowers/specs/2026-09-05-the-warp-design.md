# The Warp — design

**Campaign:** The Warp · **Decision block:** 0776–0785 · **Ledger:**
[`2026-09-05-the-warp.md`](../ledgers/2026-09-05-the-warp.md) ·
**Predecessor:** [The Weft](2026-09-03-the-weft-design.md)

The **warp** is the set of macro fields already running continuously through
the world; the **weft** is the fine detail The Weft wove across them. The
Weft measured whether the weft *tracks* the warp and found its central
prediction false. This campaign asks the question from the walker's side:
what does a walker actually get to read, and could a knowledgeable one
predict the weft from it? It is the inverted campaign the Weft named — *start
from what a walker should be able to infer, and let frequency fall out of
it* — and every claim in §1 was measured before a line of it was designed.

---

## 1. The premise, measured

`windows/lab/tests/suite/warp_probe.rs` (committed on this branch, run once,
1.2 s) tabulates every sign the walk-band sentence renders at seed 42 against
each derived kind's occurrence, over the Weft's own land-eligible population
(`n = 11,218`; spring 403, overhang 843, thicket 1,517, erratic 428). Every
reading is paired with a **lagged null** — the same signs paired with the
occurrence bits of the facet 1,000 places later in vertex order, a cyclic
permutation that keeps both marginals and breaks the link — so a rich sign's
finite-sample bias is read off rather than argued about. The Weft's own H3
estimator is one row of the table and reproduces its committed readings
exactly (0.007812 / 0.002497 / 0.038604 / 0.000000), which is the positive
control that this is the Weft's population.

**1.1 What the walker is told today, in bits, net of null.**

| sign (classes) | spring | overhang | thicket | erratic |
| --- | ---: | ---: | ---: | ---: |
| biome word (19) | 0.0032 | 0.0009 | 0.0356 | 0.0000 |
| rock class (16) — **not rendered** | 0.0035 | 0.0005 | 0.0142 | 0.0002 |
| wetness word (3) | 0.0006 | 0.0001 | 0.0125 | 0.0002 |
| relief / aspect / openness (3 each) | ≤ 0.0002 | ≤ 0.0002 | ≤ 0.0002 | ≤ 0.0002 |
| visible today: biome + noun + wetness (177) | 0.0025 | 0.0023 | 0.0390 | −0.0009 |
| after §4: biome + rock + wetness (271) | 0.0061 | 0.0026 | 0.0381 | 0.0005 |
| the Weft's H3 ceiling (cause, 4 bins) | 0.0078 | 0.0025 | 0.0386 | 0.0000 |

Three things follow. **Thicket is legible at its ceiling today**: its causes
are temperature and moisture, and those *are* the biome word — that is why
it won the Weft's ordering. **Spring reaches a third of its ceiling** from
what is rendered, and four fifths once the rock class is rendered — so the
rock word is worth adding, and it is the whole of what rendering can buy.
**The three address-noise axes sit at their nulls**, as noise must; and the
whole sentence read as one tuple (5,840 classes) reads 0.15–0.37 bits with a
null of the same size — a number that would have looked like legibility and
is bias.

**1.2 Where a kind's occurrences stand on their own cause.** Facets /
occurrences / rate per bin of the kind's `macro_state` (four equal bins over
`[0,1]`), and the share of occurrences on a facet whose cause reads ≥ 0.5:

| kind | b0 | b1 | b2 | b3 | share ≥ 0.5 |
| --- | --- | --- | --- | --- | ---: |
| spring | 10,754 / 333 / 0.031 | 342 / 39 / 0.114 | 122 / 31 / 0.254 | — | **0.077** |
| overhang | 7,399 / 474 / 0.064 | 3,400 / 321 / 0.094 | 371 / 42 / 0.113 | 48 / 6 / 0.125 | **0.057** |
| thicket | 5,901 / 392 / 0.066 | 2,836 / 478 / 0.169 | 2,195 / 545 / 0.248 | 286 / 102 / 0.357 | 0.427 |
| erratic | — | — | 11,218 / 428 / 0.038 | — | (constant cause) |

**This is the finding the campaign is built on.** Spring's cause class is
rare — 464 of 11,218 facets read ≥ 0.25 — and the Weft recipe's floor,
`abundance × (1 − contextuality) × noise`, runs over the other 96% of land.
So **333 of 403 springs stand on a facet with no cause at all.** A
knowledgeable observer told the whole truth about carbonate and drainage
would still be unable to predict 92% of springs. The Weft's falsified
ordering was never about the four-bin estimator: the 19-class biome word
ranks thicket above spring exactly as the cause does. It is a base-rate
effect — a small false-alarm rate over a large population outnumbers a high
hit rate over a small one — and it lives in the recipe, which is the only
place a campaign can move it. `SURF-legibility-ranking-may-be-estimator-
resolution` is answered, in the negative, before this spec begins.

**1.3 What that makes the inverted campaign.** Rendering the causes (§4)
lets a walker reach the ceiling; it cannot raise the ceiling. Raising the
ceiling means concentrating a sign kind's occurrences where its sign is —
authoring the *floor* and the *reliability* of each kind and letting its
frequency be whatever the world's cause geography makes it (§6). And the
instrument that says whether any of it worked must read what the walker
reads, beside a null, from the walker's side (§5).

---

## 2. Vocabulary (ledger #1's dictionary; every later section uses these words exactly)

- **Warp** — the continuous macro fields, blended below the grid
  (`FieldPack`: carbonate, induration, drainage, slope, temperature,
  moisture; and `RockClass`, a per-vertex nominal).
- **Weft** — the derived features (`WeftKind`), a pure function of seed and
  position.
- **Sign** — a rendered, world-determined, at-facet, discrete token: a word
  the walk-band sentence emits whose value is a function of the world at
  that facet. Today: the biome word and the wetness word. After §4: the rock
  word and the steepness word too.
- **False sign** — a rendered token with causal semantics whose value is
  address noise: the descriptor noun, relief, aspect, openness. Measured as
  a control (§5.3), never deleted (§4.3).
- **Ceiling legibility** — MI(cause; occurrence), the Weft's H3: what the
  world knows.
- **Channel legibility** — MI(signs; occurrence), net of null: what the
  walker is told. Bounded above by the ceiling's continuous form (data
  processing), not by its four-bin estimate.
- **Learned legibility** — the held-out gain of an observer who fitted the
  sign table on a sample: what a walker can acquire.
- **Naive observer** — predicts the base rate. **Oracle observer** —
  predicts `P(Y | signs)` from the whole population. **Learner** — the
  oracle's table fitted on a training half, scored on the held-out half.
- **Found fraction** — the share of a kind's occurrences on a facet whose
  cause reads ≥ 0.5: signal detection's positive predictive value. A kind's
  occurrence is *found* when a knowledgeable observer could have predicted
  it, *extruded* when none could.
- **Told** — the upper failure: a sign that restates the feature
  (wallpaper). The preregistration bands against it (§7 H5).
- **Honest silence** — no sign word where no cause is; the reason §4 adds
  words and grounds none.

---

## 3. Scope — three halves

| half | deliverable | what it buys |
| --- | --- | --- |
| **A** the signs (§4) | the rock word and the steepness word in the walk-band sentence, each one function shared with the instrument | a walker can reach the ceiling: spring 0.0025 → 0.0061 of 0.0078 measured |
| **B** the instrument (§5) | channel, learner, found-fraction and false-sign readouts, each net of null, on the Weft's pools | legibility measured from the walker's side, with the bias it would otherwise report subtracted |
| **C** the inversion (§6) | the sign kinds' recipes re-parameterised as reliability + floor, floor authored near zero, frequency reported | the ceiling itself rises: found fraction 0.077 → a frozen band |

Thicket and erratic are **not** re-parameterised: thicket is texture and
already at its ceiling, erratic is the control. Both are the campaign's
non-regression controls — their derived output must be byte-identical to the
tree this branch is cut from (§7).

---

## 4. Half A — the signs

**4.1 The rock word.** `RockClass` at the room's dominant corner vertex —
the same read `reflectance_mixture_with_weights` performs for colour and
`FabricContext::at` performs for building fabric: categorical
nearest-corner, never blended, the `WaterKind` precedent (decision 0121:
nominal fields must partition; 0123: a document finer than the model behind
a field discloses the field's resolution rather than inventing detail). It
is a per-vertex word and will read constant across a vertex's ~4,096 rooms;
that is correct and disclosed, not a defect. Rendered as its own clause
after the descriptor — *"… in the lands of Doaba. Underfoot, pale limestone."*
— with one authored phrase per `RockClass` variant (data, in
`hornvale_worldgen::warp`; seed 42's land shows 16 of them), and silent afloat (the same `vantage.is_none()`
gate the weft clause uses; a walker does not stand on the sea floor).

**4.2 The steepness word.** `FieldPack.slope` blended at the facet
(`blend_at`, bilinear, continuous), saturated exactly as overhang's own
recipe saturates it — `tanh(|slope| / GORGE_SLOPE)` — and cut at two
authored thresholds into three words: *level*, *sloping*, *steep*. A new
clause, not a regrounding of the relief axis (§4.3): relief says hollow or
rise from a draw; steepness says how hard the ground tilts from the warp.
Overhang's cause is `induration × steep`, so with the rock word (induration
is a property of the class) the walker holds both halves of overhang's
sign, as they hold both halves of spring's (limestone × wet).

**4.3 What is deliberately not changed (ledger #6).** The descriptor noun's
variety draw and the relief, aspect and openness axes stay exactly as they
are, draw order intact (a save-format contract since The Rill). They are
texture by their own documentation, and under this campaign's instrument
they are the false-sign baseline (§5.3): a walker learns to give them no
weight, which is what §5.2's learner will show. The wetness word is left as
The Rill grounded it.

**4.4 One implementation per sign (ledger #7; decision 0778 expected).**
`hornvale_worldgen::warp` exposes `rock_sign(terrain, vertex) -> RockSign`,
`steepness_sign(slope_blend) -> Steepness` and `wetness_sign(micro) ->
Wetness` (the last wraps the ±0.33 threshold `grammar.rs` keeps private
today, so the lab stops duplicating it). The prose renders the enum's word;
the lab tabulates the enum's discriminant. There is no second copy of any
threshold — The Rill named a second copy of a partition as the failure mode
and this campaign inherits that rule with the mechanism to keep it: a test
that renders a room and re-derives its sign tuple through the same functions
and requires the words to match.

**4.5 Determinism and artifacts — a branch table, not a prediction.** The
rock and steepness words are derived reads over existing fields: no draw, no
stream label, no ledger fact, no epoch (`windows/CLAUDE.md`: a derived read
over an existing field consumes nothing). `cli/tests/fixtures/world-seed-42.json`
must not move (pinned by `weft_ledger_guard.rs`'s fact count already). What
moves is prose:

| after `make rebaseline`, the diff over `docs/generated-paths.txt` shows | then |
| --- | --- |
| only session fixtures (`clients/game/core/tests/fixtures/`), gallery transcripts, and `docs/audits/` | expected — commit in the same change |
| a lab `rows.csv` or `schema.json` | expected only after §5's metrics register; otherwise STOP |
| `world-seed-42.json`, any almanac, the elevation map, or a scene fixture | STOP — a sign has drawn or committed something; the derived-only claim is false |

---

## 5. Half B — the instrument

**5.1 The population and the tuple.** The Weft's grid pool (one
representative facet per geosphere vertex, land-eligible only) extended per
reading with the sign tuple `(biome word, rock sign, steepness sign, wetness
sign)` — exactly the walker's sentence after §4, through §4.4's functions.
`n ≈ 11,000` land facets against a tuple of at most 19 × 16 × 3 × 3 classes.

**5.2 Readouts, per kind, all registered as metrics (`warp-*`) except where noted — eight families × four kinds = 32 registrations (G4 amendment, 2026-09-05: the plan's self-review found H3's "≥ 0.5 × oracle" and H5's guard each need a registered number; an earlier draft said six families).**

| readout | definition |
| --- | --- |
| channel MI | discrete MI in bits between the sign tuple and occurrence over the population |
| channel null | the same statistic averaged over five cyclic shifts of the occurrence bits (1,000 … 5,000 places in vertex order); a cyclic shift is a permutation, so this is a permutation null with both marginals held |
| channel MI, net | channel MI − channel null (computed in the readout, not registered) |
| found fraction | share of the kind's occurrences on a facet whose `macro_state` ≥ 0.5 (undefined for erratic; its cause is constant) |
| best-class lift | max over sign classes with support ≥ 100 of `P(Y | class) / P(Y)` — the walker-facing number, "three times as likely here" |
| learner gain | the sign table fitted on even-indexed land facets, scored on odd-indexed ones as log-loss reduction against the base rate, in bits per facet; negative when the table overfits. **Smoothing (amended 2026-09-05, Task 5, before any readout seed was built):** a class with `n` fits and `k` hits predicts `(k + α·p̂) / (n + α)` with `p̂` the fit half's base rate and `α = 10` — an equivalent-sample-size prior toward the base rate. The first implementation used Laplace `(k + 1)/(n + 2)`, a prior toward 0.5, which on a 3.6% event over 469 tuple classes predicted every thin class at up to one half and paid ~4.6 bits per miss: learner gain read −0.049 for spring at seed 42, an instrument artefact, not a world reading. An unseen class predicts `p̂` exactly, so an uninformative table scores 0, never below it by construction. |
| false-sign MI, net | channel MI net of null for the tuple `(noun, relief, aspect, openness)` — the control |
| oracle gain | the sign table fitted and scored on all land, bits per facet — the in-sample bound the learner is measured against (H3) |
| max class rate | max over sign classes with support ≥ 100 of `P(Y \| class)` — H5's wallpaper guard |

The learning curve — the same learner on the Weft's walk pool at 60, 240 and
960 steps — is a readout in the calibration test, descriptive only, not a
registered metric and not gated.

**5.3 What the erratic and the noise axes are for.** The erratic must read
zero on every channel readout net of null; the four false signs must read at
their nulls for every kind. A campaign that could not show both has an
instrument that credits noise, and the finding would be about the
instrument. Both are preregistered (§7 H4).

**5.4 Cost (ledger #7). MEASURED 2026-09-05 (Task 5), and the premise below was wrong.** The sign columns ride the existing grid pool, and the learner is one pass over the same rows — but "three map reads per facet" was false: the wetness sign's grounding calls the rill network's nearest-branch query per facet, and an ablation attributed **100% of the pool's added 0.218 CPU-s/world** to it; biome, rock and steepness are free. The honest total, all 32 metrics against the pre-task registry over 20 worlds in release (medians of three), is **0.331 CPU-s/world added** — over the 0.25 rule below. **Ruling (ledger #10): accepted as-is, not subsampled.** Subsampling to every second facet halves `n` to ~5,600, doubles the null's bias and raises its standard deviation from 0.0020 to 0.0035 bits, which would blunt §5.3's controls to save ~23 s of census wall per refresh. The rule was written for a cost it did not foresee; the instrument's resolution is what the campaign is for. (The brief's a/b design — the Weft's metrics alone vs the Warp's alone — could not see the sign columns at all, because both studies build the same shared pool; the pre-task-vs-all-32 difference is the measurement.)
Decision rule, measured in release on the Mac before registration: added
cost ≤ 0.25 CPU-s/world registers as-is; more subsamples the pool for the
sign columns and this section records the subsample. Registration restages
every `"all"` study's fixture (lab CLAUDE.md); the census refresh at close
(`make sluice-census`) pays it once, in the same sitting as the Weft's
columns would have moved anyway (§6).

---

## 6. Half C — the inversion

**6.1 The recipe, regrouped.** The Weft computes
`p = abundance · (contextuality · cause + (1 − contextuality) · noise)`.
Algebraically that is `p = rate · cause + floor · noise` with
`rate = abundance · contextuality` and `floor = abundance · (1 −
contextuality)` — the Weft's two dials are a rotation of the two quantities
the readout actually measures. This campaign authors the rotated pair
directly, per kind, `plumb`-tagged: **reliability** (`rate`: how often the
sign pays when the cause saturates) and **floor** (how often the feature
appears where no cause is). Decision 0687 is untouched — `cause` is still
the continuous `macro_state` — and no stream label, leg or draw changes.
**Thicket and erratic keep the Weft's exact expression**, not a regrouped
one: `a·(c·m + (1−c)·n)` and `(a·c)·m + (a·(1−c))·n` can differ in the last
unit, and an occurrence is a comparison against that value, so a regrouping
alone could flip a facet. The rate/floor form is the sign kinds' path; the
non-regression controls stay on the path that produced the committed
census. If the implementation unifies the two, the unification is proven
byte-identical on thicket and erratic over the whole seed-42 grid first.

**6.2 The response is sharpened for the sign kinds, and this is the
design, not a tuning.** §1.2 shows that even at `floor = 0`, a graded
response `rate · cause` over 10,754 low-cause facets would still outnumber
the 122 saturated ones. A sign kind's occurrence must be concentrated where
its *sign* is, and a sign is a level set; so spring and overhang read their
cause through an authored soft step — `smoothstep(cause; lo, hi)` — whose
edges are the same thresholds §4's words cut at, so the class a walker can
name is the class the feature lives in. The occurrence draw (a second,
decorrelated position-continuous field at the kind's correlation length) is
what still decides *which* facets inside the class carry the feature — the
Weft's own guard against wallpaper, unchanged.

**6.3 Frequency falls out.** Nothing here sets a count. A kind's frequency
is `P(Y) = rate · E[step(cause)] + floor · E[noise]` over land, reported per
seed in the readout as the campaign's *consequence*, beside the Weft's
committed 0.984% / 2.058% / 3.703% / 1.045% (`the_weft.rs`'s own figures).
Spring and overhang will occur less often than before; that is what
"found rather than extruded" costs, and it is a fidelity call for Nathan
(§12).

**6.4 Calibration protocol (decision 0016).** The four authored constants
per sign kind (`rate`, `floor`, `lo`, `hi`) are set on **seed 42 only**,
against §7's bands, before any readout seed is built; the ledger records the
constants and the seed-42 values at the moment they are frozen. The readout
seeds (§7) are built once, after, and never inform a constant. Thicket's and
erratic's constants are not touched at all.

---

## 7. Preregistered measurement (decision 0016)

Calibration seed: **42**. Readout seeds: **13, 7, 1, 100** (The Prospect's
five less the calibration seed). Every claim is over the four readout seeds;
seed 42 is reported beside them and gates nothing. "Net" means net of the
five-shift null. A falsified prediction is a finding and is reported as the
headline.

**H1 — found rather than extruded.** On each readout seed, found fraction
spring ≥ 0.60 and overhang ≥ 0.60 (from 0.077 and 0.057 at seed 42 today;
calibrated to 0.787 and 0.772 on seed 42 — at the cost, for spring, of a step
edge above the walk band's whole cause range there: existence density fell
0.0359 → 0.0134 and the 78-walk encounter rate reads 0, the magnitude of §12
flag 1, carried to G6);
thicket within [0.30, 0.55] (its recipe is unchanged; 0.427 today), which is
the non-regression band.

**H2 — the walker can tell.** On each readout seed, best-class lift for
spring and for overhang ≥ 2 × the erratic's best-class lift at the same
support floor on the same seed (the erratic's lift is the null lift — 1.7×
at seed 42 today, from cardinality alone — so an absolute bar would be a
number nobody derived). Reported, not gated: spring's channel net as a share
of its ceiling, and the spring/overhang ordering of channel net, raw and
normalised by each kind's H(Y).
**Amended 2026-09-05 (Task 6, calibration seed only, before any readout seed
was built): the clause "channel MI net for spring ≥ channel MI net for
overhang" is WITHDRAWN from the gate.** Mutual information in bits scales with
a kind's entropy, so a between-kind ordering can be met by making the lower
kind rarer — and calibration round 3 did exactly that, cutting overhang's
reliability to 0.16 and its frequency twelve-fold to satisfy a clause about
spring, while spring's own reading did not move (0.086464 in every round). A
bar that a kind can pass by disappearing is defective. Each kind is gated on
its own legibility (H1, the lift bar above, H3, H5); overhang was then
re-calibrated on those alone and its reliability set to 0.50, the middle of
its passing range (0.50 and 0.75 hold every band; 0.90 trips H5), for H5
headroom of 1.62× and because a sign that pays one time in two where its cause
saturates is the found-not-told band this campaign is for.

**H3 — it can be learned.** On each readout seed, learner gain > 0 for
spring, overhang and thicket. The erratic's learner gain ≤ 0.001 bits/facet
(one-sided; amended 2026-09-05 from "within ±0.001" — a held-out table over
~469 classes of noise cannot beat the base rate and MUST lose to it by an amount
set by cardinality and population, −0.0138 at seed 42; the control is that the
erratic never GAINS, not that its loss is small).
**Amended 2026-09-05 (Task 5, before any readout seed was built): the clause
"≥ 0.5 × the oracle's in-sample gain" is withdrawn from the gate and the
ratio learner / (channel MI net of null) is REPORTED instead.** The oracle as
defined has no meaning as a denominator: smoothed, it reads negative in-sample
for a 3.6% event (−0.014 for spring at seed 42); unsmoothed, its in-sample
log-loss reduction is algebraically the raw channel MI, which the null exists to
correct. `warp-oracle-gain-*` stays registered and reported; nothing gates on it.

**H4 — the instrument credits nothing to noise.** On every readout seed and
every kind: erratic channel MI net ≤ 0.008 bits (one-sided), and the false-sign
tuple's MI net within ±0.002 bits.
**Amended 2026-09-05 (Task 5, before any readout seed was built) from ±0.001
and ≤ 0.001, and the reason is the estimator's own resolution.** The
permutation null of a discrete MI has a spread as well as a mean: the χ²
approximation gives mean `(K−1)/(2·n·ln 2)` and a standard deviation that at
seed 42 measures **0.001967 bits** for the 469-class sign tuple and **0.000464**
for the 27-class false-sign tuple (the predicted mean, 0.030091, is exactly what
`warp-channel-null-erratic` read — the positive control that the null is the
null). A bar of 0.001 on a statistic whose null spread is 0.002 fails the
control roughly a third of the time by noise alone, which makes H4 a coin flip
rather than a control. Both bars are now **four null standard deviations**,
fixed from the seed-42 measurement (0.008 and 0.002) rather than recomputed per
seed, so they cannot move with a reading.

**H5 — nothing is told.** On every readout seed, no sign class with support
≥ 100 has `P(Y | class)` > 0.75 for any kind (0.357 is today's maximum). A
sign kind above it has become wallpaper, and the reading is a finding about
§6.2's `hi`, not a success.

**Non-regression.** Thicket's and erratic's `weft-*` census columns and
their occurrence bits on seed 42 are byte-identical to the branch point's;
`make lab-diff STUDY=the-census` at close shows no thicket or erratic column
moved. Every relation the Weft's `weft_density.rs` still asserts stays
asserted.

**Distribution, not point.** Each readout is recorded per seed; the
chronicle reports min and median across the four, and the frozen bars above
are on every seed (a count of four), never on a pooled figure.

---

## 8. Non-goals

- **Rumors and the region invariant** — deferred with their producer by
  the Weft (`SURF-rumors-need-a-producer-before-the-region-invariant`);
  a rumor is a remote, degraded sign and this campaign reads only at-facet
  signs (ledger #4).
- **A knowledgeable body that speaks its inference** — the species-gated
  reading verb (follow-up row). This campaign makes the signs visible to
  every walker and measures whether a table over them pays; it does not put
  the table in a mouth.
- **Re-grounding relief, aspect, openness or the noun** (§4.3, ledger #6).
- **Fixing the `examine` datum's raw-field leak** (follow-up row).
- **Biome monotony at its source** — the biome word stays per-vertex.
- **Any change to thicket or erratic.**

## 9. Risks

- **Density retreats for the sign kinds.** Deliberate (§6.3), but The
  Weft's headline (0.2563 of land-eligible facets) was carried into the
  Confidence Gradient; the re-score must say which half moved and why.
- **The bands in §7 were set from one seed's table.** H1's 0.60 and H2's
  2× are authored bars; if seed 42's calibration cannot reach them without
  H5 tripping, that is reported as the finding and no bar moves after a
  readout seed is built.
- **Fixture churn.** Every session fixture and gallery transcript carrying
  a room sentence moves once for §4; every `"all"` study fixture moves once
  for §5. One rebaseline and one census refresh, at close.
- **A sharpened response makes edges.** A level-set contour is a real
  physical shape (springs on a contact line), but if the readout shows a
  kind's Moran's I falling below the Weft's address-hash guard band, the
  finding is reported and the step widened only with a ledger entry saying
  so.

## 10. Definition of Done

Beyond the standard DoD (chronicle, retrospective, freshness sweep):

- Confidence Gradient re-score (decision 0030): *dense* stays settled;
  *found rather than extruded* gains its first measured reading and moves
  accordingly.
- Registry: `SURF-legibility-is-inference-from-signs` repointed at the
  chronicle; `SURF-legibility-ranking-may-be-estimator-resolution` flipped
  to answered (base rate, not estimator); the four follow-up rows the
  ledger names are filed.
- Census refresh at close (`make sluice-census`), and the Gnomon injection
  fixtures re-authored in the same sitting if the registry grew.
- Decisions minted inside **0776–0785** only.

## 11. Decisions expected

- **0776** — Legibility is measured from the walker's side: a sign is a
  rendered, world-determined, at-facet, discrete token, and every channel
  reading is paired with its permutation null.
- **0777** — A kind's reliability and floor are authored by intent and its
  frequency is a reported consequence; the found fraction is the readout.
- **0778** — A sign has one implementation, read by the prose and by the
  instrument; no threshold has a second copy.
- **0779** — The Weft's falsified ordering was a base-rate effect in the
  recipe, not an estimator artefact (closing the registry row's hypothesis).

## 12. G3 flags

1. **Fidelity: the sign kinds get rarer.** §6 trades frequency for
   found-ness on spring and overhang. The exact counts are a consequence,
   not a target, and will only be known at calibration. The alternative —
   leave the recipes and accept a 92%-extruded sign case — is the state
   the Weft shipped.
2. **Scope: the diegetic reading verb is deferred.** Rendering signs makes
   legibility player-reachable with the player's own head; a body that
   speaks its inference is a further campaign (ledger follow-up).
3. **Cost: one rebaseline, one census refresh**, both at close, plus the
   Gnomon arms if §5 registers metrics (it does: 32, four kinds × eight readouts).
4. **Determinism: no epoch, no draw, no ledger fact** — a claim §4.5's
   branch table tests rather than asserts.
