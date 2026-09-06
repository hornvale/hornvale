# 0777. A kind's reliability and floor are authored, and its frequency falls out

**Status:** Accepted (2026-09-05) · **Campaign:** The Warp · **Decider:**
Nathan (ideonomy, 1 pass, 1 overturn) · **Relates:**
[0016](0016-studies-preregister-hypotheses.md),
[0686](0686-a-kinds-prevalence-never-normalises-against-its-siblings.md),
[0687](0687-a-derived-surface-reads-continuous-causes-never-categorical-labels.md),
[0776](0776-legibility-is-measured-from-the-walkers-side.md)

In the context of the measured finding that **333 of seed 42's 403 springs
stood on a facet with no cause at all** — the sign kind was 92% extruded by
its own recipe — we decided that a sign kind's prevalence is authored as
**`reliability · smoothstep(cause; lo, hi) + floor · noise`, with `floor`
set by intent (0.0 for both sign kinds) and the kind's frequency reported as
a consequence rather than dialled**, accepting that spring and overhang
became 2.7× and 3.5× rarer at seed 42 and that spring is absent from the
78-walk band on two of five seeds.

## Context

The Weft computes `p = abundance · (contextuality · cause + (1 −
contextuality) · noise)`. Algebraically that is `p = rate · cause + floor ·
noise` with `rate = abundance · contextuality` and `floor = abundance ·
(1 − contextuality)`: the Weft's two dials are a rotation of the two
quantities a walker-side readout actually measures.

The rotation matters because the mixture weights treat signal and noise
symmetrically while their populations do not. Spring's cause class is rare —
464 of 11,218 land facets read ≥ 0.25, about 4% — so a `contextuality` of
0.85 reads "85% caused" and yields 8% caused occurrences: the floor runs
over the other 96%
and swamps the signal. That is signal detection's base-rate effect, and the
share of a kind's occurrences standing on a cause of ≥ 0.5 is its positive
predictive value. Measured before the campaign was designed: **0.077 for
spring, 0.057 for overhang.**

Rendering the causes cannot move that number. Rendering changes what the
walker sees; it does not change where the feature is.

## Decision

For the two **sign** kinds (spring, overhang):

- **`reliability`** — how often the feature pays where the cause saturates.
- **`floor`** — how often it appears where no cause is. Authored at **0.0**;
  no band required lifting it off zero, which the calibration protocol
  required be stated if one had.
- **A soft step** — `smoothstep(cause; lo, hi)`, whose edges are the same
  thresholds the rendered words cut at, so the class a walker can name is the
  class the feature lives in. A graded `rate · cause` would not have sufficed:
  even at `floor = 0`, 10,754 low-cause facets outnumber the 122 saturated
  ones.
- **Frequency is reported, never set.** `P(Y) = reliability · E[step(cause)]
  + floor · E[noise]` over land, printed per seed as the campaign's
  consequence.

**Thicket and erratic keep the Weft's exact expression**, not a regrouped
one — `a·(c·m + (1−c)·n)` and `(a·c)·m + (a·(1−c))·n` can differ in the last
unit and an occurrence is a comparison against that value, so a regrouping
alone could flip a facet. Both are the campaign's non-regression controls and
a recorded byte-golden proves their derived output unmoved.

## Why

**It works, and the readout says by how much.** Found fraction over the four
readout seeds: spring min 0.68519, median 0.77717; overhang min 0.74348,
median 0.84622 — against a preregistered bar of 0.60 and a starting 0.077 /
0.057. Thicket, untouched, stayed inside its [0.30, 0.55] non-regression band
on every seed.

**A between-kind bar is not admissible, and this is the sharpest evidence
the project has.** The frozen H2 carried a clause requiring spring's channel
net to exceed overhang's. Mutual information in bits scales with the event's
own entropy, so calibration round 3 satisfied it by cutting overhang's
reliability to 0.16 and its frequency twelvefold — while spring's own reading
did not move by a digit (0.086464 in every round). A bar a kind can pass by
disappearing is defective; the clause was withdrawn on the calibration seed
before any readout seed was built, and the readout then showed it would have
failed 4/4 anyway, raw and normalised by each kind's `H(Y)`. Each kind is
gated on its own legibility instead, and the ordering is reported.

**Reliability sits in the middle of its passing range, not at the top.**
Three rungs were measured at seed 42: 0.50 and 0.75 hold every band, 0.90
trips the wallpaper guard (max class rate 0.76068 against a bar of 0.75).
`OVERHANG_RATE = 0.50` — the reliability dial — was chosen for H5 headroom
of 1.62× against 1.17× at 0.75, and because a sign that pays one time in two
where its cause saturates is *found*; three times in four is close to being
its cause's
restatement, which the campaign's own vocabulary calls *told*.

**The cost is real and is the fidelity trade.** Existence density at seed 42
fell 0.03593 → 0.01337 for spring and 0.07515 → 0.02148 for overhang. The
78-walk encounter rate for spring reads 0.00000 on seeds 13 and 42 and
0.01305 / 0.01577 / 0.00247 on seeds 7, 1 and 100 — encounterable on three of
the four readout seeds, absent from the walk band on two of five worlds.
Nothing was retuned to soften that: the found-fraction bar and walk-band
visibility of spring on seed 42 are mutually exclusive, because the largest
spring cause anywhere in that band is about 0.244 and the step opens at 0.35.

## Consequences

**Abundance and contextuality are gone for the sign kinds** and stay for
thicket and erratic. Decision 0686 is untouched: nothing normalises a kind's
prevalence against its siblings, and the two authored dials are still each
kind's own.

**A zero floor removes the noise term entirely for those kinds**, so the
address-hashing mutation the Weft's prevalence battery is built on cannot
move spring's or overhang's numbers. Those rows now discriminate the
continuity of the macro state — a real property, and a different one. The
claim is corrected where it is asserted rather than left to inference.

*Ledger: `docs/superpowers/ledgers/2026-09-05-the-warp.md` #3, #11, and the
Task 6 calibration sections; spec §6, §7. Constants:
`windows/worldgen/src/weft/kinds.rs`.*
