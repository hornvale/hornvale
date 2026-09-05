# 0779. The Weft's falsified ordering was a base-rate effect

**Status:** Accepted (2026-09-05) · **Campaign:** The Warp · **Decider:**
Nathan · **Relates:**
[0016](0016-studies-preregister-hypotheses.md),
[0686](0686-a-kinds-prevalence-never-normalises-against-its-siblings.md),
[0776](0776-legibility-is-measured-from-the-walkers-side.md),
[0777](0777-a-kinds-reliability-and-floor-are-authored-and-its-frequency-falls-out.md)

In the context of The Weft having left an open fork — its falsified `spring >
thicket` legibility ordering was either a property of the world or an artefact
of a four-bin mutual-information estimator — we decided, on a measurement
taken before this campaign's spec was written, that **the ordering was a
base-rate effect in the prevalence recipe and not an estimator artefact**,
accepting that the hypothesis is closed in the negative and that whatever a
finer estimator might buy elsewhere, it buys nothing here.

## Context

The Weft froze `spring > thicket > overhang > erratic` and measured
`thicket 0.038604 > spring 0.007812 > overhang 0.002497 > erratic 0.000000`.
Its close filed the fork as a registry row with an explicit prohibition: do
not retune either recipe to answer it; a follow-up needs an
estimator-resolution control or a learner-shaped measure.

## Decision

The fork resolves to **world, by way of recipe** — a base-rate effect, not a
discretization artefact. The registry row is closed in the negative.

## Why — two independent readings, both from one probe

`windows/lab/tests/suite/warp_probe.rs`, run once for 1.2 s over the Weft's
own land-eligible population at seed 42 (`n = 11,218`; spring 403, overhang
843, thicket 1,517, erratic 428), with the Weft's own H3 estimator as one row
of the table reproducing its committed readings exactly
(0.007812 / 0.002497 / 0.038604 / 0.000000) — the positive control that this
is the Weft's population.

**1. A different estimator ranks them the same way.** The nineteen-class biome
word, net of a lagged null, reads spring 0.0032 and thicket 0.0356. So a
sign with nearly five times the four-bin estimator's resolution puts thicket
above spring exactly as the four bins did. The discretization is not what
produced the ordering.

**2. The mechanism is visible in the recipe.** Binning each kind's own cause
into four equal bins over `[0,1]`:

| kind | facets in the lowest cause bin | occurrences there | share of occurrences with cause ≥ 0.5 |
| --- | ---: | ---: | ---: |
| spring | 10,754 | **333 of 403** | **0.077** |
| overhang | 7,399 | 474 of 843 | 0.057 |
| thicket | 5,901 | 392 of 1,517 | 0.427 |

**333 of 403 springs stood on a facet with no cause at all.** Spring's cause
class is 464 of 11,218 facets and the Weft recipe's floor,
`abundance × (1 − contextuality) × noise`, runs over the other 96% of land. A
small false-alarm rate over a large population outnumbers a high hit rate over
a small one — signal detection's base-rate effect. Thicket's causes are
temperature and moisture, and those *are* the biome word, so thicket was
already at its ceiling; a knowledgeable observer told the whole truth about
carbonate and drainage would still have been unable to predict 92% of springs.

**The prohibition was honoured.** No recipe was retuned to answer this. The
probe was committed and run before a line of this campaign's design existed;
the recipes that later moved (0777) moved under their own frozen readout, and
the answer here does not depend on them.

## Consequences

**A confirmation of the Weft's ordering would not have been evidence.** At the
Warp's own constants, the ordering was reproduced *exactly as preregistered*
at one rung of overhang's reliability — `spring 0.086464 > thicket 0.038604 >
overhang 0.020901 > erratic 0.000000` at `OVERHANG_RATE = 0.16` — and that
rung existed only to satisfy a between-kind clause since withdrawn. Spring's
own reading was identical (0.086464) at every rung; the whole movement was
overhang's rarity. A prediction confirmed by making a different kind twelve
times rarer is not being tested by its confirmation, which is the reason a
between-kind comparison in bits is reported and never gated (0777).

**Spring rose 11.1× against its own past** — the Weft's 0.007812 to 0.086464
at seed 42, measured on the Weft's own estimator — which is the comparison the
campaign actually claims.

**At the shipped constants the ordering is falsified again, by a different
relation**: `spring > overhang > thicket > erratic`, where `thicket >
overhang` now fails and the Weft's `thicket > spring` does not. That is a
finding recorded at its assertion, not a bar.

*Ledger: `docs/superpowers/ledgers/2026-09-05-the-warp.md` #2, #3, and
"Task 6 — fix round 2"; spec §1.2. Probe:
`windows/lab/tests/suite/warp_probe.rs`.*
