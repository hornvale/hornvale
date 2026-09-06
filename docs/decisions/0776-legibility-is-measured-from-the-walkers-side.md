# 0776. Legibility is measured from the walker's side

**Status:** Accepted (2026-09-05) · **Campaign:** The Warp · **Decider:**
Nathan (ideonomy, 2 passes, 1 overturn) · **Relates:**
[0016](0016-studies-preregister-hypotheses.md),
[0669](0669-a-sites-tier-is-placed-or-derived.md),
[0686](0686-a-kinds-prevalence-never-normalises-against-its-siblings.md),
[0687](0687-a-derived-surface-reads-continuous-causes-never-categorical-labels.md)

In the context of The Weft having measured legibility as mutual information
between a facet's hidden macro state and its derived features — a quantity a
walker can never observe — we decided that **legibility is measured over the
tuple of rendered signs the walk-band sentence emits, where a sign is a
rendered, world-determined, at-facet, discrete token, and every channel
reading is reported net of a five-shift permutation null**, accepting that
the walker-side reading is bounded above by, and usually far below, the
world-side ceiling the Weft's estimator reports.

## Context

The Weft's H3 read `MI(cause; occurrence)`. Nothing in the game renders
`cause`. So the number was a statement about what the *world* knows, and the
campaign's own criterion — a place is *found* when a knowledgeable observer
could have predicted it from visible signs and a naive one could not — names
an observer, a channel and two predictions, none of which that estimator
modelled.

Measuring the channel instead introduces the estimator's own hazard, and it
is large. The whole room sentence read as one tuple is 5,840 classes over
11,218 land facets, and it measures 0.15–0.37 bits with a null of the same
size: a number that would have looked like legibility and is finite-sample
bias. A rich sign tuple inflates discrete mutual information by construction,
so a reading without a null is not a reading.

## Decision

Three parts, and all three are load-bearing:

1. **A sign is rendered, world-determined, at-facet and discrete.** The
   instrument reads exactly the tuple the sentence emits — biome word, rock
   word, steepness word, wetness word — and nothing else. Negating each
   property names a sibling that is deliberately *not* a sign: the hidden
   cause (the Weft's ceiling), the **false sign** (address noise wearing
   causal semantics: the descriptor noun, relief, aspect, openness), the
   remote sign (a neighbouring facet's word, the rill band, a rumor), and the
   continuous datum (`examine`'s raw °C and moisture).
2. **Every channel reading is paired with a permutation null** — the same
   statistic over the occurrence bits of the facet 1,000 … 5,000 places later
   in vertex order, averaged over five shifts. A cyclic shift is a
   permutation, so both marginals are held and only the link is broken.
   "Net" always means net of that null.
3. **The false signs and the causeless control are measured, not deleted.**
   The erratic must read zero on every channel readout net of null, and the
   noise axes must read at their nulls, or the instrument credits noise and
   the finding is about the instrument.

## Why

The null is not a formality; it is what makes the bars derivable. The χ²
approximation predicts the null's mean at `(K−1)/(2·n·ln 2)` — **0.030091
bits** for the 469-class sign tuple at seed 42 — and
`warp-channel-null-erratic` read **0.03009126**. That agreement is the
positive control that the null is the null.

It also sets the resolution. The same approximation gives the null a standard
deviation of **0.001967 bits** at 469 classes and **0.000464** at the 27-class
false-sign tuple, so the campaign's first draft of H4 — bars of ±0.001 —
would have failed its own control about a third of the time by noise alone.
The bars were reset to four null standard deviations (0.008 one-sided for the
erratic's channel net, ±0.002 for the false-sign net), fixed from the seed-42
measurement rather than recomputed per seed, before any readout seed existed.
Read out on four seeds they cleared by a factor of ≈ 2.3 at the worst
reading: erratic channel net worst 0.00134, false-sign net worst |0.00088|.

The walker-side reading is also the one that can be *acted* on. A held-out
learner over the same table recovers 0.568–0.978 of spring's channel net and
0.960–1.025 of overhang's across the readout seeds, while the false-sign
tuple's own net stays inside ±0.00088 on all sixteen kind-seed readings and
the causeless control's learner gain is negative on every seed — a table
fitted on noise must lose to the base rate, and does.

## Consequences

**The ceiling and the channel are different numbers and both stay.** Decision
0687 is untouched: a recipe still reads the continuous cause, never a rendered
word. The Weft's `weft-legibility-mi-*` columns keep measuring the ceiling;
the Warp's `warp-channel-*` columns measure what is told. Efficiency is the
ratio, and it is reported, never gated.

**A between-kind comparison in bits is not a bar** — see
[0777](0777-a-kinds-reliability-and-floor-are-authored-and-its-frequency-falls-out.md).

*Ledger: `docs/superpowers/ledgers/2026-09-05-the-warp.md` #1, #4, #5, #10;
spec §1, §2, §5. Instrument:
`windows/lab/tests/suite/warp_readout.rs`, `windows/lab/src/metrics.rs`.*
