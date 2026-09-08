# The Staple D3B — gradient sufficiency design

**Status:** G3 approved 2026-09-08; diagnostic implementation complete,
pending G6 merge/close review.

## 1. Purpose and boundary

D3B is a diagnostic continuation of D3. It asks whether the current Hornvale
world computes materially different local value regimes before D4 asks how
specialization emerges.

D3B does not assign occupations, add `Function` labels, introduce persistent
portfolios, add production flows, change exchange behavior, add an epoch, or
authorize a census re-baseline. The result may be:

- **dead pole 1:** no materially usable source gradient exists;
- **dead pole 2:** source variation exists, but the current projection is
  uniform or its measurement saturates; or
- **cleared:** source variation survives into varied, causally joined local
  positions, so D4 may study emergence.

The SugarScape lesson is structural: heterogeneous opportunities, needs or
capabilities, local movement/access, production, and bilateral exchange can
produce differentiated positions without assigned occupations. Roadside
Picnic/Stalker-like anomalous, hazardous, rare, transformative, or
knowledge-gated sources remain future axes until they have a total join to the
live settlement path.

## 2. Task 0 unit and denominator

The counted unit is one live `BakeOccupation` record at `History::now`.
`Occupation::site`, `Occupation::people`, and `BakeOccupation::community` are
the join fields. The denominator for seed `s` is the existing
`census(h).alive_at_now`, not a new population or relation denominator.

The fixed seed roster is reported seed by seed. Empty seeds are explicit. A
seed with fewer than two live, phase-complete, joined units is **underpowered**;
it is not evidence for either dead pole and cannot participate in an all-seed
success verdict.

The readout must assert:

- the denominator is nonzero before counting a nonempty seed;
- every counted record is live at `History::now`;
- the source join is total and duplicate-free;
- the projection join is total and duplicate-free for every included unit;
- zero-demand units are excluded explicitly; and
- missing, disabled-treatment, conservation, and non-negativity branches are
  visible rather than converted to zeroes.

## 3. Paired instrument

Task 0 has two readouts over the same live occupation units.

### 3.1 Source support

Source support measures materially occupied opportunity before roles or
portfolios are named. Initial eligible axes are restricted to values already
on a settlement-affecting path:

- per-people local capacity (`Bake::eff_capacity`);
- water/fertility opportunity where the readout follows the existing
  consuming path, including river proximity and moisture/fertility; and
- local connectivity as a transmission/access axis, never as source value by
  itself.

The initial measurement bands reuse existing semantic bars:

| axis | bands |
| --- | --- |
| local surplus | `<= 0.4`, `(0.4, 0.6]`, `> 0.6` |
| river proximity | `0`, `(0, 1)`, `1` |
| local capacity | `< 150`, `[150, 200)`, `>= 200` |

The surplus bars are the existing culture structure gates; river endpoints
and the interior are the existing `river_proximity` semantics; 150 and 200
are existing hamlet/longhouse population bars applied to capacity, following
the Hidage precedent. These are diagnostic bands, not new domain constants.
Categorical biome/coast subsistence labels, textual site concepts, population
alone, `Function`, and `Notability` are not source evidence.

A source guard passes for an adequately powered seed only when at least two
independent eligible axes occupy at least two bands each, and at least two
joint source signatures are occupied by live units. Singleton signatures are
reported but cannot alone establish positive sufficiency.

Alchemy substrates and reachable transformations, underworld energy, hazards,
relics, and anomalous extraction are deferred until each proves a total join
to this live unit and its actual consuming path.

### 3.2 Projection realization

Projection measures what the existing D2 dynamics actually realize, not a
reconstruction from biome or labels. The primary signature is a phase-
integrated typed A/B coverage-and-shortfall vector, normalized against each
unit's fixed complementary demand. The existing basket is 0.5 A / 0.5 B.
The existing twelve phase steps are retained so seasonal harvest is not
reduced to a closing-stock snapshot.

The required companion readout records per-unit exchange access by type:
proposed, accepted, settled, partial, refused, and impossible outcomes over
the local one-hop conductance-positive network. Access is transmission, not
source value, and is not folded into a single scalar.

The current whole-bake `ExchangeCensus` is insufficient because it loses
community identity. The future diagnostic seam must therefore be zero-impact
and per-live-unit, analogous to D3's return witness. The emitted occupation
record does not currently carry these private typed stocks or local outcomes.

## 4. Comparison without invented thresholds

The diagnostic retains both:

1. the raw continuous A/B vector; and
2. a structural signature for branching.

For each coverage component, the structural band is `0`, `(0, 1)`, or `1`,
using the existing clamped coverage domain. The signature also records whether
A is below, equal to, or above B after phase integration. It therefore
distinguishes balanced adequacy, typed imbalance, and persistent deprivation
without creating role vocabulary.

Raw vectors remain visible for within-band spread. If an emitted artifact needs
stable equality, it uses the existing eight-significant-digit `quantize`
boundary; quantization is never used in the compute path and is not a new
world threshold.

## 5. Preregistered branch table

The verdict is per adequately powered, nonempty seed. Pooled totals are
descriptive only.

```text
no eligible source axis has >1 occupied band on every adequate seed
    -> DEAD POLE 1: no usable source gradient

source guard passes, but every adequate seed has one committed projection
vector/signature after measurement-saturation review
    -> DEAD POLE 2: realized projection collapse/saturation

source varies, structural signatures are equal, but committed raw vectors
vary within the signature
    -> MEASUREMENT SATURATION: report hidden spread; do not call success

source and projection guards pass on every adequate seed
    -> CLEARED: gradient sufficiency established; D4 may study emergence

some adequate seeds pass and some fail, or any seed is underpowered/missing
    -> MIXED/UNDERPOWERED: no specialization design and no D4 claim
```

The implementation must report the per-seed table, empty seeds, pooled source
and projection counts, signature sizes, raw-vector spread, and all vacuity
branches. One large seed, hub, population, degree, or exchange total cannot
rescue a flat or underpowered seed.

## 6. Non-goals and future axes

D3B does not decide whether specialization is derived, persistent, or a
history reading. It only establishes whether the current system exposes enough
varied local value gradients for that question to be meaningful.

Future fantasy axes may include subterranean energy, anomalous materials,
ruins/relics, hazards, transformation knowledge, and access constraints. Each
must first demonstrate a mechanism-backed, total live-community join and pass
the same source/projection separation, vacuity, and anti-hub rules. No future
axis may enter as a decorative label or be added solely to force D3B clear.

## 7. G3 questions

Nathan's review is requested on:

1. the conservative requirement that two independent source axes vary;
2. the use of existing 0.4/0.6, river-endpoint, and 150/200 semantic bars;
3. the phase-integrated typed vector plus separate exchange-access companion;
4. the per-seed verdict with explicit underpowered branch; and
5. the zero-impact per-community diagnostic seam required to observe D2
   realization.

No code or implementation plan should begin until this draft is approved.
