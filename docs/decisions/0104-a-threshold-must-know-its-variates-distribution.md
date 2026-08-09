# 0104. A threshold must know the distribution of the variate it is compared against — warp at the call site, never in the shared field

**Status:** Accepted (2026-08-06) · **Decider:** Nathan · **Relates:**
[0016](0016-studies-preregister-hypotheses.md),
[0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md),
[0041](0041-libm-for-portable-transcendentals.md),
[0092](0092-derivation-at-named-sites.md)

In the context of *The Hollow* finding that the cave presence gate had compared
a probability directly against a fractal-noise sample for the whole life of the
model, facing the fact that the sample's marginal is near-Gaussian and massed
at one half rather than uniform on `[0,1]`, we decided that **a threshold
comparison is only meaningful when the distribution of the variate it reads is
known and stated**, and that **the correction belongs at the call site, warping
the sample, never inside the shared field function** — accepting a per-call-site
warp and its calibration constants as the price of not breaking the field's
other readers.

## The failure this exists to prevent

```rust
// domains/terrain/src/provider.rs — the gate, from The Lode until The Hollow
let prob  = presence_prob(cave_proneness_at(id), belt);   // a "probability"
let noise = sphere_fbm01(seed, pos, FREQ, OCTAVES);       // NOT uniform
if noise < prob { /* a cave */ }
```

`noise < prob` reads as a Bernoulli(`prob`) trial and is one **only if `noise`
is uniform on `[0,1]`**. It is not. `sphere_fbm01` at four octaves is a sum of
interpolated lattice values; its marginal is near-Gaussian and concentrated
about one half. Measured over **655,488 samples** (64 seeds × a level-5 globe):

```
mean                0.500274
sd                  0.076443
skew               -0.0103
excess kurtosis    -0.0592
range              [0.161278, 0.831510]
raw deciles on [0,1]   [0, 10, 2837, 60176, 263277, 265707, 60871, 2592, 18, 0]
```

The gate's `prob` never exceeded **0.4132** anywhere on land, so the comparison
operated entirely inside that distribution's left tail. A nominal 0.325 fired at
**0.011** — twenty-nine times low. Over 30 worlds and 469,122 land cells the
result was 0.26% prevalence, and the census put 999 of 1000 worlds in its lowest
bucket.

The defect is not a miscalibration. It is a **category error**: a value was
*named* a probability, *documented* as a probability field, and compared as one,
against a variate whose distribution nobody had measured.

**The sibling code makes the point without meaning to.** The ore point process
runs the identical gate and reads healthy — because its areal ores skip the
noise test outright, and the census's dominant commodity is salt, an areal ore,
in 98.6% of worlds. The features that appeared reliably were exactly the ones
that never asked the gate.

## The decision

1. **A threshold comparison must name the distribution of the variate it
   reads.** Where the variate is not uniform and the threshold is expressed as
   a probability, the variate is warped to uniform before the comparison. Where
   the threshold is instead calibrated against the raw marginal, the constant's
   doc comment says so, in those words, so the next reader cannot mistake it for
   a probability.
2. **The warp is monotone**, and monotonicity is the load-bearing property
   rather than a convenience. The noise field serves two purposes at once: it
   sets the presence *rate* and it makes features *cluster*. A monotone
   transform preserves the spatial ordering **exactly**, so the clustering is
   untouched by construction while the marginal is corrected. It is the unique
   operation that repairs the first purpose without touching the second — and it
   is why a uniform hash, which would also fix the rate, was rejected.
3. **The warp lives at the call site, not in the shared field function.** This
   is the half of the decision most likely to be got wrong later, and the reason
   is enumerable rather than stylistic. `sphere_fbm01` has three callers in
   `domains/terrain`:

   | Caller | How it reads the sample | Effect of warping the shared function |
   |---|---|---|
   | `cave_at` | as a probability, against a threshold | the defect — needs the warp |
   | `deposit_at` | passes it to `deposit_grade_tonnage` as a **value** | would rescale every deposit's grade and tonnage |
   | `prehuman_scar_at` | against a constant its own doc records as **calibrated against this exact marginal** | would silently invalidate a correct calibration |

   **The third caller is the one that did the honest thing**, and it is the one
   a shared-function fix would have broken. A field function returns a field;
   what a *comparison* against that field means is a property of the comparison,
   and belongs where the comparison is written.
4. **The warp's constants are measurements, pinned by a test over a pooled
   population** — not free parameters. The shape is licensed by the measurement
   rather than assumed: skew and excess kurtosis are both ~0, so a normal-CDF
   (tanh) approximation is the right family. Had the field been skewed, a
   different transform would have been required, and that was a live risk until
   it was measured.

## Consequences

- **The gate became a probability, measured.** Realized rate now tracks nominal
  to within 5.6% in the worst populated bucket, against 29× low before.
  Prevalence 0.26% → 11.93% of land; the census's floor bucket went 99.9% → 0.0%.
- **Clustering was untouched, as predicted by construction** — 96.74% → 98.52%,
  the rise being a density effect of many more placed features rather than a
  change in spatial structure. This was frozen as the campaign's *falsifier*: had
  it fallen, the monotonicity argument would have been wrong and the approach
  would have needed reconsidering.
- **The pooled population is part of the guarantee.** A single globe holds only
  ~10² independent noise blobs at this frequency, so its own mean wanders
  0.4835–0.5237 across seeds. A single-globe uniformity test fails for roughly a
  quarter of seeds **however well calibrated the transform is**, and would lead
  an implementer to fit the constants to one world. Calibration constants for a
  field are pinned against a pooled sample, never one draw.
- **A calibrated-against-the-raw-marginal threshold is legitimate and stays.**
  This decision does not require every threshold to be a probability. It
  requires that which of the two it is be *stated*.
- **The point-ore call site knowingly retains the defect**, masked by the areal
  bypass, and carries a source comment saying so. That is scope, not exemption.
- **The measurement estimator follows from the same reasoning.** For independent
  Bernoulli trials with probabilities `pᵢ`, `E[hits] = Σpᵢ`, so a calibration
  readout compares a realized rate against the **mean** nominal probability of a
  bucket, not its midpoint. A midpoint is a proxy valid only where a bucket's
  interior is evenly spread; over an exhaustive table on a bimodal field it is
  not, and it reads a correctly-calibrated gate as a 39% miss.

## See also

The mirror image of this is already registered as
`PSY-distribution-shape` — a *dispersion* authored as a width with no shape,
drawn uniform because no family was ever chosen. This record is the other
direction: a value **treated as** uniform when it is not. Both are the same
requirement seen from opposite sides, and neither is restated here.

`The Hollow` spec §§2.3, 3.2; `uniformize` in
`domains/terrain/src/features.rs`; the pooled uniformity test in the same
file's `mod tests`.
