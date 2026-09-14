//! THE TRENCHER, Task 12: does the four-metabolite disaggregation buy
//! VARIETY, or is it one axis wearing four names?
//!
//! Task 4 replaced the underworld's mean-of-seven energy reduction with four
//! per-metabolite supply axes (`HYDROGEN`, `REDUCED_IRON`, `REDUCED_SULPHUR`,
//! `METHANE`) and Task 5 calibrated the ruler they project onto. Their
//! per-rung MEDIANS are published in `windows/worldgen/src/energy.rs`'s
//! `chemical_supply` doc. Nothing has ever measured their SPREAD, which axis
//! actually leads at a given chamber, or whether the four move together — and
//! those three, not the medians, decide whether a chamber can be described by
//! its chemistry.
//!
//! # This file ships no behaviour change
//!
//! Every number here is computed **inside this probe**, over the live shipped
//! fields. `energy.rs` and `windows/worldgen/src/lib.rs` are untouched. The
//! four methane arms (below) are alternative arithmetic over the same live
//! inputs, never an edit to the code that produces them — the same posture
//! `trencher_probe.rs` takes for its own four-arm calibration comparison.
//!
//! # What is measured, and on what
//!
//! Over [`Q6_SEEDS`] (the twelve seeds `subterranean_energy_probe.rs:58`
//! preregisters) at `BuildDepth::Terrain`, every cave-bearing vertex × every
//! underground rung that carries a reading:
//!
//! 1. **Distribution per axis, per rung** — min/p10/p25/median/p75/p90/max,
//!    plus mean, sd and coefficient of variation.
//! 2. **Dominance** — which of the four is largest at each reading, as a
//!    share, per rung and pooled.
//! 3. **Correlation** — Pearson and Spearman, pairwise, pooled.
//! 4. **The hydrogen hypothesis** — item 4 of the task brief: `HYDROGEN` sums
//!    two reactions peaking at *opposite* ends of the silica field
//!    (serpentinization at 0.05, radiolysis at 0.9, half-width 0.35 each), so
//!    it was predicted to be a near-constant background with low variance
//!    relative to its mean. The coefficients of variation printed by (1) are
//!    the test; the verdict is read off them and not asserted in advance.
//! 5. **How many distinct chemistries** — two definitions, both printed: the
//!    descending ORDER of the four axes (24 possible), and a four-level BAND
//!    tuple against the only authored ladder in the neighbourhood
//!    (`domains/climate/src/underworld.rs`'s `E_LEAN`/`E_FED`/`E_RICH`).
//! 6. **The four methane arms** — the brief's A/B/C/D, each scored for its
//!    own distribution and for the dominance it would produce if it shipped.
//!
//! # The common modifier, and why it is DERIVED rather than hardcoded
//!
//! `chemical_supply` multiplies all four metabolites by `1 + gain * g`, where
//! `g` is [`EnergySource::Geothermal`]'s yield. Both `GEOTHERMAL_MODIFIER_GAIN`
//! and that product are private to `energy.rs`, so this file recovers the
//! modifier from the shipped reading itself: for any axis whose *raw* (pre-
//! modifier) yield is non-trivial, `shipped / raw` is exactly the modifier.
//! [`Reading::from_site`] cross-checks every such axis against the first,
//! which makes the recovery its own positive control — a mis-summed raw axis
//! (hydrogen is the sum of TWO reactions, the one place a re-derivation can
//! silently disagree) disagrees on the ratio and fails the run rather than
//! quietly rescaling an arm.
//!
//! **Dominance and rank correlation are invariant to it**, since it is a
//! strictly positive common factor; only the magnitudes move. The arms are
//! therefore computed raw and scaled once, and the raw/post distinction is
//! reported for the Pearson matrix, where a common multiplier genuinely
//! *creates* correlation and reading only the post matrix would overstate how
//! coupled the underlying chemistry is.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen batteries.
//! `world_at`, `Q6_SEEDS`, `UNDERGROUND_RUNGS` and `pct` below are copied
//! from `subterranean_energy_probe.rs` rather than imported — test modules do
//! not share private helpers across files — and are unchanged from that
//! file's own definitions.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::math;
use hornvale_kernel::{Band, Seed, VertexMap};
use hornvale_terrain::delve::rung_evaluation_depth_m;
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::energy::{EnergySource, chemical_supply_field_per_rung};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, Substrate, WorldComponents, build_world_to_with_artifacts,
    climate_of, substrate_field, subterranean_substrate_field_per_rung,
};
use std::collections::BTreeMap;

/// The twelve seeds `subterranean_energy_probe.rs:58` preregisters as
/// `Q6_SEEDS`, reproduced in its own listed order, so this file's readout
/// describes the same world population every other Trencher probe does.
const Q6_SEEDS: [u64; 12] = [1, 7, 42, 99, 123, 256, 512, 777, 1024, 1234, 4096, 9001];

/// The five underground rungs `Band::all()` carries below `Surface` —
/// `subterranean_energy_probe.rs`'s own constant, reproduced so
/// `Band::Surface`'s always-`None` slot is never iterated.
const UNDERGROUND_RUNGS: [Band; 5] = [
    Band::Undercroft,
    Band::Shallows,
    Band::Deeps,
    Band::Underdeep,
    Band::Nadir,
];

/// The four metabolite axes, in the order every `[f64; 4]` in this file uses.
/// Matches `ChemicalSupply`'s own field order.
const AXES: [&str; 4] = ["hydrogen", "reduced_iron", "reduced_sulphur", "methane"];

/// `domains/climate/src/underworld.rs`'s authored `E_LEAN` corpus value. The
/// constants there are private to that module and there is no shared
/// classifier, so the band edges are reproduced here exactly as
/// `trencher_probe.rs` reproduces them, and for the same reason.
const E_LEAN: f64 = 0.25;

/// `domains/climate/src/underworld.rs`'s authored `E_FED` corpus value.
const E_FED: f64 = 0.5;

/// `domains/climate/src/underworld.rs`'s authored `E_RICH` corpus value.
const E_RICH: f64 = 0.75;

/// `domains/climate/src/underworld.rs`'s authored `E_TEEMING` corpus value —
/// the ladder's top rung. Task 15 needs it (unlike Task 12, which only ever
/// classified into `band_of`'s four buckets, `RICH+` being the open-ended
/// top): the coordinator's question is specifically whether GEO pushes axes
/// PAST this ceiling, which `band_of`'s `RICH+` bucket cannot distinguish
/// from merely reaching it.
const E_TEEMING: f64 = 1.0;

/// Below this a metabolite supply is treated as ABSENT rather than merely
/// small — the threshold the "is this axis ever present at all?" count uses.
/// Chosen well under the smallest published per-rung median (methane's
/// `0.025`) so a genuine trace still counts as present.
const PRESENCE_EPSILON: f64 = 1e-9;

/// Relative tolerance for the cross-axis agreement check on the recovered
/// geothermal modifier (see the module doc). Loose enough for the handful of
/// ulps three multiplies cost, tight enough that a genuinely mis-derived raw
/// axis cannot slip through.
const MODIFIER_AGREEMENT_TOLERANCE: f64 = 1e-9;

/// Build `seed_value` to `BuildDepth::Terrain` and return its terrain and
/// surface substrate field. Copied verbatim from
/// `subterranean_energy_probe.rs`'s `world_at`.
fn world_at(seed_value: u64, wc: &WorldComponents) -> (GeneratedTerrain, VertexMap<Substrate>) {
    let seed = Seed(seed_value);
    let artifacts = build_world_to_with_artifacts(
        seed,
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        BuildDepth::Terrain,
    )
    .expect("probe seed builds");
    let world = artifacts.world;
    let terrain = artifacts
        .terrain
        .expect("terrain is Some at BuildDepth::Terrain");
    let climate = climate_of(&world).expect("climate reconstructs");
    let geo = terrain.geosphere();
    let surface = substrate_field(
        geo,
        &terrain,
        &climate,
        climate.obliquity_deg(),
        climate.insolation(),
        &climate.regime(),
    );
    (terrain, surface)
}

/// Nearest-rank percentile of an ascending-sorted slice — copied verbatim
/// from `subterranean_energy_probe.rs`'s `pct`. `q` is a fraction in `[0,1]`.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let i = (((sorted.len() - 1) as f64) * q).round() as usize;
    sorted[i]
}

/// One chamber reading's four metabolite supplies, both before and after the
/// geothermal modifier, plus the three inputs the methane arms need.
#[derive(Debug, Clone, Copy)]
struct Reading {
    /// Which of [`Q6_SEEDS`] this reading came from, so between-world and
    /// within-world variety can be told apart.
    seed: u64,
    /// This chamber's rock silica fraction — the single lithological input
    /// BOTH the hydrogen and the reduced-iron axes read, recorded so the
    /// anti-correlation between them can be attributed rather than guessed at.
    silica: f64,
    /// Pre-modifier yields, axis order [`AXES`]. Hydrogen is the SUM of
    /// serpentinization and radiolysis, as `chemical_supply` sums it.
    raw: [f64; 4],
    /// The recovered `1 + gain * g` common multiplier (see the module doc).
    modifier: f64,
    /// This chamber's rock carbonate fraction — methane arm input.
    carbonate: f64,
    /// This chamber's rock porosity — methane arm input.
    porosity: f64,
    /// This chamber's metamorphic grade — the sole lithological input to the
    /// reduced-sulphur axis, recorded for the driver histogram below.
    metamorphic_grade: f64,
    /// This rung's `Substrate::moisture` — methane arm input.
    moisture: f64,
    /// [`EnergySource::Serpentinization`]'s own raw (pre-modifier) yield —
    /// stored separately from `raw[0]` (which is already `serp_raw +
    /// radiolysis_raw`, `chemical_supply`'s own hydrogen summation) because
    /// Task 15's GEO arm (below) must take the geometric-mean transform of
    /// *each source* before summing, not of their sum.
    serp_raw: f64,
    /// [`EnergySource::Radiolysis`]'s own raw yield — see `serp_raw`.
    radiolysis_raw: f64,
    /// [`EnergySource::Geothermal`]'s own raw yield — the gradient term that
    /// feeds `chemical_supply`'s `1 + gain * g` modifier. Not part of `raw`
    /// (which holds only the four routed metabolite axes) because Geothermal
    /// routes to [`SupplyRoute::Modifier`], not to an axis.
    geothermal_raw: f64,
}

impl Reading {
    /// The post-modifier supplies a consumer actually sees, axis order
    /// [`AXES`] — what `chemical_supply` returns. This IS Task 15's CONTROL
    /// arm: the shipped yield form, unmodified, re-derived from this tree's
    /// own live fields rather than cited from Task 12's stale baseline.
    fn post(&self) -> [f64; 4] {
        [
            self.raw[0] * self.modifier,
            self.raw[1] * self.modifier,
            self.raw[2] * self.modifier,
            self.raw[3] * self.modifier,
        ]
    }

    /// Task 15's GEO arm: every [`EnergySource`]'s yield replaced by the
    /// geometric mean of its own gating terms (the k-th root of its own
    /// product, k = [`source_arity`]), axis order [`AXES`]. `gain` is the
    /// geothermal modifier's gain, recovered once for the whole sample by
    /// [`recovered_geothermal_gain`] rather than re-derived per reading.
    ///
    /// Each of the six per-metabolite sources' `yield_at` arm is a PURE
    /// product of `source_arity` factors (verified by reading all seven arms
    /// — see the module doc's seven-arm table), so the k-th root of the
    /// already-computed raw yield IS the geometric mean of its factors; there
    /// is no need to decompose `bump()`/`water_gate()` by hand.
    fn geo_axes(&self, gain: f64) -> [f64; 4] {
        let h = geo_yield(EnergySource::Serpentinization, self.serp_raw)
            + geo_yield(EnergySource::Radiolysis, self.radiolysis_raw);
        let fe = geo_yield(EnergySource::IronReduction, self.raw[1]);
        let s = geo_yield(EnergySource::SulphideOxidation, self.raw[2]);
        let ch4 = geo_yield(EnergySource::Methanogenesis, self.raw[3]);
        let g = geo_yield(EnergySource::Geothermal, self.geothermal_raw);
        let modifier = 1.0 + gain * g;
        [h * modifier, fe * modifier, s * modifier, ch4 * modifier]
    }

    /// The THIRD arm: geometric mean applied ONLY to the two sources whose
    /// arity exceeds the population's mode (`SulphideOxidation`,
    /// `Methanogenesis`, both k=3) — every k=2 source (`Serpentinization`,
    /// `Radiolysis`, `IronReduction`, `Geothermal`) is left exactly as
    /// shipped, an unmodified product. This isolates the coordinator's
    /// question directly: does correcting ONLY the excess-arity sources
    /// capture GEO's benefit, or does GEO's benefit actually come from the
    /// general upward push a k-th root applies to every source it touches,
    /// including the ones that were never over-multiplied?
    ///
    /// Because Geothermal (k=2) is untouched here, its yield and therefore
    /// the modifier are IDENTICAL to CONTROL's — no independent gain recovery
    /// is needed; `self.modifier` (already recovered from shipped data) is
    /// exactly right.
    fn selective_geo_axes(&self) -> [f64; 4] {
        let h = self.raw[0]; // serp_raw + radiolysis_raw, shipped (k=2 each)
        let fe = self.raw[1]; // shipped (k=2)
        let s = geo_yield(EnergySource::SulphideOxidation, self.raw[2]); // k=3
        let ch4 = geo_yield(EnergySource::Methanogenesis, self.raw[3]); // k=3
        [
            h * self.modifier,
            fe * self.modifier,
            s * self.modifier,
            ch4 * self.modifier,
        ]
    }
}

/// The number of factors [`EnergySource::yield_at`]'s own product multiplies
/// for `source` — verified by reading all seven `yield_at` arms (Task 15,
/// ledger #39): six are 2-term products (`Serpentinization`, `IronReduction`,
/// `Radiolysis`, `Geothermal`, `DetritalImport` — the last unused by this
/// file, since it routes to `Detritus` rather than one of [`AXES`]) and two
/// are 3-term products (`SulphideOxidation`, `Methanogenesis`). **Correction
/// to ledger #39's own table**: it reads `SulphideOxidation` as "3, each
/// shaped", but `metamorphic_grade` is a raw `MaterialBuffer` fraction with
/// no `bump()`/`water_gate()` applied to it, the same as `Methanogenesis`'s
/// `carbonate` and `porosity` — so `SulphideOxidation` is a 3-term product
/// with one raw factor and two shaped ones (`front`, `water_gate`), not three
/// shaped ones. The arity — which is what the geometric-mean transform
/// actually depends on — is unaffected: both `SulphideOxidation` and
/// `Methanogenesis` are 3-term products and every other arm is 2-term.
fn source_arity(source: EnergySource) -> f64 {
    match source {
        EnergySource::SulphideOxidation | EnergySource::Methanogenesis => 3.0,
        EnergySource::Serpentinization
        | EnergySource::IronReduction
        | EnergySource::Radiolysis
        | EnergySource::Geothermal
        | EnergySource::DetritalImport => 2.0,
    }
}

/// The geometric-mean transform of one source's own raw yield: the k-th root
/// of the product `yield_at` already computed, k = [`source_arity`]. Uses
/// [`math::powf`] rather than `f64::sqrt`/`cbrt`: `cbrt` is on `clippy.toml`'s
/// disallowed-methods list (platform libm divergence, decision 0041) and a
/// single spelling covers both k=2 and k=3 — the same posture `methane_arms`'
/// arm D already takes.
fn geo_yield(source: EnergySource, raw_product: f64) -> f64 {
    math::powf(raw_product.max(0.0), 1.0 / source_arity(source))
}

/// Readings with a raw geothermal yield below this are excluded from
/// [`recovered_geothermal_gain`]'s recovery: `(modifier - 1) / geothermal_raw`
/// is numerically unstable as `geothermal_raw -> 0` (and, at exact zero,
/// `modifier` is identically `1.0` regardless of gain — unrecoverable, not
/// merely noisy).
const GAIN_RECOVERY_FLOOR: f64 = 0.3;

/// Maximum spread allowed across [`recovered_geothermal_gain`]'s recovered
/// candidates before it refuses. `chemical_supply`'s modifier is `1 + gain *
/// g` with one constant `gain` — a real spread beyond ordinary float noise
/// would mean the modifier is not that constant-gain form, which this probe
/// assumes when building the GEO arm's own modifier.
const GAIN_AGREEMENT_TOLERANCE: f64 = 1e-9;

/// Recovers `energy.rs`'s private `GEOTHERMAL_MODIFIER_GAIN` from the shipped
/// per-reading modifier ([`Reading::modifier`], itself recovered from shipped
/// data by [`collect_readings`]) and this file's own re-derived raw
/// geothermal yield ([`Reading::geothermal_raw`]) — mirroring the "recover,
/// don't hardcode" posture the module doc already uses for the modifier
/// itself, since the gain constant is private to `energy.rs` and this file
/// must not depend on its literal value. A positive control, not merely a
/// derivation: every candidate above [`GAIN_RECOVERY_FLOOR`] must agree
/// within [`GAIN_AGREEMENT_TOLERANCE`], which fails loudly if the modifier
/// were ever something other than one constant times the geothermal yield.
fn recovered_geothermal_gain(readings: &[Reading]) -> f64 {
    let mut candidates: Vec<f64> = readings
        .iter()
        .filter(|r| r.geothermal_raw > GAIN_RECOVERY_FLOOR)
        .map(|r| (r.modifier - 1.0) / r.geothermal_raw)
        .collect();
    assert!(
        candidates.len() >= 100,
        "only {} readings clear the geothermal-yield recovery floor of {GAIN_RECOVERY_FLOOR} \
         — cannot recover the modifier gain",
        candidates.len()
    );
    candidates.sort_by(f64::total_cmp);
    let gain = candidates[candidates.len() / 2];
    let max_dev = candidates
        .iter()
        .map(|c| (c - gain).abs())
        .fold(0.0_f64, f64::max);
    assert!(
        max_dev <= GAIN_AGREEMENT_TOLERANCE,
        "recovered geothermal gain disagrees by {max_dev} across {} candidate readings — the \
         shipped modifier is not the single-constant-gain form this probe's GEO arm assumes",
        candidates.len()
    );
    gain
}

/// Which index of a four-vector is largest. Ties go to the LOWER index, which
/// is deterministic and stated rather than inherited from an iterator's
/// documented behaviour; exact ties across four independent products are
/// vanishingly rare except at the all-zero reading, which is counted
/// separately by the caller.
fn argmax4(v: &[f64; 4]) -> usize {
    let mut best = 0usize;
    for (i, x) in v.iter().enumerate().skip(1) {
        if *x > v[best] {
            best = i;
        }
    }
    best
}

/// min / p10 / p25 / median / p75 / p90 / max / mean / sd / CV of a sample,
/// as one printable row. `values` is sorted in place.
fn summarize(label: &str, values: &mut [f64]) -> String {
    if values.is_empty() {
        return format!("{label:<34} n=0");
    }
    values.sort_by(f64::total_cmp);
    let n = values.len() as f64;
    let mean = values.iter().sum::<f64>() / n;
    let var = values.iter().map(|x| (x - mean) * (x - mean)).sum::<f64>() / n;
    let sd = var.sqrt();
    let cv = if mean.abs() > PRESENCE_EPSILON {
        sd / mean
    } else {
        f64::NAN
    };
    format!(
        "{label:<34} n={:<7} min={:.6} p10={:.6} p25={:.6} med={:.6} p75={:.6} p90={:.6} max={:.6} mean={:.6} sd={:.6} cv={cv:.4}",
        values.len(),
        values[0],
        pct(values, 0.10),
        pct(values, 0.25),
        pct(values, 0.50),
        pct(values, 0.75),
        pct(values, 0.90),
        values[values.len() - 1],
        mean,
        sd,
    )
}

/// Pearson product-moment correlation of two equal-length samples. `NaN` when
/// either has zero variance (a constant column has no correlation defined —
/// reported as `NaN` rather than silently as `0.0`).
fn pearson(a: &[f64], b: &[f64]) -> f64 {
    assert_eq!(a.len(), b.len(), "pearson needs paired samples");
    let n = a.len() as f64;
    let ma = a.iter().sum::<f64>() / n;
    let mb = b.iter().sum::<f64>() / n;
    let mut cov = 0.0;
    let mut va = 0.0;
    let mut vb = 0.0;
    for (x, y) in a.iter().zip(b) {
        cov += (x - ma) * (y - mb);
        va += (x - ma) * (x - ma);
        vb += (y - mb) * (y - mb);
    }
    if va <= 0.0 || vb <= 0.0 {
        return f64::NAN;
    }
    cov / (va.sqrt() * vb.sqrt())
}

/// Midrank (ties averaged) ranks of a sample, so [`pearson`] over these is
/// Spearman's rho.
fn midranks(v: &[f64]) -> Vec<f64> {
    let mut order: Vec<usize> = (0..v.len()).collect();
    order.sort_by(|&x, &y| v[x].total_cmp(&v[y]));
    let mut out = vec![0.0; v.len()];
    let mut i = 0usize;
    while i < order.len() {
        let mut j = i;
        while j + 1 < order.len() && v[order[j + 1]].total_cmp(&v[order[i]]).is_eq() {
            j += 1;
        }
        let avg = (i + j) as f64 / 2.0 + 1.0;
        for &k in &order[i..=j] {
            out[k] = avg;
        }
        i = j + 1;
    }
    out
}

/// Shannon entropy of a discrete distribution given as counts, in bits,
/// normalized by `log2(occupied buckets)` so `1.0` means "spread evenly over
/// however many buckets are used". `NaN` for fewer than two occupied buckets.
fn normalized_entropy(counts: &[usize]) -> f64 {
    let total: usize = counts.iter().sum();
    let occupied = counts.iter().filter(|c| **c > 0).count();
    if total == 0 || occupied < 2 {
        return f64::NAN;
    }
    let mut h = 0.0;
    for &c in counts {
        if c > 0 {
            let p = c as f64 / total as f64;
            h -= p * math::ln(p);
        }
    }
    h / math::ln(occupied as f64)
}

/// The band a supply falls in against `underworld.rs`'s authored ladder:
/// `0` below `E_LEAN`, `1` in `[E_LEAN, E_FED)`, `2` in `[E_FED, E_RICH)`,
/// `3` at or above `E_RICH`.
fn band_of(x: f64) -> usize {
    if x < E_LEAN {
        0
    } else if x < E_FED {
        1
    } else if x < E_RICH {
        2
    } else {
        3
    }
}

/// The five-rung classification Task 15 needs and [`band_of`] cannot give:
/// `band_of`'s top bucket is open-ended (`RICH+`, everything `>= E_RICH`), so
/// it cannot tell "reaches `E_RICH`" from "exceeds `E_TEEMING`" — exactly the
/// distinction the coordinator's question turns on (does GEO merely reach the
/// top of the ladder, or push past it the way `EnvironmentVector::new`'s
/// `[0,1]` contract would refuse?). `0` below `E_LEAN`, `1` in
/// `[E_LEAN, E_FED)`, `2` in `[E_FED, E_RICH)`, `3` in `[E_RICH, E_TEEMING)`,
/// `4` at or above `E_TEEMING`.
fn ladder_band(x: f64) -> usize {
    if x < E_LEAN {
        0
    } else if x < E_FED {
        1
    } else if x < E_RICH {
        2
    } else if x < E_TEEMING {
        3
    } else {
        4
    }
}

/// Human-readable labels for [`ladder_band`]'s five indices, in order.
const LADDER_BAND_LABELS: [&str; 5] = [
    "below LEAN",
    "LEAN-FED",
    "FED-RICH",
    "RICH-TEEMING",
    "at/above TEEMING",
];

/// One initial per axis, in [`AXES`] order, for the ordering key. **Not the
/// axis names' own first letters**: `reduced_iron` and `reduced_sulphur` both
/// begin with `r`, so a key built from first letters collapses distinct
/// orderings into one string and undercounts the realized set — found by
/// running this file, which reported "12 of 24" against a key with only 12
/// distinguishable values.
const AXIS_INITIALS: [char; 4] = ['H', 'F', 'S', 'M'];

/// The descending order of a four-vector, as a four-character key over
/// [`AXIS_INITIALS`] — which axis leads, which is second, and so on. Ties
/// break by axis order, the same rule [`argmax4`] uses. 24 keys are possible.
fn order_key(v: &[f64; 4]) -> String {
    let mut idx = [0usize, 1, 2, 3];
    idx.sort_by(|&a, &b| v[b].total_cmp(&v[a]).then(a.cmp(&b)));
    idx.iter().map(|i| AXIS_INITIALS[*i]).collect()
}

/// The ratio of a reading's largest metabolite supply to its second largest —
/// how far ahead the leader is. `f64::INFINITY` when the runner-up is zero and
/// the leader is not (a single-metabolite chamber, maximally describable);
/// `NaN` when the reading supplies nothing at all.
///
/// This is item 5's operative statistic, not a decoration: a leader at 1.01x
/// the runner-up gives an author nothing to say, however many distinct
/// orderings the population realizes.
fn leader_margin(v: &[f64; 4]) -> f64 {
    let mut s = *v;
    s.sort_by(|a, b| b.total_cmp(a));
    if s[0] <= PRESENCE_EPSILON {
        return f64::NAN;
    }
    if s[1] <= PRESENCE_EPSILON {
        return f64::INFINITY;
    }
    s[0] / s[1]
}

/// The absolute difference between a reading's largest and second-largest
/// metabolite supply — leader minus runner-up. `NaN` when the reading
/// supplies nothing at all (same guard as [`leader_margin`]); otherwise
/// always finite and `>= 0`, including the single-metabolite case where it
/// equals the leader outright (no `INFINITY` branch needed, unlike the
/// ratio).
///
/// **Task 15 needs this, not [`leader_margin`], to adjudicate ledger #39's
/// H4.** H4's threshold (`0.02`) is stated on the scale the axis VALUES
/// themselves occupy (post-modifier metabolite supplies run roughly
/// `[0, 1.6]` in this sample), and [`leader_margin`]'s ratio is bounded below
/// by `1.0` by construction — the leader is sorted first — so a `>= 0.02`
/// bound on the ratio holds at *every* reading regardless of how compressed
/// the four axes are, making it a vacuous test of H4 rather than the
/// discriminating one the ledger's prose describes ("all four axes read
/// nearly equal everywhere"). The difference is the only reading under which
/// H4 can fail, and is what [`report_task15_yield_form_measurement`] uses.
fn leader_margin_diff(v: &[f64; 4]) -> f64 {
    let mut s = *v;
    s.sort_by(|a, b| b.total_cmp(a));
    if s[0] <= PRESENCE_EPSILON {
        return f64::NAN;
    }
    s[0] - s[1]
}

/// The four candidate methane yields at one reading, in the brief's own
/// order: A as shipped (`carbonate * porosity * moisture`), B
/// `min(carbonate, porosity) * moisture`, C `min(carbonate, porosity,
/// moisture)`, D the geometric mean of the three.
///
/// D uses [`math::powf`] rather than `f64::cbrt`: `cbrt` is on `clippy.toml`'s
/// disallowed list (platform libm divergence, decision 0041) and
/// `kernel/src/math.rs` ships no wrapper for it, so the portable third power
/// is the determinism-correct spelling of the same quantity for a
/// non-negative argument.
fn methane_arms(carbonate: f64, porosity: f64, moisture: f64) -> [f64; 4] {
    let product = carbonate * porosity * moisture;
    [
        product,
        carbonate.min(porosity) * moisture,
        carbonate.min(porosity).min(moisture),
        math::powf(product.max(0.0), 1.0 / 3.0),
    ]
}

/// Collect every cave-bearing vertex-rung reading over [`Q6_SEEDS`], as
/// `[Vec<Reading>; 6]` indexed by `Band as usize` (index 0, `Surface`, stays
/// empty — `chemical_supply_field_per_rung` never populates it).
fn collect_readings(wc: &WorldComponents) -> [Vec<Reading>; 6] {
    let mut per_rung: [Vec<Reading>; 6] = Default::default();

    for &seed_value in &Q6_SEEDS {
        let (terrain, surface) = world_at(seed_value, wc);
        let geo = terrain.geosphere();
        let moisture_field = subterranean_substrate_field_per_rung(geo, &terrain, &surface);
        let supply = chemical_supply_field_per_rung(geo, &terrain, &moisture_field);

        for vertex in geo.vertices() {
            let Some(cave) = terrain.cave_at(vertex) else {
                continue;
            };
            let material = terrain.material_at(vertex);
            let gradient = terrain.geothermal_gradient_at(vertex);
            let drainage = terrain.drainage_at(vertex);
            let entry = supply.get(vertex);

            for &rung in &UNDERGROUND_RUNGS {
                let idx = rung as usize;
                let Some(shipped) = entry[idx] else {
                    continue;
                };
                let Some(depth_m) = rung_evaluation_depth_m(rung, gradient, cave.depth_reach_m)
                else {
                    continue;
                };
                let Some(sub) = moisture_field.get(vertex)[idx] else {
                    continue;
                };

                let y = |s: EnergySource| {
                    s.yield_at(&material, gradient, depth_m, sub.moisture, drainage)
                };
                let serp_raw = y(EnergySource::Serpentinization);
                let radiolysis_raw = y(EnergySource::Radiolysis);
                let geothermal_raw = y(EnergySource::Geothermal);
                let raw = [
                    serp_raw + radiolysis_raw,
                    y(EnergySource::IronReduction),
                    y(EnergySource::SulphideOxidation),
                    y(EnergySource::Methanogenesis),
                ];
                let shipped_axes = [
                    shipped.hydrogen,
                    shipped.reduced_iron,
                    shipped.reduced_sulphur,
                    shipped.methane,
                ];

                // Recover the common modifier, and cross-check every axis
                // that can speak to it. See the module doc: this is the
                // positive control on the raw re-derivation above.
                let mut modifier: Option<f64> = None;
                for k in 0..4 {
                    if raw[k] > PRESENCE_EPSILON {
                        let m = shipped_axes[k] / raw[k];
                        match modifier {
                            None => modifier = Some(m),
                            Some(first) => assert!(
                                (m - first).abs() <= MODIFIER_AGREEMENT_TOLERANCE * first.max(1.0),
                                "axis {} recovers modifier {m} against {first} from an earlier \
                                 axis at seed {seed_value}, rung {rung:?} — the raw re-derivation \
                                 disagrees with the shipped field",
                                AXES[k]
                            ),
                        }
                    }
                }

                per_rung[idx].push(Reading {
                    seed: seed_value,
                    silica: material.silica,
                    raw,
                    // Every arm vanishes wherever raw methane does (all three
                    // alternatives are zero whenever `carbonate * porosity *
                    // moisture` is), so an all-zero reading's modifier is
                    // unobservable AND irrelevant; 1.0 is the identity.
                    modifier: modifier.unwrap_or(1.0),
                    carbonate: material.carbonate,
                    porosity: material.porosity,
                    metamorphic_grade: material.metamorphic_grade,
                    moisture: sub.moisture,
                    serp_raw,
                    radiolysis_raw,
                    geothermal_raw,
                });
            }
        }
    }

    per_rung
}

/// claim: readout(off-gate, prints every number Task 12's brief requires
/// before drawing any verdict; asserts only the sample is non-vacuous and the
/// modifier recovery is self-consistent) — over the frozen twelve-seed set at
/// `BuildDepth::Terrain`.
///
/// **No preregistered prediction is asserted here, deliberately.** The brief's
/// item 4 (hydrogen's coefficient of variation is low relative to its
/// siblings') is stated so it can fail, and the way it fails is by the
/// printed CV column disagreeing with it — an assertion encoding the
/// prediction would have to be retuned to whatever was measured, which is the
/// practice decision 0016 forbids. `task-12-report.md` records the verdict.
#[test]
#[ignore = "probe: the four-metabolite variety readout over twelve seeds at BuildDepth::Terrain (twelve world builds); run by hand (The Trencher, Task 12)"]
fn report_the_metabolite_variety_measurement() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let per_rung = collect_readings(&wc);

    let pooled: Vec<Reading> = per_rung.iter().flatten().copied().collect();
    assert!(
        pooled.len() > 10_000,
        "only {} readings across {} seeds — vacuous",
        pooled.len(),
        Q6_SEEDS.len()
    );
    println!("== sample ==");
    println!("readings pooled: {}", pooled.len());
    for &rung in &UNDERGROUND_RUNGS {
        println!("  {rung:?}: {}", per_rung[rung as usize].len());
    }

    // ---- 1. distribution per axis, per rung, and pooled ---------------
    println!("\n== 1. distribution per axis (POST-modifier: what a consumer sees) ==");
    for (k, name) in AXES.iter().enumerate() {
        for &rung in &UNDERGROUND_RUNGS {
            let mut v: Vec<f64> = per_rung[rung as usize]
                .iter()
                .map(|r| r.post()[k])
                .collect();
            println!("{}", summarize(&format!("{name} @ {rung:?}"), &mut v));
        }
        let mut v: Vec<f64> = pooled.iter().map(|r| r.post()[k]).collect();
        println!("{}", summarize(&format!("{name} @ POOLED"), &mut v));
        println!();
    }

    println!("== 1b. distribution per axis (RAW: before the geothermal modifier) ==");
    for (k, name) in AXES.iter().enumerate() {
        let mut v: Vec<f64> = pooled.iter().map(|r| r.raw[k]).collect();
        println!("{}", summarize(&format!("{name} raw @ POOLED"), &mut v));
    }
    let mut modifiers: Vec<f64> = pooled.iter().map(|r| r.modifier).collect();
    println!(
        "{}",
        summarize("geothermal modifier @ POOLED", &mut modifiers)
    );

    println!("\n== 1c. presence: share of readings above {PRESENCE_EPSILON:e} ==");
    for (k, name) in AXES.iter().enumerate() {
        let present = pooled
            .iter()
            .filter(|r| r.post()[k] > PRESENCE_EPSILON)
            .count();
        println!(
            "{name:<20} present at {present}/{} = {:.4}",
            pooled.len(),
            present as f64 / pooled.len() as f64
        );
    }

    // ---- 2. DOMINANCE -------------------------------------------------
    println!("\n== 2. DOMINANCE: which axis is largest at each reading ==");
    let mut pooled_hist = [0usize; 4];
    for &rung in &UNDERGROUND_RUNGS {
        let sample = &per_rung[rung as usize];
        let mut hist = [0usize; 4];
        for r in sample {
            hist[argmax4(&r.post())] += 1;
        }
        let n = sample.len().max(1) as f64;
        let shares: Vec<String> = AXES
            .iter()
            .enumerate()
            .map(|(k, name)| format!("{name}={:.4}", hist[k] as f64 / n))
            .collect();
        println!("{rung:?}: {}", shares.join("  "));
        for k in 0..4 {
            pooled_hist[k] += hist[k];
        }
    }
    let n = pooled.len() as f64;
    let shares: Vec<String> = AXES
        .iter()
        .enumerate()
        .map(|(k, name)| format!("{name}={:.4}", pooled_hist[k] as f64 / n))
        .collect();
    println!("POOLED: {}", shares.join("  "));
    let top = argmax4(&[
        pooled_hist[0] as f64,
        pooled_hist[1] as f64,
        pooled_hist[2] as f64,
        pooled_hist[3] as f64,
    ]);
    println!(
        "pooled leader: {} with {:.4} of all readings",
        AXES[top],
        pooled_hist[top] as f64 / n
    );

    // ---- 3. CORRELATION -----------------------------------------------
    println!("\n== 3. CORRELATION (pooled) ==");
    let post_cols: Vec<Vec<f64>> = (0..4)
        .map(|k| pooled.iter().map(|r| r.post()[k]).collect())
        .collect();
    let raw_cols: Vec<Vec<f64>> = (0..4)
        .map(|k| pooled.iter().map(|r| r.raw[k]).collect())
        .collect();
    let rank_cols: Vec<Vec<f64>> = post_cols.iter().map(|c| midranks(c)).collect();

    println!("Pearson, POST-modifier (what a consumer sees):");
    for i in 0..4 {
        let row: Vec<String> = (0..4)
            .map(|j| format!("{:>8.4}", pearson(&post_cols[i], &post_cols[j])))
            .collect();
        println!("  {:<18} {}", AXES[i], row.join(" "));
    }
    println!("Pearson, RAW (modifier removed — the underlying chemistry):");
    for i in 0..4 {
        let row: Vec<String> = (0..4)
            .map(|j| format!("{:>8.4}", pearson(&raw_cols[i], &raw_cols[j])))
            .collect();
        println!("  {:<18} {}", AXES[i], row.join(" "));
    }
    println!("Spearman, POST-modifier (rank; invariant to the common factor):");
    for i in 0..4 {
        let row: Vec<String> = (0..4)
            .map(|j| format!("{:>8.4}", pearson(&rank_cols[i], &rank_cols[j])))
            .collect();
        println!("  {:<18} {}", AXES[i], row.join(" "));
    }

    // ---- 5. HOW MANY DISTINCT CHEMISTRIES -----------------------------
    println!("\n== 5. LEADER MARGIN: how far ahead the largest axis is ==");
    let margins: Vec<f64> = pooled.iter().map(|r| leader_margin(&r.post())).collect();
    let mut finite: Vec<f64> = margins.iter().copied().filter(|m| m.is_finite()).collect();
    let infinite = margins.iter().filter(|m| m.is_infinite()).count();
    let undefined = margins.iter().filter(|m| m.is_nan()).count();
    println!(
        "single-metabolite readings (runner-up is zero): {infinite} = {:.4}; nothing supplied at all: {undefined}",
        infinite as f64 / n
    );
    println!("{}", summarize("leader/second (finite only)", &mut finite));
    for threshold in [1.25_f64, 1.5, 2.0, 4.0] {
        let over = margins.iter().filter(|m| **m >= threshold).count();
        println!(
            "  leader >= {threshold:.2}x runner-up: {over} = {:.4}",
            over as f64 / n
        );
    }
    println!("per-axis: readings that axis leads, and leads by >= 2x:");
    for (k, name) in AXES.iter().enumerate() {
        let leads = pooled.iter().filter(|r| argmax4(&r.post()) == k).count();
        let clear = pooled
            .iter()
            .filter(|r| argmax4(&r.post()) == k && leader_margin(&r.post()) >= 2.0)
            .count();
        println!(
            "  {name:<18} leads {leads} ({:.4})   clearly {clear} ({:.4})",
            leads as f64 / n,
            clear as f64 / n
        );
    }

    println!("\n== 5a. distinct chemistries by ORDER (descending rank of the four axes) ==");
    let mut orders: BTreeMap<String, usize> = BTreeMap::new();
    for r in &pooled {
        *orders.entry(order_key(&r.post())).or_insert(0) += 1;
    }
    let mut ordered: Vec<(&String, &usize)> = orders.iter().collect();
    ordered.sort_by(|a, b| b.1.cmp(a.1).then(a.0.cmp(b.0)));
    println!("distinct orders realized: {} of 24 possible", ordered.len());
    for (key, count) in &ordered {
        println!("  {key}  {:.4}  (n={count})", **count as f64 / n);
    }
    let order_counts: Vec<usize> = ordered.iter().map(|(_, c)| **c).collect();
    println!(
        "normalized entropy over realized orders: {:.4}",
        normalized_entropy(&order_counts)
    );

    println!("\n== 5b. distinct chemistries by BAND tuple (E_LEAN/E_FED/E_RICH per axis) ==");
    let mut buckets: BTreeMap<[usize; 4], usize> = BTreeMap::new();
    for r in &pooled {
        let p = r.post();
        let key = [band_of(p[0]), band_of(p[1]), band_of(p[2]), band_of(p[3])];
        *buckets.entry(key).or_insert(0) += 1;
    }
    let mut bucket_rows: Vec<([usize; 4], usize)> = buckets.into_iter().collect();
    bucket_rows.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
    println!(
        "distinct band buckets occupied: {} of 256 possible",
        bucket_rows.len()
    );
    let mut cumulative = 0usize;
    for (key, count) in bucket_rows.iter().take(20) {
        cumulative += count;
        println!(
            "  H{} Fe{} S{} C{}   {:.4}  (cum {:.4}, n={count})",
            key[0],
            key[1],
            key[2],
            key[3],
            *count as f64 / n,
            cumulative as f64 / n
        );
    }
    let bucket_counts: Vec<usize> = bucket_rows.iter().map(|(_, c)| *c).collect();
    println!(
        "normalized entropy over occupied buckets: {:.4}",
        normalized_entropy(&bucket_counts)
    );
    println!("per-axis band occupancy (share of readings in each band):");
    for (k, name) in AXES.iter().enumerate() {
        let mut b = [0usize; 4];
        for r in &pooled {
            b[band_of(r.post()[k])] += 1;
        }
        println!(
            "  {name:<18} <lean={:.4} lean={:.4} fed={:.4} rich+={:.4}",
            b[0] as f64 / n,
            b[1] as f64 / n,
            b[2] as f64 / n,
            b[3] as f64 / n
        );
    }

    // ---- 5c. what the four axes actually READ -------------------------
    println!("\n== 5c. the lithological inputs behind the four axes ==");
    let mut silica: Vec<f64> = pooled.iter().map(|r| r.silica).collect();
    println!("{}", summarize("silica", &mut silica));
    let mut carb: Vec<f64> = pooled.iter().map(|r| r.carbonate).collect();
    println!("{}", summarize("carbonate", &mut carb));
    let mut poro: Vec<f64> = pooled.iter().map(|r| r.porosity).collect();
    println!("{}", summarize("porosity", &mut poro));
    let mut grade: Vec<f64> = pooled.iter().map(|r| r.metamorphic_grade).collect();
    println!("{}", summarize("metamorphic_grade", &mut grade));
    let mut moist: Vec<f64> = pooled.iter().map(|r| r.moisture).collect();
    println!("{}", summarize("moisture", &mut moist));
    // Deciles of each driver, because the percentile row above cannot show a
    // BIMODAL input as anything other than a wide one, and at least two of
    // these are bimodal (carbonate is strictly two-valued — `lithology.rs`'s
    // `carbonate_at` returns 0.7 or 0.05 and nothing else).
    println!("driver histograms, ten equal bins over [0,1] (share of readings):");
    for (name, values) in [
        ("silica", &silica),
        ("carbonate", &carb),
        ("porosity", &poro),
        ("metamorphic_grade", &grade),
        ("moisture", &moist),
    ] {
        let mut bins = [0usize; 10];
        for x in values {
            let b = ((x * 10.0) as usize).min(9);
            bins[b] += 1;
        }
        let row: Vec<String> = bins
            .iter()
            .map(|c| format!("{:.3}", *c as f64 / n))
            .collect();
        println!("  {name:<18} {}", row.join(" "));
    }
    // How many values each driver actually TAKES. A driver with a handful of
    // realized values cannot hand a metabolite a continuous range however
    // wide its min..max looks, and `carbonate_at` (`lithology.rs`) returning
    // only `0.7` or `0.05` is the clearest case — but it was not the only
    // one, which is why this is counted rather than assumed.
    println!("distinct realized values per driver (exact bit equality):");
    for (name, values) in [
        ("silica", &silica),
        ("carbonate", &carb),
        ("porosity", &poro),
        ("metamorphic_grade", &grade),
        ("moisture", &moist),
    ] {
        let mut seen: std::collections::BTreeSet<u64> = std::collections::BTreeSet::new();
        for x in values {
            seen.insert(x.to_bits());
        }
        println!(
            "  {name:<18} {} distinct of {} readings",
            seen.len(),
            n as usize
        );
    }
    // The three silica bumps are centred at 0.05 / 0.45 / 0.90 with half-width
    // 0.35, so serpentinization spans (-0.30, 0.40), iron reduction (0.10,
    // 0.80) and radiolysis (0.55, 1.25): hydrogen is structurally ZERO for
    // silica in [0.40, 0.55], the one window neither hydrogen band covers.
    let dead = pooled
        .iter()
        .filter(|r| (0.40..=0.55).contains(&r.silica))
        .count();
    println!(
        "readings with silica in [0.40, 0.55] (the hydrogen-dead window): {dead} = {:.4}",
        dead as f64 / n
    );

    println!("\n== 5d. dominance PER SEED (is the variety within a world or between worlds?) ==");
    for &seed_value in &Q6_SEEDS {
        let sample: Vec<&Reading> = pooled.iter().filter(|r| r.seed == seed_value).collect();
        let mut hist = [0usize; 4];
        for r in &sample {
            hist[argmax4(&r.post())] += 1;
        }
        let m = sample.len().max(1) as f64;
        let shares: Vec<String> = AXES
            .iter()
            .enumerate()
            .map(|(k, name)| format!("{name}={:.4}", hist[k] as f64 / m))
            .collect();
        let mut orders: BTreeMap<String, usize> = BTreeMap::new();
        for r in &sample {
            *orders.entry(order_key(&r.post())).or_insert(0) += 1;
        }
        println!(
            "seed {seed_value:<5} n={:<6} {}  orders={}",
            sample.len(),
            shares.join("  "),
            orders.len()
        );
    }

    // ---- 6. THE FOUR METHANE ARMS -------------------------------------
    println!("\n== 6. METHANE, four arms (A shipped, B/C/D candidates) ==");
    let arm_names = [
        "A carbonate*porosity*moisture (shipped)",
        "B min(c,p)*m",
        "C min(c,p,m)",
        "D cbrt(c*p*m)",
    ];
    for (a, arm_name) in arm_names.iter().enumerate() {
        let mut v: Vec<f64> = pooled
            .iter()
            .map(|r| methane_arms(r.carbonate, r.porosity, r.moisture)[a] * r.modifier)
            .collect();
        println!("{}", summarize(arm_name, &mut v));
    }
    println!("\narm A control: shipped methane column, for byte comparison against arm A above");
    let mut shipped_methane: Vec<f64> = pooled.iter().map(|r| r.post()[3]).collect();
    println!("{}", summarize("shipped methane", &mut shipped_methane));

    println!("\ndominance under each arm (the other three axes held as shipped):");
    for (a, arm_name) in arm_names.iter().enumerate() {
        let mut hist = [0usize; 4];
        let mut per_rung_hist: [[usize; 4]; 6] = [[0usize; 4]; 6];
        for (idx, sample) in per_rung.iter().enumerate() {
            for r in sample {
                let p = r.post();
                let arm = methane_arms(r.carbonate, r.porosity, r.moisture)[a] * r.modifier;
                let w = argmax4(&[p[0], p[1], p[2], arm]);
                hist[w] += 1;
                per_rung_hist[idx][w] += 1;
            }
        }
        let shares: Vec<String> = AXES
            .iter()
            .enumerate()
            .map(|(k, name)| format!("{name}={:.4}", hist[k] as f64 / n))
            .collect();
        println!("  {arm_name}");
        println!("    POOLED: {}", shares.join("  "));
        for &rung in &UNDERGROUND_RUNGS {
            let h = per_rung_hist[rung as usize];
            let m = per_rung[rung as usize].len().max(1) as f64;
            println!(
                "    {rung:?}: methane wins {:.4} ({} readings)",
                h[3] as f64 / m,
                h[3]
            );
        }
    }

    println!("\narm zero-set agreement (every alternative vanishes where A does):");
    let mut a_zero = 0usize;
    let mut disagree = 0usize;
    for r in &pooled {
        let arms = methane_arms(r.carbonate, r.porosity, r.moisture);
        if arms[0] <= PRESENCE_EPSILON {
            a_zero += 1;
            if arms[1..].iter().any(|x| *x > PRESENCE_EPSILON) {
                disagree += 1;
            }
        }
    }
    println!("  readings where arm A is ~0: {a_zero}; of those, B/C/D non-zero at {disagree}");
}

/// One arm's summary statistics over the pooled sample, printed by
/// [`report_task15_yield_form_measurement`] and also returned so the H1-H5
/// verdicts can be computed from the same numbers the printout shows rather
/// than a second, silently-divergent calculation.
struct ArmSummary {
    /// Pooled dominance share per axis, [`AXES`] order.
    dominance: [f64; 4],
    /// `dominance`'s max minus its min.
    spread: f64,
    /// How many of `Band::Undercroft`'s readings this arm's reduced-sulphur
    /// axis (index 2) wins.
    sulphur_undercroft_wins: usize,
    /// Bit-exact distinct value count per axis over the whole pooled sample.
    distinct: [usize; 4],
    /// Median leader-margin RATIO (leader/second, [`leader_margin`]) over
    /// finite readings. Reported for continuity with Task 12's own
    /// "leader >= Nx runner-up" framing; NOT what H4 is adjudicated against
    /// (see [`leader_margin_diff`]'s doc).
    median_leader_margin_ratio: f64,
    /// Median leader-margin DIFFERENCE (leader minus second,
    /// [`leader_margin_diff`]) over non-`NaN` readings — the quantity ledger
    /// #39's H4 threshold (`0.02`) is actually stated on.
    median_leader_margin_diff: f64,
    /// Per-axis p10, [`AXES`] order.
    axis_p10: [f64; 4],
    /// Per-axis median, [`AXES`] order — what decides the ladder-band
    /// classification in item 1 of the coordinator's question.
    axis_median: [f64; 4],
    /// Per-axis p90, [`AXES`] order.
    axis_p90: [f64; 4],
    /// Per-axis fraction of readings that are EXACTLY zero, [`AXES`] order.
    axis_zero_frac: [f64; 4],
    /// Per-axis fraction of readings that EXCEED `1.0` (`E_TEEMING`),
    /// [`AXES`] order — item 2 of the coordinator's question:
    /// `EnvironmentVector::new` rejects anything outside `[0,1]`, so this is
    /// how much of each axis would be out of range if handed to it raw.
    axis_over_teeming_frac: [f64; 4],
}

/// Computes and prints one arm's full Task 15 measurement (dominance overall
/// and per rung, spread, Undercroft sulphur wins, distinct-value counts,
/// leader-margin distribution, and per-axis value distribution), returning
/// the subset [`ArmSummary`] needs for the H1-H5 verdicts. `axes_of` is the
/// arm's own axis function — [`Reading::post`] for CONTROL, [`Reading::geo_axes`]
/// (closed over the recovered gain) for GEO.
fn measure_arm(
    label: &str,
    pooled: &[Reading],
    per_rung: &[Vec<Reading>; 6],
    axes_of: impl Fn(&Reading) -> [f64; 4],
) -> ArmSummary {
    println!("\n---- arm: {label} ----");
    let n = pooled.len() as f64;

    // 1. dominance, pooled and per rung.
    let mut pooled_hist = [0usize; 4];
    let mut rung_hist: [[usize; 4]; 6] = [[0usize; 4]; 6];
    for &rung in &UNDERGROUND_RUNGS {
        let idx = rung as usize;
        for r in &per_rung[idx] {
            let w = argmax4(&axes_of(r));
            pooled_hist[w] += 1;
            rung_hist[idx][w] += 1;
        }
    }
    let dominance: [f64; 4] = std::array::from_fn(|k| pooled_hist[k] as f64 / n);
    let row: Vec<String> = AXES
        .iter()
        .zip(dominance)
        .map(|(name, s)| format!("{name}={s:.4}"))
        .collect();
    println!("dominance POOLED: {}", row.join("  "));
    let spread = dominance.iter().copied().fold(f64::MIN, f64::max)
        - dominance.iter().copied().fold(f64::MAX, f64::min);
    println!("spread (max dominance - min dominance): {spread:.4}");

    println!("dominance per rung:");
    for &rung in &UNDERGROUND_RUNGS {
        let idx = rung as usize;
        let m = per_rung[idx].len().max(1) as f64;
        let row: Vec<String> = AXES
            .iter()
            .enumerate()
            .map(|(k, name)| format!("{name}={:.4}", rung_hist[idx][k] as f64 / m))
            .collect();
        println!("  {rung:?} (n={}): {}", per_rung[idx].len(), row.join("  "));
    }
    let undercroft_idx = Band::Undercroft as usize;
    let sulphur_undercroft_wins = rung_hist[undercroft_idx][2];
    println!(
        "sulphur wins at Undercroft: {sulphur_undercroft_wins} of {}",
        per_rung[undercroft_idx].len()
    );

    // 2. distinct-value count per axis, bit-exact, over the whole sample.
    let mut distinct = [0usize; 4];
    for (k, name) in AXES.iter().enumerate() {
        let mut seen: std::collections::BTreeSet<u64> = std::collections::BTreeSet::new();
        for r in pooled {
            seen.insert(axes_of(r)[k].to_bits());
        }
        distinct[k] = seen.len();
        println!("distinct {name}: {}", distinct[k]);
    }

    // 3. leader-margin distribution — both readings (see leader_margin_diff's
    // doc for why the RATIO cannot adjudicate H4).
    let ratios: Vec<f64> = pooled.iter().map(|r| leader_margin(&axes_of(r))).collect();
    let mut finite_ratio: Vec<f64> = ratios.iter().copied().filter(|m| m.is_finite()).collect();
    finite_ratio.sort_by(f64::total_cmp);
    let median_leader_margin_ratio = pct(&finite_ratio, 0.50);
    println!(
        "leader margin RATIO (finite only, n={}): p10={:.6} p25={:.6} median={:.6} p75={:.6} \
         p90={:.6}",
        finite_ratio.len(),
        pct(&finite_ratio, 0.10),
        pct(&finite_ratio, 0.25),
        median_leader_margin_ratio,
        pct(&finite_ratio, 0.75),
        pct(&finite_ratio, 0.90),
    );

    let diffs: Vec<f64> = pooled
        .iter()
        .map(|r| leader_margin_diff(&axes_of(r)))
        .collect();
    let mut finite_diff: Vec<f64> = diffs.iter().copied().filter(|m| !m.is_nan()).collect();
    finite_diff.sort_by(f64::total_cmp);
    let median_leader_margin_diff = pct(&finite_diff, 0.50);
    println!(
        "leader margin DIFFERENCE (non-NaN only, n={}): p10={:.6} p25={:.6} median={:.6} \
         p75={:.6} p90={:.6}",
        finite_diff.len(),
        pct(&finite_diff, 0.10),
        pct(&finite_diff, 0.25),
        median_leader_margin_diff,
        pct(&finite_diff, 0.75),
        pct(&finite_diff, 0.90),
    );

    // 4. per-axis value distribution, including the >E_TEEMING fraction the
    // coordinator's item 2 asks for.
    println!("per-axis value distribution:");
    let mut axis_p10 = [0.0; 4];
    let mut axis_median = [0.0; 4];
    let mut axis_p90 = [0.0; 4];
    let mut axis_zero_frac = [0.0; 4];
    let mut axis_over_teeming_frac = [0.0; 4];
    for (k, name) in AXES.iter().enumerate() {
        let mut v: Vec<f64> = pooled.iter().map(|r| axes_of(r)[k]).collect();
        let zero_frac = v.iter().filter(|x| **x == 0.0).count() as f64 / n;
        let over_teeming_frac = v.iter().filter(|x| **x > E_TEEMING).count() as f64 / n;
        v.sort_by(f64::total_cmp);
        axis_p10[k] = pct(&v, 0.10);
        axis_median[k] = pct(&v, 0.50);
        axis_p90[k] = pct(&v, 0.90);
        axis_zero_frac[k] = zero_frac;
        axis_over_teeming_frac[k] = over_teeming_frac;
        println!(
            "  {name:<18} p10={:.6} median={:.6} p90={:.6} zero_frac={:.4} \
             over_teeming_frac={:.4}",
            axis_p10[k], axis_median[k], axis_p90[k], zero_frac, over_teeming_frac
        );
    }

    ArmSummary {
        dominance,
        spread,
        sulphur_undercroft_wins,
        distinct,
        median_leader_margin_ratio,
        median_leader_margin_diff,
        axis_p10,
        axis_median,
        axis_p90,
        axis_zero_frac,
        axis_over_teeming_frac,
    }
}

/// THE TRENCHER, Task 15: does the geometric-mean yield form fix methane's
/// arity deficit without destroying discrimination? Ledger #39 froze five
/// predictions (H1-H5) BEFORE this code existed; this test measures CONTROL
/// (the shipped yield form) and GEO (every yield replaced by the k-th root of
/// its own product — see [`source_arity`]/[`geo_yield`]) over the same
/// twelve-seed sample [`report_the_metabolite_variety_measurement`] uses, and
/// prints the numbers that adjudicate each prediction.
///
/// **Re-measured on THIS tree, not cited from Task 12.** Task 12's dominance
/// shares (H .258 / Fe .339 / S .280 / CH4 .124, quoted in ledger #39 only as
/// the prediction's reference point) predate Task 13's widening of
/// `carbonate` (2 -> 19,681 distinct values) and `metamorphic_grade` (5 ->
/// 4,248) — both of which feed yields this ruling is about — so CONTROL below
/// is this probe's own live control, not a citation.
///
/// **This file ships no behaviour change.** `energy.rs` is untouched; GEO is
/// alternative arithmetic over [`Reading`]'s already-computed raw per-source
/// yields, the same posture section 6 above takes for the four methane arms.
///
/// claim: readout(off-gate, prints every number ledger #39's H1-H5
/// predictions need and states each as HELD/FALSIFIED with the deciding
/// figure; asserts only that the sample is non-vacuous and the geothermal
/// gain recovery is self-consistent — no H1-H5 verdict is itself an
/// assertion, because a falsified prediction is a legitimate finding here
/// (decision 0016) and encoding "H4 must hold" as a test failure would
/// forbid printing the result this probe exists to produce).
#[test]
#[ignore = "probe: Task 15's CONTROL-vs-GEO yield-form comparison over twelve seeds at \
            BuildDepth::Terrain (twelve world builds); run by hand (The Trencher, Task 15, \
            ledger #39)"]
fn report_task15_yield_form_measurement() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let per_rung = collect_readings(&wc);
    let pooled: Vec<Reading> = per_rung.iter().flatten().copied().collect();
    assert!(
        pooled.len() > 10_000,
        "only {} readings across {} seeds — vacuous",
        pooled.len(),
        Q6_SEEDS.len()
    );

    let gain = recovered_geothermal_gain(&pooled);
    println!("== Task 15: CONTROL vs GEO ==");
    println!(
        "readings pooled: {}   recovered geothermal modifier gain: {gain:.9}",
        pooled.len()
    );

    let control = measure_arm("CONTROL (shipped)", &pooled, &per_rung, Reading::post);
    let geo = measure_arm(
        "GEO (geometric mean of each source's own factors)",
        &pooled,
        &per_rung,
        |r| r.geo_axes(gain),
    );

    // Task 12's arm D (cbrt on methane alone, everything else held as
    // shipped) — the ledger's named fallback if GEO fails H4. Computed and
    // printed unconditionally, not only when H4 fails, so the report is
    // reproducible from its own printed numbers alone.
    println!("\n---- fallback comparison: Task 12's arm D (methane cbrt alone) ----");
    let arm_d_axes = |r: &Reading| -> [f64; 4] {
        let p = r.post();
        let methane_d = methane_arms(r.carbonate, r.porosity, r.moisture)[3] * r.modifier;
        [p[0], p[1], p[2], methane_d]
    };
    let arm_d = measure_arm(
        "Task 12 arm D (methane cbrt, H/Fe/S shipped)",
        &pooled,
        &per_rung,
        arm_d_axes,
    );

    // A THIRD arm, requested by the coordinator: geometric mean applied ONLY
    // to the two k=3 sources (SulphideOxidation, Methanogenesis), every k=2
    // source left exactly as shipped. Isolates whether GEO's benefit comes
    // from correcting excess arity specifically, or from the general upward
    // push a k-th root applies to every source it touches (including the
    // ones that were never over-multiplied) — see `selective_geo_axes`'s doc.
    println!("\n---- third arm: SELECTIVE (geo-mean only the two k=3 sources) ----");
    let selective = measure_arm(
        "SELECTIVE (SulphideOxidation + Methanogenesis geo-meaned; H/Fe/Geothermal shipped)",
        &pooled,
        &per_rung,
        Reading::selective_geo_axes,
    );

    // ---- H1-H5, adjudicated against GEO's own printed numbers ----------
    println!("\n== H1-H5 verdicts (ledger #39) ==");

    let h1 = geo.dominance[3] >= 0.18 && geo.spread < 0.215;
    println!(
        "H1 (arity explains the deficit): methane dominance={:.4} (need >=0.18), spread={:.4} \
         (need <0.215) -> {}",
        geo.dominance[3],
        geo.spread,
        if h1 { "HELD" } else { "FALSIFIED" }
    );

    let h2 = geo.sulphur_undercroft_wins == 0;
    println!(
        "H2 (depth structure survives): sulphur wins {} readings at Undercroft (need 0) -> {}",
        geo.sulphur_undercroft_wins,
        if h2 { "HELD" } else { "FALSIFIED" }
    );

    let geo_max_dominance = geo.dominance.iter().copied().fold(f64::MIN, f64::max);
    let geo_min_distinct = geo.distinct.iter().copied().min().unwrap_or(0);
    let h3 = geo_max_dominance <= 0.60 && geo_min_distinct >= 1_000;
    println!(
        "H3 (no degeneracy): max axis dominance={:.4} (need <=0.60), min distinct values={} \
         (need >=1000) -> {}",
        geo_max_dominance,
        geo_min_distinct,
        if h3 { "HELD" } else { "FALSIFIED" }
    );

    // H4 is adjudicated on the DIFFERENCE reading, not the ratio
    // (leader_margin_diff's doc explains why the ratio cannot ever fail a
    // >=0.02 bound). The ratio is printed above for reference and would have
    // reported HELD unconditionally, which is itself part of this finding.
    let h4 = geo.median_leader_margin_diff >= 0.02;
    println!(
        "H4 (DECISIVE — discrimination survives): median leader margin DIFFERENCE={:.6} (need \
         >=0.02) -> {}   [median RATIO={:.6}, printed for reference only — see \
         leader_margin_diff's doc for why the ratio cannot adjudicate this]",
        geo.median_leader_margin_diff,
        if h4 { "HELD" } else { "FALSIFIED" },
        geo.median_leader_margin_ratio,
    );
    if !h4 {
        println!(
            "  H4 FALSIFIED: the uniform geometric mean is REFUSED regardless of H1-H3. Arm D \
             (methane cbrt alone) median leader margin DIFFERENCE={:.6}, methane \
             dominance={:.4}, spread={:.4}.",
            arm_d.median_leader_margin_diff, arm_d.dominance[3], arm_d.spread
        );
    }

    let h5 = geo.dominance[3] <= 0.35;
    println!(
        "H5 (overshoot is also a finding): methane dominance={:.4} (overcorrection if >0.35) -> {}",
        geo.dominance[3],
        if h5 {
            "HELD"
        } else {
            "FALSIFIED (overcorrected)"
        }
    );

    println!(
        "\nfor reference, CONTROL's own re-measured dominance: {}",
        AXES.iter()
            .zip(control.dominance)
            .map(|(name, s)| format!("{name}={s:.4}"))
            .collect::<Vec<_>>()
            .join("  ")
    );

    // ---- coordinator follow-up: does GEO discriminate the LADDER, or just
    // relocate the compression from the bottom to the top? -----------------
    println!("\n== coordinator follow-up: per-axis value distributions, all arms ==");
    let arms: [(&str, &ArmSummary); 4] = [
        ("CONTROL", &control),
        ("GEO", &geo),
        ("SELECTIVE (k=3 sources only)", &selective),
        ("arm D (methane cbrt fallback)", &arm_d),
    ];
    for (label, s) in &arms {
        println!("  {label}:");
        for (k, name) in AXES.iter().enumerate() {
            println!(
                "    {name:<18} p10={:.6} median={:.6} p90={:.6} zero_frac={:.4} \
                 over_teeming_frac={:.4}",
                s.axis_p10[k],
                s.axis_median[k],
                s.axis_p90[k],
                s.axis_zero_frac[k],
                s.axis_over_teeming_frac[k]
            );
        }
    }

    println!(
        "\n== coordinator item 1: how many of the four axes' MEDIANS fall in each ladder band \
         ==\n(ladder: below LEAN <{E_LEAN} | LEAN-FED [{E_LEAN},{E_FED}) | FED-RICH \
         [{E_FED},{E_RICH}) | RICH-TEEMING [{E_RICH},{E_TEEMING}) | at/above TEEMING \
         >={E_TEEMING})"
    );
    for (label, s) in &arms {
        let mut band_counts = [0usize; 5];
        for &m in &s.axis_median {
            band_counts[ladder_band(m)] += 1;
        }
        let row: Vec<String> = LADDER_BAND_LABELS
            .iter()
            .zip(band_counts)
            .filter(|(_, c)| *c > 0)
            .map(|(label, c)| format!("{label}={c}"))
            .collect();
        println!("  {label:<32} {}", row.join("  "));
    }

    println!("\n== coordinator item 2: fraction of readings exceeding E_TEEMING (1.0) ==");
    for (label, s) in &arms {
        let row: Vec<String> = AXES
            .iter()
            .zip(s.axis_over_teeming_frac)
            .map(|(name, f)| format!("{name}={f:.4}"))
            .collect();
        println!("  {label:<32} {}", row.join("  "));
    }
}
