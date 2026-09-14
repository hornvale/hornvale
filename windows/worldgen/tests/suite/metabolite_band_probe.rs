//! The per-axis metabolite bands, and the guard that substitutes for the
//! consumer they do not yet have.
//!
//! # Why this file needs an unusual kind of guard
//!
//! No species niche weights `HYDROGEN`, `REDUCED_IRON`, `REDUCED_SULPHUR` or
//! `METHANE` — the count is zero on all four (ledger #45/#46). The bands these
//! tests cover therefore describe a chemistry nothing yet eats, which makes
//! them **unfalsifiable by the world**: no settlement moves, no creature is
//! mis-sited, no committed artifact drifts if a cut is wrong or if the
//! distribution slides out from under it. Every ordinary consequence that
//! would normally catch a bad calibration is absent.
//!
//! So the consumer is replaced by two instruments that do not need one:
//!
//! 1. **Occupancy** — every band an axis can reach must actually be reached.
//!    A band no reading can enter is not a band, and this campaign has already
//!    shipped that defect twice (a vacuous falsifier in ledger #40, and the
//!    asymptotically-unreachable `E_TEEMING` that #41 declined to create).
//! 2. **Drift** — each axis's median must still sit where the cuts were
//!    authored against. This is the deliberate stand-in for the missing
//!    consumer: it is the only thing in the tree that goes red if the supply
//!    distribution moves and the bands silently start meaning something else.
//!
//! Neither instrument becomes obsolete when biota arrive; both get stronger,
//! because a real consumer then adds consequences on top rather than replacing
//! the checks.
use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Band, Geosphere, Seed, VertexMap};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::energy::{
    ChemicalSupply, HYDROGEN_CUTS, METHANE_CUTS, MetaboliteBand, REDUCED_IRON_CUTS,
    REDUCED_SULPHUR_CUTS, chemical_supply_field_per_rung, metabolite_band,
};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, Substrate, WorldComponents, build_world_to_with_artifacts,
    climate_of, substrate_field, subterranean_substrate_field_per_rung,
};

/// Seeds sampled. Three, not one: a single world is an anecdote, which ledger
/// #45 established at the cost of a 44% figure that turned out to be a 3-4x
/// outlier against a ~10% effect.
const SEEDS: [u64; 3] = [0, 7, 42];

/// The five underground rungs, in depth order.
const RUNGS: [Band; 5] = [
    Band::Undercroft,
    Band::Shallows,
    Band::Deeps,
    Band::Underdeep,
    Band::Nadir,
];

/// Axis labels, in the order [`ChemicalSupply::metabolite_bands`] returns.
const AXES: [&str; 4] = ["hydrogen", "reduced_iron", "reduced_sulphur", "methane"];

/// The five bands, in ladder order.
const BANDS: [MetaboliteBand; 5] = [
    MetaboliteBand::Absent,
    MetaboliteBand::Trace,
    MetaboliteBand::Thin,
    MetaboliteBand::Ample,
    MetaboliteBand::Abundant,
];

/// Why a band can be empty over this probe's sample. The two are different
/// claims and only one of them is falsifiable by sampling harder, which is
/// exactly why they must not share a row type: an "it cannot happen" that is
/// really an "I did not look hard enough" is a false statement about the
/// world, and it would sit here looking identical to a true one.
#[derive(Clone, Copy, Debug, PartialEq)]
enum WhyEmpty {
    /// The world **cannot** produce this pair. Two-way guarded by
    /// [`no_pair_claimed_impossible_has_occurred`]: if one ever occurs, the
    /// claim was false and the row must be deleted.
    NeverOccurs,
    /// The world **does** produce this pair, but more rarely than this
    /// probe's sample can resolve.
    ///
    /// `per_100k` is **asserted, not decorative** — see
    /// [`a_rarity_claim_is_checked_against_what_actually_occurs`]. The first
    /// draft of this variant carried the number for a reader to check by hand
    /// and nothing read it, which is a seventh instance of
    /// `PROC-prose-claims-no-assertion-checks` authored by the very campaign
    /// that logged the first six. A frequency nothing compares against is a
    /// claim, not a measurement.
    ///
    /// It is not guarded in the *other* direction (observing zero confirms
    /// rarity rather than refuting it), so the assertion is one-sided by
    /// design: the falsifier for "rare" is "common".
    BelowSampleResolution {
        /// Occurrences per 100,000 readings, measured 2026-09-14 over the
        /// 12-seed, 104,845-reading run.
        per_100k: f64,
    },
}

/// `(axis index, band, why, evidence)` for every pair this probe expects to
/// find empty.
///
/// THIS LIST MAY ONLY SHRINK. It is the three-valued ratchet `tropes check`,
/// type-audit's `waiver(...)` and seam-guard's `expect(survives: ...)` all
/// use, and for the same reason: a guard that fails on the mere existence of a
/// gap goes red on day one and gets trained away, while one that never fails
/// is ignored just as fast.
const EXPECTED_EMPTY: [(usize, MetaboliteBand, WhyEmpty, &str); 2] = [
    (
        3,
        MetaboliteBand::Absent,
        WhyEmpty::NeverOccurs,
        "methanogenesis is the geometric mean of carbonate, porosity and moisture, and the three \
         are never simultaneously zero in a chamber -- measured zero_frac 0.0000 over 104,845 \
         readings",
    ),
    (
        1,
        MetaboliteBand::Absent,
        WhyEmpty::BelowSampleResolution { per_100k: 20.0 },
        "iron reduction is shut only where the silica bump and the water gate BOTH close, which \
         happens -- measured zero_frac 0.0002 over 104,845 readings, about 1 in 5,000. This \
         probe's 3 seeds carry ~29,300 readings, so it expects ~6 and observing none is ordinary. \
         The band is real; this sample cannot resolve it. Do NOT promote this to NeverOccurs.",
    ),
];

/// Build one seed to `BuildDepth::Terrain` and return its per-rung supply
/// field. Same construction the capacity loops use, so the bands are
/// classified over exactly the numbers production would classify.
fn supply_field(
    seed_value: u64,
    wc: &WorldComponents,
) -> (GeneratedTerrain, VertexMap<[Option<ChemicalSupply>; 6]>) {
    let artifacts = build_world_to_with_artifacts(
        Seed(seed_value),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        BuildDepth::Terrain,
    )
    .expect("band probe seed builds");
    let world = artifacts.world;
    let terrain = artifacts.terrain.expect("terrain at BuildDepth::Terrain");
    let climate = climate_of(&world).expect("climate reconstructs");
    let geo: &Geosphere = terrain.geosphere();
    let surface: VertexMap<Substrate> = substrate_field(
        geo,
        &terrain,
        &climate,
        climate.obliquity_deg(),
        climate.insolation(),
        &climate.regime(),
    );
    let per_rung = subterranean_substrate_field_per_rung(geo, &terrain, &surface);
    let supply = chemical_supply_field_per_rung(geo, &terrain, &per_rung);
    (terrain, supply)
}

/// `[axis][band]` occurrence counts.
type BandCounts = [[usize; 5]; 4];
/// Every raw reading, per axis.
type AxisValues = [Vec<f64>; 4];
/// `[rung][axis][band]` occurrence counts.
type BandCountsByRung = [[[usize; 5]; 4]; 5];

/// `[axis][band]` counts, the per-axis values, and the same counts split by
/// rung — pooled over every seed, vertex and rung that carries a reading.
fn tally(wc: &WorldComponents) -> (BandCounts, AxisValues, BandCountsByRung) {
    let mut counts = [[0usize; 5]; 4];
    let mut values: [Vec<f64>; 4] = Default::default();
    let mut by_rung = [[[0usize; 5]; 4]; 5];
    for seed in SEEDS {
        let (terrain, supply) = supply_field(seed, wc);
        let geo = terrain.geosphere();
        for vertex in geo.vertices() {
            let rungs = supply.get(vertex);
            for (slot, rung) in RUNGS.iter().enumerate() {
                let Some(chem) = rungs[*rung as usize].as_ref() else {
                    continue;
                };
                let raw = [
                    chem.hydrogen,
                    chem.reduced_iron,
                    chem.reduced_sulphur,
                    chem.methane,
                ];
                for (axis, band) in chem.metabolite_bands().iter().enumerate() {
                    let b = BANDS.iter().position(|x| x == band).expect("known band");
                    counts[axis][b] += 1;
                    by_rung[slot][axis][b] += 1;
                    values[axis].push(raw[axis]);
                }
            }
        }
    }
    (counts, values, by_rung)
}

fn is_expected_empty(axis: usize, band: MetaboliteBand) -> bool {
    EXPECTED_EMPTY
        .iter()
        .any(|(a, b, _, _)| *a == axis && *b == band)
}

/// claim: invariant(forall-axis-band, occupancy) — every band an axis can
/// reach is reached. A band nothing falls into is a category that cannot
/// describe a place, and with no ecological consumer nothing else in the tree
/// would ever notice.
#[test]
fn every_reachable_axis_band_pair_is_occupied() {
    let wc = WorldComponents::assemble().expect("registries");
    let (counts, _, _) = tally(&wc);
    let mut empty = Vec::new();
    for (axis, row) in counts.iter().enumerate() {
        for (b, n) in row.iter().enumerate() {
            if *n == 0 && !is_expected_empty(axis, BANDS[b]) {
                empty.push(format!("{}/{:?}", AXES[axis], BANDS[b]));
            }
        }
        eprintln!(
            "BAND {:<16} absent={} trace={} thin={} ample={} abundant={}",
            AXES[axis], row[0], row[1], row[2], row[3], row[4]
        );
    }
    assert!(
        empty.is_empty(),
        "these axis/band pairs are empty over {} seeds and are not on the EXPECTED_EMPTY roster: \
         {empty:?}. Either the cut is placed where the distribution never goes — in which case \
         move the cut, because a band no reading can enter is not a band — or the pair really is \
         unreachable, in which case add it to EXPECTED_EMPTY, choosing WhyEmpty::NeverOccurs only if the world truly \
         cannot produce it and WhyEmpty::BelowSampleResolution (with the measured frequency) \
         otherwise.",
        SEEDS.len()
    );
}

/// claim: invariant(forall-impossible-claim, no-stale-exemption) — the other
/// direction, and the one that keeps the roster honest. An exemption asserts
/// the world CANNOT produce a pair; if it can, the claim is false and the row
/// must go. Without this, `UNREACHABLE` could only ever be satisfied, and it
/// would rot exactly the way a one-directional acknowledgement does.
#[test]
fn no_pair_claimed_impossible_has_occurred() {
    let wc = WorldComponents::assemble().expect("registries");
    let (counts, _, _) = tally(&wc);
    for (axis, band, why, reason) in EXPECTED_EMPTY {
        if why != WhyEmpty::NeverOccurs {
            // A `BelowSampleResolution` row claims rarity, not impossibility.
            // Observing one CONFIRMS it, so it is deliberately not asserted
            // here -- asserting it would make the row unfalsifiable in the
            // wrong direction.
            continue;
        }
        let b = BANDS.iter().position(|x| *x == band).expect("known band");
        assert_eq!(
            counts[axis][b], 0,
            "{}/{band:?} is on EXPECTED_EMPTY as WhyEmpty::NeverOccurs — claiming \"{reason}\" \
             — but {} readings land in it. The claim is false; delete the row rather than \
             downgrading it to BelowSampleResolution to keep it green.",
            AXES[axis], counts[axis][b]
        );
    }
}

/// claim: invariant(band-membership-varies-with-depth) — K2 from ledger #42.
/// The bands must partition HABITAT, not merely numbers: if every axis read
/// the same way at every depth, they would describe the world no better than a
/// constant does, whatever their occupancy looked like.
///
/// # This test measures a SHIFT, not a mode, and the first draft measured the mode
///
/// The first version compared each axis's **modal** band at the shallowest
/// rung against the deepest, and reported only methane as moving. That was an
/// artifact of the statistic, not a fact about the world: hydrogen's mode is
/// `Absent` at every rung because 54% of its readings are absent, iron's is
/// `Abundant` everywhere, and sulphur's is `Thin` everywhere — while sulphur's
/// dominance over the same rungs swings from **0.000 at `Undercroft` to 0.287
/// at `Underdeep`**, which is the strongest depth signal any axis has. A mode
/// is a single order statistic and it moves only when the bulk crosses a cut;
/// it is blind to a distribution sliding underneath it.
///
/// So the measure is the **share of readings in the top two bands**
/// (`Ample` + `Abundant`) at the shallowest rung versus the deepest. That
/// tracks the population actually moving up or down the ladder, which is what
/// "does this axis distinguish depths" means for an author siting a species.
#[test]
fn the_band_distribution_shifts_with_depth() {
    let wc = WorldComponents::assemble().expect("registries");
    let (_, _, by_rung) = tally(&wc);
    let rich_share = |slot: usize, axis: usize| -> f64 {
        let total: usize = (0..5).map(|b| by_rung[slot][axis][b]).sum();
        if total == 0 {
            return f64::NAN;
        }
        (by_rung[slot][axis][3] + by_rung[slot][axis][4]) as f64 / total as f64
    };
    let mut biggest = 0.0_f64;
    for (axis, label) in AXES.iter().enumerate() {
        let (top, bottom) = (rich_share(0, axis), rich_share(RUNGS.len() - 1, axis));
        let shift = (bottom - top).abs();
        eprintln!(
            "BAND rich-share {label:<16} {:?}={:.4} -> {:?}={:.4}  shift={:+.4}",
            RUNGS[0],
            top,
            RUNGS[4],
            bottom,
            bottom - top
        );
        if shift.is_finite() && shift > biggest {
            biggest = shift;
        }
    }
    assert!(
        biggest >= 0.05,
        "no axis shifts its Ample+Abundant share by even 5 percentage points between {:?} and \
         {:?} (largest shift {biggest:.4}). The bands are not describing habitat — an author \
         could not use them to tell one depth from another, which is the question they exist to \
         answer.",
        RUNGS[0],
        RUNGS[4]
    );
}

/// claim: invariant(absent-names-absence) — K3 from ledger #42. Hydrogen's
/// large exactly-zero population must land in a band that NAMES absence rather
/// than being pooled with merely-poor readings: "no hydrogen here" and "a
/// little hydrogen here" are different affordances for a niche, and collapsing
/// them is the lossy abstraction the per-axis basis exists to avoid.
#[test]
fn a_zero_reading_is_named_absent_and_a_positive_one_never_is() {
    assert_eq!(metabolite_band(0.0, HYDROGEN_CUTS), MetaboliteBand::Absent);
    assert_eq!(
        metabolite_band(f64::MIN_POSITIVE, HYDROGEN_CUTS),
        MetaboliteBand::Trace,
        "the smallest positive supply is a TRACE, not an absence — `Absent` is exactly zero, \
         which the geometric-mean yield form preserves (0^(1/k) == 0)"
    );
    for cuts in [
        HYDROGEN_CUTS,
        REDUCED_IRON_CUTS,
        REDUCED_SULPHUR_CUTS,
        METHANE_CUTS,
    ] {
        assert!(
            cuts.trace_max < cuts.thin_max && cuts.thin_max < cuts.ample_max,
            "cuts must ascend: {cuts:?}"
        );
        assert_eq!(metabolite_band(cuts.trace_max, cuts), MetaboliteBand::Trace);
        assert_eq!(metabolite_band(cuts.thin_max, cuts), MetaboliteBand::Thin);
        assert_eq!(metabolite_band(cuts.ample_max, cuts), MetaboliteBand::Ample);
        assert_eq!(
            metabolite_band(cuts.ample_max * 2.0, cuts),
            MetaboliteBand::Abundant
        );
    }
}

/// Nearest-rank median of an unsorted sample.
fn median(v: &mut [f64]) -> f64 {
    v.sort_by(f64::total_cmp);
    v[v.len() / 2]
}

/// Each axis's **zero share** and its **median where present**, as measured
/// when the cuts were authored (2026-09-14, over `SEEDS` at
/// `BuildDepth::Terrain`), with the tolerance a re-measure may drift within.
///
/// # Why two numbers and not a plain median
///
/// The first draft tracked the median of all readings and it was degenerate on
/// the axis that needed it most: **hydrogen's median is exactly `0.0000`**,
/// because 54.6% of its readings are `Absent`. A statistic pinned to zero
/// cannot report drift — it stays at zero while the present-population moves
/// anywhere it likes, then jumps discontinuously the moment the absent share
/// crosses one half. The pair below is degenerate on no axis: the zero share
/// tracks how often the gates shut, and the median-where-present tracks how
/// much arrives when they do not, and a real distribution move must show in
/// one or the other.
///
/// # These are the substitute for the consumer the axes do not have
///
/// With no niche weighting any of them, a supply distribution can slide
/// arbitrarily far without a settlement moving, a creature being mis-sited, or
/// a committed artifact drifting — the bands would quietly start meaning
/// something else and every ordinary instrument would stay green. This is the
/// only thing in the tree that objects.
///
/// Tolerances are wide on purpose: this guard catches a distribution that has
/// MOVED, not a value that has wobbled. A tight window would redden on
/// ordinary terrain work and be trained away, which is the failure mode
/// `EXPECTED_EMPTY`'s own comment describes.
/// `(axis, authored zero share, its tolerance, authored median-where-present,
/// its tolerance)`.
///
/// # The tolerances are PER AXIS, and one shared number was the same mistake twice
///
/// The first draft used a single `ZERO_SHARE_TOLERANCE = 0.08` for all four.
/// Against zero shares of `0.546 / 0.0000 / 0.0304 / 0.0000` that is ~15%
/// relative on hydrogen and unbounded on the two axes that are never zero: iron
/// could acquire an 8% absent population — 8,000 per 100,000, against a
/// `BelowSampleResolution` row declaring 20 — without the guard moving. So the
/// drift check was four hundred times looser than the claim it was supposed to
/// protect, on exactly the axis that needed it.
///
/// That is **the same error this file's own band cuts exist to avoid**, made
/// one level up: four differently-shaped distributions do not share a
/// threshold. A tolerance is a threshold. The bands were given per-axis cuts
/// after measuring; their guard was not, until it was.
const AUTHORED_SHAPE: [(usize, f64, f64, f64, f64); 4] = [
    // Hydrogen: a large, real absent population, so an absolute band around it.
    (0, 0.5460, 0.0600, 0.7509, 0.1200),
    // Iron: essentially never zero; a tight cap, because any real absent
    // population here refutes the rarity row above rather than being drift.
    (1, 0.0000, 0.0050, 0.9955, 0.1200),
    // Sulphur: a small but genuine absent population, gated by the ΔT front.
    (2, 0.0304, 0.0200, 0.4700, 0.1200),
    // Methane: never zero anywhere, and `METHANE/Absent` is claimed
    // NeverOccurs — so the tolerance is the tightest of the four.
    (3, 0.0000, 0.0020, 0.5423, 0.1200),
];

/// claim: invariant(forall-axis, distribution-has-not-drifted) — the stand-in
/// for the missing ecological consumer. See [`AUTHORED_SHAPE`].
#[test]
fn each_axis_still_sits_where_its_cuts_were_authored() {
    let wc = WorldComponents::assemble().expect("registries");
    let (_, values, _) = tally(&wc);
    let mut drifted = Vec::new();
    for (axis, authored_zero, zero_tol, authored_median, median_tol) in AUTHORED_SHAPE {
        let all = &values[axis];
        let zeros = all.iter().filter(|v| **v <= 0.0).count();
        let zero_share = zeros as f64 / all.len() as f64;
        let mut present: Vec<f64> = all.iter().copied().filter(|v| *v > 0.0).collect();
        assert!(
            !present.is_empty(),
            "{}: every reading is zero — the axis has gone silent entirely, which no tolerance \
             should absorb",
            AXES[axis]
        );
        let m = median(&mut present);
        eprintln!(
            "BAND shape {:<16} zero={zero_share:.4} (authored {authored_zero:.4}, {:+.4})  \
             median-present={m:.4} (authored {authored_median:.4}, {:+.4})",
            AXES[axis],
            zero_share - authored_zero,
            m - authored_median
        );
        if (zero_share - authored_zero).abs() > zero_tol {
            drifted.push(format!(
                "{} zero share: authored {authored_zero:.4}, live {zero_share:.4}",
                AXES[axis]
            ));
        }
        if (m - authored_median).abs() > median_tol {
            drifted.push(format!(
                "{} median-where-present: authored {authored_median:.4}, live {m:.4}",
                AXES[axis]
            ));
        }
    }
    assert!(
        drifted.is_empty(),
        "these axes' supply distributions have moved from where their band cuts were authored: \
         {drifted:?}. NOTHING ELSE IN THE TREE WILL TELL YOU THIS — no species weights these \
         axes, so no settlement moves and no artifact drifts when they slide. Re-read the cuts \
         against the new distribution, re-check occupancy, and re-author both this shape and the \
         cuts in the same commit."
    );
}

/// How far above its declared rate an occurrence count may sit before the
/// rarity claim is refused. Generous because the counts are small and Poisson:
/// at 20 per 100,000 this probe's ~29,300 readings expect ~6, and a run that
/// happened to see 20 says nothing. A row wrong by the 400x that the zero-share
/// tolerance alone would have permitted is caught easily.
const RARITY_SLACK: f64 = 10.0;

/// claim: invariant(forall-rarity-claim, declared-rate-is-checked) — a
/// `BelowSampleResolution` row states a frequency, and this is what makes that
/// number mean something. Without it the field is prose: the zero-share drift
/// guard alone tolerates 0.08, i.e. 8,000 per 100,000, so a row declaring 20
/// could be wrong by four hundred times and nothing would object.
#[test]
fn a_rarity_claim_is_checked_against_what_actually_occurs() {
    let wc = WorldComponents::assemble().expect("registries");
    let (counts, values, _) = tally(&wc);
    for (axis, band, why, reason) in EXPECTED_EMPTY {
        let WhyEmpty::BelowSampleResolution { per_100k } = why else {
            continue;
        };
        let b = BANDS.iter().position(|x| *x == band).expect("known band");
        let readings = values[axis].len() as f64;
        let expected = per_100k / 100_000.0 * readings;
        let ceiling = expected * RARITY_SLACK + RARITY_SLACK;
        let observed = counts[axis][b] as f64;
        eprintln!(
            "BAND rarity {}/{band:?} declared={per_100k}/100k over {readings:.0} readings \
             expects {expected:.1}, observed {observed:.0}, ceiling {ceiling:.1}",
            AXES[axis]
        );
        assert!(
            observed <= ceiling,
            "{}/{band:?} declares {per_100k} per 100,000 — \"{reason}\" — but {observed:.0} of \
             {readings:.0} readings land in it, over the {ceiling:.1} this rate allows. The \
             declared rate is wrong, or the pair is no longer rare and the row should go.",
            AXES[axis]
        );
    }
}
