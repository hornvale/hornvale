//! The Underworld — the underworld corpus as points in The Axes' basis
//! (spec §4.4), and the two preregistered claims it is the instrument for
//! (spec §5, H4 and H5).
//!
//! **Every test here asserts on a non-empty corpus explicitly.** That is not
//! defensive padding: an empty `underworld_assignment()` satisfies "no two
//! names share a vector" and "light takes at most two distinct values"
//! *perfectly*, and this campaign has already found seven instruments that
//! looked like they were measuring and were not. The size guard
//! ([`the_underworld_corpus_is_the_size_these_tests_assume`]) plus the
//! per-test non-emptiness clauses are what make a red here mean something.

use hornvale_climate::axes::{AssignedName, assignment};
use hornvale_climate::underworld::{UnderworldName, underworld_assignment};
use hornvale_kernel::{
    AxisValence, Band, ENERGY, EnvironmentAxis, EnvironmentVector, LIGHT, PHYSIOGNOMY, SUBSTRATE,
    WATER, environment_v1_basis,
};
use std::collections::{BTreeMap, BTreeSet};

/// The five axes spec §4.4 says the underworld occupies. `DISTURBANCE` is
/// deliberately absent: it is the basis's only `Rate` axis and The Axes' A-4
/// records it as a declared axis nothing can occupy yet.
const OCCUPIED: [EnvironmentAxis; 5] = [PHYSIOGNOMY, ENERGY, WATER, SUBSTRATE, LIGHT];

/// The three cave formations The Axes already placed. This slice **is** "the
/// cave region of the space" that A-3 predicted the underworld would land in;
/// naming it here rather than describing it is what makes A-3 evaluable at
/// all.
const CAVE_REGION: [&str; 3] = ["karst-cave", "lava-tube", "fracture-cave"];

fn assigned() -> Vec<&'static UnderworldName> {
    underworld_assignment()
        .iter()
        .filter(|n| !n.vector.is_unassigned())
        .collect()
}

fn resisters() -> Vec<&'static UnderworldName> {
    underworld_assignment()
        .iter()
        .filter(|n| n.vector.is_unassigned())
        .collect()
}

/// Guards the denominator every other test in this file divides by. If the
/// corpus grows or shrinks, the numbers reported in the campaign's chronicle
/// were taken against a different corpus and must be re-taken deliberately —
/// not silently inherited. Modelled on The Axes'
/// `the_corpus_is_the_74_names_the_bounds_assume`.
#[test]
fn the_underworld_corpus_is_the_size_these_tests_assume() {
    assert_eq!(
        underworld_assignment().len(),
        24,
        "the underworld corpus is 22 assigned communities plus 2 resisters"
    );
    assert_eq!(assigned().len(), 22, "assigned communities");
    assert_eq!(resisters().len(), 2, "names the axes could not place");
}

/// **The property `DelveZone` used to carry structurally, now carried by
/// hand.** Before The Drift, `UnderworldName::zone` was `DelveZone`, an enum
/// with no `Surface` variant — the type system itself forbade authoring a row
/// there, and `cli/tests/suite/delve_roster_mirror.rs` asserted the omission
/// was deliberate. `zone` is `hornvale_kernel::Band` now, which *does* carry
/// `Surface` (the overworld is a rung of the same ladder, spec §4.6), so
/// nothing stops a row from being authored against it by mistake. This test
/// is what replaces the old type-level guarantee: no underworld community is
/// at the surface, checked as a corpus invariant rather than assumed from the
/// type.
#[test]
fn no_underworld_community_occupies_the_surface() {
    let names = underworld_assignment();
    assert!(
        !names.is_empty(),
        "an empty corpus vacuously satisfies this"
    );
    for name in names {
        assert_ne!(
            name.zone,
            Band::Surface,
            "{} is authored at Band::Surface, but no underworld community \
             occupies the overworld",
            name.name
        );
    }
}

/// The underworld module's own doc makes a numeric claim — realised
/// cardinalities 5/5/6/6/2, summing to 24 over 22 assigned names, which is why
/// it declines to call the decomposition *compressive* in The Axes' sense.
/// This measures that claim so it cannot rot into a sentence nobody rechecks.
///
/// **It asserts the claim, and deliberately does not assert a compression
/// bound.** The Axes' `p1_the_decomposition_is_compressive` derives its `<= 30`
/// from a 74-name corpus; importing that number here would be inheriting a
/// bound computed against a different denominator, which is precisely the
/// mistake that guard exists to prevent.
#[test]
fn the_realised_cardinalities_are_what_the_module_doc_says() {
    let mut seen: BTreeMap<u16, BTreeSet<u64>> = BTreeMap::new();
    for name in assigned() {
        for axis in OCCUPIED {
            let v = name.vector.get(axis).expect("assigned names carry it");
            seen.entry(axis.id).or_default().insert(v.to_bits());
        }
    }
    let card: Vec<usize> = OCCUPIED
        .iter()
        .map(|a| seen.get(&a.id).map_or(0, BTreeSet::len))
        .collect();
    let sum: usize = card.iter().sum();
    let product: u128 = card.iter().map(|c| *c as u128).product();
    println!(
        "underworld cardinalities (physiognomy/energy/water/substrate/light) \
         {card:?} -> sum {sum}, product {product}, over {} assigned names",
        assigned().len()
    );

    assert_eq!(card, vec![5, 5, 6, 6, 2], "the module doc names these");
    assert_eq!(sum, 24, "the module doc names this sum");
    assert!(
        product >= 22,
        "coverage: the grid must at least be able to hold the corpus"
    );
    assert!(
        sum > assigned().len(),
        "the module doc says this decomposition costs MORE symbols than the \
         corpus it describes, and declines to claim compression on that basis. \
         If that has stopped being true, the doc is now wrong."
    );
}

#[test]
fn every_underworld_name_carries_the_axes_it_can() {
    let names = assigned();
    assert!(
        names.len() >= 20,
        "an empty or near-empty corpus satisfies every other test in this \
         file vacuously; got {} assigned names",
        names.len()
    );

    for name in &names {
        for axis in OCCUPIED {
            assert!(
                name.vector.get(axis).is_some(),
                "{} declines {} — spec §4.4 says all five occupied axes are \
                 readable underground, LIGHT included (it collapses to few \
                 values, it does not go absent)",
                name.name,
                axis.label
            );
        }
        // The sixth axis stays empty here for the same reason it is empty in
        // the surface corpus: a rate is not a state.
        assert_eq!(
            name.vector.axis_ids().len(),
            5,
            "{} carries an axis outside the five §4.4 occupies",
            name.name
        );
    }

    // A resister carries NOTHING — the zero vector is how an unplaceable name
    // is represented, and a half-assigned one would be neither.
    for name in resisters() {
        assert!(
            name.vector.is_unassigned(),
            "{} was counted as a resister but carries axes",
            name.name
        );
    }

    // Every name points at a cave formation it is a community of. An empty
    // `genera` would silently drop the name out of the A-3 evaluation below.
    for name in underworld_assignment() {
        assert!(
            !name.genera.is_empty(),
            "{} names no genus, so A-3 cannot be evaluated for it",
            name.name
        );
        for genus in name.genera {
            assert!(
                CAVE_REGION.contains(genus),
                "{}'s genus {genus:?} is not one of the three cave formations",
                name.name
            );
        }
    }
}

/// The Axes' collision clause, applied to this corpus.
///
/// **What this would be satisfied by, stated because a criterion nobody has
/// tried to break is not a criterion.** It is satisfied trivially by any
/// corpus generated from distinct inputs through an injective function — that
/// is the failure mode this campaign found elsewhere, where a criterion
/// counted a function's outputs and was satisfied by its own `match`. It is
/// **not** trivial here: `underworld_assignment()` is a hand-authored table of
/// 22 five-tuples chosen from a 5×5×6×6×2 grid, and nothing in its
/// construction prevents two authored rows from being identical. The Axes' own
/// corpus collided twice on the first fit (`ice`/`snowfield`,
/// `desert`/`erg`). Proven able to fire by mutation — see the task report.
#[test]
fn no_two_underworld_names_share_a_vector() {
    let names = assigned();
    assert!(!names.is_empty(), "an empty corpus cannot collide");

    let mut by_vector: BTreeMap<Vec<(u16, u64)>, Vec<&'static str>> = BTreeMap::new();
    for name in &names {
        let key: Vec<(u16, u64)> = environment_v1_basis()
            .iter()
            .filter_map(|a| name.vector.get(*a).map(|v| (a.id, v.to_bits())))
            .collect();
        by_vector.entry(key).or_default().push(name.name);
    }
    let collisions: Vec<&Vec<&'static str>> = by_vector.values().filter(|n| n.len() > 1).collect();
    assert!(
        collisions.is_empty(),
        "distinct underworld names collided on one vector, so the axes are \
         too coarse for the underworld: {collisions:?}"
    );
    assert_eq!(
        by_vector.len(),
        names.len(),
        "the distinct-vector count must equal the name count"
    );
}

/// **Spec §5, H5 — PREREGISTERED before any of this corpus was authored.**
/// The `LIGHT` axis takes at most 2 distinct values across the underworld
/// corpus.
///
/// Result, measured 2026-08-17: **2 distinct values — `{0.0, 0.2}`. H5 HOLDS.**
/// Every community below the entrance rung reads `0.0`; the four that break the
/// surface (a cave mouth, a sinkhole shaft, a lava-tube skylight, a fault slot
/// reaching daylight) read `0.2`. Nothing in §3.8's inputs produces a third
/// value: there is no bioluminescence term and `MaterialBuffer::thaumic` is
/// identically zero in this tier.
///
/// If a later campaign moves this, the rule from the plan applies: change the
/// expected count to the measured one, mark it here as a falsified
/// preregistration with the date, and do **not** retune the assignment.
#[test]
fn light_takes_at_most_two_distinct_values() {
    let names = assigned();
    assert!(
        names.len() >= 20,
        "H5 is a claim about a corpus; {} names is not one",
        names.len()
    );

    let distinct: BTreeSet<u64> = names
        .iter()
        .map(|n| {
            n.vector
                .get(LIGHT)
                .expect("every assigned name carries LIGHT")
                .to_bits()
        })
        .collect();
    let readable: Vec<f64> = distinct.iter().map(|b| f64::from_bits(*b)).collect();
    println!(
        "H5: LIGHT takes {} distinct values: {readable:?}",
        distinct.len()
    );

    assert!(
        distinct.len() <= 2,
        "PREREGISTERED PREDICTION FALSIFIED (spec §5 H5): LIGHT takes {} \
         distinct values {readable:?}, not at most 2. This is a FINDING: \
         record the measured count in this doc comment with today's date and \
         change the bound — never adjust the assignment to rescue it.",
        distinct.len()
    );
    // The floor the ceiling above needs: a corpus where LIGHT took ONE value
    // would also pass `<= 2`, and would mean the entrance zone was never
    // modelled rather than that light collapsed to a floor and a threshold.
    assert_eq!(
        distinct.len(),
        2,
        "LIGHT collapsed to a single value; the surface-breaching rung's \
         twilight is missing from the corpus"
    );
}

/// **Spec §4.4 — energy INVERTS with depth**, and the campaign's best find.
/// Shallow chambers are powered by detrital import; deep ones by
/// chemolithotrophy off the geothermal gradient. A corpus where energy fell
/// monotonically with depth would mean the inversion was not modelled at all.
///
/// This asserts the **shape**, not merely the absence of monotonicity: the
/// mid-ladder `Deeps` trough must sit strictly below BOTH the shallow half and
/// the deep half. Non-monotonicity alone is a floor with no ceiling — a corpus
/// with one arbitrary bump would clear it.
#[test]
fn energy_is_not_monotone_in_depth() {
    let names = assigned();
    assert!(
        !names.is_empty(),
        "an empty corpus is monotone in everything"
    );

    let ladder = [
        Band::Undercroft,
        Band::Shallows,
        Band::Deeps,
        Band::Underdeep,
        Band::Nadir,
    ];
    let mut means: Vec<(Band, f64, usize)> = Vec::new();
    for zone in ladder {
        let e: Vec<f64> = names
            .iter()
            .filter(|n| n.zone == zone)
            .map(|n| n.vector.get(ENERGY).expect("assigned names carry ENERGY"))
            .collect();
        assert!(
            !e.is_empty(),
            "{zone:?} holds no community, so the depth axis is not spanned \
             and a monotonicity claim over it is vacuous"
        );
        let mean = e.iter().sum::<f64>() / e.len() as f64;
        means.push((zone, mean, e.len()));
    }
    println!("ENERGY by delve zone (shallow -> deep):");
    for (zone, mean, n) in &means {
        println!(
            "  {zone:<12}  n={n:2}  mean ENERGY {mean:.3}",
            zone = format!("{zone:?}")
        );
    }

    // 1. Not monotone non-increasing (energy does not simply fall with depth).
    let non_increasing = means.windows(2).all(|w| w[0].1 >= w[1].1);
    assert!(
        !non_increasing,
        "ENERGY falls monotonically with depth — the inversion was not \
         modelled: {means:?}"
    );
    // 2. Not monotone non-decreasing either. Without this clause a corpus that
    //    simply made the deep richest would pass, and detrital import — the
    //    shallow half of the mechanism — would be absent.
    let non_decreasing = means.windows(2).all(|w| w[0].1 <= w[1].1);
    assert!(
        !non_decreasing,
        "ENERGY rises monotonically with depth — detrital import is missing \
         from the shallow half: {means:?}"
    );

    // 3. The shape: a trough at the ladder's middle, fed from above and from
    //    below. `Deeps` is where detrital import has run out and the
    //    geothermal gradient has not yet paid.
    let shallow: f64 = zone_mean(&names, &[Band::Undercroft, Band::Shallows]);
    let trough: f64 = zone_mean(&names, &[Band::Deeps]);
    let deep: f64 = zone_mean(&names, &[Band::Underdeep, Band::Nadir]);
    println!(
        "the inversion: shallow(Undercroft+Shallows) {shallow:.3} > trough(Deeps) \
         {trough:.3} < deep(Underdeep+Nadir) {deep:.3}"
    );
    assert!(
        shallow > trough,
        "detrital import must make the shallow half richer than the trough: \
         {shallow:.3} vs {trough:.3}"
    );
    assert!(
        deep > trough,
        "chemolithotrophy must make the deep half richer than the trough: \
         {deep:.3} vs {trough:.3} — THIS is the inversion, and without it the \
         deep is merely poorer"
    );

    // 4. And at the level of individual names, not just means: some deeper
    //    name out-energises some shallower one, and vice versa.
    let rises = names.iter().any(|a| {
        names
            .iter()
            .any(|b| a.zone < b.zone && energy(a) < energy(b))
    });
    let falls = names.iter().any(|a| {
        names
            .iter()
            .any(|b| a.zone < b.zone && energy(a) > energy(b))
    });
    assert!(rises && falls, "the inversion must hold name-wise too");
}

/// The inversion's sharpest single statement, asserted rather than narrated.
///
/// `sump-gallery` (Deeps) and `deep-karst-void` (Nadir) are the same void in
/// the same rock at two depths: identical on PHYSIOGNOMY, WATER, SUBSTRATE and
/// LIGHT, and **differing only on ENERGY**, with the deeper one strictly
/// richer. That is spec §4.4's sentence — *the deep is not poorer, it is
/// differently powered* — reduced to a single controlled comparison, with
/// four of the five axes held fixed as the control.
///
/// It lived only in prose until a reviewer verified it by hand. Prose a
/// reviewer has to check is a missing assertion.
#[test]
fn the_same_void_at_two_depths_differs_on_energy_alone() {
    let find = |n: &str| -> &'static UnderworldName {
        underworld_assignment()
            .iter()
            .find(|u| u.name == n)
            .unwrap_or_else(|| panic!("{n} left the corpus"))
    };
    let shallower = find("sump-gallery");
    let deeper = find("deep-karst-void");

    assert!(
        shallower.zone < deeper.zone,
        "the pair must straddle the ladder: {:?} vs {:?}",
        shallower.zone,
        deeper.zone
    );
    for axis in [PHYSIOGNOMY, WATER, SUBSTRATE, LIGHT] {
        assert_eq!(
            shallower.vector.get(axis).map(f64::to_bits),
            deeper.vector.get(axis).map(f64::to_bits),
            "the control fails: the pair differs on {}, so ENERGY is not the \
             only thing separating them and this is no longer a controlled \
             comparison",
            axis.label
        );
    }
    assert!(
        energy(deeper) > energy(shallower),
        "the deeper void must be RICHER: {} at {} vs {} at {}",
        deeper.name,
        energy(deeper),
        shallower.name,
        energy(shallower)
    );
    println!(
        "the controlled pair: {} ({:?}) ENERGY {:.2} -> {} ({:?}) ENERGY {:.2}, \
         four axes held fixed",
        shallower.name,
        shallower.zone,
        energy(shallower),
        deeper.name,
        deeper.zone,
        energy(deeper)
    );
}

fn energy(n: &UnderworldName) -> f64 {
    n.vector.get(ENERGY).expect("assigned names carry ENERGY")
}

fn zone_mean(names: &[&'static UnderworldName], zones: &[Band]) -> f64 {
    let e: Vec<f64> = names
        .iter()
        .filter(|n| zones.contains(&n.zone))
        .map(|n| energy(n))
        .collect();
    assert!(!e.is_empty(), "no community in {zones:?}");
    e.iter().sum::<f64>() / e.len() as f64
}

/// Valence-aware distance between two vectors, over the axes **both** carry:
/// mean per-axis distance, where an `Ordinal`/`Scalar` axis contributes
/// `|a - b|` and a `Nominal` axis contributes 0 or 1.
///
/// The nominal treatment is not a nicety. `SUBSTRATE` is declared
/// `AxisValence::Nominal` in the kernel — "the index is a class, never a
/// magnitude" — so `|0.0 - 0.2|` between soil and sand is not a smaller
/// difference than `|0.0 - 1.0|` between soil and organic. Treating it as a
/// magnitude would make every distance below quietly wrong.
fn distance(a: &EnvironmentVector, b: &EnvironmentVector) -> f64 {
    let mut total = 0.0;
    let mut shared = 0usize;
    for axis in environment_v1_basis() {
        let (Some(x), Some(y)) = (a.get(*axis), b.get(*axis)) else {
            continue;
        };
        shared += 1;
        total += match axis.valence {
            AxisValence::Nominal => {
                if x.to_bits() == y.to_bits() {
                    0.0
                } else {
                    1.0
                }
            }
            _ => (x - y).abs(),
        };
    }
    assert!(shared > 0, "two vectors sharing no axis cannot be compared");
    total / shared as f64
}

/// **A GRID control, and only a grid control.** The underworld corpus must
/// quantise onto the same numeric **levels** the surface corpus uses: if it had
/// invented its own — 0.35 where the surface uses 0.4 — every A-3 number would
/// be an artifact of the regrading and would look like a real displacement.
/// Proven able to fire (an off-grid 0.35 reddens it; see the task report).
///
/// **What it does NOT control, stated because an earlier version of this doc
/// claimed it did.** It called itself "the control that makes A-3 a
/// measurement rather than an artifact of the regrading", which is false in
/// the way this campaign keeps finding: it checks that the *levels* coincide —
/// which was never in doubt, both corpora were authored from the same
/// constants — while `PHYSIOGNOMY`'s **semantics** demonstrably differ between
/// them. Sharing a ruler is not sharing a meaning. That made it a grid control
/// wearing a semantic control's label, and this campaign's ninth instrument
/// that read as evidence for something it could not see.
///
/// **A-3 is therefore computed across a semantic regrounding on one axis, and
/// across a grain change on the same one.** `axes.rs` reads `PHYSIOGNOMY` off
/// vegetation structure on land and animal structure in the sea (`coral-head`
/// = closed, `vent`/`tubeworm-thicket` = shrub-equivalent); this corpus reads
/// it off *void morphology*, which is abiotic. And `karst-cave` is a
/// **formation** read from the surface classifier while `flowstone-hall` is a
/// **community inside one**, so every A-3 distance's `PHYSIOGNOMY` component
/// compares a morphology reading against a vegetation reading **at a different
/// grain**. Nothing here or anywhere else controls for that; it is a stated
/// limitation of the A-3 verdict, not a defect in it.
#[test]
fn the_underworld_uses_the_surface_corpus_value_grid() {
    let mut grid: BTreeMap<u16, BTreeSet<u64>> = BTreeMap::new();
    for AssignedName { vector, .. } in assignment() {
        for axis in environment_v1_basis() {
            if let Some(v) = vector.get(*axis) {
                grid.entry(axis.id).or_default().insert(v.to_bits());
            }
        }
    }
    assert!(!grid.is_empty(), "the surface corpus is empty");

    let mut checked = 0usize;
    for name in assigned() {
        for axis in OCCUPIED {
            let v = name.vector.get(axis).expect("assigned").to_bits();
            let levels = grid.get(&axis.id).expect("surface corpus occupies it");
            assert!(
                levels.contains(&v),
                "{} places {} at {} — a level the surface corpus never uses, \
                 so the two corpora are not on one grid and no distance \
                 between them is meaningful",
                name.name,
                axis.label,
                f64::from_bits(v)
            );
            checked += 1;
        }
    }
    assert_eq!(checked, 22 * 5, "every occupied axis of every name checked");
}

/// **A finding the A-3 measurement surfaced, committed so it cannot be lost:
/// three underworld communities are INDISTINGUISHABLE from a surface or marine
/// name on the axes the two carry.**
///
/// Within each corpus no two names collide (that is
/// [`no_two_underworld_names_share_a_vector`] and The Axes' own P-1). Across
/// the two corpora they do, and the pairs are physically apt rather than
/// sloppy — which is the interesting part. A flooded mud-filled sump and
/// lightless open ocean water genuinely are both dark, cold, saturated and
/// soft-floored; a hot deep karst void and a black smoker field genuinely are
/// both dark, hot, saturated, rock-floored chemolithotroph communities. The
/// basis is resolving what a place *is*, and by that measure it is right that
/// they coincide.
///
/// **What it costs, stated plainly:** a consumer that scores a niche against a
/// place vector — Task 7's `environment_fit` — cannot tell these pairs apart
/// on this basis, and no amount of tuning the underworld corpus would fix it,
/// because the collision is with the surface corpus rather than inside either.
/// Separating them needs a REALM discriminator the six-axis basis does not
/// carry.
///
/// **Nothing shipped is broken by it, and that is checkable rather than
/// hopeful:** `axes::assignment()` has **no production consumer**. Grep over
/// the tree (`axes::assignment|hornvale_climate::axes`, excluding `target/`)
/// finds it only in this crate's four test files —
/// `coarse_constrains_fine.rs`, `held_out_marine.rs`, `preregistration.rs`,
/// this one — and `windows/worldgen/tests/axis_geometry.rs`. Every hit is a
/// test. Vector distinctness across the two corpora is a
/// property of a measurement instrument, not of any world.
///
/// Recorded as an exact set, ratchet-style: a change here is a change to what
/// the basis can resolve, and must be a deliberate one.
#[test]
fn three_underworld_names_are_indistinguishable_from_a_surface_name() {
    let mut coincident: Vec<(&'static str, &'static str)> = Vec::new();
    for name in assigned() {
        for other in assignment().iter().filter(|a| !a.vector.is_unassigned()) {
            if distance(&name.vector, &other.vector) == 0.0 {
                coincident.push((name.name, other.name));
            }
        }
    }
    coincident.sort_unstable();
    println!("cross-corpus coincidences: {coincident:?}");
    assert_eq!(
        coincident,
        vec![
            // Exact on all five axes.
            ("deep-karst-void", "smoker-field"),
            // Exact on all five axes.
            ("mud-sump", "lightless-water"),
            // Exact on the four axes `ice` carries; `ice` declines LIGHT
            // because its own variants scatter light differently, so this is a
            // coincidence on the shared axes rather than a full collision.
            ("tube-ice-trap", "ice"),
        ],
        "the set of cross-corpus coincidences moved. That is a change in what \
         the six-axis basis can resolve, not a test to be updated silently: \
         record the new set and say what moved it."
    );
}

/// **Spec §5, H4 — The Axes' A-3, evaluated.** That retrospective recorded a
/// forward prediction: *"campaign 2's underworld communities should land in
/// the cave region of the space"*, explicitly unevaluable at the time and
/// ungated. This campaign evaluates it and states the result either way.
///
/// Operationalised as: for each underworld community, its nearest neighbour
/// among the surface corpus's **64 assigned** names under [`distance`] (74
/// less The Axes' 10 resisters, which carry the zero vector and are not
/// candidates). A-3 holds for a name iff that nearest neighbour is one of the
/// three cave formations.
///
/// [`distance`] is Gower's distance over the shared axes, `Nominal` handled as
/// 0-or-1. Read its doc before reading a number off this test.
///
/// **THE WEAK FORM RESTS ON THE NEAREST-NEIGHBOUR COUNT ALONE.** The chance
/// rate is its denominator: **3 of the 64 candidate names are cave
/// formations, so a corpus scattered at random scores 4.7%.** (An earlier
/// version of this doc divided by 74 and reported 4.1%; the candidate set was
/// already filtered to assigned names, so the denominator did not match the
/// thing being measured. Corrected 2026-08-17. The verdict does not move.)
///
/// **The mean-distance comparison below is NOT a second, independent
/// control, and is no longer presented as one.** The three caves are a tight
/// cluster in the dark/rocky/low-physiognomy corner while the other 61 names
/// span the lit, warm, vegetated half of the space, so *any* dark rocky corpus
/// beats it — it cannot distinguish "landed in the cave region" from "landed
/// anywhere dark". It is kept as a sanity floor, because a corpus that
/// **failed** it would falsify A-3 outright, and it is asserted first for that
/// reason. It corroborates nothing.
///
/// **One real bias in [`distance`], measured rather than hypothesised.** The
/// mean is taken over *shared* axes, so a surface name that DECLINES an axis
/// is scored on fewer terms and is systematically advantaged. Four of the 13
/// misses are exactly such names — `temperate-forest` and `open-water` decline
/// `SUBSTRATE`, `ice` declines `LIGHT` — so this is a live effect on the
/// result, not a theoretical caveat. It cuts against A-3 rather than for it.
///
/// **Result, measured 2026-08-17: A-3 holds in the weak form and FAILS in the
/// strong one, systematically.** The assertions below hold the weak form. The
/// per-name print is the finding: the communities that miss are the
/// chemolithotroph and phreatic halves, and they miss toward `vent` and its
/// variants and the deep pelagic ladder — the sea's own dark, hot,
/// rock-floored communities. That is the right answer for the wrong region,
/// and it is reported rather than papered.
#[test]
fn a3_the_underworld_lands_in_the_cave_region() {
    let surface = assignment();
    let caves: Vec<&AssignedName> = surface
        .iter()
        .filter(|a| CAVE_REGION.contains(&a.name))
        .collect();
    assert_eq!(caves.len(), 3, "the cave region is three formations");
    let others: Vec<&AssignedName> = surface
        .iter()
        .filter(|a| !a.vector.is_unassigned() && !CAVE_REGION.contains(&a.name))
        .collect();
    assert_eq!(
        others.len(),
        61,
        "the non-cave comparison set: 74 names less 3 caves less 10 resisters"
    );

    // THE CANDIDATE SET, named once and used for both the loop below and the
    // chance denominator. Keeping these two the same object is the whole fix
    // for the 4.1%-vs-4.7% defect: the denominator must be the set the
    // nearest-neighbour search actually draws from, and when they were two
    // expressions they drifted apart silently.
    let candidates: Vec<&AssignedName> = surface
        .iter()
        .filter(|a| !a.vector.is_unassigned())
        .collect();
    assert_eq!(
        candidates.len(),
        caves.len() + others.len(),
        "the candidate set must be exactly the caves plus the comparison set, \
         or the chance rate is computed against a different population than \
         the search ran over"
    );
    assert_eq!(candidates.len(), 64, "74 names less The Axes' 10 resisters");

    let names = assigned();
    assert!(!names.is_empty(), "A-3 over an empty corpus is vacuous");

    let mut in_region = 0usize;
    let mut mean_cave_total = 0.0;
    let mut mean_other_total = 0.0;
    println!("A-3: nearest surface neighbour of each underworld community");
    for name in &names {
        // Deterministic tie-break by name, so a tie never depends on corpus
        // order — the workspace's float-sort rule applied to an argmin.
        let mut best: Option<(&'static str, f64)> = None;
        for other in &candidates {
            let d = distance(&name.vector, &other.vector);
            let better = match best {
                None => true,
                Some((bn, bd)) => match d.total_cmp(&bd) {
                    std::cmp::Ordering::Less => true,
                    std::cmp::Ordering::Equal => other.name < bn,
                    std::cmp::Ordering::Greater => false,
                },
            };
            if better {
                best = Some((other.name, d));
            }
        }
        let (nearest, d) = best.expect("the surface corpus is non-empty");
        let hit = CAVE_REGION.contains(&nearest);
        if hit {
            in_region += 1;
        }
        let cave_mean: f64 = caves
            .iter()
            .map(|c| distance(&name.vector, &c.vector))
            .sum::<f64>()
            / 3.0;
        let other_mean: f64 = others
            .iter()
            .map(|c| distance(&name.vector, &c.vector))
            .sum::<f64>()
            / others.len() as f64;
        mean_cave_total += cave_mean;
        mean_other_total += other_mean;
        println!(
            "  {:<20} zone={:?} nearest={:<22} d={:.3}  cave-mean={:.3} \
             other-mean={:.3}  {}",
            name.name,
            name.zone,
            nearest,
            d,
            cave_mean,
            other_mean,
            if hit { "IN-REGION" } else { "outside" }
        );
    }
    let rate = in_region as f64 / names.len() as f64;
    let chance = caves.len() as f64 / candidates.len() as f64;
    mean_cave_total /= names.len() as f64;
    mean_other_total /= names.len() as f64;
    println!(
        "A-3: {in_region}/{} nearest-neighbour hits = {:.1}% against a {:.1}% \
         chance rate (3 caves in {} candidates), a lift of {:.1}x. \
         SANITY FLOOR ONLY, not corroboration: mean distance to the cave \
         region {mean_cave_total:.3} vs {mean_other_total:.3} to the other {} \
         names.",
        names.len(),
        rate * 100.0,
        chance * 100.0,
        candidates.len(),
        rate / chance,
        others.len()
    );

    // THE SANITY FLOOR, asserted first because a corpus failing it would
    // falsify A-3 outright. It is NOT independent evidence for the weak form:
    // the 3 caves cluster in the dark/rocky/low-physiognomy corner and the 61
    // others span the lit/warm/vegetated half, so any dark rocky corpus clears
    // it. See this test's doc.
    assert!(
        mean_cave_total < mean_other_total,
        "A-3 FALSIFIED outright: the corpus is no closer to the cave region \
         ({mean_cave_total:.3}) than to the rest of the space \
         ({mean_other_total:.3})"
    );
    // THE WEAK FORM, and it rests on this clause alone. The 4x bound was
    // written before the corpus was authored; the measured lift is ~8.7x.
    assert!(
        rate > chance * 4.0,
        "A-3 FALSIFIED: {in_region}/{} = {:.1}% of the corpus has a cave \
         formation as its nearest neighbour, against a {:.1}% chance rate. \
         Record the measured rate here with today's date rather than moving \
         the bound.",
        names.len(),
        rate * 100.0,
        chance * 100.0
    );
}

/// **The arity ratchet — the denominator `environment_fit`'s mean divides by.**
///
/// `hornvale_species::environment_fit` (Task 7) averages per-axis
/// dissimilarity over the axes a niche and a place **share**. A mean over an
/// intersection is not comparable across places of differing arity: a chamber
/// stating two axes can score `1.0` on both while a chamber stating five is
/// scored on all five, so the sparser vector is systematically advantaged. The
/// same bias is already recorded on the A-3 test above, where it is a live
/// effect on the result rather than a caveat.
///
/// **Measured 2026-08-17, and the answer is what makes the bias inert for a
/// single-realm ranking:** the arity of the underworld corpus is **constant**.
/// All 22 assigned communities state exactly the five axes of [`OCCUPIED`];
/// the 2 resisters state none, which is the zero vector meaning *unassigned*
/// rather than a sparse assignment. So ranking chambers for one kind against
/// this corpus compares equal-arity vectors throughout and the mean's
/// denominator is the same integer every time.
///
/// **The surface corpus is where it varies, and by exactly one axis.** Its
/// histogram is recorded here rather than described because it is the contrast
/// that makes the paragraph above mean something: 57 names at arity 5, **7 at
/// arity 4**, and The Axes' 10 resisters at 0. Any cross-corpus comparison —
/// A-3 above is one — is therefore comparing unequal denominators on 7 of its
/// 64 candidates.
///
/// Ratchet-style, like the coincidence set: a move here changes what a mean
/// over this corpus means, and must be a deliberate act.
#[test]
fn the_underworld_corpus_has_constant_arity_and_the_surface_corpus_does_not() {
    let mut underworld: BTreeMap<usize, Vec<&'static str>> = BTreeMap::new();
    for name in underworld_assignment() {
        underworld
            .entry(name.vector.axis_ids().len())
            .or_default()
            .push(name.name);
    }
    let underworld_counts: Vec<(usize, usize)> = underworld
        .iter()
        .map(|(arity, names)| (*arity, names.len()))
        .collect();
    println!("underworld arity histogram: {underworld_counts:?}");
    assert_eq!(
        underworld_counts,
        vec![(0, 2), (5, 22)],
        "the underworld corpus's arity moved. A mean over shared axes is only \
         comparable across equal-arity vectors, so this is a change in what a \
         chamber ranking means: record the new histogram and say what moved it."
    );

    // Non-emptiness is asserted by the pair above (an empty corpus would give
    // an empty histogram, not this one), but the ARITY claim needs one more
    // thing said out loud: 5 is the size of `OCCUPIED`, not a coincidence.
    assert_eq!(
        OCCUPIED.len(),
        5,
        "the constant arity above is the occupied-axis count, and must stay tied to it"
    );

    let mut surface: BTreeMap<usize, usize> = BTreeMap::new();
    for name in assignment() {
        *surface.entry(name.vector.axis_ids().len()).or_default() += 1;
    }
    let surface_counts: Vec<(usize, usize)> = surface.into_iter().collect();
    println!("surface arity histogram: {surface_counts:?}");
    assert_eq!(
        surface_counts,
        vec![(0, 10), (4, 7), (5, 57)],
        "the surface corpus's arity moved, which changes the denominator bias \
         every cross-corpus distance in this file carries."
    );
}
