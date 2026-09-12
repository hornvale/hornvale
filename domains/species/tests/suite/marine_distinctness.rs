//! **M5 — the six marine kinds are distinct on axes that ARRIVE.**
//!
//! THE TIDEMARK, Task 3, and the guard against the defect the predecessors
//! shipped. The Delvers authored two subterranean peoples and withdrew them:
//! "a kind whose identity is DEPTH cannot be expressed by an axis measured in
//! metres above sea level", and `habitat_realm_registry` states the general
//! form — "the trap is not authoring a subterranean kind, it is
//! distinguishing two kinds by DEPTH, which nothing in the model can say."
//!
//! So the campaign's rule, spec §3.4, testable rather than hortatory:
//!
//! > **No two marine kinds may differ only by stratum.** Every pair must be
//! > separable on at least one axis the model already carries.
//!
//! This is a permanent TEST rather than a one-off measurement (the brief's
//! own instruction), because a later campaign re-authoring one of these six
//! can collapse a pair without noticing, and the number it would collapse is
//! not written down anywhere else.
//!
//! # The exclusions, and why each is stricter than "count the differences"
//!
//! **The exclusion is the point** (spec §8, M5): "an earlier draft of this
//! measurement counted any authored difference, which would have let a pair
//! separated only by insolation curves score 1 and pass — a distinctness test
//! satisfied by a distinction the engine cannot see."
//!
//! Four axes are excluded here, and the last of the four is stricter than the
//! spec asks for:
//!
//! 1. **`elevation`** — this IS stratum. Depth and height-above-sea-level are
//!    the same quantity in the ocean with a sign flip, so counting the
//!    elevation response would be counting exactly the thing the rule forbids
//!    a pair from resting on.
//! 2. **`moisture`** — spec §3.4's table marks it as not arriving. A
//!    submerged sample's moisture is a constant, and the value a marine
//!    kind's curve is evaluated against at placement is the SURFACE moisture
//!    of the vertex.
//! 3. **`insolation`** — the same table marks it "populated per band, read by
//!    nothing".
//! 4. **`temperature`** — and this one the table marks as ARRIVING, which is
//!    true of the plumbing and false of the effect. `tolerance_liebig` takes
//!    an elevation fast path: `if elevation <= floor_buf { return elevation }`,
//!    where the elevation response is the only UNFLOORED axis and therefore
//!    cannot exceed its own `devotion`. Every people this roster ships is
//!    authored PREPARED — `devotion_elev` `0.30` against sovereignty floors
//!    of 0.42–0.45 — so the fast path fires at every vertex and temperature,
//!    moisture and insolation are computed and discarded. `sea_elf_condition_
//!    niche`'s own doc has said so since The Radiation ("PREPARED: never
//!    binds"). Excluding it costs this test nothing.
//!
//! What is LEFT is where the six actually differ, and `biome_affinity` is the
//! load-bearing member: a graded per-biome factor, resolved by biome NAME,
//! multiplied OUTSIDE the Liebig minimum, and therefore the only per-vertex
//! environmental channel a PREPARED people has at all.
//!
//! # What this measurement does NOT claim, said because it claimed it once
//!
//! The sentence above used to end: "...and buys it the property that every
//! axis it counts is an axis that reaches a placed settlement." **That is
//! false, and it is false in the campaign's own signature shape** — a
//! sentence true of the exclusions and false of what is left, in a header
//! written to warn about exactly that. `per_species_capacity_at` takes
//! `BiosphereTraits`, `HabitatRealm` and `BiomeAffinity` and nothing else, so
//! `sociality`, `status_basis`, `in_group_radius` and `activity_cycle` reach
//! placement NOWHERE. Four of the axes counted below are model-carried and do
//! not site a settlement.
//!
//! What this test measures is what spec §8's M5 asks for — "the model-carried
//! axes on which the pair differs" — and §3.4 names `SocietyVector` and
//! `PerceptionVector` among the axes available, so counting them is the
//! preregistration honoured rather than stretched. The assertion is correct
//! as it stands and is deliberately unchanged.
//!
//! **The placement-reaching subset is the smaller number, and it is recorded
//! here rather than asserted**, because asserting it would be a second,
//! unpreregistered measurement smuggled into M5's name. For the worst pair —
//! `reef-mason` vs `triton`, the pair M5's minimum of 5 names — the axes that
//! reach `per_species_capacity_at` are `mass` and `biome_affinity`: **2**.
//! Read M5 as "the six are distinct kinds", never as "the six are distinct
//! places"; M2 and M7 are the measurements that speak to placement.

use hornvale_kernel::{
    ANIMAL_PREY, CHEMOSYNTHATE, DETRITUS, KindId, MARINE_FORAGE, MINERAL, PHOTOSYNTHATE,
    PLANT_FORAGE, ResourceAxis,
};
use hornvale_species::{
    HabitatRealm, biome_affinity_registry, biosphere_registry, habitat_realm_registry,
    perception_registry, society_registry,
};

/// The seven supply axes `windows/worldgen`'s `SUPPLY_AXIS_ORDER` scores, in
/// that order. Restated here rather than imported because a domain crate
/// cannot depend on a window; `the_supply_axes_this_test_counts_are_the_ones_
/// a_niche_can_carry` below keeps the two honest by requiring every axis a
/// marine kind actually weights to appear in this list.
const SUPPLY_AXES: [ResourceAxis; 7] = [
    PHOTOSYNTHATE,
    PLANT_FORAGE,
    MINERAL,
    DETRITUS,
    ANIMAL_PREY,
    MARINE_FORAGE,
    CHEMOSYNTHATE,
];

/// Every marine kind, ascending by `KindId`.
fn marine_kinds() -> Vec<KindId> {
    habitat_realm_registry()
        .iter()
        .filter(|(_, realm)| **realm == HabitatRealm::Marine)
        .map(|(kind, _)| *kind)
        .collect()
}

/// The axes this measurement counts, as `(name, reader)` pairs producing a
/// comparable rendering of one kind's value. A rendering rather than a
/// comparison so a failure can print what the two kinds actually carry.
fn axis_readings(kind: KindId) -> Vec<(&'static str, String)> {
    let bio = biosphere_registry();
    let b = bio
        .get(&kind)
        .unwrap_or_else(|| panic!("{kind:?} has a biosphere row"));
    let society = society_registry();
    let perception = perception_registry();
    let affinity = biome_affinity_registry();

    let mut out: Vec<(&'static str, String)> = vec![
        ("social_form", format!("{:?}", b.social_form)),
        ("trophic_mode", format!("{:?}", b.trophic_mode)),
        ("thermal_strategy", format!("{:?}", b.thermal_strategy)),
        ("life_schedule", format!("{:?}", b.schedule)),
        ("mass", format!("{:?}", b.mass)),
        ("potency", format!("{}", b.potency)),
    ];
    for axis in SUPPLY_AXES {
        out.push((axis.label, format!("{}", b.niche.weight(axis))));
    }
    let s = society
        .get(&kind)
        .unwrap_or_else(|| panic!("{kind:?} is minded and social, so it has a society row"));
    out.push(("sociality", format!("{:?}", s.sociality)));
    out.push(("status_basis", format!("{:?}", s.status_basis)));
    out.push(("in_group_radius", format!("{}", s.in_group_radius)));
    let p = perception
        .get(&kind)
        .unwrap_or_else(|| panic!("{kind:?} speaks, so it has a perception row"));
    out.push(("activity_cycle", format!("{:?}", p.activity)));
    // The affinity is rendered as its whole authored shape — default plus
    // the ordered per-biome overrides — because a difference in ANY of those
    // is a difference in where the kind's capacity is scaled, and the shape
    // is what placement reads.
    let a = affinity
        .get(&kind)
        .unwrap_or_else(|| panic!("{kind:?} carries a biome affinity row"));
    out.push(("biome_affinity", format!("{:?}", a)));
    out
}

/// The campaign's headline distinctness measurement.
///
/// Prints the full per-pair table on every run, so the number is a printed
/// measurement rather than prose that can rot, and asserts only the minimum.
#[test]
fn no_pair_of_marine_kinds_differs_only_by_stratum() {
    let kinds = marine_kinds();
    assert_eq!(
        kinds.len(),
        6,
        "M5 is defined over the six marine peoples; the realm store holds \
         {kinds:?}"
    );

    let readings: Vec<Vec<(&'static str, String)>> =
        kinds.iter().map(|k| axis_readings(*k)).collect();

    let mut minimum = usize::MAX;
    let mut worst: Option<(&str, &str)> = None;
    let mut pairs = 0usize;
    println!(
        "M5 — pairwise differing axes (stratum, moisture, insolation and temperature excluded):"
    );
    for i in 0..kinds.len() {
        for j in (i + 1)..kinds.len() {
            pairs += 1;
            let differing: Vec<&str> = readings[i]
                .iter()
                .zip(&readings[j])
                .filter(|((a_name, a), (b_name, b))| {
                    assert_eq!(a_name, b_name, "the two readings must be aligned");
                    a != b
                })
                .map(|((name, _), _)| *name)
                .collect();
            println!(
                "  {:>15} vs {:<15} {:>2}  {}",
                kinds[i].0,
                kinds[j].0,
                differing.len(),
                differing.join(", ")
            );
            if differing.len() < minimum {
                minimum = differing.len();
                worst = Some((kinds[i].0, kinds[j].0));
            }
        }
    }
    assert_eq!(pairs, 15, "six kinds give fifteen unordered pairs");
    let (a, b) = worst.expect("fifteen pairs were compared");
    println!("M5 minimum over {pairs} pairs: {minimum} (at {a} vs {b})");
    assert!(
        minimum >= 1,
        "M5 FAILS at ({a}, {b}): that pair differs on {minimum} model-carried \
         axes once stratum and the non-arriving axes are excluded, which is \
         the Delvers' defect reproduced — two kinds separated by depth alone. \
         The remedy is to merge the two kinds or re-author one, NOT to argue \
         that their depths are far apart (spec §8, M5)."
    );
}

/// **Anti-vacuity, in the direction that actually threatens this test.**
///
/// The measurement above would pass trivially if `axis_readings` silently
/// stopped reading something — a registry lookup that returned a default, an
/// axis dropped from `SUPPLY_AXES`. So: every axis a marine kind actually
/// weights must be one this test counts, and the reading vector must be the
/// length the list above implies.
#[test]
fn the_supply_axes_this_test_counts_are_the_ones_a_niche_can_carry() {
    let bio = biosphere_registry();
    for kind in marine_kinds() {
        let b = bio.get(&kind).expect("a marine kind has a biosphere row");
        let mut weighted = 0usize;
        for axis in SUPPLY_AXES {
            if b.niche.weight(axis) > 0.0 {
                weighted += 1;
            }
        }
        assert!(
            weighted >= 1,
            "{kind:?} weights none of the axes this test counts — either the \
             kind draws no supply at all (it would be unplaceable) or \
             SUPPLY_AXES has drifted from the kernel's basis"
        );
        assert_eq!(
            axis_readings(kind).len(),
            6 + SUPPLY_AXES.len() + 3 + 1 + 1,
            "{kind:?}'s reading vector is not the arity this file documents"
        );
    }
}

/// **The excluded axes are excluded because they cannot discriminate, and
/// that is asserted rather than argued for the two where it is structural.**
///
/// `moisture` and `insolation` are authored ONCE, in
/// `marine_prepared_climate`, and shared by five of the six; the abyssal elf
/// takes the elf family's pair for the reason its own doc gives. So five of
/// the six are byte-identical on both axes by construction, and a future
/// campaign that gave one of them a bespoke moisture curve would be authoring
/// a difference the marine instrument cannot express. This fails if that
/// happens, which is the point: the exclusion in the measurement above is
/// then no longer conservative, it is hiding something.
#[test]
fn the_five_sharing_kinds_are_byte_identical_on_the_two_axes_that_do_not_arrive() {
    let bio = biosphere_registry();
    let shared: Vec<KindId> = marine_kinds()
        .into_iter()
        .filter(|k| k.0 != "abyssal-elf")
        .collect();
    assert_eq!(shared.len(), 5, "the abyssal elf is the one exception");
    let first = bio
        .get(&shared[0])
        .expect("a marine kind has a biosphere row")
        .condition_niche;
    for kind in &shared[1..] {
        let cn = bio
            .get(kind)
            .expect("a marine kind has a biosphere row")
            .condition_niche;
        assert_eq!(
            cn.moisture, first.moisture,
            "{kind:?}'s marine moisture curve differs from its siblings'. \
             `marine_prepared_climate` authors ONE reading for all five \
             deliberately: at placement this curve is evaluated against the \
             SURFACE moisture of the vertex, so a per-kind difference here is \
             a difference the marine instrument cannot express"
        );
        assert_eq!(
            cn.insolation, first.insolation,
            "{kind:?}'s marine insolation curve differs from its siblings'. \
             The pelagic light ladder is populated and read by nothing, and \
             the value this curve meets at placement is the surface field — \
             see `marine_prepared_climate`"
        );
    }
}
