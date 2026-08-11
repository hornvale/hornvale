//! THE RADIATION (C2d): the six elves' biome-affinity rows, checked for the
//! four SILENT failure modes before anything measures them.
//!
//! `range_readout.rs::every_authored_affinity_row_is_well_formed` already
//! enforces non-uniformity, key spelling, the `> 0.0` floor, the `<= 1.0`
//! ceiling and no duplicate keys, over EVERY row in the registry — so the six
//! elf rows inherit all of it. This file adds only what is specific to the
//! family: that all six are present, that Sea is confined to the shelf, and
//! that High's row is Wood's.

use hornvale_kernel::KindId;

/// The family, in the order the registry lists it.
const ELVES: [&str; 6] = [
    "desert-elf",
    "drow",
    "high-elf",
    "sea-elf",
    "snow-elf",
    "wood-elf",
];

/// **Sea takes the PRODUCTIVE SHALLOW BAND, not the whole ocean.**
///
/// The campaign's second premise-check and its correction. "The ocean is 2.7x
/// the land" (29,896 cells against 11,066 on seed 42) is a real number with the
/// wrong denominator: no elf gets all the land either. On the shelf band Sea
/// gets **1,425 cells** as a three-seed mean (42/7/1234) — larger than Desert's
/// 241, smaller than Snow's 3,849 — and the roster comes out balanced with no
/// outlier. The runaway exists ONLY if Sea is authored to the whole ocean.
///
/// This test is the thing that keeps it from being. The deep classes must not
/// be lifted above the row's default; the four shelf classes must be, and
/// strictly.
#[test]
fn the_sea_elf_is_confined_to_the_shelf_band() {
    const SHELF: [&str; 4] = ["coral-reef", "kelp-forest", "upwelling", "epipelagic"];
    const DEEP: [&str; 5] = [
        "mesopelagic",
        "bathypelagic",
        "abyssal",
        "hadal-trench",
        "sea-ice",
    ];
    let registry = hornvale_species::biome_affinity_registry();
    let sea = registry
        .get(&KindId("sea-elf"))
        .expect("sea-elf carries a biome affinity row");

    for b in SHELF {
        assert!(
            sea.factor(b) > sea.default,
            "sea-elf's {b} factor ({}) is not above its default ({}); the \
             shelf band must be a STRONGHOLD, and a factor at the default is \
             indistinguishable from silence",
            sea.factor(b),
            sea.default
        );
    }
    for b in DEEP {
        assert!(
            sea.factor(b) <= sea.default,
            "sea-elf's {b} factor ({}) is above its default ({}). A SETTLED \
             people needs shallow productive water — which is also why human \
             settlement is coastal — and `marine_forage_supply_field` already \
             grades the water that way (Upwelling 1.0, reef/kelp 0.85, \
             epipelagic 0.45, mesopelagic 0.15, bathypelagic 0.05). The \
             affinity SHARPENS that ranking; it must not contradict it, and it \
             must not open 25,640 cells of deep ocean.",
            sea.factor(b),
            sea.default
        );
    }
}

/// High's row is Wood's row, exactly. §3.6's contrast — Wood vs High isolates
/// MIND — is only single-variable if their environment is identical, and the
/// affinity is the loudest environmental channel either of them has.
#[test]
fn high_elfs_affinity_is_wood_elfs() {
    let registry = hornvale_species::biome_affinity_registry();
    let wood = registry.get(&KindId("wood-elf")).expect("wood-elf row");
    let high = registry.get(&KindId("high-elf")).expect("high-elf row");
    assert_eq!(
        wood, high,
        "high-elf's affinity differs from wood-elf's. High diverges in \
         psyche, society and language ONLY (spec §3.6, §4); an environmental \
         divergence makes P3 a two-variable comparison and High stops being a \
         control."
    );
}

/// Drow's row is Wood's row too — its authored separation is the realm gate
/// and nothing else, so that P4's mutation (remove the realm row, watch the
/// separation vanish) isolates the gate rather than the gate plus a biome
/// difference.
#[test]
fn drows_affinity_is_wood_elfs() {
    let registry = hornvale_species::biome_affinity_registry();
    let wood = registry.get(&KindId("wood-elf")).expect("wood-elf row");
    let drow = registry.get(&KindId("drow")).expect("drow row");
    assert_eq!(
        wood, drow,
        "drow's affinity differs from wood-elf's. Its only authored \
         separation from the surface elves is the REALM GATE (spec §3.5); a \
         second difference would make P4's mutation uninterpretable."
    );
}

/// **The ladder's LEVEL is derived, and this is where the derivation is
/// enforced rather than merely documented.**
///
/// Every factor in every row must be one of the three authored preference rungs
/// mapped through the kind's own sovereignty floor —
/// `floor + (1 - floor) * p` for `p` in
/// {[`AFFINITY_STRONGHOLD`](hornvale_species::AFFINITY_STRONGHOLD),
/// [`AFFINITY_NEAR`](hornvale_species::AFFINITY_NEAR),
/// [`AFFINITY_MARGINAL`](hornvale_species::AFFINITY_MARGINAL)} — and every
/// row's `default` must be that floor exactly.
///
/// This exists because the number it replaced, `0.25`, was **never derived**:
/// it entered The Range from illustrative test-fixture code in that campaign's
/// plan, was adopted as an authored constant, and was then adopted again as
/// house style for six more kinds without anyone having to state where it came
/// from. Nothing in the workspace could have noticed. A row that opts out of
/// the mapping — a hand-written literal, a level chosen because it makes some
/// other test pass — is now a red test rather than a quiet re-arrival at the
/// same problem.
///
/// The ordering half is not decoration. Applying `sovereignty_floor` as a bare
/// `default` while leaving the rungs as literal factors would put gnoll's
/// marginal step (`0.45`) *below* its own default (`0.495`) and the woolly
/// mammoth's below its `0.692` — a declared preference scoring worse than
/// silence. The remap is what makes `stronghold > near > marginal > default`
/// hold for any floor, and this asserts it per row rather than trusting the
/// algebra.
#[test]
fn every_row_is_the_ladder_mapped_through_the_kinds_sovereignty_floor() {
    use hornvale_species::{AFFINITY_MARGINAL, AFFINITY_NEAR, AFFINITY_STRONGHOLD};
    let biosphere = hornvale_species::biosphere_registry();
    let registry = hornvale_species::biome_affinity_registry();
    let mut checked = 0usize;

    for (kind, aff) in registry.iter() {
        // Drow and High carry WOOD's row entire — shape and level — because
        // each is a single-variable control (realm, mind) and a control that
        // also differs environmentally controls nothing. High shares wood's
        // mass anyway; drow does not (52.0 kg vs 55.0), so for drow this is a
        // deliberate departure from its own floor and is named here.
        let source = match kind.0 {
            "drow" | "high-elf" => "wood-elf",
            own => own,
        };
        let bio = biosphere
            .get(&KindId(source))
            .unwrap_or_else(|| panic!("{source} has no biosphere row"));
        let floor = hornvale_kernel::sovereignty_floor(bio.mass, bio.potency);
        let mapped = |p: f64| floor + (1.0 - floor) * p;
        // Whose mass and potency the failure message is about. Without this the
        // red for a drow regression reads `sovereignty_floor(55 kg, …)` while
        // drow is 52.0 kg, and the next reader spends the session looking for
        // the bug in the biosphere registry instead of in the substitution three
        // lines above. A failure message that does not explain its own numbers
        // is the same defect this whole arc keeps finding, one level down.
        let whose = if source == kind.0 {
            String::from("its own")
        } else {
            format!(
                "{source}'s, NOT {}'s — drow and high-elf take wood-elf's row \
                 entire, level included, so that each stays a single-variable \
                 control",
                kind.0
            )
        };

        println!(
            "   {:<16} level from {:<14} floor {floor:.6}  near {:.6}  marginal {:.6}",
            kind.0,
            source,
            mapped(AFFINITY_NEAR),
            mapped(AFFINITY_MARGINAL),
        );

        assert_eq!(
            aff.default.to_bits(),
            floor.to_bits(),
            "{kind:?}'s default is {} but its derived level is \
             sovereignty_floor({} kg, potency {}) = {floor}, and that mass and \
             potency are {whose}. The default is NOT an authored number — it is \
             the model's own statement of how much environmental unsuitability \
             that kind's mass and potency buy it off, and a literal here is the \
             `0.25` failure returning",
            aff.default,
            bio.mass.kilograms(),
            bio.potency,
        );

        for (biome, factor) in &aff.by_biome {
            let rung = [AFFINITY_STRONGHOLD, AFFINITY_NEAR, AFFINITY_MARGINAL]
                .into_iter()
                .find(|p| mapped(*p).to_bits() == factor.to_bits());
            assert!(
                rung.is_some(),
                "{kind:?} sets {biome:?} to {factor}, which is not any of the \
                 three ladder rungs mapped through its floor ({:.6} / {:.6} / \
                 {:.6}). A row states a SHAPE — stronghold, near, marginal — and \
                 never a level; build it with \
                 `BiomeAffinity::from_preferences`",
                mapped(AFFINITY_STRONGHOLD),
                mapped(AFFINITY_NEAR),
                mapped(AFFINITY_MARGINAL),
            );
        }

        assert!(
            mapped(AFFINITY_STRONGHOLD) > mapped(AFFINITY_NEAR)
                && mapped(AFFINITY_NEAR) > mapped(AFFINITY_MARGINAL)
                && mapped(AFFINITY_MARGINAL) > aff.default,
            "{kind:?}'s ladder is not strictly ordered: stronghold {:.6} > near \
             {:.6} > marginal {:.6} > default {:.6} must hold, or a DECLARED \
             preference scores below silence",
            mapped(AFFINITY_STRONGHOLD),
            mapped(AFFINITY_NEAR),
            mapped(AFFINITY_MARGINAL),
            aff.default,
        );

        // The top rung must be exactly 1.00 for every kind, whatever its
        // floor — that is what makes the eight rows comparable rung for rung
        // rather than eight private scales. Measured exact for all eight
        // present masses; a future kind that failed this would be telling us
        // the top of the ladder had stopped being a common reference point,
        // which is worth a red test rather than a silent drift.
        assert_eq!(
            mapped(AFFINITY_STRONGHOLD),
            1.0,
            "{kind:?}'s stronghold maps to {} rather than 1.0",
            mapped(AFFINITY_STRONGHOLD)
        );
        checked += 1;
    }
    assert!(checked > 0, "no row was checked; the guard is vacuous");
}

/// All six carry a row. Spec §3.7's parenthetical says "five of six" and is
/// treated as an erratum: P3(a)'s primary arm needs High to carry Wood's row
/// (bit-identical fields are impossible otherwise) and P4's wording — the
/// separation vanishes "up to Drow's own biome and curve authoring" —
/// presupposes Drow has biome authoring.
#[test]
fn all_six_elves_carry_an_affinity_row() {
    let registry = hornvale_species::biome_affinity_registry();
    for name in ELVES {
        assert!(
            registry.contains(&KindId(name)),
            "{name} has no biome-affinity row; all six are authored on the \
             affinity route (spec §3.1)"
        );
    }
}
