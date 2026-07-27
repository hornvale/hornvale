//! The composition root's alchemy carry: terrain and biosphere vocabulary in,
//! `hornvale_alchemy::Substrate` out.
//!
//! This is the ONLY place the two vocabularies meet. `domains/alchemy` knows
//! about qualities, not about rocks — it never imports `hornvale-terrain` or
//! `hornvale-species` — so the translation lives here, in the same shape and
//! for the same reason as [`crate::envelope_of`] does for language.
//!
//! Every function here is PURE: no seed, no stream, no draw. That is what
//! keeps The Reagent free of a save-format contract.

use hornvale_alchemy::Substrate;
use hornvale_alchemy::production::{PRODUCTIONS, admits};
use hornvale_alchemy::quality::qualities_of;
use hornvale_climate::GeneratedClimate;
use hornvale_species::BiosphereTraits;
use hornvale_terrain::{Commodity, GeneratedTerrain, RockClass, SoilOrder};

/// Carry an ore deposit into a substrate. `grade` is the deposit's already-
/// drawn ore grade in [0,1] — the one place a drawn quantity reaches alchemy,
/// and therefore the physical basis of the provenance confound.
/// type-audit: bare-ok(ratio: grade)
pub fn substrate_of_commodity(c: Commodity, grade: f64) -> Substrate {
    let (metallic, organic, saline, refractory) = match c {
        Commodity::Copper => (0.90, 0.00, 0.10, 0.40),
        Commodity::Gold => (1.00, 0.00, 0.00, 0.60),
        Commodity::LeadZinc => (0.85, 0.00, 0.15, 0.35),
        Commodity::Iron => (0.90, 0.00, 0.00, 0.70),
        Commodity::Salt => (0.00, 0.00, 1.00, 0.10),
        Commodity::Coal => (0.00, 0.90, 0.00, 0.15),
        Commodity::Gems => (0.20, 0.00, 0.00, 0.95),
        Commodity::Tin => (0.80, 0.00, 0.05, 0.25),
        Commodity::Bauxite => (0.60, 0.00, 0.10, 0.50),
    };
    Substrate {
        metallic,
        organic,
        saline,
        refractory,
        purity: grade.clamp(0.0, 1.0),
    }
}

/// Carry a rock class into a substrate, by petrological family. Grouped rather
/// than enumerated one-per-variant because the families are what the quality
/// axes actually respond to.
pub fn substrate_of_rock(r: RockClass) -> Substrate {
    let (metallic, organic, saline, refractory) = match r {
        // Felsic igneous.
        RockClass::Granite | RockClass::Rhyolite => (0.25, 0.00, 0.00, 0.75),
        // Mafic igneous.
        RockClass::Gabbro | RockClass::Basalt | RockClass::Andesite => (0.45, 0.00, 0.00, 0.80),
        // Siliceous clastics and their metamorphic equivalent.
        RockClass::Sandstone
        | RockClass::Conglomerate
        | RockClass::Chert
        | RockClass::Quartzite => (0.10, 0.00, 0.00, 0.90),
        // Argillaceous.
        RockClass::Shale | RockClass::Slate => (0.20, 0.05, 0.05, 0.50),
        // Chemical.
        RockClass::Evaporite => (0.00, 0.00, 1.00, 0.10),
        RockClass::Ironstone => (0.85, 0.00, 0.00, 0.70),
        RockClass::ReefLimestone | RockClass::Marble => (0.05, 0.10, 0.05, 0.35),
        RockClass::Coal => (0.00, 0.90, 0.00, 0.15),
        // Higher-grade metamorphic.
        RockClass::Schist | RockClass::Gneiss => (0.30, 0.00, 0.00, 0.70),
        // Unconsolidated.
        RockClass::Alluvium => (0.15, 0.10, 0.05, 0.40),
    };
    // Bedrock is what it is: no ore-grade dilution applies.
    Substrate {
        metallic,
        organic,
        saline,
        refractory,
        purity: 1.0,
    }
}

/// Carry a soil order into a substrate.
pub fn substrate_of_soil(s: SoilOrder) -> Substrate {
    let (metallic, organic, saline, refractory) = match s {
        SoilOrder::Laterite => (0.55, 0.05, 0.00, 0.50),
        SoilOrder::Podzol => (0.10, 0.40, 0.00, 0.25),
        SoilOrder::Chernozem => (0.05, 0.60, 0.00, 0.20),
        SoilOrder::Aridisol => (0.10, 0.10, 0.60, 0.30),
        SoilOrder::Loam => (0.10, 0.45, 0.05, 0.25),
        SoilOrder::Andosol => (0.25, 0.35, 0.00, 0.50),
        SoilOrder::Leptosol => (0.20, 0.10, 0.00, 0.60),
        SoilOrder::Histosol => (0.00, 0.95, 0.00, 0.10),
        SoilOrder::Gley => (0.10, 0.50, 0.05, 0.20),
    };
    Substrate {
        metallic,
        organic,
        saline,
        refractory,
        purity: 1.0,
    }
}

/// Carry living matter into a substrate. Every organism is, alchemically, the
/// same kind of thing at this fidelity: organic and nothing else. Species
/// differentiation is deliberately out of scope for The Reagent.
pub fn substrate_of_life() -> Substrate {
    Substrate {
        metallic: 0.0,
        organic: 1.0,
        saline: 0.05,
        refractory: 0.05,
        purity: 1.0,
    }
}

/// Carry a species' biosphere traits into a substrate. Currently identical to
/// [`substrate_of_life`]; the parameter is taken so that later campaigns can
/// differentiate by trait without changing every call site.
pub fn substrate_of_traits(_t: &BiosphereTraits) -> Substrate {
    substrate_of_life()
}

/// Which productions a world endowed with these material sources can reach.
///
/// Returned sorted and deduplicated so the result is deterministic and
/// directly comparable between worlds.
/// type-audit: bare-ok(identifier-text: return)
pub fn reachable_productions(sources: &[Substrate]) -> Vec<&'static str> {
    let mut names: Vec<&'static str> = PRODUCTIONS
        .iter()
        .filter(|p| sources.iter().any(|s| admits(p, &qualities_of(s))))
        .map(|p| p.name)
        .collect();
    names.sort_unstable();
    names.dedup();
    names
}

/// A deterministic total order over `Substrate`'s five ratio fields.
///
/// `Substrate` itself derives neither `Ord` nor `Hash` (its fields are `f64`,
/// which has no total order under `PartialOrd` — NaN is unordered — and no
/// authored value here is ever NaN, but the type stays honest about that).
/// Callers that need a fixed, reproducible order (so two worlds' substance
/// sets are directly comparable) sort with this instead. Per project
/// convention, float comparison uses `total_cmp`, chained field-by-field so
/// the order is total even though no single field is.
fn substrate_order(a: &Substrate, b: &Substrate) -> std::cmp::Ordering {
    a.metallic
        .total_cmp(&b.metallic)
        .then_with(|| a.organic.total_cmp(&b.organic))
        .then_with(|| a.saline.total_cmp(&b.saline))
        .then_with(|| a.refractory.total_cmp(&b.refractory))
        .then_with(|| a.purity.total_cmp(&b.purity))
}

/// A world's substance samples, derived from its generated terrain and
/// climate — the seed-level closing of the carry functions above (spec §8
/// evidence items 2/3).
///
/// NOT a material inventory, despite the length: this returns one entry per
/// distinct `Substrate` VALUE, and because each ore deposit carries its own
/// drawn `grade`, a default globe yields thousands of entries against only
/// ~24 distinct material CATEGORIES. `dedup` collapses repeated rock and soil
/// substrates but is nearly inert on ore, whose grades are distinct floats.
/// If you want heterogeneity, count categories — the length of this `Vec` is
/// dominated by grade sampling, not by material variety.
///
/// Walks every land cell of `terrain`'s Geosphere and lands a substrate for
/// each geological source present — the bedrock ([`substrate_of_rock`]), the
/// climate-coupled soil ([`substrate_of_soil`], via [`crate::soil_of`]), and
/// any ore deposit ([`substrate_of_commodity`], via [`crate::deposit_of`]) —
/// plus living matter ([`substrate_of_life`]) wherever the cell is
/// habitable (`climate`'s habitability mask: the existing "could host a
/// vale-like settlement" signal, reused here as this campaign's biosphere-
/// presence proxy rather than authoring a new one). Ocean cells contribute
/// nothing — The Reagent's inventories are terrestrial, matching the
/// terrestrial-supply frame the rest of the composition root already uses.
///
/// Deduplicated and sorted by [`substrate_order`] so the result is
/// deterministic — the same seed's terrain/climate always walks the same
/// cells in the same order, and the sort removes any dependence on that
/// walk order besides — and directly comparable between worlds.
pub fn substances_of_world(
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
) -> Vec<Substrate> {
    let geo = terrain.geosphere();
    let soils = crate::soil_of(terrain, climate, geo);
    let mut substances: Vec<Substrate> = Vec::new();
    for cell in geo.cells() {
        if terrain.is_ocean(cell) {
            continue;
        }
        substances.push(substrate_of_rock(terrain.rock_at(cell)));
        substances.push(substrate_of_soil(*soils.get(cell)));
        if let Some(deposit) = crate::deposit_of(terrain, climate, geo, cell) {
            substances.push(substrate_of_commodity(deposit.commodity, deposit.grade));
        }
        if *climate.habitability().get(cell) {
            substances.push(substrate_of_life());
        }
    }
    substances.sort_by(substrate_order);
    substances.dedup();
    substances
}

/// The productions a world's material endowment can reach — [`substances_of_world`]
/// piped through [`reachable_productions`]. The seed-level entry point spec
/// §8 evidence items 2/3 ask for: same seed, same list, byte-identically;
/// materially different geology, materially different list.
/// type-audit: bare-ok(identifier-text: return)
pub fn reachable_productions_of_world(
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
) -> Vec<&'static str> {
    reachable_productions(&substances_of_world(terrain, climate))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Salt dissolves; gold does not. The carry has to preserve the material
    /// distinctions that make productions differ.
    #[test]
    fn salt_dissolves_and_gold_does_not() {
        let salt = qualities_of(&substrate_of_commodity(Commodity::Salt, 0.8));
        let gold = qualities_of(&substrate_of_commodity(Commodity::Gold, 0.8));
        assert!(
            salt.solubility > 0.6,
            "salt must be soluble: {}",
            salt.solubility
        );
        assert!(
            gold.solubility < 0.3,
            "gold must not be: {}",
            gold.solubility
        );
    }

    /// Coal burns; granite does not.
    #[test]
    fn coal_burns_and_granite_does_not() {
        let coal = qualities_of(&substrate_of_commodity(Commodity::Coal, 0.7));
        let granite = qualities_of(&substrate_of_rock(RockClass::Granite));
        assert!(coal.combustibility > 0.6);
        assert!(granite.combustibility < 0.2);
    }

    /// Ore grade reaches the quality layer: poor ore is more caustic than
    /// rich ore of the SAME commodity. This is the provenance confound's
    /// physical mechanism, and the reason a doctrine formed in one valley
    /// fails in the next.
    #[test]
    fn ore_grade_moves_causticity() {
        let rich = qualities_of(&substrate_of_commodity(Commodity::Copper, 0.9));
        let poor = qualities_of(&substrate_of_commodity(Commodity::Copper, 0.1));
        assert!(poor.causticity > rich.causticity);
    }

    /// Worlds made of different things reach different productions. This is
    /// the campaign's one substantive claim about the WORLD rather than about
    /// the code.
    #[test]
    fn different_material_endowments_reach_different_productions() {
        let mining_world = [
            substrate_of_commodity(Commodity::Copper, 0.6),
            substrate_of_commodity(Commodity::Iron, 0.7),
            substrate_of_rock(RockClass::Granite),
        ];
        let marsh_world = [
            substrate_of_soil(SoilOrder::Histosol),
            substrate_of_commodity(Commodity::Salt, 0.5),
            substrate_of_life(),
        ];

        let mining = reachable_productions(&mining_world);
        let marsh = reachable_productions(&marsh_world);

        assert_ne!(mining, marsh, "endowments this different must diverge");
        assert!(
            marsh.contains(&"ferment-must"),
            "a living marsh can ferment: {marsh:?}"
        );
        assert!(
            !mining.contains(&"ferment-must"),
            "bare rock cannot: {mining:?}"
        );
    }

    /// The carry is a pure function: same input, same output, always. It
    /// takes no seed and consumes no stream, which is what keeps The Reagent
    /// free of a save-format contract.
    #[test]
    fn the_carry_is_pure() {
        for _ in 0..3 {
            assert_eq!(
                substrate_of_commodity(Commodity::Tin, 0.42),
                substrate_of_commodity(Commodity::Tin, 0.42)
            );
        }
    }

    /// `reachable_productions` documents a sort+dedup contract: the result
    /// is "sorted and deduplicated so the result is deterministic and
    /// directly comparable between worlds." Feed it several sources whose
    /// UNION admits every authored production; the production table's own
    /// declaration order is not alphabetical, so the natural (unsorted)
    /// iteration order differs from the required output. Assert the
    /// property itself — strictly increasing adjacent pairs proves both
    /// sortedness and freedom from duplicates in one shot — rather than a
    /// hardcoded expected list, so this keeps working as the table grows.
    #[test]
    fn reachable_productions_is_sorted_and_deduplicated() {
        let sources = [
            substrate_of_life(),
            substrate_of_commodity(Commodity::Gold, 1.0),
            substrate_of_commodity(Commodity::Salt, 1.0),
        ];
        let names = reachable_productions(&sources);

        assert!(
            names.len() >= 2,
            "need at least two reachable productions to exercise ordering: {names:?}"
        );
        assert!(
            names.windows(2).all(|w| w[0] < w[1]),
            "result must be strictly increasing — sorted and free of duplicates: {names:?}"
        );
    }

    /// Build a `Substrate` from its five raw fields, for the table-pinning
    /// tests below where the expected values are read off the authored
    /// tables directly.
    fn substrate(
        metallic: f64,
        organic: f64,
        saline: f64,
        refractory: f64,
        purity: f64,
    ) -> Substrate {
        Substrate {
            metallic,
            organic,
            saline,
            refractory,
            purity,
        }
    }

    /// Pin every `Commodity` variant's row in `substrate_of_commodity`'s
    /// authored table. These rows are authored data with no computation to
    /// check against — a miscopied or wrongly-grouped row is exactly the
    /// defect that must not ship silently, so every variant is listed
    /// explicitly. Note the limit: this is a hand-written array, not an
    /// exhaustive match, so adding a variant to `Commodity` will NOT break
    /// this test — add the new row here by hand.
    #[test]
    fn substrate_of_commodity_matches_the_authored_table() {
        let grade = 1.0;
        let cases: [(Commodity, Substrate); 9] = [
            (Commodity::Copper, substrate(0.90, 0.00, 0.10, 0.40, 1.0)),
            (Commodity::Gold, substrate(1.00, 0.00, 0.00, 0.60, 1.0)),
            (Commodity::LeadZinc, substrate(0.85, 0.00, 0.15, 0.35, 1.0)),
            (Commodity::Iron, substrate(0.90, 0.00, 0.00, 0.70, 1.0)),
            (Commodity::Salt, substrate(0.00, 0.00, 1.00, 0.10, 1.0)),
            (Commodity::Coal, substrate(0.00, 0.90, 0.00, 0.15, 1.0)),
            (Commodity::Gems, substrate(0.20, 0.00, 0.00, 0.95, 1.0)),
            (Commodity::Tin, substrate(0.80, 0.00, 0.05, 0.25, 1.0)),
            (Commodity::Bauxite, substrate(0.60, 0.00, 0.10, 0.50, 1.0)),
        ];
        for (commodity, expected) in cases {
            assert_eq!(
                substrate_of_commodity(commodity, grade),
                expected,
                "{commodity:?}"
            );
        }
    }

    /// Pin every `RockClass` variant's row in `substrate_of_rock`'s authored
    /// table, grouped by petrological family exactly as the production code
    /// groups them. Listed one variant at a time. Note the limit: this is a
    /// hand-written array, not an exhaustive match, so adding a variant to
    /// `RockClass` will NOT break this test — add the new row here by hand.
    #[test]
    fn substrate_of_rock_matches_the_authored_table() {
        let cases: [(RockClass, Substrate); 19] = [
            (RockClass::Granite, substrate(0.25, 0.00, 0.00, 0.75, 1.0)),
            (RockClass::Rhyolite, substrate(0.25, 0.00, 0.00, 0.75, 1.0)),
            (RockClass::Gabbro, substrate(0.45, 0.00, 0.00, 0.80, 1.0)),
            (RockClass::Basalt, substrate(0.45, 0.00, 0.00, 0.80, 1.0)),
            (RockClass::Andesite, substrate(0.45, 0.00, 0.00, 0.80, 1.0)),
            (RockClass::Sandstone, substrate(0.10, 0.00, 0.00, 0.90, 1.0)),
            (
                RockClass::Conglomerate,
                substrate(0.10, 0.00, 0.00, 0.90, 1.0),
            ),
            (RockClass::Chert, substrate(0.10, 0.00, 0.00, 0.90, 1.0)),
            (RockClass::Quartzite, substrate(0.10, 0.00, 0.00, 0.90, 1.0)),
            (RockClass::Shale, substrate(0.20, 0.05, 0.05, 0.50, 1.0)),
            (RockClass::Slate, substrate(0.20, 0.05, 0.05, 0.50, 1.0)),
            (RockClass::Evaporite, substrate(0.00, 0.00, 1.00, 0.10, 1.0)),
            (RockClass::Ironstone, substrate(0.85, 0.00, 0.00, 0.70, 1.0)),
            (
                RockClass::ReefLimestone,
                substrate(0.05, 0.10, 0.05, 0.35, 1.0),
            ),
            (RockClass::Marble, substrate(0.05, 0.10, 0.05, 0.35, 1.0)),
            (RockClass::Coal, substrate(0.00, 0.90, 0.00, 0.15, 1.0)),
            (RockClass::Schist, substrate(0.30, 0.00, 0.00, 0.70, 1.0)),
            (RockClass::Gneiss, substrate(0.30, 0.00, 0.00, 0.70, 1.0)),
            (RockClass::Alluvium, substrate(0.15, 0.10, 0.05, 0.40, 1.0)),
        ];
        for (rock, expected) in cases {
            assert_eq!(substrate_of_rock(rock), expected, "{rock:?}");
        }
    }

    /// Pin every `SoilOrder` variant's row in `substrate_of_soil`'s authored
    /// table. Listed one variant at a time. Note the limit: this is a
    /// hand-written array, not an exhaustive match, so adding a variant to
    /// `SoilOrder` will NOT break this test — add the new row here by hand.
    #[test]
    fn substrate_of_soil_matches_the_authored_table() {
        let cases: [(SoilOrder, Substrate); 9] = [
            (SoilOrder::Laterite, substrate(0.55, 0.05, 0.00, 0.50, 1.0)),
            (SoilOrder::Podzol, substrate(0.10, 0.40, 0.00, 0.25, 1.0)),
            (SoilOrder::Chernozem, substrate(0.05, 0.60, 0.00, 0.20, 1.0)),
            (SoilOrder::Aridisol, substrate(0.10, 0.10, 0.60, 0.30, 1.0)),
            (SoilOrder::Loam, substrate(0.10, 0.45, 0.05, 0.25, 1.0)),
            (SoilOrder::Andosol, substrate(0.25, 0.35, 0.00, 0.50, 1.0)),
            (SoilOrder::Leptosol, substrate(0.20, 0.10, 0.00, 0.60, 1.0)),
            (SoilOrder::Histosol, substrate(0.00, 0.95, 0.00, 0.10, 1.0)),
            (SoilOrder::Gley, substrate(0.10, 0.50, 0.05, 0.20, 1.0)),
        ];
        for (soil, expected) in cases {
            assert_eq!(substrate_of_soil(soil), expected, "{soil:?}");
        }
    }

    /// Build a world only as deep as terrain genesis (`BuildDepth::Terrain`)
    /// — the cheapest depth that yields a real, generated terrain/climate
    /// pair — then reconstruct both off it exactly the way every other
    /// `*_lines_from`/`history_for` view in `lib.rs` does. Reused by every
    /// seed-level test below so world generation happens once per seed.
    fn terrain_and_climate_with_pins(
        seed: u64,
        terrain_pins: &hornvale_terrain::TerrainPins,
    ) -> (GeneratedTerrain, GeneratedClimate) {
        let wc = crate::WorldComponents::assemble().expect("component roster assembles");
        let world = crate::build_world_to(
            hornvale_kernel::Seed(seed),
            &crate::SkyPins::default(),
            crate::SkyChoice::Generated,
            terrain_pins,
            &crate::SettlementPins::default(),
            &wc,
            crate::BuildDepth::Terrain,
        )
        .expect("a terrain-depth build succeeds");
        let terrain = crate::terrain_of(&world).expect("terrain reconstructs");
        let climate = crate::climate_from(&world, &terrain).expect("climate reconstructs");
        (terrain, climate)
    }

    /// [`terrain_and_climate_with_pins`] at the crate's default terrain pins
    /// — the ordinary, full-size globe.
    fn terrain_and_climate_for(seed: u64) -> (GeneratedTerrain, GeneratedClimate) {
        terrain_and_climate_with_pins(seed, &hornvale_terrain::TerrainPins::default())
    }

    /// A small, mostly-ocean globe: pinned so land area (and therefore
    /// rock/soil/ore variety) stays small. Used only by the divergence test
    /// below — see its doc comment for why a full default-size globe cannot
    /// exercise this claim.
    fn sparse_terrain_pins() -> hornvale_terrain::TerrainPins {
        hornvale_terrain::TerrainPins {
            ocean_fraction: Some(0.95),
            plates: Some(2),
            continents: Some(1),
            globe_level: Some(4),
            ..hornvale_terrain::TerrainPins::default()
        }
    }

    /// Spec §8 evidence item 2: the same seed yields exactly the same
    /// substance set and the same reachable-production list, byte-
    /// identically, across independent world (re)generations.
    #[test]
    fn same_seed_yields_the_same_substances_and_productions() {
        let (terrain_a, climate_a) = terrain_and_climate_for(42);
        let (terrain_b, climate_b) = terrain_and_climate_for(42);

        let substances_a = substances_of_world(&terrain_a, &climate_a);
        let substances_b = substances_of_world(&terrain_b, &climate_b);
        assert_eq!(
            substances_a, substances_b,
            "the same seed must yield the same substance set"
        );

        let productions_a = reachable_productions_of_world(&terrain_a, &climate_a);
        let productions_b = reachable_productions_of_world(&terrain_b, &climate_b);
        assert_eq!(
            productions_a, productions_b,
            "the same seed must yield the same reachable-production list"
        );
    }

    /// Spec §8 evidence item 3 — the campaign's one substantive claim about
    /// the WORLD rather than the code: two seeds with materially different
    /// geology reach materially different production sets.
    ///
    /// What was tried first: seeds 1 and 42 at the crate's DEFAULT terrain
    /// pins. Their substance sets do diverge (asserted below is not
    /// vacuous), but every seed tried at default pins — 0 through 39, by
    /// hand, plus 1 and 42 — reaches the SAME all-7 reachable-production
    /// set. That is not a bug: `PRODUCTIONS` has only 7 entries today (see
    /// `domains/alchemy/src/production.rs`), several of their thresholds are
    /// generous (e.g. `grind-stone`'s malleability <= 0.3 admits nearly
    /// every rock; `dissolve-salt` is admitted by ordinary Aridisol soil,
    /// not only evaporite), and a default-size globe has thousands of land
    /// cells — enough that essentially every world's soil/rock/climate
    /// variety alone saturates the whole table regardless of seed. So
    /// production-set divergence needs a world small enough that this
    /// saturation does NOT happen: [`sparse_terrain_pins`] pins a small,
    /// mostly-ocean globe (few land cells, few plates), and at THAT
    /// configuration seeds do diverge — e.g. seed 0 reaches 6 productions
    /// (no `dissolve-salt`: this particular small landmass never rolls
    /// evaporite rock or arid-enough soil) while seed 2 reaches all 7. Pins
    /// are held fixed and identical between the two builds; only the seed
    /// differs, so the divergence is attributable to the seed, exactly as
    /// item 3 asks for.
    #[test]
    fn materially_different_geology_reaches_materially_different_productions() {
        // The substance-set claim holds even at default pins.
        let (terrain_a, climate_a) = terrain_and_climate_for(1);
        let (terrain_b, climate_b) = terrain_and_climate_for(42);
        let substances_a = substances_of_world(&terrain_a, &climate_a);
        let substances_b = substances_of_world(&terrain_b, &climate_b);
        assert_ne!(
            substances_a, substances_b,
            "seeds 1 and 42 must land materially different substance sets"
        );

        // The reachable-production claim needs the small-globe pins (see
        // the doc comment above for why default-size worlds saturate).
        let pins = sparse_terrain_pins();
        let (sparse_a, sparse_climate_a) = terrain_and_climate_with_pins(0, &pins);
        let (sparse_b, sparse_climate_b) = terrain_and_climate_with_pins(2, &pins);
        let productions_a = reachable_productions_of_world(&sparse_a, &sparse_climate_a);
        let productions_b = reachable_productions_of_world(&sparse_b, &sparse_climate_b);
        assert_ne!(
            productions_a, productions_b,
            "seeds 0 and 2 at the same small-globe pins must reach different production sets: {productions_a:?} vs {productions_b:?}"
        );
    }

    /// Companion to `reachable_productions_is_sorted_and_deduplicated`, for
    /// the substance side: `substances_of_world` must return no two adjacent
    /// equal entries (dedup) and must be sorted by [`substrate_order`].
    /// `Substrate` has no `Ord`, so unlike the productions test (which gets
    /// both properties from one "strictly increasing" sweep over `&str`)
    /// this needs two separate checks.
    #[test]
    fn substances_of_world_is_sorted_and_deduplicated() {
        let (terrain, climate) = terrain_and_climate_for(42);
        let substances = substances_of_world(&terrain, &climate);
        assert!(
            substances.len() > 1,
            "need at least two substances to exercise ordering: {} found",
            substances.len()
        );
        assert!(
            substances
                .windows(2)
                .all(|w| substrate_order(&w[0], &w[1]) != std::cmp::Ordering::Greater),
            "substances must be sorted by substrate_order"
        );
        assert!(
            substances.windows(2).all(|w| w[0] != w[1]),
            "substances must be deduplicated (no adjacent equal entries)"
        );
    }

    /// A real generated world reaches a non-empty, sensible production set:
    /// every name it returns is one the production table actually declares.
    #[test]
    fn a_real_world_reaches_a_nonempty_sensible_production_set() {
        let (terrain, climate) = terrain_and_climate_for(42);
        let productions = reachable_productions_of_world(&terrain, &climate);
        assert!(
            !productions.is_empty(),
            "a real world must reach at least one production"
        );
        for name in &productions {
            assert!(
                PRODUCTIONS.iter().any(|p| &p.name == name),
                "{name} must be a production the table actually declares"
            );
        }
    }
}
