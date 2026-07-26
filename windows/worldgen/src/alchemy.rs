//! The composition root's alchemy carry: terrain and biosphere vocabulary in,
//! `hornvale_alchemy::Substrate` out.
//!
//! This is the ONLY place the two vocabularies meet. `domains/alchemy` knows
//! about qualities, not about rocks — it never imports `hornvale-terrain` or
//! `hornvale-species` — so the translation lives here, in the same shape and
//! for the same reason as [`crate::envelope_of`] does for language.
//!
//! Every function here is PURE: no seed, no stream, no draw. That is what
//! keeps The Assay free of a save-format contract.

use hornvale_alchemy::Substrate;
use hornvale_alchemy::production::{PRODUCTIONS, admits};
use hornvale_alchemy::quality::qualities_of;
use hornvale_species::BiosphereTraits;
use hornvale_terrain::{Commodity, RockClass, SoilOrder};

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
/// differentiation is deliberately out of scope for The Assay.
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
    /// takes no seed and consumes no stream, which is what keeps The Assay
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
}
