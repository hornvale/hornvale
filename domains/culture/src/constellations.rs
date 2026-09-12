//! Culture-owned interpretation of the naked-eye sky.
//!
//! Astronomy supplies observer-filtered physical candidates; this module owns
//! grouping, names, and meanings. It deliberately has no astronomy dependency.

/// A stable, observer-filtered naked-eye star candidate at one observation.
/// type-audit: bare-ok(identifier-text: id), pending(wave-1: right_ascension_deg), pending(wave-1: declination_deg), pending(wave-1: apparent_magnitude), bare-ok(prose: physical_descriptor)
#[derive(Clone, Debug, PartialEq)]
pub struct SkyCandidate {
    /// Stable source identity, opaque to culture.
    pub id: String,
    /// Right ascension in degrees.
    pub right_ascension_deg: f64,
    /// Declination in degrees.
    pub declination_deg: f64,
    /// Apparent magnitude; smaller values are brighter.
    pub apparent_magnitude: f64,
    /// Optional physical descriptor supplied by the astronomy adapter.
    pub physical_descriptor: Option<String>,
}

/// The grouping preference a culture applies to the same physical candidates.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GroupingRule {
    /// Group canonical candidates in stable pairs from brightest outward.
    BrightnessPairs,
    /// Group canonical candidates in stable pairs from northern declinations.
    NorthernPairs,
}

/// Culture-owned words and grouping policy for sky interpretation.
/// type-audit: bare-ok(identifier-text: name_prefix), bare-ok(prose: meaning)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConstellationCulture {
    /// Prefix used for generated constellation names.
    pub name_prefix: String,
    /// Meaning attached to each generated group.
    pub meaning: String,
    /// How this culture chooses groups.
    pub grouping: GroupingRule,
}

/// A cultural grouping of visible stable-star identities.
/// type-audit: bare-ok(identifier-text: name), bare-ok(prose: meaning), bare-ok(identifier-text: member_ids)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Constellation {
    /// Culture-owned name.
    pub name: String,
    /// Culture-owned interpretation.
    pub meaning: String,
    /// Stable source IDs, in the culture's canonical group order.
    pub member_ids: Vec<String>,
}

/// Features intentionally deferred from this naked-eye constellation slice.
/// They require separate physical/event models and are not cultural inputs.
/// type-audit: bare-ok(identifier-text)
pub const DEFERRED_SKY_FEATURES: [&str; 4] = [
    "eclipsing binaries",
    "transient stellar events",
    "dense meteor-stream clumps",
    "terminal stellar evolution",
];

/// Derive culture-owned constellations from one already-filtered candidate set.
/// Empty input is valid and yields no constellations. The input is never
/// mutated; output depends only on the culture and candidate values.
pub fn derive_constellations(
    culture: &ConstellationCulture,
    candidates: &[SkyCandidate],
) -> Vec<Constellation> {
    let mut ordered = candidates.to_vec();
    match culture.grouping {
        GroupingRule::BrightnessPairs => ordered.sort_by(|a, b| {
            a.apparent_magnitude
                .total_cmp(&b.apparent_magnitude)
                .then_with(|| a.id.cmp(&b.id))
        }),
        GroupingRule::NorthernPairs => ordered.sort_by(|a, b| {
            b.declination_deg
                .total_cmp(&a.declination_deg)
                .then_with(|| a.id.cmp(&b.id))
        }),
    }
    ordered
        .chunks(2)
        .enumerate()
        .map(|(index, members)| Constellation {
            name: format!("{} {}", culture.name_prefix, index + 1),
            meaning: culture.meaning.clone(),
            member_ids: members
                .iter()
                .map(|candidate| candidate.id.clone())
                .collect(),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn candidates() -> Vec<SkyCandidate> {
        vec![
            SkyCandidate {
                id: "north-faint".into(),
                right_ascension_deg: 10.0,
                declination_deg: 70.0,
                apparent_magnitude: 4.0,
                physical_descriptor: None,
            },
            SkyCandidate {
                id: "south-bright".into(),
                right_ascension_deg: 20.0,
                declination_deg: -40.0,
                apparent_magnitude: 1.0,
                physical_descriptor: Some("giant".into()),
            },
            SkyCandidate {
                id: "equator".into(),
                right_ascension_deg: 30.0,
                declination_deg: 0.0,
                apparent_magnitude: 2.0,
                physical_descriptor: None,
            },
        ]
    }

    #[test]
    fn two_cultures_group_and_name_the_same_sky_differently() {
        let candidates = candidates();
        let bright = derive_constellations(
            &ConstellationCulture {
                name_prefix: "Spear".into(),
                meaning: "hunters follow it".into(),
                grouping: GroupingRule::BrightnessPairs,
            },
            &candidates,
        );
        let north = derive_constellations(
            &ConstellationCulture {
                name_prefix: "River".into(),
                meaning: "the dead travel here".into(),
                grouping: GroupingRule::NorthernPairs,
            },
            &candidates,
        );
        assert_ne!(bright, north);
        assert_eq!(bright[0].member_ids, vec!["south-bright", "equator"]);
        assert_eq!(north[0].member_ids, vec!["north-faint", "equator"]);
    }

    #[test]
    fn empty_candidate_set_is_valid_for_daylight_or_low_acuity() {
        let culture = ConstellationCulture {
            name_prefix: "Any".into(),
            meaning: "nothing yet".into(),
            grouping: GroupingRule::BrightnessPairs,
        };
        assert!(derive_constellations(&culture, &[]).is_empty());
    }

    #[test]
    fn deferred_sky_features_are_not_constellation_inputs() {
        assert_eq!(DEFERRED_SKY_FEATURES.len(), 4);
        assert_eq!(DEFERRED_SKY_FEATURES[0], "eclipsing binaries");
        assert_eq!(DEFERRED_SKY_FEATURES[1], "transient stellar events");
        assert_eq!(DEFERRED_SKY_FEATURES[2], "dense meteor-stream clumps");
        assert_eq!(DEFERRED_SKY_FEATURES[3], "terminal stellar evolution");
    }
}
