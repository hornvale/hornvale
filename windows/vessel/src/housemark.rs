//! The cultural mark a vessel derives from a settlement people's society.

use std::fmt;

use hornvale_species::{Sociality, SocietyVector};

/// The authority reading carried into a vessel's cultural grammar.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum AuthorityMark {
    /// Authority is organized by command.
    Command,
    /// Authority is organized in common.
    Common,
}

/// The posture a society takes at its in-group threshold.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ThresholdPosture {
    /// The group boundary is drawn inward.
    Inward,
    /// The group boundary occupies the middle band.
    Plain,
    /// The group boundary is drawn outward.
    Outward,
}

/// The cultural mark a vessel derives from one society vector.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Housemark {
    /// The society's authority reading.
    pub authority: AuthorityMark,
    /// The society's in-group threshold reading.
    pub threshold: ThresholdPosture,
}

/// Why a society vector cannot receive a housemark.
/// type-audit: bare-ok(diagnostic-value: UnclassifiedRadius.0)
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum HousemarkError {
    /// The in-group radius belongs to no admitted threshold band.
    UnclassifiedRadius(f64),
}

impl fmt::Display for HousemarkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnclassifiedRadius(radius) => write!(
                f,
                "unclassified in-group radius {radius}; admitted bands are [0.0, 0.35], [0.5, 0.6], and [0.65, 1.0]"
            ),
        }
    }
}

impl std::error::Error for HousemarkError {}

impl Housemark {
    /// Derive this vessel-owned mark from a society vector.
    pub fn try_from_society(society: SocietyVector) -> Result<Self, HousemarkError> {
        let authority = match society.sociality {
            Sociality::Hierarchic => AuthorityMark::Command,
            Sociality::Communal => AuthorityMark::Common,
        };
        let threshold = match society.in_group_radius {
            x if (0.0..=0.35).contains(&x) => ThresholdPosture::Inward,
            x if (0.5..=0.6).contains(&x) => ThresholdPosture::Plain,
            x if (0.65..=1.0).contains(&x) => ThresholdPosture::Outward,
            x => return Err(HousemarkError::UnclassifiedRadius(x)),
        };
        Ok(Self {
            authority,
            threshold,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use hornvale_species::{KindId, Sociality, SocietyVector, StatusBasis, society_registry};

    use super::{AuthorityMark, Housemark, ThresholdPosture};

    fn society(sociality: Sociality, in_group_radius: f64) -> SocietyVector {
        SocietyVector {
            sociality,
            status_basis: StatusBasis::Rank,
            in_group_radius,
        }
    }

    #[test]
    fn classifies_authority_and_every_admitted_radius_boundary() {
        let cases = [
            (
                Sociality::Hierarchic,
                0.0,
                AuthorityMark::Command,
                ThresholdPosture::Inward,
            ),
            (
                Sociality::Communal,
                0.35,
                AuthorityMark::Common,
                ThresholdPosture::Inward,
            ),
            (
                Sociality::Hierarchic,
                0.5,
                AuthorityMark::Command,
                ThresholdPosture::Plain,
            ),
            (
                Sociality::Communal,
                0.6,
                AuthorityMark::Common,
                ThresholdPosture::Plain,
            ),
            (
                Sociality::Hierarchic,
                0.65,
                AuthorityMark::Command,
                ThresholdPosture::Outward,
            ),
            (
                Sociality::Communal,
                1.0,
                AuthorityMark::Common,
                ThresholdPosture::Outward,
            ),
        ];

        for (sociality, radius, authority, threshold) in cases {
            assert_eq!(
                Housemark::try_from_society(society(sociality, radius)),
                Ok(Housemark {
                    authority,
                    threshold
                }),
                "radius {radius} must stay in its admitted band"
            );
        }
    }

    #[test]
    fn refuses_open_gaps_and_out_of_range_radii_with_their_admitted_bands() {
        for radius in [-0.1, 0.4, 0.625, 1.1] {
            let error = Housemark::try_from_society(society(Sociality::Hierarchic, radius))
                .expect_err("a radius outside the three inclusive bands must refuse");
            let message = error.to_string();
            assert!(message.contains(&radius.to_string()), "{message}");
            assert!(message.contains("[0.0, 0.35]"), "{message}");
            assert!(message.contains("[0.5, 0.6]"), "{message}");
            assert!(message.contains("[0.65, 1.0]"), "{message}");
        }
    }

    #[test]
    fn live_society_registry_populates_the_six_housemark_classes() {
        let mut classes: BTreeMap<(AuthorityMark, ThresholdPosture), Vec<KindId>> = BTreeMap::new();
        for (kind, society) in society_registry().iter() {
            let mark = Housemark::try_from_society(*society)
                .unwrap_or_else(|error| panic!("{kind:?} has no housemark: {error}"));
            classes
                .entry((mark.authority, mark.threshold))
                .or_default()
                .push(*kind);
        }

        println!("H1 housemark roster: {classes:#?}");
        assert_eq!(classes.len(), 6, "H1 requires all six housemark classes");
        assert!(
            classes
                .values()
                .all(|witnesses| witnesses.len() < society_registry().len()),
            "no housemark class may absorb every society row"
        );
        assert!(
            classes
                .values()
                .filter(|witnesses| witnesses.len() > 1)
                .count()
                >= 3,
            "H1 requires at least three classes with multiple witnesses"
        );
    }
}
