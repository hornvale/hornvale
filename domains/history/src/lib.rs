//! History: the occupation data model for the living-community engine
//! (living-community campaign 1, "The Community & its History"). A world's
//! present is the last frame of a committed deep-history skeleton — a
//! living settlement is a community still alive at t=now, a ruin is one
//! that died. This crate owns the skeleton's shape (`OccupationRecord` and
//! its enums), the ledger predicates that commit it, and — from Task 2
//! onward — the pure local flesh derivations (persona/residue/structures)
//! that read a record back into texture. The bake that actually walks the
//! millennia runs at the composition root (`windows/worldgen`), since a
//! domain may depend on nothing but the kernel (constitutional) and the
//! bake must read terrain, paleoclimate, and demography together.
#![warn(missing_docs)]

pub mod flesh;
pub mod record;
pub mod streams;

use hornvale_kernel::{ConceptRegistry, RegistryError};

/// Predicate: the world-level "now" — the standard day the deep-history
/// bake's present sits at (`windows/worldgen::history_bake::BakeConfig`'s
/// `end_year`), committed once per world (on the world entity) by the
/// composition root right after the bake commits. `present_day` (in
/// `windows/almanac`) reads this back directly instead of approximating the
/// present as the latest committed occupation event — the approximation
/// undercounted every ruin's age and tenure by the bake's post-history
/// stretch (T7 review gap; the bake's last occupation event predates
/// `end_year` because history is a stochastic walk, not a schedule that
/// lands exactly on the boundary).
/// type-audit: bare-ok(identifier-text)
pub const HISTORY_NOW: &str = "history-now";
/// Predicate marking an entity as one occupation record (one span of a
/// people occupying a site).
/// type-audit: bare-ok(identifier-text)
pub const IS_OCCUPATION: &str = "is-occupation";
/// Predicate: the people (`KindId`) occupying the site (functional, Text).
/// type-audit: bare-ok(identifier-text)
pub const OCC_PEOPLE: &str = "occ-people";
/// Predicate: the Geosphere cell the occupation sits on (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const OCC_SITE: &str = "occ-site";
/// Predicate: the standard day the occupation began (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const OCC_FOUNDED: &str = "occ-founded";
/// Predicate: the standard day the occupation ended, absent while alive
/// (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const OCC_ENDED: &str = "occ-ended";
/// Predicate: the occupation's peak population (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const OCC_PEAK: &str = "occ-peak";
/// Predicate: the occupation's technological horizon (functional, Text).
/// type-audit: bare-ok(identifier-text)
pub const OCC_TECH: &str = "occ-tech";
/// Predicate: what the occupation was for (functional, Text).
/// type-audit: bare-ok(identifier-text)
pub const OCC_FUNCTION: &str = "occ-function";
/// Predicate: why the occupation ended, absent while alive (functional,
/// Text).
/// type-audit: bare-ok(identifier-text)
pub const OCC_CAUSE: &str = "occ-cause";
/// Predicate: how the occupation ended — nature, or at another entity's
/// hand (functional, Text/Number pair per `Ended`).
/// type-audit: bare-ok(identifier-text)
pub const OCC_ENDED_BY: &str = "occ-ended-by";
/// Predicate: how the occupation began — genesis at a site, or founded from
/// another community (functional, Text/Number pair per `Founding`).
/// type-audit: bare-ok(identifier-text)
pub const OCC_FOUNDED_FROM: &str = "occ-founded-from";
/// Predicate: how notable the occupation was (functional, Text).
/// type-audit: bare-ok(identifier-text)
pub const OCC_NOTABILITY: &str = "occ-notability";
/// Predicate marking an occupation's site as a ruin (a dead occupation's
/// present-frame residue).
/// type-audit: bare-ok(identifier-text)
pub const IS_RUIN: &str = "is-ruin";

/// Register history's contribution to the concept registry: the world-level
/// `history-now` scalar, one predicate per `OccupationRecord` field, plus the
/// `is-occupation`/`is-ruin` marker predicates.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(
        HISTORY_NOW,
        true,
        "the standard day the world's present sits at (the bake's end year)",
    )?;
    registry.register_predicate(IS_OCCUPATION, true, "subject is an occupation record")?;
    registry.register_predicate(OCC_PEOPLE, true, "the people occupying the site")?;
    registry.register_predicate(OCC_SITE, true, "the Geosphere cell the occupation sits on")?;
    registry.register_predicate(OCC_FOUNDED, true, "the standard day the occupation began")?;
    registry.register_predicate(
        OCC_ENDED,
        true,
        "the standard day the occupation ended, absent while alive",
    )?;
    registry.register_predicate(OCC_PEAK, true, "the occupation's peak population")?;
    registry.register_predicate(OCC_TECH, true, "the occupation's technological horizon")?;
    registry.register_predicate(OCC_FUNCTION, true, "what the occupation was for")?;
    registry.register_predicate(
        OCC_CAUSE,
        true,
        "why the occupation ended, absent while alive",
    )?;
    registry.register_predicate(
        OCC_ENDED_BY,
        true,
        "how the occupation ended: nature, or another entity's hand",
    )?;
    registry.register_predicate(
        OCC_FOUNDED_FROM,
        true,
        "how the occupation began: genesis, or founded from another community",
    )?;
    registry.register_predicate(OCC_NOTABILITY, true, "how notable the occupation was")?;
    registry.register_predicate(IS_RUIN, true, "subject is a ruin (a dead occupation)")?;
    Ok(())
}

/// Every seed-derivation label this crate documents. The root is reserved
/// for the deep-history bake (Task 3), which runs at the composition root
/// (`windows/worldgen`) but documents its draws under this same
/// `history/*` namespace; the local flesh derivations (Task 2) draw against
/// the sub-labels below, relative to whatever occupation-scoped seed the
/// caller hands them.
/// type-audit: bare-ok(identifier-text)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (
            streams::ROOT.as_str(),
            "root stream for history: reserved for the deep-history bake \
             (Task 3, run at the composition root). No draw is made \
             against it directly.",
        ),
        (
            streams::RESIDUE.as_str(),
            "flesh::residue_of's deterministic flavor draws (Task 2).",
        ),
        (
            streams::STRUCTURES.as_str(),
            "flesh::structures_of's dwelling-count variance draws (Task 2).",
        ),
        (
            streams::BAKE.as_str(),
            "the deep-history bake's epoch dynamics: grow/found/migrate/raid/collapse \
             draws, taken sequentially from one stream in commit order (Task 3, run at \
             the composition root).",
        ),
        (
            streams::GENESIS.as_str(),
            "the deep-history bake's genesis draws: proto-community count, site picks, \
             and tech-advance offset (Task 3). Further derives a per-people sub-stream \
             `history/genesis/<people-kind>` via `StreamLabel::dynamic(people.0)`.",
        ),
        (
            streams::FLESH.as_str(),
            "the per-occupation flesh seed the legibility surface derives before \
             expanding residue/structures on demand (never committed).",
        ),
    ]
}

/// History as a registrable unit for the composition-root roster (mirrors
/// `hornvale_settlement::Settlement`).
pub struct History;

impl hornvale_kernel::Domain for History {
    fn crate_name(&self) -> &'static str {
        env!("CARGO_PKG_NAME")
    }
    fn register_concepts(
        &self,
        registry: &mut hornvale_kernel::ConceptRegistry,
    ) -> Result<(), hornvale_kernel::RegistryError> {
        crate::register_concepts(registry)
    }
    fn stream_labels(&self) -> Vec<(&'static str, &'static str)> {
        crate::stream_labels()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn concepts_register_without_conflict() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
    }

    #[test]
    fn stream_labels_declare_the_root() {
        let labels: Vec<&str> = stream_labels().iter().map(|(l, _)| *l).collect();
        assert!(labels.contains(&"history"));
    }
}
