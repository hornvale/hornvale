//! The composition root's component set: the per-domain component registries
//! joined by `KindId`. A kind is the set of components carrying its key; "the
//! goblin" exists only as this assembled view. `assemble()` gathers each
//! domain's registry and enforces referential integrity (the load-time
//! invariant that replaces `Option<PeopledTraits>`).
#![warn(missing_docs)]

use hornvale_kernel::{ComponentStore, KindId};
use hornvale_species::{BiosphereTraits, PerceptionVector, PsychVector};

use crate::BuildError;

/// The joined component registries of a world's kinds. (Language-owned speech
/// components are added in the speech-relocation task.)
/// type-audit: bare-ok(identifier-text: family_of)
pub struct WorldComponents {
    /// Universal body component — the canonical entity set.
    pub biosphere: ComponentStore<KindId, BiosphereTraits>,
    /// Peopled psychology.
    pub psyche: ComponentStore<KindId, PsychVector>,
    /// Peopled perception.
    pub perception: ComponentStore<KindId, PerceptionVector>,
    /// Universal taxonomy: a kind's family label.
    pub family_of: ComponentStore<KindId, &'static str>,
}

impl WorldComponents {
    /// Gather every domain's registry and enforce referential integrity: the
    /// peopled cluster shares one key-set, and every peopled kind has a
    /// biosphere row. Fails loudly with the physical reason.
    pub fn assemble() -> Result<Self, BuildError> {
        let biosphere = hornvale_species::biosphere_registry();
        let psyche = hornvale_species::psyche_registry();
        let perception = hornvale_species::perception_registry();
        let family_of = hornvale_species::family_of();

        check_integrity(&biosphere, &psyche, &perception, &family_of)?;

        Ok(Self {
            biosphere,
            psyche,
            perception,
            family_of,
        })
    }
}

/// Enforce peopled-cluster coherence: psyche and perception share one
/// key-set, and every peopled kind has a biosphere row and a family row.
fn check_integrity(
    biosphere: &ComponentStore<KindId, BiosphereTraits>,
    psyche: &ComponentStore<KindId, PsychVector>,
    perception: &ComponentStore<KindId, PerceptionVector>,
    family_of: &ComponentStore<KindId, &'static str>,
) -> Result<(), BuildError> {
    if !psyche.ids().eq(perception.ids()) {
        return Err(BuildError::MalformedKind(
            "psyche and perception registries must share one key-set".into(),
        ));
    }
    for k in psyche.ids() {
        if !biosphere.contains(k) {
            return Err(BuildError::MalformedKind(format!(
                "peopled kind {k:?} has no biosphere component"
            )));
        }
        if !family_of.contains(k) {
            return Err(BuildError::MalformedKind(format!(
                "kind {k:?} has no family"
            )));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_species::{biosphere_registry, family_of, perception_registry, psyche_registry};

    #[test]
    fn canonical_registries_pass_integrity() {
        let biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let perception = perception_registry();
        let family_of = family_of();
        assert!(check_integrity(&biosphere, &psyche, &perception, &family_of).is_ok());
    }

    #[test]
    fn key_set_mismatch_between_psyche_and_perception_fails_loudly() {
        let biosphere = biosphere_registry();
        let mut psyche = psyche_registry();
        let perception = perception_registry();
        let family_of = family_of();

        // Clone an existing value under a key that exists nowhere else, so
        // psyche's key-set no longer equals perception's.
        let extra = *psyche.iter().next().expect("at least one peopled kind").1;
        psyche.insert(KindId("nonexistent"), extra);

        let result = check_integrity(&biosphere, &psyche, &perception, &family_of);
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }

    #[test]
    fn peopled_kind_missing_biosphere_row_fails_loudly() {
        let full_biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let perception = perception_registry();
        let family_of = family_of();

        // Drop the biosphere row for a peopled kind (any key present in
        // psyche's key-set) so the referential-integrity check has a
        // dangling reference to catch.
        let missing = *psyche.ids().next().expect("at least one peopled kind");
        let biosphere: ComponentStore<KindId, BiosphereTraits> = full_biosphere
            .iter()
            .filter(|(k, _)| **k != missing)
            .map(|(k, v)| (*k, v.clone()))
            .collect();

        let result = check_integrity(&biosphere, &psyche, &perception, &family_of);
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }

    #[test]
    fn peopled_kind_missing_family_row_fails_loudly() {
        let biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let perception = perception_registry();
        let full_family_of = family_of();

        let missing = *psyche.ids().next().expect("at least one peopled kind");
        let family_of: ComponentStore<KindId, &'static str> = full_family_of
            .iter()
            .filter(|(k, _)| **k != missing)
            .map(|(k, v)| (*k, *v))
            .collect();

        let result = check_integrity(&biosphere, &psyche, &perception, &family_of);
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }
}
