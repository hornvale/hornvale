//! The composition root's component set: the per-domain component registries
//! joined by `KindId`. A kind is the set of components carrying its key; "the
//! goblin" exists only as this assembled view. `assemble()` gathers each
//! domain's canonical registry; `from_roster` derives the same set from an
//! arbitrary roster (Lab's synthetic kinds). Both enforce referential
//! integrity (the load-time invariant that replaces `Option<PeopledTraits>`).
#![warn(missing_docs)]

use hornvale_kernel::{ComponentStore, KindId};
use hornvale_species::{BiosphereTraits, PeopledTraits, PerceptionVector, PsychVector, SpeciesDef};

use crate::BuildError;

/// The joined component registries of a world's kinds. Speech components
/// (articulation, lexicon, family proto) are language-owned; the composition
/// root translates a kind's authored `SpeciesDef` speech into the language
/// types field-by-field (`from_roster`).
/// type-audit: bare-ok(identifier-text: family_of)
pub struct WorldComponents {
    /// Universal body component — the canonical entity set.
    pub biosphere: ComponentStore<KindId, BiosphereTraits>,
    /// Peopled psychology.
    pub psyche: ComponentStore<KindId, PsychVector>,
    /// Peopled perception.
    pub perception: ComponentStore<KindId, PerceptionVector>,
    /// Peopled phonology (language-owned).
    pub articulation: ComponentStore<KindId, hornvale_language::ArticulationVector>,
    /// Peopled lexicon (language-owned).
    pub lexicon: ComponentStore<KindId, hornvale_language::speech::Lexicon>,
    /// Family proto vectors, keyed by family label (language-owned; canonical).
    pub family_proto: ComponentStore<KindId, hornvale_language::ArticulationVector>,
    /// Universal taxonomy: a kind's family label.
    pub family_of: ComponentStore<KindId, &'static str>,
}

impl WorldComponents {
    /// Gather every domain's canonical registry and enforce referential
    /// integrity: the peopled cluster shares one key-set, and every peopled
    /// kind has a biosphere row. Fails loudly with the physical reason.
    pub fn assemble() -> Result<Self, BuildError> {
        let biosphere = hornvale_species::biosphere_registry();
        let psyche = hornvale_species::psyche_registry();
        let perception = hornvale_species::perception_registry();
        let articulation = hornvale_language::articulation_registry();
        let lexicon = hornvale_language::lexicon_registry();
        let family_proto = hornvale_language::family_proto();
        let family_of = hornvale_species::family_of();

        check_integrity(
            &biosphere,
            &psyche,
            &perception,
            &articulation,
            &lexicon,
            &family_of,
        )?;

        Ok(Self {
            biosphere,
            psyche,
            perception,
            articulation,
            lexicon,
            family_proto,
            family_of,
        })
    }

    /// Build the component set from an arbitrary roster (NOT the canonical
    /// registries) — this is how a custom roster (e.g. Lab's synthetic kinds
    /// like `serpent`/`goblin-twin`, which are not in `species::registry()`)
    /// enters the build. Byte-identical to reading the roster's own
    /// `SpeciesDef` fields directly: the speech stores translate the def's
    /// species-typed articulation/lexicon into the language types field-by-
    /// field, so a synthetic roster's custom values (serpent's `tonality =
    /// 1.0`) are preserved rather than looked up canonically. `assemble()`
    /// stays the canonical-registry path.
    pub fn from_roster(roster: &[SpeciesDef]) -> Result<Self, BuildError> {
        // biosphere / family_of: one per kind (all kinds), keyed by KindId(def.name).
        let biosphere: ComponentStore<KindId, BiosphereTraits> = roster
            .iter()
            .map(|d| (KindId(d.name), d.biosphere.clone()))
            .collect();
        let family_of: ComponentStore<KindId, &'static str> =
            roster.iter().map(|d| (KindId(d.name), d.family)).collect();

        // psyche / perception / articulation / lexicon: one per PEOPLED kind.
        let psyche: ComponentStore<KindId, PsychVector> = roster
            .iter()
            .filter_map(|d| d.peopled.as_ref().map(|p| (KindId(d.name), p.psych)))
            .collect();
        let perception: ComponentStore<KindId, PerceptionVector> = roster
            .iter()
            .filter_map(|d| d.peopled.as_ref().map(|p| (KindId(d.name), p.perception)))
            .collect();
        let articulation: ComponentStore<KindId, hornvale_language::ArticulationVector> = roster
            .iter()
            .filter_map(|d| {
                d.peopled
                    .as_ref()
                    .map(|p| (KindId(d.name), to_language_articulation(&p.articulation)))
            })
            .collect();
        let lexicon: ComponentStore<KindId, hornvale_language::speech::Lexicon> = roster
            .iter()
            .filter_map(|d| {
                d.peopled
                    .as_ref()
                    .map(|p| (KindId(d.name), to_language_lexicon(p)))
            })
            .collect();

        // family_proto: the canonical 3 families. A synthetic roster's
        // singleton families simply have no proto row, which downstream
        // name-gen already handles as None — same as today.
        let family_proto = hornvale_language::family_proto();

        check_integrity(
            &biosphere,
            &psyche,
            &perception,
            &articulation,
            &lexicon,
            &family_of,
        )?;

        Ok(Self {
            biosphere,
            psyche,
            perception,
            articulation,
            lexicon,
            family_proto,
            family_of,
        })
    }
}

/// Translate a species-typed articulation vector into language's own copy,
/// field-by-field — the single translation site `from_roster` uses. Every
/// scalar is a direct 1:1 carry (both share the same 0–1 scale); the exotic
/// manner maps one variant at a time (same variant order).
fn to_language_articulation(
    a: &hornvale_species::ArticulationVector,
) -> hornvale_language::ArticulationVector {
    hornvale_language::ArticulationVector {
        labiality: a.labiality,
        vowel_space: a.vowel_space,
        voicing: a.voicing,
        sibilance: a.sibilance,
        voice_loudness: a.voice_loudness,
        tonality: a.tonality,
        exotic: match a.exotic {
            hornvale_species::ExoticManner::None => hornvale_language::ExoticManner::None,
            hornvale_species::ExoticManner::Trill => hornvale_language::ExoticManner::Trill,
            hornvale_species::ExoticManner::Click => hornvale_language::ExoticManner::Click,
            hornvale_species::ExoticManner::Ejective => hornvale_language::ExoticManner::Ejective,
        },
    }
}

/// Translate a peopled kind's stopgap social vocabulary into language's own
/// `speech::Lexicon` — the noun + rung words carried verbatim.
fn to_language_lexicon(p: &PeopledTraits) -> hornvale_language::speech::Lexicon {
    hornvale_language::speech::Lexicon {
        noun: p.noun,
        worker_override: p.worker_override,
        warrior: p.warrior,
        artisan: p.artisan,
        shaman: p.shaman,
        top: p.top,
    }
}

/// Enforce peopled-cluster coherence: the four peopled stores (psyche,
/// perception, articulation, lexicon) share one key-set — the peoples — and
/// every peopled kind has a biosphere row and a family row.
fn check_integrity(
    biosphere: &ComponentStore<KindId, BiosphereTraits>,
    psyche: &ComponentStore<KindId, PsychVector>,
    perception: &ComponentStore<KindId, PerceptionVector>,
    articulation: &ComponentStore<KindId, hornvale_language::ArticulationVector>,
    lexicon: &ComponentStore<KindId, hornvale_language::speech::Lexicon>,
    family_of: &ComponentStore<KindId, &'static str>,
) -> Result<(), BuildError> {
    if !psyche.ids().eq(perception.ids()) {
        return Err(BuildError::MalformedKind(
            "psyche and perception registries must share one key-set".into(),
        ));
    }
    if !psyche.ids().eq(articulation.ids()) {
        return Err(BuildError::MalformedKind(
            "psyche and articulation registries must share one key-set".into(),
        ));
    }
    if !psyche.ids().eq(lexicon.ids()) {
        return Err(BuildError::MalformedKind(
            "psyche and lexicon registries must share one key-set".into(),
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
    use hornvale_language::{articulation_registry, lexicon_registry};
    use hornvale_species::{biosphere_registry, family_of, perception_registry, psyche_registry};

    #[test]
    fn canonical_registries_pass_integrity() {
        let biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let perception = perception_registry();
        let articulation = articulation_registry();
        let lexicon = lexicon_registry();
        let family_of = family_of();
        assert!(
            check_integrity(
                &biosphere,
                &psyche,
                &perception,
                &articulation,
                &lexicon,
                &family_of
            )
            .is_ok()
        );
    }

    #[test]
    fn from_roster_matches_assemble_on_the_default_roster() {
        // The default roster's translated speech equals the canonical
        // language registries exactly — byte-identity's load-bearing claim.
        let roster = crate::default_roster();
        let wc = WorldComponents::from_roster(&roster).expect("well-formed default roster");
        let canonical = WorldComponents::assemble().expect("well-formed canonical registries");
        assert!(wc.psyche.ids().eq(canonical.psyche.ids()));
        assert!(wc.articulation.ids().eq(canonical.articulation.ids()));
        assert!(wc.lexicon.ids().eq(canonical.lexicon.ids()));
        for k in canonical.articulation.ids() {
            assert_eq!(wc.articulation.get(k), canonical.articulation.get(k));
            assert_eq!(wc.lexicon.get(k), canonical.lexicon.get(k));
        }
    }

    #[test]
    fn key_set_mismatch_between_psyche_and_perception_fails_loudly() {
        let biosphere = biosphere_registry();
        let mut psyche = psyche_registry();
        let perception = perception_registry();
        let articulation = articulation_registry();
        let lexicon = lexicon_registry();
        let family_of = family_of();

        // Clone an existing value under a key that exists nowhere else, so
        // psyche's key-set no longer equals perception's.
        let extra = *psyche.iter().next().expect("at least one peopled kind").1;
        psyche.insert(KindId("nonexistent"), extra);

        let result = check_integrity(
            &biosphere,
            &psyche,
            &perception,
            &articulation,
            &lexicon,
            &family_of,
        );
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }

    #[test]
    fn key_set_mismatch_between_psyche_and_articulation_fails_loudly() {
        let biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let perception = perception_registry();
        let mut articulation = articulation_registry();
        let lexicon = lexicon_registry();
        let family_of = family_of();

        // Drop a peopled kind's articulation row so the peopled cluster no
        // longer shares one key-set (psyche/perception still agree, so the
        // articulation check is the one that must fire).
        let drop = *psyche.ids().next().expect("at least one peopled kind");
        articulation = articulation
            .iter()
            .filter(|(k, _)| **k != drop)
            .map(|(k, v)| (*k, *v))
            .collect();

        let result = check_integrity(
            &biosphere,
            &psyche,
            &perception,
            &articulation,
            &lexicon,
            &family_of,
        );
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }

    #[test]
    fn key_set_mismatch_between_psyche_and_lexicon_fails_loudly() {
        let biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let perception = perception_registry();
        let articulation = articulation_registry();
        let full_lexicon = lexicon_registry();
        let family_of = family_of();

        let drop = *psyche.ids().next().expect("at least one peopled kind");
        let lexicon: ComponentStore<KindId, hornvale_language::speech::Lexicon> = full_lexicon
            .iter()
            .filter(|(k, _)| **k != drop)
            .map(|(k, v)| (*k, v.clone()))
            .collect();

        let result = check_integrity(
            &biosphere,
            &psyche,
            &perception,
            &articulation,
            &lexicon,
            &family_of,
        );
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }

    #[test]
    fn peopled_kind_missing_biosphere_row_fails_loudly() {
        let full_biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let perception = perception_registry();
        let articulation = articulation_registry();
        let lexicon = lexicon_registry();
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

        let result = check_integrity(
            &biosphere,
            &psyche,
            &perception,
            &articulation,
            &lexicon,
            &family_of,
        );
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }

    #[test]
    fn peopled_kind_missing_family_row_fails_loudly() {
        let biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let perception = perception_registry();
        let articulation = articulation_registry();
        let lexicon = lexicon_registry();
        let full_family_of = family_of();

        let missing = *psyche.ids().next().expect("at least one peopled kind");
        let family_of: ComponentStore<KindId, &'static str> = full_family_of
            .iter()
            .filter(|(k, _)| **k != missing)
            .map(|(k, v)| (*k, *v))
            .collect();

        let result = check_integrity(
            &biosphere,
            &psyche,
            &perception,
            &articulation,
            &lexicon,
            &family_of,
        );
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }
}
