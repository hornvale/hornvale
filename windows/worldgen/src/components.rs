//! The composition root's component set: the per-domain component registries
//! joined by `KindId`. A kind is the set of components carrying its key; "the
//! goblin" exists only as this assembled view. `assemble()` gathers each
//! domain's canonical registry; `from_stores` builds a validated set from
//! explicit per-domain stores (Lab's synthetic kinds, composed from the
//! canonical registries with per-kind overrides). Both enforce referential
//! integrity (the load-time invariant that replaces the old optional peopled
//! component: a psyche/perception key with no biosphere row is rejected).
//! The biosphere store is the set of kinds with bodies; the kind roster is
//! [`WorldComponents::kinds`], the union of every store's key-set.
#![warn(missing_docs)]

use hornvale_kernel::{ComponentStore, KindId};
use hornvale_species::{BiosphereTraits, MindVector, PerceptionVector, SocietyVector};

use crate::BuildError;

/// The joined component registries of a world's kinds. Speech components
/// (articulation, lexicon, family proto) are language-owned and already carry
/// the language types; body/mind and taxonomy are species-owned. `assemble`
/// gathers the canonical registries; `from_stores` composes a validated custom
/// set (Lab's synthetic solo kinds).
/// type-audit: bare-ok(identifier-text: family_of)
pub struct WorldComponents {
    /// Universal body component — the canonical entity set.
    pub biosphere: ComponentStore<KindId, BiosphereTraits>,
    /// Peopled psychology.
    pub psyche: ComponentStore<KindId, MindVector>,
    /// Community psychology — carried only by `Settled` kinds.
    pub society: ComponentStore<KindId, SocietyVector>,
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
    /// Deity-kind traits (religion-owned; no biosphere row).
    pub deity: ComponentStore<KindId, hornvale_religion::DeityTraits>,
    /// Culture-kind traits (culture-owned; no biosphere row).
    pub culture: ComponentStore<KindId, hornvale_culture::CultureTraits>,
    /// Material-kind traits (terrain-owned; no biosphere row).
    pub material: ComponentStore<KindId, hornvale_terrain::MaterialTraits>,
}

impl WorldComponents {
    /// Gather every domain's canonical registry and enforce referential
    /// integrity: the peopled cluster shares one key-set, and every peopled
    /// kind has a biosphere row. Fails loudly with the physical reason.
    pub fn assemble() -> Result<Self, BuildError> {
        let biosphere = hornvale_species::biosphere_registry();
        let psyche = hornvale_species::psyche_registry();
        let society = hornvale_species::society_registry();
        let perception = hornvale_species::perception_registry();
        let articulation = hornvale_language::articulation_registry();
        let lexicon = hornvale_language::lexicon_registry();
        let family_proto = hornvale_language::family_proto();
        let family_of = hornvale_species::family_of();
        let deity = hornvale_religion::deity_registry();
        let culture = hornvale_culture::culture_registry();
        let material = hornvale_terrain::material_registry();

        check_integrity(
            &biosphere,
            &psyche,
            &society,
            &perception,
            &articulation,
            &lexicon,
            &family_proto,
            &family_of,
        )?;

        Ok(Self {
            biosphere,
            psyche,
            society,
            perception,
            articulation,
            lexicon,
            family_proto,
            family_of,
            deity,
            culture,
            material,
        })
    }

    /// Assemble a component set from explicit per-domain stores, enforcing
    /// referential integrity (the load-time invariant). The validated entry
    /// for a caller composing a custom kind-set from the canonical registries
    /// — Lab's synthetic solo rosters (`serpent`/`goblin-twin`), which re-key
    /// goblin's canonical components under a fresh `KindId` and apply per-kind
    /// overrides (serpent's tonal articulation). `assemble()` stays the
    /// canonical-registry path. Fails loudly with the physical reason.
    /// type-audit: bare-ok(identifier-text: family_of)
    #[allow(clippy::too_many_arguments)]
    pub fn from_stores(
        biosphere: ComponentStore<KindId, BiosphereTraits>,
        psyche: ComponentStore<KindId, MindVector>,
        society: ComponentStore<KindId, SocietyVector>,
        perception: ComponentStore<KindId, PerceptionVector>,
        articulation: ComponentStore<KindId, hornvale_language::ArticulationVector>,
        lexicon: ComponentStore<KindId, hornvale_language::speech::Lexicon>,
        family_proto: ComponentStore<KindId, hornvale_language::ArticulationVector>,
        family_of: ComponentStore<KindId, &'static str>,
        deity: ComponentStore<KindId, hornvale_religion::DeityTraits>,
        culture: ComponentStore<KindId, hornvale_culture::CultureTraits>,
        material: ComponentStore<KindId, hornvale_terrain::MaterialTraits>,
    ) -> Result<Self, BuildError> {
        check_integrity(
            &biosphere,
            &psyche,
            &society,
            &perception,
            &articulation,
            &lexicon,
            &family_proto,
            &family_of,
        )?;

        Ok(Self {
            biosphere,
            psyche,
            society,
            perception,
            articulation,
            lexicon,
            family_proto,
            family_of,
            deity,
            culture,
            material,
        })
    }
}

/// A selector over the per-domain component registries, for reflection —
/// "which kinds carry this component" (UNI-21's capability query; the GOAP
/// available-action set).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ComponentTag {
    /// Universal body component.
    Biosphere,
    /// Peopled psychology.
    Psyche,
    /// Community psychology — `Settled` kinds only.
    Society,
    /// Peopled perception.
    Perception,
    /// Peopled phonology.
    Articulation,
    /// Peopled lexicon.
    Lexicon,
    /// Family proto vectors.
    FamilyProto,
    /// Taxonomy family label.
    FamilyOf,
    /// Deity-kind traits.
    Deity,
    /// Culture-kind traits.
    Culture,
    /// Material-kind traits.
    Material,
}

impl WorldComponents {
    /// The kinds carrying a given component, in ascending `KindId` order.
    pub fn kinds_with(&self, tag: ComponentTag) -> Vec<KindId> {
        match tag {
            ComponentTag::Biosphere => self.biosphere.ids().copied().collect(),
            ComponentTag::Psyche => self.psyche.ids().copied().collect(),
            ComponentTag::Society => self.society.ids().copied().collect(),
            ComponentTag::Perception => self.perception.ids().copied().collect(),
            ComponentTag::Articulation => self.articulation.ids().copied().collect(),
            ComponentTag::Lexicon => self.lexicon.ids().copied().collect(),
            ComponentTag::FamilyProto => self.family_proto.ids().copied().collect(),
            ComponentTag::FamilyOf => self.family_of.ids().copied().collect(),
            ComponentTag::Deity => self.deity.ids().copied().collect(),
            ComponentTag::Culture => self.culture.ids().copied().collect(),
            ComponentTag::Material => self.material.ids().copied().collect(),
        }
    }

    /// Every kind in the world: the union of all component stores' key-sets,
    /// ascending. The kind roster — NOT the biosphere store, which is only
    /// the set of kinds with bodies (genesis iterates biosphere for species
    /// entities; deity/culture/material kinds have no biosphere row).
    pub fn kinds(&self) -> Vec<KindId> {
        let mut all: std::collections::BTreeSet<KindId> = std::collections::BTreeSet::new();
        all.extend(self.biosphere.ids().copied());
        all.extend(self.psyche.ids().copied());
        all.extend(self.society.ids().copied());
        all.extend(self.perception.ids().copied());
        all.extend(self.articulation.ids().copied());
        all.extend(self.lexicon.ids().copied());
        all.extend(self.family_proto.ids().copied());
        all.extend(self.family_of.ids().copied());
        all.extend(self.deity.ids().copied());
        all.extend(self.culture.ids().copied());
        all.extend(self.material.ids().copied());
        all.into_iter().collect()
    }
}

/// Enforce the nested-capacity lattice (The Eremite): articulation and
/// lexicon share one key-set; perception and articulation are each subsets
/// of psyche (a creature may carry a mind without perception or speech — the
/// solitary minded); every minded kind has a biosphere row; and a `Settled`
/// kind carries the full peopled cluster (psyche, perception, articulation,
/// lexicon). Also enforce forward-proto coherence (the load-time invariant,
/// both constructors): every family label carried by ≥2 kinds in `family_of`
/// must have a `family_proto` entry, so a multi-member family always has a
/// proto ancestral vector to draw its shared proto phonology from (a
/// singleton family stays its own proto).
#[allow(clippy::too_many_arguments)]
fn check_integrity(
    biosphere: &ComponentStore<KindId, BiosphereTraits>,
    psyche: &ComponentStore<KindId, MindVector>,
    society: &ComponentStore<KindId, SocietyVector>,
    perception: &ComponentStore<KindId, PerceptionVector>,
    articulation: &ComponentStore<KindId, hornvale_language::ArticulationVector>,
    lexicon: &ComponentStore<KindId, hornvale_language::speech::Lexicon>,
    family_proto: &ComponentStore<KindId, hornvale_language::ArticulationVector>,
    family_of: &ComponentStore<KindId, &'static str>,
) -> Result<(), BuildError> {
    // Nested capacities (The Eremite): speech ⊆ mind, perception ⊆ mind, and a
    // Settled people carries the full peopled cluster. A creature may carry a
    // mind (psyche) without perception or speech — the solitary minded.
    if !articulation.ids().eq(lexicon.ids()) {
        return Err(BuildError::MalformedKind(
            "articulation and lexicon registries must share one key-set".into(),
        ));
    }
    for k in perception.ids() {
        if !psyche.contains(k) {
            return Err(BuildError::MalformedKind(format!(
                "kind {k:?} perceives but has no mind (psyche)"
            )));
        }
    }
    for k in articulation.ids() {
        if !psyche.contains(k) {
            return Err(BuildError::MalformedKind(format!(
                "kind {k:?} speaks but has no mind (psyche)"
            )));
        }
        if !family_of.contains(k) {
            return Err(BuildError::MalformedKind(format!(
                "speaking kind {k:?} has no family"
            )));
        }
    }
    for k in psyche.ids() {
        if !biosphere.contains(k) {
            return Err(BuildError::MalformedKind(format!(
                "minded kind {k:?} has no biosphere component"
            )));
        }
    }
    // A settling people is the full peopled cluster (what the old all-equal
    // invariant guaranteed for the peoples, now scoped to Settled).
    for (k, bio) in biosphere.iter() {
        if bio.social_form == hornvale_species::SocialForm::Settled
            && !(psyche.contains(k)
                && perception.contains(k)
                && articulation.contains(k)
                && lexicon.contains(k))
        {
            return Err(BuildError::MalformedKind(format!(
                "Settled kind {k:?} is missing a peopled component"
            )));
        }
    }
    // The Cloister: society ⟺ Settled. Every Settled kind carries a society
    // vector; no non-Settled kind does (a Solitary carries a mind but no society).
    let settled: std::collections::BTreeSet<KindId> = biosphere
        .iter()
        .filter(|(_, b)| b.social_form == hornvale_species::SocialForm::Settled)
        .map(|(k, _)| *k)
        .collect();
    if !society.ids().copied().eq(settled.iter().copied()) {
        return Err(BuildError::MalformedKind(
            "society vector key-set must equal the Settled key-set".into(),
        ));
    }
    // Forward-proto coherence: count each family label's membership, then
    // require a `family_proto` entry for every label carried by ≥2 kinds.
    let mut family_counts: std::collections::BTreeMap<&'static str, usize> =
        std::collections::BTreeMap::new();
    for (_, fam) in family_of.iter() {
        *family_counts.entry(*fam).or_insert(0) += 1;
    }
    for (fam, count) in &family_counts {
        if *count >= 2 && !family_proto.contains(&KindId(fam)) {
            return Err(BuildError::MalformedKind(format!(
                "family {fam:?} has {count} members but no family_proto entry"
            )));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_language::{articulation_registry, family_proto, lexicon_registry};
    use hornvale_species::{
        biosphere_registry, family_of, perception_registry, psyche_registry, society_registry,
    };

    #[test]
    fn canonical_registries_pass_integrity() {
        let biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let society = society_registry();
        let perception = perception_registry();
        let articulation = articulation_registry();
        let lexicon = lexicon_registry();
        let family_proto = family_proto();
        let family_of = family_of();
        assert!(
            check_integrity(
                &biosphere,
                &psyche,
                &society,
                &perception,
                &articulation,
                &lexicon,
                &family_proto,
                &family_of
            )
            .is_ok()
        );
    }

    #[test]
    fn key_set_mismatch_between_psyche_and_perception_fails_loudly() {
        let biosphere = biosphere_registry();
        let mut psyche = psyche_registry();
        let society = society_registry();
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
            &society,
            &perception,
            &articulation,
            &lexicon,
            &family_proto(),
            &family_of,
        );
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }

    #[test]
    fn key_set_mismatch_between_psyche_and_articulation_fails_loudly() {
        let biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let society = society_registry();
        let perception = perception_registry();
        let mut articulation = articulation_registry();
        let lexicon = lexicon_registry();
        let family_of = family_of();

        // Drop a speaker's articulation row so articulation and lexicon no
        // longer share one key-set (the `articulation.ids == lexicon.ids` check
        // must fire). Any articulation kind works — dropping it desyncs the two
        // stores; the first key may be a people or a dragon (both speak since
        // The Solitary Tongue).
        let drop = *articulation
            .ids()
            .next()
            .expect("at least one speaking kind");
        articulation = articulation
            .iter()
            .filter(|(k, _)| **k != drop)
            .map(|(k, v)| (*k, *v))
            .collect();

        let result = check_integrity(
            &biosphere,
            &psyche,
            &society,
            &perception,
            &articulation,
            &lexicon,
            &family_proto(),
            &family_of,
        );
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }

    #[test]
    fn key_set_mismatch_between_psyche_and_lexicon_fails_loudly() {
        let biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let society = society_registry();
        let perception = perception_registry();
        let articulation = articulation_registry();
        let full_lexicon = lexicon_registry();
        let family_of = family_of();

        let drop = *articulation
            .ids()
            .next()
            .expect("at least one speaking kind");
        let lexicon: ComponentStore<KindId, hornvale_language::speech::Lexicon> = full_lexicon
            .iter()
            .filter(|(k, _)| **k != drop)
            .map(|(k, v)| (*k, v.clone()))
            .collect();

        let result = check_integrity(
            &biosphere,
            &psyche,
            &society,
            &perception,
            &articulation,
            &lexicon,
            &family_proto(),
            &family_of,
        );
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }

    #[test]
    fn peopled_kind_missing_biosphere_row_fails_loudly() {
        let full_biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let society = society_registry();
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
            &society,
            &perception,
            &articulation,
            &lexicon,
            &family_proto(),
            &family_of,
        );
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }

    #[test]
    fn peopled_kind_missing_family_row_fails_loudly() {
        let biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let society = society_registry();
        let perception = perception_registry();
        let articulation = articulation_registry();
        let lexicon = lexicon_registry();
        let full_family_of = family_of();

        let missing = *articulation
            .ids()
            .next()
            .expect("at least one speaking kind");
        let family_of: ComponentStore<KindId, &'static str> = full_family_of
            .iter()
            .filter(|(k, _)| **k != missing)
            .map(|(k, v)| (*k, *v))
            .collect();

        let result = check_integrity(
            &biosphere,
            &psyche,
            &society,
            &perception,
            &articulation,
            &lexicon,
            &family_proto(),
            &family_of,
        );
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }

    #[test]
    fn multi_member_family_missing_its_proto_fails_loudly() {
        let biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let society = society_registry();
        let perception = perception_registry();
        let articulation = articulation_registry();
        let lexicon = lexicon_registry();
        let family_proto = family_proto();
        let canonical_family_of = family_of();

        // Reassign two peopled kinds to a fresh family label that has no
        // `family_proto` entry, so the roster carries a 2-member family with
        // no proto ancestral vector — the forward-proto invariant must fire.
        let mut ids = psyche.ids();
        let a = *ids.next().expect("at least one peopled kind");
        let b = *ids.next().expect("at least two peopled kinds");
        let family_of: ComponentStore<KindId, &'static str> = canonical_family_of
            .iter()
            .map(|(k, v)| {
                if *k == a || *k == b {
                    (*k, "orphan-family")
                } else {
                    (*k, *v)
                }
            })
            .collect();

        let result = check_integrity(
            &biosphere,
            &psyche,
            &society,
            &perception,
            &articulation,
            &lexicon,
            &family_proto,
            &family_of,
        );
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }

    #[test]
    fn psyche_without_speech_passes() {
        // The Eremite: a creature may carry a mind (psyche) without
        // perception or speech — the solitary minded. Not added to
        // perception/articulation/lexicon.
        let mut biosphere = biosphere_registry();
        let mut psyche = psyche_registry();
        let society = society_registry();
        let perception = perception_registry();
        let articulation = articulation_registry();
        let lexicon = lexicon_registry();
        let family_of = family_of();

        let new_kind = KindId("solitary-mind");
        let mind = *psyche.iter().next().expect("at least one peopled kind").1;
        psyche.insert(new_kind, mind);
        let mut body = biosphere
            .iter()
            .next()
            .expect("at least one biosphere row")
            .1
            .clone();
        body.social_form = hornvale_species::SocialForm::Solitary;
        biosphere.insert(new_kind, body);

        let result = check_integrity(
            &biosphere,
            &psyche,
            &society,
            &perception,
            &articulation,
            &lexicon,
            &family_proto(),
            &family_of,
        );
        assert!(result.is_ok(), "{result:?}");
    }

    #[test]
    fn speech_without_psyche_fails() {
        // A kind in articulation+lexicon but not psyche must fail: speech
        // requires a mind (articulation/lexicon ⊆ psyche).
        let biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let society = society_registry();
        let perception = perception_registry();
        let mut articulation = articulation_registry();
        let mut lexicon = lexicon_registry();
        let family_of = family_of();

        let new_kind = KindId("mute-no-more");
        let art = *articulation
            .iter()
            .next()
            .expect("at least one articulation row")
            .1;
        articulation.insert(new_kind, art);
        let lex = lexicon
            .iter()
            .next()
            .expect("at least one lexicon row")
            .1
            .clone();
        lexicon.insert(new_kind, lex);

        let result = check_integrity(
            &biosphere,
            &psyche,
            &society,
            &perception,
            &articulation,
            &lexicon,
            &family_proto(),
            &family_of,
        );
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }

    #[test]
    fn settled_missing_a_peopled_component_fails() {
        // A Settled kind must carry the full peopled cluster (psyche,
        // perception, articulation, lexicon) — the nested-lattice successor
        // to the old all-equal invariant, scoped to Settled.
        let mut biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let society = society_registry();
        let perception = perception_registry();
        let articulation = articulation_registry();
        let lexicon = lexicon_registry();
        let family_of = family_of();

        let new_kind = KindId("unheeded-settler");
        let mut body = biosphere
            .iter()
            .next()
            .expect("at least one biosphere row")
            .1
            .clone();
        body.social_form = hornvale_species::SocialForm::Settled;
        biosphere.insert(new_kind, body);

        let result = check_integrity(
            &biosphere,
            &psyche,
            &society,
            &perception,
            &articulation,
            &lexicon,
            &family_proto(),
            &family_of,
        );
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }

    #[test]
    fn integrity_rejects_a_non_settled_kind_carrying_society() {
        // The Cloister: society ⟺ Settled. A Solitary kind (a dragon) must
        // not carry a society vector; give red-dragon a bogus society row
        // through the public constructor and expect a loud rejection.
        let biosphere = biosphere_registry();
        let psyche = psyche_registry();
        let mut society = society_registry();
        let bogus = *society.iter().next().expect("at least one society row").1;
        society.insert(KindId("red-dragon"), bogus); // Solitary, not Settled
        let perception = perception_registry();
        let articulation = articulation_registry();
        let lexicon = lexicon_registry();
        let family_of = family_of();

        let result = WorldComponents::from_stores(
            biosphere,
            psyche,
            society,
            perception,
            articulation,
            lexicon,
            family_proto(),
            family_of,
            ComponentStore::new(),
            ComponentStore::new(),
            ComponentStore::new(),
        );
        assert!(matches!(result, Err(BuildError::MalformedKind(_))));
    }

    #[test]
    fn the_kind_roster_is_the_union_of_all_stores() {
        // Spec §4.4: "the biosphere store is the canonical entity set" is
        // retired — deity/culture/material kinds carry no biosphere row.
        let wc = WorldComponents::assemble().unwrap();
        let kinds = wc.kinds();
        for label in [
            "deity",
            "culture",
            "granite",
            "limestone",
            "owlbear",
            "goblin",
        ] {
            assert!(
                kinds.iter().any(|k| k.0 == label),
                "union roster must contain {label}"
            );
        }
        // Non-species kinds are NOT in the biosphere store (the genesis /
        // placement constraint): genesis must not mint them as species.
        for label in ["deity", "culture", "granite", "limestone"] {
            assert!(wc.biosphere.get_by_label(label).is_none());
        }
        // Ascending order (BTree-backed union).
        let mut sorted = kinds.clone();
        sorted.sort();
        assert_eq!(kinds, sorted);
    }

    #[test]
    fn kinds_with_covers_the_new_component_tags() {
        let wc = WorldComponents::assemble().unwrap();
        assert_eq!(wc.kinds_with(ComponentTag::Deity), vec![KindId("deity")]);
        assert_eq!(
            wc.kinds_with(ComponentTag::Culture),
            vec![KindId("culture")]
        );
        assert_eq!(
            wc.kinds_with(ComponentTag::Material),
            vec![KindId("granite"), KindId("limestone")]
        );
    }

    #[test]
    fn kinds_with_biosphere_is_the_full_roster_and_psyche_is_the_peopled_subset() {
        let wc = WorldComponents::assemble().unwrap();
        let bio = wc.kinds_with(ComponentTag::Biosphere);
        let psy = wc.kinds_with(ComponentTag::Psyche);
        assert!(!bio.is_empty());
        // every peopled (psyche) kind has a biosphere row (referential integrity)
        assert!(psy.iter().all(|k| bio.contains(k)));
        // fauna (menagerie) exist in biosphere but not psyche => strict subset
        assert!(psy.len() < bio.len());
        // ascending KindId order (BTreeMap-backed store)
        let mut sorted = bio.clone();
        sorted.sort();
        assert_eq!(bio, sorted);
    }
}
