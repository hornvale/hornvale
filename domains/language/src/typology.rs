//! Typology bundles: which **rules** build a family's words, as against which
//! **values** a shared rule uses.
//!
//! A bundle is a named authored row, never a point in a product space. Five
//! morphologies × three coda laws × three harmonies × three orthographies is
//! 135 combinations and four ship; admitting the cross-product would leave 131
//! unexercised paths that read to the next campaign as supported. Authoring
//! rows rather than admitting combinations is the same discipline
//! `family_proto()` uses and decision 0011 applies to studies.

use hornvale_kernel::{Component, ComponentStore, KindId};

/// How a family builds words from roots.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Morphology {
    /// A consonantal skeleton threaded with a vocalic template (Arabic,
    /// Hebrew). A paradigm slot selects the template.
    Templatic,
    /// One morpheme, one meaning, concatenated transparently (Turkish,
    /// Finnish). A derived concept is affixed, not compounded.
    Agglutinative,
    /// One morpheme per word; no inflection. Contrast is carried by tone and
    /// by word order.
    Isolating,
    /// Cluster-tolerant compounding — the engine shipped before The Burr.
    Concatenative,
}

/// What a syllable may begin with.
///
/// Load-bearing for the `sonorant-open` bundle specifically: the sonorant
/// FLOOR only guarantees a liquid is in the inventory, and Stage 2 measured
/// that this changes nothing on its own — proto-elf held `/r/` while its drawn
/// onset templates (`sibilant, stop, nasal`) had no slot able to host it, so
/// not one root carried a liquid and every daughter inherited that. See spec
/// §3.7.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OnsetLaw {
    /// Whatever the phonotactics draw produces — today's behaviour.
    Drawn,
    /// A two-slot onset whose second slot is a sonorant, by construction
    /// rather than by draw. This is what produces `Cr-`/`Cl-` onsets.
    SonorantSecond,
    /// A single-slot onset; no clusters.
    Single,
}

/// What a syllable may end in.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CodaLaw {
    /// Whatever the phonotactics draw produces — today's behaviour.
    Drawn,
    /// A coda is required, and drawn from obstruents only.
    ObstruentObligatory,
    /// A coda is optional and drawn from a small closed sonorant set.
    SonorantClosed,
    /// Open syllables, or a single nasal.
    OpenOrNasal,
}

/// Whether vowels within a word must agree, and on what.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Harmony {
    /// No word-level vowel constraint — today's behaviour.
    None,
    /// Every vowel in a word agrees on backness (Finnish, Turkish).
    Backness,
}

/// How this family's segments are spelled in the romanization. A view over
/// `Segment`, so this field alone moves no stream draw — but it does rewrite
/// every committed name string (spec §3.6).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Orthography {
    /// `ŋ` → `ng`, `ʃ` → `sh` — today's global map.
    Digraph,
    /// Diacritics in place of digraphs, so clusters stay legible.
    Diacritic,
    /// An apostrophe separates a digraph from a following consonant.
    Apostrophe,
}

/// A named typology bundle: which rules build this family's words.
/// type-audit: bare-ok(identifier-text: name)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Typology {
    /// The bundle's name, for artifacts and diagnostics.
    pub name: &'static str,
    /// How words are built from roots.
    pub morphology: Morphology,
    /// What a syllable may begin with.
    pub onset_law: OnsetLaw,
    /// What a syllable may end in.
    pub coda_law: CodaLaw,
    /// Whether vowels within a word must agree.
    pub harmony: Harmony,
    /// How this family's segments are spelled.
    pub orthography: Orthography,
}

impl Component for Typology {}

/// Every shipped bundle name. `every_bundle_is_used_by_some_family` asserts
/// each is reached by a family, so this list cannot grow without a home.
/// type-audit: bare-ok(identifier-text)
pub const ALL_BUNDLE_NAMES: [&str; 4] = [
    "templatic",
    "sonorant-open",
    "isolating-tonal",
    "concatenative",
];

/// The Khuzdul-region bundle: a consonantal skeleton threaded with vowels,
/// heavy closed syllables, no harmony.
pub fn templatic() -> Typology {
    Typology {
        name: "templatic",
        morphology: Morphology::Templatic,
        onset_law: OnsetLaw::Drawn,
        coda_law: CodaLaw::ObstruentObligatory,
        harmony: Harmony::None,
        orthography: Orthography::Apostrophe,
    }
}

/// The Quenya-region bundle: open-syllable preferring, affixing, with
/// word-level backness harmony — the one bundle that requires a sonorant.
pub fn sonorant_open() -> Typology {
    Typology {
        name: "sonorant-open",
        morphology: Morphology::Agglutinative,
        onset_law: OnsetLaw::SonorantSecond,
        coda_law: CodaLaw::SonorantClosed,
        harmony: Harmony::Backness,
        orthography: Orthography::Diacritic,
    }
}

/// Short roots, no affixes, contrast carried by pitch. The cheapest bundle to
/// build because it does *less* — and the one that finally reaches the tone
/// tier, which is authored at 0.0 on all 23 rows today.
pub fn isolating_tonal() -> Typology {
    Typology {
        name: "isolating-tonal",
        morphology: Morphology::Isolating,
        onset_law: OnsetLaw::Single,
        coda_law: CodaLaw::OpenOrNasal,
        harmony: Harmony::None,
        orthography: Orthography::Digraph,
    }
}

/// The status quo, named. This bundle is the campaign's own control: its
/// output must not move beyond what the epoch bump forces, which is what
/// separates "we built typology" from "we perturbed the seed".
pub fn concatenative() -> Typology {
    Typology {
        name: "concatenative",
        morphology: Morphology::Concatenative,
        onset_law: OnsetLaw::Drawn,
        coda_law: CodaLaw::Drawn,
        harmony: Harmony::None,
        orthography: Orthography::Digraph,
    }
}

/// Typology bundles keyed by **family label**, exactly as `family_proto()` is
/// keyed. A kind with no family falls back to [`concatenative`].
pub fn family_typology() -> ComponentStore<KindId, Typology> {
    [
        (KindId("dwarf"), templatic()),
        (KindId("elf"), sonorant_open()),
        (KindId("draconic"), isolating_tonal()),
        (KindId("goblinoid"), concatenative()),
        (KindId("plant"), concatenative()),
    ]
    .into_iter()
    .collect()
}

/// The bundle for a family label, defaulting to [`concatenative`] for a kind
/// with no family — the unfamilied kinds (human, gnoll) and any future
/// single-member family. Looks up by label content (`get_by_label`) rather
/// than constructing a `KindId`, because a runtime label borrowed from a
/// caller cannot satisfy `KindId`'s `&'static str` field.
/// type-audit: bare-ok(identifier-text: family)
pub fn typology_for(family: Option<&str>) -> Typology {
    match family {
        Some(f) => family_typology()
            .get_by_label(f)
            .copied()
            .unwrap_or_else(concatenative),
        None => concatenative(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every bundle must be reached by at least one family. An unexercised
    /// bundle is an unmeasured code path that reads as supported (a rule
    /// this campaign, The Burr, records as a decision at its close —
    /// `docs/decisions/` has no record yet as of this commit), and the
    /// reason this campaign ships four rather than six.
    #[test]
    fn every_bundle_is_used_by_some_family() {
        let authored = family_typology();
        let used: Vec<&str> = authored.iter().map(|(_, t)| t.name).collect();
        for name in ALL_BUNDLE_NAMES {
            assert!(
                used.contains(&name),
                "bundle '{name}' is authored but no family uses it — either \
                 give it a family or delete it (the anti-vacuity rule The \
                 Burr records at its close)"
            );
        }
    }

    /// The converse: no family names a bundle that does not exist.
    #[test]
    fn every_family_names_a_real_bundle() {
        for (kind, t) in family_typology().iter() {
            assert!(
                ALL_BUNDLE_NAMES.contains(&t.name),
                "family {kind:?} names unknown bundle '{}'",
                t.name
            );
        }
    }

    /// The control bundle must be phonotactically identical to today, or
    /// Stage 3's readout cannot separate "we built typology" from "we
    /// perturbed the seed" (spec §6 P4).
    #[test]
    fn the_concatenative_bundle_is_the_status_quo() {
        let c = concatenative();
        assert_eq!(c.morphology, Morphology::Concatenative);
        assert_eq!(c.onset_law, OnsetLaw::Drawn);
        assert_eq!(c.coda_law, CodaLaw::Drawn);
        assert_eq!(c.harmony, Harmony::None);
        assert_eq!(c.orthography, Orthography::Digraph);
    }
}
