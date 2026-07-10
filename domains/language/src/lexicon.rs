//! Lexicon assembly: two passes over a culture's concept exposures.
//! `Steeped` concepts each draw their own proto-root and evolve it (Task 6)
//! into a [`LexEntry::Root`]; `KnowsOf` concepts look up
//! [`crate::packs::compound_recipe`] and join their two components' modern
//! forms — which must already be `Steeped` roots — in drawn [`Headedness`]
//! order into a [`LexEntry::Compound`]; concepts with no recipe, or whose
//! components aren't held as roots, and every `Unknown` concept, fall back
//! to a [`LexEntry::Gap`] carrying the recountable reason.
#![warn(missing_docs)]

use crate::etymology::{Derivation, draw_cascade, evolve, proto_root};
use crate::packs::compound_recipe;
use crate::phoneme::Segment;
use crate::phonology::Phonology;
use hornvale_kernel::Seed;
use std::collections::BTreeMap;

/// How well a culture has come to know a concept — gates whether
/// [`build_lexicon`] gives it a root word, a compound, or a reasoned gap.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExposureClass {
    /// The concept is core to daily experience: it gets its own proto-root
    /// (a [`LexEntry::Root`]).
    Steeped,
    /// The concept is known but not core to daily experience: it is named,
    /// if at all, as a compound of two `Steeped` concepts (a
    /// [`LexEntry::Compound`]) via [`crate::packs::compound_recipe`].
    KnowsOf,
    /// The concept has not been encountered.
    Unknown {
        /// The recountable reason the culture hasn't encountered this
        /// concept.
        reason: GapReason,
    },
}

/// The recountable reason a concept has no word: composed upstream by
/// whichever domain determined the gap (e.g. "no settlement in or beside
/// coastal cells" for a landlocked culture's missing `sea`; "hue ladder
/// depth 3 from night-vision 0.8" for an unacquired color term), or, when
/// [`build_lexicon`] itself falls back a `KnowsOf` compound to a gap,
/// composed here from the missing recipe or component.
/// type-audit: bare-ok(prose)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GapReason {
    /// A gap rooted in lived experience — the culture never encountered the
    /// concept's referent (geography, biome, contact).
    Experiential(String),
    /// A gap rooted in perception — the concept's referent exists but the
    /// species' senses (or an acquisition ladder) haven't resolved it yet.
    Perceptual(String),
}

/// Which component comes first when [`build_lexicon`] joins a compound's two
/// root forms. Modifier-head order varies by language, so this is drawn per
/// species rather than fixed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Headedness {
    /// The head comes first, then the modifier (e.g. head "water" +
    /// modifier "many" joins as "water-many").
    HeadFirst,
    /// The modifier comes first, then the head (e.g. modifier "many" + head
    /// "water" joins as "many-water").
    HeadLast,
}

/// The two headedness orders [`draw_headedness`] draws from.
const HEADEDNESS_OPTIONS: [Headedness; 2] = [Headedness::HeadFirst, Headedness::HeadLast];

/// Draw a species' compound-joining order, from
/// `seed.derive("language").derive(species).derive("lexicon").derive("headedness")`.
/// type-audit: bare-ok(identifier-text)
pub fn draw_headedness(seed: &Seed, species: &str) -> Headedness {
    let mut stream = seed
        .derive("language")
        .derive(species)
        .derive("lexicon")
        .derive("headedness")
        .stream();
    *stream
        .pick(&HEADEDNESS_OPTIONS)
        .expect("HEADEDNESS_OPTIONS is a fixed non-empty array")
}

/// A word's three surface views — the same shape
/// [`crate::naming::GeneratedName`] renders for generated proper nouns,
/// reused here (via [`crate::naming::render_views`]) for lexicon entries so
/// no caller re-derives romanization/IPA/espeak logic: `roman` is the
/// almanac's ASCII-ish spelling, `ipa` is the book's phonetic rendering,
/// `espeak` is the espeak-ng formulation.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WordViews {
    /// The ASCII-ish romanization, capitalized on its first letter.
    pub roman: String,
    /// The IPA rendering.
    pub ipa: String,
    /// The espeak-ng formulation (`[[...]]`).
    pub espeak: String,
}

/// One concept's lexicon entry: a bare root, a recipe compound of two other
/// roots, or a reasoned gap.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LexEntry {
    /// A concept with its own proto-root, evolved to its modern form.
    Root {
        /// The full sound-change derivation: proto-root through the drawn
        /// cascade to the modern form.
        derivation: Derivation,
        /// The modern form's three surface views.
        views: WordViews,
    },
    /// A concept named as a compound of two rooted concepts, joined per the
    /// lexicon's drawn [`Headedness`].
    Compound {
        /// The modifier concept id (e.g. `"many"` in "many water").
        modifier: String,
        /// The head concept id (e.g. `"water"` in "many water").
        head: String,
        /// The joined compound's three surface views.
        views: WordViews,
    },
    /// A concept with no word: the recountable reason why.
    Gap {
        /// Why this concept has no word.
        reason: GapReason,
    },
}

/// A species' full generated vocabulary: every input concept's
/// [`LexEntry`], keyed by concept id, plus the drawn [`Headedness`]
/// compounds were assembled under. Built once by [`build_lexicon`]; a
/// [`BTreeMap`] keeps both lookup and iteration order deterministic.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Lexicon {
    /// The species this lexicon was built for.
    pub species: String,
    /// The drawn compound-joining order every [`LexEntry::Compound`] in this
    /// lexicon was assembled under.
    pub headedness: Headedness,
    entries: BTreeMap<String, LexEntry>,
}

impl Lexicon {
    /// The entry for `concept`, if this lexicon has one.
    /// type-audit: bare-ok(identifier-text)
    pub fn entry(&self, concept: &str) -> Option<&LexEntry> {
        self.entries.get(concept)
    }

    /// Every concept id and its entry, in concept-id order.
    pub fn entries(&self) -> impl Iterator<Item = (&str, &LexEntry)> {
        self.entries
            .iter()
            .map(|(concept, entry)| (concept.as_str(), entry))
    }
}

/// Look up `concept`'s [`LexEntry::Root`] modern segments in `entries`, or
/// `None` if `concept` has no entry yet or its entry isn't a root.
fn root_modern<'a>(
    entries: &'a BTreeMap<String, LexEntry>,
    concept: &str,
) -> Option<&'a [Segment]> {
    match entries.get(concept) {
        Some(LexEntry::Root { derivation, .. }) => Some(&derivation.modern),
        _ => None,
    }
}

/// Render a bare segment sequence's [`WordViews`], via
/// [`crate::naming::render_views`] — the shared segment→views reduction
/// naming.rs exposes.
fn word_views(segments: &[Segment]) -> WordViews {
    let rendered = crate::naming::render_views(segments);
    WordViews {
        roman: rendered.roman,
        ipa: rendered.ipa,
        espeak: rendered.espeak,
    }
}

/// Build `concept`'s [`LexEntry::Compound`] from its authored recipe, or a
/// [`LexEntry::Gap`] with a composed [`GapReason::Experiential`] reason when
/// the recipe is missing or either component isn't already held as a root
/// in `entries`.
fn compound_entry(
    concept: &str,
    entries: &BTreeMap<String, LexEntry>,
    headedness: Headedness,
) -> LexEntry {
    let Some((modifier, head)) = compound_recipe(concept) else {
        return LexEntry::Gap {
            reason: GapReason::Experiential(format!("no compound recipe for '{concept}'")),
        };
    };
    let (Some(mod_segs), Some(head_segs)) =
        (root_modern(entries, modifier), root_modern(entries, head))
    else {
        return LexEntry::Gap {
            reason: GapReason::Experiential(format!(
                "compound recipe for '{concept}' needs '{modifier}' and '{head}' held as roots"
            )),
        };
    };
    let joined: Vec<Segment> = match headedness {
        Headedness::HeadFirst => head_segs.iter().chain(mod_segs.iter()).copied().collect(),
        Headedness::HeadLast => mod_segs.iter().chain(head_segs.iter()).copied().collect(),
    };
    LexEntry::Compound {
        modifier: modifier.to_string(),
        head: head.to_string(),
        views: word_views(&joined),
    }
}

/// Build a species' full lexicon: draw a [`Headedness`] and one shared
/// sound-change cascade, then two passes over `exposures`. Pass one gives
/// every `Steeped` concept its own proto-root, evolved to a
/// [`LexEntry::Root`]. Pass two resolves every `KnowsOf` concept to a
/// [`LexEntry::Compound`] via [`crate::packs::compound_recipe`] over the two
/// components rooted in pass one (falling back to a [`LexEntry::Gap`] with a
/// composed [`GapReason::Experiential`] reason when the recipe is missing or
/// a component isn't held as a root), and passes every `Unknown` concept's
/// carried reason straight through to a [`LexEntry::Gap`]. Pure and total:
/// same inputs always produce an identical [`Lexicon`].
/// type-audit: bare-ok(identifier-text)
pub fn build_lexicon(
    seed: &Seed,
    species: &str,
    ph: &Phonology,
    exposures: &BTreeMap<String, ExposureClass>,
) -> Lexicon {
    let headedness = draw_headedness(seed, species);
    let cascade = draw_cascade(seed, species);

    let mut entries: BTreeMap<String, LexEntry> = BTreeMap::new();

    // Pass 1: roots for every Steeped concept — pass 2's compounds need
    // these already present in `entries`.
    for (concept, class) in exposures {
        if matches!(class, ExposureClass::Steeped) {
            let proto = proto_root(seed, species, concept, ph);
            let derivation = evolve(&proto, &cascade, ph);
            let views = word_views(&derivation.modern);
            entries.insert(concept.clone(), LexEntry::Root { derivation, views });
        }
    }

    // Pass 2: compounds for every KnowsOf concept; gaps for the rest.
    for (concept, class) in exposures {
        match class {
            ExposureClass::Steeped => {}
            ExposureClass::KnowsOf => {
                let entry = compound_entry(concept, &entries, headedness);
                entries.insert(concept.clone(), entry);
            }
            ExposureClass::Unknown { reason } => {
                entries.insert(
                    concept.clone(),
                    LexEntry::Gap {
                        reason: reason.clone(),
                    },
                );
            }
        }
    }

    Lexicon {
        species: species.to_string(),
        headedness,
        entries,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::phonology::{Envelope, ExoticSeg, draw_phonology};

    /// A permissive phonology, matching etymology.rs's own test fixture:
    /// full vowel space and every place/manner combination reachable, so
    /// proto-roots and cascades have plenty of segments to draw from.
    fn test_phonology() -> Phonology {
        draw_phonology(
            &Seed(37),
            "test",
            &Envelope {
                labiality: 1.0,
                vowel_space: 1.0,
                voicing: 1.0,
                sibilance: 1.0,
                voice_loudness: 1.0,
                exotic: ExoticSeg::None,
            },
        )
    }

    fn sea_exposures() -> BTreeMap<String, ExposureClass> {
        let mut exposures = BTreeMap::new();
        exposures.insert("water".to_string(), ExposureClass::Steeped);
        exposures.insert("many".to_string(), ExposureClass::Steeped);
        exposures.insert("sea".to_string(), ExposureClass::KnowsOf);
        exposures
    }

    #[test]
    fn every_input_concept_yields_exactly_one_entry() {
        let ph = test_phonology();
        let mut exposures = sea_exposures();
        exposures.insert(
            "blue".to_string(),
            ExposureClass::Unknown {
                reason: GapReason::Perceptual(
                    "hue ladder depth 3 from night-vision 0.8".to_string(),
                ),
            },
        );
        let lex = build_lexicon(&Seed(1), "test", &ph, &exposures);
        for concept in exposures.keys() {
            assert!(
                lex.entry(concept).is_some(),
                "{concept} should have exactly one entry"
            );
        }
        assert_eq!(
            lex.entries().count(),
            exposures.len(),
            "lexicon must not fabricate or drop entries"
        );
    }

    #[test]
    fn knows_of_concept_compounds_from_steeped_components_in_headedness_order() {
        let ph = test_phonology();
        let exposures = sea_exposures();
        let lex = build_lexicon(&Seed(1), "test", &ph, &exposures);

        let water_roman = match lex.entry("water").unwrap() {
            LexEntry::Root { views, .. } => views.roman.to_lowercase(),
            other => panic!("water should be a root, got {other:?}"),
        };
        let many_roman = match lex.entry("many").unwrap() {
            LexEntry::Root { views, .. } => views.roman.to_lowercase(),
            other => panic!("many should be a root, got {other:?}"),
        };

        let (modifier, head, sea_roman) = match lex.entry("sea").unwrap() {
            LexEntry::Compound {
                modifier,
                head,
                views,
            } => (modifier.clone(), head.clone(), views.roman.to_lowercase()),
            other => panic!("sea should be a compound, got {other:?}"),
        };
        assert_eq!(modifier, "many");
        assert_eq!(head, "water");

        match lex.headedness {
            Headedness::HeadFirst => {
                assert!(
                    sea_roman.starts_with(&water_roman) && sea_roman.ends_with(&many_roman),
                    "HeadFirst: {sea_roman:?} should be head ({water_roman:?}) then modifier ({many_roman:?})"
                );
            }
            Headedness::HeadLast => {
                assert!(
                    sea_roman.starts_with(&many_roman) && sea_roman.ends_with(&water_roman),
                    "HeadLast: {sea_roman:?} should be modifier ({many_roman:?}) then head ({water_roman:?})"
                );
            }
        }
    }

    #[test]
    fn knows_of_concept_with_an_unheld_component_falls_back_to_experiential_gap() {
        let ph = test_phonology();
        let mut exposures = BTreeMap::new();
        // "sea" needs both "many" and "water" held as Steeped roots; "many"
        // is omitted, so the compound can't be assembled.
        exposures.insert("water".to_string(), ExposureClass::Steeped);
        exposures.insert("sea".to_string(), ExposureClass::KnowsOf);
        let lex = build_lexicon(&Seed(1), "test", &ph, &exposures);
        match lex.entry("sea").unwrap() {
            LexEntry::Gap {
                reason: GapReason::Experiential(_),
            } => {}
            other => panic!("expected an Experiential Gap, got {other:?}"),
        }
    }

    #[test]
    fn build_lexicon_is_pure() {
        let ph = test_phonology();
        let exposures = sea_exposures();
        let a = build_lexicon(&Seed(5), "test", &ph, &exposures);
        let b = build_lexicon(&Seed(5), "test", &ph, &exposures);
        assert_eq!(a, b, "same inputs must yield an identical lexicon");
    }

    #[test]
    fn headedness_flips_compound_component_order() {
        let ph = test_phonology();
        let exposures = sea_exposures();

        let mut head_first_seed = None;
        let mut head_last_seed = None;
        for i in 0..200u64 {
            match draw_headedness(&Seed(i), "test") {
                Headedness::HeadFirst if head_first_seed.is_none() => head_first_seed = Some(i),
                Headedness::HeadLast if head_last_seed.is_none() => head_last_seed = Some(i),
                _ => {}
            }
            if head_first_seed.is_some() && head_last_seed.is_some() {
                break;
            }
        }
        let head_first_seed =
            head_first_seed.unwrap_or_else(|| panic!("HeadFirst should occur within 200 seeds"));
        let head_last_seed =
            head_last_seed.unwrap_or_else(|| panic!("HeadLast should occur within 200 seeds"));

        fn root_roman(lex: &Lexicon, concept: &str) -> String {
            match lex.entry(concept).unwrap() {
                LexEntry::Root { views, .. } => views.roman.to_lowercase(),
                other => panic!("{concept} should be a root, got {other:?}"),
            }
        }
        fn compound_roman(lex: &Lexicon, concept: &str) -> String {
            match lex.entry(concept).unwrap() {
                LexEntry::Compound { views, .. } => views.roman.to_lowercase(),
                other => panic!("{concept} should be a compound, got {other:?}"),
            }
        }

        let first_lex = build_lexicon(&Seed(head_first_seed), "test", &ph, &exposures);
        assert_eq!(first_lex.headedness, Headedness::HeadFirst);
        let water_first = root_roman(&first_lex, "water");
        let many_first = root_roman(&first_lex, "many");
        let sea_first = compound_roman(&first_lex, "sea");
        assert!(
            sea_first.starts_with(&water_first) && sea_first.ends_with(&many_first),
            "HeadFirst: {sea_first:?} should start with head {water_first:?} and end with modifier {many_first:?}"
        );

        let last_lex = build_lexicon(&Seed(head_last_seed), "test", &ph, &exposures);
        assert_eq!(last_lex.headedness, Headedness::HeadLast);
        let water_last = root_roman(&last_lex, "water");
        let many_last = root_roman(&last_lex, "many");
        let sea_last = compound_roman(&last_lex, "sea");
        assert!(
            sea_last.starts_with(&many_last) && sea_last.ends_with(&water_last),
            "HeadLast: {sea_last:?} should start with modifier {many_last:?} and end with head {water_last:?}"
        );
    }
}
