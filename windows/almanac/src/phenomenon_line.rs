//! One phenomenon, as a reader sees it: rendered from its referent at the
//! moment of reading, never stored. A producer cannot know who is looking
//! (`ObserverContext` carries no species), so a stored string could only ever
//! be neutral or wrong — which is why a phenomenon's reader-facing text is
//! built here, from registered concept ids, rather than shipped alongside it.
//!
//! **A phenomenon has no subject.** It is a thing noticed, not a proposition
//! about a thing, so neither clause realizer fits: `realize_common` and
//! `realize_tongue_deep` both realize `X is a Y` and both demand an `X` this
//! channel does not carry. Inventing one would be the same authored English
//! this campaign is removing. What a phenomenon has is a *referent* — a head
//! concept and its qualifiers — so what gets realized here is a **noun
//! phrase**, in whichever register the reader is owed.
//!
//! The two registers are structurally different, and deliberately so:
//!
//! - **Common** (no speaker) resolves through [`CommonVocabulary::word_for`],
//!   which is total, and wears Common's definite article: `the great moon`.
//! - **A tongue** (a speaker) resolves through that people's [`Lexicon`],
//!   which is partial, and wears **no** article — a tongue's determiner
//!   system is not something Hornvale has drawn, and bolting `the` onto a
//!   goblin noun would import English grammar into a language that never
//!   asked for it. Modifier/head order is not invented either: it is the
//!   lexicon's own drawn [`Headedness`], the same parameter that already
//!   orders every compound this people builds.

use hornvale_kernel::Phenomenon;
use hornvale_language::{CommonVocabulary, Headedness, LexEntry, Lexicon};

use crate::Speaker;

/// One concept in Common: infallible, because Common is the author's register
/// and [`CommonVocabulary::word_for`] is total.
fn common_word(vocab: &CommonVocabulary, concept: &str) -> String {
    vocab.word_for(concept)
}

/// One concept in a people's tongue, **circumlocuting on a gap** rather than
/// going silent: a lexicalized concept surfaces as that tongue's own word; a
/// concept this people has no word for surfaces as its neutral Common word
/// instead (spec §3.1 — an absent word means the thing gets described, not
/// refused). The reader still learns the phenomenon is there, and the concept
/// id never reaches prose either way.
///
/// A gap therefore costs one word of register, not a whole line. That is the
/// cheapest honest failure available: the alternatives are dropping the line
/// (content loss, the thing this campaign exists to stop) or printing the key
/// (the second sign of the leak it exists to close).
fn tongue_word(lexicon: &Lexicon, vocab: &CommonVocabulary, concept: &str) -> String {
    match lexicon.entry(concept) {
        Some(LexEntry::Root { views, .. } | LexEntry::Compound { views, .. }) => {
            views.roman.clone()
        }
        // `LexEntry::Gap` (a reasoned absence) and `None` (never offered to
        // this lexicon at all) are the same thing to a reader: this people
        // cannot say it. Both circumlocute.
        Some(LexEntry::Gap { .. }) | None => common_word(vocab, concept),
    }
}

/// The reader-facing text for one phenomenon: its referent, realized as a
/// noun phrase in the speaker's own words, or in Common where a world has no
/// peoples to speak for it. Carries **no** salience — the caller owns the
/// bullet's shape — and no trailing punctuation, because a noun phrase is not
/// a sentence.
///
/// The kind is **not** rendered: it is a registry key, and a key in
/// reader-facing prose is the second sign of the leak this campaign closes
/// (spec §3.1). A reader gets nothing from `celestial-body` that `the moon`
/// does not already tell them.
///
/// Qualifiers are realized in the same register as the head and in the
/// producer's declared order, never dropped — a qualifier is a registered
/// concept carrying real content (`great` moon versus `little` moon is the
/// whole difference between two of seed 42's sky phenomena), so losing one
/// would be exactly the content loss this campaign is closing.
/// type-audit: bare-ok(prose: return)
pub fn phenomenon_line(
    phenomenon: &Phenomenon,
    speaker: Option<&Speaker>,
    vocab: &CommonVocabulary,
) -> String {
    let referent = &phenomenon.referent;
    match speaker {
        None => {
            let mut words: Vec<String> = referent
                .qualifiers
                .iter()
                .map(|q| common_word(vocab, q))
                .collect();
            words.push(common_word(vocab, &referent.concept));
            format!("the {}", words.join(" "))
        }
        Some(speaker) => {
            let head = tongue_word(&speaker.lexicon, vocab, &referent.concept);
            let qualifiers: Vec<String> = referent
                .qualifiers
                .iter()
                .map(|q| tongue_word(&speaker.lexicon, vocab, q))
                .collect();
            let words = match speaker.lexicon.headedness {
                Headedness::HeadFirst => {
                    let mut words = vec![head];
                    words.extend(qualifiers);
                    words
                }
                Headedness::HeadLast => {
                    let mut words = qualifiers;
                    words.push(head);
                    words
                }
            };
            words.join(" ")
        }
    }
}

/// A speaker whose tongue is `Steeped` in exactly `concepts` and has never
/// met anything else — the shared test fixture for both this module and the
/// document-level render tests in `lib.rs`. Built through `build_lexicon`
/// rather than a hand-stuffed map because [`Lexicon`] owns its entries
/// privately, so this is a genuinely drawn vocabulary.
///
/// `morph` is the inert bundle (no evidential or noun-class marking): this
/// renderer realizes a noun phrase, not a predication, and today reads only
/// `speaker.lexicon`. See the module docs.
#[cfg(test)]
pub(crate) fn test_speaker(concepts: &[&str]) -> Speaker {
    use hornvale_kernel::Seed;
    use hornvale_language::{
        CascadeRegime, ClassPosition, Envelope, ExoticSeg, ExposureClass, MorphDepth,
        TongueMorphology, build_lexicon, draw_phonology, tongue_grammar,
    };
    use std::collections::BTreeMap;

    // A permissive phonology, matching `domains/language`'s own lexicon
    // fixture idiom (that helper is private to its module).
    let ph = draw_phonology(
        &Seed(37),
        "test",
        &Envelope {
            labiality: 1.0,
            vowel_space: 1.0,
            voicing: 1.0,
            sibilance: 1.0,
            voice_loudness: 1.0,
            tonality: 0.0,
            exotic: ExoticSeg::None,
        },
    );
    let exposures: BTreeMap<String, ExposureClass> = concepts
        .iter()
        .map(|c| ((*c).to_string(), ExposureClass::Steeped))
        .collect();
    Speaker {
        species: "test".to_string(),
        lexicon: build_lexicon(
            &Seed(1),
            "test",
            "test",
            &ph,
            &ph,
            &exposures,
            &[],
            CascadeRegime::SETTLED,
        ),
        grammar: tongue_grammar(&Seed(1), "test", &ph),
        morph: TongueMorphology {
            evidential_depth: MorphDepth::None,
            noun_class_depth: MorphDepth::None,
            class_position: ClassPosition::Suffix,
            evidential: BTreeMap::new(),
            class: BTreeMap::new(),
        },
        sky_animate: false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Phenomenon, Referent, Venue};

    fn moon() -> Phenomenon {
        Phenomenon {
            kind: "celestial-body".to_string(),
            referent: Referent::qualified("moon", &["great"]),
            period_days: Some(27.3),
            salience: 0.7,
            venue: Venue::NightSky,
            // Task 6 deletes this field; it is present only so this file
            // compiles before that task runs.
            description: String::new(),
        }
    }

    /// With no speaker, the line is the neutral Common realization — what an
    /// out-of-world reader gets when the world has no peoples.
    #[test]
    fn a_referent_renders_without_a_speaker() {
        let line = phenomenon_line(&moon(), None, &CommonVocabulary::default());
        assert!(line.contains("moon"), "must name the concept: {line}");
        assert!(
            !line.contains("celestial-body"),
            "a registry key must never reach prose: {line}"
        );
    }

    /// A qualifier reaches the rendering — `great` is a registered concept and
    /// the line must be able to say it.
    #[test]
    fn a_qualifier_reaches_the_line() {
        let v = CommonVocabulary::default();
        let bare = Phenomenon {
            referent: Referent::of("moon"),
            ..moon()
        };
        assert_ne!(
            phenomenon_line(&bare, None, &v),
            phenomenon_line(&moon(), None, &v),
            "a qualified referent must render differently from a bare one"
        );
    }

    /// The Common register wears Common's article and reads as a noun phrase,
    /// not a sentence: no subject was invented, and nothing terminates it.
    #[test]
    fn the_common_register_is_an_article_plus_a_noun_phrase() {
        let v = CommonVocabulary::default();
        let bare = Phenomenon {
            referent: Referent::of("moon"),
            ..moon()
        };
        assert_eq!(phenomenon_line(&bare, None, &v), "the moon");
        assert_eq!(phenomenon_line(&moon(), None, &v), "the great moon");
    }

    /// A declared Common word wins here exactly as it does in the clause
    /// realizer — the vocabulary is consulted, never bypassed.
    #[test]
    fn a_declared_common_word_reaches_the_line() {
        let mut v = CommonVocabulary::default();
        v.declare("sun-like-star", "sun-like star");
        let p = Phenomenon {
            referent: Referent::of("sun-like-star"),
            ..moon()
        };
        assert_eq!(phenomenon_line(&p, None, &v), "the sun-like star");
    }

    /// The deliverable: with a speaker, the line is that people's own word,
    /// not Common's. This is the whole point of the campaign — the same
    /// phenomenon reads differently depending on who is looking.
    #[test]
    fn a_speaker_says_it_in_their_own_word() {
        let v = CommonVocabulary::default();
        let speaker = test_speaker(&["moon"]);
        let bare = Phenomenon {
            referent: Referent::of("moon"),
            ..moon()
        };
        let tongue = phenomenon_line(&bare, Some(&speaker), &v);
        assert_ne!(
            tongue,
            phenomenon_line(&bare, None, &v),
            "a speaker's line must not be the neutral Common line"
        );
        assert!(
            !tongue.contains("moon"),
            "the tongue's word for the moon is not the English one: {tongue}"
        );
        assert!(
            !tongue.contains("celestial-body"),
            "a registry key must never reach prose: {tongue}"
        );
    }

    /// A tongue wears no article. Hornvale draws no determiner system, so
    /// bolting Common's `the` onto a drawn noun would import English grammar
    /// into a language that never asked for it.
    #[test]
    fn a_tongue_wears_no_common_article() {
        let v = CommonVocabulary::default();
        let speaker = test_speaker(&["moon"]);
        let bare = Phenomenon {
            referent: Referent::of("moon"),
            ..moon()
        };
        let tongue = phenomenon_line(&bare, Some(&speaker), &v);
        assert!(
            !tongue.starts_with("the "),
            "a tongue line must not wear Common's article: {tongue}"
        );
    }

    /// A concept this tongue has no word for **circumlocutes into Common**
    /// rather than dropping the line or printing the key: the head still
    /// says `moon` in the tongue, and `great` falls back to its Common word.
    /// Spec §3.1 — an absent word means the thing gets described, not refused.
    #[test]
    fn a_gap_circumlocutes_instead_of_going_silent() {
        let v = CommonVocabulary::default();
        // `great` is simply not offered to this lexicon, the commonest shape
        // of "this people has no word for it".
        let speaker = test_speaker(&["moon"]);
        let line = phenomenon_line(&moon(), Some(&speaker), &v);
        assert!(
            line.contains("great"),
            "the qualifier must survive as its Common word: {line}"
        );
        let bare = Phenomenon {
            referent: Referent::of("moon"),
            ..moon()
        };
        assert!(
            line.contains(&phenomenon_line(&bare, Some(&speaker), &v)),
            "the head must still be the tongue's own word: {line}"
        );
    }

    /// Modifier/head order in a tongue is the lexicon's own drawn
    /// `Headedness` — the same parameter that orders every compound this
    /// people builds — never an English default imposed on it.
    #[test]
    fn qualifier_order_follows_the_tongues_drawn_headedness() {
        let v = CommonVocabulary::default();
        let speaker = test_speaker(&["moon"]);
        let head = phenomenon_line(
            &Phenomenon {
                referent: Referent::of("moon"),
                ..moon()
            },
            Some(&speaker),
            &v,
        );
        let line = phenomenon_line(&moon(), Some(&speaker), &v);
        match speaker.lexicon.headedness {
            Headedness::HeadFirst => assert_eq!(line, format!("{head} great")),
            Headedness::HeadLast => assert_eq!(line, format!("great {head}")),
        }
    }

    /// Rendering is a pure function of the referent and the register: same
    /// inputs, same bytes, every time (the artifact contract).
    #[test]
    fn rendering_is_deterministic() {
        let v = CommonVocabulary::default();
        let speaker = test_speaker(&["moon"]);
        assert_eq!(
            phenomenon_line(&moon(), Some(&speaker), &v),
            phenomenon_line(&moon(), Some(&speaker), &v)
        );
    }

    /// The kind is dropped entirely — a reader gains nothing from
    /// `celestial-body` that `the moon` does not already tell them, and a
    /// registry key in reader-facing prose is the second sign of the leak
    /// this campaign closes.
    #[test]
    fn the_kind_never_reaches_the_line() {
        let v = CommonVocabulary::default();
        let mut p = moon();
        p.kind = "an-unlikely-registry-key".to_string();
        assert_eq!(phenomenon_line(&p, None, &v), "the great moon");
    }
}
