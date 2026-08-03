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
//!   orders every compound this people builds. The head noun additionally
//!   carries C7 noun-class marking ([`tongue_head`]) — a class marker binds
//!   to a noun, not to a predication, so it is meaningful here even though
//!   this is a bare noun phrase and neither clause realizer runs.

use hornvale_kernel::Phenomenon;
use hornvale_language::{
    ClassPosition, CommonVocabulary, Headedness, LexEntry, Lexicon, MorphDepth, NounClass, affix,
    noun_class_with_sky,
};

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

/// The referent's head concept, realized in a speaker's tongue with C7
/// noun-class marking applied at whatever depth `speaker.morph` draws — the
/// morphological half of [`hornvale_language::realize_tongue_deep`]'s
/// noun-class arm, reapplied here because a phenomenon line has no clause
/// for that realizer to run (see the module docs); [`noun_class_with_sky`]
/// answers the SAME classification either way.
///
/// Marking only ever binds to a NATIVE lexicon entry (`Root`/`Compound`): a
/// concept that circumlocutes into Common (a [`LexEntry::Gap`], or no entry
/// at all) borrows a foreign word, and grafting this people's own class
/// morpheme onto someone else's noun would not be marking anything, so a
/// circumlocuted head renders exactly as [`tongue_word`] already renders
/// it — unmarked.
///
/// `MorphDepth::Affix` additionally needs the word's own retained
/// phonological segments, which only [`LexEntry::Root`] keeps —
/// [`LexEntry::Compound`] discards its already-joined segments at build
/// time (the identical retained-segments gap
/// `hornvale_language::grammar::layer_affix` — not exported, but its doc
/// comment names the same gap — panics on for a clause complement). None of
/// [`hornvale_language::SKY_OVERRIDE`]'s four sky bodies are ever compound
/// concepts (`hornvale_language::compound_recipe` only ever answers `sea`,
/// `mountain`, `coast`, `lake`, all `NounClass::Inanimate` and none of them
/// observed as a phenomenon referent today), so this arm is unreached by
/// anything this renderer marks; it degrades to the unmarked word rather
/// than panicking, because a renderer feeding a committed artifact must
/// never panic on a case its own data cannot yet produce.
fn tongue_head(speaker: &Speaker, vocab: &CommonVocabulary, concept: &str) -> String {
    let (plain, segments) = match speaker.lexicon.entry(concept) {
        Some(LexEntry::Root { derivation, views }) => {
            (views.roman.clone(), Some(&derivation.modern))
        }
        Some(LexEntry::Compound { views, .. }) => (views.roman.clone(), None),
        Some(LexEntry::Gap { .. }) | None => return common_word(vocab, concept),
    };

    let class_value = match noun_class_with_sky(speaker.sky_animate, concept) {
        NounClass::Animate => "animate",
        NounClass::Inanimate => "inanimate",
    };
    let Some(marker) = speaker.morph.class.get(class_value) else {
        return plain;
    };

    match speaker.morph.noun_class_depth {
        MorphDepth::None => plain,
        MorphDepth::Particle => match speaker.morph.class_position {
            ClassPosition::Prefix => format!("{} {plain}", marker.roman),
            ClassPosition::Suffix => format!("{plain} {}", marker.roman),
        },
        MorphDepth::Affix => match segments {
            Some(segs) => affix(segs, &marker.segments, speaker.morph.class_position).roman,
            None => plain,
        },
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
            let head = tongue_head(speaker, vocab, &referent.concept);
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
/// `morph`'s bundle is inert by default (`MorphDepth::None` on both axes,
/// no marker forms) so every existing caller keeps its C3-floor surface; a
/// test exercising noun-class marking overrides `morph`/`sky_animate` on
/// the returned `Speaker` (see `sky_override_concepts_mark_by_...` below).
#[cfg(test)]
pub(crate) fn test_speaker(concepts: &[&str]) -> Speaker {
    use hornvale_kernel::Seed;
    use hornvale_language::{
        CascadeRegime, ClassPosition, Envelope, ExoticSeg, ExposureClass, MorphDepth,
        TongueMorphology, build_lexicon, draw_phonology,
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
    use hornvale_language::{Manner, MorphForm, Place, Segment};

    fn moon() -> Phenomenon {
        Phenomenon {
            kind: "celestial-body".to_string(),
            referent: Referent::qualified("moon", &["great"]),
            period_days: Some(27.3),
            salience: 0.7,
            venue: Venue::NightSky,
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

    /// The deliverable: a sky-override concept's noun-class marking flips
    /// with the culture's agentive day-schema draw. Asserts on RENDERED
    /// TEXT, never a `NounClass` value — feeding the classifier's own
    /// output back to itself would prove nothing about the renderer, which
    /// is exactly how this campaign shipped an earlier bug undetected.
    #[test]
    fn sky_override_marks_by_the_agentive_day_schema_particle_depth() {
        let v = CommonVocabulary::default();
        let mut speaker = test_speaker(&["sun"]);
        speaker.morph.noun_class_depth = MorphDepth::Particle;
        speaker.morph.class_position = ClassPosition::Suffix;
        speaker.morph.class.insert(
            "animate",
            MorphForm {
                segments: vec![],
                roman: "aya".to_string(),
            },
        );
        speaker.morph.class.insert(
            "inanimate",
            MorphForm {
                segments: vec![],
                roman: "ombo".to_string(),
            },
        );
        let p = Phenomenon {
            referent: Referent::of("sun"),
            ..moon()
        };

        speaker.sky_animate = true;
        let animate_line = phenomenon_line(&p, Some(&speaker), &v);
        speaker.sky_animate = false;
        let inanimate_line = phenomenon_line(&p, Some(&speaker), &v);

        assert_ne!(
            animate_line, inanimate_line,
            "the same sky concept must render differently under an agentive \
             vs. non-agentive day-schema: animate={animate_line:?} \
             inanimate={inanimate_line:?}"
        );
        assert!(
            animate_line.ends_with("aya"),
            "the animate marker must reach the rendered line: {animate_line}"
        );
        assert!(
            inanimate_line.ends_with("ombo"),
            "the inanimate marker must reach the rendered line: {inanimate_line}"
        );
    }

    /// The same deliverable at `MorphDepth::Affix` — the segment-level join
    /// (`hornvale_language::affix`), not just a free particle. Real seed-42
    /// species draw this depth ~30% of the time (`NOUN_CLASS_DEPTH_WEIGHTS`),
    /// so it must work, not merely the particle case.
    #[test]
    fn sky_override_marks_by_the_agentive_day_schema_affix_depth() {
        let v = CommonVocabulary::default();
        let mut speaker = test_speaker(&["sun"]);
        let p = Phenomenon {
            referent: Referent::of("sun"),
            ..moon()
        };
        let unmarked = phenomenon_line(&p, Some(&speaker), &v);

        speaker.morph.noun_class_depth = MorphDepth::Affix;
        speaker.morph.class_position = ClassPosition::Suffix;
        speaker.morph.class.insert(
            "animate",
            MorphForm {
                segments: vec![Segment::Consonant {
                    place: Place::Alveolar,
                    manner: Manner::Sibilant,
                    voiced: false,
                }],
                roman: "s".to_string(),
            },
        );
        speaker.morph.class.insert(
            "inanimate",
            MorphForm {
                segments: vec![Segment::Consonant {
                    place: Place::Labial,
                    manner: Manner::Stop,
                    voiced: false,
                }],
                roman: "p".to_string(),
            },
        );

        speaker.sky_animate = true;
        let animate_line = phenomenon_line(&p, Some(&speaker), &v);
        speaker.sky_animate = false;
        let inanimate_line = phenomenon_line(&p, Some(&speaker), &v);

        assert_ne!(
            animate_line, unmarked,
            "affix marking must change the rendered word: {animate_line}"
        );
        assert_ne!(
            inanimate_line, unmarked,
            "affix marking must change the rendered word: {inanimate_line}"
        );
        assert_ne!(
            animate_line, inanimate_line,
            "the two classes' affixes must render differently: \
             animate={animate_line:?} inanimate={inanimate_line:?}"
        );
    }

    /// A circumlocuted head — this people has no native word for the
    /// concept, so it borrows Common's — never receives this tongue's own
    /// class morpheme. Grafting a native morpheme onto a foreign word would
    /// not be marking anything.
    #[test]
    fn a_circumlocuted_head_is_never_class_marked() {
        let v = CommonVocabulary::default();
        // Only "moon" is exposed — "sun" circumlocutes into Common.
        let mut speaker = test_speaker(&["moon"]);
        speaker.morph.noun_class_depth = MorphDepth::Particle;
        speaker.morph.class_position = ClassPosition::Suffix;
        speaker.morph.class.insert(
            "animate",
            MorphForm {
                segments: vec![],
                roman: "aya".to_string(),
            },
        );
        speaker.sky_animate = true; // "sun" would be Animate if marked.

        let line = phenomenon_line(
            &Phenomenon {
                referent: Referent::of("sun"),
                ..moon()
            },
            Some(&speaker),
            &v,
        );
        assert!(
            !line.contains("aya"),
            "a foreign (Common-circumlocuted) word must never carry this \
             tongue's own class marker: {line}"
        );
    }
}
