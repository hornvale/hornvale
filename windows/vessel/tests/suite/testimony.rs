//! The lexical gap (The Confidant, Task 4): what two cultures' lexicons say
//! about a driven host's true felt state, through
//! `hornvale_vessel::testimony::testify`.
//!
//! **The trap this file exists to avoid (Task 4 brief):** as of Task 3, no
//! culture's REAL exposure pipeline (`windows/worldgen`'s
//! `exposure_of_impl`) ever grants `Steeped`/`KnowsOf` over a felt-state
//! concept — `domains/language::felt_state_pack`'s own doc says so
//! ("deliberately NOT chained into any Swadesh pack"), and grepping
//! `exposure_of_impl` confirms every felt-state concept falls through to its
//! closing catch-all Experiential gap for every species. So a test built
//! from two REAL, world-generated cultures would report the identical
//! fallback for both, unable to distinguish a working implementation from a
//! broken one.
//!
//! The remedy used here is the SAME degree of freedom every other test in
//! `domains/language/src/lexicon.rs` already exercises: `build_lexicon`'s
//! `exposures` map is concept-agnostic (`ExposureClass` carries no notion of
//! "which pack" a concept belongs to), so a test may legitimately hand it
//! `ExposureClass::Steeped` for a felt-state concept id directly, exactly as
//! the file's own `sea_exposures()` hands it `Steeped` for `"water"`. This
//! is exercising the lexicon mechanism's existing freedom, not authoring a
//! new production exposure rule — nothing in `windows/worldgen` changes.

use hornvale_kernel::Seed;
use hornvale_language::{
    CascadeRegime, Envelope, ExoticSeg, ExposureClass, GapReason, draw_phonology,
};
use hornvale_vessel::liveness::AffectLabel;
use hornvale_vessel::testimony::{FeltStateWord, testify};
use std::collections::BTreeMap;

/// A permissive phonology (mirrors `domains/language/src/lexicon.rs`'s own
/// `test_phonology` fixture): full vowel space, every place/manner
/// combination reachable.
fn test_phonology(species: &str, seed: u64) -> hornvale_language::Phonology {
    draw_phonology(
        &Seed(seed),
        species,
        &Envelope {
            labiality: 1.0,
            vowel_space: 1.0,
            voicing: 1.0,
            sibilance: 1.0,
            voice_loudness: 1.0,
            tonality: 0.0,
            exotic: ExoticSeg::None,
        },
        &hornvale_language::typology::concatenative(),
    )
}

/// An exposures map where exactly `steeped` concept ids are `Steeped` and
/// every other felt-state concept id (`content`, `eager`, `searching`,
/// `frustrated`, `lost`, `helpless`) is an `Unknown` gap with a distinct,
/// recountable reason.
fn felt_state_exposures(steeped: &[&str]) -> BTreeMap<String, ExposureClass> {
    let all = [
        "content",
        "eager",
        "searching",
        "frustrated",
        "lost",
        "helpless",
    ];
    let mut exposures = BTreeMap::new();
    for concept in all {
        let class = if steeped.contains(&concept) {
            ExposureClass::Steeped
        } else {
            ExposureClass::Unknown {
                reason: GapReason::Experiential(format!(
                    "this test culture never named '{concept}'"
                )),
            }
        };
        exposures.insert(concept.to_string(), class);
    }
    exposures
}

fn build(species: &str, seed: u64, steeped: &[&str]) -> hornvale_language::Lexicon {
    let ph = test_phonology(species, seed);
    let exposures = felt_state_exposures(steeped);
    hornvale_language::build_lexicon(
        &Seed(seed),
        species,
        species,
        &ph,
        &ph,
        &exposures,
        &[],
        CascadeRegime::SETTLED,
    )
}

/// Step 1's discriminating property: two authored test cultures, one true
/// internal state (`Content`), two different words out. Culture A has its
/// own word for `content`; culture B does not, and its nearest known felt
/// state is `eager` (the only other one it holds).
#[test]
fn two_cultures_report_different_words_for_the_same_true_state() {
    let culture_a = build("kobold", 5, &["content"]);
    let culture_b = build("goblin", 11, &["eager"]);

    let a = testify(&culture_a, AffectLabel::Content).expect("kobold has a word for content");
    let b = testify(&culture_b, AffectLabel::Content).expect("goblin falls back to eager's word");

    let (a_word, b_word) = match (&a, &b) {
        (
            FeltStateWord::Direct(views),
            FeltStateWord::Nearest {
                word, reported_as, ..
            },
        ) => {
            assert_eq!(
                *reported_as,
                AffectLabel::Eager,
                "goblin's only known felt state is eager, so that must be what it reports"
            );
            (views.roman.clone(), word.roman.clone())
        }
        other => panic!(
            "expected (Direct, Nearest), got {other:?} -- the two cultures should differ in KIND \
             of answer, not just content"
        ),
    };
    assert_ne!(
        a_word, b_word,
        "same true state, two cultures, must produce two different words: got {a_word:?} for \
         both"
    );
}

/// Step 4's fallback proof: within ONE culture, a state it HAS a word for
/// reports directly, and a state it does NOT have a word for falls back to
/// its nearest known neighbour -- two structurally different outputs from
/// the same lexicon.
#[test]
fn the_fallback_fires_only_when_the_culture_lacks_the_word() {
    let lex = build("hobgoblin", 7, &["content", "eager"]);

    let has_it = testify(&lex, AffectLabel::Content).expect("content is Steeped");
    assert!(
        matches!(has_it, FeltStateWord::Direct(_)),
        "a culture that HAS the word must report Direct, got {has_it:?}"
    );

    // Searching is gapped; its nearest neighbour among {content, eager} is
    // content (distance 2 to both content and eager under the doc-authored
    // circumplex positions -- tie broken toward Content, which sorts first
    // in AffectLabel's declaration order).
    let lacks_it = testify(&lex, AffectLabel::Searching).expect("eager or content stands in");
    match &lacks_it {
        FeltStateWord::Nearest {
            reported_as,
            reason,
            ..
        } => {
            assert!(
                matches!(reported_as, AffectLabel::Content | AffectLabel::Eager),
                "the substitute must be one of the culture's actually-known words, got \
                 {reported_as:?}"
            );
            assert!(
                matches!(reason, GapReason::Experiential(_)),
                "the carried reason must be searching's own gap, got {reason:?}"
            );
        }
        other => panic!("a culture that LACKS the word must report Nearest, got {other:?}"),
    }

    match (&has_it, &lacks_it) {
        (FeltStateWord::Direct(_), FeltStateWord::Nearest { .. }) => {}
        _ => panic!("the two cases must differ in KIND of output, not just value"),
    }
}

/// A culture with no felt-state word at all reports nothing to substitute
/// -- `testify` must not fabricate a word from thin air.
#[test]
fn a_culture_with_no_felt_state_words_reports_none() {
    let lex = build("otyugh", 13, &[]);
    assert_eq!(
        testify(&lex, AffectLabel::Content),
        None,
        "no known felt-state word anywhere in this lexicon; nothing to substitute"
    );
}
