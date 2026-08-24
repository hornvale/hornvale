//! The lexical residue (The Confidant, spec §3.1, Task 4): what a culture's
//! own tongue can say about a driven host's true [`AffectLabel`], read
//! through its [`hornvale_language::Lexicon`]. A culture that has a word for
//! the true state reports it directly; one that does not reports the
//! nearest state it DOES have a word for, together with the
//! [`hornvale_language::GapReason`] the true state lacks one — the
//! self-deception this campaign models falls straight out of vocabulary
//! coverage, with no separate deception mechanism.
//!
//! **This module adds no parallel exposure or gap machinery.** The
//! mechanism is entirely `domains/language`'s existing two-pass lexicon
//! (`build_lexicon`, `ExposureClass`, `GapReason`,
//! [`hornvale_language::LexEntry`]); this module only adds the
//! `AffectLabel`-keyed lookup a domain crate structurally cannot host
//! itself. `AffectLabel` lives here, in a window; a domain depends on the
//! kernel and nothing else (`domains/CLAUDE.md`'s one rule), so
//! `hornvale_language` cannot import it — the same reason
//! `windows/vessel/tests/suite/felt_state_concepts.rs` keeps the two
//! rosters in step from this side rather than the other.

use crate::liveness::AffectLabel;
use hornvale_language::{GapReason, LexEntry, Lexicon, WordViews};

/// `AffectLabel`'s registered concept id in `domains/language`'s
/// `felt_state_pack` — the same mapping
/// `windows/vessel/tests/suite/felt_state_concepts.rs`'s own `concept_name`
/// holds the two rosters in step against (that test file cannot import this
/// one; integration tests do not share code across files without a
/// `tests/common` module, and this mapping is one match arm either way).
/// Exhaustive by construction: an unmatched future `AffectLabel` variant
/// fails to compile until it is named here.
///
/// `pub(crate)` (The Confidant, Task 6): `windows/vessel/src/session.rs`'s
/// `ask` verb reads it back to gloss whichever concept a testimony actually
/// REPORTS (`FeltStateWord::Direct`'s implicit concept, or `Nearest`'s own
/// `reported_as`) into the same short id `hornvale_language`'s Common
/// vocabulary derives a word from — never the true label a divergent
/// testimony must not leak.
pub(crate) fn concept_id(label: AffectLabel) -> &'static str {
    match label {
        AffectLabel::Content => "content",
        AffectLabel::Eager => "eager",
        AffectLabel::Searching => "searching",
        AffectLabel::Frustrated => "frustrated",
        AffectLabel::Lost => "lost",
        AffectLabel::Helpless => "helpless",
    }
}

/// Every `AffectLabel` variant, in declaration order — the fixed,
/// deterministic scan [`nearest`] walks, so two candidates tied on
/// [`circumplex_distance`] always resolve to the same winner (`Content`
/// before `Eager` before `Searching` before `Frustrated` before `Lost`
/// before `Helpless`) rather than depending on map iteration order.
const ALL_LABELS: [AffectLabel; 6] = [
    AffectLabel::Content,
    AffectLabel::Eager,
    AffectLabel::Searching,
    AffectLabel::Frustrated,
    AffectLabel::Lost,
    AffectLabel::Helpless,
];

/// `(valence, arousal)` on the three-point `-1`/`0`/`1` scale `AffectLabel`'s
/// own doc comments place each variant on: positive/neutral/negative
/// valence, low/mid/high arousal. This is the coarsest scale the docs
/// support — no sub-integer precision is invented.
///
/// Five of the six are read straight off the enum's own doc comments.
/// `Helpless`'s valence is likewise stated there ("Negative, persistent"),
/// but its doc is silent on arousal specifically. That value is instead
/// drawn from `arbitrate`'s own authored contract for the state
/// (`liveness.rs`, the `helpless` short-circuit): "Arousal stays high (the
/// need is real and unmet); valence is negative" — the same file, the same
/// type, the computation that actually produces a `Helpless` reading. This
/// is reading an authored placement, not inventing one; see this task's
/// report if that judgment call needs revisiting.
fn circumplex_position(label: AffectLabel) -> (i32, i32) {
    match label {
        AffectLabel::Content => (1, -1),
        AffectLabel::Eager => (1, 1),
        AffectLabel::Searching => (0, 0),
        AffectLabel::Frustrated => (-1, 1),
        AffectLabel::Lost => (-1, -1),
        AffectLabel::Helpless => (-1, 1),
    }
}

/// Manhattan distance between two labels' [`circumplex_position`]s.
fn circumplex_distance(a: AffectLabel, b: AffectLabel) -> i32 {
    let (av, aa) = circumplex_position(a);
    let (bv, ba) = circumplex_position(b);
    (av - bv).abs() + (aa - ba).abs()
}

/// `label`'s word in `lexicon`, if `lexicon` holds it as a
/// [`LexEntry::Root`] or [`LexEntry::Compound`] — the two entry kinds that
/// carry a spoken word at all.
fn known_word(lexicon: &Lexicon, label: AffectLabel) -> Option<&WordViews> {
    match lexicon.entry(concept_id(label)) {
        Some(LexEntry::Root { views, .. } | LexEntry::Compound { views, .. }) => Some(views),
        _ => None,
    }
}

/// Among the other five labels, the one nearest `label` on the circumplex
/// (Manhattan distance, ties broken by [`ALL_LABELS`] declaration order)
/// that `lexicon` DOES hold a word for. `None` if `lexicon` has no word for
/// any felt state at all.
fn nearest(lexicon: &Lexicon, label: AffectLabel) -> Option<AffectLabel> {
    let mut best: Option<(AffectLabel, i32)> = None;
    for candidate in ALL_LABELS {
        if candidate == label || known_word(lexicon, candidate).is_none() {
            continue;
        }
        let d = circumplex_distance(label, candidate);
        let better = match best {
            None => true,
            Some((_, best_d)) => d < best_d,
        };
        if better {
            best = Some((candidate, d));
        }
    }
    best.map(|(label, _)| label)
}

/// What a culture's tongue says about a driven host's felt state: its own
/// word, or the nearest state's word standing in for it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FeltStateWord {
    /// The culture has its own word for the true state.
    Direct(WordViews),
    /// The culture has no word for the true state; this is the nearest
    /// circumplex neighbour it DOES have a word for, carrying the reason
    /// the true state itself has none.
    Nearest {
        /// The nearest known state's word — what the host actually says.
        word: WordViews,
        /// Which state that word actually names (not the true state).
        reported_as: AffectLabel,
        /// Why the true state has no word of its own.
        reason: GapReason,
    },
}

/// What `lexicon`'s culture says about `label`: its exact word if it has
/// one, else the word for the nearest state it does have (by
/// [`circumplex_distance`]) carrying the true state's [`GapReason`]. `None`
/// only when `lexicon` has no word for `label` AND no word for any other
/// felt state either — the culture is silent on feeling entirely, and
/// there is nothing to substitute. `label`'s concept must be present in
/// `lexicon` (built from an `exposures` map that includes it) for a
/// [`GapReason`] to be available; if it is entirely absent this also
/// returns `None`, since there is no reason to carry.
pub fn testify(lexicon: &Lexicon, label: AffectLabel) -> Option<FeltStateWord> {
    if let Some(views) = known_word(lexicon, label) {
        return Some(FeltStateWord::Direct(views.clone()));
    }
    let Some(LexEntry::Gap { reason }) = lexicon.entry(concept_id(label)) else {
        return None;
    };
    let reported_as = nearest(lexicon, label)?;
    let word = known_word(lexicon, reported_as)?.clone();
    Some(FeltStateWord::Nearest {
        word,
        reported_as,
        reason: reason.clone(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The six positions this module reads off `AffectLabel`'s own docs
    /// (`Helpless`'s arousal via `arbitrate`'s contract — see
    /// [`circumplex_position`]'s doc). Pins the derivation so a future
    /// edit to the mapping is a deliberate, reviewed change.
    #[test]
    fn circumplex_positions_match_the_authored_docs() {
        assert_eq!(circumplex_position(AffectLabel::Content), (1, -1));
        assert_eq!(circumplex_position(AffectLabel::Eager), (1, 1));
        assert_eq!(circumplex_position(AffectLabel::Searching), (0, 0));
        assert_eq!(circumplex_position(AffectLabel::Frustrated), (-1, 1));
        assert_eq!(circumplex_position(AffectLabel::Lost), (-1, -1));
        assert_eq!(circumplex_position(AffectLabel::Helpless), (-1, 1));
    }

    /// `Frustrated` and `Helpless` share a circumplex position under this
    /// derivation (both negative valence, high arousal) — a real
    /// consequence of the doc-authored placement, not a defect: `Helpless`
    /// is documented as the persistent upgrade of `Frustrated` (and
    /// `Lost`), the same felt region sustained rather than a different
    /// one. Pinned so it reads as a decision, not a surprise.
    #[test]
    fn frustrated_and_helpless_coincide_on_the_circumplex() {
        assert_eq!(
            circumplex_distance(AffectLabel::Frustrated, AffectLabel::Helpless),
            0
        );
    }
}
