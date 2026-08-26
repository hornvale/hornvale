//! What a people believes a rider IS — derived, never authored.
//!
//! The frontier essay draws the line at *has a rider-doctrine* vs
//! *improvises*: "a people with the doctrine names you correctly and knows what
//! to do about it, and a people without one explains you with the words it has
//! — intrusive thoughts, a haunting, a fever, a god, a wandering ancestor."
//! `god` and `spirit` are on the IMPROVISING side. No rider concept is
//! registered in this world (zero exist; see the spec's section 10.4), so every
//! people improvises and [`ImprovisedName`] has no doctrine arm at all. Adding
//! one is a deliberate, owner-approved act, not a fallthrough.

use hornvale_kernel::World;
use hornvale_language::{GapReason, LexEntry, Lexicon};

/// What a people with no word for a rider reaches for instead.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImprovisedName {
    /// Settled, organized cult: apparatus, precedent, a prescribed response.
    God,
    /// Settled, folk cult: a word, and no machinery behind it.
    Spirit,
    /// Unsettled: not even a word, carrying the lexicon's own reason.
    Wordless {
        /// Why this culture has no word for what the rider is.
        reason: GapReason,
    },
}

/// How willing the doctrine prior alone leaves a host, before any conduct is
/// folded in. Ordered: the least-equipped people is the most open.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Openness {
    /// Has apparatus for you. Least willing to speak.
    Guarded,
    /// Has a word for you and nothing behind it.
    Wary,
    /// Has nothing to invoke. Most willing to speak.
    Open,
}

/// The prior's sign. The essay's doctrine-holding people "knows what to do
/// about it", so equipment reduces willingness rather than raising it — the
/// opposite of the intuitive reading, and the one the essay supports.
pub fn openness(name: &ImprovisedName) -> Openness {
    match name {
        ImprovisedName::God => Openness::Guarded,
        ImprovisedName::Spirit => Openness::Wary,
        ImprovisedName::Wordless { .. } => Openness::Open,
    }
}

/// Which word `people` reaches for. Settled peoples are Steeped in both `god`
/// and `spirit` (`windows/worldgen/src/lib.rs:5938`), so lexical coverage
/// separates settled from unsettled and `cult-form` chooses between the two
/// settled arms. `cult-form` is uniform per people (measured: 0 of 15 mixed on
/// seed 42), so the FIRST belief held by this people decides — a scan, not a
/// vote, and the uniformity is asserted by this task's own test rather than
/// assumed here.
/// type-audit: bare-ok(identifier-text: people)
pub fn improvised_name(world: &World, lexicon: &Lexicon, people: &str) -> ImprovisedName {
    let unsettled_reason = |concept: &str| match lexicon.entry(concept) {
        Some(LexEntry::Gap { reason }) => Some(reason.clone()),
        _ => None,
    };
    if let Some(reason) = unsettled_reason("god").or_else(|| unsettled_reason("spirit")) {
        return ImprovisedName::Wordless { reason };
    }
    match cult_form_of(world, people).as_deref() {
        Some("organized") => ImprovisedName::God,
        _ => ImprovisedName::Spirit,
    }
}

/// The `cult-form` of the first belief held at a site this species peoples,
/// by ledger order.
///
/// **`held-by` targets a SETTLEMENT entity, not a people**, so the join runs
/// species -> `occ-people` -> site -> `held-by` -> belief -> `cult-form`. On
/// seed 42 species and site happen to be 1:1 (15 and 15), but that is a
/// property of one world, not a guarantee, so this takes the first by ledger
/// order and the task's own test asserts uniformity PER SPECIES — which is the
/// denominator the campaign's numbers are quoted on.
///
/// Public since The Reticence, Task 6: the per-people report
/// (`hornvale_lab::render_reticence_report`) shows this raw value beside the
/// [`ImprovisedName`] it collapses to (`Some("organized")` -> [`God`](ImprovisedName::God),
/// everything else -> [`Spirit`](ImprovisedName::Spirit), which cannot
/// distinguish "folk" from "no cult-form fact at all"). Re-deriving the join
/// a second time in `windows/lab` instead of exposing this would duplicate
/// the query this crate already owns.
/// type-audit: bare-ok(identifier-text: people), bare-ok(identifier-text: return)
pub fn cult_form_of(world: &World, people: &str) -> Option<String> {
    use hornvale_kernel::Value;
    use hornvale_religion::{CULT_FORM, HELD_BY};
    let sites: std::collections::BTreeSet<_> = world
        .ledger
        .find("occ-people")
        .filter(|f| matches!(&f.object, Value::Text(t) if t == people))
        .map(|f| f.subject)
        .collect();
    world
        .ledger
        .find(HELD_BY)
        .filter(|f| matches!(&f.object, Value::Entity(e) if sites.contains(e)))
        .find_map(|f| world.ledger.text_of(f.subject, CULT_FORM))
        .map(str::to_string)
}
