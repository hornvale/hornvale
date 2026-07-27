//! Prose for a CHAMBER. Deliberately not `windows/locale`'s describer: that one
//! is written for ~1.7 km places and, verified at depth 21, reports biome,
//! elevation and a terrain micro-regime — it would describe a dwelling's room
//! as seafloor. Prose is the constitutionally primary surface (§3.5), so a
//! chamber gets its own prose built from what the chamber actually holds.

use crate::brief::Brief;
use crate::interior::{AnchorKind, Interior};

/// The noun for an anchor kind, as prose says it. `Ground` has no noun: it is
/// the chamber's own floor, not a thing standing in it.
pub(crate) fn noun(kind: AnchorKind) -> Option<&'static str> {
    match kind {
        AnchorKind::Ground => None,
        AnchorKind::Hearth => Some("a hearth"),
        AnchorKind::Threshold => Some("a doorway"),
        AnchorKind::Bed => Some("a bed"),
        AnchorKind::Vessel => Some("a water jar"),
        AnchorKind::Screen => Some("a screen"),
        AnchorKind::Alcove => Some("an alcove"),
        AnchorKind::Pool => Some("a still pool"),
        AnchorKind::Log => Some("a fallen log"),
    }
}

/// Every noun a chamber's prose will name, in the interior's own deterministic
/// anchor order. The ONE catalogue: `describe_chamber` renders from it and the
/// session's `enter <named>` resolves against it, so a player can only ever be
/// asked to name a thing the prose actually said (the same discipline The
/// Purview's chart follows in sharing the prose's nouns).
pub(crate) fn chamber_nouns(interior: &Interior) -> Vec<&'static str> {
    interior
        .ids()
        .iter()
        .filter_map(|&id| noun(interior.anchor(id).kind))
        .collect()
}

/// A chamber's prose: what stands in it, in the interior's own deterministic
/// anchor order. Usually one sentence — the single-anchor case reads better as
/// two ("A small room. A hearth stands here.") — so the invariant that is
/// actually pinned is that **every branch ends in a period and contains no
/// empty clause**, not a sentence count. Do not promise "one sentence" here;
/// an earlier draft did, and the single-anchor branch quietly broke it.
///
/// `brief` is read, not carried: a built place is a *room*, an unbuilt one is a
/// *hollow*, and that single word is the difference between a dwelling and a
/// cave mouth. (An unused parameter would be dead weight and a reviewer would
/// be right to flag it.)
/// type-audit: bare-ok(prose: return)
pub fn describe_chamber(interior: &Interior, brief: &Brief) -> String {
    let place = if brief.built { "room" } else { "hollow" };
    let nouns = chamber_nouns(interior);
    match nouns.len() {
        0 => format!("A bare {place}, its floor swept and its corners empty."),
        1 => format!("A small {place}. {} stands here.", capitalize(nouns[0])),
        _ => {
            let (last, rest) = nouns.split_last().expect("len >= 2");
            format!("A small {place}, holding {} and {}.", rest.join(", "), last)
        }
    }
}

/// Capitalize a noun phrase's first letter for sentence-initial use.
fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        None => String::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::brief::Brief;
    use crate::interior::{AnchorKind, Interior};

    fn brief() -> Brief {
        Brief::from_parts(None, None, None, None, true, true)
    }

    fn interior_with(kinds: &[AnchorKind]) -> Interior {
        let mut i = Interior::new();
        let mut prev = None;
        for &k in kinds {
            let id = i.push(k, None);
            if let Some(p) = prev {
                i.connect(p, id);
            }
            prev = Some(id);
        }
        i
    }

    #[test]
    fn a_chamber_names_what_it_holds() {
        let text = describe_chamber(
            &interior_with(&[AnchorKind::Ground, AnchorKind::Hearth, AnchorKind::Bed]),
            &brief(),
        );
        assert!(text.contains("hearth"), "got: {text}");
        assert!(text.contains("bed"), "got: {text}");
    }

    #[test]
    fn a_chamber_never_speaks_of_terrain() {
        let text = describe_chamber(
            &interior_with(&[AnchorKind::Ground, AnchorKind::Hearth]),
            &brief(),
        );
        // The locale describer's FIELD LABELS — these catch a wholesale
        // call-through, which is the failure that was measured at depth 21.
        for banned in ["biome", "elevation", "moisture", "regime"] {
            assert!(
                !text.contains(banned),
                "chamber prose leaked a terrain field label {banned:?}: {text}"
            );
        }
        // And the ADJECTIVES it actually emits (`windows/locale/src/grammar.rs`).
        // A stylistic bleed — someone hand-writing terrain-flavoured prose —
        // is likelier than a call-through, and the label list above would miss
        // it entirely. `" dry"` carries a leading space on purpose so a future
        // legitimate noun like "laundry" does not trip it.
        for banned in ["sun-warmed", "shaded", "unremarkable ground", " dry"] {
            assert!(
                !text.contains(banned),
                "chamber prose leaked a terrain adjective {banned:?}: {text}"
            );
        }
    }

    #[test]
    fn an_empty_chamber_still_reads_as_a_place() {
        let text = describe_chamber(&interior_with(&[AnchorKind::Ground]), &brief());
        assert!(!text.trim().is_empty());
        assert!(text.ends_with('.'), "prose is a sentence: {text}");
    }

    #[test]
    fn every_branch_ends_in_a_period_with_no_empty_clause() {
        // The 0-anchor branch was the only one with a punctuation assertion,
        // and it is the branch a player will almost never see. These are the
        // common ones.
        for kinds in [
            vec![AnchorKind::Ground],
            vec![AnchorKind::Ground, AnchorKind::Hearth],
            vec![AnchorKind::Ground, AnchorKind::Hearth, AnchorKind::Bed],
            vec![
                AnchorKind::Ground,
                AnchorKind::Hearth,
                AnchorKind::Bed,
                AnchorKind::Vessel,
            ],
        ] {
            let text = describe_chamber(&interior_with(&kinds), &brief());
            assert!(text.ends_with('.'), "not punctuated: {text:?}");
            assert!(!text.contains("  "), "double space: {text:?}");
            assert!(
                !text.contains(" ."),
                "empty clause before a period: {text:?}"
            );
            assert!(!text.contains(",."), "dangling comma: {text:?}");
        }
    }

    #[test]
    fn prose_is_a_pure_function_of_the_interior_and_brief() {
        let i = interior_with(&[AnchorKind::Ground, AnchorKind::Hearth]);
        assert_eq!(
            describe_chamber(&i, &brief()),
            describe_chamber(&i, &brief())
        );
    }

    #[test]
    fn the_brief_changes_the_word_for_the_place() {
        // `brief` must be READ, not merely carried: a built place is a room,
        // an unbuilt one is a hollow.
        let i = interior_with(&[AnchorKind::Ground, AnchorKind::Hearth]);
        let wild = Brief::from_parts(None, None, None, None, false, true);
        assert_ne!(describe_chamber(&i, &brief()), describe_chamber(&i, &wild));
        assert!(describe_chamber(&i, &wild).contains("hollow"));
    }
}
