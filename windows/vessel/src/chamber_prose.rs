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
        AnchorKind::Strongbox => Some("a strongbox"),
        AnchorKind::HighSeat => Some("a high seat"),
        AnchorKind::Loom => Some("a loom"),
        AnchorKind::Anvil => Some("an anvil"),
        AnchorKind::Altar => Some("an altar"),
    }
}

/// One authored line per kind: what a closer look at this thing gives you.
///
/// Exhaustive on purpose, with no catch-all arm. A new `AnchorKind` fails to
/// compile here until someone writes what it looks like, which is the guard that
/// stopped `look` and `examine` disagreeing in The Lintel — and Task 6 will make
/// it fire again on the kinds the chamber roles need.
///
/// `Ground` has no NOUN (it is the chamber's own floor, not a thing standing in
/// it) but it does have a detail: the render's legend names `the floor`, and §6
/// requires every noun the plan depicts to answer. So this match is total over
/// kinds where [`noun`] is not.
///
/// Kept short, concrete and free of terrain words —
/// `a_chamber_never_speaks_of_terrain` is already a test about prose, and
/// `no_detail_speaks_of_terrain` is its counterpart here, because a detail line
/// is read in the same room by the same player.
///
/// (No `type-audit:` tag: the extractor only reads bare-`pub` items
/// (`tools/type-audit/src/extract.rs`), so a tag here would be a verdict the tool
/// never gave — the same reason `noun` and `chamber_nouns` above carry none.)
pub(crate) fn detail(kind: AnchorKind) -> &'static str {
    match kind {
        AnchorKind::Ground => "Trodden floor, swept toward the walls.",
        AnchorKind::Hearth => "Stones set in a ring, and the ash inside them still warm.",
        AnchorKind::Threshold => "A gap left in the wall, worn smooth at the jamb.",
        AnchorKind::Bed => "A low frame, strung across and piled with what was to hand.",
        AnchorKind::Vessel => "A wide-mouthed jar, cool to the touch, standing half full.",
        AnchorKind::Screen => "A standing panel, set to break the line of sight.",
        AnchorKind::Alcove => "A recess cut back from the main space, deep enough to sit in.",
        AnchorKind::Pool => "Still water, holding the light that reaches it.",
        AnchorKind::Log => "A fallen trunk, its bark sloughing where the damp got in.",
        AnchorKind::Strongbox => {
            "A banded chest, low and heavier than it looks, its lid seated flush."
        }
        AnchorKind::HighSeat => {
            "A carved chair, set so that whoever sits in it sees the door first."
        }
        AnchorKind::Loom => {
            "An upright frame, its warp weighted, a hand's width of cloth grown up it."
        }
        AnchorKind::Anvil => "A block of iron on a sunk stump, bright where the work lands.",
        AnchorKind::Altar => "A low stone table, worn hollow at the centre and darkly stained.",
    }
}

/// What a closer look at a drawn WALL gives you.
///
/// A wall is not an anchor — nothing in the interior graph is one — but the plan
/// depicts it and names it, so §6 obliges it to answer. The line says what a wall
/// MEANS in this world as well as what it looks like: a wall is a CELL of the
/// building's fabric, impassable by §7 rule 2, so "no gap wide enough to pass" is
/// the wall law spoken in the world's own voice rather than a decorative sentence.
///
/// It also reads correctly of a thing with THICKNESS, which Task 4b's reification
/// gave it — "set close and plumb" is masonry, not a zero-width boundary — so the
/// sentence needed no change when the model did. Recorded because the reverse
/// would have been a §6 drift, and a sentence that survives a model change by
/// luck is worth noticing before the next one.
const WALL_DETAIL: &str = "Set close and plumb, with no gap in it wide enough to pass.";

/// The detail behind a noun the RENDER's legend names, if it is one.
///
/// The plan depicts three things and only one of them is an anchor. `the floor`
/// and `a wall` are real features of a room, and a player who reads a floor plan
/// and types `examine wall` is owed an answer — so they answer here rather than
/// being excluded from the legend, which would leave the plan's picture depicting
/// two things it refuses to discuss.
///
/// Matched against the render's OWN constants, so the legend and this lookup
/// cannot drift into two vocabularies for one picture.
pub(crate) fn glyph_detail(noun: &str) -> Option<&'static str> {
    use crate::lattice::render::{DOORWAY_NOUN, FLOOR_NOUN, WALL_NOUN};
    if noun == FLOOR_NOUN {
        Some(detail(AnchorKind::Ground))
    } else if noun == WALL_NOUN {
        Some(WALL_DETAIL)
    } else if noun == DOORWAY_NOUN {
        Some(detail(AnchorKind::Threshold))
    } else {
        None
    }
}

/// Every noun a chamber's prose will name, in the interior's own deterministic
/// anchor order. The ONE catalogue `describe_chamber` renders from, so the
/// nouns a chamber says are exactly the nouns it holds (the same discipline The
/// Purview's chart follows in sharing the prose's nouns).
///
/// The session also consults it as a LENIENT fallback in `enter <named>`: a
/// prose noun is accepted only where the chamber has exactly one aperture.
/// Chambers of one structure differ as of The Blocking, but every chamber role's
/// prose names a doorway, so noun lists still cannot reliably tell two apertures
/// apart. Apertures themselves are named by
/// DIRECTION (`further in`), which is not a prose noun at all — so this
/// catalogue does not bound what the player may be asked to type, only what a
/// chamber's prose may say.
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
        Brief::from_parts(None, None, None, None, 0, true, true)
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

    /// Every kind, listed once. Written out rather than derived, and kept in
    /// step by [`detail`]'s exhaustive match: a new kind fails to compile there,
    /// and `every_kind_has_a_detail` below is what notices it missing here.
    const EVERY_KIND: [AnchorKind; 14] = [
        AnchorKind::Ground,
        AnchorKind::Hearth,
        AnchorKind::Threshold,
        AnchorKind::Bed,
        AnchorKind::Vessel,
        AnchorKind::Screen,
        AnchorKind::Alcove,
        AnchorKind::Pool,
        AnchorKind::Log,
        AnchorKind::Strongbox,
        AnchorKind::HighSeat,
        AnchorKind::Loom,
        AnchorKind::Anvil,
        AnchorKind::Altar,
    ];

    #[test]
    fn every_kind_has_a_detail() {
        // The list above cannot go stale silently: `detail` is exhaustive, so a
        // tenth kind compiles only once it is written there, and this asserts the
        // list here covers as many distinct kinds as `noun` distinguishes.
        let mut seen = std::collections::BTreeSet::new();
        for kind in EVERY_KIND {
            assert!(seen.insert(kind), "{kind:?} listed twice");
            let d = detail(kind);
            assert!(d.ends_with('.'), "{kind:?}: a detail is a sentence: {d:?}");
            assert!(!d.trim().is_empty(), "{kind:?}: an empty detail");
        }
        // Ground has no noun and every other kind does, so fourteen kinds must
        // yield thirteen nouns — the arithmetic that catches a kind dropped from
        // the list.
        assert_eq!(
            EVERY_KIND.iter().filter(|&&k| noun(k).is_some()).count(),
            13,
            "the kind list has drifted from `noun`'s own match"
        );
    }

    #[test]
    fn no_detail_speaks_of_terrain() {
        // `a_chamber_never_speaks_of_terrain`'s counterpart. A detail line is read
        // in the same room by the same player, so the locale describer's
        // vocabulary is as wrong here as it is in the prose.
        for kind in EVERY_KIND {
            for banned in [
                "biome",
                "elevation",
                "moisture",
                "regime",
                "sun-warmed",
                "shaded",
                "unremarkable ground",
                " dry",
            ] {
                assert!(
                    !detail(kind).contains(banned),
                    "{kind:?}'s detail leaked a terrain word {banned:?}: {:?}",
                    detail(kind)
                );
            }
        }
        assert!(!WALL_DETAIL.contains(" dry") && !WALL_DETAIL.contains("shaded"));
    }

    #[test]
    fn every_noun_the_plans_legend_names_has_a_detail() {
        // §6's contract at its narrowest: the render names three things, and all
        // three must answer. A legend entry with no detail is the plan depicting
        // something it refuses to discuss.
        for noun in [
            crate::lattice::render::FLOOR_NOUN,
            crate::lattice::render::WALL_NOUN,
            crate::lattice::render::DOORWAY_NOUN,
        ] {
            assert!(
                glyph_detail(noun).is_some(),
                "the plan's legend names {noun:?} and nothing answers for it"
            );
        }
        assert!(glyph_detail("a noun no plan draws").is_none());
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
        let wild = Brief::from_parts(None, None, None, None, 0, false, true);
        assert_ne!(describe_chamber(&i, &brief()), describe_chamber(&i, &wild));
        assert!(describe_chamber(&i, &wild).contains("hollow"));
    }
}
