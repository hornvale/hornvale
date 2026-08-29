//! Prose for a CHAMBER. Deliberately not `windows/locale`'s describer: that one
//! is written for ~1.7 km places and, verified at depth 21, reports biome,
//! elevation and a terrain micro-regime — it would describe a dwelling's room
//! as seafloor. Prose is the constitutionally primary surface (§3.5), so a
//! chamber gets its own prose built from what the chamber actually holds.

use crate::brief::Brief;
use crate::interior::{AnchorId, AnchorKind, Interior};

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

/// The nouns of every anchor lying directly `within` `id` (spec §3.6's
/// `Ntpp`), in the interior's own deterministic anchor order — a plain read
/// of [`crate::interior::Anchor::within`], not a derivation.
fn nouns_within(interior: &Interior, id: AnchorId) -> Vec<&'static str> {
    interior
        .ids()
        .into_iter()
        .filter(|&other| interior.anchor(other).within == Some(id))
        .filter_map(|other| noun(interior.anchor(other).kind))
        .collect()
}

/// `examine`'s answer for the anchor `id` (The Offer, Task 6, spec §3.6,
/// amended): [`detail`]'s sentence, plus what lies within it when `kind`
/// carries [`crate::affordance::ObjectProperty::Encloses`] — the
/// interactive-fiction rule that contents show when a container is open or
/// transparent. IV.a has no closed state to gate on (there is no way to
/// close anything), so every `Encloses` carrier reveals unconditionally;
/// what stays silent is a carrier with nothing `within` it, never the
/// property itself.
///
/// **Gated on `Encloses`, not merely on `within` being non-empty.** An
/// anchor kind that does not advertise `encloses` must stay silent even if
/// something happened to sit `within` it — otherwise this would degrade
/// into "reveal whatever `Interior` composed" rather than "reveal what the
/// property promises". Nothing in today's grammar ever attaches anything
/// within a non-`Encloses` kind (the only authored `Attach::Within` in the
/// whole pattern inventory is the fire within the alcove), so this gate is
/// unreachable from production data — exactly why
/// `a_non_enclosing_anchor_never_reports_contents_even_if_something_sits_
/// within_it` below builds one by hand.
pub(crate) fn examine_detail(interior: &Interior, id: AnchorId) -> String {
    let kind = interior.anchor(id).kind;
    let base = detail(kind);
    if !crate::affordance::encloses(crate::affordance::thing_kind_of(kind)) {
        return base.to_string();
    }
    let contents = nouns_within(interior, id);
    if contents.is_empty() {
        return base.to_string();
    }
    let (last, rest) = contents.split_last().expect("checked non-empty above");
    let listed = if rest.is_empty() {
        (*last).to_string()
    } else {
        format!("{} and {}", rest.join(", "), last)
    };
    format!("{base} Within it: {listed}.")
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

    /// Every kind, listed once — [`AnchorKind::ALL`], which is generated from
    /// the enum's own declaration (`interior/anchor.rs`).
    ///
    /// **This used to be a hand-written `[AnchorKind; 14]` here**, whose
    /// comment claimed it was "kept in step by [`detail`]'s exhaustive match".
    /// It was not: the compiler forces an ARM in `detail`, never an ENTRY in a
    /// list beside it, so an appended variant would have compiled with this
    /// list unchanged and every test below would have swept one kind short and
    /// stayed green. The Chattel's Task 9 fix round measured exactly that on
    /// the sibling rosters. Now there is one roster and it grows with the enum.
    const EVERY_KIND: &[AnchorKind] = AnchorKind::ALL;

    #[test]
    fn every_kind_has_a_detail() {
        // The roster grows with the enum, so this sweep cannot silently narrow;
        // what it still adds is that `detail` answers for every kind and that
        // `noun`'s own match distinguishes as many as the arithmetic below says.
        let mut seen = std::collections::BTreeSet::new();
        for &kind in EVERY_KIND {
            assert!(seen.insert(kind), "{kind:?} listed twice");
            let d = detail(kind);
            assert!(d.ends_with('.'), "{kind:?}: a detail is a sentence: {d:?}");
            assert!(!d.trim().is_empty(), "{kind:?}: an empty detail");
        }
        // Ground has no noun and every other kind does, so fourteen kinds must
        // yield thirteen nouns. The roster can no longer go short, so what this
        // now catches is the other direction: an APPENDED variant reddens here
        // rather than sliding through `noun`'s new arm unremarked.
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
        for &kind in EVERY_KIND {
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

    // --- Task 6: `encloses`, revealed on `examine` (spec §3.6, amended) ---

    /// The real case Task 6's census found: `the-fire`'s only authored
    /// `Attach::Within` target is `the-alcove`, and it is the sole `within`
    /// relation the whole grammar ever produces (`{(Hearth, Alcove)}` across
    /// all 60 production gate combinations, `task-6-report.md`). Examining
    /// the alcove must name the hearth.
    ///
    /// Mutation this must catch: return [`detail`]'s base sentence
    /// unconditionally, dropping the `within` read entirely. Run below.
    #[test]
    fn examining_an_alcove_names_the_hearth_within_it() {
        let mut i = Interior::new();
        let alcove = i.push(AnchorKind::Alcove, None);
        i.push(AnchorKind::Hearth, Some(alcove));

        let text = examine_detail(&i, alcove);
        assert!(
            text.starts_with(detail(AnchorKind::Alcove)),
            "the base detail must survive: {text:?}"
        );
        assert!(
            text.contains("a hearth"),
            "an alcove enclosing a hearth must name it: {text:?}"
        );
    }

    /// The other direction of the SAME case: the hearth does not enclose the
    /// alcove just because the alcove encloses the hearth — `within` is not
    /// symmetric, and `Hearth` carries no `Encloses` property at all.
    #[test]
    fn the_enclosed_anchor_does_not_report_its_own_container() {
        let mut i = Interior::new();
        let alcove = i.push(AnchorKind::Alcove, None);
        let hearth = i.push(AnchorKind::Hearth, Some(alcove));

        assert_eq!(
            examine_detail(&i, hearth),
            detail(AnchorKind::Hearth),
            "a hearth does not carry `encloses`, so examining it must be unchanged"
        );
    }

    /// The live silent case: `Strongbox` carries `encloses` (`affordance::
    /// object_registry`) but the grammar never places anything within one —
    /// `the-strongbox` is `Attach::Beside(Vessel)`, a sibling, never a
    /// container (Task 6 investigation, confirmed structurally and by
    /// census). Examining it must not fabricate contents.
    ///
    /// Mutation this must catch: report every OTHER anchor in the interior
    /// instead of consulting `within` (the mutation named in the task
    /// brief). A bare `Strongbox` with unrelated siblings in the same
    /// interior is the sharpest test of that: a naive "list the rest of the
    /// room" implementation would name them; this must not.
    #[test]
    fn an_empty_strongbox_reports_no_contents() {
        let mut i = Interior::new();
        let strongbox = i.push(AnchorKind::Strongbox, None);
        i.push(AnchorKind::Vessel, None); // a sibling, not a contained anchor

        assert_eq!(
            examine_detail(&i, strongbox),
            detail(AnchorKind::Strongbox),
            "an anchor with nothing `within` it must not fabricate contents"
        );
    }

    /// Direction 3 of the task brief: a NON-enclosing anchor must report
    /// nothing extra even if something happens to sit `within` it. Nothing
    /// in the authored grammar ever attaches anything within a `Bed` — this
    /// is deliberately hand-built, per the task brief's own instruction,
    /// because production data cannot reach this branch.
    ///
    /// Mutation this must catch: gate on `within` being non-empty instead of
    /// on [`crate::affordance::ObjectProperty::Encloses`] — i.e. drop the
    /// `crate::affordance::encloses(kind)` check from `examine_detail`
    /// entirely. Run below.
    #[test]
    fn a_non_enclosing_anchor_never_reports_contents_even_if_something_sits_within_it() {
        assert!(
            !crate::affordance::encloses(crate::affordance::thing_kind_of(AnchorKind::Bed)),
            "precondition: Bed must not carry Encloses, or this test proves nothing"
        );
        let mut i = Interior::new();
        let bed = i.push(AnchorKind::Bed, None);
        i.push(AnchorKind::Hearth, Some(bed));

        assert_eq!(
            examine_detail(&i, bed),
            detail(AnchorKind::Bed),
            "a non-enclosing kind must stay silent about what sits `within` it"
        );
    }

    /// Bridges the hand-built fixtures above to the real grammar: the same
    /// `interior_of` production path Task 6's census ran
    /// (`built && cold`, locale band), composing through the real
    /// `INVENTORY`/`compose` rather than a fixture. Confirms the alcove that
    /// composition draws is the same one `examine_detail` reports through.
    #[test]
    fn the_real_locale_grammar_composes_a_hearth_within_an_alcove_and_examine_names_it() {
        struct ColdBuilt;
        impl crate::liveness::Terrain for ColdBuilt {
            fn elevation(&self, _r: &hornvale_kernel::Facet) -> f64 {
                0.0
            }
            fn is_fresh_water(&self, _r: &hornvale_kernel::Facet) -> bool {
                false
            }
            fn temperature(
                &self,
                _r: &hornvale_kernel::Facet,
                _d: hornvale_kernel::WorldTime,
            ) -> f64 {
                -20.0
            }
            fn is_built(&self, _r: &hornvale_kernel::Facet) -> bool {
                true
            }
        }
        let room = hornvale_kernel::Facet {
            face: 0,
            path: Vec::new(),
        };
        let interior = crate::interior::interior_of(&room, &ColdBuilt);
        let alcove = interior
            .ids()
            .into_iter()
            .find(|&id| interior.anchor(id).kind == AnchorKind::Alcove)
            .expect("built && cold draws an alcove — the-fire's own precondition");

        let text = examine_detail(&interior, alcove);
        assert!(
            text.contains("a hearth"),
            "the real grammar's only within-relation must be named: {text:?}"
        );
    }
}
