//! Prose for a CHAMBER. Deliberately not `windows/locale`'s describer: that one
//! is written for ~1.7 km places and, verified at depth 21, reports biome,
//! elevation and a terrain micro-regime — it would describe a dwelling's room
//! as seafloor. Prose is the constitutionally primary surface (§3.5), so a
//! chamber gets its own prose built from what the chamber actually holds.

use crate::brief::Brief;
use crate::interior::{AnchorId, Interior};
use hornvale_kernel::{ComponentStore, KindId};
use hornvale_thing::kinds;

/// What prose calls a kind, and what `examine` says about it.
///
/// **Two accessors read this table, and they take DIFFERENT key types — that
/// asymmetry is forced, not sloppy.** [`noun`] takes `&str`; [`detail`] takes
/// [`KindId`]. `KindId` holds a `&'static str`, but three of `noun`'s callers
/// read the ledger — [`hornvale_kernel::Ledger::kind_of`] returns
/// `Option<&str>` borrowed from a `String` in the fact store, a runtime slice
/// that cannot become a `KindId` without leaking — so `noun` must accept the
/// widest of the two and the anchor-side callers simply pass `kind.0`.
/// `detail`'s callers are all interior-side and already hold a real `KindId`,
/// so it keeps the typed parameter and the typo-safety that comes with it. A
/// reader who finds two accessors on one table with two key types will be
/// tempted to unify them — unifying downward (`detail` to `&str`) loses that
/// typo-safety, and unifying upward (`noun` to `KindId`) is impossible, since
/// a ledger-read label is never `'static`. Do not "fix" this; it is the
/// retired `noun_for_label`'s reason for having existed, folded into one
/// table instead of two.
/// type-audit: bare-ok(identifier-text: noun), bare-ok(prose: detail)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ChamberProse {
    /// How prose names this kind, or `None` for a kind that IS the room rather
    /// than a thing standing in it — the floor — or for a kind this module has
    /// authored no noun for (`cave-mouth`, which `passage.rs` mints and no
    /// room's grammar composes, has always got `None` here). `detail` is total
    /// where this is not, because the plan's legend requires every noun it
    /// depicts to answer and the floor is depicted.
    pub noun: Option<&'static str>,
    /// The line `examine` gives. Short, concrete, and free of terrain words —
    /// `no_detail_speaks_of_terrain` is the guard.
    pub detail: &'static str,
}

/// The chamber-prose rows: one per thing-kind the grammar may place.
///
/// Prose is presentation, so it lives in the window; `hornvale_thing`'s
/// `ThingTraits::display` is the domain's own label and stays there. Two
/// tables answering two questions, not one table answering both — the same
/// split The Chattel's Task 7 made when it deleted `ThingTraits::portable`
/// rather than letting two tables answer "may a body carry this".
///
/// **Replaces two 15-arm `match`es (The Wicket, Task 4).** Those were
/// exhaustive matches over a closed enum until Task 2, so a kind with no arm
/// failed to *compile*. A `KindId` is a label, so no match over it can be
/// exhaustive; this table plus `windows/vessel/tests/suite/kind_totality.rs`'s
/// two-way totality gate (spec §5.1's G-b/G-c,
/// `every_roster_kind_has_chamber_prose` and
/// `every_chamber_prose_row_is_a_roster_kind`) is what stands in for the
/// compiler now.
pub fn chamber_prose_registry() -> ComponentStore<KindId, ChamberProse> {
    [
        (
            KindId("alcove"),
            ChamberProse {
                noun: Some("an alcove"),
                detail: "A recess cut back from the main space, deep enough to sit in.",
            },
        ),
        (
            KindId("altar"),
            ChamberProse {
                noun: Some("an altar"),
                detail: "A low stone table, worn hollow at the centre and darkly stained.",
            },
        ),
        (
            KindId("anvil"),
            ChamberProse {
                noun: Some("an anvil"),
                detail: "A block of iron on a sunk stump, bright where the work lands.",
            },
        ),
        (
            KindId("bed"),
            ChamberProse {
                noun: Some("a bed"),
                detail: "A low frame, strung across and piled with what was to hand.",
            },
        ),
        (
            KindId("bench"),
            ChamberProse {
                noun: Some("a bench"),
                detail: "A long plank on sturdy legs, worn smooth where people gather.",
            },
        ),
        (
            KindId("brazier"),
            ChamberProse {
                noun: Some("a brazier"),
                detail: "A squat iron bowl on three legs, coals glowing low within it.",
            },
        ),
        (
            KindId("cave-mouth"),
            ChamberProse {
                noun: None,
                detail: "A rough gap torn in the rock, cold air moving through it.",
            },
        ),
        (
            KindId("door"),
            ChamberProse {
                noun: Some("a door"),
                detail: "A leaf of banded wood in the opening, hung to be shut.",
            },
        ),
        (
            KindId("ground"),
            ChamberProse {
                noun: None,
                detail: "Trodden floor, swept toward the walls.",
            },
        ),
        (
            KindId("hearth"),
            ChamberProse {
                noun: Some("a hearth"),
                detail: "Stones set in a ring, and the ash inside them still warm.",
            },
        ),
        (
            KindId("high-seat"),
            ChamberProse {
                noun: Some("a high seat"),
                detail: "A carved chair, set so that whoever sits in it sees the door first.",
            },
        ),
        (
            KindId("key"),
            ChamberProse {
                noun: Some("a key"),
                detail: "A short shank of worked iron, its ward cut in a single stepped notch.",
            },
        ),
        (
            KindId("log"),
            ChamberProse {
                noun: Some("a fallen log"),
                detail: "A fallen trunk, its bark sloughing where the damp got in.",
            },
        ),
        (
            KindId("loom"),
            ChamberProse {
                noun: Some("a loom"),
                detail: "An upright frame, its warp weighted, a hand's width of cloth grown up it.",
            },
        ),
        (
            KindId("pool"),
            ChamberProse {
                noun: Some("a still pool"),
                detail: "Still water, holding the light that reaches it.",
            },
        ),
        (
            KindId("screen"),
            ChamberProse {
                noun: Some("a screen"),
                detail: "A standing panel, set to break the line of sight.",
            },
        ),
        (
            KindId("strongbox"),
            ChamberProse {
                noun: Some("a strongbox"),
                detail: "A banded chest, low and heavier than it looks, its lid seated flush.",
            },
        ),
        (
            KindId("threshold"),
            ChamberProse {
                noun: Some("a doorway"),
                detail: "A gap left in the wall, worn smooth at the jamb.",
            },
        ),
        (
            KindId("vessel"),
            ChamberProse {
                noun: Some("a water jar"),
                detail: "A wide-mouthed jar, cool to the touch, standing half full.",
            },
        ),
    ]
    .into_iter()
    .collect()
}

/// The noun for a thing-kind, as prose says it. `ground` has no noun: it is
/// the chamber's own floor, not a thing standing in it, and neither does a
/// kind this module has authored no prose for.
///
/// **It takes a `&str`, not a [`hornvale_kernel::KindId`] — see
/// [`ChamberProse`]'s own doc for why the two accessors on this one table
/// take different key types.**
pub(crate) fn noun(kind: &str) -> Option<&'static str> {
    chamber_prose_registry()
        .get_by_label(kind)
        .and_then(|p| p.noun)
}

/// [`detail`] keyed on a runtime LABEL rather than a [`KindId`] — [`noun`]'s
/// asymmetry, arriving for the same reason and for the same caller.
///
/// A thing read back off a committed `instance-of` fact is a `&str` borrowed
/// from the ledger, and [`hornvale_kernel::KindId`] holds a `&'static str`, so
/// such a label cannot be turned into one at all. The Brattice's underground
/// `examine` (`session.rs`) resolves a dropped thing's kind exactly that way.
///
/// `Option`, where [`detail`] panics: a label off the ledger is a runtime
/// string that no roster guarantees, so there is nothing here for a totality
/// gate to make unreachable. The panic in [`detail`] stays, because a
/// [`KindId`] IS from the authored roster and a missing line there is the
/// quiet failure that function's own doc argues about.
pub(crate) fn detail_of_label(kind: &str) -> Option<&'static str> {
    chamber_prose_registry()
        .get_by_label(kind)
        .map(|p| p.detail)
}

/// A list of nouns as one prose fragment — "a key", or "a key and a loaf", or
/// "a key, a loaf and a lamp" — or `None` for an empty list.
///
/// The SAME formatter [`contents_of`] is made of, factored out so that what a
/// chest says it holds and what `carrying` says a body holds cannot drift
/// into two spellings of one list.
pub(crate) fn listed(items: &[&str]) -> Option<String> {
    let (last, rest) = items.split_last()?;
    Some(if rest.is_empty() {
        (*last).to_string()
    } else {
        format!("{} and {}", rest.join(", "), last)
    })
}

/// One authored line per kind: what a closer look at this thing gives you.
///
/// **It REFUSES rather than defaults, and that replaces a compiler guarantee
/// with a loud one (The Wicket, Task 2).** This was an exhaustive `match` over
/// a closed enum, so a kind with no line failed to *compile* — the guard that
/// stopped `look` and `examine` disagreeing in The Lintel. A `KindId` is a
/// label, so no lookup over it can be exhaustive; what the refusal below DOES
/// is therefore the whole design. It panics. A fallback default would put a
/// plausible sentence in a real room forever, which is the quiet failure the
/// enum never allowed.
/// `windows/vessel/tests/suite/kind_totality.rs`'s two-way totality gate
/// (spec §5.1's G-b/G-c) is what makes the panic unreachable from the
/// authored roster.
///
/// `ground` has no NOUN (it is the chamber's own floor, not a thing standing in
/// it) but it does have a detail: the render's legend names `the floor`, and §6
/// requires every noun the plan depicts to answer. So this is total over
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
pub(crate) fn detail(kind: KindId) -> &'static str {
    chamber_prose_registry()
        .get(&kind)
        .map(|p| p.detail)
        .unwrap_or_else(|| panic!("no chamber prose for kind {:?}", kind.0))
}

/// The nouns of every anchor lying directly `within` `id` (spec §3.6's
/// `Ntpp`), in the interior's own deterministic anchor order — a plain read
/// of [`crate::interior::Anchor::within`], not a derivation.
fn nouns_within(interior: &Interior, id: AnchorId) -> Vec<&'static str> {
    interior
        .ids()
        .into_iter()
        .filter(|&other| interior.anchor(other).within == Some(id))
        .filter_map(|other| noun(interior.anchor(other).kind.0))
        .collect()
}

/// A [`noun`] with its indefinite article taken off — `"a strongbox"` becomes
/// `"strongbox"`, so a caller can say `the {bare}` about a thing the player
/// has just named.
///
/// **Article surgery, kept to one function and to the two articles this
/// module actually authors.** Every string [`noun`] returns begins `"a "` or
/// `"an "`; nothing here is a mass noun, a plural or a proper name, so a
/// general article model would be machinery for cases that do not exist.
/// `the_articles_this_module_authors_are_the_two_it_strips` asserts that over
/// `hornvale_thing::THING_KINDS`, the roster itself — so a future kind whose
/// noun starts some other way reddens rather than being silently returned
/// whole. It swept a generated enum roster until The Wicket, and the roster
/// it sweeps now is the one Task 1 froze as an ordered set.
///
/// It returns the input UNCHANGED when neither article is present, rather
/// than panicking: a wrong article in a sentence is a cosmetic defect and a
/// panic mid-verb is not, and the test above is what actually holds the
/// invariant.
pub(crate) fn without_article(noun: &str) -> &str {
    noun.strip_prefix("an ")
        .or_else(|| noun.strip_prefix("a "))
        .unwrap_or(noun)
}

/// What lies directly within `id`, as one prose fragment, or `None` when
/// nothing does — the listing half of [`examine_detail`], factored out
/// because `open`'s own reply names the same contents in the same words.
///
/// One formatter, not two: an `open` that spelled its list differently from
/// `examine`'s would describe one chest two ways in consecutive turns, and
/// nothing but a reader's eye would object.
pub(crate) fn contents_of(interior: &Interior, id: AnchorId) -> Option<String> {
    listed(&nouns_within(interior, id))
}

/// `examine`'s answer for the anchor `id` (The Offer, Task 6, spec §3.6,
/// amended): [`detail`]'s sentence, plus what lies within it when `kind`
/// carries [`crate::affordance::ObjectProperty::Encloses`] — the
/// interactive-fiction rule that contents show when a container is open or
/// transparent.
///
/// # `opened` is the "or transparent" half, and it went live in Task 11
///
/// This doc used to end: *"IV.a has no closed state to gate on (there is no
/// way to close anything), so every `Encloses` carrier reveals
/// unconditionally."* That was exact when written and stopped being true the
/// moment `Openable`, `openness` and the `open`/`close` verbs shipped. The
/// rule the paragraph already cited has two arms and only one of them was
/// implemented; the other is now:
///
/// ```text
///   reveal contents  <=>  carries Encloses
///                    AND  (does NOT carry Openable  OR  opened)
/// ```
///
/// An `alcove` carries `Encloses` and not `Openable` — a recess has no lid —
/// so it is the *transparent* arm and still reveals unconditionally. A
/// `strongbox` carries both, so it is the *open* arm and keeps its key out of
/// sight until someone opens it. Both are pinned:
/// `an_open_container_reveals_its_contents_and_a_shut_one_does_not`.
///
/// **`opened` is the CALLER's answer, and the caller is where the ledger
/// is.** This function holds no `Ledger`, no day and no room, so it cannot
/// fold `thing::is_open` itself; `Session::examine_chamber` does that and
/// hands the boolean in. The value is IGNORED for a kind that carries no
/// `Openable`, which is why an interior-only test may pass either.
///
/// **What `false` means for a container nobody has touched.** [`OPENNESS`]'s
/// absence means "whatever the seed drew"
/// (`crate::thing::OPENNESS`), and no generator draws an openness for a
/// container — unlike a cave mouth, whose fallback is `barrier_of`. So the
/// authored default lives at the reader, in `Session::container_is_open`, and
/// it is *shut*: a chest standing in a room a player has never entered is
/// closed, which is what a banded chest with "its lid seated flush" already
/// says in its own [`detail`] line.
///
/// **Gated on `Encloses`, not merely on `within` being non-empty.** An
/// anchor kind that does not advertise `encloses` must stay silent even if
/// something happened to sit `within` it — otherwise this would degrade
/// into "reveal whatever `Interior` composed" rather than "reveal what the
/// property promises". The grammar now attaches things within two kinds
/// (`the-fire` within an alcove, `the-key-in-the-strongbox` within a
/// strongbox) and both carry `Encloses`, so this gate is still unreachable
/// from production data — exactly why
/// `a_non_enclosing_anchor_never_reports_contents_even_if_something_sits_
/// within_it` below builds one by hand.
/// type-audit: bare-ok(flag: opened)
pub(crate) fn examine_detail(interior: &Interior, id: AnchorId, opened: bool) -> String {
    let kind = interior.anchor(id).kind;
    let base = detail(kind);
    if !crate::affordance::encloses(kind) {
        return base.to_string();
    }
    if crate::affordance::carries(kind, crate::affordance::ObjectProperty::Openable) && !opened {
        return base.to_string();
    }
    match contents_of(interior, id) {
        None => base.to_string(),
        Some(listed) => format!("{base} Within it: {listed}."),
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
        Some(detail(kinds::GROUND))
    } else if noun == WALL_NOUN {
        Some(WALL_DETAIL)
    } else if noun == DOORWAY_NOUN {
        Some(detail(kinds::THRESHOLD))
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
        .filter_map(|&id| noun(interior.anchor(id).kind.0))
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
///
/// **Since The Prospect (Task 7) it is read for a second thing: the SITE's own
/// name.** `Brief::site`'s name is `Some` for a settlement and `None` for a
/// cave or an exotic site, so a named site places `in {name}` into the
/// opening noun phrase — "A small room in Doaba, holding a doorway and a
/// screen" — and an unnamed one renders exactly the sentence it always did.
/// The clause goes in the noun phrase rather than on a line of its own
/// because the name is a property of the room, not an event in it, and
/// because that placement holds in all three branches below without needing a
/// fourth.
///
/// **The name is the PLACE's.** It reaches here through `Site::name`, which
/// `brief_of` fills from `Terrain::settlement_name` — the room-keyed
/// settlement-territory map. This function is given no body, no session and
/// no possession, so it *cannot* accidentally name the creature's own home
/// village instead; that structural inability is the guarantee, not a
/// convention.
/// type-audit: bare-ok(prose: return)
pub fn describe_chamber(interior: &Interior, brief: &Brief) -> String {
    let place = if brief.built { "room" } else { "hollow" };
    // `in Doaba` where the site has a name, nothing where it has none — an
    // unnamed site must not be given a borrowed name or an empty clause.
    let named = brief
        .site
        .as_ref()
        .and_then(|site| site.name.as_deref())
        .map(|name| format!(" in {name}"))
        .unwrap_or_default();
    let nouns = chamber_nouns(interior);
    match nouns.len() {
        0 => format!("A bare {place}{named}, its floor swept and its corners empty."),
        1 => format!(
            "A small {place}{named}. {} stands here.",
            capitalize(nouns[0])
        ),
        _ => {
            let (last, rest) = nouns.split_last().expect("len >= 2");
            format!(
                "A small {place}{named}, holding {} and {}.",
                rest.join(", "),
                last
            )
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
    use crate::interior::Interior;
    use crate::site::{Site, SiteKind};

    fn brief() -> Brief {
        Brief::from_parts(
            None,
            None,
            None,
            None,
            None,
            0,
            true,
            true,
            Some(Site::placed(SiteKind::Settlement, None)),
            None,
        )
    }

    /// The same brief, with the settlement site NAMED — the shape `brief_of`
    /// produces at a real settlement since The Prospect's Task 7.
    fn named_brief(name: &str) -> Brief {
        Brief::from_parts(
            None,
            None,
            None,
            None,
            None,
            0,
            true,
            true,
            Some(Site::placed(SiteKind::Settlement, Some(name.to_string()))),
            None,
        )
    }

    /// A named site puts its name in the chamber's opening noun phrase, and
    /// an unnamed one renders the sentence it always did — no borrowed name
    /// and no empty clause (Task 7).
    ///
    /// Both directions in one test on purpose: the positive assertion alone
    /// would pass an implementation that hard-coded a clause for every
    /// chamber, and the negative alone would pass one that emitted nothing
    /// ever. The name is deliberately not "Doaba": this function is handed a
    /// `Brief` and nothing else, so a real settlement's name here would not
    /// distinguish "read from `Site::name`" from "read from anywhere at all".
    #[test]
    fn a_named_site_is_named_in_its_chambers_prose() {
        let interior = interior_with(&[KindId("hearth")]);

        let named = describe_chamber(&interior, &named_brief("Nornholm"));
        assert!(
            named.contains("in Nornholm"),
            "a named site must be named in the chamber it holds: {named}"
        );

        let anonymous = describe_chamber(&interior, &brief());
        // `!anonymous.contains(" in ")` used to stand here alone. That is a
        // substring test of ENGLISH, not of the clause `describe_chamber`
        // builds — it only ever held because no anchor noun in this
        // fixture's roster contains the two characters " in ", and a future
        // anchor that does (a "shrine" is fine; a "dining hall" is not)
        // would fail this assertion for a reason unrelated to the site's
        // name. Two narrower checks replace it: a SHAPE check that the
        // sentence has no in-clause at all between the room word and the
        // period that follows (`describe_chamber`'s own `" in {name}"` vs
        // `""` branch, `chamber_prose.rs`), and a check that the specific
        // borrowed name this test would catch does not appear.
        assert!(
            anonymous.starts_with("A small room. "),
            "an unnamed site must leave no in-clause between the room word \
             and the sentence that follows: {anonymous}"
        );
        assert!(
            !anonymous.contains(" in Nornholm"),
            "a site with no name must not acquire one: {anonymous}"
        );
    }

    /// The name reaches every branch of the noun phrase, not only the one a
    /// single fixture happens to exercise. `describe_chamber` has three
    /// arms — no anchors, one, and many — and the clause is placed once for
    /// all three; a per-arm `format!` is exactly the kind of edit that would
    /// drop it from the arm nobody tested.
    #[test]
    fn every_anchor_count_names_the_site() {
        let named = named_brief("Nornholm");
        for kinds in [
            &[][..],
            &[KindId("hearth")][..],
            &[KindId("hearth"), KindId("doorway")][..],
        ] {
            let text = describe_chamber(&interior_with(kinds), &named);
            assert!(
                text.contains("in Nornholm"),
                "the site's name must survive the {}-anchor branch: {text}",
                kinds.len()
            );
        }
    }

    fn interior_with(anchor_kinds: &[KindId]) -> Interior {
        let mut i = Interior::new();
        let mut prev = None;
        for &k in anchor_kinds {
            let id = i.push(k, None);
            if let Some(p) = prev {
                i.connect(p, id);
            }
            prev = Some(id);
        }
        i
    }

    /// **Every kind the authored grammar can place has a [`detail`] line** —
    /// swept over [`crate::interior::INVENTORY`] itself, never over a roster
    /// beside it.
    ///
    /// **This is the assertion that stopped existing when the enum did (The
    /// Wicket, Task 2), and its absence was a player-facing crash rather than
    /// a tidiness problem.** [`detail`] used to be an exhaustive `match` over a
    /// closed enum, so a pattern could only name a variant and every variant
    /// had to have an arm or the crate would not compile. A pattern now names
    /// a label, and [`detail`] ends in a wildcard that PANICS. That panic is
    /// reached from [`examine_detail`] and from `Session::sighting`'s
    /// `PlanMark.datum`, so an `INVENTORY` row whose kind has no arm is a
    /// runtime crash in a real chamber the first time anyone looks at the
    /// room.
    ///
    /// **Why the two sweeps beside it do not cover this.**
    /// [`every_kind_has_a_detail`] and
    /// [`the_articles_this_module_authors_are_the_two_it_strips`] run over
    /// `hornvale_thing::THING_KINDS` itself now (Task 4 deleted the
    /// hand-written `EVERY_KIND` they used to sweep), so neither can narrow
    /// independently of the roster any more. But a kind `INVENTORY` names
    /// that is absent from `THING_KINDS` entirely would still slip past both
    /// of them — that is `every_kind_the_grammar_names_is_a_roster_row`'s job
    /// (`windows/vessel/tests/suite/kind_totality.rs`, spec §5.1's G-a), not
    /// either sweep here. Sweeping `INVENTORY` directly is what makes this
    /// test unable to narrow regardless: the population is the authored
    /// grammar itself.
    ///
    /// It reads all three slots a pattern can name a kind in — the anchor it
    /// contributes, the kind it `requires`, and the
    /// `Attach::Beside`/`Attach::Within` target — because all three reach
    /// `Interior::push` or a `first_of` lookup, and a kind that reaches an
    /// interior reaches prose.
    ///
    /// **Task 4's G-a and G-b will imply this transitively and it stays
    /// anyway.** G-a says every `INVENTORY` kind is a `THING_KINDS` row and
    /// G-b says every `THING_KINDS` row has a chamber-prose row, so the
    /// composition gives this. "Transitively implied" is precisely the
    /// reasoning that goes wrong quietly — it survives one of the two links
    /// being narrowed, or scoped, or moved to a different roster — and the
    /// direct assertion costs one loop.
    ///
    /// **A missing arm arrives as [`detail`]'s own refusal, quoting the
    /// kind**, rather than as an assertion here. Pre-empting it would need a
    /// second copy of the arm list in this test, which is the duplicated
    /// table decision 0261 warns about and whose cheapest repair deletes the
    /// check.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: point `the-altar`'s `kind` in
    /// [`crate::interior::INVENTORY`] at `KindId("altar-stone")` — a kind
    /// with no [`detail`] arm. It compiles, `INVENTORY` keeps its length,
    /// every composition still validates, and `THING_KINDS` is untouched.
    ///
    /// **`the-altar` rather than a pattern anyone walks past, and the choice
    /// is what makes this evidence.** The obvious mutation — misspelling
    /// [`detail`]'s own `"vessel"` arm — was run first and reddened FOUR
    /// tests, this one plus [`every_kind_has_a_detail`],
    /// [`no_detail_speaks_of_terrain`] and a session-level golden, because
    /// `the-water-jar` is drawn in every built composition. A mutation caught
    /// several ways over says nothing about which test is holding the
    /// property. `the-altar` is `roles: &[Role::Shrine]`, and The Custodian
    /// measured `Role::Shrine` occurring **zero** times in any flagship a
    /// possession starts at, so no behavioural test renders a chamber holding
    /// one — which is exactly the shape of the gap this test exists for: a
    /// kind reachable by the GRAMMAR but by no test's transcript.
    ///
    /// Applied with `scripts/mutate.py`, run unfiltered over the crate,
    /// restored from a `cp` backup and re-run green afterwards. Red observed
    /// 2026-09-01 — **exactly one test failed**:
    ///
    /// ```text
    /// FAIL [   0.011s] ( 17/876) hornvale-vessel chamber_prose::tests::every_kind_the_grammar_places_has_a_detail
    /// thread 'chamber_prose::tests::every_kind_the_grammar_places_has_a_detail'
    /// panicked at windows/vessel/src/chamber_prose.rs:120:18:
    /// no chamber prose for kind "altar-stone"
    ///      Summary [ 190.620s] 876 tests run: 875 passed, 1 failed, 3 skipped
    /// ```
    #[test]
    fn every_kind_the_grammar_places_has_a_detail() {
        use crate::interior::{Attach, INVENTORY};

        let mut checked = 0usize;
        for p in INVENTORY.iter() {
            let mut named = vec![("kind", p.kind)];
            if let Some(r) = p.requires {
                named.push(("requires", r));
            }
            match p.attach {
                Attach::Beside(k) | Attach::Within(k) => named.push(("attach", k)),
                Attach::Hub => {}
            }
            for (slot, id) in named {
                // `detail` REFUSES rather than defaults, so a kind with no arm
                // panics HERE, naming itself. What this loop adds is the
                // population: the authored grammar, not a list beside it.
                let line = detail(id);
                assert!(
                    line.ends_with('.') && !line.trim().is_empty(),
                    "pattern {:?} names {:?} in its {slot}, whose detail is not \
                     a sentence: {line:?}",
                    p.name,
                    id.0
                );
                checked += 1;
            }
        }
        // Anti-vacuity, and the accounting a census owes: a future edit that
        // dropped a slot from the walk above would satisfy every assertion by
        // measuring less. 20 patterns, each naming a `kind`; 14 of them also
        // name a `requires`; 16 also name an `Attach` target. The Housemark's
        // three appended relations each name all three slots, moving this from
        // 41 to 50.
        assert_eq!(
            checked, 50,
            "the sweep no longer reads every kind INVENTORY names: {checked} \
             slots, not 50"
        );
    }

    #[test]
    fn every_kind_has_a_detail() {
        // Swept over `hornvale_thing::THING_KINDS` itself (Task 4 deleted the
        // hand-written `EVERY_KIND` beside it), so this sweep cannot silently
        // narrow independently of the roster; what it adds is that `detail`
        // answers for every declared kind and that `noun` distinguishes as
        // many as the arithmetic below says.
        let mut seen = std::collections::BTreeSet::new();
        for &label in hornvale_thing::THING_KINDS {
            assert!(seen.insert(label), "{label:?} listed twice");
            let d = detail(KindId(label));
            assert!(d.ends_with('.'), "{label:?}: a detail is a sentence: {d:?}");
            assert!(!d.trim().is_empty(), "{label:?}: an empty detail");
        }
        // `ground` and `cave-mouth` have no noun and every other roster kind
        // does, so nineteen kinds must yield seventeen nouns (The Wicket's Task
        // 5 appended `brazier`, moving this from 14 to 15; The Brattice's Task
        // 5 appended `door`, moving it to 16 — both have a noun). This
        // used to catch
        // an APPENDED enum variant on the run that first compiled it (that is
        // how The Chattel's `Key` was caught, going red at 13 against 14). It
        // cannot do that any more — the roster is a hand-written list itself
        // now — so what it holds today is the narrower claim that the roster
        // and `noun`'s table still agree.
        assert_eq!(
            hornvale_thing::THING_KINDS
                .iter()
                .filter(|&&label| noun(label).is_some())
                .count(),
            17,
            "the roster has drifted from `noun`'s own table"
        );
    }

    #[test]
    fn no_detail_speaks_of_terrain() {
        // `a_chamber_never_speaks_of_terrain`'s counterpart. A detail line is read
        // in the same room by the same player, so the locale describer's
        // vocabulary is as wrong here as it is in the prose. Swept over
        // `hornvale_thing::THING_KINDS` itself — see `every_kind_has_a_detail`'s
        // own comment for why that sweep can no longer narrow.
        for &label in hornvale_thing::THING_KINDS {
            let kind = KindId(label);
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
            &interior_with(&[kinds::GROUND, kinds::HEARTH, kinds::BED]),
            &brief(),
        );
        assert!(text.contains("hearth"), "got: {text}");
        assert!(text.contains("bed"), "got: {text}");
    }

    #[test]
    fn a_chamber_never_speaks_of_terrain() {
        let text = describe_chamber(&interior_with(&[kinds::GROUND, kinds::HEARTH]), &brief());
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
        let text = describe_chamber(&interior_with(&[kinds::GROUND]), &brief());
        assert!(!text.trim().is_empty());
        assert!(text.ends_with('.'), "prose is a sentence: {text}");
    }

    #[test]
    fn every_branch_ends_in_a_period_with_no_empty_clause() {
        // The 0-anchor branch was the only one with a punctuation assertion,
        // and it is the branch a player will almost never see. These are the
        // common ones.
        for kinds in [
            vec![kinds::GROUND],
            vec![kinds::GROUND, kinds::HEARTH],
            vec![kinds::GROUND, kinds::HEARTH, kinds::BED],
            vec![kinds::GROUND, kinds::HEARTH, kinds::BED, kinds::VESSEL],
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
        let i = interior_with(&[kinds::GROUND, kinds::HEARTH]);
        assert_eq!(
            describe_chamber(&i, &brief()),
            describe_chamber(&i, &brief())
        );
    }

    #[test]
    fn the_brief_changes_the_word_for_the_place() {
        // `brief` must be READ, not merely carried: a built place is a room,
        // an unbuilt one is a hollow.
        let i = interior_with(&[kinds::GROUND, kinds::HEARTH]);
        let wild = Brief::from_parts(None, None, None, None, None, 0, false, true, None, None);
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
        let alcove = i.push(kinds::ALCOVE, None);
        i.push(kinds::HEARTH, Some(alcove));

        // An alcove carries no `Openable`, so the flag is ignored — see
        // `examine_detail`'s two-arm rule. `false` is passed to make that
        // explicit: a recess has no lid to be shut.
        let text = examine_detail(&i, alcove, false);
        assert!(
            text.starts_with(detail(kinds::ALCOVE)),
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
        let alcove = i.push(kinds::ALCOVE, None);
        let hearth = i.push(kinds::HEARTH, Some(alcove));

        assert_eq!(
            examine_detail(&i, hearth, false),
            detail(kinds::HEARTH),
            "a hearth does not carry `encloses`, so examining it must be unchanged"
        );
    }

    /// An `Encloses` carrier with nothing inside it stays silent — for want
    /// of CONTENTS, never for want of the property.
    ///
    /// **This test's stated premise was falsified by the campaign that
    /// wrote it, and the correction is louder than a quiet edit because a
    /// reader reasons FROM a premise.** It read: *"the grammar never places
    /// anything within one — `the-strongbox` is `Attach::Beside(Vessel)`, a
    /// sibling, never a container (Task 6 investigation, confirmed
    /// structurally and by census)."* True when written and the reason
    /// spec §3.8 demanded contents be authored; Task 11 authored
    /// `the-key-in-the-strongbox`, so a production strongbox now holds a key
    /// and `the_grammar_puts_exactly_these_things_inside_other_things` reports
    /// `{(Alcove, Hearth), (Strongbox, Key)}`. What the test CHECKS is
    /// unchanged and still worth checking — the fixture below is empty by
    /// construction, whatever the grammar does elsewhere.
    ///
    /// It passes `opened = true` deliberately: with `false` the `Openable`
    /// arm would return the base sentence before the emptiness check ever
    /// ran, and the test would pass without exercising what it names. Open
    /// and empty is the state that actually discriminates.
    ///
    /// Mutation this must catch: report every OTHER anchor in the interior
    /// instead of consulting `within` (the mutation named in the task
    /// brief). A bare `Strongbox` with unrelated siblings in the same
    /// interior is the sharpest test of that: a naive "list the rest of the
    /// room" implementation would name them; this must not.
    #[test]
    fn an_empty_strongbox_reports_no_contents() {
        let mut i = Interior::new();
        let strongbox = i.push(kinds::STRONGBOX, None);
        i.push(kinds::VESSEL, None); // a sibling, not a contained anchor

        assert_eq!(
            examine_detail(&i, strongbox, true),
            detail(kinds::STRONGBOX),
            "an anchor with nothing `within` it must not fabricate contents"
        );
    }

    /// The `Openable` arm of the interactive-fiction rule, both directions:
    /// a shut chest keeps its contents out of sight and an open one shows
    /// them, while an `Encloses` carrier with no lid (`Alcove`) is unaffected
    /// by the flag entirely.
    ///
    /// **The third assertion is what stops this being a test of one boolean.**
    /// Without it, an implementation that gated EVERY `Encloses` carrier on
    /// `opened` would pass the first two — and it would silently shut the
    /// alcove, hiding the one `within` relation the grammar had before this
    /// campaign.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: drop the `Openable` clause from
    /// `examine_detail`'s second gate, i.e. `if carries(thing_kind,
    /// Openable) && !opened` -> `if false && !opened`. Confirmed 2026-08-29,
    /// unfiltered over the whole crate (`cargo nextest run -p hornvale-vessel
    /// --no-fail-fast`, `847 tests run: 846 passed, 1 failed` — this one and
    /// nothing else, which is what says the gate is held HERE rather than
    /// incidentally by some transcript golden):
    ///
    /// ```text
    /// assertion `left == right` failed: a shut chest must not name what is
    /// inside it
    ///   left: "A banded chest, low and heavier than it looks, its lid seated
    ///          flush. Within it: a key."
    ///  right: "A banded chest, low and heavier than it looks, its lid seated
    ///          flush."
    /// ```
    #[test]
    fn an_open_container_reveals_its_contents_and_a_shut_one_does_not() {
        let mut i = Interior::new();
        let strongbox = i.push(kinds::STRONGBOX, None);
        i.push(kinds::KEY, Some(strongbox));

        assert_eq!(
            examine_detail(&i, strongbox, false),
            detail(kinds::STRONGBOX),
            "a shut chest must not name what is inside it"
        );
        assert!(
            examine_detail(&i, strongbox, true).contains("a key"),
            "an open chest must name what is inside it: {:?}",
            examine_detail(&i, strongbox, true)
        );

        let mut a = Interior::new();
        let alcove = a.push(kinds::ALCOVE, None);
        a.push(kinds::HEARTH, Some(alcove));
        assert_eq!(
            examine_detail(&a, alcove, false),
            examine_detail(&a, alcove, true),
            "an Encloses carrier with no lid must ignore `opened` — a recess \
             is transparent, and gating it would hide the grammar's oldest \
             within-relation"
        );
    }

    /// Every noun this module authors opens with one of the two articles
    /// [`without_article`] strips, so `the {bare}` is well-formed for every
    /// anchor kind rather than for the ones someone happened to check.
    ///
    /// Swept over `hornvale_thing::THING_KINDS`, the roster itself (Task 4
    /// deleted the hand-written `EVERY_KIND` it used to sweep) — see
    /// [`every_kind_has_a_detail`]'s own comment for what the sweep can and
    /// can no longer catch since the anchor-kind enum was deleted.
    #[test]
    fn the_articles_this_module_authors_are_the_two_it_strips() {
        for &label in hornvale_thing::THING_KINDS {
            let Some(n) = noun(label) else { continue };
            assert!(
                n.starts_with("a ") || n.starts_with("an "),
                "{label:?}'s noun {n:?} carries neither article without_article \
                 knows, so `the {}` would read wrong",
                without_article(n)
            );
            assert_ne!(
                without_article(n),
                n,
                "{label:?}'s noun {n:?} lost no article"
            );
        }
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
            !crate::affordance::encloses(kinds::BED),
            "precondition: Bed must not carry Encloses, or this test proves nothing"
        );
        let mut i = Interior::new();
        let bed = i.push(kinds::BED, None);
        i.push(kinds::HEARTH, Some(bed));

        assert_eq!(
            examine_detail(&i, bed, false),
            detail(kinds::BED),
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
            .find(|&id| interior.anchor(id).kind == kinds::ALCOVE)
            .expect("built && cold draws an alcove — the-fire's own precondition");

        let text = examine_detail(&interior, alcove, false);
        assert!(
            text.contains("a hearth"),
            "the real grammar's only within-relation must be named: {text:?}"
        );
    }
}
