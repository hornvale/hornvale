//! Creature identity (The Legend, Task 9): [`creature_glyph`] draws a
//! creature as its own noun's initial, deterministically, with no authored
//! species table. See `src/lexicon.rs`'s module doc for why the audit's 2.1
//! item ("a creature and a boulder are the same character, on every seed,
//! with no flag that changes it") is closed by this function.
//!
//! **Fix round 2: stable letters, collisions tolerated.** The original
//! version of this function took a `rank` derived from every OTHER creature
//! sharing the render, so the same species could draw a different letter
//! turn to turn depending purely on who else was in view — a per-frame slot
//! number, not identity (see `src/lexicon.rs`'s module doc, "Stable letters,
//! collisions tolerated"). This file's tests below assert the property
//! Nathan chose instead: a creature ALWAYS draws its own initial, full stop,
//! and same-initial species are deliberately left ambiguous — the cursor,
//! not the glyph, disambiguates them. Do not reintroduce a rank parameter
//! to chase distinctness; that is the defect this fix round removed.

use hornvale_game_core::{
    CandidateSource,
    Chart,
    ChartCell, // lexicon: `ChartCell` is the wire's own frozen type name for a chart room — an area, not a vertex
    ChartMarks,
    Mark,
    Micro,
    Spatial,
    creature_glyph,
};

#[test]
fn a_goblin_and_a_bugbear_draw_differently() {
    assert_ne!(creature_glyph("goblin"), creature_glyph("bugbear"));
}

#[test]
fn the_glyph_is_the_nouns_own_initial() {
    // IDENTITY: the character spells the thing, which is why it needs no
    // legend.
    assert_eq!(creature_glyph("goblin"), 'g');
    assert_eq!(creature_glyph("bugbear"), 'b');
}

#[test]
fn a_fifteenth_people_needs_no_edit() {
    // The Radiation moved the roster 9 -> 15 on 2026-08-27. An authored
    // table would have gone stale that day (MAP-derivation-outlives-its-
    // wiki).
    assert!(creature_glyph("an-unheard-of-people").is_ascii_alphabetic());
}

#[test]
fn a_noun_with_no_ascii_initial_still_draws_something() {
    // Infallible by design: a render must not panic on a document that
    // parsed — the rule `chart.rs`'s `weight_of` already follows.
    assert!(!creature_glyph("\u{4e2d}\u{6587}").is_whitespace());
}

#[test]
fn the_glyph_is_stable_regardless_of_who_else_is_present() {
    // The property this fix round exists to pin: a noun's glyph is a pure
    // function of the noun alone, so a goblin drawn alone and a goblin
    // drawn next to a gargoyle, a gnoll, or a whole 12-noun roster all draw
    // the identical letter. This is the SHARPER form of the order-
    // sensitivity concern fix round 1 flagged as a "soft guarantee" — it is
    // now a hard one, because there is no render-scoped state left to be
    // sensitive to.
    let alone = creature_glyph("goblin");
    // Simulate "other creatures in view" by simply calling the function
    // with unrelated nouns in between — there is no shared state for order
    // to perturb, which is the whole point.
    let _ = creature_glyph("gargoyle");
    let _ = creature_glyph("gnoll");
    let _ = creature_glyph("giant elk");
    let with_company = creature_glyph("goblin");
    assert_eq!(
        alone, with_company,
        "a goblin's glyph must not change because a gargoyle is also on screen"
    );
}

#[test]
fn same_initial_species_deliberately_share_a_glyph() {
    // Nathan's ruling: same-initial species are ambiguous on the grid, and
    // the cursor carries the disambiguating detail, not the glyph — ADoM's
    // own convention. goblin and gnoll both open on `g` and must now draw
    // IDENTICALLY, not distinctly.
    assert_eq!(creature_glyph("goblin"), creature_glyph("gnoll"));
    assert_eq!(creature_glyph("goblin"), 'g');
    assert_eq!(creature_glyph("gnoll"), 'g');
}

#[test]
fn the_real_twelve_noun_roster_from_task_5_is_stable_not_distinct() {
    // The same 12-noun roster fix round 1 resolved to 12 pairwise-distinct
    // glyphs via a render-scoped rank. That property is gone on purpose:
    // this roster's four collision groups (`g`x3, `d`x2, `h`x2, `k`x2) must
    // each collapse to one shared letter per group, and every noun's own
    // glyph must equal its own first letter, independent of the roster.
    let roster = [
        "goblin",
        "gnoll",
        "giant elk",
        "hobgoblin",
        "human",
        "drow",
        "dire wolf",
        "kobold",
        "treant",
        "owlbear",
        "bugbear",
        "killer whale",
    ];
    for noun in roster {
        let own_initial = noun.chars().find(char::is_ascii_alphabetic).unwrap();
        assert_eq!(
            creature_glyph(noun),
            own_initial.to_ascii_lowercase(),
            "{noun} must draw its own initial regardless of roster company"
        );
    }
    // The four collision groups now collide, deliberately.
    assert_eq!(creature_glyph("goblin"), creature_glyph("gnoll"));
    assert_eq!(creature_glyph("goblin"), creature_glyph("giant elk"));
    assert_eq!(creature_glyph("drow"), creature_glyph("dire wolf"));
    assert_eq!(creature_glyph("hobgoblin"), creature_glyph("human"));
    assert_eq!(creature_glyph("kobold"), creature_glyph("killer whale"));
}

/// The chart's second completion scope (The Newel, Task 4): a settlement or
/// cave mark drawn on the walk-band chart, gated by discovery so that
/// widening completion to the map does not also widen it into a way to
/// read the map before exploring it (decision 0670: a placed site's glyph
/// draws ungated, its proper name does not).
mod chart_marks {
    use super::*;

    // An alias so the rest of this module can say "room" instead of the
    // wire's own frozen type name.
    type Room = ChartCell; // lexicon: an area, not a vertex

    /// A minimal walk-band chart room carrying exactly one mark.
    fn room_with_mark(room: u64, noun: &str, kind: &str) -> Room {
        Room {
            room,
            u: Some(0),
            v: Some(0),
            w: None,
            up: None,
            seam: false,
            state: "sensed".to_string(),
            biome: 0,
            water: 0,
            relief: 0,
            color: None,
            micro: Micro {
                relief: 0.0,
                aspect: 0.0,
                wetness: 0.0,
                openness: 1.0,
            },
            marks: vec![Mark {
                noun: noun.to_string(),
                kind: kind.to_string(),
                datum: format!("{noun} — a {kind} of this world."),
                salience: 10,
            }],
            bearing_deg: 0.0,
            distance_rad: 0.0,
        }
    }

    /// A one-room walk-band chart whose only room carries one settlement
    /// mark, at packed room id `room`.
    fn spatial_with_a_marked_settlement(room: u64, noun: &str) -> Spatial {
        Spatial::Walk {
            chart: Chart {
                radius: 1,
                depth: 12,
                biome_legend: vec![],
                water_legend: vec![],
                relief_legend: vec![],
                cells: vec![room_with_mark(room, noun, "settlement")], // lexicon: `Chart::cells` is the wire's own frozen field name — an area, not a vertex
                legend: vec![],
                sight: None,
            },
        }
    }

    /// Decision 0670: a placed site's glyph is drawn ungated; its proper
    /// name is withheld until discovery. Completion is a name surface, so
    /// it takes the same gate. Written before the source that needs it —
    /// this is the campaign's one silent-failure risk (task brief, Step
    /// 1): widening completion to chart marks with no gate at all would
    /// leave the whole suite green while Tab read the map for you.
    #[test]
    fn an_undiscovered_settlements_name_is_not_offered_as_a_completion() {
        let spatial = spatial_with_a_marked_settlement(42, "Nenotata");
        let mut scope = ChartMarks::default();
        scope.update(&spatial, |_kind, _room| false);
        let candidates = scope.candidates();
        let names: Vec<&str> = candidates.iter().map(|c| c.name.as_str()).collect();
        assert!(
            !names.contains(&"Nenotata"),
            "completion offered a settlement the player has not discovered: {names:?}"
        );
    }

    /// The positive control the negative test above needs to not be
    /// satisfiable by offering nothing at all (task brief, Step 5): a
    /// discovered settlement's name DOES complete.
    #[test]
    fn a_discovered_settlements_name_is_offered_as_a_completion() {
        let spatial = spatial_with_a_marked_settlement(42, "Nenotata");
        let mut scope = ChartMarks::default();
        scope.update(&spatial, |_kind, _room| true);
        let candidates = scope.candidates();
        let names: Vec<&str> = candidates.iter().map(|c| c.name.as_str()).collect();
        assert!(
            names.contains(&"Nenotata"),
            "a discovered settlement's name must complete: {names:?}"
        );
    }

    /// An `"agent"` mark (a live creature visible on the chart, not a
    /// placed site) is never gated by decision 0670 — it draws the same
    /// way `CurrentTurnNouns` already offers a creature in the current
    /// room, and `is_discovered` is never even consulted for it here (the
    /// closure would panic on any call; it never runs).
    #[test]
    fn an_agent_mark_completes_with_no_discovery_check_at_all() {
        let spatial = Spatial::Walk {
            chart: Chart {
                radius: 1,
                depth: 12,
                biome_legend: vec![],
                water_legend: vec![],
                relief_legend: vec![],
                cells: vec![room_with_mark(7, "Dvoashngashngo", "agent")], // lexicon: `Chart::cells` is the wire's own frozen field name — an area, not a vertex
                legend: vec![],
                sight: None,
            },
        };
        let mut scope = ChartMarks::default();
        scope.update(&spatial, |_kind, _room| {
            panic!("an agent mark must never consult the discovery predicate")
        });
        let candidates = scope.candidates();
        let names: Vec<&str> = candidates.iter().map(|c| c.name.as_str()).collect();
        assert!(
            names.contains(&"Dvoashngashngo"),
            "an agent mark must complete unconditionally: {names:?}"
        );
    }

    /// **The `_` arm: an UNRECOGNISED mark kind is withheld until
    /// discovered.** The gate used to read `"settlement" | "cave" =>
    /// is_discovered(...), _ => true` — default-OPEN — which leaked nothing
    /// only because the producer emits exactly those two kinds plus
    /// `"agent"`. `windows/scene/src/surrounds.rs` explicitly anticipates
    /// further kinds, so that safety was a property of a crate one boundary
    /// away rather than of this function. Nothing covered the `_` arm at
    /// all, which is why the inversion was invisible: the two arms with
    /// tests are the two arms that were already right.
    ///
    /// `"shrine"` is a stand-in for any kind this crate has not been taught
    /// — deliberately not one the producer emits today, since the whole
    /// claim is about kinds it does not yet emit.
    #[test]
    fn an_unrecognised_mark_kind_is_withheld_until_discovered() {
        let unknown = |room: u64| Spatial::Walk {
            chart: Chart {
                radius: 1,
                depth: 12,
                biome_legend: vec![],
                water_legend: vec![],
                relief_legend: vec![],
                cells: vec![room_with_mark(room, "Nenotata", "shrine")], // lexicon: `Chart::cells` is the wire's own frozen field name — an area, not a vertex
                legend: vec![],
                sight: None,
            },
        };

        let mut scope = ChartMarks::default();
        scope.update(&unknown(42), |_kind, _room| false);
        let names: Vec<String> = scope.candidates().iter().map(|c| c.name.clone()).collect();
        assert!(
            !names.iter().any(|n| n == "Nenotata"),
            "an unknown placed-site kind completed while undiscovered — the gate is \
             default-open again: {names:?}"
        );

        // The positive control, without which the assertion above is
        // satisfiable by dropping unknown kinds on the floor entirely.
        let mut scope = ChartMarks::default();
        scope.update(&unknown(42), |_kind, _room| true);
        let names: Vec<String> = scope.candidates().iter().map(|c| c.name.clone()).collect();
        assert!(
            names.iter().any(|n| n == "Nenotata"),
            "a DISCOVERED mark of an unknown kind must still complete: {names:?}"
        );
    }

    /// A non-walk band (chamber or underground) has no chart, so the scope
    /// clears to empty rather than holding a stale walk-band roster.
    #[test]
    fn a_non_walk_band_clears_the_scope() {
        let mut scope = ChartMarks::default();
        scope.update(
            &Spatial::Walk {
                chart: Chart {
                    radius: 1,
                    depth: 12,
                    biome_legend: vec![],
                    water_legend: vec![],
                    relief_legend: vec![],
                    cells: vec![room_with_mark(1, "Nenotata", "settlement")], // lexicon: `Chart::cells` is the wire's own frozen field name — an area, not a vertex
                    legend: vec![],
                    sight: None,
                },
            },
            |_kind, _room| true,
        );
        assert!(
            !scope.candidates().is_empty(),
            "sanity: the walk band populated the scope"
        );

        scope.update(
            &Spatial::Chamber {
                plan: hornvale_game_core::Plan {
                    extent: hornvale_game_core::PlanExtent {
                        x: 0,
                        y: 0,
                        w: 1,
                        h: 1,
                    },
                    palette: vec![],
                    cells: vec![0], // lexicon: `Plan::cells` is the wire's own frozen field name — an area, not a vertex
                    you: hornvale_game_core::PlanPoint { x: 0, y: 0 },
                    marks: vec![],
                },
            },
            |_kind, _room| true,
        );
        assert!(
            scope.candidates().is_empty(),
            "a chamber-band snapshot must clear the walk-band roster, not leave it stale"
        );
    }
}
