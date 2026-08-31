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

use hornvale_game_core::creature_glyph;

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
