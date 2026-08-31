//! Creature identity (The Legend, Task 9): [`creature_glyph`] draws a
//! creature as its own noun's initial, deterministically, with no authored
//! species table. See `src/lexicon.rs`'s module doc for why the audit's 2.1
//! item ("a creature and a boulder are the same character, on every seed,
//! with no flag that changes it") is closed by this function.

use hornvale_game_core::creature_glyph;

#[test]
fn a_goblin_and_a_bugbear_draw_differently() {
    assert_ne!(creature_glyph("goblin", 0), creature_glyph("bugbear", 0));
}

#[test]
fn the_glyph_is_the_nouns_own_initial() {
    // IDENTITY: the character spells the thing, which is why it needs no
    // legend.
    assert_eq!(creature_glyph("goblin", 0), 'g');
    assert_eq!(creature_glyph("bugbear", 0), 'b');
}

#[test]
fn a_collision_resolves_deterministically_and_never_by_an_authored_table() {
    // goblin and gnoll both want `g`. Rank is registry order.
    let a = creature_glyph("goblin", 0);
    let b = creature_glyph("gnoll", 1);
    assert_ne!(a, b);
    assert_eq!(creature_glyph("gnoll", 1), b, "not deterministic");
}

#[test]
fn a_fifteenth_people_needs_no_edit() {
    // The Radiation moved the roster 9 -> 15 on 2026-08-27. An authored
    // table would have gone stale that day (MAP-derivation-outlives-its-
    // wiki).
    assert!(creature_glyph("an-unheard-of-people", 7).is_ascii_alphabetic());
}

#[test]
fn a_noun_with_no_ascii_initial_still_draws_something() {
    // Infallible by design: a render must not panic on a document that
    // parsed — the rule `chart.rs`'s `weight_of` already follows.
    assert!(!creature_glyph("\u{4e2d}\u{6587}", 0).is_whitespace());
}

#[test]
fn a_three_way_collision_on_the_same_initial_resolves_distinctly() {
    // The real load Task 5's specimen sheet measured on a 12-noun roster:
    // goblin, gnoll and giant elk all open on `g`.
    use hornvale_game_core::creature_ranks;
    let ranks = creature_ranks(["goblin", "gnoll", "giant elk"]);
    let g = |n: &str| creature_glyph(n, ranks[n]);
    let (a, b, c) = (g("goblin"), g("gnoll"), g("giant elk"));
    assert_ne!(a, b);
    assert_ne!(a, c);
    assert_ne!(b, c);
}

#[test]
fn creature_ranks_gives_the_first_alphabetical_noun_its_own_initial() {
    use hornvale_game_core::creature_ranks;
    let ranks = creature_ranks(["gnoll", "goblin"]);
    // Alphabetically, "gnoll" < "goblin", so "gnoll" claims rank 0 for `g`
    // and draws its own initial.
    assert_eq!(ranks["gnoll"], 0);
    assert_eq!(creature_glyph("gnoll", ranks["gnoll"]), 'g');
    assert_eq!(ranks["goblin"], 1);
}

#[test]
fn the_real_twelve_noun_roster_from_task_5_resolves_every_collision_group() {
    use hornvale_game_core::creature_ranks;
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
    let ranks = creature_ranks(roster);
    let glyphs: Vec<(&str, char)> = roster
        .iter()
        .map(|&n| (n, creature_glyph(n, ranks[n])))
        .collect();
    for (name, glyph) in &glyphs {
        println!("{name}: {glyph}");
    }
    // Every noun in this roster must be pairwise distinguishable.
    for i in 0..glyphs.len() {
        for j in (i + 1)..glyphs.len() {
            assert_ne!(
                glyphs[i].1, glyphs[j].1,
                "{} and {} draw identically ({})",
                glyphs[i].0, glyphs[j].0, glyphs[i].1
            );
        }
    }
}
