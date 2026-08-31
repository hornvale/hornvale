use hornvale_game_core::{
    Grid, PaletteEntry, Plan, PlanExtent, PlanMark, PlanPoint, Snapshot, Spatial, plan,
    register::binding_of,
};

const FIXTURE: &str = include_str!("fixtures/session-seed-42-chamber.json");

fn chamber_plan() -> hornvale_game_core::Plan {
    match Snapshot::parse(FIXTURE).unwrap().spatial {
        Spatial::Chamber { plan } => plan,
        Spatial::Walk { .. } => panic!("fixture must be a chamber-band turn"),
    }
}

#[test]
fn the_plan_draws_the_shipped_vocabulary() {
    let p = chamber_plan();
    let mut g = Grid::new(40, 14);
    plan::draw(&p, &mut g, (0, 0));
    let text = g.to_plain_text();
    assert!(text.contains('#'), "a wall");
    assert!(text.contains('.'), "the floor");
    assert!(text.contains('@'), "you");
}

#[test]
fn you_are_drawn_exactly_once() {
    let p = chamber_plan();
    let mut g = Grid::new(40, 14);
    plan::draw(&p, &mut g, (0, 0));
    assert_eq!(g.to_plain_text().matches('@').count(), 1);
}

/// A grid tall enough to hold the whole seed-42 fixture (extent 19x19),
/// unlike the brief's 40x14 (which is wide enough but not tall enough, and
/// deliberately still passes the two tests above since `draw` silently
/// clips rather than panicking).
fn full_grid() -> Grid {
    Grid::new(20, 20)
}

/// The `@` lands exactly at `plan.you`, offset by the extent — not merely
/// "somewhere," which `you_are_drawn_exactly_once` alone cannot tell apart
/// from an `@` at the wrong cell.
#[test]
fn you_lands_at_the_plans_own_coordinate() {
    let p = chamber_plan();
    let mut g = full_grid();
    plan::draw(&p, &mut g, (0, 0));
    let x = (p.you.x - p.extent.x) as u16;
    let y = (p.you.y - p.extent.y) as u16;
    assert_eq!(
        g.get(x, y).unwrap().glyph,
        Some('@'),
        "expected `@` at the plan's own you-coordinate ({x}, {y})"
    );
}

/// The reference picture, straight from Hornvale's OWN canonical console
/// renderer for `vessel/plan/v1` (`windows/vessel/src/lattice/render.rs`'s
/// `render`, reached indoors through the `map` verb) — NOT this crate's own
/// logic. `hornvale-game-core` may not depend on `hornvale-vessel` (the
/// containment rule: no hornvale crate in this client's graph), so this is
/// a golden, not a shared function call.
///
/// Regenerate with:
/// ```text
/// printf 'enter\nmap\nrelease\n' > /tmp/hv-plan.txt
/// cargo run --release -p hornvale -- possess --seed 42 --script /tmp/hv-plan.txt
/// ```
/// then copy the 19 lines of the plan picture verbatim, between the `[plan:
/// chamber …]` caption and the `  legend:` footer.
///
/// Unlike `chart.rs`'s reference shape, this campaign's plan vocabulary
/// (`#`/`.`/`+`/`@`) is the SAME vocabulary the sim's own renderer uses (see
/// this crate's `src/plan.rs` module doc), so the comparison below pins the
/// actual glyphs, not merely which positions are filled — a stronger check
/// than `chart.rs` can make, because a plan is dense (every cell in the
/// extent draws something) and a shape-only comparison would be vacuous:
/// any full 19x19 rectangle has the same "shape" regardless of what fills
/// it.
// A raw string, deliberately: a `"\` line-continuation here would eat the
// first line's own leading spaces (Rust trims leading whitespace after a
// backslash-newline), silently corrupting the one thing this golden exists
// to pin. (This plan happens to have no leading spaces on any row, since
// every row starts with the border wall — but the discipline is the same
// one `chart.rs` documents, and the raw string costs nothing to keep.)
/// **RE-DERIVED AT THE GLASSHOUSE'S CLOSE (2026-08-15).** The campaign's
/// climate correction moved seed 42's world, and with it the chamber the
/// committed `session-seed-42-chamber.json` fixture records — so this golden
/// went stale for the same reason every other post-refresh witness did, and
/// this test was the `clients` set's single red.
///
/// It was re-taken by running the recipe above against the sim, NOT by
/// copying the client's own output into it. That distinction is the whole
/// value of this test: the client's picture and the sim's are two independent
/// renderers of `vessel/plan/v1`, and pasting the left-hand side of the
/// failure into the right-hand side would turn a cross-implementation check
/// into a tautology that can never fail again. The two agreed byte-for-byte
/// once re-taken, which is the result that makes the re-pin safe — the client
/// was already correct and only the transcription was old.
const REFERENCE_PICTURE: &str = r"###################
#....#............#
#....#............#
#....+............#
#....#............#
#....#............#
#....#######+######
#....#............#
#....#............#
#..@.#............#
#....#............#
#....#............#
#....#............#
#....#............#
#....#............#
#....#............#
#....#............#
#....#............#
###################";

/// Extract the tight bounding box of every non-blank cell as its actual
/// glyphs, row by row — the plan analogue of `chart.rs`'s `grid_shape`, but
/// keeping the real character rather than collapsing to `#`/blank, since
/// this module's vocabulary matches the sim's own exactly (see the module
/// doc) and a byte-for-byte pin is available and stronger.
fn grid_picture(g: &Grid) -> Vec<String> {
    let mut bounds: Option<(u16, u16, u16, u16)> = None;
    for y in 0..g.height() {
        for x in 0..g.width() {
            if g.get(x, y).is_some_and(|c| !c.is_blank()) {
                bounds = Some(match bounds {
                    None => (x, x, y, y),
                    Some((x0, x1, y0, y1)) => (x0.min(x), x1.max(x), y0.min(y), y1.max(y)),
                });
            }
        }
    }
    let Some((x0, x1, y0, y1)) = bounds else {
        return Vec::new();
    };
    (y0..=y1)
        .map(|y| {
            (x0..=x1)
                .map(|x| g.get(x, y).and_then(|c| c.glyph).unwrap_or(' '))
                .collect::<String>()
        })
        .collect()
}

/// The client's plan must place and letter every cell exactly where
/// Hornvale's own canonical `map` verb does (`REFERENCE_PICTURE` above). A
/// projection that is internally consistent but geometrically wrong (a
/// transposed axis, an unapplied extent offset) would still pass every
/// other test in this file; only a real comparison against the sim's own
/// output catches it — see `chart.rs`'s module doc for the precedent this
/// campaign already found that mistake once.
///
/// **Marks are cleared before this comparison (fix round 1, The Legend
/// Task 10).** The committed fixture now carries a real `"furnishing"`
/// mark (a screen, at plan-local `(2, 13)`), and this client correctly
/// draws it as its own glyph — a deliberate divergence from
/// `REFERENCE_PICTURE`, which comes from the sim's own console `map` verb
/// and never draws a mark of any kind (the module doc's own history). This
/// golden's job is catching a wrong PROJECTION (an axis swap, a missed
/// offset), not re-litigating whether a mark draws its own glyph — that is
/// `src/plan.rs`'s own unit tests' job, and
/// `the_fixtures_own_furnishing_mark_draws_its_glyph` below pins that the
/// one real mark this fixture carries still draws correctly when marks are
/// NOT cleared.
#[test]
fn the_shape_matches_the_sims_own_ascii_render() {
    let mut p = chamber_plan();
    p.marks.clear();
    let mut g = full_grid();
    plan::draw(&p, &mut g, (0, 0));
    let want: Vec<String> = REFERENCE_PICTURE.lines().map(|l| l.to_string()).collect();
    assert_eq!(grid_picture(&g), want);
}

/// The companion half of the golden above: with the fixture's own marks
/// left intact, its one real furnishing mark (a screen, at plan-local
/// `(2, 13)`) draws the furnishing glyph — proving the divergence the
/// golden strips out is exactly this, and only this, moved.
#[test]
fn the_fixtures_own_furnishing_mark_draws_its_glyph() {
    let p = chamber_plan();
    let mut g = full_grid();
    plan::draw(&p, &mut g, (0, 0));
    let x = (2 - p.extent.x) as u16;
    let y = (13 - p.extent.y) as u16;
    assert_eq!(
        g.get(x, y).unwrap().glyph,
        Some('?'),
        "the fixture's own furnishing mark (a screen) must draw the furnishing glyph"
    );
}

/// Decision 0389 enforced against a REAL render, mirroring
/// `clients/game/bin/tests/plate_vocabulary.rs`'s guard of the same name
/// against the world map's own vocabulary. Before this test the
/// chamber-band plan was one of two panes `no_character_is_bound_twice`
/// could not see at all — it validates `register::REGISTER` against itself
/// and has no way to notice a pane drawing a glyph the table never
/// claimed, which is exactly the mechanism behind the three historical
/// `.`/`+`/`#` collisions (progress.md, "the register is unpinned for two
/// of three panes").
#[test]
fn every_drawn_glyph_is_claimed_by_the_register() {
    let p = chamber_plan();
    let mut g = full_grid();
    plan::draw(&p, &mut g, (0, 0));
    let glyphs: std::collections::BTreeSet<char> = (0..g.height())
        .flat_map(|y| (0..g.width()).map(move |x| (x, y)))
        .filter_map(|(x, y)| g.get(x, y).and_then(|c| c.glyph))
        .collect();
    for glyph in glyphs {
        assert!(
            binding_of(glyph).is_some() || glyph.is_ascii_alphabetic(),
            "{glyph:?} is drawn but unclaimed"
        );
    }
}

/// A minimal 3x2 plan (mirrors `src/plan.rs`'s own private `small_plan` test
/// helper, unreachable from this external integration binary): wall, floor,
/// threshold on row 0; wall, floor, wall on row 1. `you` stands on the
/// floor at (1, 1), leaving (0, 0), (2, 0), and (0, 1) free for marks.
fn minimal_plan() -> Plan {
    Plan {
        extent: PlanExtent {
            x: 0,
            y: 0,
            w: 3,
            h: 2,
        },
        palette: vec![
            PaletteEntry {
                kind: "wall".to_string(),
                chambers: vec![],
                color: None,
            },
            PaletteEntry {
                kind: "floor".to_string(),
                chambers: vec![0],
                color: None,
            },
            PaletteEntry {
                kind: "threshold".to_string(),
                chambers: vec![0, 1],
                color: None,
            },
        ],
        cells: vec![0, 1, 2, 0, 1, 0], // lexicon: `Plan::cells` is this crate's own AREA-sense row-major grid, not a mesh vertex.
        you: PlanPoint { x: 1, y: 1 },
        marks: vec![],
    }
}

/// One agent mark at `(x, y)`.
fn agent_mark(x: i32, y: i32, noun: &str) -> PlanMark {
    PlanMark {
        x,
        y,
        noun: noun.to_string(),
        kind: "agent".to_string(),
        datum: format!("A {noun} stands here."),
        salience: 1,
    }
}

/// Fix round 2's review finding, `plan::draw`'s half — see `tests/chart.rs`'s
/// `a_creatures_glyph_is_stable_through_chart_draw_regardless_of_company`
/// for the property and why it must be pinned through the real entry point,
/// not just `creature_glyph` in isolation: a caller of `creature_glyph`
/// inside `plan::draw`'s marks pass could collect the plan's visible nouns
/// and de-duplicate them before calling, which would never touch
/// `creature_glyph`'s signature and so would pass every assertion in
/// `tests/lexicon.rs`.
///
/// Same discriminating company as the chart test: `giant elk` and `gnoll`
/// both sort alphabetically before `goblin` and share its initial — the
/// exact case fix round 1's `creature_ranks` resolved by moving `goblin`
/// off `g`.
#[test]
fn a_creatures_glyph_is_stable_through_plan_draw_regardless_of_company() {
    let mut alone = minimal_plan();
    alone.marks = vec![agent_mark(2, 0, "goblin")];
    let mut g_alone = Grid::new(5, 5);
    plan::draw(&alone, &mut g_alone, (0, 0));

    let mut crowded = minimal_plan();
    crowded.marks = vec![
        agent_mark(2, 0, "goblin"),
        // Sorts before "goblin", shares its initial.
        agent_mark(0, 0, "giant elk"),
        // Same initial as "goblin".
        agent_mark(0, 1, "gnoll"),
    ];
    let mut g_crowded = Grid::new(5, 5);
    plan::draw(&crowded, &mut g_crowded, (0, 0));

    let alone_glyph = g_alone.get(2, 0).unwrap().glyph;
    let crowded_glyph = g_crowded.get(2, 0).unwrap().glyph;
    assert_eq!(
        alone_glyph, crowded_glyph,
        "a goblin's own glyph must not change because giant elk and gnoll also share the plan"
    );
    assert_eq!(alone_glyph, Some('g'));
}
