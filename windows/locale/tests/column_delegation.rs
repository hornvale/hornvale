//! The Fathom, Task 3: `LocaleContext::water_column_at`/`expr_at_stratum`
//! collapsed onto `GeneratedClimate::strata_at`/`biome_expr_at_stratum`
//! (Task 1) instead of standing as a second, independently-hand-rolled
//! derivation of the same water column.
//!
//! Three claims:
//!
//! - **The before-arm holds.** `fixtures/column_before.txt` was captured on
//!   the unmodified, pre-delegation methods (81 seed-42 cells, every
//!   `Stratum` variant per cell). Re-running the identical capture procedure
//!   against the delegating code and diffing byte-for-byte is the only
//!   honest way to show nothing moved — a fixture re-derived from the new
//!   code and compared to itself would prove nothing.
//! - **The two derivations agree wherever both are defined** — exhaustively,
//!   over all 40,962 cells, not just the fixture's 81-cell sample.
//! - **A regression guard against de-delegation**, also exhaustive. Read the
//!   doc comment on `agreement_is_not_proof_of_delegation` below before
//!   trusting this test more than it can support: it is not the positive
//!   control the task brief asked for, and the report says why one could not
//!   be constructed.

use hornvale_climate::{Realm, Stratum};
use hornvale_kernel::{CellId, Seed, World};
use hornvale_locale::LocaleContext;
use std::fmt::Write as _;

/// Every `Stratum` variant, declaration order — shared by the fixture
/// capture and this file's regeneration of it, so the two enumerate
/// identically regardless of how the enum is ever reordered.
const ALL_STRATA: [Stratum; 11] = [
    Stratum::Surface,
    Stratum::Epipelagic,
    Stratum::Mesopelagic,
    Stratum::Bathypelagic,
    Stratum::Abyssal,
    Stratum::Hadal,
    Stratum::Regolith,
    Stratum::Cover,
    Stratum::Basement,
    Stratum::Roots,
    Stratum::Underneath,
];

/// The before-arm, captured on unmodified `water_column_at`/`expr_at_stratum`
/// before this task's delegation landed (commit `aadf5920`, "capture
/// pre-delegation column fixture").
const BEFORE_ARM: &str = include_str!("fixtures/column_before.txt");

fn world() -> World {
    World::new(Seed(42))
}

/// Regenerates the fixture's exact text (same stride rule, same cell order,
/// same `{:?}` formatting) against whatever `LocaleContext` is compiled in —
/// i.e. against the CURRENT code. Sharing this with the original capture
/// procedure (not re-deriving the stride independently) is what makes the
/// comparison meaningful: any drift in the text is a drift in what the two
/// methods answer, not in how the test samples cells.
fn regenerate(ctx: &LocaleContext) -> String {
    let cell_count = ctx.climate().geosphere().cell_count();
    let stride = (cell_count / 80).max(1);
    let mut out = String::new();
    for id in (0..cell_count).step_by(stride) {
        let cell = CellId(id as u32);
        let col = ctx.water_column_at(cell);
        writeln!(out, "CELL {id} COLUMN {col:?}").unwrap();
        for s in ALL_STRATA {
            let expr = ctx.expr_at_stratum(cell, s);
            writeln!(out, "CELL {id} STRATUM {s:?} EXPR {expr:?}").unwrap();
        }
    }
    out
}

/// Claim 1 — the before-arm holds. Every sampled value the fixture recorded
/// is reproduced exactly by the delegating implementation.
#[test]
fn the_before_arm_holds() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let after = regenerate(&ctx);
    assert_eq!(
        after, BEFORE_ARM,
        "water_column_at/expr_at_stratum moved after delegating to \
         GeneratedClimate — The Fathom may not move behaviour"
    );
}

/// Claim 2 — the two derivations agree wherever both are defined: for every
/// cell and every in-column stratum (at or above the cell's own floor),
/// `expr_at_stratum(cell, s)` equals `climate.biome_expr_at_stratum(cell,
/// s).unwrap()`. Exhaustive over the whole 40,962-cell globe, not just the
/// fixture's sample.
#[test]
fn expr_at_stratum_agrees_with_climate_in_column() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let climate = ctx.climate();
    let cell_count = climate.geosphere().cell_count();
    let mut checked = 0usize;
    for id in 0..cell_count {
        let cell = CellId(id as u32);
        let e = climate.biome_expr_at(cell);
        let ladder = e.realm.strata();
        let floor = ladder
            .iter()
            .position(|s| *s == e.stratum)
            .expect("a cell's stratum is on its own realm's ladder");
        for &s in &ladder[..=floor] {
            let got = ctx.expr_at_stratum(cell, s);
            let want = climate.biome_expr_at_stratum(cell, s).unwrap();
            assert_eq!(got, want, "cell {id} stratum {s:?}");
            checked += 1;
        }
    }
    assert!(checked > 0, "the sweep must exercise at least one cell");
}

/// Claim 3, as the task brief asks for it — `water_column_at` and
/// `climate.strata_at` agree on every water cell, exhaustively.
///
/// **This is NOT the positive control the brief wanted, and the report says
/// so.** `water_column_at`'s water-realm branch is, post-delegation, the
/// single expression `self.climate.strata_at(cell)` — so this assertion is
/// true by construction from the source it is checking, for the same reason
/// [`expr_at_stratum_agrees_with_climate_in_column`] is: both compare a
/// delegating wrapper's output to the callee it delegates to, and a
/// pass-through cannot disagree with what it passes through to. No `CellId`
/// in this world (or any world) can make it fail while the delegation
/// stands, so it is not evidence that duplication was replaced — only
/// reading the diff in `windows/locale/src/lib.rs` is. What this test *is*
/// good for: an exhaustive (all 40,962 cells, not the fixture's 81-cell
/// sample) regression guard against a future edit that reintroduces an
/// independent, silently-diverging hand-rolled column derivation here.
#[test]
fn water_column_at_agrees_with_climate_strata_at_on_every_water_cell() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let climate = ctx.climate();
    let cell_count = climate.geosphere().cell_count();
    let mut water_cells = 0usize;
    for id in 0..cell_count {
        let cell = CellId(id as u32);
        if climate.biome_expr_at(cell).realm != Realm::WATERWORLD {
            continue;
        }
        water_cells += 1;
        assert_eq!(
            ctx.water_column_at(cell),
            climate.strata_at(cell),
            "cell {id}"
        );
    }
    assert!(
        water_cells > 0,
        "the sweep must find at least one water cell"
    );
}
