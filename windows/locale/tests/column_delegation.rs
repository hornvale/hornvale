//! The Fathom, Task 3: `LocaleContext::water_column_at`/`expr_at_stratum`
//! collapsed onto `GeneratedClimate::strata_at`/`biome_expr_at_stratum`
//! (Task 1) instead of standing as a second, independently-hand-rolled
//! derivation of the same water column.
//!
//! Three claims:
//!
//! - **The before-arm held, ONCE, and is now spent.**
//!   `fixtures/column_before.txt` was captured on the unmodified,
//!   pre-delegation methods (81 seed-42 cells, every `Stratum` variant per
//!   cell); re-running the identical capture against the delegating code and
//!   diffing byte-for-byte was the only honest way to show nothing moved.
//!   That proof was made at `aadf5920` and cannot be re-made — decision
//!   0131's terrain epoch moved the world the sample is drawn from, the
//!   fixture was regenerated, and what remains is a drift tripwire rather
//!   than cross-refactor evidence. See `the_sampled_column_is_byte_stable`.
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

/// Claim 1 — the sampled column is byte-stable.
///
/// **THE FATHOM'S BEFORE-ARM IS SPENT, AND THIS TEST IS NO LONGER IT**
/// (decision 0132, 2026-08-14). The fixture was captured at `aadf5920` on the
/// unmodified, pre-delegation methods, and comparing the delegating code
/// against it proved that the delegation moved nothing. That proof was made
/// and holds; it is a fact about `aadf5920..` and cannot be re-made.
///
/// The Glasshouse's terrain epoch then moved every coastline, so the sampled
/// cells' water columns legitimately changed, and the fixture was regenerated.
/// From here it compares the current derivation against itself across time —
/// which the module doc above is right to say "would prove nothing" about a
/// REFACTOR. It is still worth keeping as a cheap drift tripwire on 81 cells
/// × 11 strata, but read it as that and not as evidence about The Fathom.
///
/// **The ongoing guard is claim 2, not this one.** The exhaustive agreement
/// between `expr_at_stratum` and `climate.biome_expr_at_stratum` over all
/// 40,962 cells is what actually holds the delegation in place, it is
/// world-independent, and it passed through this epoch untouched. A future
/// campaign that needs a true before-arm here must capture a fresh one before
/// its own change, exactly as The Fathom did.
///
/// **Regenerated a second time (Stage B Tasks 4/5, the thermostat and the
/// latitude profile).** The thermostat and the area-mean-zero latitude
/// profile move the climate field the water column reads from — a two-line
/// diff in the fixture. Still just a drift tripwire, not evidence about any
/// refactor.
#[test]
fn the_sampled_column_is_byte_stable() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let after = regenerate(&ctx);
    assert_eq!(
        after, BEFORE_ARM,
        "water_column_at/expr_at_stratum moved against the committed sample — \
         if a change to the WORLD caused it, regenerate the fixture and say so; \
         if nothing about the world moved, this is a real regression"
    );
}

/// Claim 2 — the two derivations agree wherever both are defined: for every
/// cell and every in-column stratum (at or above the cell's own floor),
/// `expr_at_stratum(cell, s)` equals `climate.biome_expr_at_stratum(cell,
/// s).unwrap()`. Exhaustive over the whole 40,962-cell globe, not just the
/// fixture's sample.
///
/// claim: structural(seed: 42) — false-positive seed-loop flag; `s` binds a
/// Stratum walked over one fixed world's column, not a seed.
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
