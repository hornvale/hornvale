//! The Lot: seed → one life, drawn from everyone who ever lived, behind raw
//! `extern "C"` exports for the project book's exhibit (ledger #17, amending
//! Task 10's placement in the world catalog).
//!
//! Mirrors the Casement's vessel wasm (decision 0052) and the world
//! catalog: no wasm-bindgen, strings cross as (ptr, len) pairs over linear
//! memory, the module imports nothing. wasm32-unknown-unknown is
//! single-threaded; the three statics below are the whole state model.
//!
//! `hl_*` is deliberately disjoint from the Casement's `hv_*` and the
//! catalog's `hw_*`, so a page can host all three (`clients/CLAUDE.md`).
//! This crate exists apart from the catalog because the four exports it
//! used to live in pushed the catalog's gzipped size past its release-asset
//! gate on the canonical box's older binaryen — an exhibit in an
//! unpublished book is not a released download, so it carries no size gate
//! of its own (the Casement is the precedent: 803 KiB gzipped, ungated).
#![warn(missing_docs)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, Vertex, World};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, build_world};

/// The current world, if any.
static mut WORLD: Option<World> = None;
/// The lot context for the live world, built on the first `hl_lot*` call and
/// reused; cleared with `WORLD` — a context outliving its world would draw
/// lives from the previous planet.
static mut LOT_CTX: Option<hornvale_lot::context::LotContext> = None;
/// The output text (lot JSON or an error envelope) JS reads back.
static mut OUT: String = String::new();

/// Replace the output text.
fn set_out(text: String) {
    let out_ptr = &raw mut OUT;
    unsafe { *out_ptr = text }
}

/// Place a `{"error": …}` envelope in the output buffer.
fn set_error(msg: &str) {
    set_out(serde_json::json!({ "error": msg }).to_string());
}

/// Build the world for `seed` with default pins and a generated sky — the
/// catalog's `hw_new` shape (four `build_world` arguments since the
/// Zenith). Replaces any prior world, clearing its lot context first. 0 on
/// success; 1 with an error envelope when genesis refuses.
#[unsafe(no_mangle)]
pub extern "C" fn hl_new(seed: u64) -> i32 {
    let world_ptr = &raw mut WORLD;
    let lot_ptr = &raw mut LOT_CTX;
    unsafe { *world_ptr = None };
    unsafe { *lot_ptr = None };
    match build_world(
        Seed(seed),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
    ) {
        Ok(w) => {
            unsafe { *world_ptr = Some(w) };
            set_out(String::new());
            0
        }
        Err(e) => {
            set_error(&format!("the genesis of seed {seed} refused: {e}"));
            1
        }
    }
}

/// The live world's lot context, derived on first use and reused for every
/// later lot call. `LOT_CTX` is only ever `Some` alongside the `WORLD` it
/// was built from — `hl_new` clears the two together — so the context this
/// hands back always describes `world`.
fn lot_ctx(
    world: &World,
) -> Result<&'static hornvale_lot::context::LotContext, hornvale_lot::LotError> {
    let ctx_ptr = &raw mut LOT_CTX;
    if unsafe { (*ctx_ptr).as_ref() }.is_none() {
        let built = hornvale_lot::context::assemble(world)?;
        unsafe { *ctx_ptr = Some(built) };
    }
    Ok(unsafe { (*ctx_ptr).as_ref() }.expect("just built above"))
}

/// Draw lot `index` of the live world (no year or site pin) and emit
/// `lot/life/v1` JSON. 0 ok; 2 lot error (envelope set); -3 when no world is
/// live.
#[unsafe(no_mangle)]
pub extern "C" fn hl_lot(index: u64) -> i32 {
    let world_ptr = &raw const WORLD;
    let Some(world) = (unsafe { (*world_ptr).as_ref() }) else {
        set_error("no world; call hl_new first");
        return -3;
    };
    let ctx = match lot_ctx(world) {
        Ok(c) => c,
        Err(e) => {
            set_error(&e.to_string());
            return 2;
        }
    };
    match hornvale_lot::draw::draw(
        ctx,
        hornvale_lot::LotIndex(index),
        &hornvale_lot::Pick::default(),
    ) {
        Ok(life) => {
            let story = hornvale_lot::slots::tell(world, ctx, &life);
            set_out(hornvale_lot::json::life_json(ctx, &life, &story));
            0
        }
        Err(e) => {
            set_error(&e.to_string());
            2
        }
    }
}

/// Draw lot `index` of the live world, pinning a birth `year` and/or an
/// occupation `site`, and emit `lot/life/v1` JSON. `site == u32::MAX` means
/// "no site pin", so a caller can pin a year alone without a sentinel
/// vertex colliding with a real one (vertex 0 is a legal site). 0 ok; 2 lot
/// error — a non-finite `year`, or the pin itself refused with the physical
/// reason (envelope set); -3 when no world is live.
#[unsafe(no_mangle)]
pub extern "C" fn hl_lot_pinned(index: u64, year: f64, site: u32) -> i32 {
    let world_ptr = &raw const WORLD;
    let Some(world) = (unsafe { (*world_ptr).as_ref() }) else {
        set_error("no world; call hl_new first");
        return -3;
    };
    if !year.is_finite() {
        set_error("year must be finite");
        return 2;
    }
    let ctx = match lot_ctx(world) {
        Ok(c) => c,
        Err(e) => {
            set_error(&e.to_string());
            return 2;
        }
    };
    let pick = hornvale_lot::Pick {
        year: Some(year),
        site: if site == u32::MAX {
            None
        } else {
            Some(Vertex(site))
        },
    };
    match hornvale_lot::draw::draw(ctx, hornvale_lot::LotIndex(index), &pick) {
        Ok(life) => {
            let story = hornvale_lot::slots::tell(world, ctx, &life);
            set_out(hornvale_lot::json::life_json(ctx, &life, &story));
            0
        }
        Err(e) => {
            set_error(&e.to_string());
            2
        }
    }
}

/// Emit the live world's `lot/curve/v1` JSON — the When graph's souls-ever
/// total and births-per-epoch table. 0 ok; 2 lot error (envelope set); -3
/// when no world is live.
#[unsafe(no_mangle)]
pub extern "C" fn hl_lot_curve() -> i32 {
    let world_ptr = &raw const WORLD;
    let Some(world) = (unsafe { (*world_ptr).as_ref() }) else {
        set_error("no world; call hl_new first");
        return -3;
    };
    let ctx = match lot_ctx(world) {
        Ok(c) => c,
        Err(e) => {
            set_error(&e.to_string());
            return 2;
        }
    };
    let curve = hornvale_lot::draw::curve(ctx);
    set_out(hornvale_lot::json::curve_json(&curve));
    0
}

/// Emit the live world's `lot/places/v1` JSON — every occupation alive in
/// `year`. A non-finite `year` is refused. 0 ok; 2 lot error (envelope set);
/// -3 when no world is live.
#[unsafe(no_mangle)]
pub extern "C" fn hl_lot_places(year: f64) -> i32 {
    let world_ptr = &raw const WORLD;
    let Some(world) = (unsafe { (*world_ptr).as_ref() }) else {
        set_error("no world; call hl_new first");
        return -3;
    };
    if !year.is_finite() {
        set_error("year must be finite");
        return 2;
    }
    let ctx = match lot_ctx(world) {
        Ok(c) => c,
        Err(e) => {
            set_error(&e.to_string());
            return 2;
        }
    };
    let places = hornvale_lot::draw::places(ctx, year);
    set_out(hornvale_lot::json::places_json(year, &places));
    0
}

/// Pointer to the current output text (UTF-8, `hl_out_len` bytes).
#[unsafe(no_mangle)]
pub extern "C" fn hl_out_ptr() -> *const u8 {
    let out_ptr = &raw const OUT;
    unsafe { (&(*out_ptr)).as_ptr() }
}

/// Length in bytes of the current output text.
#[unsafe(no_mangle)]
pub extern "C" fn hl_out_len() -> usize {
    let out_ptr = &raw const OUT;
    unsafe { (&(*out_ptr)).len() }
}
