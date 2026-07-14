//! The Casement's wasm seam: the possess/vessel loop behind five raw
//! `extern "C"` exports, for the project book's live-possession exhibit
//! (spec: docs/superpowers/specs/2026-07-13-the-casement-design.md).
//!
//! The module imports **nothing** — no clock, no network, no DOM. Five
//! exports, memory in, prose out; that emptiness is the exhibit's
//! one-line sandbox audit. No wasm-bindgen: strings cross as (ptr, len)
//! pairs over the module's linear memory.
#![warn(missing_docs)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World, WorldTime};
use hornvale_terrain::TerrainPins;
use hornvale_vessel::{PossessOpts, Session, Turn};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

/// One live possession: the heap world and the session borrowing it.
struct Possession {
    /// The owned world, held raw so re-possession can reclaim and drop it.
    world: *mut World,
    /// The session; it must drop before the world it borrows.
    session: Session<'static>,
}

// wasm32-unknown-unknown is single-threaded; these statics are the whole
// state model. Access goes through `&raw` to satisfy static_mut_refs.

/// The current possession, if any.
static mut STATE: Option<Possession> = None;
/// The output text JS reads via `hv_out_ptr`/`hv_out_len`.
static mut OUT: String = String::new();
/// The input buffer JS writes UTF-8 command bytes into.
static mut INBUF: [u8; 4096] = [0; 4096];

/// Replace the output text.
#[allow(clippy::deref_addrof)]
fn set_out(text: String) {
    unsafe { *(&raw mut OUT) = text }
}

/// Tear down the current possession: session first, then its world.
#[allow(clippy::deref_addrof)]
fn teardown() {
    if let Some(p) = unsafe { (*(&raw mut STATE)).take() } {
        let Possession { world, session } = p;
        drop(session);
        // SAFETY: `world` came from Box::into_raw in hv_start, and the
        // session borrowing it was dropped on the line above.
        drop(unsafe { Box::from_raw(world) });
    }
}

/// Build the world for `seed` (default pins, generated sky, day 0) and
/// start a possession, placing the opening text in the output buffer.
/// Returns 0 on success. On failure the sim's own error text is placed in
/// the output buffer and 1 (genesis refused) or 2 (possession failed) is
/// returned. Any prior possession is torn down first — never leaked.
#[unsafe(no_mangle)]
#[allow(clippy::deref_addrof)]
pub extern "C" fn hv_start(seed: u64) -> i32 {
    teardown();
    let world = match build_world(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    ) {
        Ok(w) => w,
        Err(e) => {
            set_out(format!("The genesis of seed {seed} refused: {e}"));
            return 1;
        }
    };
    let world = Box::into_raw(Box::new(world));
    // SAFETY: `world` stays alive until teardown(), which drops the
    // session borrowing it before reclaiming the box.
    let world_ref: &'static World = unsafe { &*world };
    let opts = PossessOpts {
        day: WorldTime { day: 0.0 },
        echo: false,
    };
    match Session::start(world_ref, &opts) {
        Ok((session, opening)) => {
            unsafe { *(&raw mut STATE) = Some(Possession { world, session }) };
            set_out(opening);
            0
        }
        Err(e) => {
            // SAFETY: the failed start returned no session; nothing
            // borrows the world on this path.
            drop(unsafe { Box::from_raw(world) });
            set_out(format!("The possession failed: {e}"));
            2
        }
    }
}

/// Pointer to the 4096-byte input buffer JS writes UTF-8 commands into.
#[unsafe(no_mangle)]
pub extern "C" fn hv_in_ptr() -> *mut u8 {
    (&raw mut INBUF).cast()
}

/// Handle one command of `len` bytes from the input buffer. Returns 0
/// (possession continues), 1 (released), or a negative protocol error:
/// -1 length exceeds the buffer, -2 not UTF-8, -3 no live possession.
#[unsafe(no_mangle)]
#[allow(clippy::deref_addrof)]
pub extern "C" fn hv_handle(len: usize) -> i32 {
    let buf = unsafe { &*(&raw const INBUF) };
    if len > buf.len() {
        return -1;
    }
    let Ok(line) = core::str::from_utf8(&buf[..len]) else {
        return -2;
    };
    let Some(p) = (unsafe { (&mut *(&raw mut STATE)).as_mut() }) else {
        return -3;
    };
    match p.session.handle(line) {
        Turn::Out(s) => {
            set_out(s);
            0
        }
        Turn::Released(s) => {
            set_out(s);
            1
        }
    }
}

/// Pointer to the current output text (UTF-8, `hv_out_len` bytes).
#[unsafe(no_mangle)]
#[allow(clippy::deref_addrof)]
pub extern "C" fn hv_out_ptr() -> *const u8 {
    unsafe { (&(*(&raw const OUT))).as_ptr() }
}

/// Length in bytes of the current output text.
#[unsafe(no_mangle)]
#[allow(clippy::deref_addrof)]
pub extern "C" fn hv_out_len() -> usize {
    unsafe { (&(*(&raw const OUT))).len() }
}
