//! The catalog: seed + pins → scene JSON behind raw `extern "C"` exports
//! for external clients (spec: 2026-07-14-goldengrove-design.md §3).
//!
//! Mirrors the Casement's vessel wasm (decision 0052): no wasm-bindgen,
//! strings cross as (ptr, len) pairs over linear memory, the module
//! imports nothing. wasm32-unknown-unknown is single-threaded; the three
//! statics are the whole state model.
#![warn(missing_docs)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

/// The current world, if any.
static mut WORLD: Option<World> = None;
/// The output text (scene JSON or an error envelope) JS reads back.
static mut OUT: String = String::new();
/// The input buffer JS writes UTF-8 pins JSON into.
static mut INBUF: [u8; 4096] = [0; 4096];

/// Replace the output text.
fn set_out(text: String) {
    let out_ptr = &raw mut OUT;
    unsafe { *out_ptr = text }
}

/// Place a `{"error": …}` envelope in the output buffer.
fn set_error(msg: &str) {
    set_out(serde_json::json!({ "error": msg }).to_string());
}

/// Pin keys routed to `hornvale_astronomy::parse_pin`.
const SKY_KEYS: &[&str] = &[
    "moons",
    "wanderers",
    "rotation",
    "day-hours",
    "obliquity",
    "year-days",
    "neighbor",
    "spin",
];
/// Pin keys routed to `hornvale_terrain::parse_pin`.
const TERRAIN_KEYS: &[&str] = &[
    "plates",
    "ocean-fraction",
    "supercontinent",
    "globe-level",
    "continents",
];
/// Pin keys routed to the settlement pin parser.
const SETTLEMENT_KEYS: &[&str] = &["species"];

/// Parsed pin bundle: everything `build_world` wants.
struct Pins {
    sky: SkyPins,
    choice: SkyChoice,
    terrain: TerrainPins,
    settlement: SettlementPins,
}

/// Parse a flat JSON object of pins keyed by the CLI's flag vocabulary.
/// Values may be strings, numbers, or bools; each becomes a `key=value`
/// pin string, so pin syntax never drifts from the CLI (its own rule).
fn parse_pins(bytes: &[u8]) -> Result<Pins, String> {
    let v: serde_json::Value =
        serde_json::from_slice(bytes).map_err(|e| format!("pins JSON: {e}"))?;
    let obj = v.as_object().ok_or("pins JSON must be an object")?;
    let mut pins = Pins {
        sky: SkyPins::default(),
        choice: SkyChoice::Generated,
        terrain: TerrainPins::default(),
        settlement: SettlementPins::default(),
    };
    for (key, val) in obj {
        let val = match val {
            serde_json::Value::String(s) => s.clone(),
            serde_json::Value::Number(n) => n.to_string(),
            serde_json::Value::Bool(b) => b.to_string(),
            _ => {
                return Err(format!(
                    "pin '{key}': value must be a string, number, or bool"
                ));
            }
        };
        if key == "sky" {
            pins.choice = match val.as_str() {
                "generated" => SkyChoice::Generated,
                "constant" => SkyChoice::Constant,
                other => return Err(format!("sky: unknown value '{other}'")),
            };
        } else if SKY_KEYS.contains(&key.as_str()) {
            hornvale_astronomy::parse_pin(&format!("{key}={val}"), &mut pins.sky)
                .map_err(|e| e.to_string())?;
        } else if TERRAIN_KEYS.contains(&key.as_str()) {
            hornvale_terrain::parse_pin(&format!("{key}={val}"), &mut pins.terrain)
                .map_err(|e| e.to_string())?;
        } else if SETTLEMENT_KEYS.contains(&key.as_str()) {
            hornvale_worldgen::settlement_pins::parse_pin(
                &format!("{key}={val}"),
                &mut pins.settlement,
            )
            .map_err(|e| e.to_string())?;
        } else {
            return Err(format!("unknown pin '{key}'"));
        }
    }
    Ok(pins)
}

/// Genesis: build the world for `seed` under `pins`, replacing any prior
/// world. 0 on success; 1 with an error envelope when genesis refuses.
fn genesis(seed: u64, pins: &Pins) -> i32 {
    let world_ptr = &raw mut WORLD;
    unsafe { *world_ptr = None };
    match build_world(
        Seed(seed),
        &pins.sky,
        pins.choice,
        &pins.terrain,
        &pins.settlement,
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

/// Build the world for `seed` with default pins and a generated sky.
#[unsafe(no_mangle)]
pub extern "C" fn hw_new(seed: u64) -> i32 {
    let pins = Pins {
        sky: SkyPins::default(),
        choice: SkyChoice::Generated,
        terrain: TerrainPins::default(),
        settlement: SettlementPins::default(),
    };
    genesis(seed, &pins)
}

/// Build the world for `seed` with pins read as JSON (`len` bytes) from
/// the input buffer. Returns like `hw_new`, plus -1 (len exceeds the
/// buffer), -2 (not UTF-8), -3 (bad pins JSON / unknown pin, envelope set).
///
/// Clears the prior world *before* parsing, even on the -1/-2/-3 early
/// returns: any `hw_new*` call invalidates the prior world, full stop —
/// a caller must never be able to observe a stale world surviving a
/// refused pinned call.
#[unsafe(no_mangle)]
pub extern "C" fn hw_new_pinned(seed: u64, len: usize) -> i32 {
    let world_ptr = &raw mut WORLD;
    unsafe { *world_ptr = None };
    let inbuf_ptr = &raw const INBUF;
    let buf = unsafe { &*inbuf_ptr };
    if len > buf.len() {
        return -1;
    }
    if core::str::from_utf8(&buf[..len]).is_err() {
        return -2;
    }
    match parse_pins(&buf[..len]) {
        Ok(pins) => genesis(seed, &pins),
        Err(e) => {
            set_error(&e);
            -3
        }
    }
}

/// Emit the current world's `scene/system/v1` JSON into the out buffer.
/// 0 ok; 2 scene error (envelope set); -3 when no world is live.
#[unsafe(no_mangle)]
pub extern "C" fn hw_scene_system() -> i32 {
    let world_ptr = &raw const WORLD;
    let Some(world) = (unsafe { (*world_ptr).as_ref() }) else {
        set_error("no world; call hw_new first");
        return -3;
    };
    match hornvale_scene::system_scene(world) {
        Ok(s) => {
            set_out(hornvale_scene::system_json(&s));
            0
        }
        Err(e) => {
            set_error(&format!("{e}"));
            2
        }
    }
}

/// Emit the current world's `scene/tiles/v1` JSON at `width` tiles across.
/// 0 ok; 2 scene error (width odd / out of range; envelope set); -3 when
/// no world is live.
#[unsafe(no_mangle)]
pub extern "C" fn hw_scene_tiles(width: u32) -> i32 {
    let world_ptr = &raw const WORLD;
    let Some(world) = (unsafe { (*world_ptr).as_ref() }) else {
        set_error("no world; call hw_new first");
        return -3;
    };
    match hornvale_scene::tiles_scene(world, width) {
        Ok(s) => {
            set_out(hornvale_scene::scene_json(&s));
            0
        }
        Err(e) => {
            set_error(&format!("{e}"));
            2
        }
    }
}

/// Emit the current world's `scene/tiles-region/v1` JSON for one tile address.
/// 0 ok; 2 scene error (bad address; envelope set); -3 when no world is live.
#[unsafe(no_mangle)]
pub extern "C" fn hw_scene_tiles_region(
    face: u32,
    level: u32,
    ix: u32,
    iy: u32,
    samples: u32,
) -> i32 {
    let world_ptr = &raw const WORLD;
    let Some(world) = (unsafe { (*world_ptr).as_ref() }) else {
        set_error("no world; call hw_new first");
        return -3;
    };
    match hornvale_scene::tiles_region_scene(world, face, level, ix, iy, samples) {
        Ok(s) => {
            set_out(hornvale_scene::region_json(&s));
            0
        }
        Err(e) => {
            set_error(&format!("{e}"));
            2
        }
    }
}

/// Pointer to the 4096-byte input buffer JS writes pins JSON into.
#[unsafe(no_mangle)]
pub extern "C" fn hw_in_ptr() -> *mut u8 {
    (&raw mut INBUF).cast()
}

/// Pointer to the current output text (UTF-8, `hw_out_len` bytes).
#[unsafe(no_mangle)]
pub extern "C" fn hw_out_ptr() -> *const u8 {
    let out_ptr = &raw const OUT;
    unsafe { (&(*out_ptr)).as_ptr() }
}

/// Length in bytes of the current output text.
#[unsafe(no_mangle)]
pub extern "C" fn hw_out_len() -> usize {
    let out_ptr = &raw const OUT;
    unsafe { (&(*out_ptr)).len() }
}
