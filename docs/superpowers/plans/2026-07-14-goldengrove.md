# Goldengrove Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove the old orrery (terminal + in-book), build the world-wasm
catalog (`clients/world-wasm/`), and stand up the `hornvale/goldengrove`
3D client (three.js system view + globe view) consuming that catalog.

**Architecture:** Three stages. (A) The removal — the `hornvale orrery`
verb, ANSI renderer, cast encoder, `clients/orrery/`, and its book
artifacts die; star chart, atlas, and `scene/system/v1` survive. (B) The
catalog — an out-of-workspace cdylib mirroring `clients/vessel/wasm`
(extern "C", no wasm-bindgen) exposing seed+pins → `scene/system/v1` +
`scene/tiles/v1` JSON, golden-contracted byte-identical to the native CLI,
released as tagged GitHub assets. (C) The planetarium — a new repo
harvesting bitterbridge/goldengrove's three.js views and
`clients/orrery/`'s TS ephemeris/parse/palette modules, deployed to Pages.

**Tech Stack:** Rust (edition 2024, serde/serde_json only), wasm32-unknown-unknown,
node ≥ 20 (smoke drivers), Vite + TypeScript + three.js + vitest (client repo only).

**Spec:** `docs/superpowers/specs/2026-07-14-goldengrove-design.md`

## Global Constraints

- Hornvale-side commits pass `make gate` (fmt + clippy -D warnings + nextest + doctests); `cargo fmt` is the final step before every commit.
- Workspace dependency allowlist is serde + serde_json ONLY; the wasm crate is OUT of the workspace (root `Cargo.toml` `exclude`) and still uses only path-deps + serde_json.
- No `HashMap`/`HashSet` anywhere (clippy.toml enforces); no wall-clock time in Rust code.
- Every pub item gets a one-line doc comment (`#![warn(missing_docs)]`); primitives at pub boundaries in workspace crates need `type-audit:` tags — the wasm crate is outside the workspace and the audit, like vessel.
- wasm size gate: `hornvale_world_wasm.wasm` ≤ 1 MB raw (1048576 bytes).
- Commit messages end with: `Claude-Session: https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw`
- Client repo (`hornvale/goldengrove`): `npm test` + `npm run build` green at every commit; it owns its own toolchain (decision 0023) — no hornvale gate applies there.
- Never run `make rebaseline` with censuses; censuses are AWS-only (not needed by any task here — the removal touches no worldgen).
- Campaign-branch rule: run `make preflight` and absorb main at each stage boundary (A→B, B→C).

## File Structure

**hornvale/hornvale (worktree `goldengrove`, branch `worktree-goldengrove`):**

```
cli/src/main.rs                      orrery verb removed (help, match arm, cmd_orrery)
cli/tests/orrery.rs                  DELETED
domains/astronomy/src/render.rs      orrery half removed (Cell/GlyphSet/orrery_*); chart_* stays
kernel/src/cast.rs                   DELETED (orphaned by the removal)
kernel/src/lib.rs                    cast module + asciinema_v2 export removed
clients/orrery/                      DELETED (src, testdata, deno.json, deno.lock)
book/src/gallery/orrery-seed-42.md   DELETED
book/src/gallery/orrery.js           DELETED
book/src/gallery/scene-system-seed-42.json  DELETED (scene-tiles-*.json STAYS — atlas food)
book/src/SUMMARY.md                  gallery entry removed (chronicle entries STAY)
book/src/domains/astronomy.md        orrery prose section rewritten (star chart stays)
.github/workflows/ci.yml             orrery job removed; drift list drops clients/orrery/testdata; world job added
scripts/regenerate-artifacts.sh      scene-system + ephemeris-golden lines removed
Cargo.toml (root)                    exclude += "clients/world-wasm"
Makefile                             wasm-world + world-check targets
clients/world-wasm/Cargo.toml        NEW standalone crate hornvale-world-wasm
clients/world-wasm/src/lib.rs        NEW extern "C" ABI (hw_*)
clients/world-wasm/drive.mjs         NEW golden smoke driver
.github/workflows/release-world-wasm.yml  NEW tag-triggered release
windows/scene/examples/ephemeris_golden.rs  KEPT (goldengrove regenerates its fixture from it)
```

**hornvale/goldengrove (new repo, checkout `~/Projects/hornvale/goldengrove`):**

```
package.json / vite.config.ts / tsconfig.json   harvested from bitterbridge/goldengrove web/
src/main.ts                    entry: URL state → worker genesis → views
src/sim/scene.ts               harvested from hornvale clients/orrery/src/scene.ts (types + parseSystem/parseTiles)
src/sim/ephemeris.ts           harvested from clients/orrery (worldPhase/synodicDays/moonPhase/rotationPhase)
src/sim/palette.ts             harvested from clients/orrery (starTint/elevationColor)
src/sim/moon.ts                harvested from clients/orrery (illuminatedFraction/litOffset)
src/sim/catalog.ts             NEW: wasm loader over the hw_* ABI
src/sim/worker.ts              NEW: genesis off the main thread
src/state/url.ts               harvested from goldengrove web/src/state (adapted: seed + view + day)
src/time/clock.ts              harvested from goldengrove web/src/time
src/views/system.ts            NEW three.js system view (uses ephemeris)
src/views/globe.ts             three.js globe: goldengrove cubeSphere.ts + biomePalette.ts adapted to TilesScene
src/views/zoom.ts              NEW camera transition system ↔ globe
src/ui/hud.ts                  harvested from goldengrove web/src/ui
testdata/ephemeris-seed-42.json  harvested golden fixture (regen: cargo run -p hornvale-scene --example ephemeris_golden)
.github/workflows/deploy.yml   Pages deploy; fetches pinned catalog release
```

---

## Stage A — the removal

### Task 1: Remove the terminal orrery (verb, renderer, casts, encoder)

**Files:**
- Modify: `cli/src/main.rs` (help lines 47–48, match arm ~line 101, `cmd_orrery` ~lines 616–700)
- Modify: `domains/astronomy/src/render.rs` (the orrery half, ~line 300 onward: `Cell`, `GlyphSet`, `ORRERY_HEIGHT`, `orrery_cols`, `orrery_ansi` + their tests; KEEP `chart_ascii`, `chart_png`, `spectral_color`, and any phase helpers the chart/almanac share)
- Modify: `kernel/src/lib.rs` (remove `mod cast;` and `pub use cast::asciinema_v2;`, line 27)
- Delete: `kernel/src/cast.rs`, `cli/tests/orrery.rs`

**Interfaces:**
- Consumes: nothing from other tasks.
- Produces: a workspace with no `orrery` verb; `hornvale help` no longer lists it. Later tasks rely on `hornvale_scene::{system_scene, system_json, tiles_scene, scene_json}` and `hornvale star-chart` remaining untouched.

- [ ] **Step 1: Map the full consumer set before cutting**

```bash
grep -rn "orrery_ansi\|orrery_cols\|ORRERY_HEIGHT\|GlyphSet\|asciinema_v2\|cast::" --include="*.rs" cli/ domains/ kernel/ windows/
```

Expected consumers: `cli/src/main.rs` (cmd_orrery), `cli/tests/orrery.rs`, `domains/astronomy/src/render.rs` (definitions + unit tests), `kernel/src/lib.rs:27`, `kernel/src/cast.rs`. If anything ELSE appears (e.g. a windows crate), STOP and report — the spec says the removal is render-layer only.

- [ ] **Step 2: Delete the dead files**

```bash
git rm cli/tests/orrery.rs kernel/src/cast.rs
```

- [ ] **Step 3: Cut `cli/src/main.rs`**

Remove: the two `hornvale orrery` help lines (47–48), the `Some("orrery") => cmd_orrery(&args),` match arm, and the whole `cmd_orrery` function (doc comment included). Do not touch `cmd_star_chart` or `cmd_scene`.

- [ ] **Step 4: Cut the orrery half of `domains/astronomy/src/render.rs`**

Remove `Cell`, `GlyphSet` (+ its `impl`), `ORRERY_HEIGHT`, `orrery_cols`, `orrery_ansi`, and every `#[cfg(test)]` test exercising them. Keep everything the star chart uses (`chart_ascii`, `chart_png`, `spectral_color`) and any phase-band helpers shared with the almanac (`phase_eighth` lives in `provider.rs` — untouched). After cutting, `grep -n "orrery" domains/astronomy/src/render.rs` should return only prose comments you then also clean up.

- [ ] **Step 5: Cut the kernel export**

In `kernel/src/lib.rs` remove `mod cast;` and `pub use cast::asciinema_v2;`. Confirm `kernel/examples/first_light.rs` does not reference cast (verified pre-plan; re-verify with `grep -n cast kernel/examples/first_light.rs` — expect no hits).

- [ ] **Step 6: Gate**

```bash
cargo fmt
make gate 2>&1 | tail -20
```

Expected: green. If `cli/tests/architecture.rs` or docs tests complain about paths, the complaint names the file — fix the reference, not the test.

- [ ] **Step 7: Commit**

```bash
git add -A && git commit -m "refactor(orrery)!: remove the terminal orrery — verb, ANSI renderer, glyphs, cast encoder

Nathan's G3 ruling (goldengrove spec §1): the orrery was never a great
fit; Goldengrove inherits the role. Star chart, atlas, and the
scene/system/v1 kind survive. Render-layer only — no streams, facts,
or save-format surface moves.

Claude-Session: https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
```

### Task 2: Remove the live orrery client and its book/CI surface

**Files:**
- Delete: `clients/orrery/` (entire tree), `book/src/gallery/orrery-seed-42.md`, `book/src/gallery/orrery.js`, `book/src/gallery/scene-system-seed-42.json`
- Modify: `book/src/SUMMARY.md` (line 88 `- [The Orrery of Seed 42](./gallery/orrery-seed-42.md)`), `book/src/domains/astronomy.md` (~lines 41–60), `.github/workflows/ci.yml` (orrery job ~lines 86–108; drift list line 57), `scripts/regenerate-artifacts.sh` (line 95 scene-system; lines 113–114 ephemeris golden)

**Interfaces:**
- Consumes: Task 1 (the verb is already gone, so no book page can render it).
- Produces: a book and CI with no orrery references. Later tasks rely on `book/src/gallery/scene-tiles-seed-42.json` REMAINING (atlas food) and `windows/scene/examples/ephemeris_golden.rs` REMAINING (Task 6 harvests its output fixture).

- [ ] **Step 1: Delete the client and artifacts**

```bash
git rm -r clients/orrery
git rm book/src/gallery/orrery-seed-42.md book/src/gallery/orrery.js book/src/gallery/scene-system-seed-42.json
```

- [ ] **Step 2: Book edits**

In `book/src/SUMMARY.md` remove only the gallery line (`The Orrery of Seed 42`). Chronicle entries (`23-the-orrery.md`, `26-the-live-orrery.md`) STAY — history. In `book/src/domains/astronomy.md`, rewrite the orrery paragraphs (~41–60): keep the star-chart prose; replace live-orrery prose with one sentence noting the system view now lives in Goldengrove, the external 3D client (no link yet — it deploys at close; the close's freshness sweep adds it). Then sweep:

```bash
grep -rn "orrery" book/src/ | grep -v chronicle | grep -v idea-registry | grep -v generated
```

Fix every remaining hit the same way (registry rows are handled at close, not now).

- [ ] **Step 3: CI + regen script edits**

In `.github/workflows/ci.yml`: delete the whole `orrery:` job (name "Orrery client" block through its last step, ~lines 86–108); in the "Artifacts are current" drift `git diff --exit-code` list remove `clients/orrery/testdata/`. In `scripts/regenerate-artifacts.sh`: delete line 95 (`scene system … > book/src/gallery/scene-system-seed-42.json`) and the two-line "orrery ephemeris golden" block (lines 113–114). Keep line 94 (scene tiles — atlas). Validate: `yq '.jobs | keys' .github/workflows/ci.yml` and `shellcheck scripts/regenerate-artifacts.sh`.

- [ ] **Step 4: Gate + docs consistency + book build**

```bash
cargo fmt && make gate 2>&1 | tail -5
cargo test -p hornvale --test docs_consistency
mdbook build book
```

Expected: all green (docs_consistency catches any SUMMARY/link damage by name).

- [ ] **Step 5: Commit**

```bash
git add -A && git commit -m "refactor(orrery)!: remove the live orrery client, gallery artifacts, and CI job

clients/orrery, orrery.js, the gallery page, and the committed
scene-system example die; scene-tiles (atlas) and the ephemeris_golden
example (Goldengrove's fixture source) stay.

Claude-Session: https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
```

**Stage boundary:** run `make preflight`; absorb main into the branch if it moved.

---

## Stage B — the catalog

### Task 3: The `hornvale-world-wasm` crate

**Files:**
- Create: `clients/world-wasm/Cargo.toml`, `clients/world-wasm/src/lib.rs`
- Modify: root `Cargo.toml` (add `"clients/world-wasm"` to the workspace `exclude` list, next to `"clients/vessel/wasm"`)

**Interfaces:**
- Consumes: `hornvale_worldgen::{build_world, SkyChoice, SettlementPins, settlement_pins}`, `hornvale_astronomy::{SkyPins, parse_pin}`, `hornvale_terrain::{TerrainPins, parse_pin}`, `hornvale_scene::{system_scene, system_json, tiles_scene, scene_json}`, `hornvale_kernel::Seed`.
- Produces: the `hw_*` ABI later tasks and the client build against:
  `hw_new(seed: u64) -> i32`, `hw_new_pinned(seed: u64, len: usize) -> i32`,
  `hw_scene_system() -> i32`, `hw_scene_tiles(width: u32) -> i32`,
  `hw_in_ptr() -> *mut u8`, `hw_out_ptr() -> *const u8`, `hw_out_len() -> usize`.
  Status codes: 0 ok · 1 genesis refused · 2 scene error · negative = protocol
  (-1 len > 4096, -2 not UTF-8, -3 no world / bad pins JSON). Non-zero puts a
  JSON error envelope `{"error":"…"}` in the out buffer.
  **ABI note (refines spec §3):** the seed rides as a u64 *argument* on both
  entry points, never inside the pins JSON — JSON numbers cannot carry a u64
  faithfully above 2^53.

- [ ] **Step 1: Cargo.toml**

```toml
[package]
name = "hornvale-world-wasm"
version = "0.1.0"
edition = "2024"
license = "MIT"
description = "The catalog: worldgen + scene JSON behind extern-C exports for external clients."

# Standalone workspace (root Cargo.toml excludes this path, like
# clients/vessel/wasm) so this crate owns the size-critical profile.
[workspace]

[lib]
crate-type = ["cdylib"]

[dependencies]
serde_json = "1"
hornvale-kernel = { path = "../../kernel" }
hornvale-worldgen = { path = "../../windows/worldgen" }
hornvale-scene = { path = "../../windows/scene" }
hornvale-astronomy = { path = "../../domains/astronomy" }
hornvale-terrain = { path = "../../domains/terrain" }

[profile.release]
# Vessel's measured profile (decision 0052): ties go to the smaller binary.
opt-level = "z"
lto = true
codegen-units = 1
panic = "abort"
strip = true
```

And in the ROOT `Cargo.toml`, add `"clients/world-wasm"` to the existing `exclude` array.

- [ ] **Step 2: src/lib.rs — the full ABI**

```rust
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
    "moons", "wanderers", "rotation", "day-hours", "obliquity", "year-days", "neighbor", "spin",
];
/// Pin keys routed to `hornvale_terrain::parse_pin`.
const TERRAIN_KEYS: &[&str] = &[
    "plates", "ocean-fraction", "supercontinent", "globe-level", "continents",
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
            _ => return Err(format!("pin '{key}': value must be a string, number, or bool")),
        };
        if key == "sky" {
            pins.choice = match val.as_str() {
                "generated" => SkyChoice::Generated,
                "constant" => SkyChoice::Constant,
                other => return Err(format!("sky: unknown value '{other}'")),
            };
        } else if SKY_KEYS.contains(&key.as_str()) {
            hornvale_astronomy::parse_pin(&format!("{key}={val}"), &mut pins.sky)
                .map_err(|e| format!("{e}"))?;
        } else if TERRAIN_KEYS.contains(&key.as_str()) {
            hornvale_terrain::parse_pin(&format!("{key}={val}"), &mut pins.terrain)
                .map_err(|e| format!("{e}"))?;
        } else if SETTLEMENT_KEYS.contains(&key.as_str()) {
            hornvale_worldgen::settlement_pins::parse_pin(
                &format!("{key}={val}"),
                &mut pins.settlement,
            )
            .map_err(|e| format!("{e}"))?;
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
    match build_world(Seed(seed), &pins.sky, pins.choice, &pins.terrain, &pins.settlement) {
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
#[unsafe(no_mangle)]
pub extern "C" fn hw_new_pinned(seed: u64, len: usize) -> i32 {
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
```

If `hornvale_astronomy::parse_pin` / `hornvale_terrain::parse_pin` /
`settlement_pins::parse_pin` signatures differ slightly (they return
`Result<(), String>` per `cli/src/main.rs:138-194` usage), adjust the
`map_err` shims to match the compiler — the CLI parsers at those lines are
the reference. If `SkyChoice` is not `Copy`, pass `pins.choice.clone()`.

- [ ] **Step 3: Build for wasm + lint**

```bash
rustup target add wasm32-unknown-unknown 2>/dev/null || true
cargo build --manifest-path clients/world-wasm/Cargo.toml --release --target wasm32-unknown-unknown
cargo fmt --check --manifest-path clients/world-wasm/Cargo.toml
cargo clippy --manifest-path clients/world-wasm/Cargo.toml --target wasm32-unknown-unknown -- -D warnings
```

Expected: builds; `target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm` exists.

- [ ] **Step 4: Workspace gate still green (exclusion correct)**

```bash
make gate 2>&1 | tail -5
```

- [ ] **Step 5: Commit**

```bash
git add -A && git commit -m "feat(world-wasm): the catalog crate — seed + pins to scene JSON over extern C

hw_new / hw_new_pinned / hw_scene_system / hw_scene_tiles on the vessel
ABI pattern (0052): no wasm-bindgen, JSON over linear memory, out of the
workspace. Seed rides as a u64 argument, never inside pins JSON.

Claude-Session: https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
```

### Task 4: The golden smoke — `drive.mjs`, `make wasm-world`, `make world-check`

**Files:**
- Create: `clients/world-wasm/drive.mjs`
- Modify: `Makefile` (two targets, added to `.PHONY`)

**Interfaces:**
- Consumes: Task 3's ABI exactly as declared.
- Produces: `make world-check` — the crate's whole local gate (build + lint + byte-identity + size). CI (Task 5) runs it verbatim.

- [ ] **Step 1: Write the failing smoke first**

`clients/world-wasm/drive.mjs`:

```js
// The catalog's golden smoke: wasm scene JSON must be byte-identical to
// the native CLI's (the two-language golden contract at the wasm seam).
// Usage: node drive.mjs <wasm> <native-system.json> <native-tiles.json> \
//                       <tiles-width> <native-pinned-tiles.json>
import { readFileSync } from "node:fs";

const [wasmPath, sysPath, tilesPath, widthStr, pinnedTilesPath] = process.argv.slice(2);
if (!pinnedTilesPath) {
  console.error("usage: node drive.mjs <wasm> <sys.json> <tiles.json> <width> <pinned-tiles.json>");
  process.exit(2);
}
const width = Number(widthStr);
const { instance } = await WebAssembly.instantiate(readFileSync(wasmPath), {});
const e = instance.exports;
const out = () =>
  new TextDecoder().decode(new Uint8Array(e.memory.buffer, e.hw_out_ptr(), e.hw_out_len()));
const fail = (what, detail) => {
  console.error(`world-wasm smoke FAILED — ${what}${detail ? `: ${detail}` : ""}`);
  process.exit(1);
};
const expect = (code, want, what) => {
  if (code !== want) fail(what, `status ${code}: ${out()}`);
};
const golden = (got, path, what) => {
  const want = readFileSync(path, "utf8").trim();
  if (got.trim() !== want) fail(what, `wasm and native JSON differ (native: ${path})`);
};

// Default genesis, both scenes, byte-identical to native.
expect(e.hw_new(42n), 0, "hw_new(42)");
expect(e.hw_scene_system(), 0, "hw_scene_system");
golden(out(), sysPath, "scene/system/v1 (seed 42)");
expect(e.hw_scene_tiles(width), 0, "hw_scene_tiles");
golden(out(), tilesPath, "scene/tiles/v1 (seed 42)");

// Pinned genesis (terrain pin: deterministic force, satisfiable on any seed).
const pins = new TextEncoder().encode(JSON.stringify({ plates: "12" }));
new Uint8Array(e.memory.buffer, e.hw_in_ptr(), pins.length).set(pins);
expect(e.hw_new_pinned(42n, pins.length), 0, "hw_new_pinned(42, plates=12)");
expect(e.hw_scene_tiles(width), 0, "hw_scene_tiles (pinned)");
golden(out(), pinnedTilesPath, "scene/tiles/v1 (seed 42, plates=12)");

// Error paths: unknown pin → -3 with envelope; scene without world intact.
const bad = new TextEncoder().encode(JSON.stringify({ nonsense: "1" }));
new Uint8Array(e.memory.buffer, e.hw_in_ptr(), bad.length).set(bad);
if (e.hw_new_pinned(42n, bad.length) !== -3) fail("unknown pin", "expected -3");
if (!JSON.parse(out()).error) fail("unknown pin", "no error envelope");
// A refused/errored pinned call cleared the world: scenes must refuse too.
if (e.hw_scene_system() !== -3) fail("scene after cleared world", "expected -3");

console.log("world-wasm smoke OK (system + tiles + pinned byte-identical; error envelopes sound)");
```

Note the deliberate semantics the last check pins down: `hw_new_pinned`
clears the prior world before parsing (per Task 3's `genesis`, which
clears first — actually parse errors return before `genesis` runs, so the
old world *survives* a `-3`). **Adjust one or the other:** the simple,
documented behavior is "any `hw_new*` call invalidates the prior world."
Make `hw_new_pinned` clear `WORLD` before parsing (add
`unsafe { *(&raw mut WORLD) = None };` as its first statement, with the
doc comment saying so). The smoke above then passes as written.

- [ ] **Step 2: Make targets**

Add to `Makefile` (mirror the `wasm-vessel`/`vessel-check` pair; add both names to `.PHONY`):

```make
wasm-world: ## Build the world catalog wasm (external clients consume this; never committed)
	rustup target add wasm32-unknown-unknown 2>/dev/null || true
	cargo build --manifest-path clients/world-wasm/Cargo.toml --release --target wasm32-unknown-unknown

world-check: wasm-world ## The catalog's local gate: lint + golden byte-identity smoke + size gate
	cargo fmt --check --manifest-path clients/world-wasm/Cargo.toml
	cargo clippy --manifest-path clients/world-wasm/Cargo.toml --target wasm32-unknown-unknown -- -D warnings
	cargo run -p hornvale -- new --seed 42 --out /tmp/hv-wc.json
	cargo run -p hornvale -- scene system --world /tmp/hv-wc.json > /tmp/hv-wc-system.json
	cargo run -p hornvale -- scene tiles --world /tmp/hv-wc.json --width 256 > /tmp/hv-wc-tiles.json
	cargo run -p hornvale -- new --seed 42 --plates 12 --out /tmp/hv-wc-pinned.json
	cargo run -p hornvale -- scene tiles --world /tmp/hv-wc-pinned.json --width 256 > /tmp/hv-wc-pinned-tiles.json
	node clients/world-wasm/drive.mjs \
	  clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm \
	  /tmp/hv-wc-system.json /tmp/hv-wc-tiles.json 256 /tmp/hv-wc-pinned-tiles.json
	@size=$$(wc -c < clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm); \
	  echo "world wasm size: $$size bytes"; \
	  [ $$size -le 1048576 ] || { echo "SIZE GATE FAILED: > 1 MiB"; exit 1; }
```

(If the standalone crate's target dir is the shared root `target/` instead
of `clients/world-wasm/target/`, use the path `cargo build` actually
produced — `find . -name hornvale_world_wasm.wasm` after Step 3 of Task 3
tells you; vessel's Makefile target at line 140 shows the local-dir shape.)

- [ ] **Step 3: Run it — expect the documented failure first**

```bash
make world-check
```

First run: FAILS at the "scene after cleared world" check (or at the
`hw_new_pinned` clear-semantics mismatch) until Task 3's `hw_new_pinned`
gets the clear-first line described in Step 1. Apply that one-line change.

- [ ] **Step 4: Run to green**

```bash
make world-check
```

Expected final line: `world-wasm smoke OK …` and the size line ≤ 1048576.

- [ ] **Step 5: Commit**

```bash
git add -A && git commit -m "test(world-wasm): golden byte-identity smoke + make world-check (size-gated)

wasm scene JSON == native CLI scene JSON for seed 42, default and
pinned (plates=12); error envelopes checked; <=1 MiB gate.

Claude-Session: https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
```

### Task 5: CI job + tag-triggered release

**Files:**
- Modify: `.github/workflows/ci.yml` (new `world` job, sibling of `vessel`)
- Create: `.github/workflows/release-world-wasm.yml`

**Interfaces:**
- Consumes: `make world-check` (Task 4).
- Produces: release assets `hornvale_world.wasm` + `hornvale_world.wasm.sha256` on tags `world-wasm-v*` — the URLs Task 7's client pins.

- [ ] **Step 1: CI job**

Add to `ci.yml` (after the `vessel` job, same indentation):

```yaml
  world:
    name: World catalog (world-wasm)
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - uses: Swatinem/rust-cache@v2
        with:
          workspaces: |
            .
            clients/world-wasm
      - name: Catalog gate (lint + golden smoke + size)
        run: make world-check
```

- [ ] **Step 2: Release workflow**

`.github/workflows/release-world-wasm.yml`:

```yaml
name: Release world-wasm
on:
  push:
    tags: ["world-wasm-v*"]
permissions:
  contents: write
jobs:
  release:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - name: Gate, then build the asset
        run: make world-check
      - name: Package + checksum
        run: |
          cp clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm hornvale_world.wasm
          shasum -a 256 hornvale_world.wasm > hornvale_world.wasm.sha256
      - name: Create the release
        env:
          GH_TOKEN: ${{ github.token }}
        run: gh release create "$GITHUB_REF_NAME" hornvale_world.wasm hornvale_world.wasm.sha256 --title "$GITHUB_REF_NAME" --notes "The world catalog: hornvale as a wasm library (spec 2026-07-14-goldengrove-design.md). Scene JSON is byte-identical to the native CLI at this commit."
```

(Use the same wasm path fix as Task 4 Step 2 if the target dir differs.)

- [ ] **Step 3: Validate + commit**

```bash
yq '.jobs | keys' .github/workflows/ci.yml
yq '.on.push.tags' .github/workflows/release-world-wasm.yml
git add -A && git commit -m "ci(world-wasm): catalog gate job + tag-triggered release with checksummed asset

Claude-Session: https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
```

**Stage boundary:** `make preflight`; absorb main if it moved. Stage C runs
in the NEW repo — the hornvale branch is now feature-complete until close.

---

## Stage C — the planetarium

*(All tasks below run in `~/Projects/hornvale/goldengrove` once created.
`npm test` + `npm run build` are the per-commit gate. The two harvest
sources are `/Users/nathan/Projects/bitterbridge/goldengrove/web/` and the
hornvale worktree BEFORE Task 2's deletion — use
`git show HEAD~N:clients/orrery/src/scene.ts` style extraction from the
hornvale repo, or copy the files aside before Stage A lands; simplest is
`git -C <hornvale-worktree> show <task1-parent-sha>:clients/orrery/src/X.ts > src/sim/X.ts`.)*

### Task 6: Create the repo and harvest a building skeleton

**Files:**
- Create: repo `hornvale/goldengrove` (public), checkout `~/Projects/hornvale/goldengrove`
- Harvest from bitterbridge/goldengrove `web/`: `package.json`, `package-lock.json`, `tsconfig.json`, `vite.config.ts`, `index.html`, `src/styles.css`, `src/util/prng.ts`(+test), `src/time/clock.ts`(+test), `src/state/url.ts`(+test), `src/ui/hud.ts`(+test), `src/views/cubeSphere.ts`(+test), `src/views/biomePalette.ts`(+test), `src/views/color.ts`
- Harvest from hornvale `clients/orrery/src` (pre-removal blob): `scene.ts`, `ephemeris.ts`, `palette.ts`, `moon.ts` + their `_test.ts` files → `src/sim/`, and `testdata/ephemeris-seed-42.json`
- Create: `README.md`, `LICENSE` (copy bitterbridge/goldengrove's MIT), `src/main.ts` (minimal boot stub)

**Interfaces:**
- Consumes: nothing built yet — pure harvest.
- Produces: a repo where `npm ci && npm test && npm run build` is green, exporting `parseSystem(text): SystemScene`, `parseTiles(text): TilesScene`, `worldPhase/moonPhase/rotationPhase/synodicDays`, `starTint`, `elevationColor`, `cubeSphere` utilities — the vocabulary every later task uses.

- [ ] **Step 1: Create + clone (externally visible; authorized at G3)**

```bash
gh repo create hornvale/goldengrove --public --description "The planetarium: a 3D client for Hornvale worlds" --clone=false
git clone git@github.com:hornvale/goldengrove.git ~/Projects/hornvale/goldengrove
cd ~/Projects/hornvale/goldengrove
```

- [ ] **Step 2: Harvest**

Copy the bitterbridge files listed above preserving relative layout
(`web/src/X` → `src/X`; `web/package.json` → `package.json`). Copy the
orrery TS modules into `src/sim/` and the fixture into `testdata/`. Drop
`three` version to whatever `web/package.json` pins (`^0.166.0`) — keep it.
Convert the orrery modules' Deno-style imports (`./scene.ts` extensioned
imports, `Deno.test`) to the repo's vitest style: rename `X_test.ts` →
`X.test.ts`, replace `Deno.test("name", fn)` with vitest
`import { test, expect } from "vitest"` blocks, and assertions
(`assertEquals(a, b)` → `expect(a).toEqual(b)`; `assertAlmostEquals(a, b, eps)`
→ `expect(a).toBeCloseTo(b, digits)` with digits chosen to match eps;
`assertThrows` → `expect(fn).toThrow()`). The ephemeris test reads the
golden fixture — port its Deno file-read to a vitest `readFileSync` from
`testdata/`.

- [ ] **Step 3: Minimal `src/main.ts` boot stub**

```ts
// Boot stub: proves the toolchain; Task 7 replaces this with the worker
// pipeline and Task 8+ with the views.
const el = document.createElement("pre");
el.textContent = "goldengrove: awaiting a world";
document.body.append(el);
```

Remove harvested files' imports of anything NOT harvested (`sim/wasm.ts`,
`sim/types.ts`, gg views that need the old Sim). If a harvested view
module imports a missing type, do not harvest that module yet — Tasks 8–9
bring views in with their rewiring. The skeleton's rule: **everything
committed compiles and its tests pass.**

- [ ] **Step 4: README with lineage**

```markdown
# goldengrove

The planetarium: a glamorous 3D client for [Hornvale](https://github.com/hornvale/hornvale)
worlds. Enter a seed; genesis runs in your browser via the world-wasm
catalog; the system and the globe render in three.js.

Lineage: the three.js views descend from bitterbridge/goldengrove; the
ephemeris/scene modules from hornvale's retired in-book orrery client.
Presentation here is deliberately non-deterministic (hornvale decision
0022: the sim emits data, clients render). MIT.
```

- [ ] **Step 5: Gate + first push**

```bash
npm ci && npm test && npm run build
git add -A && git commit -m "chore: bootstrap — harvest views (bitterbridge/goldengrove) + sim modules (hornvale clients/orrery)

Claude-Session: https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
git push -u origin main
```

### Task 7: The data layer — catalog loader, worker, dev wasm

**Files:**
- Create: `src/sim/catalog.ts`, `src/sim/worker.ts`, `src/sim/catalog.test.ts`
- Modify: `src/main.ts`, `package.json` (script `wasm:local`), `vite.config.ts` (worker format es if needed)

**Interfaces:**
- Consumes: the `hw_*` ABI (Task 3), `parseSystem`/`parseTiles` (Task 6).
- Produces:
  ```ts
  // catalog.ts
  export class CatalogError extends Error { constructor(msg: string, public code: number) } 
  export interface Catalog {
    generate(seed: bigint, pins?: Record<string, string>): void; // throws CatalogError on refusal
    sceneSystem(): string;   // raw JSON text
    sceneTiles(width: number): string;
  }
  export async function loadCatalog(wasmUrl: string): Promise<Catalog>;
  // worker.ts protocol
  // in : { type: "generate", seed: string, pins?: Record<string,string>, tilesWidth: number }
  // out: { type: "world", system: SystemScene, tiles: TilesScene }
  //    | { type: "error", message: string }
  ```

- [ ] **Step 1: Failing test for the buffer protocol**

`src/sim/catalog.test.ts` — test the pure helpers with a fake exports
object (no real wasm in unit tests; the real wasm is exercised by the
smoke in Step 4):

```ts
import { expect, test } from "vitest";
import { readOut, writeIn, statusError } from "./catalog";

function fakeExports(outText: string) {
  const mem = new WebAssembly.Memory({ initial: 1 });
  const bytes = new TextEncoder().encode(outText);
  new Uint8Array(mem.buffer, 64, bytes.length).set(bytes);
  return {
    memory: mem,
    hw_in_ptr: () => 0,
    hw_out_ptr: () => 64,
    hw_out_len: () => bytes.length,
  };
}

test("readOut decodes the out buffer", () => {
  expect(readOut(fakeExports("hello") as never)).toBe("hello");
});

test("writeIn round-trips UTF-8 through the in buffer", () => {
  const e = fakeExports("");
  const n = writeIn(e as never, '{"plates":"12"}');
  expect(new TextDecoder().decode(new Uint8Array(e.memory.buffer, 0, n))).toBe('{"plates":"12"}');
});

test("statusError surfaces the envelope", () => {
  const err = statusError(1, '{"error":"the genesis of seed 7 refused: …"}');
  expect(err.code).toBe(1);
  expect(err.message).toContain("refused");
});
```

Run `npm test` — FAILS (module missing).

- [ ] **Step 2: Implement `catalog.ts`**

```ts
// The catalog: hornvale's world-wasm behind a typed loader. The ABI is
// hw_* over linear memory (hornvale spec 2026-07-14-goldengrove §3).
export class CatalogError extends Error {
  constructor(message: string, public code: number) {
    super(message);
  }
}

interface HwExports {
  memory: WebAssembly.Memory;
  hw_new(seed: bigint): number;
  hw_new_pinned(seed: bigint, len: number): number;
  hw_scene_system(): number;
  hw_scene_tiles(width: number): number;
  hw_in_ptr(): number;
  hw_out_ptr(): number;
  hw_out_len(): number;
}

export function readOut(e: HwExports): string {
  return new TextDecoder().decode(new Uint8Array(e.memory.buffer, e.hw_out_ptr(), e.hw_out_len()));
}

export function writeIn(e: HwExports, text: string): number {
  const bytes = new TextEncoder().encode(text);
  if (bytes.length > 4096) throw new CatalogError("pins JSON exceeds the 4096-byte buffer", -1);
  new Uint8Array(e.memory.buffer, e.hw_in_ptr(), bytes.length).set(bytes);
  return bytes.length;
}

export function statusError(code: number, outText: string): CatalogError {
  let message = outText;
  try {
    const env = JSON.parse(outText);
    if (typeof env?.error === "string") message = env.error;
  } catch {
    /* raw text is the message */
  }
  return new CatalogError(message, code);
}

export interface Catalog {
  generate(seed: bigint, pins?: Record<string, string>): void;
  sceneSystem(): string;
  sceneTiles(width: number): string;
}

export async function loadCatalog(wasmUrl: string): Promise<Catalog> {
  const res = await fetch(wasmUrl);
  if (!res.ok) throw new CatalogError(`catalog fetch failed: ${res.status} ${wasmUrl}`, -100);
  const { instance } = await WebAssembly.instantiate(await res.arrayBuffer(), {});
  const e = instance.exports as unknown as HwExports;
  const check = (code: number) => {
    if (code !== 0) throw statusError(code, readOut(e));
  };
  return {
    generate(seed, pins) {
      if (pins && Object.keys(pins).length > 0) {
        check(e.hw_new_pinned(seed, writeIn(e, JSON.stringify(pins))));
      } else {
        check(e.hw_new(seed));
      }
    },
    sceneSystem() {
      check(e.hw_scene_system());
      return readOut(e);
    },
    sceneTiles(width) {
      check(e.hw_scene_tiles(width));
      return readOut(e);
    },
  };
}
```

- [ ] **Step 3: Implement `worker.ts` + wire `main.ts`**

```ts
// worker.ts — genesis off the main thread (it takes seconds).
import { loadCatalog } from "./catalog";
import { parseSystem, parseTiles } from "./scene";

self.onmessage = async (ev: MessageEvent) => {
  const { seed, pins, tilesWidth } = ev.data;
  try {
    const catalog = await loadCatalog(new URL("/hornvale_world.wasm", self.location.origin).href);
    catalog.generate(BigInt(seed), pins);
    const system = parseSystem(catalog.sceneSystem());
    const tiles = parseTiles(catalog.sceneTiles(tilesWidth));
    self.postMessage({ type: "world", system, tiles });
  } catch (err) {
    self.postMessage({ type: "error", message: err instanceof Error ? err.message : String(err) });
  }
};
```

`main.ts`: spawn the worker (`new Worker(new URL("./sim/worker.ts", import.meta.url), { type: "module" })`),
post `{ type: "generate", seed: "42", tilesWidth: 512 }`, render "generating…"
until the reply, then dump `system.star.class_name` + tile dimensions into
the stub `<pre>` (views replace this in Task 8). Genesis-refused replies
render the message verbatim — the sim's physical reason is the UI copy.

- [ ] **Step 4: Dev wasm + end-to-end check**

`package.json` script (dev convenience; CI fetches the release instead):

```json
"wasm:local": "cp ../hornvale/clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm public/hornvale_world.wasm"
```

(Path is relative to the sibling checkout layout Nathan chose; document it
in the README. Add `public/hornvale_world.wasm` to `.gitignore` — the
catalog is never committed, decision 0052's regime.)

```bash
npm run wasm:local && npm test && npm run build && npx vite preview &
# open the preview URL: expect the star line for seed 42 after genesis.
```

- [ ] **Step 5: Commit + push**

```bash
git add -A && git commit -m "feat(sim): catalog loader + genesis worker over the hw_* ABI

Claude-Session: https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
git push
```

### Task 8: The system view

**Files:**
- Create: `src/views/system.ts`, `src/views/system.test.ts`
- Modify: `src/main.ts` (mount the view), `src/ui/hud.ts` (scrubber wiring)

**Interfaces:**
- Consumes: `SystemScene` (Task 6 types), `worldPhase/moonPhase/rotationPhase/synodicDays` (harvested ephemeris — golden-pinned), `starTint`, gg `clock.ts`, three.js.
- Produces: `createSystemView(scene: SystemScene): { object3d: THREE.Object3D; update(day: number): void; worldPosition(day: number): THREE.Vector3 }` — Task 10's zoom targets `worldPosition`.

- [ ] **Step 1: Failing test for the layout math (pure, no WebGL)**

```ts
import { expect, test } from "vitest";
import { orbitAngle, moonLocalPosition } from "./system";
import type { SystemScene } from "../sim/scene";

const sys = {
  schema: "scene/system/v1",
  seed: 42,
  star: { class_name: "yellow dwarf (G)", luminosity_rel: 1, hz_inner_au: 0.95, hz_outer_au: 1.4 },
  world: { orbit_au: 1, year_days: 360, day_length_days: 1, obliquity_deg: 20, year_phase_offset: 0.25 },
  moons: [{ sidereal_days: 30, phase_offset: 0, distance_mm: 384, size_rel: 1 }],
} as SystemScene;

test("orbitAngle honors the genesis phase offset", () => {
  // worldPhase(sys, 0) = 0.25 turns → angle π/2.
  expect(orbitAngle(sys, 0)).toBeCloseTo(Math.PI / 2, 10);
});

test("a moon returns to its phase after one sidereal period", () => {
  const a = moonLocalPosition(sys, 0, 0);
  const b = moonLocalPosition(sys, 0, 30);
  expect(a.x).toBeCloseTo(b.x, 8);
  expect(a.z).toBeCloseTo(b.z, 8);
});
```

- [ ] **Step 2: Implement**

`orbitAngle(sys, day) = worldPhase(sys, day) * 2π` (the harvested,
golden-pinned `worldPhase` is the single source of angular truth — never
reimplement it). `moonLocalPosition(sys, i, day)` from `moonPhase` and a
schematic radial ladder (moons at compressed distances with an on-screen
legend — ORRERY-scale-honesty's lesson: admit the lie in a caption).
`createSystemView` builds: star sphere tinted `starTint(class_name)` +
point light + bloom-friendly emissive; HZ annulus (translucent ring
between `hz_inner_au`/`hz_outer_au`); orbit line circles; world sphere;
moon spheres. `update(day)` repositions from the ephemeris; the HUD
scrubber drives `day` via gg's `clockToDay`. Free-hand glamour (starfield
sprinkle from gg `starfield.ts` with client-side PRNG, lens flare) is
licensed — it must not read the scene for anything but seeding flavor.

- [ ] **Step 3: Gate, wire, verify visually**

```bash
npm test && npm run build && npm run wasm:local && npx vite preview
```

Seed 42: star + HZ band + orbiting world + its moons animate under the scrubber.

- [ ] **Step 4: Commit + push**

```bash
git add -A && git commit -m "feat(views): the system view — golden-pinned ephemeris drives three.js orbits

Claude-Session: https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
git push
```

### Task 9: The globe view

**Files:**
- Create: `src/views/globe.ts`, `src/views/globe.test.ts`
- Modify: `src/main.ts`

**Interfaces:**
- Consumes: `TilesScene`, gg `cubeSphere.ts`, `biomePalette.ts`/`color.ts`, orrery `elevationColor`, `rotationPhase` + `worldPhase` + `obliquity_deg` (terminator).
- Produces: `createGlobeView(tiles: TilesScene, sys: SystemScene): { object3d: THREE.Object3D; update(day: number): void }`.

- [ ] **Step 1: Failing tests for sampling + terminator**

```ts
import { expect, test } from "vitest";
import { sampleTile, subsolarPoint } from "./globe";

test("sampleTile maps lat/lon to the row-major equirect lattice", () => {
  // 4×2 lattice: row 0 is lat +90..0, col 0 is lon -180.
  const tiles = { width: 4, height: 2, elevation_m: [0, 1, 2, 3, 4, 5, 6, 7] } as never;
  expect(sampleTile(tiles, 45, -180, "elevation_m")).toBe(0);
  expect(sampleTile(tiles, -45, 90, "elevation_m")).toBe(7);
});

test("subsolar latitude swings ±obliquity over the year", () => {
  const sys = { world: { obliquity_deg: 20, year_days: 360, year_phase_offset: 0, day_length_days: 1 } } as never;
  const lats = [0, 90, 180, 270].map((d) => subsolarPoint(sys, d).lat);
  expect(Math.max(...lats)).toBeCloseTo(20, 5);
  expect(Math.min(...lats)).toBeCloseTo(-20, 5);
});
```

(Exact convention: `scene/tiles/v1` is row-major, top row first, lat 90→−90,
lon −180→180, pixel centers — `windows/scene/src/lib.rs:68-71`. Encode that
in `sampleTile` and let the test values follow; if your pixel-center math
shifts the expected indices, fix the TEST values to the convention, never
the convention.)

- [ ] **Step 2: Implement**

Cube-sphere mesh (gg `cubeSphere.ts`) displaced by
`sampleTile(tiles, lat, lon, "elevation_m")` with a declared exaggeration
constant (`RELIEF_EXAGGERATION = 60`, shown in the HUD legend — spec §4½:
the render admits its lie). Vertex colors: ocean tiles → depth-shaded blue
(`elevationColor`), land → `biomePalette` keyed through `biome_legend`
names. `features` → small markers with name sprites. Terminator:
`subsolarPoint(sys, day)` from obliquity + year phase (+ rotation phase for
longitude when `day_length_days` is non-null; frozen longitude when locked)
feeding a directional light; night side falls to shader darkness.
`update(day)` moves the light and spins the mesh by `rotationPhase`.

- [ ] **Step 3: Gate + visual check, Step 4: Commit + push**

```bash
npm test && npm run build && npm run wasm:local && npx vite preview
git add -A && git commit -m "feat(views): the globe — real relief, biome palette, honest terminator

Claude-Session: https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
git push
```

### Task 10: The zoom, URL state, and error surfaces

**Files:**
- Create: `src/views/zoom.ts`
- Modify: `src/main.ts`, `src/state/url.ts` (+ its test)

**Interfaces:**
- Consumes: both views' `object3d`/`update`, `worldPosition` (Task 8), gg `url.ts`.
- Produces: the shipped v1 UX — `#seed=<u64>&view=system|globe&day=<f64>` round-tripping.

- [ ] **Step 1: URL state test first** — extend the harvested `url.ts` test: parse/serialize `{ seed: "1337", view: "globe", day: 118.3 }`; unknown params ignored; bad seed (non-integer) yields a parse error the UI can show.
- [ ] **Step 2: Implement** — one shared `requestAnimationFrame` loop owns `day`; the zoom is a camera dolly from system framing to `worldPosition(day)` framing (ease in/out, ~1.5 s), cross-fading view groups. Error surfaces: genesis refusal (worker `error` reply) renders the sim's reason full-screen with the seed named; catalog fetch failure names the wasm URL; `parse.ts` schema mismatch prints both schema strings.
- [ ] **Step 3: Gate + visual check** (`npm test && npm run build`, preview: seed round-trips through the URL, zoom animates, `#seed=abc` shows the parse error).
- [ ] **Step 4: Commit + push** (message: `feat: zoom ladder, deep-linkable URL state, honest error surfaces`, with the session trailer).

### Task 11: First catalog release + Pages deploy

**Files:**
- Create (goldengrove): `.github/workflows/deploy.yml`
- Modify (goldengrove): `README.md` (pinned catalog version), `package.json` (script `wasm:release`)
- Tag (hornvale): `world-wasm-v1`

**Interfaces:**
- Consumes: Task 5's release workflow; Tasks 6–10's green build.
- Produces: the live instrument at `https://hornvale.github.io/goldengrove/`.

- [ ] **Step 1: Cut the first catalog release (externally visible; authorized at G3)** — from the hornvale worktree, AFTER Stage B is merged to main at close-time **or** from the campaign branch if Nathan wants the deploy live pre-merge: `git tag world-wasm-v1 && git push origin world-wasm-v1`; verify the release assets appear (`gh release view world-wasm-v1 --repo hornvale/hornvale`). If the campaign closes before tagging, fold this step into the close checklist — the deploy cannot precede the catalog.
- [ ] **Step 2: `wasm:release` script** —

```json
"wasm:release": "curl -fL -o public/hornvale_world.wasm https://github.com/hornvale/hornvale/releases/download/world-wasm-v1/hornvale_world.wasm && curl -fL https://github.com/hornvale/hornvale/releases/download/world-wasm-v1/hornvale_world.wasm.sha256 | shasum -a 256 -c -"
```

(The checksum file names `hornvale_world.wasm`; run the check from the
download dir or rewrite the path with `sed` — verify locally once and pin
whichever form passes.)
- [ ] **Step 3: `deploy.yml`** —

```yaml
name: Deploy
on:
  push:
    branches: [main]
permissions:
  contents: read
  pages: write
  id-token: write
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with: { node-version: 22 }
      - run: npm ci
      - run: npm test
      - run: npm run wasm:release
      - run: npm run build -- --base=/goldengrove/
      - uses: actions/upload-pages-artifact@v3
        with: { path: dist }
  deploy:
    needs: build
    runs-on: ubuntu-latest
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    steps:
      - id: deployment
        uses: actions/deploy-pages@v4
```

Enable Pages (source: GitHub Actions) via `gh api repos/hornvale/goldengrove/pages -X POST -f build_type=workflow` (or the settings UI).
- [ ] **Step 4: Verify live** — push, watch the run, open `https://hornvale.github.io/goldengrove/#seed=42`: genesis runs, both views render. Then try a second seed from the URL.
- [ ] **Step 5: Commit + push** (message: `ci: Pages deploy pinned to world-wasm-v1`, session trailer).

### Task 12: Close prep in hornvale (book pointer, registry, decision)

**Files (hornvale worktree):**
- Modify: `book/src/domains/astronomy.md` (add the live link), `book/src/gallery/` index if one lists exhibits, `book/src/frontier/idea-registry.md` (ORRERY-* rows), `docs/decisions/00XX-external-clients-consume-a-versioned-wasm-catalog.md` (next free number at merge time)

**Interfaces:**
- Consumes: the live URL (Task 11).
- Produces: the merged-reality book + durable decision record; feeds `closing-a-campaign` (chronicle, retro, freshness sweep, G6 — run via that skill, not this plan).

- [ ] **Step 1: Book pointer** — astronomy.md's rewritten paragraph gains the link (`https://hornvale.github.io/goldengrove/`); sweep `grep -rn "orrery" book/src/ | grep -v chronicle` once more.
- [ ] **Step 2: Registry rows** — in the ORRERY enrichment table: mark `ORRERY-any-world`, `ORRERY-deep-link`, `ORRERY-semantic-zoom` → `shipped` (Goldengrove); re-point still-open rows' provenance to Goldengrove where the terminal orrery was named. Add one new row: `RENDER-6 | The world-wasm catalog — hornvale as a versioned wasm library (seed+pins → scene JSON) for external clients; one catalog, many planetaria | shipped | high | goldengrove spec`.
- [ ] **Step 3: Decision record** — draft `external clients consume Hornvale as a versioned wasm catalog` (repo boundary = determinism boundary; scene schemas are cross-repo contracts; assets on `world-wasm-v*` tags), decision-log format, next free number checked at merge.
- [ ] **Step 4: Gate + docs consistency + commit** (`make gate`, `cargo test -p hornvale --test docs_consistency`; message: `docs(goldengrove): book pointer, registry flips, catalog decision record`, session trailer). Then invoke `closing-a-campaign` for chronicle/retro/sweep/G6 — Nathan's merge stop.

---

## Self-review notes (G4, autopilot)

- **Spec coverage:** §1 removal → Tasks 1–2; §3 catalog → Tasks 3–5 (ABI refined: seed as argument — ledgered); §4 client → Tasks 6–10; §4 deploy → Task 11; §5 errors → Tasks 3/7/10; §6 testing → Tasks 4/6/8/9; §8 decisions → Task 12. §7 non-goals: no task touches ground/sky, neighbors, MAP-23, npm, star chart, atlas. Gap check: spec §4's "harvest starfield" appears only as licensed sprinkle in Task 8 — deliberate (no neighbor data).
- **Type consistency:** `hw_*` names match across Tasks 3/4/7; `SystemScene`/`TilesScene` come from the single harvested `scene.ts`; `worldPosition` produced (T8) before consumed (T10).
- **Known softness, accepted:** wasm target-dir path (both affected steps carry the same probe instruction); Deno→vitest port is mechanical but listed translation-by-translation in Task 6.
