//! The Cistern's structural guard: the world catalog's terrain-facing scene
//! exports must route through the `_in` variants, reusing the one
//! `SceneContext` the catalog holds per world.
//!
//! Why a source scan and not a behavioural test: `clients/world-wasm` is a
//! standalone workspace (its own manifest, `opt-level = "z"`, `crate-type =
//! ["cdylib"]`), so this workspace's test binaries cannot link it, and its
//! state lives in `static mut`s behind an `extern "C"` surface only a wasm
//! host can drive. The behavioural half of the net is
//! `clients/world-wasm/drive.mjs` (run by `make world-check` and CI); this
//! test is the half that runs on every `make gate`, milliseconds cheap.

use std::fs;
use std::path::{Path, PathBuf};

/// The catalog source this test governs, relative to the repo root.
const CATALOG: &str = "clients/world-wasm/src/lib.rs";

/// The repo root: the parent of this crate's manifest dir (`cli/`).
/// Filesystem-based, not git-based — the suite also runs in rsync'd trees
/// that are not git repositories (same reasoning as `heavy_tier.rs`).
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ has a parent")
        .to_path_buf()
}

/// Why this rule exists, appended to every failure message. A guard whose
/// failure does not teach is a guard someone deletes.
const WHY: &str = "\n\n\
    The catalog must build ONE `hornvale_scene::SceneContext` per world — the \
    `SCENE_CTX` static, reached ONLY through the `scene_ctx` accessor — and \
    pass it to the `_in` variants. Two things have to hold, and calling an \
    `_in` variant is only the first: the context handed to it must be the \
    REUSED one. A scene export that builds its own context and then passes it \
    to an `_in` variant is byte-identical and just as slow as the `&World` \
    form — it is the regression this guard exists to catch, and no golden or \
    equivalence test can see it, because only the cost changes.\n\
    Deriving a context is terrain plus climate plus both nearest-cell indices, \
    ~638 ms, 91.6% of a region patch (The Sextant's measurement), so a \
    per-call rebuild re-derives the entire planet on every scene request the \
    client makes — precisely the cost The Cistern removed (11.1x on region \
    tiles). See docs/superpowers/specs/2026-07-28-the-cistern-design.md.\n\
    If you are deliberately changing this contract, change the spec first.";

/// The catalog's terrain-facing scene exports, by the signature line each
/// one's body is sliced from. These are the exports whose cost is dominated
/// by context derivation; the terrain-free ones (`system`, `moons`,
/// `neighbors`, `eclipses`) take no context and are not governed here.
const TERRAIN_EXPORTS: &[(&str, &str)] = &[
    (
        "hw_scene_tiles",
        "pub extern \"C\" fn hw_scene_tiles(width: u32) -> i32 {",
    ),
    (
        "hw_scene_tiles_selected",
        "pub extern \"C\" fn hw_scene_tiles_selected(width: u32, len: usize) -> i32 {",
    ),
    (
        "hw_scene_tiles_region",
        "pub extern \"C\" fn hw_scene_tiles_region(",
    ),
];

#[test]
fn the_catalog_reuses_one_scene_context_per_world() {
    let path = repo_root().join(CATALOG);
    let text =
        fs::read_to_string(&path).unwrap_or_else(|e| panic!("{CATALOG} is readable ({e}){WHY}"));

    // The `_in` variants must be the ones in use.
    for wanted in ["tiles_scene_in", "tiles_region_scene_in"] {
        assert!(
            text.contains(wanted),
            "{CATALOG} no longer calls `{wanted}`.{WHY}"
        );
    }

    // And the context-deriving `&World` forms must not be. The trailing `(`
    // is load-bearing: it is what distinguishes `tiles_scene(` from
    // `tiles_scene_in(`.
    for banned in [
        "hornvale_scene::tiles_scene(",
        "hornvale_scene::tiles_region_scene(",
    ] {
        assert!(
            !text.contains(banned),
            "{CATALOG} calls `{banned}` — the context-deriving `&World` form.{WHY}"
        );
    }

    // Calling an `_in` variant is necessary but NOT sufficient: an export can
    // call `SceneContext::build` itself and hand the result to the `_in`
    // variant, which is byte-identical, passes every assertion above, and is
    // exactly as slow as the `&World` form. So each terrain-facing export
    // must reach the shared context through the `scene_ctx` accessor.
    for (name, signature) in TERRAIN_EXPORTS {
        let body = fn_body(&text, signature);
        assert!(
            body.contains("scene_ctx("),
            "{CATALOG}'s `{name}` does not call `scene_ctx(` — it is not reusing \
             the per-world context.{WHY}"
        );
    }

    // ...and the accessor must be the ONLY place a context is built. This is
    // the assertion that closes the loop: with exactly one `SceneContext::build`
    // in the file, and it inside `scene_ctx`, no export can have grown a
    // private derivation.
    let builds = text.matches("SceneContext::build").count();
    assert_eq!(
        builds, 1,
        "{CATALOG} contains {builds} `SceneContext::build` call(s); exactly one \
         is allowed, inside the `scene_ctx` accessor.{WHY}"
    );
    let accessor = fn_body(
        &text,
        "fn scene_ctx(world: &World) -> Result<&'static SceneContext, hornvale_scene::SceneError> {",
    );
    assert!(
        accessor.contains("SceneContext::build"),
        "{CATALOG}'s one `SceneContext::build` is not inside the `scene_ctx` \
         accessor.{WHY}"
    );

    // The context is only sound because it is dropped with the world it
    // describes. Both `hw_new*` entry points clear it; if either stops, a
    // scene call can serve the previous planet's terrain under the new seed.
    assert!(
        text.contains("static mut SCENE_CTX"),
        "{CATALOG} no longer holds a `SCENE_CTX` static.{WHY}"
    );
    let clears = text.matches("*ctx_ptr = None").count();
    assert!(
        clears >= 2,
        "{CATALOG} clears `SCENE_CTX` in {clears} place(s); both `genesis` and \
         `hw_new_pinned` must clear it.{WHY}"
    );

    // ...and PLACEMENT, which the count above cannot see. Spec §3.3: the
    // clear happens in the same statement region as `WORLD`'s, never in a
    // later branch and never only on the success path. Both mutations that
    // break the invariant — sinking the clear into `hw_new_pinned`'s
    // `Ok(pins)` arm, or below its `-1`/`-2` returns — leave the count at 2,
    // so only an index comparison enforces what the message claims.
    //
    // `genesis`: the clear must precede the world it would otherwise outlive.
    assert_before(
        &fn_body(&text, "fn genesis(seed: u64, pins: &Pins) -> i32 {"),
        "genesis",
        "*ctx_ptr = None",
        "build_world(",
        "a context built for the OLD world must be dropped before the new one \
         is built, or the first scene call after genesis serves the previous \
         planet",
    );
    // `hw_new_pinned`: the clear must precede the FIRST early return, which
    // puts it ahead of all three (-1 length, -2 UTF-8, -3 bad pins).
    assert_before(
        &fn_body(
            &text,
            "pub extern \"C\" fn hw_new_pinned(seed: u64, len: usize) -> i32 {",
        ),
        "hw_new_pinned",
        "*ctx_ptr = None",
        "return -1",
        "any hw_new* call invalidates the prior world FULL STOP; a clear that \
         sits after an early return (or only in the `Ok(pins)` arm) leaves a \
         refused pinned call holding a context for the previous planet",
    );
}

/// The body text of the function whose signature line is `signature`: from
/// that signature to the next top-level `}` (a line that is exactly `}`,
/// which rustfmt guarantees for an item's closing brace). Panics loudly if
/// the signature moved — a placement assertion that silently stops finding
/// its subject is worse than no assertion.
fn fn_body(text: &str, signature: &str) -> String {
    let start = text.find(signature).unwrap_or_else(|| {
        panic!(
            "{CATALOG} no longer contains the signature `{signature}`; this test \
             slices that function's body to check WHERE `SCENE_CTX` is cleared, \
             so it cannot verify the invariant until the signature here is \
             updated to match.{WHY}"
        )
    });
    let rest = &text[start..];
    let end = rest.find("\n}\n").map_or(rest.len(), |i| i + 2);
    rest[..end].to_string()
}

/// Assert `first` appears before `second` in `body`, with both required to
/// be present. `what` explains the consequence of the wrong order.
fn assert_before(body: &str, func: &str, first: &str, second: &str, what: &str) {
    let at_first = body
        .find(first)
        .unwrap_or_else(|| panic!("{CATALOG}'s `{func}` does not contain `{first}`.{WHY}"));
    let at_second = body.find(second).unwrap_or_else(|| {
        panic!(
            "{CATALOG}'s `{func}` does not contain `{second}`, so this test can no \
             longer tell whether `{first}` precedes it.{WHY}"
        )
    });
    assert!(
        at_first < at_second,
        "{CATALOG}'s `{func}` has `{first}` AFTER `{second}` — {what}.{WHY}"
    );
}
