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
    The catalog must build ONE `hornvale_scene::SceneContext` per world (the \
    `SCENE_CTX` static) and pass it to the `_in` variants. The `&World` forms \
    (`tiles_scene`, `tiles_region_scene`) derive a fresh context internally — \
    terrain plus climate plus both nearest-cell indices, ~638 ms, 91.6% of a \
    region patch (The Sextant's measurement) — so calling one from the catalog \
    re-derives the entire planet on every scene request the client makes, \
    which is precisely the cost The Cistern removed. See \
    docs/superpowers/specs/2026-07-28-the-cistern-design.md.\n\
    If you are deliberately changing this contract, change the spec first.";

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
