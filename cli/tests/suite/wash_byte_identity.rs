//! **H3, The Wash's acceptance gate.** The claim under test: appearance is
//! derived, never committed, never read back. The campaign moved the world
//! map onto the kernel's spectral colour pipeline (`clients/game`), and the
//! only non-client production change it made is
//! `windows/locale/src/lib.rs` — an extraction of the wetness-grounding
//! block `describe_with_weights` already ran into a shared helper
//! (`grounded_wetness_for`), plus a shared corner-blend helper
//! (`blend_at_corners`) and two new facet-level entry points
//! (`reflectance_at_facet`, `reflectance_at_facet_cached`) the plate calls.
//! If that extraction had changed so much as a rounding step, it would move
//! every shipped surface `describe_with_weights` already feeds — silently,
//! because nothing else in the gate reads this file's own committed
//! artifacts on `cli`'s behalf.
//!
//! **This copies `repose_byte_identity.rs`'s shape rather than inventing a
//! second one** (that file's own module doc explains why `cli/` hosts a
//! workspace-wide byte-identity claim: it is the one crate that depends on
//! everything). What differs is the fixture: The Repose's claim is about
//! *world generation* (`build_world`'s output never moves), which this
//! campaign never touches — `windows/locale` is downstream of `World`,
//! reads it through `&self`, and cannot write back to it by construction.
//! The Wash's actual exposure is different: does the extracted composition
//! still emit the SAME bytes `describe_with_weights` emitted before the
//! refactor? A test asserting `World` itself is unmoved by calling the new
//! read-only methods would be true by the type system alone and could never
//! go red under any perturbation to this file — the memory note on file is
//! "a check that can never fire is worse than an absent one" — so this file
//! does not carry one.
//!
//! **The fixture chosen already reaches the changed code, on purpose.**
//! `windows/scene/tests/fixtures/surrounds-seed-42-flagship.json` is a
//! `scene/surrounds/v2` document built through `LocaleContext::describe`
//! (via `surrounds_scene`), and its per-address `micro.wetness` field is
//! exactly `grounded_wetness_for`'s output composed through
//! `blend_at_corners`'s moisture blend — the two pieces of arithmetic this
//! campaign extracted. `windows/scene`'s own `golden.rs` already pins these
//! bytes and already runs in every stage gate; this is a **second reader**
//! of the identical committed file, from the crate that owns the
//! workspace-wide invariant, the same duplication `repose_byte_identity.rs`
//! accepts for its own world-JSON probe against `lens_purity.rs`.
//!
//! **Deliberately not `hornvale_kernel::golden::assert_golden`.** That
//! fixture already has a designated writer
//! (`REBASELINE=1 cargo test -p hornvale-scene --test suite -- golden`,
//! per `windows/scene/tests/suite/golden.rs`'s own module doc). Comparing
//! with `assert_golden` a second time from here would give this crate's
//! `REBASELINE=1` run write access to the same file, which could then
//! silently accept a divergence between the two call sites — the exact
//! hazard `repose_byte_identity.rs::assert_committed_bytes` names for its
//! own probes. A plain string comparison has no such footgun.

use hornvale_kernel::{Value, WorldTime, math::unit_sphere_from_lat_lon};
use hornvale_locale::{LocaleContext, walk_depth};
use hornvale_scene::{surrounds_json, surrounds_scene};

/// The repository root: the parent of this crate's manifest dir (`cli/`).
fn repo_root() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ has a parent")
        .to_path_buf()
}

/// Seed 42 under the generated sky, default pins, full depth — byte-identical
/// to what `windows/scene/tests/suite/golden.rs::seed_42_world` builds, read
/// from the committed fixture instead (decision 0607) so this file adds no
/// new row to `cli/tests/fixtures/world-build-sites.tsv`.
fn seed_42_world() -> hornvale_kernel::World {
    hornvale_worldgen::seed_42_world()
}

/// The flagship settlement's lat/lon, read from the world's own facts —
/// duplicated from `windows/scene/tests/suite/golden.rs::flagship_latlon`
/// (an integration test can't reach that crate's private helpers either).
fn flagship_latlon(world: &hornvale_kernel::World) -> (f64, f64) {
    let v = hornvale_settlement::village_info(world).expect("seed 42 has a village");
    let lat = match world.ledger.value_of(v.id, hornvale_settlement::LATITUDE) {
        Some(Value::Number(n)) => *n,
        _ => panic!("flagship settlement has no latitude fact"),
    };
    let lon = match world.ledger.value_of(v.id, hornvale_settlement::LONGITUDE) {
        Some(Value::Number(n)) => *n,
        _ => panic!("flagship settlement has no longitude fact"),
    };
    (lat, lon)
}

/// The seed-42 flagship `scene/surrounds/v2` document, built exactly as
/// `windows/scene/tests/suite/golden.rs::surrounds_seed_42_flagship_json`
/// builds it.
fn surrounds_seed_42_flagship_json() -> String {
    let w = seed_42_world();
    let ctx = LocaleContext::build(&w).expect("seed 42 builds a locale context");
    let depth = walk_depth(&ctx);
    let (lat, lon) = flagship_latlon(&w);
    let observer = hornvale_kernel::Facet::containing(unit_sphere_from_lat_lon(lat, lon), depth);
    surrounds_json(&surrounds_scene(&w, &observer, 4, WorldTime::GENESIS).unwrap())
}

/// Compare a rendered artifact against its committed bytes, naming the
/// first divergence. Copied from `repose_byte_identity.rs` verbatim (down
/// to the panic shape) rather than shared, because the two files' failure
/// messages point at different campaigns and different remedies, and a
/// shared helper would have to choose one.
fn assert_committed_bytes(relative: &str, actual: &str) {
    let path = repo_root().join(relative);
    let expected = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("committed artifact {relative} is readable: {e}"));
    if expected == actual {
        return;
    }
    let at = expected
        .char_indices()
        .zip(actual.char_indices())
        .find(|((_, a), (_, b))| a != b)
        .map(|((i, _), _)| i)
        .unwrap_or_else(|| expected.len().min(actual.len()));
    let window = |s: &str| {
        let lo = at.saturating_sub(80);
        let hi = (at + 80).min(s.len());
        s.get(lo..hi).unwrap_or(s).to_string()
    };
    panic!(
        "THE WASH MOVED A SHIPPED ARTIFACT: {relative}\n\
         windows/locale's wetness-grounding extraction (grounded_wetness_for,\
         blend_at_corners) must not change what describe_with_weights emits —\
         a red here is a leak in that extraction, not a rebaseline: find where\
         the extracted composition diverges from what it replaced.\n\
         first divergence at byte {at} (expected {} bytes, got {})\n\
         expected: …{}…\n  actual: …{}…",
        expected.len(),
        actual.len(),
        window(&expected),
        window(actual)
    );
}

/// The seed-42 flagship surrounds document is unmoved by The Wash's
/// extraction of `windows/locale`'s wetness-grounding block. This is the
/// same committed fixture `windows/scene::golden::
/// surrounds_v1_land_and_mark_bytes_are_pinned` pins, read a second time
/// from `cli/` — the crate every other workspace-wide invariant is already
/// asserted from (module doc above).
#[test]
fn seed_42_surrounds_flagship_is_unmoved_by_the_wash() {
    assert_committed_bytes(
        "windows/scene/tests/fixtures/surrounds-seed-42-flagship.json",
        &surrounds_seed_42_flagship_json(),
    );
}
