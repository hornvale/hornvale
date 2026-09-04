//! The world cache and its three-layer validity protocol (The Overture, Task
//! 8): write a built world to disk, and decide whether a previously-written
//! one may be reused instead of paying for genesis again.
//!
//! # The three layers, cheapest refusal first (spec §6)
//!
//! 1. **Seed and pins.** Free: a plain string compare against a small sidecar
//!    file, no world load at all. Mismatch → regenerate.
//! 2. **`derived_under` diffed against the current stream-label roster**
//!    (`hornvale::streams::what_moved`, already written and tested — this
//!    module does not reimplement it). Needs the world loaded, but no
//!    genesis. Any moved label → regenerate.
//! 3. **The astronomy prefix tripwire.** Regenerate to
//!    [`BuildDepth::Astronomy`] (0.4 ms) and compare its facts, in order,
//!    against the cached world's own leading facts. Catches an undeclared
//!    change no label bump records; does NOT prove validity beyond the rung
//!    it samples (a settlements-only formula change would not show — spec
//!    §6 says so and this module does not oversell it).
//!
//! Any layer refusing returns `None` and costs nothing more than that
//! layer's own check — layer 1 alone answers most restarts of the same seed
//! under the same pins.
//!
//! # Why `Cache::write`/`Cache::load_if_valid` take `pins` explicitly,
//! against the plan brief's sketch
//!
//! The brief's Step 1 sketch calls `Cache::write(&tmp(),
//! &world_with(default_pins()))` — two arguments, no pins — and later calls
//! `c.load_if_valid(&tmp(), seed(42), &other_pins())` as a METHOD on the
//! value `write` returned. Both shapes are unworkable once checked against
//! the code, for two separate reasons, and this module corrects both
//! (the same practice this campaign's other tasks have followed when a
//! brief's sketch didn't survive contact with the source — e.g. Task 2's
//! corrected `World` line numbers, Task 7's corrected `can_speak` rung):
//!
//! - **Pins are not recoverable from a `World`.** They are not committed as
//!   facts (`windows/worldgen` never writes a "pin echo" fact — verified by
//!   grep before writing this module), and [`hornvale_kernel::World::derived_under`]
//!   is a DIFFERENT thing entirely: stream-LABEL metadata (layer 2's own
//!   input), keyed by label, not by pin. So the seed+pins a cached world was
//!   built under can only be recorded by whoever built it, at write time —
//!   which means `write` must take them as an argument, not derive them from
//!   the `World` it is handed.
//! - **A fresh process has no live `Cache` value to call a method on.** The
//!   whole point of this cache is to survive a process restart; on restart,
//!   `main.rs` has only a directory (from [`crate::state_dir`]), never a
//!   `Cache` some earlier process's `write` returned. So `load_if_valid` is
//!   an associated function taking `dir` explicitly, not a method on a
//!   receiver nothing can supply after a restart.
//!
//! [`GenesisPins`] is the bundle that replaces the brief's bare
//! `default_pins()`/`other_pins()` helpers with a named, `PartialEq` type —
//! every field of every pin struct genesis takes already derives `PartialEq`
//! (`SkyPins`, `TerrainPins`, `SettlementPins`, `SkyChoice`), so bundling adds
//! no new comparison machinery.
//!
//! # Why the tripwire compares `Fact`s directly, not JSON strings
//!
//! `windows/worldgen/tests/suite/depth.rs`'s own prefix test serializes each
//! fact with `serde_json` before comparing, with a comment explaining why:
//! "the serialization boundary that quantizes floats, not just `PartialEq`".
//! That was true before decision 0041 unified transcendentals; it is no
//! longer why quantization happens to matter here at all, because
//! quantization happens at `Ledger::commit`, not only at serialization
//! (`kernel/src/ledger.rs`: `fact.object = Value::Number(quantize::quantize(n))`
//! runs before the fact is pushed) — every `Fact` already sitting in a
//! `Ledger` is already in its canonical, platform-stable form. `Fact` derives
//! `PartialEq`, so a plain field comparison here already reflects everything
//! the JSON round trip would. Not reaching for `serde_json` also keeps it out
//! of this crate's real dependency list — Task 3's timings module deliberately
//! kept `clients/game/bin` free of it outside `[dev-dependencies]`, and this
//! module preserves that.

use hornvale::streams;
use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{BuildDepth, SettlementPins, WorldComponents, build_world_to};
use std::path::{Path, PathBuf};

/// The subdirectory this cache's two files live under, inside
/// [`crate::state_dir`]'s directory — grouped apart from
/// `overture-timings.tsv`'s flat sibling file because the world file and its
/// sidecar are meaningless read apart from each other.
pub const CACHE_DIR_NAME: &str = "overture-cache";

/// The cache's world file, inside whatever directory the caller names.
pub const WORLD_FILE: &str = "overture-cache-world.json";
/// The cache's layer-1 sidecar: the seed+pins fingerprint the world was
/// written under.
pub const REQUEST_FILE: &str = "overture-cache-request.txt";

/// Every parameter genesis takes besides the seed, bundled — see the module
/// doc's "Why `Cache::write`/`Cache::load_if_valid` take `pins` explicitly"
/// section for why this exists at all rather than the brief's bare
/// `default_pins()`/`other_pins()` helpers.
#[derive(Debug, Clone, PartialEq)]
pub struct GenesisPins {
    /// The sky provider's own scenario pins.
    pub sky: SkyPins,
    /// The tectonic scenario pins.
    pub terrain: TerrainPins,
    /// The settlement placement pins.
    pub settlement: SettlementPins,
}

impl GenesisPins {
    /// The default pins `clients/game/bin`'s only genesis entry point
    /// actually uses today (`overture::genesis::spawn`) — there is no CLI
    /// flag yet for a custom pin. Named so main.rs and the cache's own tests
    /// share one definition of "the request this client makes" rather than
    /// two copies drifting apart.
    pub fn default_request() -> GenesisPins {
        GenesisPins {
            sky: SkyPins::default(),
            terrain: TerrainPins::default(),
            settlement: SettlementPins::default(),
        }
    }

    /// A single-line, round-trip-free fingerprint of `seed` plus every pin
    /// field, canonical because `Debug`'s derived output is deterministic for
    /// equal values — no parser is needed on the other end, only a string
    /// compare, which is what makes layer 1 free.
    fn fingerprint(&self, seed: Seed) -> String {
        format!(
            "{seed:?}\tsky={:?}\tterrain={:?}\tsettlement={:?}\n",
            self.sky, self.terrain, self.settlement
        )
    }
}

/// A receipt that [`Cache::write`] succeeded, carrying the directory it wrote
/// to. Not required to read the cache back — see [`Cache::load_if_valid`],
/// an associated function a fresh process calls directly.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cache {
    /// The directory this cache's files live in.
    pub dir: PathBuf,
}

impl Cache {
    /// Stamp `world` with the current stream-label roster
    /// (`hornvale::streams::versioned_labels`, the composition root's own
    /// convention — mirrors `cli::main::cmd_new`'s `streams::stamp` call
    /// immediately before `World::save`) and write it, plus the seed+pins
    /// fingerprint layer 1 reads, into `dir`.
    ///
    /// Creates `dir` if it does not exist. Best-effort by design (matching
    /// [`crate::state_dir`]'s own philosophy): any I/O failure here must cost
    /// the NEXT run its cache and nothing else, so a caller is expected to
    /// `.ok()` this rather than treat it as fatal to a genesis that already
    /// succeeded.
    pub fn write(dir: &Path, pins: &GenesisPins, world: &World) -> std::io::Result<Cache> {
        std::fs::create_dir_all(dir)?;
        let stamped = streams::stamp(world.clone(), &streams::versioned_labels());
        std::fs::write(dir.join(WORLD_FILE), stamped.to_json())?;
        std::fs::write(dir.join(REQUEST_FILE), pins.fingerprint(stamped.seed))?;
        Ok(Cache {
            dir: dir.to_path_buf(),
        })
    }

    /// The three-layer validity protocol, in refusal order. `None` at any
    /// layer means: regenerate — the caller has paid for nothing beyond that
    /// layer's own check.
    ///
    /// An associated function, not a method — see the module doc's "a fresh
    /// process has no live `Cache` value" note.
    pub fn load_if_valid(dir: &Path, seed: Seed, pins: &GenesisPins) -> Option<World> {
        // Layer 1: seed and pins, free — a string compare against a sidecar,
        // no world file touched at all on a mismatch.
        let want = pins.fingerprint(seed);
        let have = std::fs::read_to_string(dir.join(REQUEST_FILE)).ok()?;
        if have != want {
            return None;
        }

        // Layer 2: `derived_under` diffed against the current roster. Needs
        // the world loaded (a bad/missing/corrupt file also reduces to
        // `None` here, same as `state_dir`'s own total-read philosophy).
        let world = World::load(&dir.join(WORLD_FILE)).ok()?;
        let now = streams::versioned_labels();
        if !streams::what_moved(&world.derived_under, &now).is_empty() {
            return None;
        }

        // Layer 3: the astronomy prefix tripwire.
        let wc = WorldComponents::assemble().ok()?;
        let fresh = build_world_to(
            seed,
            &pins.sky,
            &pins.terrain,
            &pins.settlement,
            &wc,
            BuildDepth::Astronomy,
        )
        .ok()?;
        if !prefix_agrees(&world, &fresh) {
            return None;
        }

        Some(world)
    }
}

/// True if `cached`'s own leading facts are exactly `fresh`'s facts, in
/// order — the byte-identical-prefix property [`BuildDepth`]'s own doc
/// states, checked directly on already-quantized [`hornvale_kernel::Fact`]
/// values (see the module doc's "Why the tripwire compares `Fact`s
/// directly" section for why no serialization round trip is needed here).
///
/// `cached.ledger.len() < fresh.ledger.len()` is treated as a mismatch
/// outright rather than comparing whatever shorter prefix `zip` would allow
/// — a cached FULL-depth world that somehow committed FEWER facts than a
/// fresh ASTRONOMY-only build is already invalid on its face, and a `zip`
/// that silently truncated to the shorter side would report agreement on a
/// prefix that isn't actually the whole astronomy rung.
fn prefix_agrees(cached: &World, fresh: &World) -> bool {
    cached.ledger.len() >= fresh.ledger.len()
        && cached
            .ledger
            .iter()
            .zip(fresh.ledger.iter())
            .all(|(a, b)| a == b)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    /// A fresh scratch directory per test, so tests never share on-disk
    /// state (the same discipline `overture::timings`'s own tests use).
    fn tmp() -> PathBuf {
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        let n = COUNTER.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!(
            "hornvale-game-cache-test-{}-{n}",
            std::process::id()
        ));
        std::fs::create_dir_all(&dir).expect("create scratch dir");
        dir
    }

    fn seed(n: u64) -> Seed {
        Seed(n)
    }

    fn default_pins() -> GenesisPins {
        GenesisPins::default_request()
    }

    fn other_pins() -> GenesisPins {
        GenesisPins {
            settlement: SettlementPins {
                species: Some("elf".to_string()),
            },
            ..GenesisPins::default_request()
        }
    }

    /// A real (cheap) seed-42 world at [`BuildDepth::Astronomy`] — enough
    /// ledger content to make the seed/pins/label checks meaningful, and
    /// built once per test process.
    fn world_with(pins: &GenesisPins) -> World {
        let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
        let world = build_world_to(
            seed(42),
            &pins.sky,
            &pins.terrain,
            &pins.settlement,
            &wc,
            BuildDepth::Astronomy,
        )
        .expect("seed 42 builds");
        streams::stamp(world, &streams::versioned_labels())
    }

    /// Seed 42 built at [`BuildDepth::Full`] and cached once — the world the
    /// tripwire tests validate a fresh Astronomy rebuild against. Built once
    /// per process: seed 42 at `Full` is the campaign's own standard fixture
    /// cost (~1.7-3 s in `--release`, per Task 7's own measurement), and every
    /// test here reads it rather than rebuilding it.
    fn cached_world() -> &'static World {
        static WORLD: std::sync::OnceLock<World> = std::sync::OnceLock::new();
        WORLD.get_or_init(|| {
            let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
            let pins = default_pins();
            let world = hornvale_worldgen::build_world_from_components(
                seed(42),
                &pins.sky,
                &pins.terrain,
                &pins.settlement,
                &wc,
            )
            .expect("seed 42 builds");
            streams::stamp(world, &streams::versioned_labels())
        })
    }

    // --- Layer 1: seed and pins -------------------------------------------

    #[test]
    fn a_cache_with_different_pins_is_refused() {
        let dir = tmp();
        let world = world_with(&default_pins());
        Cache::write(&dir, &default_pins(), &world).unwrap();
        assert!(
            Cache::load_if_valid(&dir, seed(42), &other_pins()).is_none(),
            "a cache built under different pins was accepted"
        );
    }

    #[test]
    fn a_cache_with_a_different_seed_is_refused() {
        let dir = tmp();
        let world = world_with(&default_pins());
        Cache::write(&dir, &default_pins(), &world).unwrap();
        assert!(
            Cache::load_if_valid(&dir, seed(43), &default_pins()).is_none(),
            "a cache built for a different seed was accepted"
        );
    }

    #[test]
    fn a_cache_with_the_same_seed_and_pins_survives_layer_one() {
        // Non-vacuity twin: layer 1 alone must not be so strict that nothing
        // ever passes it (the failure `a_cache_with_different_pins_is_refused`
        // alone could not rule out — a `load_if_valid` that always returns
        // `None` would pass it too).
        let dir = tmp();
        let world = world_with(&default_pins());
        Cache::write(&dir, &default_pins(), &world).unwrap();
        assert!(Cache::load_if_valid(&dir, seed(42), &default_pins()).is_some());
    }

    #[test]
    fn a_missing_cache_directory_is_refused_not_a_panic() {
        let dir = tmp().join("never-written");
        assert!(Cache::load_if_valid(&dir, seed(42), &default_pins()).is_none());
    }

    // --- Layer 2: the label diff (delegates to `hornvale::streams`, already
    // tested there — these tests exercise THIS module's use of it) ---------

    #[test]
    fn a_moved_label_is_refused() {
        // `Cache::write` always RESTAMPS to the current roster (matching
        // `cli::main::cmd_new`'s own convention — see the module doc), which
        // is correct in production (a cache is written moments after a real
        // genesis run, so "just built under" and "current" cannot differ
        // within one process) but means going through `write` here could
        // never construct a STALE stamp to test against. So this test writes
        // the cache files directly, the same bypass
        // `an_empty_stamp_claims_nothing_moved_and_is_accepted` below uses,
        // to simulate what a PAST run (under older code) would have left
        // on disk for THIS run (current code) to find.
        let dir = tmp();
        let mut world = world_with(&default_pins());
        let now = streams::versioned_labels();
        let (label, current) = now
            .iter()
            .next()
            .expect("the current roster has at least one versioned label");
        // A stamp that claims an OLDER version of a label the current roster
        // has since moved on from.
        let mut stale = now.clone();
        stale.insert(label.clone(), format!("{current}-stale"));
        world.derived_under = stale;
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join(WORLD_FILE), world.to_json()).unwrap();
        std::fs::write(dir.join(REQUEST_FILE), default_pins().fingerprint(seed(42))).unwrap();
        assert!(
            Cache::load_if_valid(&dir, seed(42), &default_pins()).is_none(),
            "a stamp disagreeing with the current roster was accepted"
        );
    }

    #[test]
    fn an_empty_stamp_claims_nothing_moved_and_is_accepted() {
        // The non-vacuity direction `windows/CLAUDE.md`'s own warning names:
        // an empty `derived_under` (a world saved before stamping existed)
        // must NOT read as "everything moved". `Cache::write` always stamps,
        // so this constructs the pre-stamping shape by hand.
        let dir = tmp();
        let mut world = world_with(&default_pins());
        world.derived_under = std::collections::BTreeMap::new();
        // Bypass `Cache::write`'s own stamping so the empty map survives to
        // disk exactly as constructed.
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join(WORLD_FILE), world.to_json()).unwrap();
        std::fs::write(dir.join(REQUEST_FILE), default_pins().fingerprint(seed(42))).unwrap();
        assert!(
            Cache::load_if_valid(&dir, seed(42), &default_pins()).is_some(),
            "an empty stamp must claim nothing moved, not everything"
        );
    }

    // --- Layer 3: the astronomy prefix tripwire ----------------------------

    #[test]
    fn the_prefix_tripwire_is_quiet_on_an_unchanged_tree() {
        // Layer 3, and spec §10 says this one MAY FAIL for an ARBITRARY
        // genesis change (see cache.rs's module doc and the task report for
        // the disposed H3 measurement). This test is the baseline sanity
        // check the brief's own sketch asked for: an unmutated tree's fresh
        // astronomy rebuild must agree with the cached world's own leading
        // facts, byte for byte.
        let fresh = astronomy_prefix_regenerated(seed(42), &default_pins());
        let cached = astronomy_prefix_of(cached_world(), fresh.len());
        assert_eq!(
            cached, fresh,
            "the tripwire must be quiet on an unchanged tree"
        );
    }

    #[test]
    fn the_full_cache_hit_path_accepts_the_unmutated_seed_42_world() {
        // End to end: write the real `Full`-depth cached_world(), then load
        // it back through every layer at once.
        let dir = tmp();
        Cache::write(&dir, &default_pins(), cached_world()).unwrap();
        let loaded = Cache::load_if_valid(&dir, seed(42), &default_pins());
        assert!(loaded.is_some(), "an untouched, matching cache was refused");
        assert_eq!(
            loaded.unwrap().ledger.len(),
            cached_world().ledger.len(),
            "the loaded world must be the SAME world, not a re-derived one"
        );
    }

    #[test]
    fn a_cache_whose_world_disagrees_with_its_own_sidecar_is_refused() {
        // Layers 1 and 2 are both satisfied by construction (a real,
        // correctly-stamped, seed-42-shaped fingerprint), so ONLY layer 3 can
        // catch this: the world FILE on disk is actually a DIFFERENT seed's —
        // a corrupted or torn write, or two callers racing on the same
        // directory — while its sidecar still claims seed 42. Nothing about
        // the seed/pins string or the stamp map is wrong, so this is the one
        // scenario that isolates layer 3 rather than re-testing layer 1
        // under a different name.
        let dir = tmp();
        let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
        let forged_content = build_world_to(
            seed(43),
            &default_pins().sky,
            &default_pins().terrain,
            &default_pins().settlement,
            &wc,
            BuildDepth::Astronomy,
        )
        .expect("seed 43 builds");
        let forged_content = streams::stamp(forged_content, &streams::versioned_labels());
        std::fs::create_dir_all(&dir).unwrap();
        // Seed 43's real, correctly-stamped content …
        std::fs::write(dir.join(WORLD_FILE), forged_content.to_json()).unwrap();
        // … under seed 42's own request fingerprint.
        std::fs::write(dir.join(REQUEST_FILE), default_pins().fingerprint(seed(42))).unwrap();
        assert!(
            Cache::load_if_valid(&dir, seed(42), &default_pins()).is_none(),
            "a world file that disagrees with its own sidecar's claimed seed was accepted"
        );
    }

    /// The cached (Full-depth) world's own astronomy rung, recovered by
    /// taking its ledger's first `astro_len` facts — the prefix property
    /// guarantees this equals what a genuine Astronomy-depth build of the
    /// same seed and pins commits, if nothing has silently changed since.
    fn astronomy_prefix_of(cached: &World, astro_len: usize) -> Vec<hornvale_kernel::Fact> {
        cached.ledger.iter().take(astro_len).cloned().collect()
    }

    /// Regenerate an Astronomy-depth world for `seed`/`pins`, freshly — the
    /// tripwire's own reference, costing `windows/worldgen`'s own measured
    /// ~0.4 ms.
    fn astronomy_prefix_regenerated(seed: Seed, pins: &GenesisPins) -> Vec<hornvale_kernel::Fact> {
        let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
        let world = build_world_to(
            seed,
            &pins.sky,
            &pins.terrain,
            &pins.settlement,
            &wc,
            BuildDepth::Astronomy,
        )
        .expect("seed 42 builds");
        world.ledger.iter().cloned().collect()
    }

    // --- Non-vacuity on the bundled fingerprint itself ---------------------

    #[test]
    fn distinct_pins_produce_distinct_fingerprints() {
        assert_ne!(
            default_pins().fingerprint(seed(42)),
            other_pins().fingerprint(seed(42)),
            "different settlement pins must not fingerprint identically"
        );
    }

    #[test]
    fn distinct_seeds_produce_distinct_fingerprints() {
        assert_ne!(
            default_pins().fingerprint(seed(42)),
            default_pins().fingerprint(seed(43)),
        );
    }
}
