//! Exhaustive pin micro-enumeration (TOOL-exhaustive-enumeration).
//!
//! Where a pin space is finite and small, sampling it is the wrong
//! instrument — enumerate ALL of it at one seed and get total certainty
//! for a few dozen worlds instead of a confidence interval. This file
//! enumerates the full Cartesian product of every *discrete* scenario pin
//! that spans `build_world`'s two genesis domains (sky and terrain) at
//! seed 42:
//!
//! - rotation: `{Normal, Locked}` (2)
//! - neighbor: `{RedDwarf, SunLike, WhiteDwarf, OrangeGiant, RedGiant,
//!   BlueGiant}` (6)
//! - supercontinent: `{true, false}` (2)
//!
//! 2 x 6 x 2 = 24 combinations, every one of them generated-sky.
//!
//! **Continuous pins are excluded on purpose.** `plates` (legal range
//! 2..=64), `ocean_fraction` (legal range 0.05..=0.95), and moon counts
//! (graded `min`/`want` pairs up to 3) each carry a range or a combinatorial
//! width that is not "finite and tiny" in the sense this technique targets
//! (ideonomy note TOOL-exhaustive-enumeration) — enumerating them
//! exhaustively would mean enumerating a discretization choice as much as
//! the pin space itself, which is a sampling design question (see the
//! sibling idea TOOL-stratified-seeds), not a micro-enumeration one. The
//! three pins enumerated here are the ones whose entire legal domain is a
//! small, closed enum.
//!
//! For every combination this test asserts the build is either `Ok(world)`
//! or a loud, typed `Err(BuildError)` — never a panic — and that every
//! `Ok` combination is deterministic: building it twice from the same seed
//! and pins yields byte-identical serialized ledgers (`World::to_json`,
//! which routes through the quantized emit boundary).
//!
//! The full product's wall time has grown well past the task brief's ~15 s
//! commit-gate budget as the genesis pipeline deepened (the
//! fast-gate-tiers census, 2026-07-13, timed the sequential binary in the
//! minutes), so the test is `#[ignore]`d into the heavy tier: it runs in
//! `make gate-full`, not the default commit gate. The combos are
//! independent -- each build depends only on `(Seed(42), combo's pins)` --
//! so the sweep runs one scoped thread per combo (`std::thread::scope`)
//! rather than sequentially, cutting the heavy-tier wall time from ~505 s
//! to ~64 s (M1 Max, 10 logical CPUs, debug profile, when the product was
//! still 48 points) without touching the determinism guarantee (see
//! `full_pin_product_is_enumerated` below).
//! Depth-scoping the builds to `BuildDepth::Terrain` (MAP-25 Task 10, the
//! enumerated pins never reach past the terrain rung) cut it again, ~64 s
//! to ~16 s on the same machine.
//!
//! Re-measured 2026-09-04 (The Zenith) after the sky level came off the
//! product: **2.135 s** nextest wall for the 24 combos, against the ~16 s
//! above for 48. Reported, not asserted, and not a like-for-like machine
//! comparison with the ~505/~64/~16 s figures — read it as the current
//! cost, not as a speedup factor.
//!
//! Measured 2026-09-04 (The Zenith, seed 42): 24 built, 0 refused --
//! reported, not asserted; the split may legitimately move with physics
//! changes. Authoring measured 48 built, 0 refused on 2026-07-11, when the
//! sky choice was still a factor.
//!
//! **Every combination in this product can now refuse.** The old note here
//! said half of it (the constant-sky half, 24 combinations) was refusal-free
//! BY CONSTRUCTION -- rotation and neighbor pins were recorded on those
//! worlds but the generated-sky path that could act on them never ran, so a
//! future refusal could only arise on the generated-sky half. That
//! carve-out is gone with the tier: all 24 points build a generated sky, so
//! every one of them runs the path that can refuse. The refusal surface this
//! product searches is therefore the whole product, not half of it.

use hornvale_astronomy::{NeighborClass, RotationPin, SkyPins};
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, BuildError, SettlementPins, SkyChoice, WorldComponents, build_world_to,
};

/// Every discrete value of the three enumerated pins, in a stable order.
fn rotation_choices() -> [RotationPin; 2] {
    [RotationPin::Normal, RotationPin::Locked]
}

fn neighbor_choices() -> [NeighborClass; 6] {
    [
        NeighborClass::RedDwarf,
        NeighborClass::SunLike,
        NeighborClass::WhiteDwarf,
        NeighborClass::OrangeGiant,
        NeighborClass::RedGiant,
        NeighborClass::BlueGiant,
    ]
}

fn supercontinent_choices() -> [bool; 2] {
    [true, false]
}

/// One point in the enumerated product.
#[derive(Debug)]
struct Combo {
    rotation: RotationPin,
    neighbor: NeighborClass,
    supercontinent: bool,
}

/// The full 2 x 6 x 2 = 24-point Cartesian product, in a fixed,
/// deterministic order (no reliance on iteration order of anything but
/// these fixed arrays).
fn full_product() -> Vec<Combo> {
    let mut out = Vec::new();
    for rotation in &rotation_choices() {
        for neighbor in neighbor_choices() {
            for supercontinent in supercontinent_choices() {
                out.push(Combo {
                    rotation: rotation.clone(),
                    neighbor,
                    supercontinent,
                });
            }
        }
    }
    out
}

/// Build the world for one combo, at seed 42. Built to `BuildDepth::Terrain`
/// (MAP-25 Task 10): the enumerated pins only write astronomy+terrain facts,
/// and a depth-scoped build commits a byte-identical prefix of the full
/// build's ledger, so the determinism compare below keeps its exact scope
/// while skipping the ~75% of build cost that lives above the terrain rung.
fn build(combo: &Combo) -> Result<hornvale_kernel::World, BuildError> {
    let sky_pins = SkyPins {
        rotation: Some(combo.rotation.clone()),
        neighbor: Some(combo.neighbor),
        ..SkyPins::default()
    };
    let terrain_pins = TerrainPins {
        supercontinent: Some(combo.supercontinent),
        ..TerrainPins::default()
    };
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    build_world_to(
        Seed(42),
        &sky_pins,
        SkyChoice::Generated,
        &terrain_pins,
        &SettlementPins::default(),
        &wc,
        BuildDepth::Terrain,
    )
}

/// Exercise one combo: the build must be `Ok` or a loud `Err`, never a
/// panic, and every `Ok` must be deterministic (build twice, compare
/// serialized ledgers byte-for-byte). Returns whether the combo built.
fn check_combo(combo: &Combo) -> bool {
    let first = build(combo);
    match first {
        Ok(world_a) => {
            let world_b = build(combo).unwrap_or_else(|e| {
                panic!("second build of an Ok combo refused: {e} (combo: {combo:?})")
            });
            assert_eq!(
                world_a.to_json(),
                world_b.to_json(),
                "determinism violated: same seed and pins produced different serialized ledgers \
                 (combo: {combo:?})"
            );
            true
        }
        Err(_typed_err) => {
            // The refusal itself is the loud, typed signal (`BuildError`,
            // which wraps astronomy's and terrain's own `GenesisError`).
            // Nothing further to assert here beyond "it was Err, not a
            // panic" -- reaching this arm already proves that.
            false
        }
    }
}

/// The full 24-combo product, `#[ignore]`d into the heavy tier (fast-gate-tiers
/// spec): even parallelized its wall time exceeds the ~15 s commit-gate budget
/// as the genesis pipeline deepened, so it runs in `make gate-full`, not the
/// default commit gate. The combos are independent -- each `check_combo`
/// builds worlds purely from `(Seed(42), combo's pins)` and shares no mutable
/// state -- so the sweep runs one scoped thread per combo (`std::thread::scope`,
/// std only, modeled on `windows/lab/src/runner.rs`'s `run_pin_set`) instead of
/// a sequential loop, cutting the heavy-tier wall time from ~505 s to ~64 s (M1
/// Max, 10 logical CPUs, debug profile); terrain-depth builds (MAP-25 Task 10)
/// cut it again to ~16 s. It still asserts total certainty over
/// the product -- every combo builds or loudly refuses, and every `Ok` is
/// byte-deterministic (each `check_combo` builds its combo twice and compares
/// serialized ledgers on its own thread) -- just off the per-commit path.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn full_pin_product_is_enumerated() {
    let combos = full_product();

    // One scoped thread per combo. `check_combo` contains the test's real
    // failure signals (the determinism `assert_eq!` and the second-build
    // `panic!`), so a panicking worker's payload is re-raised on the main
    // thread with `resume_unwind` (not `.unwrap()`/`.expect()`) to preserve
    // the original panic message instead of masking it behind a generic
    // "thread panicked" one.
    let results: Vec<bool> = std::thread::scope(|scope| {
        let handles: Vec<_> = combos
            .iter()
            .map(|combo| scope.spawn(move || check_combo(combo)))
            .collect();
        handles
            .into_iter()
            .map(|handle| match handle.join() {
                Ok(built) => built,
                Err(payload) => std::panic::resume_unwind(payload),
            })
            .collect()
    });

    let mut built = 0usize;
    let mut refused = 0usize;
    for was_built in results {
        if was_built {
            built += 1;
        } else {
            refused += 1;
        }
    }

    assert_eq!(
        built + refused,
        24,
        "every one of the 24 combos must be accounted for"
    );
    // Measured (not preregistered), 2026-09-04: all 24 combos in the
    // enumerated product build successfully; none refuse. Sky's rotation
    // and neighbor pins and terrain's supercontinent pin are each legal
    // across their whole enumerated domain at seed 42, so this product's
    // discrete corner of pin space has no refusal surface to find --
    // that emptiness is itself the measured result, not an assumption.
    // The built/refused split is reported here, not asserted: forcing an
    // exact count would fail this test for a reason unrelated to what it
    // guards if a future legitimate physics change made some combo
    // correctly refuse with a typed error instead of building.
    eprintln!("full_pin_product_is_enumerated: {built} built, {refused} refused (of 24)");
}
