//! The Forebay, Task 1, Step 6 (the no-reply branch): does `RoomMeshMemo`
//! reuse pay off under a locality-shaped access pattern?
//!
//! INFORMATIVE, NEVER A GATE — same standing as `query_scaling.rs` beside
//! this file. It exists because the-leat profiled `NearestVertexIndex::scan_at`
//! at 13.4% of the `agent_scaling` bench, 93% of it arriving via
//! `Facet::corner_weights`, but `RoomMeshMemo` already caches
//! `corner_weights` and the bench already hoists one memo across all 20
//! ticks. So that 13.4% may be pure MISS cost that no cache lifecycle can
//! reduce, and nothing in the tree measured the memo's hit rate before this
//! campaign added the counters this probe reads
//! (`RoomMeshMemo::corner_weights_hits/misses`, `neighbors_hits/misses`).
//!
//! ## Why this is a weaker instrument than a real bench, stated plainly
//!
//! **The coordination attempt and why this exists at all.** The natural
//! measurement site is `windows/vessel/examples/agent_scaling.rs`'s own
//! report, but `windows/vessel/` carries a live hold-off
//! (`campaign/the-hand`, restructuring `liveness.rs`/`session.rs`). A board
//! `ask` (`thread=forebay-memo-hitrate-readout`) went unanswered through the
//! plan's Task 1 Step 6 branch table, so this is the plan's own designated
//! fallback: a synthetic kernel-side walk instead of the real bench.
//!
//! **The distribution here is synthetic, not observed.** It is a bounded
//! random walk radiating from and returning to a small number of fixed
//! "settlement" anchors — chosen because a real creature's movement is
//! locally correlated (it revisits its neighborhood far more than a uniform
//! scan over all `Facet`s would), which is the one property this probe
//! needs to even ask the right question. But the walk's branching
//! probabilities, expedition length, and settlement count are all invented
//! by this file, not measured from `windows/vessel`'s actual creature
//! movement or population density. **Do not read the number below as "the
//! real workload's hit rate is X."** Read it as "a plausibly locality-shaped
//! synthetic workload has hit rate X," which is evidence about the SHAPE of
//! the answer (is a memo's reuse structurally capable of being high under
//! revisit-heavy movement?) and not its magnitude under real gameplay
//! densities, creature counts, or `agent_scaling.rs`'s specific NPC/tick
//! mix.
//!
//! Run: `cargo run --release -p hornvale-kernel --example
//! room_mesh_memo_hitrate` ALWAYS `--release`: a debug build measures the
//! optimizer, not the memo.
//!
//! ## Measured
//!
//! Date: 2026-08-23. Box: `ambrose` (`hostname -s`). Profile: `--release`.
//! Verbatim output:
//!
//! ```text
//! settlements=5 creatures_per_settlement=20 expeditions=40 steps_per_expedition=10
//! globe_level=5 room_level=10
//!
//! corner_weights: hits=42335 misses=1665 total=44000 hit_rate=0.9622
//! neighbors:      hits=42335 misses=1665 total=44000 hit_rate=0.9622
//! ```
//!
//! Reproducible: two consecutive runs on the same box produced byte-identical
//! counts (no wall-clock, no `rand` crate — everything here is the
//! multiplicative-hash stream above, seeded from fixed constants).
//!
//! **Which headline this selects.** Per the spec's §4 table: a 96.2% hit
//! rate is the "hit rate is already high" branch — the residual is
//! structurally miss-bound, and no cache lifecycle (eviction, budget,
//! hysteresis) can shrink a miss that has never happened before. If the real
//! `agent_scaling` bench's `scan_at` 13.4% behaves anything like this
//! synthetic walk, most of it is likely genuine new-pair cost, and the
//! campaign's honest framing is "generality for the workload that has not
//! arrived yet" (§4's second row), not "this migration retires 13.4% of the
//! bench" (§4's first row) — consistent with the campaign's own falsifier
//! §10, which names exactly this outcome as the one that would fire.
//! **This is read off a synthetic distribution, not the bench** (see above):
//! it says the store's reuse *can* be high under locality, not that
//! `agent_scaling.rs` measures 96.2% today.

use hornvale_kernel::{Facet, Geosphere, NearestVertexIndex, RoomMeshMemo};

/// Deterministic pseudo-random stream (decision 0004: no `rand` crate in
/// this workspace, and none is added here). The same multiplicative-hash
/// shape the-forebay's own `Derived` property tests use
/// (`kernel/tests/suite/derived.rs`), reused here for a walk instead of a
/// key sequence.
fn next(state: &mut u64) -> u64 {
    *state = state.wrapping_mul(2_654_435_761).wrapping_add(1);
    *state
}

/// `len` deterministic child digits (0..4), consuming `rng`.
fn random_path(rng: &mut u64, len: usize) -> Vec<u8> {
    (0..len).map(|_| (next(rng) % 4) as u8).collect()
}

/// The globe level backing `Geosphere`/`NearestVertexIndex` — kept small so
/// building the index is cheap; unrelated to how deep a room address goes.
const GLOBE_LEVEL: u32 = 5;
/// The refinement depth every room address in this probe lives at. Below
/// the globe level, so `corner_weights` is always `Some` here — this probe
/// is about hit rate on a populated grid, not the above-the-grid `None`
/// case (`a_cached_none_counts_as_a_hit_not_a_miss` already pins that
/// separately).
const ROOM_LEVEL: u32 = 10;
/// How many digits of `ROOM_LEVEL` name the settlement anchor (shared by
/// every creature at that settlement) versus the creature's own offset from
/// it.
const SETTLEMENT_DEPTH: usize = 4;
/// How many settlement clusters to scatter across the base cube.
///
/// Five, against the cube's six faces, so [`settlement_anchor`]'s `% 6` still
/// yields a distinct face per settlement.
const SETTLEMENTS: usize = 5;
/// Creatures per settlement — each gets its own home address near the
/// settlement anchor and its own walk.
const CREATURES_PER_SETTLEMENT: usize = 20;
/// "Expeditions" per creature: a creature returns to its home address at
/// the start of each one, so home is revisited `EXPEDITIONS` times by
/// construction — the return-to-home-base pattern real creature movement
/// has and a uniform random scan does not.
const EXPEDITIONS: usize = 40;
/// Steps of the bounded random walk away from home per expedition.
const STEPS_PER_EXPEDITION: usize = 10;

/// The settlement anchor address for settlement `s`: a fixed face and a
/// deterministic `SETTLEMENT_DEPTH`-digit path, distinct per settlement.
///
/// **`% 6`, not `% 20`, since fix round 1.** The modulus was the
/// ICOSAHEDRON's face count, and decision 0506 made the base mesh a
/// cube-sphere with six faces — so `s = 1` minted face 7 and
/// `cube::face_unit`'s unconditional `assert!` killed the whole run
/// (`rc=101`, five of the six settlements out of range). Nothing gates
/// `examples/`: `cargo clippy --all-targets` COMPILES them and never RUNS
/// them, so all three gates were green with this instrument permanently
/// dead. `% 6` over `SETTLEMENTS = 5` still gives a distinct face per
/// settlement (0, 1, 2, 3, 4), which is the property the name claims.
fn settlement_anchor(s: usize) -> Facet {
    let face = ((s * 7) % 6) as u8;
    let mut seed = 0xA5A5_5A5A_0000_0001u64 ^ (s as u64);
    Facet {
        face,
        path: random_path(&mut seed, SETTLEMENT_DEPTH),
    }
}

/// One creature's home: the settlement anchor extended to `ROOM_LEVEL` by a
/// deterministic per-creature offset, so creatures at the same settlement
/// have distinct but nearby homes.
fn home_for(anchor: &Facet, creature_id: u64) -> Facet {
    let mut seed = 0x9E37_79B9_7F4A_7C15u64 ^ creature_id;
    let extra = random_path(&mut seed, ROOM_LEVEL as usize - SETTLEMENT_DEPTH);
    let mut path = anchor.path.clone();
    path.extend(extra);
    Facet {
        face: anchor.face,
        path,
    }
}

fn main() {
    let geo = Geosphere::new(GLOBE_LEVEL);
    let index = NearestVertexIndex::new(&geo);
    let mut memo = RoomMeshMemo::new();

    println!(
        "settlements={SETTLEMENTS} creatures_per_settlement={CREATURES_PER_SETTLEMENT} \
         expeditions={EXPEDITIONS} steps_per_expedition={STEPS_PER_EXPEDITION}"
    );
    println!("globe_level={GLOBE_LEVEL} room_level={ROOM_LEVEL}");
    println!();

    let mut creature_id = 0u64;
    for s in 0..SETTLEMENTS {
        let anchor = settlement_anchor(s);
        for _ in 0..CREATURES_PER_SETTLEMENT {
            let home = home_for(&anchor, creature_id);
            // The walk's own RNG state persists ACROSS expeditions (only the
            // position resets to `home`), so each expedition wanders a
            // different, but locally overlapping, path from the same base —
            // the source of genuine (not merely home-address) revisits.
            let mut rng = 0xD1B5_4A32_D192_ED03u64 ^ creature_id;
            for _ in 0..EXPEDITIONS {
                let mut cur = home.clone();
                let _ = cur.corner_weights_memo(&geo, &index, &mut memo);
                let mut neighbors = cur.neighbors_memo(&mut memo);
                for _ in 0..STEPS_PER_EXPEDITION {
                    let choice = (next(&mut rng) % 3) as usize;
                    cur = neighbors[choice].clone();
                    let _ = cur.corner_weights_memo(&geo, &index, &mut memo);
                    neighbors = cur.neighbors_memo(&mut memo);
                }
            }
            creature_id += 1;
        }
    }

    let cw_hits = memo.corner_weights_hits();
    let cw_misses = memo.corner_weights_misses();
    let cw_total = cw_hits + cw_misses;
    let n_hits = memo.neighbors_hits();
    let n_misses = memo.neighbors_misses();
    let n_total = n_hits + n_misses;

    println!();
    println!(
        "corner_weights: hits={cw_hits} misses={cw_misses} total={cw_total} hit_rate={:.4}",
        cw_hits as f64 / cw_total as f64
    );
    println!(
        "neighbors:      hits={n_hits} misses={n_misses} total={n_total} hit_rate={:.4}",
        n_hits as f64 / n_total as f64
    );
}
