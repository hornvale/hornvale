//! FOLD-equals-SCAN for the fear path's verdict index (The Detent, spec §5).
//!
//! The oracles below are VERBATIM COPIES of `build_emitter_scan`'s pass 1+2
//! and of `hazard_memory_memo`'s emitter-free loop as they stood immediately
//! before The Detent's Task 6 rewrote both to read
//! [`crate::resident::FrighteningGround`]. Copied rather than called, for the
//! reason every other oracle in this campaign is copied (spec §5, and The
//! Pawl's `emitter_scan_oracle` beside them): once the production body is
//! rewritten this is the only statement of the old one left, and an oracle
//! sharing code with the thing under test cannot falsify it.
//!
//! **Why the file is under `src/` and not `tests/suite/`.** The registry row
//! `TOOL-emitter-scan-tests-out-of-liveness` asks for these to live beside
//! their siblings in `windows/vessel/tests/suite/resident_folds.rs`. They
//! cannot: `EmitterScan`, `build_emitter_scan`, `threat_field`,
//! `mettle_factor`, `feels_frightening` and `DANGER_ACT` are private to
//! `crate::liveness`, and an integration-test crate cannot see even
//! `pub(crate)`. Splitting the file out of `liveness.rs` — which is what the
//! row is really about — is done by giving the module its own FILE and
//! wiring it with `#[path]`; the visibility half of the row is not
//! achievable as written and is ledgered as such.

use super::tests::{
    PlantedTerrain, agent_at_reg, commit_agent_at, haunt_npc, phantom_triple, raddr, test_folds,
};
use super::*;
use crate::ground::{GroundHazards, OwnedGround};
use crate::resident::{FrighteningGround, OwnedFolds, ResidentFolds};
use hornvale_kernel::test_lineage;

// ---------------------------------------------------------------------------
// The oracles (pre-Task-6 production bodies, verbatim)
// ---------------------------------------------------------------------------

/// A VERBATIM COPY of the pre-Task-6 `build_emitter_scan` pass 1 + pass 2 —
/// the per-member `is_emitter` verdict and the union of alarm-source rooms,
/// computed by judging `LatestVisit::rooms_at(m, t)` and `m.home` directly.
///
/// The witness call (`note_emitter_scan`) and pass 3 (the emitter timeline
/// copy) are omitted: pass 3 is untouched by this task, and the witness is
/// not part of the value under comparison.
fn scan_oracle(
    roster: &[Body],
    ledger: &Ledger,
    folds: &OwnedFolds,
    terrain: &dyn Terrain,
    t: WorldTime,
) -> (Vec<bool>, std::collections::BTreeSet<Facet>) {
    let visited: Vec<Vec<Facet>> = {
        let mut store = folds.borrow_mut();
        let (visits, _) = store.latest_visit_and_trail(ledger);
        roster
            .iter()
            .map(|m| visits.rooms_at(m.entity, t))
            .collect()
    };

    let mut alarm_source_rooms: std::collections::BTreeSet<Facet> =
        std::collections::BTreeSet::new();
    let mut is_emitter: Vec<bool> = Vec::with_capacity(roster.len());
    for (m, rooms) in roster.iter().zip(&visited) {
        let mettle = mettle_factor(m.boldness);
        let frightening =
            |room: &Facet| threat_field(room, &m.threat_niche, terrain) * mettle >= DANGER_ACT;
        let mut ever = false;
        let mut note_halo = |p: &Facet| {
            alarm_source_rooms.insert(p.clone());
            for n in p.neighbors() {
                alarm_source_rooms.insert(n);
            }
        };
        if frightening(&m.home) {
            ever = true;
            note_halo(&m.home);
        }
        for p in rooms {
            if frightening(p) {
                ever = true;
                note_halo(p);
            }
        }
        is_emitter.push(ever);
    }
    (is_emitter, alarm_source_rooms)
}

/// A VERBATIM COPY of the pre-Task-6 emitter-free loop of
/// `hazard_memory_memo`: every room of the most-recent-visit map judged by
/// `frightened_at` over an EMPTY roster (which short-circuits `alarm_at` to
/// `0.0`, so the verdict is terrain-only).
///
/// `ledger`/`folds` ride along because `frightened_at` takes them; with an
/// empty roster neither is read.
fn emitter_free_oracle(
    latest: &std::collections::BTreeMap<Facet, WorldTime>,
    npc: &Body,
    terrain: &dyn Terrain,
    ledger: &Ledger,
    folds: &OwnedFolds,
) -> std::collections::BTreeSet<Facet> {
    let mut shunned: std::collections::BTreeSet<Facet> = std::collections::BTreeSet::new();
    for (room, day) in latest {
        if frightened_at(room, npc, terrain, *day, &[], ledger, folds) {
            shunned.insert(room.clone());
        }
    }
    shunned
}

/// How many `(member, room)` pairs the oracle's predicate judges FRIGHTENING
/// at `t` — the anti-vacuity floor for the comparisons below (two scans that
/// found nothing frightening anywhere would agree without measuring
/// anything). Computed with the oracle's own predicate, off `rooms_at`, so it
/// is independent of the index under test.
fn oracle_frightening_pairs(
    roster: &[Body],
    ledger: &Ledger,
    folds: &OwnedFolds,
    terrain: &dyn Terrain,
    t: WorldTime,
) -> usize {
    let visited: Vec<Vec<Facet>> = {
        let mut store = folds.borrow_mut();
        let (visits, _) = store.latest_visit_and_trail(ledger);
        roster
            .iter()
            .map(|m| visits.rooms_at(m.entity, t))
            .collect()
    };
    roster
        .iter()
        .zip(&visited)
        .map(|(m, rooms)| {
            let mettle = mettle_factor(m.boldness);
            rooms
                .iter()
                .filter(|room| threat_field(room, &m.threat_niche, terrain) * mettle >= DANGER_ACT)
                .count()
        })
        .sum()
}

// ---------------------------------------------------------------------------
// The predicate-agreement sweep
// ---------------------------------------------------------------------------

/// The scan's predicate and the read's are the SAME function, which is what
/// lets one index per entity serve both callers (spec §2.3).
///
/// The scan asked `threat_field × mettle_factor ≥ DANGER_ACT`; the read asked
/// `feels_frightening(threat, 0.0, boldness)`, which is
/// `(threat × mettle_factor).clamp(0, 1) ≥ DANGER_ACT`. They differ only
/// where the clamp bites, and `DANGER_ACT` is `0.3`: a product above `1.0`
/// clamps to `1.0`, still ≥ `0.3`, and a product below `0.0` is unreachable
/// (`threat_value` is a dot of non-negative niche and hazard axes and
/// `mettle_factor` is `max(0, …)`). The sweep asserts it rather than arguing
/// it, and floors BOTH the clamp region and both verdicts so the equality is
/// not vacuous.
#[test]
fn the_scan_predicate_and_the_read_predicate_agree_over_the_whole_range() {
    let mut checked = 0_u64;
    let mut trues = 0_u64;
    let mut clamped = 0_u64;
    for ti in 0..=300_u32 {
        let threat = f64::from(ti) / 100.0;
        for bi in 0..=100_u32 {
            let boldness = f64::from(bi) / 100.0;
            let scan = threat * mettle_factor(boldness) >= DANGER_ACT;
            let read = feels_frightening(threat, 0.0, boldness);
            assert_eq!(
                scan, read,
                "the scan predicate and the read predicate disagree at threat {threat}, \
                 boldness {boldness} — one index cannot serve both callers"
            );
            checked += 1;
            if scan {
                trues += 1;
            }
            if threat * mettle_factor(boldness) > 1.0 {
                clamped += 1;
            }
        }
    }
    println!(
        "predicate sweep: {checked} points, {trues} frightening, {clamped} in the clamp region"
    );
    assert!(
        trues > 0 && trues < checked,
        "the sweep must contain both verdicts, or the equality is vacuous"
    );
    assert!(
        clamped > 0,
        "the sweep must reach the clamp region, or it never tests where the two could differ"
    );
}

// ---------------------------------------------------------------------------
// The index's terrain-ownership rule, demonstrated
// ---------------------------------------------------------------------------

/// A terrain whose UNCANNY hazard is planted by hand and whose every other
/// reading is a constant — the two-field pair the ownership test needs, with
/// no world to build.
struct PlantedGround {
    /// The rooms that read UNCANNY.
    uncanny: std::collections::BTreeSet<Facet>,
}

impl Terrain for PlantedGround {
    fn elevation(&self, _room: &Facet) -> f64 {
        0.0
    }
    fn is_fresh_water(&self, _room: &Facet) -> bool {
        false
    }
    fn temperature(&self, _room: &Facet, _day: WorldTime) -> f64 {
        15.0
    }
    fn hazards(&self, room: &Facet) -> Hazards {
        Hazards {
            uncanny: if self.uncanny.contains(room) {
                0.8
            } else {
                0.0
            },
            ..Hazards::ZERO
        }
    }
}

/// [`FrighteningGround`] belongs to ONE `(LocaleContext, predator field)`, and
/// this shows the aliasing that rule forbids rather than asserting it cannot
/// happen — the same shape as the room memo's own two-terrain test
/// (`a_room_memo_belongs_to_one_predator_field_and_a_second_field_gets_its_own`
/// in `windows/vessel/tests/suite/the_detent.rs`).
///
/// Two halves. **The rule obeyed:** an index per terrain, each answering as
/// itself — which is also the anti-vacuity floor, since two terrains that
/// agreed about the room could not show sharing at all. **The rule broken:**
/// one index advanced under the haunted field and then advanced again under
/// the safe one judges NOTHING (the cursor is at the trail's end and the room
/// is already in `judged`), so the safe field reads back the haunted field's
/// verdict. Nothing objects; the answer is simply wrong, and byte-visibly so.
#[test]
fn a_verdict_index_belongs_to_one_terrain_and_a_second_field_reads_the_first_ones_verdict() {
    let entity = EntityId::new(11).expect("11 is a valid entity id");
    let scary = Facet {
        face: 0,
        path: vec![1],
    };
    let day = WorldTime::from_ticks(WorldTime::TICKS_PER_STD_DAY);
    let trail = vec![(day, scary.clone())];
    // A mortal's niche: UNCANNY weighted 1, everything else 0. Steady
    // boldness leaves the felt threat unscaled (`mettle_factor(0.5) == 1`).
    let niche = ThreatNiche {
        uncanny: 1.0,
        heat: 0.0,
        cold: 0.0,
        predator: 0.0,
    };
    let boldness = 0.5;

    let haunted = PlantedGround {
        uncanny: std::iter::once(scary.clone()).collect(),
    };
    let safe = PlantedGround {
        uncanny: std::collections::BTreeSet::new(),
    };
    let judge = |terrain: &dyn Terrain, room: &Facet| {
        feels_frightening(threat_field(room, &niche, terrain), 0.0, boldness)
    };

    // The two fields genuinely disagree about this room, or nothing below can
    // see sharing. (`threat_field` is the maximum over the room AND its
    // neighbours, so the safe field must plant nothing anywhere near it — it
    // plants nothing at all.)
    assert!(
        judge(&haunted, &scary),
        "the haunted field must frighten here, or the test has no positive case"
    );
    assert!(
        !judge(&safe, &scary),
        "the safe field must NOT frighten here, or the two fields agree and the aliasing \
         below would be invisible"
    );

    // THE RULE OBEYED: one index per field, each answers as itself.
    for (terrain, want) in [
        (&haunted as &dyn Terrain, true),
        (&safe as &dyn Terrain, false),
    ] {
        let mut own = FrighteningGround::default();
        let judged = own.advance(entity, &trail, &mut |room| judge(terrain, room));
        assert_eq!(judged, 1, "the room is judged exactly once");
        assert_eq!(
            own.verdict(entity, &scary),
            Some(want),
            "an index of its own must answer as its own terrain"
        );
        assert_eq!(
            own.frightening_at(entity, day).is_empty(),
            !want,
            "the held prefix must agree with the verdict"
        );
    }

    // THE RULE BROKEN: one index, two fields. Advanced under the haunted
    // field first, then handed the safe field's judge.
    let mut shared = FrighteningGround::default();
    shared.advance(entity, &trail, &mut |room| judge(&haunted, room));
    let judged_again = shared.advance(entity, &trail, &mut |room| judge(&safe, room));
    assert_eq!(
        judged_again, 0,
        "the second field's judge is never CALLED — the cursor is at the trail's end and the \
         room already carries a verdict, which is why the staleness is silent"
    );
    assert_eq!(
        shared.verdict(entity, &scary),
        Some(true),
        "one index shared across two hazard fields returns the FIRST field's verdict under \
         the second — this is the aliasing the ownership rule forbids: an index belongs to \
         one (LocaleContext, predator field), and a caller reading under a different field \
         builds a new store (see FrighteningGround's type doc)"
    );
    assert_eq!(
        shared.frightening_at(entity, day).len(),
        1,
        "and the shunned set the safe field would read is the haunted field's, not its own"
    );
}

// ---------------------------------------------------------------------------
// The bench shape, in-crate
// ---------------------------------------------------------------------------

/// A deterministic UNCANNY overlay on top of a live `LocaleTerrain` — the one
/// input that gives these shapes any frightening ground at all.
///
/// **Why the test cannot use the world's own hazards, measured rather than
/// assumed.** The first two drafts of this file ran the bench shape on the
/// world's terrain — once with `predator: None` (exactly
/// `tests/suite/the_detent.rs`'s `bench_shape`) and once with the real
/// predator-pressure field `Session::start` builds — and both printed
/// `0 emitter slots, 0 frightening (member, room) pairs` on seeds 42 AND 6.
/// The settled peoples `derive_npcs` returns never stand on ground that
/// frightens them (which is the whole reason the seed-42 ledger hash is blind
/// to the fear path); seed 6's emitter is a WILD HERD body, and
/// `derive_herd_bodies` is private to `crate::session`, so the herd half of a
/// session's roster is not reachable from here. An equality test over two
/// empty sets proves nothing, so the hazard is planted instead: every 29th
/// packed room reads UNCANNY, which `threat_field`'s max-over-neighbours
/// spreads to roughly a third of the map and leaves the rest safe — both
/// verdicts present, in one deterministic rule with no world dependence.
///
/// The overlay sits OUTSIDE the room memo (`LocaleTerrain::with_ground`), so
/// the memoised half is still the live field blend and this adds to it.
struct HauntedTerrain<'a> {
    /// The live terrain (with its room memo) this adds to.
    inner: &'a dyn Terrain,
}

/// One in every `HAUNT_MODULUS` packed rooms is uncanny. Sparse on purpose:
/// `threat_field` takes the maximum over a room AND its neighbours, so a
/// denser rule would make every roster member an emitter and the
/// non-emitter half of the comparison vacuous.
/// plumb: universal(a synthetic probe fixture's own overlay rule, deliberately world- and species-independent so both verdicts appear on any shape — no world reads it and nothing about it varies)
const HAUNT_MODULUS: u64 = 29;

/// The UNCANNY magnitude a haunted room carries — well above `DANGER_ACT`
/// (0.3) at steady boldness, so the verdict is not sitting on the threshold.
/// plumb: universal(the same synthetic probe fixture's planted magnitude, chosen to clear DANGER_ACT by a wide margin — not a world value, so it varies along no axis a world has)
const HAUNT_UNCANNY: f64 = 0.8;

impl Terrain for HauntedTerrain<'_> {
    fn elevation(&self, room: &Facet) -> f64 {
        self.inner.elevation(room)
    }
    fn is_fresh_water(&self, room: &Facet) -> bool {
        self.inner.is_fresh_water(room)
    }
    fn temperature(&self, room: &Facet, day: WorldTime) -> f64 {
        self.inner.temperature(room, day)
    }
    fn solar_altitude(&self, room: &Facet, day: WorldTime) -> Option<f64> {
        self.inner.solar_altitude(room, day)
    }
    fn day_ticks(&self) -> Option<TickSpan> {
        self.inner.day_ticks()
    }
    fn forage_value(&self, room: &Facet) -> f64 {
        self.inner.forage_value(room)
    }
    fn hazards(&self, room: &Facet) -> Hazards {
        let mut h = self.inner.hazards(room);
        if room.pack().is_ok_and(|id| id.0 % HAUNT_MODULUS == 0) {
            h.uncanny = h.uncanny.max(HAUNT_UNCANNY);
        }
        h
    }
    fn is_built(&self, room: &Facet) -> bool {
        self.inner.is_built(room)
    }
    fn is_cold(&self, room: &Facet) -> bool {
        self.inner.is_cold(room)
    }
    fn prey_value(&self, room: &Facet) -> f64 {
        self.inner.prey_value(room)
    }
}

/// The in-crate mirror of `windows/vessel/tests/suite/the_detent.rs`'s
/// `bench_shape`, minus the counting terrain and the per-tick counters: the
/// world at `seed`, `agents` derived bodies, `ticks` ticks of
/// `DriveMovements::step_with_occupancy` over ONE caller-owned resident store,
/// mesh memo, nav cache and room memo. An integration test's `common::build`
/// is not reachable from `src/`, so the construction is repeated here.
///
/// Borrowed rather than owned, so a probe sees the LIVE store and room memo
/// the walk has been filling — the production shape, where the verdict index
/// is already warm when the scan is asked.
struct ScanShape<'a> {
    /// The locale context the terrain reads.
    ctx: &'a LocaleContext,
    /// The world's predator-pressure field — the reason any ground on these
    /// worlds is frightening at all (see `run_scan_shape`).
    predator: Option<&'a hornvale_kernel::VertexMap<f64>>,
    /// The ledger as of the probed tick.
    ledger: &'a Ledger,
    /// The derived roster.
    npcs: &'a [Body],
    /// The walk's own resident store (the verdict index lives here).
    folds: &'a OwnedFolds,
    /// The walk's own room mesh memo.
    mesh_memo: &'a RoomMeshMemo,
    /// The walk's own room memo.
    ground: &'a OwnedGround,
    /// The probed tick's instant.
    day: WorldTime,
}

/// Run the shape, calling `probe` after every tick whose 1-based index is in
/// `probe_at` — the ledger is the tick's own, so the probes see a growing
/// history rather than only the final one.
fn run_scan_shape(
    seed: u64,
    ticks: usize,
    agents: usize,
    probe_at: &[usize],
    probe: &mut dyn FnMut(&ScanShape<'_>),
) {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &Default::default(),
        &Default::default(),
        &Default::default(),
    )
    .expect("the seed builds a world");
    // The world-scoped derivation `Session::start` itself performs, so the
    // terrain below carries the PREDATOR-PRESSURE field (The Quarry).
    //
    // Without it there is no frightening ground anywhere on these worlds:
    // `tests/suite/the_detent.rs`'s `bench_shape` passes `predator: None` and
    // its roster's every room reads as safe, so an oracle comparison run over
    // that shape would compare two empty sets on both seeds and both
    // predicates. Measured, not assumed — the first draft of this test did
    // exactly that and printed `0 emitter slots, 0 frightening pairs` on
    // seeds 42 and 6 alike. The field is the one input that makes the
    // comparison bite, so it is built here.
    let wctx = crate::session::WorldContext::build(&world).expect("the world context builds");
    let predator = match (
        wctx.wc.as_ref(),
        wctx.terrain.as_ref(),
        wctx.report.as_ref(),
    ) {
        (Some(wc), Some(t), Some(r)) => Some(hornvale_worldgen::predator_pressure_from(wc, t, r)),
        _ => None,
    };
    assert!(
        predator.is_some(),
        "seed {seed}'s predator field must build, or this shape has no frightening ground"
    );
    let ctx = &wctx.ctx;
    let home_settlement = hornvale_settlement::village_info(&world)
        .expect("the flagship always exists")
        .id;
    let day_ticks = hornvale_worldgen::sky_of(&world)
        .ok()
        .and_then(|sky| sky.calendar().cloned())
        .and_then(|c| c.day_ticks());
    let mut ledger = world.ledger.clone();
    let mut registry = world.registry.clone();
    for (pred, doc) in [
        (AGENT_AT, "an agent's position on a day"),
        (DRANK, "an agent satisfied its sustenance goal"),
        (RESTED, "an agent rested on a day, for this many ticks"),
        (SLEPT, "an agent slept on a day, for this many ticks"),
        (EATEN, "an agent ate (eased its hunger) on a day"),
    ] {
        registry
            .register_predicate(pred, false, doc)
            .expect("the drive predicates register identically every run");
    }
    let npcs = derive_npcs(&world, ctx, &mut ledger, agents, home_settlement);
    let mut mesh_memo = RoomMeshMemo::new();
    let mut home_nav_cache = HomeNavCache::new();
    let folds = OwnedFolds::new(ResidentFolds::new());
    let ground: OwnedGround = OwnedGround::new(GroundHazards::new());
    let mut day = WorldTime::from_std_days(0.5).expect("0.5 is a finite day count");
    for tick in 1..=ticks {
        let from = day;
        day = WorldTime::from_ticks(day.ticks() + WorldTime::TICKS_PER_STD_DAY);
        let mesh_snapshot = mesh_memo.clone();
        {
            let base = LocaleTerrain::with_fields(
                ctx,
                None,
                predator.as_ref(),
                None,
                None,
                Some(&mesh_snapshot),
            )
            .with_ground(&ground);
            let terrain = HauntedTerrain { inner: &base };
            let sys = DriveMovements {
                npcs: npcs.clone(),
                from,
                to: day,
                params: SUSTENANCE,
                day_ticks,
                terrain: &terrain,
                folds: &folds,
            };
            // The third element is the roster write-back `Session::wait` needs
            // (The Rack, Task 3); this sampler owns no roster, so it is dropped.
            let (facts, _occupancy, _written) =
                sys.step_with_occupancy(&ledger, &mut mesh_memo, &mut home_nav_cache);
            for fact in facts {
                ledger
                    .commit(fact, &registry)
                    .expect("a drive-movements fact commits");
            }
        }
        if probe_at.contains(&tick) {
            probe(&ScanShape {
                ctx,
                predator: predator.as_ref(),
                ledger: &ledger,
                npcs: &npcs,
                folds: &folds,
                mesh_memo: &mesh_memo,
                ground: &ground,
                day,
            });
        }
    }
}

/// One probe: the new scan and the new emitter-free read must equal the
/// oracles at `t`, for every roster member. Returns
/// `(emitters_found, frightening_pairs, emitter_free_comparisons, shunned_rooms)`.
fn compare_at(shape: &ScanShape<'_>, t: WorldTime) -> (usize, usize, usize, usize) {
    let mesh = shape.mesh_memo.clone();
    let base = LocaleTerrain::with_fields(shape.ctx, None, shape.predator, None, None, Some(&mesh))
        .with_ground(shape.ground);
    let terrain = HauntedTerrain { inner: &base };

    let (want_is_emitter, want_rooms) =
        scan_oracle(shape.npcs, shape.ledger, shape.folds, &terrain, t);
    let pairs = oracle_frightening_pairs(shape.npcs, shape.ledger, shape.folds, &terrain, t);

    let got = build_emitter_scan(shape.npcs, shape.ledger, shape.folds, &terrain, t);
    let got_is_emitter: Vec<bool> = shape
        .npcs
        .iter()
        .map(|m| got.emitters.iter().any(|(b, _)| b.entity == m.entity))
        .collect();
    assert_eq!(
        got_is_emitter, want_is_emitter,
        "the indexed scan's is_emitter vector at {t:?} must equal the oracle's"
    );
    assert_eq!(
        got.alarm_source_rooms, want_rooms,
        "the indexed scan's alarm halo at {t:?} must equal the oracle's"
    );

    // The emitter-free read. Two rosters reach it, and this compares both:
    //
    // - the EMPTY roster — the recursion's own base case (`alarm_field`'s
    //   inner `affect_of` passes an empty band, which threads through here),
    //   and the only one the haunted shape can reach, since the overlay makes
    //   most members emitters under the full roster;
    // - the FULL roster, wherever the scan finds no emitter in it at all
    //   (the settled world's common case).
    let mut compared = 0_usize;
    let mut shunned_total = 0_usize;
    let mut rosters: Vec<&[Body]> = vec![&[]];
    if got.emitters.is_empty() {
        rosters.push(shape.npcs);
    }
    for roster in rosters {
        for npc in shape.npcs {
            let latest = {
                let mut store = shape.folds.borrow_mut();
                let (visits, _) = store.latest_visit_and_trail(shape.ledger);
                visits.latest_at(npc.entity, t)
            };
            let want = emitter_free_oracle(&latest, npc, &terrain, shape.ledger, shape.folds);
            let mut memo = PrimaryAfraidMemo::new();
            let mem = hazard_memory_memo(
                shape.ledger,
                shape.folds,
                npc,
                t,
                &terrain,
                roster,
                &mut memo,
            );
            assert_eq!(
                mem.shunned, want,
                "the indexed emitter-free read's shunned set at {t:?} must equal the oracle's"
            );
            assert!(
                mem.dread.is_empty(),
                "the emitter-free path records no dread — it returns before dread exists"
            );
            shunned_total += mem.shunned.len();
            compared += 1;
        }
    }
    (got.emitters.len(), pairs, compared, shunned_total)
}

/// FOLD-equals-SCAN on the bench shape, at the tick's own instant and one
/// standard day earlier (the lab's past-instant read), on TWO seeds: 42,
/// whose derived roster has no emitter, and 6, whose roster has one.
///
/// `6` is `EMITTER_SEED` in `windows/vessel/tests/suite/ledger_hash_witness.rs`
/// — hard-coded here because an integration test's constant is not reachable
/// from `src/`.
///
/// claim: invariant(forall-seed) — FOLD-equals-SCAN is a per-seed identity,
/// not a rate: the indexed scan must equal the oracle at EVERY probe on
/// EVERY seed, and one disagreement anywhere falsifies it. The two seeds are
/// pinned rather than swept because they cover the two shapes the equality
/// has to survive (a roster with an emitter and one without), and adding
/// seeds would buy repetition rather than coverage.
#[test]
fn the_indexed_scan_and_read_equal_the_pre_index_oracles() {
    let mut emitters_total = 0_usize;
    let mut slots_total = 0_usize;
    let mut pairs_total = 0_usize;
    let mut emitter_free_reads = 0_usize;
    let mut shunned_total = 0_usize;
    let mut probes = 0_usize;
    let mut per_seed: Vec<(u64, usize, usize)> = Vec::new();

    for seed in [42_u64, 6_u64] {
        let mut seed_emitters = 0_usize;
        let mut seed_pairs = 0_usize;
        run_scan_shape(seed, 15, 10, &[5, 10, 15], &mut |shape| {
            let day_earlier =
                WorldTime::from_ticks(shape.day.ticks() - WorldTime::TICKS_PER_STD_DAY);
            for t in [shape.day, day_earlier] {
                let (e, p, c, sh) = compare_at(shape, t);
                shunned_total += sh;
                slots_total += shape.npcs.len();
                seed_emitters += e;
                seed_pairs += p;
                emitter_free_reads += c;
                probes += 1;
            }
        });
        println!(
            "seed {seed}: {seed_emitters} emitter slots across the probes, \
             {seed_pairs} (member, room) pairs judged frightening"
        );
        per_seed.push((seed, seed_emitters, seed_pairs));
        emitters_total += seed_emitters;
        pairs_total += seed_pairs;
    }

    println!(
        "FOLD-equals-SCAN: {probes} probes, {emitters_total} emitter slots of {slots_total}, \
         {pairs_total} frightening (member, room) pairs, {emitter_free_reads} emitter-free reads \
         compared over {shunned_total} shunned rooms"
    );
    assert!(
        emitters_total < slots_total,
        "EVERY member was an emitter at every probe ({emitters_total} of {slots_total}) — the \
         is_emitter comparison cannot see a false verdict"
    );
    assert!(
        emitters_total > 0,
        "no probe found an emitter across seeds 42 and 6 — the is_emitter comparison is vacuous \
         (per seed: {per_seed:?})"
    );
    assert!(
        pairs_total > 0,
        "no room was judged frightening across seeds 42 and 6 — the halo comparison is vacuous \
         (per seed: {per_seed:?})"
    );
    assert!(
        emitter_free_reads > 0,
        "the emitter-free read was never compared — the second half of the equality is vacuous"
    );
    assert!(
        shunned_total > 0,
        "every emitter-free read returned an EMPTY shunned set — the set comparison is vacuous"
    );
}

// ---------------------------------------------------------------------------
// The Pawl's EmitterScan equivalence tests (The Detent Task 7: moved here
// from `liveness.rs`'s own `mod tests`, unchanged in body, because
// `EmitterScan`/`build_emitter_scan` are private to `crate::liveness` and
// this file is that module's sibling. `PlantedTerrain` and the fixture
// helpers they call stayed in `mod tests` (128 other uses depend on them
// living there) and are reached here as `pub(super)` re-exports via
// `super::tests`.
// ---------------------------------------------------------------------------

/// The old scan's emitter list: each ever-terrain-afraid member with its
/// day-sorted `f64` timeline, exactly as `EmitterScan` held it before this
/// campaign. Named only because the tuple is too wide to spell inline.
type OracleEmitters = Vec<(Body, Vec<(f64, Facet)>)>;

/// A VERBATIM COPY of the OLD `build_emitter_scan` body — the SCAN half of
/// the emitter scan's equivalence, before it read the resident store.
///
/// Copied rather than called, the same rule every other oracle in this
/// campaign follows (spec §5, and the pre-flight ruling in the campaign
/// ledger): the production body is gone, so this is the only statement of
/// the old one left, and an oracle sharing code with the thing under test
/// cannot falsify it.
fn emitter_scan_oracle(
    roster: &[Body],
    ledger: &Ledger,
    terrain: &dyn Terrain,
    t: WorldTime,
) -> (OracleEmitters, std::collections::BTreeSet<Facet>) {
    let mut emitters: Vec<(Body, Vec<(f64, Facet)>)> = Vec::new();
    let mut alarm_source_rooms: std::collections::BTreeSet<Facet> =
        std::collections::BTreeSet::new();
    for m in roster {
        let mettle = mettle_factor(m.boldness);
        let frightening =
            |room: &Facet| threat_field(room, &m.threat_niche, terrain) * mettle >= DANGER_ACT;
        let mut timeline: Vec<(f64, Facet)> = ledger
            .facts_of(m.entity, AGENT_AT)
            .filter_map(|f| {
                let d = f.day.filter(|d| *d <= t)?.as_std_days();
                match &f.object {
                    Value::Text(s) => Some((d, room_from_text(s))),
                    _ => None,
                }
            })
            .collect();
        timeline.sort_by(|a, b| a.0.total_cmp(&b.0));
        let mut ever = false;
        let mut note_halo = |p: &Facet| {
            alarm_source_rooms.insert(p.clone());
            for n in p.neighbors() {
                alarm_source_rooms.insert(n);
            }
        };
        if frightening(&m.home) {
            ever = true;
            note_halo(&m.home);
        }
        for (_, p) in &timeline {
            if frightening(p) {
                ever = true;
                note_halo(p);
            }
        }
        if ever {
            emitters.push((m.clone(), timeline));
        }
    }
    (emitters, alarm_source_rooms)
}

/// The OLD `position_at`, copied for the same reason — it reads an `f64`
/// timeline where the production one now reads `Trail`'s ticks.
fn position_at_oracle(m: &Body, timeline: &[(f64, Facet)], day: f64) -> Facet {
    let idx = timeline.partition_point(|(d, _)| *d <= day);
    if idx == 0 {
        m.home.clone()
    } else {
        timeline[idx - 1].1.clone()
    }
}

/// A roster ledger with THREE members, walking rooms of both kinds, with
/// strictly increasing days per member — the shape a real walk commits
/// (`clock::cost_of` floors at one tick, so a walker's sightings never
/// share an instant; measured in
/// `windows/vessel/tests/suite/resident_folds.rs`'s
/// `the_walk_never_commits_two_sightings_of_one_entity_at_one_instant`).
///
/// Returns the roster, the ledger and the terrain. The middle member never
/// stands anywhere frightening, so the `ever` test is exercised in BOTH
/// directions — an oracle and a production scan that both returned every
/// member would agree vacuously.
fn emitter_scan_fixture() -> (Vec<Body>, Ledger, PlantedTerrain) {
    let reg = agent_at_reg();
    let mut ledger = Ledger::default();
    let (d_room, hazard, x) = phantom_triple();
    let terrain = PlantedTerrain::hazard(std::iter::empty(), [(hazard.clone(), 0.8)]);

    // A: walks onto the hazard and off again, revisiting one room.
    let a_e = ledger.mint_entity(test_lineage(ledger.entity_count() as u16));
    let a = haunt_npc(a_e, x.clone());
    commit_agent_at(&mut ledger, &reg, a_e, &d_room, 0.5);
    commit_agent_at(&mut ledger, &reg, a_e, &hazard, 1.5);
    commit_agent_at(&mut ledger, &reg, a_e, &d_room, 2.5);
    commit_agent_at(&mut ledger, &reg, a_e, &x, 7.5);

    // B: never leaves safe ground, and its home is safe — never an emitter.
    // NOT `d_room`: `threat_field` is the maximum over a room AND its
    // neighbours, and `d_room` is one hop from the hazard, so standing
    // there frightens. (That is what makes `d_room` an alarm SOURCE for A
    // above; putting B there made every member of the fixture an emitter
    // and the equivalence below vacuous.)
    let far = raddr(-1.0);
    let b_e = ledger.mint_entity(test_lineage(ledger.entity_count() as u16));
    let b = haunt_npc(b_e, x.clone());
    commit_agent_at(&mut ledger, &reg, b_e, &x, 0.5);
    commit_agent_at(&mut ledger, &reg, b_e, &far, 3.5);
    commit_agent_at(&mut ledger, &reg, b_e, &x, 5.5);

    // C: never committed a sighting at all, but LIVES on the hazard — the
    // `frightening(&m.home)` arm, which the timeline loop cannot reach.
    let c_e = ledger.mint_entity(test_lineage(ledger.entity_count() as u16));
    let c = haunt_npc(c_e, hazard.clone());

    (vec![a, b, c], ledger, terrain)
}

#[test]
fn the_emitter_scan_fixture_separates_emitters_from_non_emitters() {
    // The anti-vacuity guard: if every member (or none) were an emitter,
    // the equivalence below would hold for a scan that ignored terrain.
    let (roster, ledger, terrain) = emitter_scan_fixture();
    let t = WorldTime::from_std_days(10.0).expect("a day value is finite");
    let (emitters, rooms) = emitter_scan_oracle(&roster, &ledger, &terrain, t);
    assert_eq!(
        emitters.len(),
        2,
        "the fixture must hold both emitters and non-emitters: {} of {} members emit",
        emitters.len(),
        roster.len()
    );
    assert!(
        !rooms.is_empty(),
        "the fixture must produce a non-empty alarm halo, or the set comparison below \
         compares two empty sets"
    );
}

#[test]
fn the_emitter_scan_read_off_the_store_equals_the_old_ledger_scan() {
    let (roster, ledger, terrain) = emitter_scan_fixture();
    // Every instant the fixture can distinguish, and the ones either side.
    let mut instants: Vec<WorldTime> = Vec::new();
    for f in ledger.iter() {
        if let Some(d) = f.day {
            for delta in [-1_i64, 0, 1] {
                instants.push(WorldTime::from_ticks(d.ticks() + delta));
            }
        }
    }
    instants.push(WorldTime::GENESIS);
    instants.push(WorldTime::from_std_days(100.0).expect("a day value is finite"));
    instants.sort();
    instants.dedup();

    let mut compared = 0_u64;
    let mut non_empty_scans = 0_u64;
    for t in &instants {
        let folds = test_folds();
        let got = build_emitter_scan(&roster, &ledger, &folds, &terrain, *t);
        let (want_emitters, want_rooms) = emitter_scan_oracle(&roster, &ledger, &terrain, *t);

        assert_eq!(
            got.alarm_source_rooms, want_rooms,
            "the alarm halo at {t:?} must equal the old ledger scan's"
        );
        assert_eq!(
            got.emitters
                .iter()
                .map(|(m, _)| m.entity)
                .collect::<Vec<_>>(),
            want_emitters
                .iter()
                .map(|(m, _)| m.entity)
                .collect::<Vec<_>>(),
            "the emitter roster at {t:?} must equal the old ledger scan's, in order"
        );
        if !got.emitters.is_empty() {
            non_empty_scans += 1;
        }

        // And `position_at` must answer identically at every instant, for
        // every emitter — the read the timeline exists for.
        for ((m, new_tl), (om, old_tl)) in got.emitters.iter().zip(want_emitters.iter()) {
            assert_eq!(m.entity, om.entity);
            for q in &instants {
                let idx = new_tl.partition_point(|(d, _)| *d <= *q);
                let new_pos = if idx == 0 {
                    m.home.clone()
                } else {
                    new_tl[idx - 1].1.clone()
                };
                let old_pos = position_at_oracle(om, old_tl, q.as_std_days());
                assert_eq!(
                    new_pos, old_pos,
                    "the emitter {:?}'s position at {q:?} (scan built at {t:?}) must \
                     equal the old timeline's",
                    m.entity
                );
                compared += 1;
            }
        }
    }
    assert!(
        non_empty_scans > 0,
        "no instant produced an emitter, so no `position_at` answer was compared"
    );
    assert!(
        compared >= 100,
        "the sweep must make a real number of position comparisons: {compared}"
    );
}
