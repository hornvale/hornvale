//! The session turn's **cost gate** (The Panes, Task 4): what one possessed
//! turn costs now that every snapshot carries the band's spatial cells.
//!
//! Budgets here are **falsification ceilings, not targets**, in the sense
//! `graph_cost.rs` established and `scene_cost.rs` restates: set above the
//! measured value so only a real regression trips them.
//!
//! **Ceilings ratchet DOWN freely. Raising one is an explicit, reviewed act**
//! and must be recorded in that constant's doc comment with the reason.
//!
//! The instrument that produced these numbers is
//! `windows/vessel/examples/turn_cost.rs`, which holds the full matched-pair
//! reading (before and after the channel) and the per-verb-class split. That
//! bench times `handle` and `snapshot()+json` separately so the split is
//! visible per class; this gate ALSO times them separately now (fix round
//! 1), but pools by two different cuts across the same ten `SEQUENCE` turns:
//! `TURN_BUDGET_MS` pools `handle+snapshot+json` together across every verb,
//! and `INDOOR_SNAPSHOT_BUDGET_MS` pools `snapshot()+json` alone across only
//! the turns a fresh `snapshot()` itself reports as indoors
//! (`SessionSnapshot::spatial`). The indoors/outdoors cut, not verb class,
//! is what tracks the real cost axis — see `INDOOR_SNAPSHOT_BUDGET_MS`'s own
//! doc for why a verb-class ceiling would have missed the same gap it
//! closes. Re-run the bench, not this test, when you want the release-
//! profile per-verb-class split too.
//!
//! **Read a red run against `scene_cost.rs`'s corrected discriminator, not
//! the old "uniform vs. local" rule** — see that file's module doc for the
//! full correction. The same resource-profile reasoning applies here:
//! `Session::start` builds a world from scratch (`terrain_of` ->
//! `hornvale_terrain::generate`, uncached), so it is the contention-sensitive
//! metric; the per-turn metrics (`handle+snapshot+json`, indoor
//! `snapshot()+json`) run against a `World` already built in memory, so they
//! are the control set. A saturated runner is expected to starve `start`
//! alone and leave the controls near their basis — the verdict below is the
//! same arithmetic `scene_cost.rs` uses, not a uniformity check.

use hornvale_kernel::Seed;
use hornvale_vessel::{PossessOpts, Session, SpatialChannel};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

// The measurement harness times derivation calls for a diagnostic (never sim
// logic, never a fact, never seeded from wall-clock) -- exempt from the
// wall-clock ban (clippy.toml / decision 0001), same pattern as
// `cli/tests/scene_cost.rs` and `cli/tests/graph_cost.rs`.
#[allow(clippy::disallowed_types)]
use std::time::Instant;

/// The fixed verb sequence, mirrored verbatim from
/// `windows/vessel/examples/turn_cost.rs::SEQUENCE`: verbs that move the
/// possession (`enter`, `out`), a verb that advances the day (`wait`), and
/// verbs that do neither (`look`, `map`, `examine`).
const SEQUENCE: &[&str] = &[
    "look",
    "map",
    "examine me",
    "wait 1",
    "look",
    "enter",
    "map",
    "look",
    "out",
    "look",
];

/// How many times to run the sequence. Medians of repeated runs, mirrored
/// from the bench.
const RUNS: usize = 5;

/// Ceiling for `Session::start`, ms.
///
/// **Dev profile is the ceiling basis** (this box, `MacBookPro`, `cargo test
/// -p hornvale --test session_cost -- --ignored --nocapture`, 2026-08-06),
/// the same convention `scene_cost.rs` follows, since `make gate-full`
/// (`scripts/gate-full-heavy.sh`) runs the heavy tier via plain `cargo
/// nextest run`, without `--release`. Three runs gave 3442.192, 3177.372,
/// 3289.123 ms — the slowest, 3442.192, is the ceiling basis.
///
/// **Budgeted at ~3x, not the usual ~2x**, because `Session::start` is
/// wall-clock noisy on this shared box: it re-sculpts terrain from scratch
/// on every call (`terrain_of` -> `hornvale_terrain::generate`, uncached at
/// the `World` level), and Task 1's own baseline reading swung from 1451 ms
/// to 2280 ms between two runs taken minutes apart (see Task 1's report,
/// `--release`) — a ~1.6x swing on its own. A ~2x ceiling on the slowest of
/// three same-session runs would still be a coin flip against a swing that
/// size; ~3x gives headroom against contention without hiding a real
/// regression, which would need to be far larger than this noise band to
/// trip it.
///
/// **Re-measured, kept unchanged (The Sighting, Task 6), 2026-08-06,
/// `MacBookPro`, quiet box (`uptime` 1-min load 3.8-4.7 throughout, after
/// waiting out another session's `nextest run` that had it at 19-42).**
/// Three fresh dev-profile runs gave 2757.136, 2712.546, 2803.291 ms —
/// slowest 2803.291, well *under* the 3442.192 ms basis above, not over it.
/// That is not evidence the ceiling can safely ratchet down: this session's
/// three runs were themselves tightly clustered (656-705 ms on the parallel
/// `--release` bench, run back to back), yet the cross-session record for
/// this same constant now spans roughly 657-3442 ms — a >5x range — purely
/// from terrain-resculpt noise on a shared box, not from any code this
/// campaign touched (Tasks 2-5 added spatial/creature-sighting cost to
/// `snapshot()`, never to genesis). Recomputing the multiplier from a
/// single quiet session's tight cluster would fit *this* session's noise
/// band and nothing else's; the ceiling stays at its previous value, and
/// this paragraph exists so the next re-measurement has both endpoints of
/// the observed range rather than just the newest one.
const START_BUDGET_MS: f64 = 10500.0;

/// Ceiling for one `handle` + `snapshot` + serialize, ms — a **pooled
/// median** across all ten verbs in [`SEQUENCE`] (moving, day-advancing,
/// and neither alike), not a per-turn ceiling every individual turn stays
/// under. Fix round 1 review found 20 of the 50 pooled samples this median
/// is drawn from exceed this very ceiling while the gate still passes — see
/// [`INDOOR_SNAPSHOT_BUDGET_MS`] for the ceiling that closes that gap.
///
/// **Dev profile is the ceiling basis**, same box/command/date as
/// [`START_BUDGET_MS`]. Three runs of this test's own combined-timer
/// measurement gave a pooled median of 3.906, 3.543, 3.740 ms — slowest
/// 3.906 ms. Budgeted at ~2x, rounded up.
///
/// The bench (`windows/vessel/examples/turn_cost.rs`) times `handle` and
/// `snapshot()+json` separately, under `--release`, and additionally splits
/// by verb class; its release-profile pooled `snapshot()+json` reading
/// (slowest of three runs: 1.249 ms) is NOT comparable to this ceiling —
/// different profile, different thing measured (split vs combined) — see
/// that file's own doc comment for the release-profile numbers and the
/// per-class split.
///
/// **Re-measured, kept unchanged (The Sighting, Task 6), 2026-08-06, same
/// quiet box as [`START_BUDGET_MS`].** Three fresh dev-profile runs of this
/// test's own pooled combined timer gave 3.769, 3.721, 3.939 ms — slowest
/// 3.939 ms, essentially flat against the 3.906 ms basis above (+0.8%) even
/// though Tasks 2-5 added real per-turn work (the anchor embedding, the
/// shadowcast, creature marks) inside `snapshot()`. `2 * 3.939 = 7.878`,
/// which still rounds up to the existing 8.0 — no change needed.
///
/// **Why the pooled figure looks flat while the release-profile bench does
/// not**: it is dominated by the `Neither` class (35 of 50 pooled samples),
/// and this gate is not designed to see a per-class shift — its own header
/// comment says so ("Re-run the bench, not this test, when you want to know
/// what moved or which verb class is responsible"). The `--release` bench
/// (below) found the real signal: `moving`-class `snapshot()+json` grew
/// 1.259 ms -> 3.706 ms (**2.94x**) — **re-measured at the merge as
/// 0.969 -> 3.380 ms (3.49x)**, once The Tense's reseed of seed 42 replaced
/// the 19x10 first chamber with a 19x19 one; the original pair is kept as
/// taken, on the smaller room, and the matched re-measurement is recorded in
/// `turn_cost.rs`'s closing section. The mechanism is unchanged: `enter` is
/// where the possession
/// first stands inside a chamber and `snapshot()` derives `sighting()`
/// fresh, uncached, on every call. A single indoor `enter` turn's combined
/// `handle` (11.093 ms) + `snapshot()+json` (3.706 ms) is 14.799 ms under
/// `--release` — already over this ceiling's *dev-profile* 8.0 ms figure by
/// comparison, though that comparison mixes profiles and isn't the number
/// this test gates on. The pooled dev-profile median stays comfortably
/// under budget only because 25 of the 35 `Neither` samples never touch a
/// chamber (`Neither` positions in [`SEQUENCE`] are 0, 1, 2, 4, 6, 7, 9 —
/// seven per run, five runs = 35 pooled; positions 6 and 7 are indoors, so
/// 10 pooled samples are indoor and 25 are outdoor); a `SEQUENCE` weighted
/// more toward chamber turns would surface this in the pooled figure too.
/// **This is exactly the gap fix round 1 review found and
/// [`INDOOR_SNAPSHOT_BUDGET_MS`] closes** — a position-in-`SEQUENCE` probe
/// (not verb-class) found 20 of the 50 pooled `turns` samples this median
/// is drawn from exceed 8.0 ms individually, entirely invisible to a
/// pooled-median gate.
const TURN_BUDGET_MS: f64 = 8.0;

/// Ceiling for one **indoor** `snapshot()+json`, ms — the cut fix round 1
/// review found and `TURN_BUDGET_MS` cannot see.
///
/// **Why this exists, not a per-verb-class ceiling.** A position-in-
/// `SEQUENCE` probe (dev profile, quiet box, 5 reps) found 20 of the 50
/// pooled `TURN_BUDGET_MS` samples exceed 8.0 ms while the gate still
/// passes, because verb class straddles the real axis: `out` is `Moving`
/// and cheap (2.26 ms combined); `enter` is `Moving` and costs 34.20 ms;
/// `map`/`look` right after `enter` are `Neither` and cost 9.05/16.86 ms
/// indoors against 1.3-3.9 ms for the same verbs outdoors. A per-class
/// ceiling on `Moving` would have been blind to the two `Neither` samples
/// already over budget — the identical failure one layer down — and it
/// would freeze a point over a bimodal population (`enter`'s own handle
/// time is ~34 ms on some reps, ~2.3 ms on others, depending on where the
/// sort lands it against `out`), which is the freeze-a-distribution trap.
/// **Indoors vs outdoors is the real cut**: it is what `sighting()`'s own
/// guard clause (`self.inside.as_ref()?`) branches on, and it is directly
/// readable off `SessionSnapshot::spatial` without inferring it from a verb
/// string.
///
/// **What this gates, and why not the combined figure**: `snapshot()+json`
/// alone, not `handle+snapshot+json`. The indoor `snapshot()+json`
/// distribution is tight (three-verb medians 9.153 / 8.996 / 8.617 ms in
/// the probe above, a ~6% spread) because it is dominated by one fixed
/// cost — `sighting()`, re-derived fresh on every call regardless of which
/// verb ran. Indoor *combined* (`handle+snapshot+json`) is not tight: it
/// spans roughly 9-34 ms because `handle`'s own cost varies hugely by verb
/// (`enter` additionally embeds the lattice and places anchors; `map`/
/// `look` do not). A ~2x ceiling over a population that already spans ~4x
/// on its own would be fitting noise, not gating a regression.
///
/// Basis: dev profile, this box (`MacBookPro`), 2026-08-06, quiet
/// (`uptime` 1-min load 1.4-2.1 throughout). Three fresh runs of this
/// test's own indoor-`snapshot()+json` median (pooled across all indoor
/// samples — `enter`, `map`, `look` alike, 15 per run) gave 8.910, 8.503,
/// 8.530 ms — slowest 8.910 ms. Budgeted at ~2x, rounded up: `2 * 8.910 =
/// 17.82` -> 18.0.
///
/// # **RAISED by The Assize: 18.0 -> 40.0 ms**, and the basis with it
///
/// An explicit, reviewed act (Nathan, 2026-08-08), recorded here because the
/// ratchet rule requires a raise to be exactly that. **Ceilings still ratchet
/// DOWN freely; this is the deliberate exception, not a precedent for
/// re-baselining a red.**
///
/// **CAUSE, BISECTED RATHER THAN ASSUMED.** The metric read 18.720 ms at main
/// against the 18.0 ceiling — and it is *not* contention: reproduced three
/// times at 1-min loadavg 5.0-5.4 while both control metrics sat at 1.08x and
/// 0.92x of their own bases. A commit-by-commit bisect over the whole range
/// since this ceiling was set puts the entire step on **one** commit:
///
/// ```text
///   109f8422  the commit that SET this ceiling      indoor  8.85 / 8.80 ms
///   211e99ca  a built cell has a fabric                     8.92
///   bcf4a596  light is a derived view over shadowcaster     8.61
///   7f198ea5  a hearth is at a wall                         8.57 / 8.67
///   c25bb1d2  PaletteEntry.color fills                     17.71 / 17.30  <--
///   f962ee95  the lens                                     17.17
///   155b0901  main                                         18.72
/// ```
///
/// `c25bb1d2` widened palette interning from `CellKind` to
/// `(CellKind, Option<[u8; 3]>)`, replacing one shared entry per wall material
/// with a per-cell `Observer::sense` + `to_srgb` summed over `light_field`'s
/// illuminants — a per-chamber lookup became per-cell work on every indoor
/// snapshot. **The four sibling Lantern commits moved it by ZERO**, and both
/// controls stayed flat across every point, so this is localised code cost.
///
/// **This is the price of per-cell colour, not a defect.** The feature does
/// exactly what it says. Budgeted at ~2x the new measurement in the same
/// method as every other ceiling here: `2 * 18.720 = 37.44` -> 40.0.
///
/// The 8.85 -> 17.71 step is left legible above rather than smoothed away: a
/// future reader must be able to see that this ceiling doubled and why, which
/// is the entire reason the ratchet rule exists.
const INDOOR_SNAPSHOT_BUDGET_MS: f64 = 40.0;

/// Ceiling for one walk-band snapshot's serialized bytes. The spec measured
/// `scene/surrounds/v1` at 7,049 bytes at radius 4; this bounds the whole
/// snapshot, which is larger.
///
/// Basis: Task 4's reading, 2026-08-06 — 11,582 bytes, unchanged across
/// three dev-profile runs of this test and three release-profile runs of
/// the bench alike (deterministic on seed 42; profile and repetition affect
/// only wall time, never content). Budgeted at ~2x, rounded up.
///
/// **Corrected and re-based (The Sighting, Task 6), 2026-08-06.** The basis
/// above was already stale before this task: `d36a6a79` ("carry the v2
/// chart into The Panes' spatial channel", same day) grew the walk band to
/// 12,273 bytes, which is what the committed fixture
/// (`windows/vessel/tests/fixtures/snapshot-seed-42-walk.json`) and this
/// task's own fresh reads both give — three dev-profile runs of this test
/// (12273, 12273, 12273 B) and three release-profile runs of the bench
/// (12273, 12273, 12273 B) alike, so the walk band itself is untouched by
/// Tasks 2-5's chamber-only work (anchors/shadowcast/marks apply only
/// indoors). The old ceiling (23200) still happens to pass, but at only
/// `23200 / 12273 = 1.89x` headroom, not the stated ~2x — an unnoticed
/// consequence of the same staleness, not a deliberate tightening. Re-based
/// on the confirmed 12,273-byte figure: `2 * 12273 = 24546`, rounded up.
/// This is a ceiling **raise** (23200 -> 24600), recorded here as the
/// required deliberate act: the reason is restoring the intended ~2x
/// margin against a basis that was already wrong, not a growth this
/// campaign's chamber work caused in the walk band.
///
/// **Basis re-recorded, ceiling unchanged (The Grain), 2026-08-10.** The walk
/// band is now **16,664 bytes** and the ceiling stays at 24600, so headroom is
/// `24600 / 16664 = 1.48x` — no longer the ~2x the paragraph above restored.
/// The ceiling is deliberately NOT raised: 1.48x is ample, and raising a
/// ceiling to preserve a round multiple would spend the ratchet's whole point.
/// What is recorded instead is where the bytes went, measured per commit
/// against the committed fixture rather than estimated:
///
/// ```text
/// 12,273  the basis the paragraph above recorded (The Sighting, 2026-08-06)
/// 13,598  this branch's own starting point (00f90ec6) — ALREADY +1,325 STALE
/// 16,536  +2,938  `micro`, four quantized axes on each of 31 cells (8ce9c887)
/// 16,638  +  102  the `resolution` disclosure block, once per document (cc171e04)
/// 16,664  +   26  `"cave": null` on the embedded locale (f800e3b5)
/// ```
///
/// Two things worth a future reader's attention. **The basis was stale before
/// this campaign touched it** — 12,273 to 13,598 happened in between and was
/// never re-recorded, which is the same silent drift The Sighting's paragraph
/// above was written to catch, recurring one campaign later. And **`micro`
/// accounts for 96% of this branch's growth** because it is per-cell: a
/// document-level block costs ~100 bytes and a per-cell block costs ~3,000 at
/// radius 4, scaling with the neighbourhood. A future per-cell field should be
/// priced that way before it is added, not after.
///
/// **Re-measured after absorbing main, ceiling unchanged (The Grain, 2026-08-11).**
/// The walk band is now **16,667 bytes** and the ceiling stays at 24600, so
/// headroom is `24600 / 16667 = 1.476x`. The `+3` is **not** this campaign's:
/// absorbing The Radiation moved main's own walk band from 13,598 to 13,601
/// bytes over the same merge base, and the 16,664 figure above rode on top of
/// the pre-absorption 13,598. The attribution is therefore additive and
/// unchanged — `micro`, `resolution` and `cave` still account for +3,066
/// between them, and the absorption contributes the remaining +3:
///
/// ```text
/// 13,598  the merge base (00f90ec6), the basis the table above starts from
/// 13,601  +    3  origin/main after The Radiation, same base — no new fields
/// 16,664         this branch's pre-absorption reading (table above)
/// 16,667  +    3  the merged tree: the SAME +3, carried through
/// ```
///
/// The three bytes are **content, not structure**. The Radiation derived the
/// affinity ladder's level (decision 0120),
/// which moved settlement placement, so the walk band's entity IDs and labels
/// changed — the first divergence is a neighbouring settlement renamed
/// `Nenagabo` -> `Geoboge` with a correspondingly shorter entity ID. No key was
/// added on either side. A future reader should read this as the fixture
/// tracking a moved world, not as a schema growing: a byte delta this small
/// with no new field is the signature of a placement change upstream.
///
/// **Re-recorded, ceiling unchanged (The Ford, stage 2, 2026-08-11).** The
/// walk band is now **17,209 bytes** and the ceiling stays at 24600, so
/// headroom is `24600 / 17209 = 1.43x` — ample, and again deliberately not
/// spent on restoring a round multiple. The whole `+542` landed in a single
/// commit, and it is **per embedded room**:
///
/// ```text
/// 16,667  the merged tree the table above ends on (fefdde7c)
/// 17,209  +  542  `channel_distance`, `channel_bands` and the per-room
///                 `resolution` disclosure, on each of the TWO locale
///                 documents this band embeds (cb452ef9) — 271 B per room
/// ```
///
/// **271 B per embedded room is the price to check the next locale field
/// against.** It is measured, not estimated: the same change moved
/// `session-seed-42.json` from 65,649 to 67,817 bytes (+3.3%, eight embedded
/// rooms), and 542 over this band's two rooms gives the same 271. The shape is
/// the mandated one — `windows/scene` emits its resolution block once per
/// document, `windows/locale` emits one per room — so a field added to
/// `Locale` is multiplied by however many rooms a document embeds, and that
/// multiplier is the thing to look up first. The paragraph above says to price
/// a per-cell field *before* adding it; this is that price for the per-room
/// case, recorded **here** rather than only in the campaign retrospective,
/// because the retrospective is not where a reader of `WALK_BYTES_BUDGET`
/// looks.
///
/// **And the drift recurred a third time.** The paragraph above closes by
/// observing that the basis was stale before The Grain touched it — "the same
/// silent drift The Sighting's paragraph above was written to catch, recurring
/// one campaign later". The Ford's stage 2 moved the band by 542 bytes and did
/// not touch this file either; the pre-merge review is what caught it. Three
/// campaigns, three misses. Nothing asserts this ledger is current — the
/// budget assertion below only asserts the band is under the ceiling, which it
/// comfortably is whether or not anyone has written down where the bytes went
/// — so a reader should treat a row here as evidence that somebody looked,
/// not as evidence that nothing has happened since.
const WALK_BYTES_BUDGET: usize = 24600;

/// The measured basis for `START_BUDGET_MS`: 3442.192 ms, the slowest of
/// three runs, host `MacBookPro`, dev profile, 2026-08-06 — the ORIGINAL
/// basis this ceiling was set from. The Sighting's Task 6 re-measure came in
/// lower (2803.291 ms) and explicitly did not rebase the ceiling — see
/// `START_BUDGET_MS`'s own doc for why. A CONSTANT rather than prose so the
/// failure path can compute a ratio, the same pattern as
/// `scene_cost.rs::GENESIS_BASIS_MS`.
const START_BASIS_MS: f64 = 3442.192;
/// The measured basis for `TURN_BUDGET_MS`: 3.906 ms, slowest of three runs,
/// same box/date/profile as `START_BASIS_MS`. The Sighting's Task 6
/// re-measure (3.939 ms) was read as "essentially flat" and left the ceiling
/// unchanged — see `TURN_BUDGET_MS`'s own doc.
const TURN_BASIS_MS: f64 = 3.906;
/// The measured basis for `INDOOR_SNAPSHOT_BUDGET_MS`.
///
/// **Moved by The Assize, 8.910 -> 18.720**, in the same reviewed act that
/// raised the ceiling above it. The old figure was 8.910 ms (The Sighting,
/// 2026-08-06, slowest of three runs); it is superseded because `c25bb1d2`
/// made per-cell colour a real cost, bisected and evidenced at
/// `INDOOR_SNAPSHOT_BUDGET_MS`'s own doc.
///
/// **The basis MUST move with the ceiling.** Leaving it at 8.910 while the
/// ceiling went to 40.0 would make this metric read 2.09x on every future
/// green run, and the verdict logic below would then report a control as
/// having "moved" forever — an alarm that fires always is an alarm nobody
/// reads.
const INDOOR_SNAPSHOT_BASIS_MS: f64 = 18.720;

/// How far a CONTROL metric may drift from its basis before the run stops
/// counting as "the controls held". Same value and reasoning as
/// `cli/tests/scene_cost.rs::CONTROL_TOLERANCE` — see that constant's doc
/// for the two data points it sits between. It gates a DIAGNOSTIC MESSAGE,
/// never a pass/fail — no assertion reads it.
const CONTROL_TOLERANCE: f64 = 1.5;

/// The machine every basis constant above was measured on, as
/// [`hornvale_lab::canonical_host`] would name it — **not** `hostname -s`,
/// which is not a stable machine identity (this repo's own Mac has answered
/// both `MacBookPro` and `Greyjoy`; see `docs/timings.md`'s host column).
///
/// This file's own doc comments name the box repeatedly: `START_BASIS_MS`
/// and `TURN_BASIS_MS` say "host `MacBookPro`"; `INDOOR_SNAPSHOT_BUDGET_MS`'s
/// basis doc says "this box (`MacBookPro`)" — all "this box, dev profile"
/// language, never `lefford`. Decision 0090 records the Mac as `Darwin
/// arm64` on 10 cores
/// (`docs/decisions/0090-the-canonical-host-is-audited-not-assumed.md:36`) —
/// `arm64` there is `uname -m`'s name for it. `canonical_host` builds this
/// id from `std::env::consts::ARCH`, the Rust compile-target name, which
/// reports 64-bit ARM as `aarch64` regardless of OS — confirmed empirically
/// on this box during The Assize — hence `aarch64-10`, not `arm64-10`.
///
/// A ratio computed against this basis from any OTHER host measures the
/// machines, not the code (The Assize) — see the verdict logic in the test
/// below, which declines to compute one off this host.
/// type-audit: bare-ok(identifier-text)
const BASIS_HOST: &str = "aarch64-10";

#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to make gate-campaign (decision 0132)"]
fn a_possessed_turn_stays_within_its_ceilings() {
    let world = build_world(
        Seed(42),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds");

    let mut starts = Vec::new();
    let mut turns = Vec::new();
    // The band split (fix round 1): `snapshot()+json` timed SEPARATELY from
    // `handle`, then classified by the band the resulting snapshot itself
    // reports (`snap.spatial`) — not by verb class, which review found
    // straddles the real axis (`out` is `Moving` and cheap; `map`/`look`
    // right after `enter` are `Neither` and expensive, because they are
    // indoors). Only the indoor bucket is gated; the outdoor one is not
    // collected since nothing here budgets it.
    let mut indoor_snaps = Vec::new();

    for _ in 0..RUNS {
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t0 = Instant::now();
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
        #[allow(clippy::disallowed_types)] // benchmark harness
        let start_ms = t0.elapsed().as_secs_f64() * 1000.0;
        starts.push(start_ms);

        for line in SEQUENCE {
            #[allow(clippy::disallowed_types)] // benchmark harness
            let t1 = Instant::now();
            let _ = session.handle(line);
            #[allow(clippy::disallowed_types)] // benchmark harness
            let handle_ms = t1.elapsed().as_secs_f64() * 1000.0;

            #[allow(clippy::disallowed_types)] // benchmark harness
            let t2 = Instant::now();
            let snap = session.snapshot().expect("a live session snapshots");
            // Serialize too: the emit is part of the per-turn cost the
            // client actually pays, and measuring construction alone would
            // under-report it.
            let json = hornvale_vessel::snapshot_json(&snap);
            std::hint::black_box(&json);
            #[allow(clippy::disallowed_types)] // benchmark harness
            let snap_ms = t2.elapsed().as_secs_f64() * 1000.0;

            turns.push(handle_ms + snap_ms);
            if matches!(snap.spatial, SpatialChannel::Chamber { .. }) {
                indoor_snaps.push(snap_ms);
            }
        }
    }

    let start_median = median(&mut starts);
    let turn_median = median(&mut turns);
    let indoor_snapshot_median = median(&mut indoor_snaps);

    // The byte figure the spec priced by radius, taken on a fresh session
    // outside the possession (the walk band, the larger of the two bands).
    let (mut session, _) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
    let walk_bytes = hornvale_vessel::snapshot_json(&session.snapshot().unwrap()).len();
    std::hint::black_box(session.handle("enter"));

    // Named so the verdict below can speak about them: `Session::start`
    // builds a world from scratch and is the only contention-sensitive
    // metric here; the two per-turn metrics run against a `World` already
    // built in memory. Same shape as `scene_cost.rs`'s `measured` array.
    let measured: [(&str, f64, f64, f64); 3] = [
        (
            "Session::start",
            start_median,
            START_BUDGET_MS,
            START_BASIS_MS,
        ),
        (
            "handle+snapshot+json",
            turn_median,
            TURN_BUDGET_MS,
            TURN_BASIS_MS,
        ),
        (
            "indoor snapshot+json",
            indoor_snapshot_median,
            INDOOR_SNAPSHOT_BUDGET_MS,
            INDOOR_SNAPSHOT_BASIS_MS,
        ),
    ];
    // A ratio against a basis measured on a DIFFERENT machine measures the
    // machines, not the code (The Assize) — see `BASIS_HOST`'s doc. Compute
    // this once and gate both the per-metric ratio column and the verdict on
    // it.
    let this_host = hornvale_lab::canonical_host();
    let bases_apply = this_host == BASIS_HOST;
    for (name, got, budget, basis) in measured {
        if bases_apply {
            println!(
                "{name:<20}{got:9.3} ms (budget {budget:>8}) {:5.2}x basis {basis}",
                got / basis
            );
        } else {
            println!("{name:<20}{got:9.3} ms (budget {budget:>8})");
        }
    }
    println!("walk snapshot bytes {walk_bytes:9} B (budget {WALK_BYTES_BUDGET})");

    if !bases_apply {
        println!(
            "VERDICT: not computed — bases were measured on {BASIS_HOST}, this is \
             {this_host}. A ratio across two machines measures the machines. Raw ms \
             above stand; the ratio column and the verdict do not."
        );
    } else {
        // THE DISCRIMINATOR, as arithmetic — see `scene_cost.rs`'s module
        // doc. `Session::start` is the contention-sensitive metric; the two
        // per-turn metrics are the CONTROL SET. Controls holding while start
        // inflates is the machine. Any control moving is the code.
        let controls_over: Vec<&str> = measured
            .iter()
            .skip(1)
            .filter(|(_, got, _, basis)| got / basis > CONTROL_TOLERANCE)
            .map(|(name, _, _, _)| *name)
            .collect();
        if controls_over.is_empty() {
            println!(
                "VERDICT: both controls within {CONTROL_TOLERANCE}x of basis. A Session::start \
                 breach here reads as CONTENTION, not a regression — re-run on a quiet box \
                 to confirm before touching any constant."
            );
        } else {
            println!(
                "VERDICT: {} control(s) moved: {controls_over:?}. This is NOT the contention \
                 signature — look at the code before blaming the box.",
                controls_over.len()
            );
        }
    }

    assert!(
        start_median < START_BUDGET_MS,
        "Session::start took {start_median:.3} ms, over the {START_BUDGET_MS} ms ceiling"
    );
    assert!(
        turn_median < TURN_BUDGET_MS,
        "one handle+snapshot+serialize took {turn_median:.3} ms, over the \
         {TURN_BUDGET_MS} ms ceiling"
    );
    assert!(
        indoor_snapshot_median < INDOOR_SNAPSHOT_BUDGET_MS,
        "an indoor snapshot()+json took {indoor_snapshot_median:.3} ms, over the \
         {INDOOR_SNAPSHOT_BUDGET_MS} ms ceiling"
    );
    assert!(
        walk_bytes < WALK_BYTES_BUDGET,
        "a walk-band snapshot serialized to {walk_bytes} bytes, over the \
         {WALK_BYTES_BUDGET} byte ceiling"
    );
}

/// The median of `xs`, which this sorts in place. `total_cmp` rather than
/// `partial_cmp().unwrap()`: the workspace sorts floats deterministically
/// and never panics on a NaN it did not expect.
fn median(xs: &mut [f64]) -> f64 {
    xs.sort_by(|a, b| a.total_cmp(b));
    xs[xs.len() / 2]
}
