# The Waymark Implementation Plan

> **COMPLETE (2026-08-01).** All tasks executed (+ Task 6b under ledger
> #8; the RefCell ruling ledger #9). health 60.7→47.5 s, heat 48.3→4.7 s,
> sky 32.9→24.5 s, band 53.8→21.5 s; water searches 1632→130; 1 M-subject
> bench @ ~13 µs cached; two honest nulls (reverse-field equivalence,
> tick-walk hypothesis). See chronicle + retro.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The sim loop stops re-deriving its geometry — the ratified sweep
tier-split, session-lived geometry memos, a cross-tick per-entity plan
cache (zero searches for stationary unchanged-belief creatures), an
equivalence-gated shared reverse field, a solver seam with two live
backends, and a massive-scale nav bench (10…10⁶ subjects) — under the
byte-identity bar, ≥15 % suite reduction from 7 696.5 s.

**Architecture:** Spec `docs/superpowers/specs/2026-07-31-the-waymark-design.md`
(G3-approved with two ratified revisions). Six stages; the evidence base is
the code map + FP/DWARF profiles cited in spec §1. Key anchors:
`decide_step`'s unconditional replan (windows/vessel/src/liveness.rs:3846),
`NavSpace` (liveness.rs:5183–5207), `plan_to_room` budget/Dijkstra-mode
(liveness.rs:3479, 5204–5206), `move_cost` (:5099), `PrimaryAfraidMemo`
threading (:937–985), `corner_weights` (kernel/src/room.rs:526),
`neighbors` (:490), `NearestCellIndex`/`scan_at`
(kernel/src/geosphere.rs:339–470, float-tie at :446 UNTOUCHABLE),
`astar` (kernel/src/astar.rs:13–40, frontier order IS the tie-break),
the `coords` bit-equality pin precedent (geosphere.rs:594), locale field
readers (windows/locale/src/lib.rs:357–515).

**Tech Stack:** Rust edition 2024; serde/serde_json/libm only; nextest.

## Global Constraints

- **Byte-identity of all sim outputs** (AGENT_AT facts, affect traces,
  health metrics, possession transcripts): every memo/cache changes when a
  computation runs, never what it returns. Staged references first
  (Task 1); `session_snapshot` is the NAMED byte-pin battery for every
  vessel-touching task (the-weir retro lesson) and runs in each such
  task's Step list.
- No `HashMap`/`HashSet`; no `RefCell`/globals/`OnceLock` for the new
  memos — caller-owned `&mut` threading (`PrimaryAfraidMemo` shape). The
  derivation lint (decision 0092) is live: new derivation call sites need
  scoped justified `#[allow]`s only at genuine construction sites.
- **The untouchables:** `astar`'s frontier order, `scan_at`'s exact-float
  tie-break, `move_cost`'s values, all stream/draw order. No new draws.
- lefford discipline: claim check before gates; absorb main between tasks;
  one test invocation per Bash call; NEVER set HV_CENSUS.
- Worktree `/home/nathan/Projects/hornvale/.claude/worktrees/the-waymark`
  (branch `the-waymark`), prewarmed.
- Commits `perf(the-waymark)`/`test(the-waymark)`/`docs(the-waymark)`,
  every commit compiles + passes touched tests; `cargo fmt` last.

---

### Task 1: Stage references and before-timings

Scratch dir `…/scratchpad/waymark-reference/` (same pattern as prior
campaigns): `world-42.json` (cmp guard), the possess transcript
(`printf 'look\nconsult\nwait\nwait\nlook\nquit\n' | hornvale possess
--seed 42`, double-run `cmp`'d), and before-timings run once each on a
quiet box: health null-control (`--exact`), `heat_hastens_thirst_end_to_end`,
`the_sky_follows_the_walker`, `sharing_never_increases_band_distress`,
and the full seed sweep (`the_null_control_holds_across_a_seed_sweep`) —
record all in `timings-before.txt` with exact commands. No commits.

### Task 2: Stage 1 — the sweep tier-split (ledger #1, ratified)

**Files:** `windows/lab/tests/health_calibration.rs`.
Split `the_null_control_holds_across_a_seed_sweep`: the gate keeps seed-42
(already separate as `the_null_control_reads_no_chronic_distress`) — the
sweep test itself becomes `#[ignore = "heavy: 4-seed breadth arm, ~350s —
runs in gate-full (decision 0093-adjacent tier split, the-waymark)"]` and
drops seed 42 from its list (it's the gate's arm) keeping 0/1/2/7 with ALL
non-vacuity guards verbatim. Verify: the ignored test still RUNS green
once via `cargo test -p hornvale-lab --test health_calibration
the_null_control_holds_across_a_seed_sweep -- --exact --ignored`
(foreground, timeout 3600000, ~280 s) — the heavy tier must not rot on
arrival. Check `cli/tests/heavy_tier.rs` for how the `heavy:` token is
enforced/enumerated and satisfy it. Commit.

### Task 3: Stage 2 — geometry memos

**Files:** `kernel/src/room.rs` (memoized variants), a small memo struct
(placement: kernel `RoomMeshMemo` owned by callers — decide after reading
the call graph; NOT global), `windows/locale/src/lib.rs` (field readers
thread it), `windows/vessel/src/liveness.rs` (NavSpace/neighbors path).
- `corner_weights` memo: `BTreeMap<RoomAddr, [(CellId, u64); 3]>`
  (verify the exact value type from `corner_weights`' signature first),
  fed by the existing fn; the five locale field readers share one memo per
  read scope; `scan_at` untouched internally.
- `neighbors` memo: `BTreeMap<RoomAddr, [RoomAddr; 3]>` (verify arity —
  cross-face cases), used from the A* successor path.
- Threading: `&mut` params on new `*_memo` variants; existing signatures
  stay (non-hot callers unchanged). Bit-equality pin test per memo (the
  `coords` pattern, geosphere.rs:594): memoized == recomputed over the
  rooms a real walk visits.
- Run: `session_snapshot` battery + health single + `make quick`. Commit.

### Task 4: Stage 3 — the plan cache and the home_nav seam

**Files:** `windows/vessel/src/liveness.rs`.
- Plan-time verifications FIRST (record in report): (a) what does
  `decide_step` consume from the :3846 plan — full path or
  `(distance, first_step)`? (b) is `believed_hazard` per-entity, and where
  are its mutation write points? (c) can it mutate within a tick?
- Implement: `home_nav(entity) → (distance, first_step)` seam; a
  cross-tick per-entity cache (lives with NPC sim state) storing the
  consumed feature, invalidated by pos-change and a per-entity avoid-epoch
  counter bumped at every believed-hazard mutation point found in (b).
  If (a) shows conditional consumption, compute lazily AND cache.
- Tests: (1) adversarial staleness — avoid set changes, plan must change;
  red-run-proven by temporarily disabling invalidation (both runs pasted).
  (2) search-count pins — instrument a deterministic counter (test-visible,
  e.g. a counter on the memo struct): stationary+unchanged ⇒ 0 after
  warm-up; moved ⇒ exactly 1; belief-change ⇒ exactly 1. (3)
  `session_snapshot` + `possession_moves` + `the_first_mark` batteries +
  health single (paste timing — expect the big drop here). Commit.

### Task 5: Stage 3b — the shared reverse field (equivalence-gated)

**Files:** `windows/vessel/src/liveness.rs` (+ kernel if the reverse
Dijkstra needs a home there).
- The property test FIRST: over a real world's walk-visited rooms, for
  empty-avoid entities, field-derived `(distance, first_step)` ==
  forward-search result, byte-for-byte (the tie-break equivalence is the
  whole question — construct the field's step-selection to mirror
  `astar`'s `(f, g, state)` order, then PROVE it).
- If the property holds: wire empty-avoid entities through the field
  (shared per home, invalidated when ANY serving entity's... no — the
  field serves only empty-avoid entities and depends only on world
  geometry + home, so it is session-lived and never invalidated; an entity
  gaining avoid entries falls off the field onto its own cache). If it
  fails: ship the test as `#[ignore]`d documentation of the failure mode,
  field disabled, per-entity cache carries (spec licenses this exit).
- Byte-identity: `session_snapshot`, transcripts, health pins again.
  Commit either way, report which exit taken.

### Task 6: Stage 4 — the solver seam

**Files:** `kernel/src/astar.rs` (trait extraction), vessel call sites.
A minimal solver trait over `SearchSpace` (name/shape at plan-time; the
existing `astar` fn becomes the trait's first impl with zero behavior
change — the fn stays as a thin delegator so existing callers don't
churn); the Stage-3b field is the second impl (or, if 3b shipped
disabled, the second impl is the reverse-Dijkstra field builder itself,
which exists regardless — it just isn't wired to home_nav). Docs + tags;
`make quick`; the kernel is touched: run the kernel + vessel + lab
batteries once each. Commit.

### Task 7: Stage 5 — the nav bench

**Files:** `windows/vessel/examples/nav_bench.rs` (the `profile_build`
precedent — an example, not a test; vessel because it exercises
NavSpace/home_nav; kernel stays bench-free).
Rungs 10/100/1k/10k/100k/1M synthetic subjects on a real seed-42 world's
mesh: spawn walkers with goals, tick the NAV SEAM ONLY (no drives/affect),
per backend; report searches, wall, per-subject marginal cost, peak RSS
(read /proc/self/status — std-only). Run once per rung on the quiet box,
foreground, timeout 3600000 (expect the top rungs to be SLOW and possibly
memory-bound — a rung that cannot complete is ITSELF the finding; record
where and why, never fake it). Results → `docs/timings.md` ledger rows +
the task report. No pass/fail. Commit the example + the ledger rows.

### Task 8: Verification + Stage 6 re-profile

Absorb main; byte-identity sweep vs Task 1 references (world-42 cmp,
possess diff); `make rebaseline` (expected drift: type-audit report iff
pub surface changed — everything else STOP); after-timings for the five
Task-1 tests (targets: health <45 s, heat <60 s, sky <25 s, band <70 s;
the sweep now `--ignored`-only, time it there); full `make gate` (claim
check first). THEN the Stage-6 re-profile: FP flamegraph of the health
battery + one vessel walk, plus the bench rungs — write the
packed-address verdict (spec-out as follow-up campaign, or drop) into the
task report with the evidence. Commit ledger rows.

### Task 9: Close

Absorb; type-audit currency; preflight; quiet-lefford `make ci` (§4.3:
≥15 % below 7 696.5 s, baseline re-records; the renamed/ignored sweep row
is a known baseline change — name it in the commit body); chronicle +
SUMMARY + retro (promote followups; carry the bench's rung table);
**registry rows** (per `book/src/frontier/CLAUDE.md` format, slug IDs):
the nav-searches-per-tick lab metric idea; the census-proxy breadth
metric idea (ledger #1's discard); the packed-RoomAddr follow-up if
Task 8 specced it. Freshness sweep; clean any scratch `target-*` from the
main checkout; G6 package; STOP for Nathan.
