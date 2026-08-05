# HANDOFF — The Tilth / The Tense, next session

Written 2026-08-05 at the end of a long session. **Every claim below is either
measured (command given) or explicitly marked as inferred.** The handoff this
session inherited was confidently wrong in three places and cost hours; the
convention exists so that does not repeat.

## Situation in one paragraph

Branch `campaign/the-tilth`, worktree `.claude/worktrees/the-tilth`, HEAD
`d14b01dd`, **34 commits ahead of main** (`f73059d5`), working tree clean, main
fully absorbed (`make preflight` is GO on ancestry and slug collisions). Two
campaigns' worth of work sits here: **The Tilth** (per-species capacity in the
bake) and **The Tense** (capacity gains a time axis; habitability stops being a
global constant). **The suite is RED: 52 failures / 2981 tests.** Do not merge
to main until §4 is worked through.

## 1. What is true of the code now

Measured with `cargo nextest run -p hornvale-worldgen --lib` and the probes named
below. All of this is committed.

- **Capacity is per-species and per-era.** `Bake` holds `caps_by_era[era][people]`
  indexed by `cur_graph` — the same index that selects the era's connection graph.
- **`Bake::factor` gates on ice alone**, and `era.ice` is identically empty on
  every production path, so it is currently inert. The thermal snowline
  (`FREEZE_C = -10 °C`) and the land test are both **gone** from the bake.
  Habitability is now a relation between a species and a cell, expressed as
  capacity.
- **Eviction triggers on pressure**, not on `eff == 0.0`. A people that cannot
  feed itself tries to leave before it dies; it starves only where there is
  nowhere to go.
- **`tolerance_liebig` is back to its PRE-stage-6 form** — temperature, moisture
  and insolation floored by `sovereignty_floor`, elevation passed `0.0`. Stages 6
  (floor elevation) and 7 (unfloor temperature) were both landed, measured, and
  **reverted** in `511d1fa9`. Its doc comment records both attempts with their
  numbers; read it before re-trying either.
- **`tolerance_tiered` exists and NOTHING CALLS IT.** It is §3.3's gate×modifier
  successor, landed in shadow mode.
- **`CAPACITY_V_MAX = 140.2` is correct and was re-derived**, not left stale.

## 2. The specs

- `docs/superpowers/specs/2026-08-05-the-tense-design.md` — the live one. H1–H7
  preregistered. §3.4 (within-cell temperature distribution, Jensen's inequality)
  is **first-class and unstarted**.
- `docs/superpowers/specs/2026-08-05-the-unfloored-axis-design.md` — The Tilth
  stage 6/7, now superseded by The Tense §3.3.
- `docs/superpowers/specs/2026-08-05-the-fallow-design.md` — **its premise is
  falsified.** H1's baseline of "1 layer" was never real; measured 16 at the time.
  Do not implement §3.1 without re-reading.

## 3. The instruments (all `#[ignore]`d; `-- --ignored --nocapture`)

| probe | answers | cost |
|---|---|---|
| `history_shape_probe.rs` | records/alive/sites/deepest per seed; cause mix + timeline | ~7 s |
| `niche_breadth_probe.rs` | per-species K>0/K>5, binding axis under 3 floor conventions, land °C | ~10 s |
| `tense_shadow.rs` | mask-vs-gate exclusion budget; moisture-as-gate | ~30 s |
| `capacity_cost_probe.rs` | per-component cost, era-replay hoisted vs naive | ~20 s |
| `tilth_probe.rs` | re-derives `K_m`, `V_max` against a **frozen** gauge | ~10 s |
| `era_substrate.rs` | NOT ignored — byte-identity + ocean-exclusion guards | ~20 s |

## 4. THE WORK, in order

### 4a. Two lib tests I broke and did not catch

```
cargo nextest run -p hornvale-worldgen --lib
```

- `history_bake::tests::a_flight_with_nowhere_to_go_is_a_death_not_a_departure`
- `history_bake::tests::a_roller_widens_its_search_past_an_unusable_neighbourhood`
  (`left: Some(1)  right: Some(3)`)

Both are plausibly *correct consequences* of the pressure-eviction change — a
community that can now flee is no longer "lost", and a roller that can settle
poorer ground stops at a nearer ring. **Decide that on the mechanism, not by
re-pinning.**

> **The trap that hid these:** `--test history_bake` is the *integration* binary.
> These live in the **lib** unit tests. I ran the former all session and claimed
> "all rule tests pass". Run `--lib` too, or `make gate`.

### 4b. Triage the other ~50

```
18 hornvale-worldgen · 12 hornvale-vessel · 11 hornvale-book · 7 hornvale-lab · 4 hornvale
```

Mostly **assertions about seed-42 world content**, not goldens — `the_additivity_law`,
the `exposure` family, lab metrics, vessel behaviour, `the_ladder_law`. The
rebaseline (`655b63ca`) already accepted the 34 real artifacts; these need
reading. Split each into *"the world legitimately moved, re-pin"* vs *"this is
telling us something"* — that split is what caught kobold being erased from
every world earlier in this campaign.

**Known pre-existing, not ours:** `demesne::k_biomass_gradient_grounding_is_unaffected_by_the_vector_supply`
fails identically at the branch point (verified by stashing).

### 4c. Nathan's open calls

- **`migration_fires_at_volume`** — seed 42 yields 4 events against a floor of 5.
  Migration now scales with how much a world's climate actually moves: seed 42's
  deep past is mild, seed 1234's is harsh (311 events). The floor was written when
  a binary mask displaced everyone on every era flip.
- **The owed census** — still a carve-out needing explicit authorization, on
  lefford. `make rebaseline` skips censuses by design; census fixtures currently
  sit at main's tip.
- **Kobold at ~18% of settlements** against a 10% cap (`branches_identity`) — lore
  says highland specialist, Nathan says "kobolds are like rats, pretty much
  anywhere". Unresolved.

### 4d. Campaign close (Definition of Done, CLAUDE.md)

Not started: chronicle entry, retrospective, Confidence-Gradient re-score, and
decision records. At least one is owed — **"habitability is a relation between a
species and a cell, not a global constant"** is a genuine ratified choice.
Mint decision numbers against `git log origin/main -- docs/decisions/`, never the
working tree (both branches see the same tip).

## 5. Findings that are settled — do not re-derive

- **`era.ice` is identically empty** on every production path. Any "make factor
  ice-only" proposal is a no-op; the previous handoff proposed it and it was wrong.
- **`devotion` is the response curve's PEAK, not its breadth.** Authoring
  "indifferent" as low devotion produces a flat cap. This is what made elevation
  bind on 100% of land for goblin/gnoll/human.
- **Mixing floored and unfloored axes under `min()` is unstable by construction** —
  a floored axis can never bind, so whichever is bare decides everything. Stage 6
  and 7 are the same bug on two different axes.
- **Moisture must NOT become a gate.** Measured: it takes human to 100% excluded.
- **Insolation is ~100% of the capacity pipeline's cost** (48-sample orbital
  integral) and is era-invariant, so it hoists. 40,962 cells carry only 10,301
  distinct latitudes — a 4× byte-identical memoisation is available and unspent.
- **Seed 1234 was a dead world for the whole campaign** (0 survivors) and now
  carries 36 alive, 70 sites, 16-deep columns, with recolonisation across
  centuries 13–19.
- **The bake HAS a recolonisation path** (daughter founding). An earlier
  diagnosis of mine said it did not; that was wrong. It simply had no survivors.

## 6. Process traps this session actually hit

1. **`--test X` ≠ the lib tests.** Cost: two undetected failures (§4a).
2. **Measuring the parts you listed and projecting as if they were all the
   parts.** Cost: a "1.1×" cost projection that measured 3.7×, because the
   per-species scoring loop was omitted.
3. **Unchecked multiplication.** "+70 minutes" for a census that is
   embarrassingly parallel (`runner.rs:210`) on a 40-core box, and timed on the
   wrong machine. Fleet: lefford 384 GB/40, MacBookPro 64 GB/~10, **ambrose
   38.7 GB/12** (this box).
4. **A gauge that re-measures its own anchor.** `tilth_probe` recomputed its
   target each run; it would have silently re-gauged `V_max` 140.2 → 118.9. Now
   frozen at 68.87 with the drift reported beside it.
5. **A test that HUNTS for a fixture instead of constructing one.** The vessel
   settlement-free test scouted seeds; The Tense made empty worlds rare, so it
   ground for 50 minutes. `BuildDepth::Terrain` gives it in 0.90 s.
6. **Branching off an unmerged branch.** A campaign was started off `main` on the
   belief its subject matter was there; caught only by an unresolved import.
7. **A single-seed comparison measures noise.** A `GENESIS_TOP_CELLS` sweep on
   seed 42 gave 433/483/558/**281** for 8/16/32/64 — non-monotonic, because
   changing the constant re-rolls the world. Use the five-seed spread.

## 7. Commands

```bash
make doctor                       # orientation
cargo nextest run -p hornvale-worldgen --lib          # §4a lives here
HV_TEST_OK=1 cargo nextest run --workspace --no-fail-fast   # the 52
make gate                         # fmt + clippy + type-audit + nextest + doctests
make preflight                    # currently GO on ancestry
```

Prefix every command with an explicit `cd` to the worktree — a stray `cd`
resets to the main checkout, and that has modified `main` by accident before.
