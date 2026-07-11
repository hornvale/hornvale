# Campaign: Lexicon homophony — measure, then displace

**Problem.** Distinct core concepts collide onto identical short word-forms
(seed 42 goblin: `hand` = `many` = `night` = *Noa*; seed 1: five concepts →
*Wa*). Diagnosed as a small effective word-space hit by the birthday paradox
with no disambiguation — **not** a keying bug and **not** a phoneme-inventory
shortage (inventories are rich; names use the full space).

**Diagnostic (4 seeds) suggested** ~68% draw / ~32% merger. **The 1000-seed
Stage-1 instrument OVERTURNED this** — merger share is far higher family-wide:
goblin 0.53, hobgoblin 0.50, bugbear **0.82**, kobold 0.42 (mean over worlds
with any collision). The cheap 4-seed sample happened to include seed 42 (0
mergers) and badly under-counted. **Consequence:** the post-evolution
re-merger check is NOT secondary — it is co-primary with proto-injective
assignment, and for bugbear it is the *dominant* source. Proto-injective
assignment alone would leave roughly half of core collisions standing.

**Core homophony is systemic**: worlds with ≥1 core-on-core collision —
goblin 86.7%, hobgoblin 85.6%, bugbear 96.1%, kobold 94.1%. Mean core pairs
per world 4.6–10.5. "Near-zero for core" is currently violated almost
everywhere; the campaign is warranted. (Mechanism unchanged: `ClusterSimplify`
dropping a 2-consonant onset's first segment; `nativize` folding.)

**Target (Nathan):** near-zero homophony for **core** vocabulary; incidental
homophony among exposure-gated periphery (biomes, deep colors) is fine.
Functional-load split is data-driven: core = `universal_stratum` ∪
`body_pack` ∪ `kin_pack`; periphery = ranked `color_pack` + biome Terrain.

## Stage 1: The instrument (measurement only — no generator change)
**Goal**: Lab metrics that measure the *right* thing, drift-checked.
**Success Criteria**: new metrics land in `registry()`; branches-family study
renders them; artifacts regenerate cleanly.
**Metrics**:
- `core-homophony-{goblin,hobgoblin,bugbear,kobold}` — count of collision
  pairs where *both* concepts are core (functional-load restricted). The
  number Nathan wants near zero.
- `homophony-merger-share-{…}` — fraction of colliding clusters that are
  mergers (≥2 distinct proto forms) vs pure draw-collisions. Decides whether
  proto-injective assignment alone suffices.
**Tests**: nonnegativity/range per daughter at seed 42; core-restricted count
≤ total `homophony-count`; a hand-built lexicon with a known draw-collision
and a known merger classifies each correctly.
**Status**: Complete — metrics landed, study wired, 1000-seed artifacts
regenerated, finding above recorded.

## Stage 2: Family-proto injective assignment (co-primary fix — draw side)
**Goal**: kill draw-collisions at the source, preserving cognates.
**Design LOCKED (Nathan, hash-table framing + 4 ideonomy runs):** deterministic
open-addressing with a **double-hash (re-draw) probe**, not linear probing.

Assignment algorithm (`assign_proto_roots`, new; replaces per-concept
`proto_root` inside `build_lexicon`'s pass 1):
1. **Scope = the full family concept UNIVERSE**, never a world's exposed
   subset (blind spot 1: else a word depends on which other concepts a world
   exposed → breaks pin-isolation + cognates). The universe is the union of
   the authored packs (`universal_stratum ∪ body_pack ∪ kin_pack ∪
   color_pack ∪ …`) plus every other family-rootable concept — a single
   canonical, ordered list.
2. **Order = core-first, then concept-id** (stable). Core (universal/body/kin)
   draws first and wins the short slots (Huffman/cuckoo priority → terse core
   + near-zero-core). Id tiebreak → **insertion-stable** (blind spot 2: a new
   concept appends without reshuffling committed words; rules out global
   minimal-perfect-hash recompute).
3. **Draw** each concept's proto-root at the family level, epoch `root/v2`
   (`language/<family>/lexicon/root/v2/<concept>`).
4. **On collision** (form already assigned): re-draw from a probe#-keyed
   sub-stream (`…/root/v2/<concept>/probe/<n>`) — double hashing, so colliders
   SCATTER (blind spot 3: linear "grow +1 syllable" makes Noa/Noka/Noke
   minimal-pair clusters). **Grow length by one syllable only after same-length
   probes exhaust** (a bounded probe budget → demand-driven length / table
   resize).
5. **Minimal-pair rejection**: a probe that is within edit-distance `<T` of an
   already-placed CORE root is rejected like a collision (blind spot 4:
   uniqueness ≠ distinguishability; the human "lookup" is phonetic).
**Save-format contract**: reseeds every root → epoch `root/v2`, artifact regen.
**Cognate safety**: assignment at family-proto only; daughters inherit + evolve.
**Success Criteria**: `core-homophony-*` at proto level = 0 across a seed
sweep; `monophyly`/`divergence-real`/pin-isolation tests still green;
byte-identical determinism holds.
**Status**: In Progress (wiring done, artifact regen running) —
  ✅ `assign_proto_roots` core (`2bf6631`): injective, core-first, double-hash
     probe, minimal-pair rejection, insertion-stable, 7 tests.
  ✅ Wired into `build_lexicon` pass 1 over `exposures.keys()`; pin-isolation
     contract tests rewritten to the `root/v2` epoch; monophyly/clean-outgroup
     lab metrics re-derive via the v2 assignment.
  ✅ 1000-seed re-measure: raw homophony −57–84% (goblin 15.06→4.04, bugbear
     28.61→12.19, kobold 25.18→3.94); clean-outgroup now perfect ([]);
     divergence ordering holds. Calibration pins re-baselined.
  ✅ All artifacts regenerated (world.json fixture, 2 CI censuses,
     branches-family, dictionary, almanacs, proto/phonology, type-audit
     report); name-based census pins re-baselined (name lengths rose slightly
     as colliders lengthen; name-collision rate *improved*); scene golden
     regenerated. Worktree type-audit build fixed via empty `[workspace]`.
     Full gate green (final confirmation running).

  **PIVOTAL FINDING** — `homophony-merger-share-*` now reads **1.00** for
  every daughter: the injective assignment eliminated *all* draw-collisions,
  so 100% of the residual is cascade/nativize mergers. Core homophony still
  affects bugbear in 78.9% of worlds (smallest inventory → most nativization
  folding), goblin/hobgoblin/kobold ~27–30%. **Stage 3 is now the ENTIRE
  remaining problem, not a co-primary half.**

## Stage 3: Post-evolution re-merger check (co-primary fix — merger side)
**Goal**: catch the ~50–82% the cascade / `nativize` re-merge per daughter
(Stage-1 finding: dominant for bugbear, ~half family-wide).
After evolving, detect core-on-core modern-form collisions a daughter
introduced and repair (append a distinguishing syllable / perturb), in
deterministic order. **Status**: Not Started

## Stage 4: Phonotactic floor (safety net) + tuning
**Goal**: bound the worst case seed-independently (no coda-less degenerate
proto). Re-measure with Stage 1 metrics; tune root-length only if the
instrument still shows core collisions. **Status**: Not Started

## Stage 5: Close — book, decision log, retrospective
Chronicle entry; re-score the relevant open-question; decision recording the
functional-load definition and the injective-assignment approach;
retrospective. **Status**: Not Started
