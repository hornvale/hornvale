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
Assign proto-roots injectively **once at the family level**, in
coreness-then-concept order, growing a root by one syllable only when its
drawn form is already taken (git-short-hash style / minimal perfect hashing).
**Save-format contract**: reseeds every root → epoch bump (`root/v2`), artifact
regen. **Cognate safety**: assignment at family-proto only; daughters inherit.
**Status**: Not Started

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
