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
**Status**: ✅ COMPLETE (commits `2bf6631` + `dab3d32`; main merged in at
`e8bf17d`, full gate 788 green) —
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

---
## CAMPAIGN EXPANDED (Nathan): homophony → a PHONOLOGY EPOCH (tone + tonogenesis)

The residual (100% mergers) and the exotic-species goal (yuan-ti/dragons) are
the SAME problem — too few phonetic DIMENSIONS. The model is segmental-only
(place/manner/voicing + 5 vowels); it has no suprasegmental tier (tone,
phonation). Restricted species get tiny inventories → homophony AND monotony.
The fix opens a second tier. Merger-repair via **tonogenesis** (a lost
segmental contrast reborn as pitch — how real tone languages arose) threads
the constitutional needle Options A/B could not: it is a REGULAR conditioned
change (regularity holds), proto unchanged (cognates hold), and yields tonal
languages as a byproduct. This is a new epoch beyond `root/v2`.

**Key design decisions (proposed; confirm before implementing):**
- **Where tone lives**: per-syllable, on the nucleus — a `Tone` enum (Level
  High/Mid/Low at minimum; Contour later). Atonal languages carry a single
  neutral tone (no contrast), so the field is universal but usually inert.
- **Tone capacity per species**: a new body-plan-derived scalar in the
  articulation vector (`tonality`), drawn to a tone count (1 = atonal, the
  humanoid default; 2–4 for serpentine/avian). Most species stay atonal.
- **Tonogenesis trigger**: when a *specific* merging sound-change (voicing
  loss, coda `FinalLoss`) collapses two forms in a tone-capable language, the
  lost feature becomes a tone — a regular conditioned split, not a lexical
  patch. Atonal species don't tonogenize (their residual is handled by the
  Stage-4 capacity floor + accepted-realistic tail).
- **Floor on CAPACITY, not segment count**: guarantee each species enough
  distinguishable syllables, reachable via segments OR tone — so yuan-ti meets
  the bar with pitch, keeping its serpentine (few-place) character.
- **Rendering ceiling is espeak, not IPA**: tone/length render well, phonation
  weakly — stay inside IPA-describable features (all of tone/phonation/clicks).

## Stage 3: Tone dimension in the phoneme model + tonogenesis repair
Add `Tone` to the segment/syllable model (per-nucleus); make it a regular
conditioned outcome of the merging rules for tone-capable languages; verify
`lexicon-regular` still holds (the split is regular) and cognates intact.
**Status**: ✅ COMPLETE (byte-identical for shipped worlds; NO re-baseline).
  ✅ §10 decisions confirmed with Nathan (after an ideonomy exploration of
     Q2/Q3): Q1 tone on `Segment::Vowel.tone`; Q2 voicing-conditioned split,
     both `ClusterSimplify`+`FinalLoss` feed one map, voiced→Low/voiceless→
     High, tone on the stranded nucleus (first vowel onset-drop, last coda-
     drop), nasal codas in the voiced/Low bucket, `Mid` banked; Q3 accept the
     atonal tail + a Stage-4 confusable-vs-free homophony metric; Q4 full
     epoch + re-baseline.
  ✅ `Tone {Neutral,High,Mid,Low}` enum (Neutral first = Ord minimum);
     `Segment::Vowel.tone` added as the LAST field (Ord tie-break locked by
     `tone_is_the_last_ord_key_of_a_vowel`). ~46 vowel literals threaded
     `Tone::Neutral`; `vowel_shift` carries tone through a quality shift;
     `feature_distance`/renderers ignore tone (Stage 5 renders marks).
  ✅ `RuleKind::Tonogenesis` + `apply_tonogenesis`: reads the pending merger's
     dropped-consonant voicing (threaded through `evolve`, NO stream draw),
     writes the tone on the stranded nucleus subject to the codomain
     constraint. 7 tests incl. reconstructability
     (`tonogenesis_preserves_a_contrast_two_roots_would_otherwise_lose_to_a_
     merger`) and atonal inertness. NOT added to `RULE_KINDS` yet — that (and
     the tone-inventory draw) lands in Stage 4's single re-baseline, so
     shipped cascades stay byte-identical here.
  ✅ Full gate green: workspace 796 tests / 0 failed, fmt, clippy -D warnings,
     type-audit check. No artifact drift (every golden/byte-identity test
     passed unchanged).

## Stage 4: Body-plan tone capacity + channel-capacity floor
`tonality` scalar in the articulation vector (yuan-ti/dragon/bird propensities
authored); a minimum-distinguishable-syllable floor met via segments OR tone;
re-measure homophony with the Stage-1 instrument.
**Status**: ✅ COMPLETE (the epoch re-baseline commit).
  ✅ `tonality: f64` added to `ArticulationVector` (7th dim) + `species-tonality`
     predicate/fact; shipped peoples authored 0.0 (atonal). Carried into
     language's `Envelope` via `envelope_of`.
  ✅ `canonical_segments` grows by the tone dimension (High/Low variants; Mid
     banked); `vowel_permitted` made tone-agnostic; `draw_phonology` draws a
     tone inventory over an isolated `phonology/tones` leg and admits toned
     vowels tone-aware — atonal draw byte-identical, tone leg isolated (tests).
  ✅ Capacity floor: `distinguishable_capacity` (pub) + `ensure_capacity_floor`
     widens a tone-capable species by PITCH to `CAPACITY_FLOOR=24`; atonal
     species never widened (their floor is `MIN_CONSONANTS`, their tail
     accepted, spec Q3). `Tonogenesis` appended to `RULE_KINDS` — the reseed.
  ✅ Lab instrument: `confusable-homophony-{4 daughters}` (same-domain subset of
     core, the Q3 measurement), `tone-count-{goblin,kobold}`,
     `distinguishable-capacity-{goblin,bugbear,kobold}`; classifier refactored
     to carry `Option<domain>`. Test-only `serpent_tonal_solo_roster` exercises
     tone-count=3 and the floor (spec §11) WITHOUT shipping a tonal people —
     shipped census stays the atonal baseline (tone-count 1). **Roster note for
     Nathan:** the tone-capable species is a Lab control (like `goblin-twin`),
     not added to the default world roster; §11's "test species in the roster"
     is met by that control, not the census.
  ✅ Epoch re-baseline (ADR 0016, in THIS commit): all artifacts regenerated
     (almanacs, dictionary, concepts/streams dumps, 2 CI censuses,
     branches-family, world.json fixture, scene/proto/lens goldens, type-audit
     report); calibration pins re-pinned to honest measured values — homophony
     means fell (goblin 4.043→3.618, bugbear 12.194→11.234; the reseed, not
     tone — shipped stay atonal), name-length/collision re-pinned. Divergence
     means/rates unchanged (aggregate-stable). Measured: confusable ≈ half of
     core (bugbear core 2.82 / confusable 1.34), so most of the atonal tail is
     FREE cross-domain homophony — Q3 accounting made concrete.

## Stage 5: Render + audio — tone across roman / IPA / espeak
Tone marks in all three views; espeak tone approximation; the audio pipeline.
**Status**: ✅ COMPLETE (drift-free — shipped peoples are atonal).
  ✅ `tone_mark_roman` (combining acute/macron/grave), `tone_mark_ipa` (Chao
     tone letters ˥˧˩), `tone_of` in `phoneme.rs`; `render_views` (the shared
     name+lexicon reducer) appends the mark after each vowel's glyph. Neutral
     renders bare, so atonal output is byte-identical (verified: regen of
     dictionary/almanac/phonology produced no diff).
  ✅ espeak stays tone-blind by design — lexical tone is espeak-weak, a known
     audio limit (spec §9); the segmental formulation stands. So the audio bank
     is unchanged: its sample names are bare stems (drawn from phonology, NOT
     evolved through the cascade), so neither the reseed nor tone touches them;
     `audio_artifacts` set-check green, `phonology.md` did not drift.
  ✅ Tests: tone marks render for High/Low, empty for Neutral; a toned word's
     roman/IPA differ from Neutral while its espeak matches (the documented
     limit). Full language suite 81 green.

## Stage 6: Close — book, decisions, retro, re-measure
Chronicle; re-score the relevant open-question; decisions (functional-load
definition, injective assignment, tonogenesis-as-merger-repair, capacity
floor); retrospective; final homophony + expressiveness re-measure.
**Status**: Not Started

*(Given the scope, this expansion likely warrants a short spec in
`docs/superpowers/specs/` before Stage 3 — the phoneme-model change touches
determinism, serialization, all render views, and the audio pipeline.)*
