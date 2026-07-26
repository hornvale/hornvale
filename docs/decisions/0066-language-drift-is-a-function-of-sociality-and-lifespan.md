# 0066. Language drift-rate is a function of sociality and lifespan

**Status:** Accepted (2026-07-22) · **Decider:** Nathan

In the context of [The Solitary Tongue](../../book/src/chronicle/the-solitary-tongue.md)
— campaign 3 of the Dragons program (BIO-37, the language half) — we decided
that a language's **drift-rate is a function of its speakers**, not a global
constant.

**The model.** A language changes at **transmission events** (generational
hand-offs), so the drift a lineage accumulates over a fixed span of world-time
scales with its transmission count ≈ community size × turnover ≈ **sociality ×
(1 / lifespan)**. A settled people (many short-lived speakers) drifts fast and
splits into dialects; a long-lived solitary (a dragon: a community of one, alive
for centuries) accrues almost no transmissions and its tongue is **frozen** — a
conservative isolate whose modern forms ≈ its ancestral proto.

**The mechanism.**
- `hornvale_language::CascadeRegime { min, max }` — a cascade-length range,
  language-owned, with `const SETTLED = (2, 4)` (the historical
  `CASCADE_LEN_RANGE`). `draw_cascade_with_regime(seed, species, regime)` draws
  within it; `draw_cascade(seed, species)` delegates with `SETTLED`, so every
  pre-existing caller is **byte-identical** (the stream derivation is unchanged).
  `build_lexicon` takes the regime and threads it to the cascade draw.
- `domains/language` never reads `SocialForm` (layering — it is kernel-only).
  The composition root `windows/worldgen` owns `cascade_regime_of(&BiosphereTraits)`
  — `Settled → (2,4)`; long-lived `Solitary → (0,1)` frozen (via
  `hornvale_species::allometry::lifespan`, threshold **120 years**, bracketed by
  the peoples ~81yr and the dragons ~163yr, clear of the wild solitary beasts
  ~110yr); `Gregarious → (1,2)` (banked, no speaker). A worldgen
  `cascade_of(world, species)` computes and applies it; every dragon-reachable
  direct cascade caller routes through it (a `wc`-holding caller uses
  `cascade_regime_of(wc.biosphere.get(kind))` directly, since `cascade_of`
  re-assembles the canonical registries).
- The three chromatic dragons gained an authored Draconic `ArticulationVector`
  (harsh, hissing, sibilant) + a stopgap lexicon, and speak a frozen isolate off
  the shared `"draconic"` family proto.

**Determinism.** **Additive, not byte-identical, not an epoch.** The four peoples
are `Settled → SETTLED`, so their cascades/lexicons/names are byte-for-byte
unchanged (proven by default-regime property tests + a full seed-42 all-four
golden). The dragons are additive: Draconic sections appear only in the
speaker-catalog reference pages (dictionary +283/−0, phonology +95/−0) and 12
authored audio clips; no world (almanac/possession/census/gallery) changes,
because dragons are unplaced (their tongue is latent, read only for the
catalogue). The measured payoff: draconic inter-daughter divergence 0.25 <
goblinoid 0.32.

**Consequence — a note on latency.** Adding the dragons to the speaker
registries surfaced one dragon-reachable "a speaker perceives" assumption
(`exposure_of_impl`, fixed with a neutral-baseline `PerceptionVector` fallback,
byte-identical for the peoples). Several sibling `.expect("peopled pass over a
fauna kind")` sites remain but are **placement-gated → dragon-unreachable** while
the menagerie is unplaced; they are captured as a followup for the campaign that
places dragons.

**Scope.** This settles drift-as-f(sociality, lifespan) and the dragons' frozen
Draconic. **Deferred:** dragon perception; placing the menagerie; a distinct
Draconic grammar/semantics; per-chromatic tongue differentiation; the
`PsychVector` split.

**See also.** [The Solitary Tongue](../../book/src/chronicle/the-solitary-tongue.md)
chronicle and its [spec](../superpowers/specs/2026-07-22-the-solitary-tongue-design.md);
decisions 0064 (potency = CR/30) and 0065 (SocialForm + the solitary mind), the
program's earlier campaigns; `domains/language/src/etymology.rs` (`CascadeRegime`);
`windows/worldgen/src/lib.rs` (`cascade_regime_of`, `cascade_of`).
