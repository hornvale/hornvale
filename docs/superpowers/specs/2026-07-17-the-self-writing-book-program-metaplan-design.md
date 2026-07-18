# The Self-Writing Book: A Program Metaplan — Design

**Status:** **Approved at G3 (2026-07-17)** · **Author:** Claude (campaign-
autopilot) · **Decider:** Nathan

> **G3 ruling (2026-07-17):** Nathan approved the program. The two load-bearing
> calls are ratified: **#2** — The Book becomes the primary published artifact
> and the frontier + chronicle move to appendices; **#1** — the grammar/schemas
> are build-state and The Book is a derived view (like the almanac), so a grammar
> change drift-checks The Book but never epochs a world save, with the standing
> principle that *new seeded draws are the only epoch-triggering part — minimize
> them* (prefer deriving linguistic parameters from existing world-state). These
> two should land as `docs/decisions/` ADR entries with C1. C1 (The First
> Sentence) now proceeds to its own spec → plan → execute.

> This is a **program metaplan**, not a campaign spec — the shape of
> `2026-07-14-ecs-program-metaplan-design.md`. It enumerates a sequence of
> shippable campaigns, fixes the commitments they share, and sets the standing
> gate. Each campaign then gets its own spec → plan → execute cycle off this
> document. Almost nothing here is greenfield: the program *unifies and grows*
> machinery Hornvale already runs (the `render_line` seam, the per-culture
> `Lexicon`, the Fact ledger, salience-ranked Phenomena, the β primitive, the
> historiography window).

## 1. What this is

The world writes The Book. Hornvale renders its own Fact graph into surface
prose through the shipped content→render seam and the per-culture lexicon, with
**zero human- or LLM-authored surface text**. The first sentence we want to read
is *"Elthandil is a planet."* — and today we cannot generate it, because
"Elthandil" is a generated name, "is a" is English, and no engine composes the
two. This program builds that engine, from the elements, starting from an empty
Book.

Two things make this the right shape:

- **It is not the unsolved NLP problem.** Open-domain generation over unbounded
  meaning is unsolved; this is **closed-world natural-language generation over a
  finite typed ontology** — the mature Reiter–Dale pipeline. `A.isA(B)` *is* a
  Fact (subject/predicate/object). Four of the pipeline's seven stages already
  exist in Hornvale (content selection = Phenomena salience; lexicalization =
  `Lexicon`; register = `VoiceParams`; partial referring expressions).
- **The Book is a completeness test for the world model** (PROC-15). A sentence
  the engine cannot render is a *modeling gap*, surfaced by CI — not a prose lag.
  The Book stops describing the world and becomes a **proof-obligation on it**.

**The north star (UNI-29):** the same engine that renders the world's
self-description ultimately renders the *program's* self-description. The
frontier, the chronicle, and the studies become derived views the engine writes,
considers, explores, and deletes according to its current knowledge and
abilities — "the program creates the worlds and manifests its understanding of
itself." That turn is **vision, not committed scope** in this metaplan; the
architecture below must not foreclose it, and C1 takes the first structural step
(The Book becomes primary; frontier + chronicle relocate to appendices).

## 2. Architecture

**One language-neutral contract, many realizers.** A `ClauseSpec` captures the
*semantic* shape of a proposition — a frame (e.g. `Classify` for `isA`),
participants bound to semantic roles, and features (definiteness, number, tense,
polarity, register, and — later — evidential source and ontological class). A
per-language grammar is a pure function `ClauseSpec → String`. This is the
shipped `render_line` seam generalized: `LineContent` (a bespoke clause spec for
tenets) becomes the general `ClauseSpec`, and the seam is reoccupied by a real
grammar exactly as its own doc-comment promised. Common (≈ limited English) is
one realizer; Goblinesque is a *different* `ClauseSpec → String` over the same
spec and the goblin lexicon. The `ClauseSpec` survived a stress-test against
zero-copula, article-less, classifier, VSO/SOV, and polysynthetic grammars
(LANG-35): word order, copula/article presence, and classifiers are all
realizer-side parameters; only polysynthesis stresses the renderer's assembly
(it must be morphological, not string concatenation).

**The chorus is a filter stack** (LANG-36). A cultural account is ground truth
composed through five per-culture filters, one per representation layer —
lexicon, knowledge, causal-model, ontology, valence — each a pure function of a
vector Hornvale already computes. The god's-eye gazetteer is the null-filter
(identity) case. The emic account is rendered; the **etic margin** is the sparse
diff `ground_truth \ emic_account`, rendered in a second typographic register
(the *Neverending Story* device). Every one of the five filters has a
grammatical home (LANG-40): knowledge→evidentiality, causal→voice/valency,
ontology→noun-class, valence→honorifics — so a drawn *grammaticalization-depth*
vector makes Common shallow and alien tongues deep.

**The Book is a derived view, CLI-emitted and drift-checked** (decision, §5).
`hornvale book --world <w>` renders The Book from a world; the committed output
is byte-identity drift-checked in CI exactly like the seed-42 almanacs. The
renderer is a **window** (`windows/book`) over the domains it presents, built
through worldgen — it obeys `kernel → domains → windows → cli`. The grammar,
schemas, and substrate are **build-state** (authored, like providers), never
world-state: changing the grammar changes the drift-checked Book artifact but
**never epochs a world save**. Models author (grammar/schemas), dice roll
(realization is deterministic and seeded).

## 3. The campaigns

Strangler-fig, floor-first. Each is its own spec → plan → execute off this
metaplan. Earlier campaigns ship a real, readable Book; later ones deepen it.

**C1 — The First Sentence (and the restructure).** `hornvale book` emits The
Book as a drift-checked artifact: `isA` facts rendered in Common via `ClauseSpec`
+ a minimal surface grammar (copula agreement, determiner + a/an, number) over
the existing lexicon and naming. Output: a genealogy/gazetteer of the world's
top-level entities — *"Elthandil is a planet."* Structural: **The Book becomes
the primary published artifact; the frontier and chronicle move to appendices**
in `SUMMARY.md`, preserving their drift-checks, the registry-ID scoping rule, and
the DoD chronicle requirement in their new location. Ships the PROC-15 coverage
skeleton (report un-renderable predicates). *(LANG-35 floor.)*

**C2 — The Predicates & the Grammar.** More predicates (`has`, `orbits`,
`lies-in`, `borders`…), sentence aggregation at depth 1 ("a planet with two
moons"), and Dale–Reiter referring expressions (name first, reduce when
unambiguous). The ~5 authored rule-families (entry-schemas per entity-type,
aggregation, referring expressions, the predicate→frame table, realization
tables). The single **terseness knob** (composition depth 1 everywhere) fixes the
dry Volo/Domesday register. *(LANG-35 build mechanics.)*

**C3 — The Tongues.** Per-language realizers: `ClauseSpec → String` per language,
routed through the per-culture `Lexicon` (Goblinesque, Koboldic, …); word order,
copula/article presence as drawn parameters; a *shallow* grammaticalization-depth
vector (no evidential/noun-class yet). The same fact in several tongues.
*(LANG-35 per-language + LANG-40 shallow.)*

**C4 — The Chorus.** The epistemic filter stack (lexicon/knowledge/ontology/
valence; causal deferred to C5). Emic account + sparse etic margin. The
**measurable dial** (LANG-41): distinctiveness × recoverability metrics in the
Lab, with the auto-calibration target *distortion ∝ 1/capability per domain* and
the inter-account-variance vacuity check. This is the campaign that turns
"what goblins understand" on. *(LANG-36 + LANG-41.)*

**C5 — The Explanations.** The causal-schema library (~12 embodied schemas on the
`packs.rs` model; derived-prior selection; application by binding slots to the
world's own generated furniture — pantheon, kin, substances) and the
metaphor-mapping layer (one shared source-domain substrate, two reads — schema
and mapping — then a β-weighted lexeme draw; dead metaphors are lexicon entries).
Composition depth 1. *(LANG-37 + LANG-38.)*

**C6 — The Doctrine.** Doctrinal/folk conflict: a second filter-stack on an
institution (SOC-1), the authority-serving distortion modelled as a **selection
bias, not deception**; the (folk-verifiability × authority-service) conflict map;
**reader-relative** rendering (the esoteric layer unlocks via the reader's
knowledge ledger). Multi-annotator margins (folk / priest / ground truth).
*(LANG-39.)*

**C7 — The Deep Grammar.** Evidential and noun-class morphology: the five filters
grammaticalized (LANG-40), grammaticalization-depth deepened for alien tongues,
the animacy coherence constraint (ontology-grammar ⟷ agentive schema), and a
morphological assembly step in the renderer. *(LANG-40 full.)*

**C8 — The Diachronic Book.** Historiography integration (LANG-42): re-render the
view at successive knowledge-states; the Kuhnian/reform cycle; non-monotonic
knowledge (closing margins as understanding grows, lost arts as it decays);
schism forks the chorus. The Book gains a time axis. *(LANG-42.)*

**Beyond the committed sequence — the north star (UNI-29):** the engine renders
the frontier, chronicle, and studies as generated self-descriptions. Named here
so C1–C8 keep the door open; its own metaplan when the engine is mature.

## 4. Shared commitments (constitutional)

- **Byte-identity.** Same seed + pins → byte-identical Book. Drift-checked like
  the almanac. Quantization at the emit boundary only.
- **Build-state vs world-state.** Grammar/schemas/substrate are authored
  build-state; the fact graph is world-state. Grammar changes drift-check the
  Book artifact and **never epoch a world save**.
- **No new dependencies.** The NLG engine is hand-rolled tables plus the kernel's
  `Seed`/`Stream` for β draws. std-only; serde-only allowlist intact.
- **Layering.** `windows/book` depends on the domains it presents, via worldgen;
  adding it edits no existing domain.
- **Models author, dice roll.** Grammar, schemas, and the source-domain substrate
  are committed and drift-checked; realization is deterministic and seeded.

## 5. Firm choices (decisions to ratify)

1. **The Book is CLI-emitted and drift-checked** (like the almanac), a derived
   view — not hand-authored, not an mdBook-native section.
2. **The Book becomes the primary published artifact; frontier + chronicle move
   to appendices**, preserving their drift-checks, registry-ID scoping, and DoD
   role in the new location.
3. **`ClauseSpec` is the language-neutral realization contract**; per-language
   grammars are `ClauseSpec → String` (the `render_line` seam generalized).
4. **The uncanny↔gibberish calibration is Lab-measurable** (distinctiveness ×
   recoverability), not a pure taste gate; the residual taste is *readability*
   only.
5. **Grammar is build-state**: a grammar change drift-checks the Book, never
   epochs a world save.
6. **Register is dry** (Volo/Domesday); composition depth 1 across metaphor,
   aggregation, and subordination.

## 6. Non-goals / carve-outs

- Not open-domain NLP or any runtime LLM. Closed-world NLG only.
- Not literary prose. A dry, enumerative gazetteer (Domesday, Pliny, the
  periploi) — not Umberto Eco.
- The self-describing-program turn (UNI-29) is the north star, **not** a
  committed campaign in this metaplan.
- Multimedia (image/music/space) is EXP-1's territory, not this program's.
- The diachronic ceiling (C8) is committed but last; the floor (C1) is static.

## 7. Standing gate

Each campaign runs its own spec → plan → execute off this document. The shared
gate: **byte-identity + drift-check of the Book artifact**, and a **PROC-15
render-coverage report** each campaign (a rising fraction of committed
fact-predicates that have a construction + a Common realization). C4 additionally
gates on the LANG-41 distinctiveness/recoverability metrics existing in the Lab.

## 8. Flagged for G3

1. **Determinism framing (load-bearing):** grammar/schemas are build-state, so a
   grammar change drift-checks the Book but does **not** epoch a world save. This
   is the constitutional call the whole program rests on — confirm it.
2. **Externally-visible restructure:** The Book becomes the published face of
   hornvale.github.io and the frontier + chronicle become appendices. This
   demotes DoD artifacts in the navigation — confirm the chronicle requirement
   and the frontier registry drift-check + ID-scoping still hold, relocated.
3. **Low-confidence assumption:** C4 depends on the LANG-41 dial being
   *measurable enough to calibrate*. If distinctiveness × recoverability doesn't
   separate good from bad accounts in practice, the chorus falls back to a taste
   gate. Flagged as the program's biggest bet.
4. **Naming:** the command (`hornvale book`?) and the artifact's in-world title.
5. **North-star boundary:** UNI-29 (the self-describing program) is confirmed as
   vision, not committed scope here.
