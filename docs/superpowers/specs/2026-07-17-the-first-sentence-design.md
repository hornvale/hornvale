# C1 — The First Sentence: Design

**Status:** Draft for G3 review (2026-07-17) · **Author:** Claude (campaign-
autopilot) · **Decider:** Nathan · **Program:**
[The Self-Writing Book metaplan](2026-07-17-the-self-writing-book-program-metaplan-design.md)

> C1 is the first campaign of the Self-Writing Book program. Its job is to get
> **one true sentence out of the engine** — *"Elthandil is a planet."* — with
> zero human/LLM surface text, and to make The Book the primary artifact. It is
> the walking skeleton: kernel predicate → committed facts → a Common grammar →
> a drift-checked Book. Everything richer (more predicates, more tongues, the
> chorus) is a later campaign.

## 1. What C1 is

Today the world cannot say "Elthandil is a planet." The registered predicates are
`name`, `located-in`, and the religion set — there is **no classification
predicate**, the planet is **not a named entity**, and its planet-ness lives in
the derived `SkyReport`, not the ledger. That gap *is* PROC-15 working: the first
sentence is un-renderable because the world does not model it as facts.

C1 closes the gap the **ledger-purist** way (Nathan's ruling): The Book reads
**only committed Facts**, so C1 mints the missing facts. The planet becomes a
first-class entity that asserts, in the ledger, what it is and what it is called.

**The name is the endonym.** A world is named by its **dominant peopled race's
own word for "the ground / the land"** — never "the third planet from the sun,"
which is a late astronomical concept a pre-Copernican people does not hold. This
mirrors Earth exactly (*Earth* ← "the ground"; *Terra*, *Gaia*, *eretz*, *arḍ*
all = "land"; *Miðgarðr* = "the middle enclosure where people live"). So the
world-name is a **lexicon lookup**, not a new draw, and the classification "is a
planet" is the god's-eye (etic) Common voice C1 renders. The per-race emic
framing ("Elthandil is all that is") arrives with the chorus in C4.

## 2. Scope — the deliverables

1. **A kernel-core `is-a` predicate**, registered by `World::new` beside `name`
   (classification is universal; it does not belong to one domain).
2. **The planet as a first-class entity.** The composition root mints an entity
   for the central body and commits two facts: `is-a planet` (its classification,
   read from structure — no draw) and `name <endonym>` (from the dominant race's
   lexicon — no draw).
3. **The dominant-race selection.** A deterministic measurement: the **peopled**
   race (biosphere-only creatures have no lexicon and are excluded) with the
   greatest **`Σ(population × mass)`**, tie-broken by ascending `KindId`. Its
   lexicon word for the universal-stratum "ground/land" concept is the world's
   name.
4. **`windows/book`** — a new window that reads the ledger, selects `is-a`/`name`
   facts, builds a `ClauseSpec`, and calls the language renderer. It owns
   document assembly and ledger reading; it never realizes strings itself.
5. **The grammar in `domains/language`** — generalize `render_line`'s
   `LineContent` into a `ClauseSpec` (frame + participants + features:
   definiteness, number, polarity), and add a Common realizer for the `Classify`
   frame: copula agreement, indefinite determiner with the a/an phonological
   rule, singular/plural. This is the shipped seam reoccupied by a real grammar.
6. **`hornvale book`** — the CLI command. The Book is **N volumes (3 to start,
   seeds 1, 2, 3)**, one per world, each a committed, drift-checked artifact.
7. **The restructure** — `SUMMARY.md` gains The Book as the primary part; the
   Frontier and Chronicle move under Appendices. Their drift-checks and
   registry-ID scoping key off directory paths, not SUMMARY position, so the move
   is safe.
8. **PROC-15 coverage skeleton** — `hornvale book` reports the predicates present
   in the ledger that lack a construction or a Common lexeme: the modeling
   to-do list.

## 3. Data flow

```
ledger fact:  (planet_entity, is-a, planet)   +  (planet_entity, name, "Elthandil")
      |
      v  windows/book: select classification facts, resolve the subject's name
ClauseSpec { frame: Classify, subject: Named("Elthandil"),
             complement: Class(planet), number: Sg, definiteness: Indef }
      |
      v  domains/language: Common realizer (ClauseSpec -> String)
"Elthandil is a planet."
      |
      v  windows/book: one volume per seed; N volumes = The Book
hornvale book  ->  committed, drift-checked artifact
```

## 4. Determinism & contracts

- **No new seeded draws.** The classification is read from structure (the
  `KindId`); the name is a lexicon lookup of existing seed-derived material. Per
  the metaplan's decision #1, this keeps the change clean — no new stream, no
  reordering of draws.
- **The ledger gains facts.** Committing `is-a` + `name` changes each world's
  ledger bytes. `world.json` is **not** a committed artifact (CI writes it to
  `/tmp`), and the almanac does not render these facts, so the committed
  **drift is the `concepts` reference dump** (the new predicate) — a **local
  regen**, not the AWS census. The census is expected clean (metrics query
  specific predicates, never `is-a`) and **must be verified at `make
  gate-full`** before merge. Any golden that pins a seed's world *hash* re-pins
  (byte-identity holds: same seed → same facts → same new value).
- **Byte-identity.** Same seed → byte-identical Book. Text is discrete, so there
  is no float/transcendental cross-platform risk.
- **The Book is a derived view / a sink.** Nothing reads rendered text back into
  the sim (the Lorenz guard-rail).
- **ADRs to land with C1:** metaplan decision #1 (grammar is build-state; The
  Book is a derived view) and #2 (The Book primary; Frontier/Chronicle →
  appendices), plus the new `is-a` classification predicate.

## 5. Testing

- **Grammar units** (in `domains/language`): the `Classify` realizer — copula
  agreement (`is`/`are`), a/an by following phoneme, singular vs generic plural
  ("As are Bs").
- **Dominant-race selection** is deterministic and total over the peopled set;
  a mutation (swap the max) flips the chosen namer — assert the mechanism, not
  just a value (per [[measure-dont-narrate-the-mechanism]]).
- **Determinism**: same seed twice → identical facts and identical Book bytes.
- **Drift-check**: the three committed volumes are regenerated in CI and fail on
  drift (like the almanac).
- **Coverage**: the PROC-15 report lists the un-constructed predicates.
- **Full gate**: `make gate-full` to confirm the census is unmoved before merge.

## 6. Firm choices (decisions)

1. **Ledger-purist**: The Book reads only committed Facts; C1 mints the
   classification and name facts (Nathan's G3 ruling).
2. **`is-a` is kernel-core**, registered by `World::new` beside `name`.
3. **Endonym naming**: the world's name is the dominant peopled race's lexicon
   word for the universal-stratum "ground/land" concept — reused, not a new
   concept, not a new draw.
4. **Dominance = `Σ(population × mass)`** over peopled races, tie-broken by
   ascending `KindId`; potency is foldable in later.
5. **The grammar extends `domains/language`** (generalized `render_line`);
   `windows/book` orchestrates and reads the ledger.
6. **N volumes**, seeds `1, 2, 3` for C1; curated diverse worlds (tidally-locked
   / earth-like / exotic, via `scout`) deferred.

## 7. Non-goals (C1)

- No per-language realization — Common only (C3).
- No epistemic chorus / filter stack, no etic margin beyond the plain god's-eye
  voice (C4).
- No sentence aggregation or referring-expression reduction — one fact, one
  sentence (C2).
- No predicates beyond `is-a`/`name` (C2), and only the **planet** is minted as
  an entity in C1; the astronomical siblings (moons, star) as entities are the
  immediate next increment.
- No curation of meaningfully-different worlds — arbitrary seeds `1, 2, 3`.

## 8. Flagged for G3

1. **Determinism blast radius:** confirmed a *local* `concepts`-dump regen, not
   the census — but the census-clean assumption is **verified at `make
   gate-full`**, not assumed. If a metric does move, that surfaces at the gate
   and we stop (census regen is a carve-out needing explicit authorization).
2. **The `is-a` predicate is a save-format contract** (a new predicate in the
   registry). Additive, no reordering — but it is a determinism-contract
   addition and leads this section by that rule.
3. **Astronomical-siblings boundary:** C1 mints only the planet. Confirm that
   the moons/star-as-entities can wait for the next increment rather than being
   in C1's floor.
4. **The universal-stratum "ground/land" concept** must already carry a lexeme
   for every peopled race (it should — it is universal-stratum — but C1 verifies
   it and, if a gap exists, that gap is itself a PROC-15 finding, not a silent
   fallback).
