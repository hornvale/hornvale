# C3 — The Tongues: Design

**Status:** Draft for G3 review (2026-07-18) · **Author:** Claude
(campaign-autopilot) · **Decider:** Nathan · **Program:**
[The Self-Writing Book metaplan](2026-07-17-the-self-writing-book-program-metaplan-design.md)
(committed campaign C3; follows the inserted Echo).

> The same fact, in several tongues. Each placed people states who they
> are — "The Vavako are goblins." — in *their own language*: their word
> order, their copula (or none), their words from their own generated
> lexicon. One parametric realizer, drawn grammar parameters, render-only.
> What a tongue cannot say, it does not say — and the Book reports the
> gap.

## 1. Scope and the honesty constraint

The metaplan's C3: per-language realizers (`ClauseSpec → String` per
tongue through the per-culture `Lexicon`), word order and copula/article
presence as drawn parameters, a shallow grammaticalization posture — no
evidentials, no noun class (C7's). This campaign adds **no concepts, no
lexemes, no ledger facts**: the C2 lesson (a concept-add displaces
lexemes) is deliberately not re-run. The consequence is load-bearing:
**no culture holds a `planet` concept** (verified; the pre-Copernican
model C1's naming argument established), so no tongue can render the
planet sentence. That is not a limitation to engineer around — it is the
first honest sliver of C4's thesis (a culture describes the world only to
the depth it understands) arriving as a measured gap rather than a
filter.

What every people CAN say, from ledger facts alone: its self-statement.
The `instance-of` collective fact renders in-tongue because both content
words are always lexicalized — the autonym (the people's word for
`person`, C2) and the own-kind word (`{species}-kind`, Steeped for every
peopled culture).

## 2. Goal

Three deliverables:

1. **`TongueGrammar`** — a per-language surface-parameter struct, drawn
   from new permanent streams: constituent order (the six permutations of
   Subject/Verb/Complement, weighted by attested typology — SOV/SVO
   dominant), copula presence (omit vs. require — zero-copula is common
   cross-linguistically), article presence (floor: none unless drawn).
   Explicitly documented as the **floor slice of LANG-40's
   grammaticalization-depth vector** — C7 extends this struct, never
   replaces it.
2. **`realize_in`** — one parametric interpreter in `domains/language`:
   walks the same `Construction` part inventory the Echo built, but
   transforms the walk by the tongue's grammar (reorder the
   Subject/Copula/Complement constituents, drop the copula when absent,
   drop articles) and lexicalizes the complement slot through the
   speaker's `Lexicon` (`entry(concept).views.roman`); a `Gap` entry
   aborts the sentence with a recountable reason (never a partial lie).
   Render-only — the Echo's boundary stands: **only Common parses**.
3. **The Book's tongues section** — each volume gains, after the Common
   lines, one line per placed people: the self-statement in their own
   tongue (roman view), with a parenthetical Common gloss. Plus the
   per-tongue coverage report: PROC-15 extended per-language (which
   renderable facts each tongue could/could not realize, with gap
   reasons) — the planet line appears here as `gap: no word for planet`,
   per tongue.

Example shape (seed 1, illustrative — real forms come from the engine):

> The Vavako are goblins.
> — Vavako ⟨goblin-word⟩ ⟨copula-if-drawn⟩. *(in Goblinesque: "The Vavako
> are goblins.")*

## 3. Architecture

- **Draws:** new permanent stream labels
  (`language/<species>/grammar/constituent-order`,
  `…/grammar/copula`, `…/grammar/articles`), registered in
  `domains/language`'s `stream_labels()`. Drawn, not derived: word order
  is historically contingent — deriving it from psychology vectors would
  ship a fake correlation as science ("models author, dice roll" blesses
  the draw). Real typological correlations, if ever wanted, enter as
  C7 coherence links (the LANG-40 animacy precedent). Draws happen at
  build/render time and are **never committed** — grammar is build-state
  (decision 0058): no epoch, no ledger drift, no census exposure;
  existing streams (lexicon, naming) are untouched, so **every lexicon
  is byte-identical** — the C2 displacement class cannot occur.
- **Layering:** `TongueGrammar` + `realize_in` live in
  `domains/language` beside the Echo's inventory. `windows/book` chooses
  what to render per tongue (the self-statement construction), builds the
  gloss (it already has the Common line), and owns the per-tongue
  coverage report. `windows/worldgen` supplies the lexicon + grammar per
  placed people (the composition root draws the params).
- **The corpus-law boundary (explicit):** the Echo's standing round-trip
  gate covers **Common lines only**. Tongue lines are render-only and
  outside the parse corpus by design; the corpus test's scope is Common's
  construction inventory, unchanged. The tongues' own invariant is
  determinism (same seed → byte-identical tongue lines) plus the
  gap-honesty rule (a sentence renders fully or not at all).

## 4. Error handling

Lexicalization hits a `Gap` → the whole sentence gaps with the entry's
recountable reason, surfaced in the per-tongue coverage report; never a
mixed-language or partial sentence. An unregistered concept id passed to
`realize_in` is a caller bug (windows/book only requests concepts it
verified) and fails loudly.

## 5. Testing

- **Determinism:** same seed → byte-identical tongue lines and coverage
  report (the gallery drift-check inherits this).
- **Parameter space:** unit tests per grammar transform — each
  constituent order permutes correctly; zero-copula drops the copula and
  its spacing; article-less drops articles (exact-string, small).
- **The self-statement law:** for seeds 1/2/3, every placed people's
  self-statement renders in its own tongue (no gaps — autonym + own-kind
  are structurally Steeped), exact-string pinned for one seed.
- **The gap law:** the planet sentence, attempted per tongue, gaps with
  `no word for planet` for every tongue on every seed (asserts the
  honesty constraint holds and the report carries it).
- **Byte-identity of everything else:** Common lines, lexicons, world
  fixtures unchanged (zero drift outside the-book.md gallery + reference
  dumps that enumerate streams).

## 6. Determinism and blast radius

New stream labels are additive save-format surface (registered constants,
published in the stream manifest — the manifest reference page will
drift; that regeneration is expected). No ledger change, no epoch, no
census exposure, lexicons byte-identical. The Book gallery gains the
tongues sections (drifting commit). Census: untouched.

## 7. Non-goals

- No parsing of tongues, ever (the Echo boundary).
- No evidentials, noun class, honorifics, or morphological assembly (C7).
- No knowledge/epistemic filtering beyond the structural lexicon gap
  (C4's chorus); no causal schemas (C5).
- No new concepts or lexemes; no planet word.
- No IPA in the Book floor (the dictionary reference carries IPA).
- No translation mechanics (the gloss is authored-side; knowledge-gated
  translation is LANG-3's sequel territory).

## 8. Flagged for G3

1. **New permanent stream labels** (save-format surface, additive):
   `language/<species>/grammar/{constituent-order,copula,articles}` —
   the once-declared-forever contract applies; confirm the naming.
2. **Drawn, not derived** (ledger #1): word order/copula/articles are
   seeded contingency, not derived from culture vectors — confirm the
   typological-honesty argument (the alternative ships astrology).
3. **The gap IS the deliverable's edge:** tongues cannot say "planet,"
   and the Book will visibly report that per tongue. If you'd rather the
   floor read as complete (suppress the gap report until C4), say so —
   the honest-gap presentation is my recommendation.

## 9. Registry / docs consequences

LANG-35 Where gains this campaign at close; LANG-40 flips `raw` →
`spec'd (floor slice: The Tongues)` when this ships (the
grammaticalization-depth vector's first real field). Chronicle slug:
`the-tongues`.
