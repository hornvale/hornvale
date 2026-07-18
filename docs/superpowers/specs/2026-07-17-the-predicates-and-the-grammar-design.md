# C2 — The Predicates & the Grammar: Design

**Status:** Draft for G3 review (2026-07-17) · **Author:** Claude (campaign-
autopilot) · **Decider:** Nathan · **Program:**
[The Self-Writing Book metaplan](2026-07-17-the-self-writing-book-program-metaplan-design.md)

> C2 is the second campaign of the Self-Writing Book program. C1 got one true
> sentence out of the engine ("Vebe is a planet."). C2 makes the sentence *say
> more* — it renders the planet's own properties, fuses them into one dry
> sentence, refers back to the subject without repeating its name — and it
> proves the dual classification model by rendering the world's peoples through
> `instance-of`. It builds only on shipped machinery (the `ClauseSpec`/Common
> realizer, `windows/book`, the ledger, the per-culture lexicon).

## 1. What C2 is

Three things, in one campaign.

**A correctness fix C1 revealed.** C1 minted a *separate* planet entity that
carries only `is-a` + `name`, while every planetary fact — `moon-count`,
`star-class`, `day-length-std`, `ocean-fraction`, `plate-count`, and the rest —
lives on the world-root entity (the first-minted entity, holder of the sky and
terrain facts). So the "planet" the Book names has no properties to describe.
C2 fixes the identity: `is-a planet` and `name` go on the **world-root entity
itself** (the fact-holder), and the separate C1 mint is retired. The first
sentence is unchanged; the entity it is about is now the one that can be
described.

**The grammar the metaplan named.** With the planet identity fixed, C2 renders a
handful of the planet's own facts as clause fragments, fuses them into one dry
sentence (aggregation), and refers back to the subject with a pronoun instead of
its name (referring expressions):

> Vebe is a planet with two moons, orbiting a yellow-white dwarf; its day lasts
> about 1.5 standard days.

**The classification proof (`instance-of`, species).** C2 wires the first
`instance-of` facts into genesis and teaches the Book to read them, validating
the dual model ratified this campaign. A collective entity is minted per placed
peopled species, named by the people's **autonym** (their lexicon word for
"person/people" — the C1 endonym move applied to a people), and classified
`instance-of` its species kind:

> The Grobnar are goblins.

## 2. The classification split (decision 0061)

C2 ratifies the coexistence Nathan chose when ECS c5's `instance-of` landed
beside C1's `is-a`:

- **`is-a`** — functional, immutable **concept-class** (a planet is always a
  planet; a star, a star). Contradiction-checked. Used for celestial classes.
- **`instance-of`** — non-functional, mutable **roster-kind** (an entity's kind
  can change: awakened beast → lich). Latest-wins. Used for roster entities
  (species now; deities, settlements later).

"The goblins are a people" is immutable, so *were* it rendered it would be
`is-a`; C2 does not render it. The `instance-of` proof renders a people **as a
roster kind** ("The Grobnar are goblins"), which is exactly what `instance-of`
means, so the campaign exercises the predicate it is proving.

## 3. Components

- **`windows/worldgen` — genesis (modify):** move the planet `is-a`/`name`
  commit onto the world-root entity; drop the separate mint. Add a stage that,
  for each *placed* peopled species, mints a collective entity, commits
  `name = ⟨autonym⟩` (the lexicon's word for the universal-stratum "person"
  concept, via `lexicon_of`; `None` → unnamed, a coverage gap) and
  `instance-of = ⟨species KindId⟩` (via the existing `mint_instance_of_kind`).
- **`domains/language` — grammar (modify `clause.rs`):** extend the `Classify`
  frame with optional trailing **modifier phrases** (`Vec<String>`), realized as
  a dry comma/`with`-joined tail; add a `Subject` that can be a name or a pronoun
  (`it`/`its`); add small primitives — cardinal number-to-words (`2 → "two"`) and
  a quantity formatter (`1.5507 → "about 1.5"`).
- **`windows/book` — document planning (modify):** for each subject, collect its
  ledger facts, map each renderable predicate to a fragment via a **construction
  table** (`predicate → fragment builder`), assemble the head clause + aggregated
  modifiers (depth 1), and apply referring-expression reduction (name on first
  mention, pronoun after) within a volume. Read `instance-of` facts as a second
  classification head. `uncovered_predicates` shrinks as constructions are added.
- **The construction table** (authored, in `windows/book`): the C2 entries are
  `is-a`/`instance-of` (heads), `moon-count → "with N moon(s)"`,
  `star-class → "orbiting a ⟨class⟩"`, `day-length-std → "its day lasts about X
  standard days"`, and one or two more (`ocean-fraction`, `plate-count`). Small
  and closed; each new entry is a line and shrinks the coverage report.

## 4. Data flow

```
world-root entity: is-a planet · name "Vebe" · moon-count 2 · star-class "…F…" · day-length-std 1.55
      |
      v  windows/book: collect subject facts, map via the construction table
head:  Classify(subject="Vebe", complement="planet")
mods:  ["with two moons", "orbiting a yellow-white dwarf",
        "its day lasts about 1.5 standard days"]
      |
      v  domains/language: realize_common (head + dry modifier tail)
"Vebe is a planet with two moons, orbiting a yellow-white dwarf; its day lasts about 1.5 standard days."

collective species entity: name "Grobnar" · instance-of "goblin"
      -> "The Grobnar are goblins."
```

## 5. Determinism & contracts

- **No new seeded draws.** The autonym is a lexicon lookup (existing seed-derived
  material), like C1's endonym; classification is structural; number/quantity
  formatting is pure. Per metaplan decision #1 this stays a derived view.
- **Ledger changes → local regen + golden re-pin.** Moving `is-a`/`name` to the
  world-root entity and adding per-species collective entities changes each
  world's ledger; re-pin `world-seed-42.json` **in the drifting commit**
  (rebaseline-golden-pins) and regenerate the reference dumps + `the-book.md`.
  No new predicate is added (`instance-of` already exists), so the `concepts`
  dump is unchanged.
- **Byte-identity.** Same seed → identical Book. Aggregation order is the ledger's
  commit order (deterministic); number-to-words and rounding are pure.
- **The Book stays a derived view / sink.** Nothing reads rendered text back in.
- **Census-clean** is verified at `make gate-full`; a census move is a carve-out
  needing explicit authorization.

## 6. Testing

- **Grammar units** (`clause.rs`): the modifier tail (`"X is a planet with two
  moons."`), pronoun subjects (`"its day …"`), cardinal words (`2 → "two"`), the
  quantity formatter (`1.5507 → "about 1.5"`).
- **Construction table** (`windows/book`): `moon-count` → the moon fragment,
  `star-class` → the star fragment; a subject with several facts aggregates into
  one sentence with the fragments in a deterministic order.
- **Referring expressions:** the subject's name appears once; later mentions
  reduce to a pronoun.
- **The species proof:** a built world's collective species entity carries
  `instance-of ⟨kind⟩` and a `name`; the Book renders "The ⟨Autonym⟩ are
  ⟨species⟩"; an autonym-less people is classified but unnamed (a coverage gap,
  never a fallback).
- **Planet-identity fix:** the world-root entity (the `moon-count` holder) is the
  one carrying `is-a planet`; no orphan planet entity remains.
- **Determinism:** same seed twice → identical Book; `make gate-full` census-clean.

## 7. Firm choices (decisions)

1. **Decision 0061 — the classification split:** `is-a` = immutable concept-class,
   `instance-of` = mutable roster-kind; both coexist, roles as above.
2. **The planet is the world-root entity** (the fact-holder), not a separate mint.
3. **Aggregation depth 1** (the dry Volo register): one head + a flat modifier
   tail, no nested subordination.
4. **The autonym is the people's lexicon word for "person/people"** — a lookup,
   not a new draw; `None` is a coverage gap, never a fallback.
5. **The construction table lives in `windows/book`** (document-planning knows
   which domain predicates render how); `domains/language` stays domain-agnostic
   (frames, number words, quantity formatting).

## 8. Non-goals (C2)

- No etic two-register device / second-color annotation — that is C4's chorus.
  C2 renders quantities plainly ("about 1.5 standard days").
- No `instance-of` for deities or settlements, and no full per-type
  classification table — species only (C3).
- No per-language realization — Common only (C3).
- Aggregation stays depth 1; no relative-clause nesting or discourse arcs.
- No new predicates *invented* — C2 renders predicates the world already commits.

## 9. Flagged for G3

1. **Golden re-pin / determinism:** moving the planet's `is-a`/`name` to the
   world-root entity and adding collective species entities changes every world's
   ledger — a local reference-dump + `the-book.md` regen and a `world-seed-42`
   re-pin, verified census-clean at `make gate-full`. No new predicate, so no
   `concepts`-dump growth.
2. **The "standard days" unit:** `day-length-std` is in standard days; whether the
   Book says "standard days" or converts to "earth-days" (the etic cross-world
   unit) is a wording call — C2 uses "standard days" plainly and leaves the
   cross-world etic framing to C4. Confirm.
3. **Autonym source:** the proof assumes the universal-stratum lexicon carries a
   "person/people" concept for each peopled race; if a race lacks it, that people
   is classified but unnamed (a PROC-15 gap surfaced, not a fallback).
4. **Low-confidence:** the exact set of celestial predicates to construct in C2
   (moon-count, star-class, day-length are certain; ocean-fraction/plate-count
   optional) — the plan will fix the set; none affects determinism.
