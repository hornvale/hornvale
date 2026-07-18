# The Echo — Design

**Status:** G3-approved (2026-07-18) · **Author:** Claude
(campaign-autopilot) · **Decider:** Nathan · **Program:**
[The Self-Writing Book metaplan](2026-07-17-the-self-writing-book-program-metaplan-design.md)
(inserted campaign — see §8) · **Serves also:** the game arc's
knowledge-transfer goal (LANG-3 floor; The Walk / The Seam lineage).

> Common speaks; Common hears itself. This campaign makes the Common grammar
> **bidirectional**: the same construction inventory that renders a
> `ClauseSpec` into a sentence runs in reverse as a parser, with the
> round-trip law `parse(realize(spec)) == spec` installed as a standing
> gate. The Book becomes its own parse corpus — every line the world writes
> about itself, the world can read back — and knowledge transfer over
> Common text (creature speaks, creature hears, fact survives transit)
> gets its first working seam.

## 1. Why now (the insertion argument)

The Common construction inventory is the smallest it will ever be: one
frame (`Classify`), a modifier tail, three construction-table fragments.
Every later campaign grows it (C3 adds none to Common but C4–C7 do, and
new predicates arrive continuously). Recasting the grammar as a
bidirectional inventory now costs one small campaign; retrofitting after
the inventory grows costs proportionally more, forever. The round-trip
law, once in the standing gate, forces every future construction to ship
with its reverse direction — comprehension structurally cannot drift from
production. Only Common ever needs this: alien tongues stay render-only
(NPC↔NPC transfer happens at the `ClauseSpec`/Fact level; the player
boundary and written Common artifacts are the only true parse surfaces).

## 2. Goal

Three deliverables, floor-first:

1. **The construction inventory.** Common's grammar becomes data: a list
   of `Construction` entries (form pattern ↔ `ClauseSpec` shape) with two
   small interpreters — `realize_common` re-implemented to walk the
   inventory forward (output **byte-identical** to today's), and
   `parse_common` matching it backward (`&str → Result<ClauseSpec,
   ParseError>`).
2. **The Book reads itself.** `windows/book`'s construction table gains
   inverses (fragment text → predicate + object), so a rendered volume
   line parses back to its subject and facts. Keystone test: **every line
   of every rendered volume round-trips** — the Book is the parse corpus.
3. **The knowledge-transfer seam.** An integration test through
   `windows/vessel`'s `Knowledge` ledger: speaker renders a fact to Common
   text, listener parses the text and absorbs the result, and the fact
   survives transit exactly. Text proven a lossless carrier for
   ClauseSpec-shaped knowledge.

## 3. Architecture

### 3.1 The construction inventory (`domains/language`)

A `Construction` is a form↔meaning pairing for one clause shape, e.g. the
`Classify` singular ("⟨Subject⟩ is a ⟨complement⟩⟨modifier tail⟩."), the
plural-generic ("⟨plural subject⟩ are ⟨complement⟩."). The inventory is a
`&'static`/`Vec` table in `domains/language` (new module or within
`clause.rs`); entries are data, the two interpreters are code (decision
0011's shape). Realization walks the matching entry forward, filling
slots; parsing tries entries in declared order, binding slots (subject
text, complement, modifier tail) and reconstructing the `ClauseSpec` —
including `Number`, `Definiteness`, and `Subject::{Name, Pronoun}`.

The refactor's safety rail: `realize_common`'s output is byte-identical
through the rewrite (the committed Book gallery and every clause test are
the drift net; no artifact may move).

### 3.2 Slot recognition and context

Common's content words are two closed sets: **category labels** (concept
and species labels — known statically) and **proper nouns** (the world's
committed `name` facts). The parser takes a small context argument (the
label set and name set) rather than guessing morphology; slot binding is
lookup, not stemming. The one systematic plural ("goblins" → "goblin" +
`Number::Pl`) inverts `species_label`'s documented naive rule. This keeps
parsing deterministic and total over the closed world — the anti-OOV
property.

### 3.3 Fact-level inverses (`windows/book`)

Each `CONSTRUCTION_ORDER` fragment gains an inverse beside its forward
builder: "with two moons" → (`moon-count`, 2 — via a `cardinal` inverse);
"orbiting a yellow-white dwarf (F)" → (`star-class`, "yellow-white dwarf
(F)"); "its day lasts about 1.5 standard days" → (`day-length-std`,
≈1.5). Quantities parse to the **quantized surface value** ("about 1.5" →
1.5, not 1.5507) — the parse target is what the sentence *says*, and the
lossless-carrier claim is scoped accordingly (see §6). The semicolon
composition is a book-level construction too: T4's "…; its day lasts…"
clause is appended by `windows/book`, so its inverse (split the sentence
at the "; " seam before clause-level parsing) lives beside it. A parsed
volume line yields `(subject name, is-a class, Vec<(predicate, surface
value)>)`.

### 3.4 The vessel seam (`windows/vessel`)

Floor: an integration test — speaker side renders a fact through the
grammar; listener side parses and `Knowledge::absorb`s it; assert the
absorbed entry equals the source. A player-facing `tell` verb ships only
if it falls out trivially from the session dispatch (flagged, §8). The
dialogue mechanics in LANG-3's row (echo-question holes, circumlocution,
mishearing) are explicitly the sequel.

## 4. Error handling

`ParseError` is typed and recountable (the `Gap` philosophy): which
construction matched furthest, which slot failed, what token was
unrecognized. No ranked candidate parses at the floor — Common's
ambiguity budget is near-zero (homophony lives in the generated tongues;
Common's closed label/name sets plus construction shape resolve slot
capture). An unparseable line in the round-trip gate is a red test, not a
soft skip.

## 5. Testing (the keystone is the gate)

- **Round-trip law, spec side:** property test — for generated
  `ClauseSpec`s over the closed frame/number/definiteness/subject/modifier
  space, `parse_common(&realize_common(s)) == Ok(s)`. Generator note (the
  Concordance lesson): the generator must cover the value space —
  pronoun subjects, empty and multi modifiers, plural generics, `an`-vowel
  complements — not just the happy path. The modifier generator draws from
  the closed fragment space (no embedded `", "` — the tail's join
  delimiter), which is legitimate because the inventory *defines* the
  language: the parser owes correctness over what the grammar generates,
  nothing more (§7).
- **Round-trip law, corpus side:** every line of every rendered volume
  (seeds 1/2/3 and the seed-42 artifact) parses, and re-realizing the
  parse reproduces the line byte-exactly.
- **Byte-identity:** the-book.md and every committed artifact unchanged
  through the realizer refactor (drift check + lens_purity).
- **The transfer test:** the vessel knowledge round-trip (§3.4).
- **Standing-gate amendment:** the round-trip corpus test joins PROC-15
  coverage in the metaplan's standing gate — every future construction
  ships bidirectional or the gate reddens.

## 6. Determinism and scope of the lossless claim

No draws anywhere — the parser is a pure function of (text, closed
context); grammar remains build-state (0058): no epoch, no ledger drift,
no census exposure. The lossless-carrier claim is scoped to the **surface
contract**: what survives transit is what the sentence says (the
quantized "about 1.5", not the full-precision 1.5507) — same boundary the
serialization quantization draws (0033), and the honest one for speech.

## 7. Non-goals

- No dialogue mechanics (echo questions, circumlocution, mishearing) — the
  LANG-3 sequel's.
- No parsing of generated tongues, ever (render-only by design).
- No free-text NLU beyond the construction inventory: the parser
  recognizes exactly what the grammar generates, nothing more.
- No conversational UX/NPC chat surface; no new vessel verbs beyond (at
  most) the trivial `tell`.
- No morphology (LANG-43's paradigms are a different campaign).

## 8. Flagged for G3

> **G3 OUTCOME (Nathan, 2026-07-18): APPROVED.** Item 1 ratified (the
> insertion stands). Item 2: floor as specified (mechanism + test; `tell`
> only if trivial). Item 3 confirmed — quantized knowledge is the intended
> semantics, with an owner direction going further: most creatures should
> not understand "1.5" / "one and a half" / "three over two" at all —
> numeracy as a per-listener comprehension filter. Deliberately deferred
> ("cross that bridge later") and banked as registry row **LANG-44**
> (numeracy as a quantity register); The Echo ships the surface-value
> contract unchanged.

1. **The metaplan insertion itself** (program-level): The Echo slots
   between C2 and C3; the metaplan gets an addendum note and the standing
   gate gains the round-trip law. Nathan approved verbally this session —
   this formalizes it.
2. **Vessel scope taste:** committed floor is mechanism + integration
   test; a player-facing `tell` verb ships only if trivial. Say the word
   if you want the verb committed regardless.
3. **Surface-value parse contract** (§6): parsing recovers what the
   sentence says, not the underlying full-precision fact. This is the
   honest speech-transfer semantics, but it means transferred knowledge
   is quantized knowledge — confirm that matches your game-layer intent.

## 9. Registry / docs consequences

LANG-3 flips `raw → spec'd (floor)` with Where → this spec; metaplan
addendum (§8.1); PROC-15 unchanged (coverage may not regress). Chronicle
slug: `the-echo`.
