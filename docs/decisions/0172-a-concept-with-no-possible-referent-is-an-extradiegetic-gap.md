# 0172. A concept with no possible referent is lexicalised as an extradiegetic gap

**Status:** Accepted (2026-08-22) · **Decider:** Nathan · **Relates:**
[0169](0169-mood-is-a-property-of-the-action-and-the-sigil-is-a-namespace.md)
(what makes a concept out-of-character in the first place)

In the context of registering concepts for operator instruments — acts like
`recount`, `survey`, `lens` that a player performs and no creature could — we
decided that **such a concept is lexicalised as
`ExposureClass::Unknown { reason: GapReason::Extradiegetic(..) }`, a fourth
variant added for it**, rather than reusing an existing gap or leaving the
concept unlexicalised.

## Context

`domains/language` already models "this culture has no word for that, for a
recountable reason," and `common_vocab.rs` states the discipline it protects:
*a gap always means something true about the world, and never an authoring
hole.* The question was whether an operator instrument fits one of the three
existing reasons. It does not, and **the reason it does not is the whole
content of this decision**:

| variant | what it says | can it close? |
|---|---|---|
| `Experiential` | this culture never met the referent | yes — meet it |
| `Perceptual` | the senses have not resolved it | yes — resolve it |
| `Unnameable` | *"the referent is real and objective"*, merely unnamed | yes — name it |
| **`Extradiegetic`** | **there is no referent in the world at all** | **never** |

All three existing gaps are contingent. `!why` is not a thing no goblin has
encountered; it is a thing no goblin *could* encounter. Reusing `Unnameable`
would have asserted the referent is real, which is the one thing that is
false.

## Consequences

- The variant carries a `String` reason, so a gap states its own case rather
  than being inferred from the concept's name.
- **Out-of-character concepts must not enter `packs.rs`**, the Swadesh-style
  core-vocabulary roster, because pack membership maps straight to
  `ExposureClass::Steeped` — and a `Steeped` concept is never `Unknown`, so the
  filter this decision exists to trigger would never be consulted. Registering
  an operator instrument as core daily vocabulary is also simply false. The
  precedent followed instead is astronomy's spectral classes: registered, and
  classified in the exposure derivation rather than the pack roster.
- `CommonVocabulary` stays **total**. Common is the author's register and has
  no speakers, so an operator instrument having a Common word is correct
  rather than awkward — the gap is about what a *culture* could name, not
  about what the author can write down.
- An extradiegetic concept is excluded from the proto-root universe, so it
  draws no word and moves no lexicon artifact. That exclusion is the
  observable consequence, and it is what the arc's first task tested.
