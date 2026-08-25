# The Interlinear — design

**Date:** 2026-08-24 · **Status:** DRAFT, awaiting G3 · **Decider:** Nathan ·
**Author:** Claude (campaign-autopilot)

> An *interlinear* is the artifact where a foreign sentence, its
> morpheme-by-morpheme analysis, and its translation sit on stacked lines. It
> is what this campaign produces: one structure, realized twice, side by side.

## 0. What this is, in one paragraph

`domains/language` can say exactly one kind of sentence — "X is a Y" — and it
says it in Common by privilege rather than by peer status. This campaign makes
the clause structure a real **interlingua**: a predicate plus structural role
bindings, with Common demoted to one realizer among the tongues. It renders a
**real occupation from a real world** in Common and in a generated tongue, with
a real gap where the tongue lacks a word. It invents no new facts. It is the
first campaign of a program whose eventual target is a creature that can be
asked what happened last night and answer honestly, including about what it
does not know.

## 1. Where we actually are

Measured on `56b821eda`, not recalled:

| | |
|---|---|
| clause frames | **one** — `Frame::Classify` (`domains/language/src/clause.rs:18`) |
| constituent orders | six, WALS-weighted: SOV 45%, SVO 42%, VSO 9%, VOS 2%, OVS 1%, OSV 1% |
| copula | drawn per tongue from its own phonology, or zero-copula |
| articles | **drawn but never realized** — no article lexeme exists |
| evidential + noun class | realized at drawn depth (None/Particle/Affix), joined at the **segment** level |
| number | drawn per species with irregularity *emerging* from sound change — **never realized in a clause** |
| tense | `tense_depth`, `tense_position`, a family `morph/tense/past` proto-affix, all with permanent stream labels — **never realized**, because there is no verb to inflect |
| `Act` concepts registered | **20** — including `die`, `know`, `eat`, `move`, `look`, `read`, `write`, `sense` |
| direct consumers of the clause API | **four files**: `windows/almanac/{lib,phenomenon_line}.rs`, `windows/book/lib.rs`, `cli/tests/suite/common_is_total.rs` |

So the phonology and morphology are well ahead of the syntax. The tongues can
inflect, mark evidentiality and order constituents six ways; they have almost
nothing to say.

### 1.1 The leak that proves the diagnosis

`ClauseSpec` calls itself "a language-neutral clause" and carries
`modifiers: Vec<String>` — pre-rendered English (`"with two moons"`,
`"orbiting a yellow-white dwarf"`), which the realizer "only joins". Two
consequences, and the second is the sharper one:

1. **Anything richer than the bare frame is untranslatable by construction.** A
   tongue realization cannot carry an English phrase, so it drops it or lies.
2. **English grammar has already leaked out of `domains/language` and into a
   window.** `windows/book/src/lib.rs:181` duplicates `indefinite_article` from
   `domains/language::clause`, with a doc comment explaining that "the
   aggregation seam keeps `domains/language` untouched". That is a window
   knowing how English articles work. It is not a lapse — it is the correct
   local response to an interlingua that was not expressive enough, which is
   exactly why the interlingua has to change rather than the window.

## 2. The foundation

**An utterance is a fact. An event is an entity. Roles are predicates on that
entity.**

This is not a new ontology. `domains/history` already does all three:

```
is-occupation        the event IS an entity
occ-people           who
occ-site             where
occ-founded/-ended   when
occ-cause            WHY
occ-ended-by         WHO ENDED IT
occ-founded-from     source
```

`occ-cause` and `occ-ended-by` are thematic roles. They live in the concept
registry like every other predicate, and the drift-checked concept page already
covers them.

**Why not a role ontology (agent/patient/experiencer/instrument).** It is the
linguistically richer answer and it was seriously considered. It costs a second
roster that must agree with the registry forever, with nothing mechanical
forcing agreement — the shape decision 0094 warns about and The Drift had to
consolidate away (`DelveRung` / `DelveZone`). Under the fact-shaped design,
adding "who caused this death" is a registered predicate, `death-by`, exactly
parallel to `occ-ended-by`.

**What this deliberately does NOT claim.** An earlier draft of this argument
said a role ontology would let the language "invent detail the world lacks".
That is wrong and Nathan corrected it: the world is deterministic and *knows*
why the guard killed the woman. Three distinct things were being collapsed:

| level | what it is | where it lives |
|---|---|---|
| 1. world truth | total, re-derivable from the seed | the simulation |
| 2. the committed record | what was written down, and therefore witnessable and transmissible | `Ledger` |
| 3. what a creature holds | a subset, distorted in transit | `vessel::Knowledge`, `windows/hearsay` |

Committing a `cause` predicate is what moves a cause from level 1 to level 2.
That is a deliberate authoring act per event kind, not a blanket rule, and it is
the line this campaign respects. **A creature speaks from level 3.** The
merchant's "I don't know why he killed her" is not a construction to be
special-cased; it is the honest rendering of a level-3 model with a hole in it,
at the granularity of one predicate.

## 3. The three commitments

These exist to answer one question — *how do we know we are not building bad
technology?* — with something checkable rather than with taste.

**C1. The grammar is data, and bidirectional.** `clause.rs` already establishes
this: a `Construction` is a form↔meaning pairing as an ordered `Part` list, and
its own doc says *"the same entry realizes forward and parses backward — a
future frame is added HERE, and is bidirectional by construction... every future
frame adds an entry, never a second code path."* That instinct is correct and
this campaign keeps it. **If adding a sentence shape ever requires a second code
path, that is the review signal that we have started writing an English
parser.**

**C2. The controlled language is declared, not implicit.** What Common can say
is a list, not the accidental shape of what the parser happens to accept.
Restrictions — question-word-initial, restricted pronouns, whatever a later
campaign needs — are declared, per language, and may differ between Common and a
tongue and may loosen over time. "What we can say" becomes a fact about the
world rather than an artifact of the implementation.

**C3. Coverage is measured against a frozen corpus.** The third sibling to
`tropes/` and `systems/`, with the same discipline (decisions 0011, 0016): the
corpus is **data**, the resolver is **code**, the corpus is frozen before
measurement, its entry count is asserted, and the verdict ratchets. See §5.

## 4. Scope

### 4.1 The interlingua

`ClauseSpec` becomes predicate-general:

- a **predicate**: a concept id, `Act` or otherwise
- **role bindings**: named roles bound to entities or concepts, where each role
  name **is a registered predicate** (`occ-site`, `occ-cause`, …)
- **speaker features**: number, definiteness, polarity, mood, evidential

`modifiers: Vec<String>` is **deleted**. Every current caller expresses its
meaning structurally instead. `Frame::Classify` survives as one construction
among others — this campaign does not remove it and does not renumber it.

### 4.2 Common becomes a realizer

Common gains a realizer with the same signature shape the tongues use, over the
same structure. It keeps its **totality guarantee** — concept→Common is
infallible, concept→tongue returns `Result<_, TongueGap>` — because that
asymmetry is a true statement about the world (a people may have no word for the
sea; the author's register always does), not a privilege of the representation.

`windows/book`'s duplicated `indefinite_article` is deleted and its behaviour
returns to `domains/language`.

### 4.3 The target sentence

The campaign is done when a **real occupation from seed 42** renders in Common
and in a generated tongue from the same structure, and when a tongue lacking a
word gaps with a recountable reason rather than emitting English.

Occupations are chosen deliberately: they exist in every world, they already
carry who / where / when / cause / ended-by, and they need **no new facts**.

## 5. The corpus

`sentences/` — a frozen, provenance-stamped corpus of things this world should
be able to say, resolved against the grammar and reported as a coverage number.

**It starts near zero, and it is supposed to.** The founding entry is Nathan's
merchant dialogue, recorded verbatim as an aspirational target:

```
You:      How's it going?
Merchant: Everything was fine until last night.
You:      What happened last night?
Merchant: Last night, there was a death in the marketplace. A guard killed a
          woman. I don't know why he killed her. Seeing it confused and upset me.
You:      Did you know the woman?
Merchant: I think her name was Gilda. I didn't know her.
...
```

Each line is annotated with what it demands — transitive frame, past tense,
temporal adverbial, negation, polar question, wh-question, epistemic hedge,
embedded clause, coordination, existential, pronoun reference, named-entity
list, witness set. **The corpus is the program's map**, and the coverage number
is how a future campaign proves it moved something.

**The resolver is code, the corpus is data**, and the corpus is frozen before
any measurement is taken. A verdict is three-valued in the shape this repo
already uses: covered / not yet / declared-out-of-scope-with-a-reason.

## 6. What this campaign deliberately does NOT do

Recorded so a reader does not propose them as omissions:

- **No new event kinds and no new facts.** Occupations are the target precisely
  because they already exist.
- **No tense.** There is nothing to inflect until a verb frame carries time, and
  the drawn tense machinery keeps waiting.
- **No questions, no parsing beyond what exists.** `parse_common` keeps its
  current coverage. Recognition is where controlled languages historically rot,
  and it gets its own campaign with the corpus already in place to measure it.
- **No pronouns or reference tracking.**
- **No articles in tongues.** `TongueGrammar.articles` stays drawn and
  unrealized; giving it a surface needs an article lexeme, which is morphology
  work.
- **Number stays naive in Common.** It is the very next campaign and this one
  must not quietly absorb it.

## 7. Verification

- **Byte-identity is the headline, again.** The interlingua change is a
  refactor: the almanac, the book and every committed artifact must render
  identically. `make rebaseline` and `make rebaseline-goldens` move nothing.
  Where a rendering *does* move, it moves because a caller's meaning was
  previously inexpressible, and each such case is named in the chronicle.
- **The gap is exercised, not assumed.** A test asserts a real tongue gapping on
  a real concept with its recountable reason — not a synthetic lexicon.
- **The corpus number is recorded** at its frozen starting value.
- **`cli/tests/suite/common_is_total.rs`** keeps passing: Common stays total.

## 8. Risks

**The one that could sink it.** `parse_common` must keep working against a
restructured `ClauseSpec`. Bidirectionality is a *claim* the current design
makes on the strength of one construction; this campaign is the first real test
of it. If round-tripping forces a second code path, that is a finding, and the
right response is to say so in the chronicle rather than to write the second
path quietly.

**Sequencing.** `campaign/the-confidant` holds a live hold-off on
`domains/language` and is appending an accession cohort; it is running a census
as of this writing. This campaign must land after it, and should re-read its
chronicle rather than only its diff — a semantic collision here is exactly the
class no gate catches.

**Scope creep toward the exciting part.** Campaign 1 is deliberately not the
exciting one. The merchant is several campaigns out and the corpus exists partly
to make that distance legible rather than tempting.

## 9. The program

First three named; the rest is fog on purpose, because an unimplemented followup
is an unverified claim.

1. **The Interlinear** *(this spec)* — the interlingua; Common demoted to a
   peer; the corpus founded.
2. **Number, both sides** — `CommonVocabulary` gains inflection at the seam that
   already exists; the tongues' drawn number paradigms finally surface. No wire
   change.
3. **An event is an entity** — generalize the occupation pattern to a second
   event kind with cause and agent, plus **witnessing**: who was co-present at
   that place and day. The merchant's substrate begins here.

Then, unplanned: knowledge-of-events and honest gaps · questions as queries ·
reference and conversation · morphological tense.
