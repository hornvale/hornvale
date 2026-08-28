# The Stile

*A stile is the upright a ladder's rungs are mortised into. [The
Mortise](the-mortise.md) cut the cavity; this campaign is the piece the rungs
seat in — and, because a ladder standing alone tells you nothing, the second
instrument that says whether its rungs are in the right places.*

Three corpora now sit in `sentences/`, and before this campaign the resolver
could read one of them:

| corpus | entries | shape | state |
|---|---|---|---|
| `the-merchant` | 12 | `id speaker text demands` | frozen, scored three times |
| `the-flood-watch` | 139 | + `scene direction note` | frozen, never scored |
| `the-ladder` | 214 | `id text introduces presupposes note` | draft, unfrozen |

The merchant corpus is transcribed speech: twelve utterances a person actually
said in a live brainstorm, under no pressure to demonstrate anything. It has
been the whole instrument since [The Interlinear](the-interlinear.md) founded
`sentences/`, and it read 0, then 2, then 5 of 12 across three campaigns. It
still reads **5 of 12** — `m05 m06 m07 m09 m10` — through the same two
assertions, unmodified, which is this campaign's regression test rather than
its result.

What is new is that two more corpora exist, that one resolver reads both
shapes, and that the number it reports is now three numbers.

## Two instruments, authored blind, and the one that was wrong

The ladder is a **capability ladder**: 214 rungs, each an ordinary sentence
that introduces exactly **one** demand token and names the rungs it
presupposes. It is a dependency graph over grammatical capability, 199
distinct tokens across 199 introductions — no token introduced twice — with
15 control rungs that introduce nothing and exist as free tripwires on
composition. It has exactly two roots: `r001` (`classify`) and `r002`
(`intransitive-frame`).

The flood-watch corpus is five scenes of investigative dialogue, authored by
the project owner, annotated against the ladder's token vocabulary. Neither
author saw the other's work.

**The cross-check between them moved the ladder, not the corpus.** Against the
150-rung first draft, the corpus demanded **56 tokens that had no rung at
all** — a third of its 149-token vocabulary. Two failures are worth naming
because they are the same failure:

- **The ellipsis and anaphora family was two tokens where fragmentary dialogue
  needs eleven.** The draft carried `ellipsis` and `discourse-anaphora`. It now
  carries `ellipsis` (general), `fragment-answer`, `subject-ellipsis`,
  `vp-ellipsis`, `null-complement-anaphora`, `verbless-clause`, `so-inversion`,
  `discourse-anaphora`, `one-anaphora`, `propositional-anaphora` and
  `detached-relative`. The draft's `ellipsis` rung was in fact a *fragment
  answer* wearing the general name, while the corpus used the general sense 41
  times — the exact under-description a derived demand set exists to prevent,
  reappearing one level up, in the vocabulary rather than in a demand list.
- **`proper-name` appeared 23 times and had no rung.** The draft had
  `named-entity-list` — three names coordinated — and nothing for one name in
  an argument position.

That second shape recurred four more times, and it is the campaign's most
transferable finding about how such a ladder goes wrong. `manner-adverb` was
missing while the clausal manner adjunct was present; `pp-modifier` missing
while locative predication was present; `infinitival-complement` (subject
control) missing while object control was present; `attributive-adjective`
missing while *predicative* property words were present. **A typology-derived
ladder reaches for what the literature has a name and an argument for, and the
unmarked member of an opposition frequently has neither.** Two unmarked cases
were considered and declined with the reason recorded — present tense and
singular number are the default realization of all 214 rung texts, not
constructions a generator could fail to build — because the reason is the part
that can later turn out to be wrong.

The revised ladder was renumbered once, in dependency order, and its file
states that this was the last free renumbering: at the time, nothing had ever
been measured against it, no test asserted its count, and no artifact cited a
rung id. That reason has since gone false without the conclusion moving —
three committed tests now cite specific rung ids by name, and several doc
comments cite more — which only sharpens the point: the renumbering was free
because it happened before anything cited an id, not because the file remains
uncited today. After freeze, ids append regardless.

## Declared, or derived — never both

The ladder carries no `demands` field, and that is the design rather than an
omission. A rung's cumulative demand set is the **transitive closure** of its
presuppositions, collecting each rung's `introduces`, computed on read. The
smallest is `r001` at one token; the largest, `r183` and `r191`, at 22.

The resolver therefore gained one internal entry type and two readers — a
declared reader for the dialogue corpora, a derived reader for the ladder —
and no corpus file carries both shapes
([decision 0386](../../../docs/decisions/0386-a-corpus-declares-its-demands-or-derives-them-never-both.md)).
Writing the closure back into the ladder would state one fact twice and
oblige an agreement test between the two copies, whose cheapest repair when it
fails is to delete the check.

Transitivity is the load-bearing property, so it is guarded by a mutation
rather than by an assertion. That guard needed care: **203 of the 214 rungs
discriminate between a one-level and a full closure, and 11 do not** — a test
built on any of those 11 passes under a shallow implementation and proves
nothing. The standing test uses `r183`, whose shallow answer is 3 tokens and
whose transitive answer is 22, and which is *itself a diamond*: its two direct
presuppositions reach shared ancestors `r001` and `r002` independently, so one
rung witnesses both reach and path reconvergence. A graph-wide scan then
settled whether reconvergence needed a second dedicated witness: **140 of 214
rungs contain a diamond somewhere in their closure**, so it is the norm, not a
rare structure.

The readers enforce different requirements at the JSON boundary. The declared
reader requires `speaker` and `demands` and panics naming the missing field;
the derived reader has neither to require. An early draft defaulted both to
empty for the convenience of one shared type — which would have made a future
malformed edit to a frozen corpus score silently wrong instead of failing at
load. `direction`'s default stays, because there its absence maps to a real
absence of fact.

## Two numbers, and a third that says "unknown"

`direction` distinguishes what the grammar must **parse** — a player line —
from what it must **produce** — an NPC line, or any ladder rung. The
flood-watch corpus splits **68 parse against 71 produce**, and at that ratio a
single blurred score can rise while the half that matters does not move.

So coverage is reported per direction
([decision 0387](../../../docs/decisions/0387-an-absent-direction-is-unknown-never-inferred.md)):

| corpus | parse | produce | unknown | total |
|---|---|---|---|---|
| the-merchant | 0 | 0 | 12 | 12 |
| the-flood-watch | 68 | 71 | 0 | 139 |
| the-ladder (draft) | 0 | 214 | 0 | 214 |

The merchant row is the interesting one, and it is **correct rather than a
gap**. That corpus states no `direction` key at all. It does carry a `speaker`
field valued `"player"` or `"merchant"`, which looks exactly like a stand-in
and is not one: mapping it would author a fact the corpus does not carry —
this campaign's opinion wearing the corpus's provenance.

The prohibition is held by a test whose failure was demonstrated rather than
described. Splicing the forbidden mapping into the declared reader turns
`merchant_entries_resolve_as_direction_unknown` red reading `parse: 4,
produce: 8, unknown: 0` — literally the forbidden speaker split, not a proxy —
while fifteen of the file's other tests stay green, so the guard is specific
as well as loud.

## 147 of 149, and why the ceiling is not 149

The cross-check between the flood-watch corpus and the ladder is now a
standing test rather than a one-time analysis. It reads **147 of 149**, and
the two absent tokens are **refused with a reason** rather than missing
([decision 0388](../../../docs/decisions/0388-the-ladder-is-a-production-instrument-and-parse-robustness-is-a-separate-axis.md)).

`unpunctuated-input` (68 instances) and `contraction-elision` (12) are
properties of the **input surface**. Both occur only on `parse` entries.
Neither names anything a generator can produce or fail to produce — every
ladder text is well-formed prose. Minting rungs for them is the tempting move
and it is the one that corrupts the instrument: a ladder mixing *the generator
can build a cleft* with *the parser tolerates a missing apostrophe* counts two
capabilities under one number and answers neither question. The ladder
declares itself a production instrument in its own `production_axis` block,
and the test asserts the refused pair **as a set, with its reason
doc-commented**, so the ceiling ships attached to its explanation rather than
as a bare figure a later reader takes for a shortfall.

The merchant corpus's 13 distinct tokens are all present on the ladder, 13 of
13 — which is the null result that matters, since the ladder was revised
against the *other* corpus and could easily have drifted away from this one.

The cross-check's own sensitivity was established twice over: nulling `r001`'s
`introduces` in a temporary copy makes `classify` newly appear absent (with
the real ladder byte-compared before and after to prove it was never written),
and stubbing the set-difference helper to return the empty set reddens both
coverage tests — the difference between *my test detects a missing token* and
*my test detects anything at all*.

## Structural assertions freeze nothing; a count assertion is the freeze

The ladder gets six structural assertions: acyclic, ids unique, no token
introduced twice, every rung's closure a superset of its presuppositions'
closures, exactly two roots, and the closure computable for all 214. Every one
of these holds at any size.

It deliberately gets **no count assertion**. `MERCHANT_ENTRIES = 12` is
precisely the mechanism that froze the merchant corpus, and freezing the
ladder is the project owner's act after review, not a side effect of wiring a
resolver. An earlier spec draft asked for both and contradicted itself; the
structural/count split is the resolution.

Acyclicity is checked by DFS colouring with children looked up **by id through
a map**, never by array position — the detail that separates a cycle detector
from one re-encoding the file's own ordering — and its positive control builds
a synthetic two-node cycle in a temporary file, confirms the error names both
ids, and asserts in the test that the real ladder's bytes are unchanged. A
reviewer added a three-node cycle and a cycle reachable only from an unrelated
mid-graph rung; both were detected and named in full.

## What this does not reach

**The ladder is unfrozen, and no count is asserted — deliberately.** A frozen
count *is* the freeze mechanism, and freezing is the project owner's act after
review. Adding `LADDER_ENTRIES` is one line. It is also the moment the ladder
stops being a draft and its rung ids become **append-only forever**, which is
why nothing in this campaign performs it.

**Unfrozen cuts only one way: append is free, rewiring mid-graph is not.**
The two closure assertions that pin exact demand sets for r004 and r183
(`cli/tests/suite/sentence_corpus.rs`) go red the moment a rung is inserted
*and wired into the middle of the graph* — exactly what the ladder's last
revision did, 64 rungs placed throughout rather than appended at the end. A
red there is not a bug: it means the closure genuinely moved and the pinned
set needs re-deriving from the committed JSON in the same commit. The ladder
is still unfrozen either way; only its *count* is unpinned, never its
existing rungs' derived closures.

**No realization witness exists for the 353 new entries, and this is the
largest honest gap the campaign ships.** [The
Mortise](the-mortise.md) turned the merchant score from a declaration into a
demonstration ([decision
0330](../../../docs/decisions/0330-the-corpus-score-is-demonstrated-not-declared.md)):
for every entry the resolver calls covered, a committed `Clause` and the
Common surface it actually realizes, checked by a test that has to be able to
fail. That witness found two real defects nothing else had — `m10` scored
covered while `realize_common` panicked, and `m07`'s first draft passing every
assertion while never constructing the embedded clause its tokens credited it
for. It does not scale to 353 entries, and a witness nobody maintains is worse
than none. So `the-flood-watch` and `the-ladder` are scored **the way the
merchant corpus was before The Mortise: declared, not demonstrated.** Witness
policy at this scale is its own question, deferred with this sentence as its
record.

**52 ladder tokens are unexercised by the flood-watch corpus, and that is not
evidence they are wrong.** Five scenes of one register cannot indict a rung
about switch-reference. None was pruned; the count is worth recording at each
cross-check, and several of the unexercised rungs are capabilities the ladder
declares out of reach on purpose.

**Two possible mis-annotations sit in the frozen flood-watch corpus, edited in
neither instrument.** `s4-u23`'s `cleft-focus` would more naturally be
`identity-predication` + `negation` under the revised ladder — the corpus's
own note already calls the reading uncertain, and the token it needed did not
exist when it froze. And `ellipsis` on fragment answers now under-describes
relative to a ladder that distinguishes `fragment-answer`; the corpus declared
that usage in its own `shape_notes`, so it is documented coarseness rather
than error, and the coarseness was in the draft ladder it was annotating
against. A third candidate turned out to be the two instruments **agreeing** on
a coarseness rather than either being wrong: `adjunct-wh-question` covers both
matrix and embedded interrogatives, where the demand is arguably scope rather
than question type, and the ladder does not name that distinction either.

**Parse-robustness is unmeasured, not measured-and-passing.** The 68 player
lines — lowercase, unpunctuated, apostrophe-free, elliptical, sometimes
malformed — are a real sample of the input surface a natural-language
interface receives. Nothing in this project scores them, and by 0388 they
cannot borrow the ladder's rungs to be scored against.

**No resolver runs over the flood-watch corpus.** It has a direction
breakdown and a vocabulary cross-check, not a coverage score. Wiring one needs
a decision about what "covered" means for a corpus whose demands the grammar
was never built toward, and inventing that mid-rewrite would have moved the
merchant number by moving its definition — the one thing decision 0016's
discipline forbids.

**The ladder is not ordered by demand weight.** `conversational-implicature`
is wanted 25 times and sits at `r205`; `proper-name` is wanted 23 times and
sits at `r008`. Both sit where their dependencies put them. Computing a build
order from a ladder and a corpus together is a fine thing to want, and it is
**code** — a resolver's job, not data
([decision 0011](../../../docs/decisions/0011-studies-are-data-metrics-are-code.md)).
