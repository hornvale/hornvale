# `sentences/`

Frozen, provenance-stamped corpora for `domains/language`, measured against
a resolver rather than authored by one — the same discipline `tropes/` and
`systems/` already carry.

The directory holds **two kinds of corpus, not one**. Two are *dialogue*:
`the-merchant.corpus.json` (recorded) and `the-flood-watch.corpus.json`
(authored) — see "Recorded and authored corpora support different claims"
below. The third is a *capability ladder*: `the-ladder.corpus.json.DRAFT`,
a dependency-ordered sequence of graded rungs drawn from the typological
literature, which is not dialogue and is not spoken by anyone.

## Data and code stay separate

A corpus file is **data**: entries annotated with what they *demand* of the
language domain (decision 0011, the studies-are-data rule). It never encodes
how a demand is satisfied. The **resolver** — the code that reads a corpus
and decides, per entry, whether today's grammar can produce or parse it —
lives in `cli/tests/suite/sentence_corpus.rs`, and nothing in
`domains/language` reads a corpus file. Growing the resolver's capability
does not touch a corpus; growing a corpus does not touch the resolver's
logic.

## A corpus declares its demands, or derives them — never both

Two shapes, one internal entry type, two readers (decision 0386).

- **Declared** — the dialogue corpora state a `demands` list per utterance,
  as their annotator wrote it. `read_declared` requires `speaker` and
  `demands` and fails loudly on either being absent, so a malformed edit to
  a frozen corpus cannot score silently wrong.
- **Derived** — the ladder states, per rung, the **one** token it
  `introduces` and the rungs it `presupposes`. A rung's cumulative demand
  set is the transitive closure of that graph, computed on read by
  `read_derived` and never written into the file.

Deriving is the feature, not an inconvenience: **a hand-written demand list
can under-describe its own sentence, and a derived set cannot, because no
human restates it.** Materialising the closure back into the ladder would
state one fact twice and oblige an agreement test between the two copies,
whose cheapest repair when it disagrees is to delete the check — so the fact
is stated once. **No corpus file may carry both shapes.**

## Frozen before measurement

A corpus is frozen before any score is taken against it (decision 0016: a
study preregisters its criteria before the code that would move them). Each
corpus's entry count is asserted by a test — `cli/tests/suite/sentence_corpus.rs`
holds one for `the-merchant.corpus.json` (12) and one for
`the-flood-watch.corpus.json` (139). Changing
a corpus is a deliberate act: bump the asserted count in the same commit
and say why in the commit message. A corpus that drifts silently under a
measurement makes every earlier score incomparable with the next one.

A file whose name ends `.DRAFT` is deliberately outside this: it is not
frozen, no count is asserted anywhere, and no score may be taken against it
until it is. `the-ladder.corpus.json.DRAFT` is the one such file today, and
its own `status` field says so.

## Direction: what must be parsed, what must be produced

An entry may state a `direction` — `parse` for a player line, `produce` for
an NPC line or any ladder rung — and this splits "can the grammar do this?"
into two different questions that a single score blurs together. With one
corpus running 68 parse against 71 produce, a blurred score can rise while
the half that matters does not move at all.

So coverage is reported **per direction**, in three buckets: `parse`,
`produce`, and `unknown` (decision 0387).

**The `unknown` bucket is a real answer, not a gap.** `the-merchant.corpus.json`
states no `direction` key, so all twelve of its entries land in `unknown`.
It does carry `speaker` (`"player"` / `"merchant"`), which looks exactly
like a stand-in and is not one: mapping it would author a fact the corpus
does not carry, and its annotator never made that call. **Direction is never
inferred — not from `speaker`, not from anything else** — and a test holds
that, which was demonstrated red by splicing the forbidden mapping in.

## Frozen is not the same as measured

**Only `the-merchant.corpus.json` is coverage-scored.** All three corpora
are loaded by the resolver, and the two newer ones get a direction breakdown
and a vocabulary cross-check against the ladder — but neither has a coverage
score, and neither is waiting on a schema change. The entry shape that reads
them shipped. What is absent is a **coverage resolver over the two new
corpora**, which is a different and larger question: what "covered" should
mean for a corpus the grammar was never built toward, decided *before* a
number exists to chase.

Freezing the data first is the point: each corpus's demand annotation was
written before any score existed to chase, which is the only order decision
0016 permits. The distinction between a corpus that is frozen and one that
is *scored* is worth keeping visible, because an unscored corpus in this
directory is doing its job, not waiting.

The ladder adds a third state to that pair: neither frozen nor scored, but
a revisable draft. The cross-check between it and the flood-watch corpus has
already moved it once — 56 of that corpus's tokens named no rung on the
ladder's first draft. That is the argument for keeping two instruments
authored independently over the same subject: where they disagree, they
name a defect neither could find alone.

## A verdict is two-valued today; a third is reserved

Resolving a corpus entry against the grammar today yields one of two
answers — `entry_covered` (`cli/tests/suite/sentence_corpus.rs`) is a bare
`bool`, and the report emits only:

- **covered** — today's grammar can produce or parse this demand;
- **not yet** — it can't, and nothing says that's fine.

A third value is reserved for when a corpus needs it —
**declared-out-of-scope-with-a-reason**, for a demand excluded on purpose
rather than one nobody has built yet. **It is still reserved**, and nothing
since has reached it: `entry_covered` remains a bare `bool`, the schema
carries no reason field, and nothing can fail on a reasonless exclusion
because there is no out-of-scope arm to be reasonless in.

Do not read the direction buckets above as that third value arriving. They
are an **orthogonal axis**: `parse`/`produce`/`unknown` is a property of the
*entry* — which question is being asked of the grammar — while the verdict
is the *answer*, and every entry in every bucket still gets one of the same
two answers. A ladder rung whose demands the grammar cannot meet is `not
yet` in the `produce` bucket, not a third thing.

When the third value does arrive, it should carry the same rule
`seam-guard`'s `expect(survives: …)` and the trope corpora's `inapplicable`
verdict already enforce — a reasonless exclusion is a failure, not a pass —
with its own test to keep that honest, the same way `STALE-DECL` keeps
`seam-guard`'s declarations honest.

## A low score is the starting state, not a defect

`the-merchant.corpus.json` is an aspirational target recorded verbatim from
Nathan's own dialogue, not a description of what `domains/language` already
does. Its coverage starts near zero on purpose. That is the corpus doing its
job — naming the gap between what the world can say and what a scene
actually needs to say — not a sign that something is broken.

## Recorded and authored corpora support different claims

`the-merchant.corpus.json` was transcribed verbatim from a live brainstorm:
its warrant is that a person said those words with no intent to demonstrate
anything. `the-flood-watch.corpus.json` is **authored scene dialogue**,
composed deliberately in the knowledge that it would be annotated, so it can
select for constructions its author finds interesting and nothing rules that
out. Each corpus's `provenance` field must say which it is and what that
costs, because a coverage number reads the same either way and the field is
the only place the difference survives. The authored corpus still has real
warrant on one half: its player lines are written the way a player types —
lowercase, unpunctuated, elliptical — and that is the actual input surface,
whatever the scene around it is.
