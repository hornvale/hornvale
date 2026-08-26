# `sentences/`

Frozen, provenance-stamped corpora of dialogue for `domains/language`,
measured against a resolver rather than authored by one — the same
discipline `tropes/` and `systems/` already carry.

## Data and code stay separate

A corpus file is **data**: recorded utterances annotated with what they
*demand* of the language domain (decision 0011, the studies-are-data rule).
It never encodes how a demand is satisfied. The **resolver** — the code that
reads a corpus and decides, per entry, whether today's grammar can produce or
parse it — lives elsewhere, in `domains/language` and its tests. Growing the
resolver's capability does not touch a corpus; growing a corpus does not
touch the resolver's logic.

## Frozen before measurement

A corpus is frozen before any score is taken against it (decision 0016: a
study preregisters its criteria before the code that would move them). Each
corpus's entry count is asserted by a test — see
`cli/tests/suite/sentence_corpus.rs` for `the-merchant.corpus.json`. Changing
the corpus is a deliberate act: bump the asserted count in the same commit
and say why in the commit message. A corpus that drifts silently under a
measurement makes every earlier score incomparable with the next one.

## A verdict is two-valued today; a third is reserved

Resolving a corpus entry against the grammar today yields one of two
answers — `entry_covered` (`cli/tests/suite/sentence_corpus.rs`) is a bare
`bool`, and the report emits only:

- **covered** — today's grammar can produce or parse this demand;
- **not yet** — it can't, and nothing says that's fine.

A third value is reserved for when a corpus needs it —
**declared-out-of-scope-with-a-reason**, for a demand excluded on purpose
rather than one nobody has built yet. It does not exist: the schema carries
no reason field, and nothing can fail on a reasonless exclusion because there
is no out-of-scope arm to be reasonless in. When it does arrive, it should
carry the same rule `seam-guard`'s `expect(survives: …)` and the trope
corpora's `inapplicable` verdict already enforce — a reasonless exclusion is
a failure, not a pass — with its own test to keep that honest, the same way
`STALE-DECL` keeps `seam-guard`'s declarations honest.

## A low score is the starting state, not a defect

`the-merchant.corpus.json` is an aspirational target recorded verbatim from
Nathan's own dialogue, not a description of what `domains/language` already
does. Its coverage starts near zero on purpose. That is the corpus doing its
job — naming the gap between what the world can say and what a scene
actually needs to say — not a sign that something is broken.
