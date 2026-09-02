# 0577. The realization witness

**Status:** Accepted (2026-09-01) · **Decider:** Nathan · **Campaign:** The Avowal

## Context

`cli/src/tropes.rs::resolve` scored a dramatic situation `Stageable` when
every token its `requires` list named resolved through the concept registry
(and, after decision 0576, the provision table). Registry membership is
append-only and `PredicateDef` is `{ name, functional, doc }` with no
object-type constraint, so a token was never hard to satisfy — naming a
predicate is not the same claim as a world being able to stage the scene
that predicate is supposed to describe. Decision 0330 already met this exact
failure on the sibling *sentence* corpus (`sentence_corpus.rs`'s
`IMPLEMENTED_DEMANDS`): *"a token added on optimism moves the score without
moving the grammar, which would make the instrument worse than no
instrument."* Its answer was a **realization witness** — a committed,
checked artifact standing behind every claimed entry. The trope corpus had
no equivalent. Both frozen corpora score 0 Stageable today (0/36
`polti-1895`, 0/409 `tvtropes-2012`), so this is the cheapest moment this
gate will ever be to add: there is no existing claim to retrofit.

## The decision

`Stageable` now requires, in addition to every requirement token resolving,
a registered **witness**: a hand-authored [`hornvale_vessel::Tableau`] that
`Session::start` can actually stage against a real `World` — its cast places
as entities, every `StagedThing` finds the cast member it names, and every
`StagedRelation` commits as a fact the concept registry accepts and the
ledger does not contradict. `tropes::resolve` gains two new parameters,
`world: &World` and `witnesses: &Witnesses` (`Witnesses = BTreeMap<String,
Tableau>`, keyed by situation id, threaded through the same way decision
0576's `Provision` table is — supplied by the caller, never rebuilt inside),
and a new `witness_stages` function that performs the check.

**What this bar proves, stated precisely — the failure this project
documents most is overstating it.** It does *not* prove any real, generated
world produces the situation. What it proves that token membership alone
never did:

1. the actants must exist as entities — the tableau has to place them;
2. `functional` must be declared correctly (a `feels-toward` wrongly marked
   functional breaks the instant one actant regards two targets);
3. the facts must pass contradiction-checking *together*, not one at a time;
4. the cost of a false claim rises from zero to a file — every claimed
   situation now costs an authored tableau.

**The gate is checked ONLY once every requirement token already resolves.**
A situation still `Blocked` on tokens never reaches `witness_stages` at all.
This is why wiring the gate moved **zero verdicts** on either frozen corpus
today — `resolve`'s witness branch is unreached by every situation in either
corpus, confirmed by `make rebaseline` producing a byte-identical demand
table (only the report's header prose changed, stating the new boundary).

**A witness failure folds into the existing `Blocked(Vec<String>)` shape**
rather than a new `Outcome` variant. An absent witness renders the sentinel
token `"witness:absent"`; a witness that fails to stage — whether its
actants (a `StagedThing` naming a cast index nobody placed) or its relations
(a predicate the registry does not hold) — renders `"witness:refused"`. The
two are distinguished so a reader of a `Blocked` reason can tell "nobody
wrote one" from "one was written and the world refuses it," but both are
refused identically by `resolve`: neither can claim `Stageable`. A third
`Outcome` variant was considered and rejected — every consumer of `Outcome`
(the report renderer, the matrix, the per-corpus ratchet tests) already
handles `Blocked` as "here are the reasons," and a witness failure is
exactly that kind of reason, not a new kind of verdict.

## Consequences

- The production witness roster, `tropes::witnesses()`, is **empty today,
  deliberately** — spec §4.2's "migration cost is zero and will never be
  this low again" means authoring a witness for a situation whose tokens do
  not yet resolve would cost a file and move no verdict.
- `resolve`'s signature grew two parameters; every call site (`cli/src/
  main.rs`'s `cmd_tropes`/`cmd_tropes_matrix`, `cli/tests/suite/
  provision.rs`, `cli/src/tropes.rs`'s own unit tests) now threads a real
  `World` through, even where (as in most of those tests) the witness branch
  is never reached — the token-only tests build one anyway because the
  parameter is unconditional, not because they exercise it.
- A future home for a real witness (once some situation's tokens resolve)
  is added by giving `witnesses()` a row and authoring the tableau — never
  by touching `resolve` again.
- The report header (`render`, `render_matrix`) now states, in prose, that
  `Stageable` measures witnessed capability and that a coverage figure
  spanning this boundary is not comparable — the number a reader takes away
  did not move, but what it certifies did.

## See also

Spec §4.2 (`docs/superpowers/specs/2026-09-01-the-avowal-design.md`);
decision 0330 (the sibling precedent on `sentences/`); decision 0576 (the
provision table this gate composes with);
`docs/superpowers/ledgers/2026-09-01-the-avowal.md` entry #8;
`cli/src/tropes.rs`; `cli/tests/suite/trope_witness.rs`.
