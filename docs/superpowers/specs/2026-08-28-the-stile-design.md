# The Stile — one resolver, three corpora, two numbers

**Campaign:** The Stile
**Branch:** `campaign/the-stile`
**Decision block:** 0386–0395 (main ceiling 0376 at reservation)
**Predecessors:** The Interlinear (founded `sentences/`), The Inquest, The Mortise
**Date:** 2026-08-28

A stile is what rungs are mortised into. The Mortise cut the cavity; this is the
upright the rungs seat in.

---

## 1. What exists, and why nothing can read it

Three corpora now sit in `sentences/`, and the resolver reads one of them.

| corpus | entries | shape | state |
|---|---|---|---|
| `the-merchant` | 12 | `id speaker text demands` | frozen, scored 3× |
| `the-flood-watch` | 139 | + `scene direction note` | frozen, never scored |
| `the-ladder` | 214 | `id text introduces presupposes note` | **DRAFT, unfrozen** |

`cli/tests/suite/sentence_corpus.rs`'s `Entry` struct requires `speaker` and
`demands`. The ladder carries **neither**, and that is deliberate: each rung
declares the **one** token it introduces plus the rungs it presupposes, and its
cumulative demand set is **derived by transitive closure**.

**The derivation is the feature, not an inconvenience.** A hand-written demand
list can under-describe its own sentence, and in a 12-entry corpus it already did
three times — m02's tokens omit adjectival predication, m10 is covered but
unspeakable, m07 was credited for a construction its witness never built. A
derived set cannot under-describe, because no human restates it.

## 2. Design

### 2.1 One internal representation, two readers

The resolver gains a single internal entry type. Two readers produce it:

- **declared** — `demands` is read as authored (`the-merchant`, `the-flood-watch`)
- **derived** — `demands` is computed as the transitive closure of `presupposes`,
  collecting each rung's `introduces` (`the-ladder`)

**A corpus file carries one shape or the other, never both.** Writing a derived
`demands` list back into the ladder would state the same fact twice — the exact
duplicated-rule shape whose cheapest repair is to delete the check, which is how
a guard is lost rather than fixed.

### 2.2 Two numbers, not one

`direction` distinguishes what the grammar must **parse** (a player line) from
what it must **produce** (an NPC line, a ladder rung). The current resolver asks
one blurred question — "can the grammar produce *or* parse this" — and with 68
parse entries against 71 produce entries in one corpus, the blur stops being free.
A single score can rise while the half that matters does not move.

So coverage is reported **per direction**.

### 2.3 Direction is OPTIONAL, and absence is not inferred

- `the-flood-watch` states `direction` per entry.
- `the-ladder` declares itself a **production** instrument in its
  `production_axis` block; every rung is `produce`.
- `the-merchant` states **nothing**, and the resolver **must not guess**.

Inferring direction from `speaker` for the merchant corpus would be authoring a
fact the corpus does not carry. Merchant entries resolve as **direction-unknown**
and are reported in their own line. Three numbers where three are true is honest;
two numbers achieved by inventing the third is not.

## 3. The constraint that is easy to miss

**The merchant corpus's score must still be 5 of 12, with the covered set
`m05 m06 m07 m09 m10`.**

That number is comparable across three campaigns — The Interlinear read 0, The
Inquest 2, The Mortise 5. Decision 0016's rule is usually stated about the
corpus, but it binds the **scoring method** the same way: a resolver that changes
what "covered" means makes every earlier score incomparable, and no commit
message can undo that. The existing assertions
(`merchant_coverage_is_five_of_twelve`, `the_covered_entries_are_*`) are the
regression test for this rewrite and **must not be relaxed to accommodate it**.

If the rewrite cannot preserve them, that is a finding to report, not a number to
update.

## 4. Non-goals

- **No re-ordering of the ladder by demand.** Dependency is the ladder's job;
  demand weight is a property of the corpora. Build order is computed from both
  and is *code*, not data (decision 0011).
- **No parse-direction rungs on the ladder.** `unpunctuated-input` sits on all 68
  player lines and no rung, because every ladder text is well-formed prose. The
  ladder is a production instrument; parse-robustness is a different axis, and
  the honest ceiling for a ladder/corpus cross-check is **147 of 149**, not 149.
- **No realization witness for 353 entries.** The Mortise's witness pins a
  committed `Clause` and its realized surface per covered entry. That does not
  scale, and pretending otherwise would produce a witness nobody maintains.
  Witness policy at this scale is its own question and is deferred with this
  sentence as its record.
- **No freeze of the ladder.** Freezing is the project owner's act, after review.
- **No edits to either frozen corpus.** Two possible mis-annotations are recorded
  in `.superpowers/sdd/ladder-revision-report.md`; both are inherited coarseness
  rather than error.

## 5. Success criteria (preregistered)

1. One internal entry type; two readers; no corpus file carries both shapes.
2. The ladder's demands are **derived**, and a rung's set is the transitive
   closure of its presuppositions — verified by a test that would fail if the
   closure were shallow (one level) rather than transitive.
3. **`the-merchant` still reads 5 of 12, covered set `m05 m06 m07 m09 m10`**, via
   the existing unmodified assertions.
4. Coverage reports **per direction**: parse, produce, and direction-unknown,
   with the merchant corpus in the third.
5. Direction is never inferred from `speaker` or from any other field.
6. The ladder gets **STRUCTURAL** assertions only: acyclic, ids unique, no token
   introduced twice, cumulative closure computable for every rung, exactly two
   roots. These hold at any size and freeze nothing.

   **It does NOT get a count assertion.** A frozen entry count is precisely the
   freeze mechanism (`MERCHANT_ENTRIES`), and §4 reserves freezing to the project
   owner. An earlier draft of this spec asked for both and contradicted itself;
   the structural/count split is the resolution. Adding `LADDER_ENTRIES` is a
   one-line act the owner performs after review, and it is the moment the ladder
   stops being a draft and its ids become append-only forever.
7. The cross-check is computable: how many of a corpus's tokens exist on the
   ladder, and which do not. Its expected value for `the-flood-watch` is
   **147 of 149**, with `unpunctuated-input` and `contraction-elision` refused
   for a stated reason rather than absent.

**A falsified prediction is a finding.** If criterion 3 cannot hold, the campaign
reports that rather than re-baselining.

## 6. Decisions expected

From 0386–0395:

- a corpus declares its demands or derives them, never both (§2.1)
- coverage is reported per direction, and an absent direction is unknown rather
  than inferred (§2.2, §2.3)
- the ladder is a production instrument; parse-robustness is a separate axis and
  the cross-check ceiling is 147 of 149 (§4)
