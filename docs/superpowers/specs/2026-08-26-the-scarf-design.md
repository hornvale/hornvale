# The Scarf — one clause, two realizers

*A scarf joint cuts two members on a long taper and laps them so the join
carries load as a single piece. It is the joint you use when you have two
timbers and need one beam.*

**Status**: spec, awaiting G3 review
**Campaign**: The Scarf
**Decision block**: 0286–0295 (reserved; main ceiling 0266)
**Predecessor**: The Interlinear (decision 0266, `book/src/chronicle/the-interlinear.md`)

---

## 1. The problem, stated as a function that should not exist

The Interlinear made `ClauseSpec` fact-shaped and demoted Common to one
realizer among the tongues. It stopped one step short, and recorded the stop
in its own chronicle: *"Common is not yet a peer in full. `TongueClause` still
takes a different shape from `ClauseSpec`."*

The cost of that stop is visible as a concrete function,
`windows/almanac/tests/suite/interlinear.rs:238`:

```rust
fn tongue_view(spec: &ClauseSpec) -> TongueClause {
    TongueClause {
        subject: match &spec.subject {
            Subject::Name(name) => name.clone(),
            Subject::Pronoun(pronoun) => (*pronoun).to_string(),
        },
        complement_concept: match &spec.object {
            Argument::Concept(id) => id.clone(),
            other => panic!("the interlinear's clause predicates a concept, not {other:?}"),
        },
        evidential: Evidential::Witnessed,
        adjuncts: spec.adjuncts.clone(),
    }
}
```

Four lossy moves in twelve lines:

1. **`Subject::Pronoun` is stringified into the tongue.** An English pronoun
   crosses into tongue output — the exact leak The Interlinear existed to close,
   surviving in the helper that demonstrates The Interlinear.
2. **A non-concept object panics.** The tongue cannot predicate a name, a count
   or a quantity.
3. **`evidential` is invented out of band.** `ClauseSpec` has no such field, so
   the projection supplies a value the caller never stated.
4. **`number` and `definiteness` are dropped silently.** A tongue is never told
   its subject is plural.

And the function lives in a **test file**, so `domains/language` has no seam at
which its own lossiness could be tested, or even seen.

## 2. What this campaign delivers

One clause type. `TongueClause` is deleted; `ClauseSpec` absorbs its one unique
field and is renamed **`Clause`**. Both realizers take `&Clause`.

`tongue_view` is deleted with it. That deletion is the campaign's product — not
a tidy-up alongside it.

## 3. Invariants

These are laws the implementation must not "improve".

### 3.1 The input is symmetric; the output is anti-symmetric

Both realizers take the same `&Clause`. They do **not** return the same type,
and must not be made to:

```
realize_common(&Clause, &CommonVocabulary) -> String                      // total
realize_tongue(&Clause, ...)               -> Result<String, TongueGap>   // partial
realize_tongue_deep(&Clause, ...)          -> Result<String, TongueGap>   // partial
```

`realize_common`'s existing doc states the reason and it is load-bearing:
Common is the author's register and `CommonVocabulary::word_for` is total, so
a gap can only ever arise on the tongue side — which is what makes a gap mean
*something true about the world* (this people has no word for the sea) rather
than an authoring hole. Introducing a `CommonGap` would destroy that reading.

### 3.2 Each realizer ignores some features of the shared clause

After the collapse `Clause` carries features neither realizer reads in full:

```
+------------------+---------------------+---------------------+
| feature          | Common              | a tongue            |
+------------------+---------------------+---------------------+
| predicate        | construction table  | asserted == IS_A    |
| subject          | Name | Pronoun      | Name; Pronoun GAPS  |
| object           | all 4 Arguments     | all 4 Arguments     |
| number           | copula + plural     | IGNORED (today)     |
| definiteness     | determiner slot     | IGNORED (always)    |
| evidential       | IGNORED (today)     | predicate-final mark|
| adjuncts         | per-role surface    | argument-order only |
+------------------+---------------------+---------------------+
```

**This is the law, not a defect list.** Read as two separate gaps — "Common
can't do evidential", "the tongue can't do number" — it invites the plausible
wrong repair of teaching a tongue to render English number. Read as a pair it
is the correct shape: a language-neutral clause states more than any one
language surfaces, and each realizer surfaces what its own grammar has.

Two of the `IGNORED (today)` cells are marked so deliberately. `number` on the
tongue side is precisely what `paradigm.rs`'s already-drawn `number_depth` and
`draw_paradigm_affix_proto` consume, and wiring them is the next campaign. This
campaign makes `number` **reach** the tongue realizer and stop there.

### 3.3 A gap means the tongue lacks a word, never that the caller erred

Consequence for two new refusal sites:

- **Unknown predicate** → `panic!`, matching `realize_common`. A missing
  construction is an authoring hole; returning `TongueGap` would assert
  something false about the world.
- **`Subject::Pronoun`** → `TongueGap`, and its `reason` must say plainly that
  the tongue has no pronoun inventory, not imply the people cannot re-mention.

## 4. Design decisions

### 4.1 `evidential` moves onto `Clause`; Common ignores it

Additive. `Clause` derives no `Serialize`, so **no save-format contract is
touched and no stream label is drawn**. The Interlinear's spec had promised
`ClauseSpec` would carry five speaker features and shipped two; this takes it
to three. Polarity and mood are still added when a role needs them, never
speculatively.

### 4.2 One `resolve_argument`, shared by the object slot and the adjunct slot

`realize_adjuncts` already resolves all four `Argument` variants — `Concept`
through the lexicon, `Name` verbatim, `Count`/`Quantity` as digits. The object
slot resolves only `Concept`.

That asymmetry is not a design. It is the two struct definitions drifting: a
tongue can today say a bare numeral **in an adjunct** and cannot say the same
numeral **in the object slot**. Extracting `resolve_argument` and pointing both
slots at it removes the inconsistency and deletes `tongue_view`'s panic rather
than relocating it.

### 4.3 A non-lexical object bears no morphology

`layer_affix` panics on a word whose segments are unknown, and only
`LexEntry::Root` carries segments. Widening the object slot therefore raises the
question of what a `Count` object does under `MorphDepth::Affix`.

**Decision: a `Name`/`Count`/`Quantity` object realizes bare and bears no
noun-class mark, and this is not the silent degradation the panic exists to
refuse.**

The evidential needs its own sentence, because it does not attach to the object
in the general case. It marks predicate-finally — the overt copula, or, under a
zero copula, the predicate nominal. So:

- **Copula present** — the evidential attaches to the copula and a non-lexical
  object changes nothing.
- **Zero copula AND a non-lexical object** — there is no nominal to bear it, and
  the clause goes unmarked for evidentiality.

Only the second case is new, and it is the same shape as a tongue that drew
`MorphDepth::None`: a clause that states an evidential the grammar has nowhere
to put. The two segment-less cases differ in kind:

- A `Compound`'s missing segments are a **lexicon bug**. `grammar.rs:308` says
  so outright: "close the lexicon gap before Affix-marking a Compound."
  Degrading silently there would hide a fixable defect.
- A numeral has no segments **by nature**. There is nothing to close, so
  declining to affix hides nothing.

Consistency check, and it is what makes this a discovery rather than a
judgement call: `realize_adjuncts` already renders `Name`/`Count`/`Quantity`
unmarked. This makes the object slot **agree with the adjunct slot** rather
than inventing a rule for it.

**This is the spec's lowest-confidence decision** — no precedent either way, and
no live caller exercises it. Flagged at G3.

### 4.4 `ClauseSpec` is renamed `Clause`

One word per concept. The `Spec` suffix existed only to distinguish it from
`TongueClause`; once there is one clause type the suffix names nothing. 41
occurrences across 6 `.rs` files (`domains/language/src/{clause,grammar,lib}.rs`,
`windows/book/src/lib.rs`, `windows/almanac/tests/suite/interlinear.rs`,
`cli/tests/suite/sentence_corpus.rs`).

**Historical documents are not rewritten.** `docs/superpowers/specs/`,
`docs/superpowers/plans/`, `docs/retrospectives/` and `docs/decisions/` record
what was true when written and stay as they are. Only live book chapters
(`book/src/domains/language.md`, `book/src/frontier/idea-registry.md`) are swept.

### 4.5 The Interlinear's chronicle carries a false claim, and this campaign corrects it

`book/src/chronicle/the-interlinear.md`, "The honest limits", states that a
tongue's role surfaces are "adpositions and case morphology, which `paradigm.rs`
already draws and nothing realizes; that is the next campaign's work."

Verified false:

```
$ grep -rniE 'adposition|postposition|preposition' domains/language/src/
domains/language/src/clause.rs:63:/// argument. How it surfaces — a preposition, a case affix, a trailing
```

One hit, and it is a doc comment. `typology.rs` has no case machinery either.
What `paradigm.rs` draws is **Number and Tense** — depths, attachment sides,
family affix protos, paradigm cells — and:

```
$ grep -rn 'paradigm_depths\|ParadigmDepths\|draw_paradigm_affix_proto\|ParadigmCell' --include=*.rs .
(no hits outside domains/language/src/paradigm.rs)
```

Zero consumers. The unrealized machinery is real; it is not the machinery the
chronicle names. Left standing, the sentence sends the next campaign to build
role-marking believing the morphology is already drawn, when in fact that work
requires new permanent stream labels — a save-format contract the chronicle's
framing hides. The DoD book sweep corrects it and says what is actually drawn.

## 5. Non-goals

- **No role-marking.** `realize_adjuncts` still matches on the argument and
  ignores `Adjunct.role`. The flagship line keeps its shape — a name, two
  lexicalized words, then three undifferentiated numerals.

  **The numerals in that line are NOT what the chronicle records, and this
  campaign did not move them.** The Interlinear's chronicle says
  `Nwamvam Qoqe Bae 8835 25 375`; the live value is
  `Nwamvam Qoqe Bae 18822 25 1600`. Verified as upstream world drift, not ours,
  three ways: the numerals are `Argument::Count` adjuncts read straight off the
  ledger (`occupation.site`, `.founded`, `.ended`); the **Common** line — through
  `realize_common`, whose rendering this campaign does not touch — carries the
  same new numbers; and the chronicle's own file was last written 2026-08-25
  while `f633df521`, the merge of 202 commits of main into that branch, is dated
  2026-08-26. The demonstration was authored, then main was absorbed, and the
  line was never re-run. **Nothing could have caught it**: `git grep 8835 -- '*.rs'`
  finds only synthetic test literals — no test asserts the flagship numerals at
  all. §4.5's chronicle correction covers this alongside the `paradigm.rs` claim.
- **No tense, polarity or mood.** Wiring `paradigm.rs` is the next campaign.
- **No pronoun inventories.** A tongue gaps on `Subject::Pronoun`.
- **No `parse_tongue`.** The collapse makes it namable; it does not build it.
- **No tongue construction table.** The tongue asserts `predicate == IS_A`.
- **No generation change.** Nothing here touches a draw, a stream, or a seed.

## 6. Success criteria

1. `git grep -c TongueClause -- '*.rs'` returns nothing.
2. `tongue_view` no longer exists in `windows/almanac/tests/suite/interlinear.rs`;
   the flagship test realizes **one `Clause`** through both realizers.
3. A test shows a tongue realizing an `Argument::Count` **object** — impossible
   before this campaign, and already possible in an adjunct.
4. A test shows `Subject::Pronoun` producing a `TongueGap` whose reason names the
   missing pronoun inventory.
5. The shallow-identity guarantee still holds: `realize_tongue_deep` with no
   marking equals `realize_tongue`.
6. Artifact drift resolves per the branch table in §7.

## 7. Artifact branch table

Written as branches rather than a prediction, because both live callers
(`windows/book/src/lib.rs:389`, `:1883`) pass a `Name` subject, a `Concept`
object, an explicit evidential and `adjuncts: Vec::new()` — so behaviour
*should* be preserved, and "should" is exactly the claim that needs a decision
rule instead of a forecast.

```
after `make rebaseline`, per path:

  book/src/gallery/ UNMOVED    -> behaviour preserved, as designed. Proceed.
  book/src/gallery/ MOVED      -> STOP. Diagnose before anything else.
                                  Both live callers are the trivial shape, so a
                                  move means §4.2 or the retype changed assembly
                                  ORDER. Do not rebaseline; find the reordering.
  docs/audits/ MOVED           -> EXPECTED. The pub boundary changed (a deleted
                                  type, a renamed type). Regenerate the
                                  type-audit report in the SAME commit.
  docs/digest/ MOVED           -> EXPECTED if this campaign mints a decision
                                  record. Regenerate in the same commit.
  cli/tests/fixtures/ MOVED    -> STOP. A byte-golden must not move; this
                                  campaign touches no generation. A move here
                                  means something reached the draw path.
  book/src/reference/ MOVED    -> STOP. Registry/manifest dumps should be inert
                                  to a type rename. Investigate.
```

`PATH=$HOME/.deno/bin:$PATH make rebaseline` — without it the atlas bundle is
skipped with a single stderr line and rc=0 (The Escapement's finding).

## 8. Stages

| # | Stage | Deliverable |
|---|-------|-------------|
| 1 | Widen the clause | `evidential` onto `ClauseSpec`; `resolve_argument` extracted and shared by both slots; §4.3's no-morphology rule. `TongueClause` still exists. |
| 2 | Retype the realizers | All three realizers take `&ClauseSpec`. Pronoun→gap, unknown-predicate→panic. Both `windows/book` sites and all 9 `grammar.rs` construction sites moved. |
| 3 | Cut the old timber | `TongueClause` and `tongue_view` deleted. The flagship test realizes one clause twice. Success criteria 1–5 green. |
| 4 | Rename and sweep | `ClauseSpec` → `Clause`. Live book chapters swept; the-interlinear chronicle corrected per §4.5. Chronicle, retrospective, artifacts, §7 branch table. |

Stage 3 is the only irreversible one and the only one that produces the
deliverable. Stages 1 and 2 are additive and independently green.

## 9. Open question carried, not answered

`parse_common` exists and has zero production callers **by design** — it is the
"give me a clause, not English" front door, and The Interlinear's reviewer
recommended keeping it. Its doc comment explains the adjunct loss but never says
it is caller-less, so a dead-code sweep sees an undefended `pub fn`.

Stage 4 adds that line to the doc comment. This retires a carry that currently
has no committed home anywhere in the repo.
