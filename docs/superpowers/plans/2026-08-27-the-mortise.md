# The Mortise — clause-in-clause connectivity: Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give `Clause` a field that points at another `Clause`, add coordination
above it, and make the merchant corpus score demonstrate rather than declare —
taking the score from 2 of 12 to 5 of 12.

**Architecture:** Two operators, deliberately named apart. **Embedding** is a
slot that holds a clause: new `Argument::Clause` / `Subject::Clause` variants,
riding the existing `Valence::Transitive` frame with no new valence.
**Coordination** is a list at a node: an additive type *above* `Clause`, so no
existing caller or construction site moves. Both are marked at a boundary
(complementizer, conjunction), both boundary markers are **drawn per tongue**,
and both markers are what keep the parser's inverse computable.

**Tech Stack:** Rust 2024, `domains/language` (kernel-only deps), `windows/book`,
`cli/tests/suite`. No new external crates — the allowlist is `serde`,
`serde_json`, `libm` (decisions 0004/0041).

**Spec:** `docs/superpowers/specs/2026-08-27-the-mortise-design.md` — read it
before Task 1. Section 4 is the design, 6 the preregistered criteria, 9 what is
cut and why.

---

## YOU OUTRANK THIS PLAN

**This is the mechanism that caught the most defects in the two campaigns before
this one.** Across The Scarf and The Inquest, **every defect found originated in
the controlling session's plan or spec text — twelve in one, sixteen in the other
— and none in implementer code.** None was found by re-reading; every one died to
somebody running a command.

So:

- **If the plan contradicts the code, the code wins.** Say so in your report.
- **If a step asserts something about the world** — "this compiles", "the diff
  will be empty", "there are N sites" — **check it before acting on it.** An
  imperative hides an assertion; a step that reads as a thing to *do* has a claim
  inside it that nobody audited.
- **If a step prescribes a specific mutation or probe, treat it as a starting
  point, not an instruction.** The plan author is outside the code; you are
  inside it. Both prescribed mutations in a previous campaign were nulls, and the
  implementer found a discriminating one by hunting, both times.
- **Refusing is a good outcome.** The best results of the last campaign were
  refusals: a destructive `git checkout`, a near-vacuous check billed in capitals
  as the campaign's most important, and a branch table that named one cause when
  there were two.

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, enforced
  workspace-wide by `clippy.toml` `disallowed-types`.
- **No wall-clock time.** Enforced the same way.
- **No new external dependencies.** Allowlist: `serde`, `serde_json`, `libm`.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field and
  variant gets a one-line doc comment. A new enum variant needs one.
- **Rust edition 2024.** Run `cargo fmt` as the final step before every commit;
  fmt-gate skips are this repo's most common review finding.
- **Typed quantities:** every primitive at a `pub` boundary carries a
  `type-audit:` verdict tag. Adding a `pub` item means the committed report at
  `docs/audits/type-audit-report.md` must be regenerated **in the same commit**.
- **`cli/tests/suite/lexicon_guard.rs` counts the word "cell" in doc-comment
  prose**, and linguistics jargon collides with this repo's place vocabulary (a
  cell is a mesh vertex). **Write "row", never "cell".** Four Inquest tasks met
  this guard.
- **Stream labels are permanent save-format contracts.** This campaign adds
  exactly two. Never rename one; a regeneration uses an epoch suffix.
- **`EPOCH_COHORTS` is append-only.** Add a new entry at the END. Never edit an
  existing cohort — epoch-first sorting makes a later concept land strictly last,
  displacing nothing. Before that discipline, `otyugh-kind` alone moved 65 facts.
- **The commit gate is `make gate-commit`.** Run it before every commit.
- **A test rename is a commit-gate change.** `docs/timings/subfloor-roster.tsv`
  selects tests by EXACT name. Renaming a test without editing that file silently
  drops it from the gate while everything stays green. Task 9 renames three.

---

## File Structure

| File | Responsibility | Tasks |
|---|---|---|
| `domains/language/src/clause.rs` | `Clause`, `Argument`, `Subject`, `Part`, `PREDICATE_VALENCE`, `common_constructions`, `realize_common`, `parse_common_with_tail` | 1, 2, 3, 4, 6, 7, 8 |
| `domains/language/src/grammar.rs` | `TongueGrammar`, `tongue_grammar` draws, `realize_tongue`, `realize_tongue_deep`, `resolve_argument`, `tongue_subject` | 4, 5, 6, 7 |
| `domains/language/src/packs.rs` | concept ids as constants; `PackEntry` registration | 1, 2 |
| `domains/language/src/accession.rs` | `EPOCH_COHORTS`, append-only | 2 |
| `domains/language/src/streams.rs` | static `StreamLabel` constants | 5, 6 |
| `domains/language/src/lib.rs` | the `stream_labels()` roster | 5, 6 |
| `cli/tests/suite/sentence_corpus.rs` | the resolver, the score, the distance report, and the new witness | 1, 9 |
| `docs/timings/subfloor-roster.tsv` | exact-name test selection for the commit gate | 1, 9 |
| `windows/book/src/lib.rs` | the production-caller candidate | 10 |

**No new source file.** The witness lives in `sentence_corpus.rs` beside the
resolver it proves, because a witness in a separate file would be a second place
the covered-set fact lives.

---

## Task 1: The realization witness, and the red it finds

The instrument comes first, so that every task after this one has its coverage
claim checked by something that runs rather than by a list someone edited.

**Files:**
- Modify: `cli/tests/suite/sentence_corpus.rs`
- Modify: `domains/language/src/packs.rs` (a `KNOW` constant)
- Modify: `domains/language/src/clause.rs` (`PREDICATE_VALENCE`)
- Modify: `docs/timings/subfloor-roster.tsv`

**Interfaces:**
- Consumes: `hornvale_language::{Clause, realize_common, CommonVocabulary}` and
  the existing `load_merchant_corpus` / `entry_covered` in this file.
- Produces: `MERCHANT_WITNESS: &[(&str, &str)]` — corpus id paired with the
  Common surface the grammar actually produces; Task 9 extends it. And
  `hornvale_language::packs::KNOW: &str`.

**Background.** `sentence_corpus.rs`'s module doc says `IMPLEMENTED_DEMANDS` is
"a hand-maintained declaration and nothing mechanically proves it". This task
supplies the proof. It must **not** assert equality with the corpus's literal
English — Common is a deliberately limited register, so that assertion would fail
for reasons unrelated to grammar and the guard would teach nothing. It asserts a
covered entry has a constructible clause that realizes **without panicking**, and
records the surface so the distance is legible.

- [ ] **Step 1: Read the ground truth before writing anything**

```bash
sed -n '150,215p' cli/tests/suite/sentence_corpus.rs
sed -n '575,585p' domains/language/src/clause.rs
sed -n '755,770p' domains/language/src/clause.rs
grep -n '"know"' domains/language/src/packs.rs
```

Confirm three things this task depends on: `MERCHANT_COVERED_IDS` is
`["m05","m10"]`; `PREDICATE_VALENCE` holds exactly `is-a`, `eat`, `kill`; `know`
is a registered concept appearing in no valence row. **If any is false, stop and
report** — the task's red depends on all three.

- [ ] **Step 2: Write the witness, expecting m10 to fail**

m05 is *"A guard killed a woman"*; m10 is *"I didn't know her"*. Build each from
the `Clause` struct's CURRENT field list — read it, do not copy one from memory.

```rust
/// The Common surface each covered entry ACTUALLY realizes, beside the
/// corpus's own English. Not an equality assertion against the corpus:
/// Common is a limited register, so a witness demanding the corpus's literal
/// text could never pass. What it proves is that a covered entry has a
/// constructible clause that realizes at all — the mechanical half
/// `IMPLEMENTED_DEMANDS` has never had.
const MERCHANT_WITNESS: &[(&str, &str)] = &[
    ("m05", "<paste what the run actually produced>"),
    ("m10", "<paste what the run actually produced>"),
];

/// Every covered entry realizes. **This is the guard the module doc says does
/// not exist**, and it went red the first time it ran: m10 was scored covered
/// while `realize_common` panicked on `know`, which had no
/// `PREDICATE_VALENCE` row. A guard that has never been red proves nothing
/// about what it catches.
#[test]
fn every_covered_entry_realizes_in_common() {
    // one clause per covered id; realize each; compare against MERCHANT_WITNESS
}
```

**The surfaces are not predictions.** Run the realizer, read what it produced,
paste that. Never write the expectation and then bend the code to it.

- [ ] **Step 3: Run it and capture the RED**

Run only this test and tee the output; grep the file afterwards rather than
re-running:

```bash
cargo nextest run -p hornvale --test suite -E 'test(every_covered_entry_realizes_in_common)' > /tmp/mortise-t1.log 2>&1; echo "exit=$?"
grep -E 'panicked|FAILED|test result' /tmp/mortise-t1.log
```

Expected: **FAIL by panic**, `Common has no construction for predicate "know"`.
Record the exact message in your report — it is this campaign's evidence that the
witness detects anything at all.

**If it does NOT fail, stop and report.** A witness green on arrival has not
demonstrated it can fail, and building the instrument first was the whole point.

- [ ] **Step 4: Name the concept, then add the row**

`packs.rs` names `EAT` and `KILL` as constants precisely so registration and the
valence row cannot drift. `know` is a bare `"know"` literal at `packs.rs:670`.
Read `KILL`'s constant and doc at `packs.rs:37-48`, follow that style, use the
constant at the registration site, then:

```rust
// domains/language/src/clause.rs — PREDICATE_VALENCE
(KNOW, Valence::Transitive),
```

**`know` gets NO new epoch cohort.** It is already registered
(`accession.rs:442`, the action suite). Adding a cohort would re-sort concepts
and move goldens for nothing.

- [ ] **Step 5: Re-run — GREEN — and record the real surface**

```bash
cargo nextest run -p hornvale --test suite -E 'test(every_covered_entry_realizes_in_common)' > /tmp/mortise-t1b.log 2>&1; echo "exit=$?"
grep -E 'FAILED|test result|left|right' /tmp/mortise-t1b.log
```

If the surface differs from your Step 2 guess, **the run is right**. Update the
constant and note the difference in your report.

- [ ] **Step 6: Put the new test in the commit gate**

Add to `docs/timings/subfloor-roster.tsv`, in the file's existing position among
the other `sentence_corpus::` rows (they sit around lines 3411-3420 — read the
surrounding format before inserting):

```
hornvale::suite$sentence_corpus::every_covered_entry_realizes_in_common
```

- [ ] **Step 7: Gate and commit**

```bash
cargo fmt
make gate-commit
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git add -A && git commit -m "test(the-mortise): the corpus score gets a witness, and it finds know"
```

The report regeneration is **not optional** — you added a `pub const`, so the
committed report drifts. Regenerate here or the drift check fails in a later task
that did not cause it.

---

## Task 2: `think`, epoch 14, and the byte-goldens

**Files:**
- Modify: `domains/language/src/packs.rs`
- Modify: `domains/language/src/accession.rs` (append to `EPOCH_COHORTS`)
- Modify: `domains/language/src/clause.rs` (`PREDICATE_VALENCE`)
- Regenerate: whatever `make rebaseline` moves

**Interfaces:**
- Consumes: `packs::KNOW` from Task 1.
- Produces: `packs::THINK: &str`, and `(THINK, Valence::Transitive)`. Tasks 3
  and 9 use `THINK` as the hedge predicate.

**Background.** `think` is the epistemic-hedge predicate for m09, *"I think her
name was Gilda"*. Unlike `know`, it is registered nowhere.

- [ ] **Step 1: Confirm `think` is genuinely absent**

```bash
grep -rn '"think"' --include=*.rs domains/ windows/ cli/
grep -n 'think' book/src/reference/concept-registry-generated.md
```

Expected: no hits. If it exists somewhere, **stop** — the cohort below would be
wrong.

- [ ] **Step 2: Write the failing test first**

Read the existing transitive test and follow its shape:

```bash
grep -n 'fn a_transitive_clause_surfaces_its_predicate_as_a_verb' -A 30 domains/language/src/clause.rs
```

```rust
/// `think` is the epistemic-hedge predicate (m09). Transitive by the same
/// argument `know` is: one argument structure with a category-flexible
/// object, so it adds a ROW and no `Valence` variant.
#[test]
fn a_hedge_clause_surfaces_think_as_a_verb() {
    // build a Clause with predicate THINK; assert the realized surface
}
```

- [ ] **Step 3: Run it — expect the missing-construction panic**

```bash
cargo nextest run -p hornvale-language -E 'test(a_hedge_clause_surfaces_think_as_a_verb)' > /tmp/mortise-t2.log 2>&1; echo "exit=$?"
grep -E 'panicked|test result' /tmp/mortise-t2.log
```

- [ ] **Step 4: Register the concept, append the cohort, add the row**

**CONTROLLER RULING (pre-dispatch verification): `think` goes in
`universal_stratum()`, NOT beside `know`.** An earlier draft of this task said
"the universal stratum beside `know`'s pack", which is incoherent — `know` is not
in the universal stratum and is not even a `PackEntry`. It lives in
`action_suite_pack()` (`packs.rs:671`), which is `&[(&str, &str)]`, registered
directly by `register_concepts`, and whose seven concepts each produce an honest
`Void::Gap` in every lexicon.

The choice, and why:

- **`kill`'s stated reasoning applies to `think` and not to `know`'s placement.**
  `kill` is in the universal stratum because "there is no biome, climate or
  perception ladder a people's word for killing could hang off, so gating it
  would be authoring a silence rather than deriving one." Nothing gates thinking
  either.
- **`action_suite_pack`'s own doc flags its placement as UNRESOLVED**, not
  principled: "a culture's exposure to literacy, cartography, or reading
  another's state is a question this task does not resolve". Following it would
  be inheriting an acknowledged open question as if it were a decision. It is
  registry row `LANG-in-character-acts-are-unspeakable`.

**The resulting asymmetry is real, and you must NAME it rather than smooth it
over.** `think` will be sayable in every tongue; `know` will still gap in all of
them. So m09 (*"I think…"*) realizes in a tongue and m06 (*"I don't know…"*) does
not. **Do not "fix" `know` by moving it** — that is The Deed's decision, it would
move exposure and goldens, and it is out of scope. Record the asymmetry in your
report; it feeds `LANG-in-character-acts-are-unspeakable` and the chronicle.


Read `packs.rs:37-48` and `accession.rs`'s epoch-13 comment before writing — a
cohort comment is expected to explain *why this concept sorts where it does*, not
merely that it was added.

```rust
// packs.rs
pub const THINK: &str = "think";

// packs.rs — a PackEntry in `universal_stratum()` (packs.rs:89), beside
// `kill`. See the CONTROLLER RULING below for why there and not beside
// `know`. `kind: ConceptKind::Act` (kernel/src/registry.rs:43 — an act a
// creature performs, the verb side of the vocabulary). Choose `ladder_rank`
// from what the universal stratum's own doc says about unranked entries
// (packs.rs:77-79), not by copying a neighbour.

// accession.rs — a NEW entry appended to the END of EPOCH_COHORTS:
&["think"],

// clause.rs — PREDICATE_VALENCE
(THINK, Valence::Transitive),
```

- [ ] **Step 5: Run the test — GREEN**

```bash
cargo nextest run -p hornvale-language -E 'test(a_hedge_clause_surfaces_think_as_a_verb)' > /tmp/mortise-t2b.log 2>&1; echo "exit=$?"
grep -E 'FAILED|test result' /tmp/mortise-t2b.log
```

- [ ] **Step 6: Regenerate, then CLASSIFY the diff — do not simply accept it**

```bash
make rebaseline
make rebaseline-goldens
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
git diff --stat   # byte-goldens are NOT in generated-paths.txt — see below
```

**BOTH commands, and this is a controller correction.** `make rebaseline` does
not touch byte-golden fixtures; they have their own accept path
(`make rebaseline-goldens`, i.e. `REBASELINE=1`). Task 2 ran only the first and
left three byte-goldens red on the branch — `lens_purity`,
`repose_byte_identity` and `solitary_tongue::peoples_lexicons_...` — which
nothing in `gate-commit` runs, so the branch looked green for two whole tasks.
`git diff --exit-code` over `docs/generated-paths.txt` cannot see them either.
Run both, and diff the WHOLE tree, not only the declared paths.

**A decision rule, not a prediction:**

| what moved | what it means | do |
|---|---|---|
| only ADDED rows naming `think` | benign registration | commit it here |
| an existing VALUE moved (a word, a name, a fact's content) | **draw perturbation** — the cohort append did not do its job | **STOP and report** |
| a byte-golden fixture moved | classify by the rule above before accepting | inspect first |

**The discriminator is whether an existing VALUE moved** — not whether the file
changed, and not whether lines were deleted. The corrected table is in
`docs/superpowers/specs/2026-08-26-the-inquest-design.md` section 8; read it
before deciding.

- [ ] **Step 7: Gate and commit**

```bash
cargo fmt && make gate-commit
git add -A && git commit -m "feat(the-mortise): think, the hedge predicate, at epoch 14"
```

---

## Task 3: `Argument::Clause` — Common says an embedded sentence

**Files:**
- Modify: `domains/language/src/clause.rs` (`Argument`, `realize_common`, a depth
  constant)
- Modify: `domains/language/src/grammar.rs` (`resolve_argument` — the compiler
  points at it)

**Interfaces:**
- Consumes: `packs::{KNOW, THINK}` from Tasks 1-2.
- Produces: `Argument::Clause(Box<Clause>)`, and a depth constant. **State the
  constant's exact name in your report** — Tasks 4 and 8 both use it.

**Background.** Spec section 4.2: this rides `Valence::Transitive`. **Do not add
a `Valence` variant.** `clause.rs:544-552` states the tripwire — a variant per
predicate rebuilds the `Frame` enum The Interlinear deleted.

- [ ] **Step 1: Write the failing test**

```rust
/// A clause complement rides the transitive frame, and the determiner is
/// suppressed before it: *"I do not know a he killed her"* is what failing to
/// suppress produces.
#[test]
fn a_clause_object_realizes_with_no_determiner() {
    // matrix: Subject::Pronoun(1sg), predicate KNOW, polarity Neg, tense Past
    // object: Argument::Clause(Box::new(<a transitive clause>))
    // assert: no determiner word immediately precedes the embedded clause
}
```

- [ ] **Step 2: Run it — expect a COMPILE failure**

`Argument` has no `Clause` variant yet. A compile failure is the right red here;
do not manufacture a behavioural red for a variant that does not exist.

- [ ] **Step 3: Add the variant with its doc comment**

```rust
/// An embedded clause, realized in place of a lexical complement.
///
/// **Added because a role needed it, which is this enum's own stated rule.**
/// The role is the clause complement of `know`/`think` — *"I do not know he
/// killed her"* — which no other variant can carry.
///
/// Depth is capped: the cap states the depth this campaign builds and can
/// show working, not a stack-safety belt. `Box` is unique ownership with no
/// `Rc`, so a clause graph cannot cycle; only depth is unbounded without it.
Clause(Box<Clause>),
```

- [ ] **Step 4: Let the compiler enumerate the sites**

```bash
cargo check --workspace --all-targets > /tmp/mortise-t3.log 2>&1; echo "exit=$?"
grep -c '^error' /tmp/mortise-t3.log
```

**CONTROLLER RULING (pre-flight): Task 5 owns ALL tongue-side realization.**
The compiler will force an arm in `resolve_argument` (`grammar.rs:203`) — and in
Task 4, `tongue_subject` (`grammar.rs:316`) — before any subordination strategy
exists to realize a nested clause with. Write
`unimplemented!("clause embedding in a tongue arrives in Task 5")` in those arms.
**A panic, not a `TongueGap`**: a gap means "true about a people", and using one
for "not built yet" would be a lie in the type that could ship silently. Add NO
tongue-path test in this task; Task 5 removes the `unimplemented!` and success
criterion 3 fails if it survives.

**A `cargo check` that fails early has enumerated NOTHING** — its error list is a
FLOOR, not a census. Size the real work independently and reconcile the two
before believing you have seen every site.

**CONTROLLER FINDING (pre-dispatch verification): the compiler will flag THREE
sites and MISS at least two, and the missed ones are where the damage is.**
Measured by grepping a rare variant (`Argument::Quantity`) rather than by
reading the enum's users:

*Compiler-enforced (exhaustive matches):*

| site | what it is |
|---|---|
| `grammar.rs:210-214` | `resolve_argument` — gets ruling 1's `unimplemented!` |
| `clause.rs:779` | `realize_common`'s complement match — the real work |
| `clause.rs:~929-945` | `common_role_surface` — the ADJUNCT path, matched as `(role, argument)` tuples |

*NOT compiler-enforced — a catch-all swallows the new variant silently:*

| site | what happens without an explicit arm |
|---|---|
| `common_role_surface`'s trailing `_ => None` | an adjunct carrying a clause renders **nothing at all**, no error |
| `grammar.rs:620-627`, `realize_tongue_deep`'s `other =>` | a clause object falls through to `resolve_argument` |
| `realize_adjuncts` (`grammar.rs`) | calls `resolve_argument` per adjunct, so a clause adjunct hits ruling 1's panic and reports the WRONG reason ("arrives in Task 5" rather than "adjuncts may not carry clauses") |

So spec 4.1's adjunct refusal **cannot be left to the compiler**. Write an
explicit arm ahead of `common_role_surface`'s `_ => None`, and refuse in
`realize_adjuncts` too so the message names the real rule. A silent `None` is
precisely the "capability ships without a decision" failure 4.1 exists to
prevent.

**Line numbers moved when Task 1 landed** — `Part::Subject` is now `clause.rs:789`
and `Part::Determiner` is `clause.rs:814`, not the 782/807 this plan says
elsewhere. Grep, do not trust the numbers.

- [ ] **Step 5: Handle every site, and REFUSE the adjunct case**

`realize_common` recurses for the complement; `Part::Determiner`
(`clause.rs:807-814`) emits nothing when the object is a clause.

**`Adjunct` holds an `Argument`, so adjuncts can now carry clauses. Spec section
4.1 REFUSES this** — adverbial subordination is a separate construction and must
not arrive as an unexamined side effect. Refuse explicitly (a panic, the same
class as the missing-construction panic: an authoring hole in this repository,
never a fact about a people), and write a test that the refusal fires.

- [ ] **Step 6: Cap the depth, and prove the cap fires**

One level: a clause complement may not itself contain a clause complement.
`realize_common` is infallible by design, so a violation panics — same class
again.

```rust
#[test]
#[should_panic(expected = "<the message you actually wrote>")]
fn a_clause_nested_two_deep_is_refused() { /* … */ }
```

- [ ] **Step 7: Run the crate, gate, commit**

```bash
cargo nextest run -p hornvale-language > /tmp/mortise-t3b.log 2>&1; echo "exit=$?"
grep -E 'FAILED|test result' /tmp/mortise-t3b.log
cargo fmt && make gate-commit
git add -A && git commit -m "feat(the-mortise): Argument::Clause, one level deep, no determiner"
```

---

## Task 4: `Subject::Clause` — embedding in subject position

**Files:**
- Modify: `domains/language/src/clause.rs` (`Subject`, `Part::Subject` at
  `clause.rs:782`)
- Modify: `domains/language/src/grammar.rs` (`tongue_subject` at
  `grammar.rs:316`)

**Interfaces:**
- Consumes: `Argument::Clause` and Task 3's depth constant.
- Produces: `Subject::Clause(Box<Clause>)`.

**Background.** Only **two** places consume a `Subject` structurally.
**Verify that before starting** — if the count has changed, the task is bigger
than it says:

```bash
grep -rn 'match .*subject\|Part::Subject =>' --include=*.rs domains/language/src/
```

The other ~75 `Subject::` references are construction sites, which a new variant
does not break.

- [ ] **Step 1: Write the failing test**

```rust
/// A clause in SUBJECT position — *"That he killed her confused me"*. The same
/// machinery the object slot uses, in a different hole. The gerund
/// (*"Seeing it"*) is a nominalization and is out of scope (spec 9.1).
#[test]
fn a_clause_subject_realizes_through_the_same_machinery() { /* … */ }
```

- [ ] **Step 2: Run it — compile failure, `Subject` has no `Clause` variant**

- [ ] **Step 3: Add the variant with its doc, handle both consumers**

**CONTROLLER RULING (pre-flight): Task 5 owns ALL tongue-side realization.**
The compiler will force an arm in `resolve_argument` (`grammar.rs:203`) — and in
Task 4, `tongue_subject` (`grammar.rs:316`) — before any subordination strategy
exists to realize a nested clause with. Write
`unimplemented!("clause embedding in a tongue arrives in Task 5")` in those arms.
**A panic, not a `TongueGap`**: a gap means "true about a people", and using one
for "not built yet" would be a lie in the type that could ship silently. Add NO
tongue-path test in this task; Task 5 removes the `unimplemented!` and success
criterion 3 fails if it survives.


Task 3's depth cap applies here too — a clause subject counts against the same
one level. Say so in the doc comment.

- [ ] **Step 4: Run, gate, commit**

```bash
cargo nextest run -p hornvale-language > /tmp/mortise-t4.log 2>&1; echo "exit=$?"
grep -E 'FAILED|test result' /tmp/mortise-t4.log
cargo fmt && make gate-commit
git add -A && git commit -m "feat(the-mortise): Subject::Clause, the same slot in a different hole"
```

---

## Task 5: Subordination is drawn — the first stream label

**Files:**
- Modify: `domains/language/src/streams.rs` (a new static `StreamLabel`)
- Modify: `domains/language/src/grammar.rs` (`TongueGrammar`, `tongue_grammar`,
  `realize_tongue`, `realize_tongue_deep`)
- Modify: `domains/language/src/lib.rs` (`stream_labels()` roster, from line 1001)

**Interfaces:**
- Consumes: `Argument::Clause` / `Subject::Clause` from Tasks 3-4.
- Produces: a `TongueGrammar` field carrying the drawn subordination strategy,
  and a new `streams::` constant. **Name both in your report** — Task 6 patterns
  its conjunction axis on whatever you build here.

**Background.** `windows/worldgen` already draws constituent order, copula
presence and article-hood per people. A hardcoded complementizer would make every
tongue subordinate like English — the exact failure `realize_tongue` exists to
prevent. **Bare parataxis is a legitimate drawn value, not a degenerate one:** a
language with no subordinator is not a language that cannot subordinate.

Read the copula draw first — it is the pattern, and it draws presence AND form
from one stream:

```bash
sed -n '130,175p' domains/language/src/grammar.rs
sed -n '1090,1105p' domains/language/src/lib.rs
```

- [ ] **Step 1: Write two failing tests — one per drawn value**

The point of two is that a single test cannot tell "the axis is drawn" from "the
axis is hardcoded to the value I happened to test".

```rust
/// A tongue that drew a complementizer emits it at the embedded clause's
/// boundary.
#[test]
fn a_tongue_with_a_complementizer_marks_the_embedded_boundary() { /* … */ }

/// A tongue that drew parataxis emits NO marker, and that is a grammar, not
/// a gap: juxtaposition is how many real languages subordinate.
#[test]
fn a_paratactic_tongue_embeds_with_no_marker() { /* … */ }
```

Build the `TongueGrammar` by hand in these tests rather than reaching for a drawn
one, so the assertion is about the realizer, not about which value a seed happened
to produce.

**Two more tests, for the two spec sections nothing else asserts.** Both are
cheap, and without them 4.4 and 4.5 are prose with no witness:

```rust
/// Decision 0296: tense is stated, never derived. An inner clause's tense is
/// ABSOLUTE and caller-stated, exactly as the matrix's is — no realizer reads
/// one to adjust the other. A realizer that backshifted would be computing a
/// relation between two clauses' deictic centres, which 0296 forbids.
#[test]
fn an_inner_clause_tense_is_not_backshifted() {
    // matrix Past + inner Past realizes the inner as Past, not as a
    // pluperfect or any other shifted form
}

/// Spec 4.5: the matrix does not rewrite the inner clause's own grounding.
/// This is where a per-clause `evidential` first earns its keep — and it is a
/// TONGUE-only payoff, because The Scarf's law (0286) has Common ignoring
/// `evidential` entirely.
#[test]
fn an_inner_clause_keeps_its_own_evidential() {
    // matrix and inner carry DIFFERENT evidentials; assert the inner's
    // survives into the tongue surface
}
```

- [ ] **Step 2: Run both — compile failure, the field does not exist**

- [ ] **Step 3: Add the field, the draw, and the realizer arms**

The strategy is at least: a complementizer (a free boundary word, drawn from the
tongue's own phonology the way the copula is) and bare parataxis. Follow
`tongue_grammar`'s existing shape — one stream, presence and form together.

**The Scarf's law still holds:** each realizer ignores part of the clause. The
inner clause's `tense`, `evidential`, `number` and `polarity` are **its own** and
the matrix must not rewrite them (spec 4.4, 4.5). Decision 0296 forbids
backshifting: no realizer reads one clause's tense to adjust another's.

**Thread the input; never rebuild it.** The evidential arm once rebuilt the
copula from `grammar.copula`, and once tense affixed first that would have
silently discarded the tense join — no panic, no red. A nested clause is a THIRD
consumer of the same assembly. Whatever you add must take the assembled value,
not reconstruct it from source.

- [ ] **Step 4: Add the roster entry — and COUNT the paths, do not count the labels**

Add to `stream_labels()` in `lib.rs`, matching the surrounding format (see
`language/<species>/grammar/copula` at line 1096).

**`stream_labels()` has no completeness check**, so a draw added without its
roster entry ships a silently incomplete manifest and nothing goes red. The
Inquest hit exactly this: the plan named two labels for one axis and the truth
was three, the third reached through a dynamic leg no roster can see.

**Count the paths your draws actually create by reading the derive chain**, then
compare against what you added. Report both numbers.

- [ ] **Step 5: Regenerate the stream manifest**

```bash
make rebaseline
make rebaseline-goldens
git diff --stat   # the whole tree: byte-goldens are not in generated-paths.txt
```

Expected to move: the stream manifest, by ADDED rows. Apply Task 2's Step-6
decision table to anything else that moves.

- [ ] **Step 6: Run, gate, commit**

```bash
cargo nextest run -p hornvale-language > /tmp/mortise-t5.log 2>&1; echo "exit=$?"
grep -E 'FAILED|test result' /tmp/mortise-t5.log
cargo fmt && make gate-commit
git add -A && git commit -m "feat(the-mortise): subordination strategy is drawn, not assumed"
```

---

## Task 6: Coordination — the list node and the second stream label

**Files:**
- Modify: `domains/language/src/clause.rs` (the coordination type and its Common
  entry point)
- Modify: `domains/language/src/streams.rs`, `grammar.rs`, `lib.rs` (the
  conjunction axis)

**Interfaces:**
- Consumes: Task 5's `TongueGrammar` field and roster pattern.
- Produces: the coordination type and its realizer entry points. **Proposed
  names, which Tasks 7, 8 and 9 refer to:**

```rust
pub struct Coordination { pub clauses: Vec<Clause> }
pub fn realize_common_coordination(c: &Coordination, vocab: &CommonVocabulary) -> String
pub fn realize_tongue_coordination(/* the tongue realizer's usual bundle */) -> Result<String, TongueGap>
```

  **These are PROPOSALS from outside the code, not requirements.** A plan author
  does not know what the tongue realizer's argument bundle actually looks like at
  the line where this has to sit. Rename or reshape freely — and if you do,
  **say so prominently in your report**, because Tasks 7, 8 and 9 were written
  against these names and their implementers see only their own task.

**Background — this is the second of the two operators, and they are NOT the
same.** Embedding is a slot that holds a clause. Coordination is a **list at a
node**. Spec section 4.10.

`realize_common` takes a `&Clause` and always will. Coordination arrives
**above** it as an additive type with its own entry point, so none of the 65
`Clause { … }` construction sites move and no existing caller changes. Confirm
that count before relying on it:

```bash
grep -rn 'Clause {' --include=*.rs domains/ windows/ cli/ | wc -l
```

This task builds **tier 1 only**: two full clauses joined. Tier 2 (subject
elision) is Task 7. Tier 3 (right-node raising) is **cut** — see spec 9.1.

- [ ] **Step 1: Write the failing tests — tier 1, both drawn values**

```rust
/// Tier 1: two full clauses joined by the tongue's drawn conjunction.
#[test]
fn two_clauses_coordinate_in_common() { /* "It confused me and it upset me." */ }

/// A tongue that drew no conjunction joins by juxtaposition, which is a
/// grammar rather than a gap — the same argument parataxis makes in Task 5.
#[test]
fn a_tongue_with_no_conjunction_juxtaposes() { /* … */ }
```

- [ ] **Step 2: Run them — compile failure, the type does not exist**

- [ ] **Step 3: Build the coordination type and its Common realizer**

Additive above `Clause`. **Do not add a field to `Clause`** — that would cost an
edit at all 65 construction sites and put a list inside a node that is not one.

- [ ] **Step 4: Draw the conjunction on the copula's exact pattern**

One static stream label, presence AND form from one stream, exactly as
`tongue_grammar` draws the copula at `grammar.rs:145-156`.

**State plainly in the label's roster prose why a function word earns a label
when a vocabulary word does not:** a word is a `dynamic(concept)` value on the
existing `PROTO_ROOT` axis (`etymology.rs:433`) and costs zero labels; a function
word's PRESENCE is typological rather than lexical, so the tongue with no
conjunction is not missing a word — it has a different grammar.

- [ ] **Step 5: Roster entry, and count the paths again**

Same discipline as Task 5 Step 4. Report the path count and the label count.

- [ ] **Step 6: Regenerate, run, gate, commit**

```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
cargo nextest run -p hornvale-language > /tmp/mortise-t6.log 2>&1; echo "exit=$?"
grep -E 'FAILED|test result' /tmp/mortise-t6.log
cargo fmt && make gate-commit
git add -A && git commit -m "feat(the-mortise): coordination is a list at a node, with a drawn conjunction"
```

---

## Task 7: Subject elision — coordination tier 2

**Files:**
- Modify: `domains/language/src/clause.rs` (`realize_common_coordination`)
- Modify: `domains/language/src/grammar.rs` (`realize_tongue_coordination` AND
  `realize_tongue_deep_coordination`)

**Interfaces:**
- Consumes: Task 6's `Coordination` and its **three** realizers.
- Produces: no new type — a behaviour change in all three.

**CONTROLLER FINDING (pre-dispatch verification): elision cannot live where an
earlier draft of this plan put it, and there are THREE realizers, not one.**

1. **`join_coordinated` (`grammar.rs:665`) operates on already-realized
   strings** — it trims trailing periods and joins with a separator. It cannot
   elide a subject, because it has no idea where the subject *is*: the tongue
   realizer orders constituents by a drawn `ConstituentOrder`, and the subject
   lands first, middle or last across the six orders (`grammar.rs:634-639`).
   String surgery to find and remove it would be fragile in exactly the way this
   codebase avoids.

   **So elision must happen INSIDE per-clause realization** — a non-first clause
   is realized *without its subject constituent* — not by post-processing the
   joined text.

2. **Task 6's fix round added a third realizer.** The surfaces are now
   `realize_common_coordination` (`clause.rs:1046`),
   `realize_tongue_coordination` (`grammar.rs:701`, the floor) and
   `realize_tongue_deep_coordination` (`grammar.rs:1254`, the production path).
   Landing elision in Common alone would re-open the very asymmetry Task 6's fix
   round closed. **All three, or state plainly why not.**

3. **`Subject` derives `PartialEq`** (`clause.rs:136`), so equality is available
   for deciding whether subjects are shared. Whether `PartialEq` is the RIGHT
   comparison is your call from inside the code — report which you used and why.

**No new drawn axis and no third stream label.** Whether a language elides a
coordinate subject at all is genuinely typological, and this campaign is not
drawing it — elision is applied as a uniform surface convention. Say so in the
doc comments, so the next campaign finds a stated limit rather than an
unexamined default.

**Background.** Tier 2 states a shared subject once: *"Seeing it confused me and
upset me"* rather than *"…confused me and it upset me"*. **Tier 3 — right-node
raising, *"confused and upset me"*, sharing subject AND object — is CUT.** Spec
9.1: what may be elided is language-specific, and getting it wrong yields
*plausible* garbage, the failure mode that survives review.

- [ ] **Step 1: Write the failing test, and its negative twin**

```rust
/// Tier 2: a shared subject is stated once.
#[test]
fn a_shared_subject_is_stated_once() { /* … */ }

/// Tier 3 is NOT attempted: a shared OBJECT is still stated on each verb.
/// This is a scope boundary, asserted so a later campaign that implements
/// right-node raising has to come here and change it deliberately.
#[test]
fn a_shared_object_is_not_raised() { /* … */ }
```

The second test is the more valuable of the two. A cut that nothing asserts is a
cut that gets silently un-cut.

- [ ] **Step 2: Run both — the first fails, the second should already pass**

If the second FAILS at this point, right-node raising has somehow already
happened and you should **stop and report**.

- [ ] **Step 3: Implement elision for the subject only**

Elide only when the subjects are equal. Equality of what, exactly, is your call
from inside the code — report which comparison you used and why.

- [ ] **Step 4: Run, gate, commit**

```bash
cargo nextest run -p hornvale-language > /tmp/mortise-t7.log 2>&1; echo "exit=$?"
grep -E 'FAILED|test result' /tmp/mortise-t7.log
cargo fmt && make gate-commit
git add -A && git commit -m "feat(the-mortise): tier 2 elides a shared subject, and tier 3 stays cut"
```

---

## Task 8: The parser follows both operators

**Files:**
- Modify: `domains/language/src/clause.rs` (`parse_common_with_tail`, from
  line 1071; the `UnknownComplement` site is line 1168)

**Interfaces:**
- Consumes: everything from Tasks 3-7.
- Produces: no new public type; `parse_common` gains reach.

**Background.** `common_constructions`' doc calls the form-meaning pairing
"bidirectional by construction". Realizing a shape nothing can parse breaks that
promise silently, so the parser extends.

The existing walk already does the right thing: it splits at the **earliest**
verb-group occurrence, which for a right-branching complement is the matrix verb.
The recursion attaches where the walk currently gives up —
`best_complement.ok_or_else(|| ParseError::UnknownComplement …)` at line 1168 —
by attempting a recursive parse of the unresolved remainder, under Task 3's depth
budget, **before** reporting the error.

**Read the walk before you touch it:**

```bash
sed -n '1071,1200p' domains/language/src/clause.rs
```

**A limit that stays open, stated so you do not think you closed it:** the parser
returns `Argument::Concept` unconditionally (line 1188), so it cannot recover
`Argument::Pronoun`. Bidirectionality was already partial before this campaign.
**Do not fix that here** — it is registry row
`LANG-parse-cannot-recover-a-pronoun-object` and out of scope.


**CONTROLLER FINDING (pre-dispatch verification): the "boundary marker
discriminates" claim is HALF FALSE for Common, and the half that is false is the
one the parser needs.**

Verified against the committed tests:

- Common embedding emits **no complementizer at all** — `"I did not know they
  killed them."` (`clause.rs`, `a_clause_object_realizes_with_no_determiner`).
  The drawn subordinator from Task 5 is a **tongue** feature; Common has none.
- Common coordination emits the fixed word `"and"`.

So there is no symmetric pair of markers. The real discriminator is
**asymmetric**: a top-level `" and "` means coordination, and its ABSENCE plus a
second verb group means embedding.

**This is not cosmetic — the naive order gets coordination wrong.** For
*"It confused me and it upset me."* the existing walk splits at the EARLIEST
verb group (`confused`), leaving `"me and it upset me"` as a complement, which is
not a clause. **A top-level conjunction test must run BEFORE the verb-group
split**, not after it.

**And tier-2 elision makes the inverse genuinely hard.** *"It confused me and
upset me."* has no subject on the second conjunct; recovering an equal
`Coordination` means re-supplying the elided subject — the exact inverse of
`elide_coordinated_subjects`. Whether that is tractable is a question for
somebody inside the parser, which is you.

**So the stop-and-report branch below is live, not decorative.** If recovering
elided coordination is not small, say so and stop: freezing parse coverage where
The Interlinear froze it, and saying so loudly, is a sanctioned outcome. What is
NOT acceptable is a large speculative parser nobody sanctioned, or silently
parsing only the un-elided case while criterion 6 claims the marker discriminates.

**Pinned cross-task risk.** Task 4 added
`Subject::Clause(_) => unreachable!(...)` in `windows/book/src/lib.rs` (~line
2363), whose premise is *"the parse direction never produces a clause-embedded
subject"*. **You are the task that can make that false.** If your parser learns
to recover an embedded subject, that arm must change in the same commit.

- [ ] **Step 1: Write the failing round-trip tests**

```rust
/// An embedded sentence round-trips: realize, parse, and get an equal Clause.
#[test]
fn an_embedded_sentence_round_trips() { /* … */ }

/// The boundary marker is the discriminator: a sentence with two verb groups
/// is embedding or coordination, and the marker says which. A complementizer
/// means a clause hangs BELOW; a conjunction means one sits BESIDE.
#[test]
fn the_marker_discriminates_embedding_from_coordination() { /* … */ }

/// The depth budget stops the descent rather than recursing forever.
#[test]
fn the_parser_stops_descending_at_the_cap() { /* … */ }
```

- [ ] **Step 2: Run them and record the failures**

```bash
cargo nextest run -p hornvale-language -E 'test(round_trips) + test(discriminates) + test(stops_descending)' > /tmp/mortise-t8.log 2>&1; echo "exit=$?"
grep -E 'FAILED|test result' /tmp/mortise-t8.log
```

- [ ] **Step 3: Extend the walk**

Recursive fallback at the `UnknownComplement` site, depth-budgeted, plus the
coordination discriminator.

**If the extension turns out NOT to be small — if the earliest-verb-group split
does not in fact land on the matrix verb, or the discriminator needs more than
the marker — STOP AND REPORT before writing a large parser.** The spec's fallback
is to freeze parse coverage where The Interlinear froze it and say so loudly.
That is a real option, not a failure; what is not acceptable is a big
speculative parser rewrite nobody sanctioned.

- [ ] **Step 4: Run the crate, gate, commit**

```bash
cargo nextest run -p hornvale-language > /tmp/mortise-t8b.log 2>&1; echo "exit=$?"
grep -E 'FAILED|test result' /tmp/mortise-t8b.log
cargo fmt && make gate-commit
git add -A && git commit -m "feat(the-mortise): the parser follows both operators, by their markers"
```

---

## Task 9: The score moves 2 to 5

**Files:**
- Modify: `cli/tests/suite/sentence_corpus.rs`
- Modify: `docs/timings/subfloor-roster.tsv`

**Interfaces:**
- Consumes: everything from Tasks 1-8.
- Produces: the moved score; no new API.

**Background — do not take these numbers on faith, recompute them.** The exact
resolution, run against the frozen corpus for this plan:

```
covered after this campaign:  5 of 12  ->  m05 m06 m07 m09 m10
remaining blockers:  temporal-adverbial 3, wh-question 2, polar-question 2,
                     witness-set 2, existential 1, named-entity-list 1
distance report shrinks to:  m01 wh-question, m02 temporal-adverbial,
                             m08 polar-question
```

- [ ] **Step 1: Recompute the resolution yourself before editing anything**

Resolve the frozen corpus against `IMPLEMENTED_DEMANDS` plus the three tokens
this campaign adds. Any script is fine; the point is that the numbers below come
from a run, not from this plan.

**If your numbers differ from the block above, yours are right and the plan is
wrong. Report the difference.**

- [ ] **Step 2: Add the three tokens, and only tokens the grammar can back**

```rust
// IMPLEMENTED_DEMANDS — The Mortise
"coordination",
"embedded-clause",
"epistemic-hedge",
```

The module doc names the backing test for every token. **Add yours** — a token
without a named backing test is exactly the optimism the doc warns makes the
instrument worse than no instrument.

- [ ] **Step 3: Move the score, the id set, and the distance report together**

`MERCHANT_COVERED`, `MERCHANT_COVERED_IDS` and `MERCHANT_ONE_MISSING` all move.
Each has a doc comment explaining what it guards; **update the prose, not only
the values** — a stale doc beside a moved number is how the last campaign's
`seam-guard` prose rotted.

- [ ] **Step 4: Extend the witness to every newly covered entry**

`MERCHANT_WITNESS` gains m06, m07 and m09, each with the surface the grammar
**actually produces** — pasted from a run, never predicted.

**This is where the campaign's honesty is cashed.** Our surfaces will differ from
the corpus's English: m06's *"why"* is an indirect question we do not build, and
m07's *"confused and upset me"* is right-node raising we cut. The witness records
that distance next to the corpus text. **Do not adjust a clause to close the gap
cosmetically** — the gap is the finding.

- [ ] **Step 5: Rename the three tests, and edit the roster IN THE SAME COMMIT**

```
merchant_coverage_is_two_of_twelve   ->  merchant_coverage_is_five_of_twelve
the_covered_entries_are_m05_and_m10  ->  <a name stating the new set>
four_entries_sit_at_one_missing_demand -> three_entries_sit_at_one_missing_demand
```

**`docs/timings/subfloor-roster.tsv` selects by EXACT name.** A rename without
the roster edit silently drops the test from `gate-commit` while everything stays
green. Verify afterwards:

```bash
grep -n 'sentence_corpus::' docs/timings/subfloor-roster.tsv
```

Every name listed must exist, and every test must be listed.

- [ ] **Step 6: Run the full corpus suite, gate, commit**

```bash
cargo nextest run -p hornvale -E 'test(sentence_corpus)' > /tmp/mortise-t9.log 2>&1; echo "exit=$?"
grep -E 'FAILED|test result' /tmp/mortise-t9.log
cargo fmt && make gate-commit
git add -A && git commit -m "feat(the-mortise): the merchant corpus reads 5 of 12, and the witness says what it says"
```

---

## Task 10: Something must actually say one

**Files:**
- Modify: `windows/book/src/lib.rs` (the production-caller candidate), **or**
  a written declaration of inertness — see below.

**Interfaces:**
- Consumes: everything from Tasks 3-8.
- Produces: either an embedded clause in a committed artifact, or a written
  reason there is none.

**Background.** `LANG-in-character-acts-are-unspeakable` records this repo's own
version of the failure: The Deed minted 14 concepts and **seven are inert**,
verified in the committed manifest, because three successive tasks each shipped
without deciding who would wire them. **Not deciding is the one outcome
foreclosed here.**

`windows/book` is the natural home — it already calls `realize_common` at seven
sites (1092, 1162, 1337, 2408 and others; confirm with a grep) and
`realize_tongue_deep` at two.

**Adjacency you must know about before editing that file:** `windows/book` has a
**readout law** (`lib.rs:3500`, `fn the_readout_law`) forbidding
`Evidential::Inferred` at any production site as floor-unreachable. An epistemic
hedge is a matrix CONSTRUCTION, not an evidential FEATURE, so the two are
orthogonal and **no amendment to that law is sanctioned by this spec**. If your
work seems to need one, **stop and report** — that is a decision, not a step.

- [ ] **Step 1: Find a site where an embedded clause is TRUE, not merely possible**

Read the existing call sites. The question is not "where could I put one" but
"where does the book already say something that is genuinely one clause inside
another". A forced insertion is worse than an honest inertness declaration.

- [ ] **Step 2: Decide, and write the decision down either way**

- **If you found a site:** wire it, and add a test asserting the artifact
  contains the embedded surface.
- **If you did not:** write the inertness declaration — where it goes, and the
  reason. "Nothing in the world currently holds a belief it would hedge" is a
  legitimate finding; "we ran out of time" is not.

Report which branch you took and the reasoning, in full. **This step's output is
a judgement, and it is the one place in this plan where the honest answer might
be "no".**

- [ ] **Step 3: Regenerate artifacts if you wired anything**

```bash
make rebaseline
make rebaseline-goldens
git diff --stat   # the whole tree: byte-goldens are not in generated-paths.txt
```

Apply Task 2's Step-6 decision table to the diff.

- [ ] **Step 4: Gate and commit**

```bash
cargo fmt && make gate-commit
git add -A && git commit -m "feat(the-mortise): <wired at X | declared inert, because Y>"
```

---

## Task 11: Definition of Done

**Files:**
- Create: `book/src/chronicle/the-mortise.md`
- Create: `docs/retrospectives/the-mortise.md`
- Create: `docs/decisions/0326-*.md` and upward (block 0326-0335 is reserved)
- Modify: `docs/retrospectives/README.md`, `book/src/frontier/idea-registry.md`
- Modify: whatever the freshness sweep finds

**Background.** Definition of Done for every merged plan includes the project
book: a chronicle entry and a freshness sweep of stale chapters. The book may
never lag merged reality.

- [ ] **Step 1: Write the decision records**

From the reserved block, at minimum:

- a clause complement rides the existing transitive frame; there is no sentential
  valence (spec 4.2);
- embedding and coordination are two operators, not one (4.10);
- embedding nests one level, by a stated cap (4.3);
- subordination strategy and conjunction are drawn per tongue (4.6, 4.10) — the
  two save-format contracts;
- the corpus score is demonstrated, not declared (4.8).

**Use the reserved block 0326-0335.** Do not compute max+1 — decision numbers are
reserved in blocks, and max+1 collides silently.

- [ ] **Step 2: Two pending corrections, folded in here**

Both are small and both were inherited from the previous campaign's handoff:

- `docs/retrospectives/the-inquest.md` says **Merged: 2026-08-26**; it landed
  past midnight on the **27th**.
- `docs/retrospectives/README.md`'s Scarf entry says **eleven** defects; the true
  count is **twelve** (the twelfth surfaced during its own DoD task).

**Verify both before changing them** — they are claims from a handoff document,
which is exactly the class this plan tells you not to trust.

- [ ] **Step 3: Write the chronicle**

Technical and mathematical altitude, comprehensible without reading the code.
It must include a **"What this does not reach"** section — that section in the
Inquest's chronicle is what made this campaign scopeable. Name at least:
right-node raising, nominalization, temporal adverbials with their adjectival-
predication trap, and that the merchant corpus is nearly spent as an instrument.

- [ ] **Step 4: Write the retrospective — process, not product**

Lead with what the ledger recorded: the coordination cut was reversed at G3
because the original pricing was wrong in two checkable places. That is a
process finding about plan-text estimates, not a product finding.

- [ ] **Step 5: Update the registry rows this campaign shipped or changed**

`LANG-clause-connectivity` moves off `raw`.
`LANG-clause-nominalization-and-ellipsis`, `LANG-adjectival-predication`,
`LANG-merchant-corpus-is-nearly-spent`,
`LANG-parse-cannot-recover-a-pronoun-object`,
`LANG-vocabulary-scale-is-unmeasured` and
`LANG-registry-is-exotic-heavy-and-core-light` already exist — check their
**Where** cells point at the chronicle you just wrote.

The registry cap is **600 characters** per idea field
(`cli/tests/suite/docs_consistency.rs:454`). Check programmatically; two rows in
this campaign already had to be trimmed.

- [ ] **Step 6: Freshness sweep, full regeneration, and the final gate**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
cargo fmt && make gate-commit
```

Then the stage gate on the canonical box, which is what actually runs the full
suite and the doctests:

```bash
git push origin campaign/the-mortise
make sluice-stage BRANCH=campaign/the-mortise REF=$(git rev-parse HEAD)
```

- [ ] **Step 7: Commit**

```bash
git add -A && git commit -m "docs(the-mortise): chronicle, retrospective, decisions 0326+"
```

---

## Success criteria — the preregistered list

Copied from spec section 6 so the executor need not switch documents. **A
falsified prediction is a finding, not a failure**; if one of these turns out
false, the null ships and the chronicle says so.

1. `Argument::Clause(Box<Clause>)` exists; every exhaustive match handles it; **no
   new `Valence` variant was added.**
2. `realize_common` produces an embedded complement with no determiner before it.
3. `realize_tongue` and `realize_tongue_deep` realize an embedded clause using a
   **drawn** subordination strategy, and a tongue that drew parataxis emits no
   marker.
4. A coordinated utterance realizes in Common and in a tongue with a **drawn**
   conjunction, and a tongue that drew none joins by juxtaposition.
5. A shared subject is stated once; right-node raising is NOT attempted.
6. `parse_common` round-trips a one-level embedded sentence, and distinguishes
   embedding from coordination by the boundary marker.
7. Nesting beyond the cap is refused in both directions, by an assertion that
   fails without the cap.
8. The merchant score moves **2 to 5**; the covered set is exactly
   `m05 m06 m07 m09 m10`; the distance report moves with it.
9. The realization witness covers every covered entry, and **its RED is
   demonstrated rather than assumed** — the implementer finds a perturbation a
   covered entry depends on, applies it, and records the failure. **The
   perturbation is not prescribed here.** The property: *a covered entry whose
   construction has been neutralized must fail the witness.*
10. Spec section 4.9 is discharged one way or the other, in writing.
11. **Two** stream labels were added, and `stream_labels()` reports every path
    the new draws create — counted, not assumed.
