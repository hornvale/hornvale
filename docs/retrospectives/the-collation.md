# Retrospective — The Collation

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-collation.md): a second frozen
corpus, one committed report per corpus, and the matrix ADR 0095 deferred.

## The dominant lesson: the rule was not in the file the reviewers read

The one defect on this branch that **no review found** was found by the first
`make gate` of the campaign, after all three tasks, one review per task, a
whole-branch review and a scoped re-review of the fix wave.

Task 3's fix round added a backstop — every corpus declared in `CORPORA` has a
committed column — as a `#[cfg(test)]` unit test inside `cli/src/tropes.rs`,
reaching the workspace root through `env!("CARGO_MANIFEST_DIR")`.
`cli/tests/build_path_embedding.rs` scans `*/src/` and does not read `cfg`
attributes, so it counts that expansion as a production build-path embedding
under decision 0090, whose cross-host binary-identity oracle holds only while
two hosts build at the same absolute path. **Three reviews read that test** —
the Task 3 re-review, the whole-branch review, and the fix-wave re-review,
which edited it — and one called its placement rationale sound.

Nothing about the test looks wrong, because nothing about the test *is* wrong
on its face. The rule it broke is stated in one place: the doc comment of the
scanner that enforces it, in a different directory, which says a `#[cfg(test)]`
module inside `src/` "would be counted here, conservatively; there are **none
today**." That sentence was false for the life of this branch and is true
again. A reviewer reading the diff, the brief and the spec has no path to it.

Two things made the gap survivable for that long. `make quick` — the
pre-commit gate every task ran — is `fmt-check clippy type-audit` and **runs no
tests**; the campaign knew this, because the plan claimed otherwise and was
corrected before execution (`5837e354`). And every task ran its tests *scoped*,
which is the project's own cost-ordered instruction and was correct: no task
had reason to run a cross-cutting enforcement test in `cli/tests/` that its own
work never touched. The class is therefore structural rather than careless —
**a rule that lives in a scanner, and only in the scanner, is invisible to
every reviewer of the code it governs**, and the only thing that sees it is the
whole-workspace gate.

The fix hit the same wall twice: the replacement comment in `cli/src/tropes.rs`
explaining the move *named the variable in prose*, and the scanner — a plain
text match — stayed red. That trap is not in the scanner's scope paragraph
either.

**What to do differently:** when a task adds a test in `*/src/`, run
`cargo test -p hornvale --test build_path_embedding` in the same step, the way
this project already expects a `pub` addition to drift the type-audit report.
It costs under a second. The general form: a brief that adds surface in a
directory carrying workspace-wide enforcement should name the enforcement it is
now subject to, because the reviewer cannot infer it from the diff.

## A promised check, a weaker check, and a weaker check again

The spec named the validation exactly: *"one test asserting the matrix's
per-column figures equal the per-corpus reports' own."* Three documents later,
that test did not exist.

The plan's Task 3 Step 1 supplied a literal code listing whose predicate was
`matrix.contains(corpus)` and `matrix.contains(&total.to_string())` — satisfied
by any cell reading `(217/409)`, so it passes on a stale document. The Task 3
review caught that. The fix round replaced it with a whole-file byte comparison
of the matrix against its own committed copy, **proved to discriminate by a
mutation** (`0 of 36` → `0 of 35`, red), and the re-review passed it.

That proof was real and was against the wrong failure. A self-golden pins the
matrix to itself; the divergence the spec cared about is `render` and
`render_matrix` — which each independently filtered `out.values()` — coming
apart, and rebaselining accepts both documents in the same pass. Only the
whole-branch review saw that neither check was the promised one. The fix made
the divergence unrepresentable (one `tally` feeding both renderers) and added a
parsing cross-derivation test, then proved it by doing the finding's own
scenario: perturb `render_matrix`, rebaseline, watch the byte check go **green**
while the new test stayed red naming both figures.

**Making a check fail on command is necessary and is not sufficient — the
mutation has to be the failure the check was promised against.** A check
written to replace one that could not fire inherits the burden of proof, not
the credit.

## The recurring defect recurred, in its exact signature

The matrix's Demand preamble read "Every bundle either catalogue requires
(52)". The queried table was right; 12 of those 52 rows name bundles no
catalogue *declares*, which expand to themselves and block by construction, and
the sentence flattened two different things into one number. This project's
standing shape — the defect is never in the query, always in the sentence
summarising it.

The fix derived the split rather than rewording the sentence: 12 is now
rendered, and the twelve rows carry a `†`. Rewording would have produced a true
sentence that goes stale the day a third catalogue lands.

## Two deliberate anti-patterns, both of which worked

**Handing the implementer figures derived by a different route, and requiring a
stop rather than a rebaseline on disagreement.** Task 3 Step 5 gave five
bundle shares computed from the committed reports by a route the renderer would
not use. They agreed digit for digit (53/19, 29/6, 25/50, 12/33, 100/100); the
controller then checked all fourteen rows of the spec's fuller table against the
rendered document, and the re-reviewer re-derived the bundle sets from both
corpora a third time. Three derivations, one number. That is evidence in a way
a passing golden is not, and it cost one paragraph of plan text.

**Naming in advance the way a test would silently degrade.** Task 2 Step 5
predicted that path derivation would turn `check_mode_fails_on_a_divergent_corpus`
from a content check into a missing-file check — identical non-zero exit,
different substance — and required the implementer to say which failure mode
the repaired test exercises. The report answered with the CLI's actual stderr,
distinguishing the content-mismatch branch from the `read_to_string` error
branch by hand. This is the cheapest thing in the campaign that worked.

## A wrong command in a plan produced a file rather than a report

Task 1 Step 3 prescribed `cargo test -p hornvale --lib` for a binary-only
crate. The first implementer fabricated a `cli/src/lib.rs` to make the flag
match a target. It was caught and sent back, the file never reached a commit,
and the re-run verified with `--bin hornvale`. The plan's text was corrected
afterwards on this branch (`e225ce5e`) rather than quietly absorbed.

The instruction that was missing is now in the plan: the crate has no lib
target, and **one must not be created to satisfy a test invocation**. An
implementer's default is to make the prescribed command work; a plan that names
a command is issuing an instruction, and a wrong one costs a fabricated file
before it costs a question.

## Operational

- **The spec's "enumerated blast radius" was incomplete, and enumeration is why
  that was cheap.** Four sites were listed for Polti's artifact rename; the
  fifth — a published book link at `book/src/chronicle/the-repertoire.md:43`
  that would have 404'd — was found by the implementer, who then grepped
  repo-wide to establish it was the only live one. A list that is wrong by one
  is still what makes the miss visible.
- **The box was shared and the ledger's queue column cannot see it.** A foreign
  commit (`03ff6ce9`, unrelated idea-registry rows) landed on this branch
  mid-campaign from another session; another session's untracked test file
  appeared and vanished inside the worktree; and the failed gate left its own
  `docs/timings.md` row dirty in a tree described as clean. The green gate ran
  **580.4 s at cpu_ratio 4.45** against the same suite's 304–384 s at 6.89–8.16
  earlier the same day with near-identical user time — the same work, roughly
  half the parallelism. `waited_s` read **0** on both rows, correctly and
  uselessly: it counts time queued behind the census claim, and gates
  deliberately take no claim (0081). The contention CLAUDE.md warns about is
  visible only in `cpu_ratio`, never in the column named for waiting.
- **Both plan corrections were committed as their own act**, before execution
  (`5837e354`, the `make quick` description) and after it (`e225ce5e`). A plan
  is part of the campaign's record; a re-runner reading it would have hit both
  traps again.

## Confidence Gradient

`book/src/open-questions.md` was checked. **No bet moved** — the chronicle's
enumeration of the chapter's wagers is correct and this campaign touches none of
them; ADR 0095's matrix commitment was never a bet on that map.

The chapter's **preamble tally did move**, and it is re-scored here per decision
0030. The running diagnosis at line 63–75 is that these defects are a property
of *plans written as literal code listings*, reviewed for faithful
transcription rather than for whether their predicate is the one the spec asked
for. This campaign produced that shape again from its own plan text, and adds
one thing the tally did not have: the shape **survived its own correction** —
the replacement check was mutation-proved, passed a review on that proof, and
still could not fail for the reason the spec named. A fourth paragraph states
that, and nothing else in the chapter changed.

## Follow-ups

- **`build_path_embedding.rs`'s scope note is accurate again but was false for
  the life of this branch**, and it does not say that its scan is a plain text
  match that counts prose in comments. Both are worth adding; the sentence
  could name the campaign that proved it.
- **Two disclosed minors stand**, both recorded in the ledger and neither
  reachable today: `declared` in the matrix means "declared by any column",
  which a third catalogue would need per-cell; and the matrix's share counts a
  numerator over `corpus.situations` against a denominator of `out.len()`, so a
  duplicated id would render above 100% — blocked by the uniqueness sweep, not
  by the arithmetic.
- **`check` on a non-canonical spelling of a corpus path** reports drift and
  points at a rebaseline that will not help, because the header carries the
  invocation path. Plan-mandated, harmless for the canonical invocations, and
  surfaced rather than fixed.
