# The Quire — retrospective

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-quire.md).

**The through-line: twelve defects were found in this campaign, and every one
of them originated in the controller's own plan or dispatch text. None
originated in implementer code.** That is not a compliment to the implementers
and not a confession of unusual sloppiness; it is the same finding
`defects-originate-in-plan-text` has now returned often enough that it should
be treated as the base rate. The interesting question is no longer *whether*
the plan is where defects live, but *which shapes* of plan sentence carry them.
This campaign produced four such shapes, below.

## 1. The imperative mood hides an assertion

Five of the twelve came in through one grammatical form. A plan step that says

> Run `git diff --exit-code <paths>` and confirm the diff is empty.

reads as an instruction and is in fact a **prediction**: it asserts the diff
*will* be empty. When it is not — because the task under it adds public items
and the type-audit report was always going to drift — the implementer is put in
the position of overriding a "STOP and escalate" clause in order to do the
right thing. One did, correctly. That is a good implementer compensating for a
bad sentence, and it should not be the safety net.

The same shape produced the campaign's worst step: a dispatch told an
implementer to run `possess --seed 42 --snapshot X`, omitting `--script`. Every
other possession call in the regeneration script passes `--script`. Without it
the command blocks on `stdin.lock()`. Wired into the regeneration script as
written, it would hang `make rebaseline` for **any developer running it from a
terminal**, and pass silently in every agent context, where stdin is already at
EOF. A defect that hides from exactly the people who would hit it. The reviewer
reproduced it as `exit=124` against a FIFO held open, then reproduced `exit=0`
after the fix — a positive control rather than a green.

Two more of the five: a plan named `members = ["core", "bin"]` for a workspace
whose `bin/` would not exist for another six tasks (a hard cargo error); and a
plan put a `marks` field at the top level of a structure where the producer
carries it per-cell.

**The rule that already existed and did not fire.** The autopilot verification
rule says a claim about live behaviour is settled by running something. It was
written for spec prose. It applies verbatim to plan steps, and nobody had said
so. An amendment landed mid-campaign — *read the producer, do not guess the
wire names* — and it immediately caught the per-cell `marks` defect. The
generalisable version:

> **A plan step in the imperative that names an expected outcome is an
> assertion. Either verify it before writing it, or write the step so that
> either outcome is a legal result.**

## 2. Two defect classes that no step-level proof could catch

Both were found by *looking at the output*, and both sat under fully green,
non-vacuous, mutation-proven suites.

- **The chart was geometrically wrong under 17 green tests and 4 mutation
  proofs.** Every test asserted a property that holds whether or not the
  projection is right: cell count, glyph selection, weight assignment, `you` in
  the middle. The defect was found by pasting the rendering next to the
  simulation's own `map` output for the same 31 cells and seeing five dense
  rows against nine sparse sheared ones.
- **`examine` burned a turn under 10 green tests.** The key sent a bare verb
  with no object, which is correct for an unconditional key-to-verb mapping and
  is still a usability defect. Found by playing.

The lesson is not "write better tests" — the tests were good, and the mutation
proofs were real. It is that **a property test and a picture answer different
questions**, and for any rendering work the picture must be inspected by
someone who can compare it against an independent rendering of the same data.
Where such a renderer exists in-repo (it did: the simulation's own ASCII map),
the comparison is cheap and should be a planned step, not a lucky one.

Corollary that did work: after the chart defect, the *indoor* plate was
pre-emptively pinned against the simulation's own indoor `map` output
byte-for-byte, and passed.

## 3. A test helper that pinned one value made a whole suite blind

An input-mapping helper constructed its key events with `KeyModifiers::NONE`
hard-coded. Every test in that suite was therefore silent about modified keys —
not failing, not skipped, simply unable to express the case. A fixture constant
chosen for convenience became an unstated precondition on every assertion built
on top of it.

**Generalises:** when a helper freezes a parameter, the suite's coverage claim
is scoped to that parameter's single value, and nothing in the suite says so.
Audit shared test constructors for frozen arguments before reading a green
suite as coverage.

## 4. Four guards that were green over the wrong property

All four were caught, none by the guard.

- **The containment guard was blind to `optional = true`.** The check greps
  `cargo tree` for a simulation dependency in the render crate. A dependency
  declared optional does not appear in the default tree. The reviewer
  *demonstrated* it — one manifest line, and the guard printed `ok` and exited
  0. Fixed with `--all-features` on both the check and its failure dump. The
  implementer then deliberately declined to widen further to `--target=all`,
  and said so, rather than implying airtightness.
- **The registry drift check passed a row whose escaped `\"` would render
  literally in the published book.** The check counts columns, treating `\|` as
  an escape; it has no opinion about quotes. Caught by reading the rendered cell
  back. Same family: green over the wrong property.
- **The pre-commit hook validates the working tree, not the index.** A
  regenerated artifact sat on disk (hook green) and was never staged, so the
  commit recorded the stale version. Found by checking `git status` *after* the
  commit. **A green pre-commit hook is not evidence that the commit contains
  what the hook checked.**
- **A revert that looked done was not.** Reverting a `Cargo.toml` edit left
  `Cargo.lock` still carrying the dependency; `cargo tree` had to be re-run to
  rewrite the lockfile. This was in the middle of proving the containment guard
  red-then-green — so an incomplete revert would have made the *green* half of
  the proof a lie.

## 5. Merge commits do not run pre-commit hooks

**Two absorptions, two mis-merges of the same file, and the second one had no
safety net at all.**

`docs/audits/type-audit-report.md` is a generated count table. On both
absorptions, git line-merged the two versions into a file matching **neither
parent** — arithmetic that was never true on either side. The first time, the
pre-commit hook caught it, because it happened to be fixed on an ordinary
commit. The second time it was inside the merge commit itself: `git merge`
exited 0 with a clean tree, the counts were wrong, and nothing in the hook,
`regenerate-artifacts.sh`, or `make preflight` looks for it. It was found only
because the merge was followed by a deliberate re-render and diff.

This is the standing "a generated artifact has no merge" rule meeting a hole in
its enforcement. The rule is known; the *timing* of the enforcement is not.
Filed as a `PROC-*` row.

## 6. Contention wasted about two hours and produced one false diagnosis

Two incidents, opposite ends of the campaign:

- The spec's cost measurements were first taken at **loadavg 50**, while the
  campaign's own `make prewarm` was running. They were **3.3× wrong** and were
  discarded. The re-measurement on a quiet box is what the spec carries.
- The first merged-tree gate ran at **loadavg 265** (15-minute average 626),
  took **92 minutes** (5538 s, from the campaign ledger — *this run was never
  written into `docs/timings.md`, so it carries no `cpu_ratio` of its own*),
  and produced a **false failure**: a wall-clock assertion in an unrelated
  crate. It was initially diagnosed as a pre-existing defect on main, on
  blob-identity evidence, and had to be walked back after it passed in 0.05 s
  on a quiet box. A separate contended gate the same day *is* ledgered —
  `2026-08-09T17:01:34Z`, 598.1 s, `cpu_ratio` **0.26** — and the same run's
  second gate, quiet, came back at `cpu_ratio` **7.03** and 398.6 s
  (`19:37:58Z`).

  The first draft of this bullet borrowed the 0.26 from the ledgered 10-minute
  run and attached it to the unledgered 92-minute one. That is precisely the
  failure this section's own lesson is about, committed inside the section
  stating it: **the 92-minute datum is the one measurement here with no load
  figure recorded beside it, which is why it was available to be mis-paired.**

Both are the root `CLAUDE.md` "one gating agent at a time on the Mac" rule
being violated in practice, and both cost more than the wait would have. The
second-round close agent instead *waited twice*, about fourteen minutes total,
and got clean numbers.

**The operational form:** a green under contention is still green, but a **red**
under contention is not information, and no *timing* from a contended run is.
Record the load average next to every measurement, and re-measure rather than
diagnose.

## 7. Honest uncertainty was the primary detection mechanism

This is the pattern most worth carrying forward, because it is cheap and it
worked repeatedly. Nearly every serious finding in this campaign surfaced
because an implementer or reviewer **flagged something they were unsure about**
instead of shipping it quietly:

- An implementer reported a mutation the plan prescribed as a **null** — the
  two derivations are pure and share no stream, so no ordering between them is
  observable — then found a mutation that does fail. Better than the plan.
- An implementer flagged that dropping an offset was undetectable against the
  real fixture, because seed 42's chamber happens to start at the origin, and
  wrote the synthetic test that covers it.
- An implementer flagged that a provenance category named a datum the wire does
  not carry, and asked whether it earned its place. It did not, and was deleted.
- An implementer flagged that the entry pane silently dropped overflowing
  prose, called it a real question rather than a nit because prose is the
  constitutional primary, and left it for adjudication.
- An implementer flagged a scope extension — reimplementing a *semantic*
  invariant of the simulation rather than a geometric one — as higher
  divergence risk, and asked for it to be checked. It was, and that check is
  what surfaced the band-fold defect underneath.
- An implementer flagged a new direct dependency before adding it. (It resolved
  to a crate already in the build transitively, so no new crate entered.)

None of these were failures. Every one of them was a report saying *here is the
part I am least sure of*, and that sentence is what a controller can act on. A
report that reads as uniformly confident gives a reviewer nothing to aim at.

## 8. An inherited diagnosis is a hypothesis — twice, in both directions

The campaign's twelfth and last defect: a reviewer concluded "no ways-on datum
exists on the wire" by reading **the client's own mirror**, which had never
mirrored the field, rather than the producer. The controller accepted the
ruling without checking and propagated it into a fix. The producer says the
opposite in its own doc comment. This happened in a campaign where *read the
producer, do not guess* had already caught three separate defects.

The mirror image, and the better outcome: a dispatch told an implementer that a
red browser test was caused by a specific commit. The implementer **checked
instead of accepting** — that commit had changed only a comment — and traced
the real cause to an absorption merge that had regenerated the fixture and moved
its one mark from one cell to another, verified by probing the renderer against
the committed fixture. That distinction decided the fix: the test's hard-coded
coordinates were a stale **witness** and re-pinning them is legitimate;
re-pinning a **claim** to match broken behaviour would not have been.

Same shape, two outcomes, and the difference was entirely whether the
downstream party treated the handed-down diagnosis as a conclusion or as a
hypothesis.

## 9. Two things a positive control proved by accident

Worth recording because in both cases the *failure* was the evidence:

- Renaming a loop binding to silence a lint took two attempts. The first
  attempt renamed the binding **and added a comment explaining why** — and the
  comment quoted the repo's all-caps corpus constant, which the lint tokenizes
  out of raw body text including comments. The comment explaining the false
  positive re-created it. The round trip is the positive control: the only
  difference between the red intermediate state and the green final one is a
  token inside a comment, which proves the lint is purely token-shaped.
- Proving terminal restoration under signals required a real pseudo-terminal
  harness, and the harness had to be fixed first: its fixed wait signalled ready
  before the child had entered raw mode, so the "restored" assertion passed
  because nothing had ever changed. The reviewer built its own harness,
  deliberately reproduced the vacuous pass, then fixed it.

## 10. Left open, deliberately

- **The band fold.** `band == "walk"` means "not inside a built structure", so
  submerged and underground fold into it, and nothing on the wire distinguishes
  them except literal prose. The campaign is *evidence* for that open question,
  not a resolution of it; the client stopped needing it resolved once the exits
  row was deleted.
- **Quit discoverability.** `release` is the last line of the simulation's help
  text and the entry pane truncates it at 80×24, so `?` cannot teach a player
  how to quit. Confirmed by live pseudo-terminal render. Recommended fix was a
  chrome quit-hint on the identity strip; Nathan ruled it a follow-up. The real
  fix is a scrollable entry pane, which dissolves this *and* the truncation
  trade-off.
- **No frame goldens.** Nathan's reasoning: goldens over a growing game churn
  constantly. Consistent with `drift-checks-freeze-bugs` and
  `a-committed-baseline-is-a-claim-with-a-date`. What replaces them is
  interactive play as a feedback loop, plus a future direction — scenarios that
  each isolate one mechanism, made golden-stable by component gating.
- **A stale lint heuristic on main.** `seed_shaped()` treats any binding named
  `s` as a seed, so any `for s in …` in the repo trips it regardless of what it
  iterates. Flagged, not fixed — another campaign's call.
