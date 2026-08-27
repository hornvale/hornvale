# Campaign The Scarf — retrospective

**Merged:** 2026-08-26

## The headline: eleven defects, all of them mine, none in implementer code

Every defect this campaign found originated in the **spec and plan text I
wrote**. Zero were found in the code the implementers produced. That is not a
compliment to the implementers or a complaint about them — it is a statement
about where the risk in this working style actually sits. The plan is a
literal code listing plus prose assertions about the tree, reviewed for
faithful transcription; the code is reviewed against the plan. Nothing in that
loop reviews the plan against the tree except someone running a command.

The eleven, grouped by what produced them:

**A destructive step nobody would have questioned.** Task 1 instructed a
`git checkout -- clause.rs` as a restore after a mutation probe, at a point
where that file held uncommitted work. It reads like housekeeping.
`scripts/mutate.py --help` warns about exactly this and offers `--to`; the plan
did not use it.

**Three count errors of one shape.** `grep -c 'ClauseSpec {'` counts the
type's own **definition** — and a `-> ClauseSpec {` return signature — as a
construction site. `ClauseSpec` was 19 by that count and 17 in fact;
`TongueClause` 10 and 9. The same carelessness produced "four new tests" for a
block that held five. A count taken with the wrong pattern is not a smaller
number, it is a different measurement.

**Two claims about what compiles.** "`tongue_view` still compiles at this
point" was false — retyping `realize_tongue_deep` breaks `Tongue::say`, so
Task 4's first step had to move into Task 3. "Add no imports" was false too:
`Number` and `Definiteness` were not in scope at the site the plan added them
to.

**An omitted artifact regeneration.** Task 4 deleted a `pub` type and did not
say to regenerate `docs/audits/type-audit-report.md`, which moved 752 → 750.
**`gate-commit` cannot catch this**: type-audit `check` is a lint over the
source, and the report is a separate drift-checked artifact. It would have gone
green locally on every commit and reddened `main` at the merge.

**A stale flagship line**, carried from a chronicle written the day before 202
commits of `main` landed — see below.

**A silently-no-op mass rename, which is the one worth writing down.** Task 5's
rename command used BSD `sed` with `\b` word boundaries. Measured:
`sed 's/\bClauseSpec\b/Clause/'` matches nothing, **exits 0, and prints no
diagnostic**. The command would have reported success and moved zero of 67
sites, and the test suite would have passed *because nothing changed*. GNU sed
supports `\b`; BSD sed uses `[[:<:]]`/`[[:>:]]`, and the two are not
interchangeable in either direction. **Verify a mass rename by count, never by
exit status.** The same task's site count was also wrong — 41 sites in 6 files
when the truth was 67 in 7 — because it was measured before Tasks 1 through 4
added sites, which is the ordinary hazard of writing a whole plan up front.

**And my own error count, which is the sharpest of the eleven.** I filed the
rustdoc-link gap saying `origin/main` carries **one** hard error. It carries
two: `unresolved link to render_views` and `unresolved link to
ConceptRegistry`. I had run the correct command. I read its output through
`head -20`, and the second error sat past line 20. The command was right; the
claim attached to it was measured on a truncated view. **Establish the total
before trimming the output for readability** — the trim is where a narrower
question replaces the one you meant to ask.

## What actually caught them

Not review. **Every one of the eleven died to somebody running a command.** The
mechanism that produced most of them is worth keeping: implementers were
instructed to **override the plan and report the deviation** rather than comply
with it, and several did precisely that — the missing imports, the forced task
reordering, the site counts. A plan that is treated as authoritative gets
transcribed; a plan that is treated as a hypothesis gets tested. The
difference cost nothing and was the campaign's whole quality mechanism.

The two corrections to The Interlinear's chronicle came the same way. The
"`paradigm.rs` already draws adpositions and case morphology" claim looked
plausible, sat in a merged chronicle, and had already survived one merge; one
`grep -rniE 'adposition|postposition|preposition'` returned a single doc
comment and settled it. The flagship line `Nwamvam Qoqe Bae 8835 25 375` was
settled by running the thing: the live value is `18822 25 1600`.

## The generalisation worth carrying: a demonstration nothing asserts is prose

The Interlinear's flagship line was not wrong when written. It went stale
because its numerals are ledger data, and 202 commits of `main` were absorbed
into that branch the following day without the demonstration being re-run.
**Nothing could have objected**: `git grep 8835 -- '*.rs'` finds only synthetic
test literals, so no test asserts those numerals at all — deliberately, since
that test file pins properties rather than sentences precisely so a parallel
campaign's registry change does not redden it.

That is a real tension and this campaign does not resolve it. What it does is
name the consequence: **absorbing `main` is exactly when a chronicle's worked
example goes stale, and a worked example is the part of a chronicle nothing
guards.** The remedy taken was additive — keep the recorded line, note that the
numerals are ledger data since moved, name the current values. Rewriting the
numbers silently would have produced a chronicle that looked correct and
erased the finding.

## Estimate deltas

Five implementation tasks, all green on first or second attempt; the code half
of the campaign was uneventful, which is what an additive-first, destructive-
last stage order is supposed to buy. Task 4's Step 1 migrated into Task 3 for
the compile reason above. The only unplanned work was the type-audit report
regeneration the plan omitted.

## Do differently next time

- **Size a mass edit by count before and after, and never by exit status.**
  `sed`, `grep -c` with a brace pattern, and `cargo check` that stops early are
  all instruments that will hand you a confident wrong number.
- **Establish a total before trimming output.** `head -20` on a diagnostic list
  is how a two-error count becomes a one-error claim.
- **When a task deletes a `pub` item, name the type-audit report in the same
  task.** The commit gate structurally cannot see it, so the plan is the only
  place that check exists.
- **Keep telling implementers to override the plan and report.** It is the only
  part of the loop that reads the plan against the tree.
