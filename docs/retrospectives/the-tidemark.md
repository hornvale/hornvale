# The Tidemark — retrospective

**Status:** closed 2026-09-13. This record is written *into* the merge rather
than after it (`closing-a-campaign` step 5b — a second merge slot for a handful
of status lines measured ~1,300 s), so the line is authored before the landing
it describes. `git log --merges` finds the commit; the reconciliation row names
the branch.

## Every defect was found by running the thing or mutating it. None by re-reading, and none by its author.

That is the campaign's through-line, and it is stronger evidence than the
count, because the count is unremarkable for this project by now. What is
worth recording is *which instrument* closed each one.

| # | defect, in controlling-session or implementer text | what killed it |
| --- | --- | --- |
| 1 | `OccupationRecord` recorded as carrying no cause field — written into the spec, the plan and a commit message | a peer re-ran the grep rather than relaying it; verified here against the struct |
| 2 | the plan's M4 specified on `availability`, a local that is never returned — unobservable as written | pre-dispatch verification against the code |
| 3 | the plan's M3a negative control neutralising one of two halves; implemented literally it counted 1,435, not 0 | the implementer writing the control **before** the live sweep |
| 4 | Task 4's whole premise — "a predator guild with no base under it" — false, because the sea has no trophic height at all | pre-dispatch premise check, which contradicted the plan rather than sharpening it |
| 5 | the roster count asserted as 41 kinds | the test's own `assert_eq!`, read after a line-range grep had spilled into a neighbouring registry |
| 6 | six marine kinds reported as all-endothermic | re-enumerating the occurrences instead of anchoring on the first |
| 7 | the habitat store's size asserted as 5 when it was 11 | the absorb agent re-deriving all three counts from the data instead of trusting the brief |
| 8 | a doc claiming "the shipped `possess` command is unchanged" when it was not — in a document whose subject is that exact hazard | the reviewer running `possess --seed 42` |
| 9 | two guards with no teeth: one satisfied by `129 != 90` before a single value was read, one whose own witness the fix had removed | the reviewer mutating them (comparing a world against itself; reverting the fix) |
| 10 | a fixture's prose falsified by this campaign's own correction, three sites, outside the workspace, with all nine of its tests still green | the correction's blast radius being swept deliberately |
| 11 | a drafted process remedy — "make a frozen population derive rather than be transcribed" — that would have removed the guard it was repairing | a peer checking a remedy that had only been asserted |

Rows 1, 5, 6 and 7 are one shape: **an enumeration bounded by `-A N`, by a line
range, by the first matching occurrence, or by a doc comment standing in for the
data.** Four instances in one campaign, three of them the controlling session's,
and the first of them travelled into a spec, a plan and a commit message before
anyone re-ran it. The rule is already in this project's notes — a truncated
search proves presence, never absence — and knowing it did not prevent a single
one. What prevented them was someone else re-running the query.

Row 8 deserves its own line. The campaign *named* the hazard ("a record that
outlives its subject produces wrong answers from readers acting in good faith"),
wrote a document about it, and committed the identical defect inside that
document, ten minutes after writing the warning. **Naming a failure mode does not
immunise you against it.**

## The stage-boundary cadence was missed twice, and the price is measurable

CLAUDE.md says campaign branches absorb main at every plan-stage boundary. This
campaign deferred the first absorb through three tasks, on the reasoning that
absorbing would disrupt a running implementer. Drift went **34 → 179 commits**,
and the absorb carried **63 conflicts, 36 of them source** — the species
registry, the language cohort table, worldgen and the vessel. Absorbing at each
boundary would have met the four peoples that landed in parallel against a much
smaller diff.

Then it happened *again*. The ruling that deferred it said "absorb and submit a
stage gate before Task 4". The absorb landed; **the submission never happened**,
and the branch had drifted 93 commits behind by the time Task 4 opened. A
deferral that names its own discharge point still needs something to check that
the discharge occurred; nothing did, and the deferral read as closed because the
first half of it had been.

The compounding cost is the one that is easy to miss. A larger absorb is not
merely more conflicts — it is the only place where two campaigns' *semantic*
interaction can surface, and the larger it is, the more of that surfaces at once.
This absorb is where a preregistered invariant from a third campaign fell, and
neither branch's gate could have caught it.

## The shared worktree's index is a shared object, and it bit twice

Both times the same way: a controller edit left **staged** when a pre-commit hook
failed on an implementer's mid-write tree. A staged file is worse than a merely
modified one, because `git commit` takes the whole index — so the controller's
edit would have landed inside the implementer's task commit with no warning and
no conflict.

The rule that came out of it: **in a shared worktree, never leave the index
populated across a subagent's turn**, and commit with explicit paths
(`git commit --only <paths>`) rather than a bare commit. The implementers that
followed did exactly that and verified it with `git show --stat` on each commit.

## A restore that looks more trustworthy than an edit

Verifying a gate both ways used `sed -i.bak` to mutate and `mv …rs.bak …rs` to
restore. **`mv` carries the backup's mtime**, which predates the edit, so cargo's
freshness check rebuilt nothing and the next three commands — a clippy run, a
green `gate-commit`, and the first confirmation run — all executed the *mutated*
binary against source that said otherwise.

Every check a reader would make agreed and was wrong together: clean `git
status`, a `grep` showing the correct constant, a green gate. The only tell was
inside the panic text, a number the run's own source could not have produced.

This is the stale-binary trap in the variant where **the restore makes the tree
look *more* trustworthy, not less.** Restore a mutation with `git checkout --
<file>`, which stamps now; if you must restore by `mv`, `touch` afterwards.

## Writing the control first is what made the measurement honest

Twice, and in opposite directions. The M3a negative control, implemented before
the live sweep, counted 1,435 where the plan predicted 0 — which exposed that
freezing vent phase freezes succession and leaves migration running, a defect in
the plan's text rather than in the world. And Task 4's premise check, run before
the roster was authored, found that the measurement the task was built around
would have passed on an empty roster, a nine-kind roster, and every roster in
between.

Both are the same discipline: **a control written after the result is a control
that gets believed.** The campaign's two headline findings are both nulls, and
neither would have been legible without a control that could have gone the other
way.

## The board was posted to all day and read once, at the end

Three findings were sitting there, and one of them was a live defect in code this
campaign had just shipped — a selector filtering on biome where it needed realm,
reachable the moment four subterranean peoples landed on main. It was found by a
board notice, not by review.

**Posting is not reading.** The session-start render is one sync behind, so the
board a session reads at start reflects the previous session's sync; reading it
deliberately, mid-campaign, costs one command and paid for itself three times in
one evening.

## Two campaigns talking beats one campaign being thorough

Every error in the day's cross-campaign exchange — three bounded-window reads on
this side, two context errors on the other — was found by the *other* side
re-running it. **Neither side caught its own.** That is not a fact about
carelessness; it is a fact about what self-review cannot do.

The strongest instance is row 11 above: a process finding was one filing away
from publishing a remedy whose effect would have been to delete a guard and keep
the green. It was caught because a peer checked a remedy that had only ever been
asserted. **A cited mitigation is a claim until someone runs it.**

## A measurement is a claim with a date, and both sessions forgot it

A peer carried a statistic through three consecutive ledger entries and a
dispatch brief; the figures came from a measurement taken before the world moved
twice. Their instruction attached to it ("never re-pin this") was *right*, and
only its stated reason was stale — which is the worse failure, because nothing
about it looks wrong.

This session's own version: an implementer reported a workspace suite at
2,179 s against 1,315 s earlier for essentially the same suite, and **flagged its
own number as untrustworthy** rather than reporting it flat — 64 slow tests
against 23, on a shared laptop, with no work of that scale added. Recording both
readings and saying which to trust is what kept a 1.66x contention artifact out
of the ledger as a cost regression.

## The routing failure this retrospective has to own

Task 3's review closed with **nine minors deferred to "the final whole-branch
review"**. That review never happened. The nine were never enumerated in the
committed ledger, only in a reviewer's report that lived in the campaign's
scratch, and the scratch dies with the worktree — so their text now exists
nowhere.

The deferral was legitimate and the destination was not. "Defer to a later
review" names an *event*, and an event that nobody schedules cannot be checked
for. The Cartulary's rule is the right one and this campaign half-applied it:
the campaign ledger is committed and slug-keyed precisely so a deferred finding
survives its worktree, and every ruling that went into it did survive. **Route a
deferral to a file, never to a future meeting.**
