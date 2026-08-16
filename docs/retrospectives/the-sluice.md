# The Sluice — retrospective

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-sluice.md): a serial merge queue
that gates the merge product rather than a branch tip, the incomparability
argument behind decision
[0139](../decisions/0139-main-advances-only-through-the-lock.md), the first
real merge through it, and the deletion of the lane's dispatch layer
(decision [0140](../decisions/0140-the-stage-gate-is-a-kind-of-queue-request.md)).

## 1. Fourteen defects trace to the controller's own plan text

Not to implementers' judgement. Every one was a claim, a predicate or a
guard I wrote into a plan or a task brief, transcribed faithfully, and found
by someone executing it. The running list, in the order they were found:

`set-state`'s `set -e` trap; unsanitized `note` corrupting the queue file (a
class, not a field — `branch` and `sha` had the same hole after the first
fix); `mktemp` defaulting to a different filesystem, making the queue's
rewrite non-atomic; a "same branch, non-ancestor" test that used a different
branch, so the ancestor guard was never isolated; a hardcoded three-script
list in the claim-shape test, which the chamber's own writer would have had
to be added to by hand — the exact drift that test exists to prevent; a
vacuous conflict test with an unpinned `HV_SLUICE_BASE`; an exit code
conflating "conflict" with "broken base"; the `case "$sha" in [0-9a-f]*)`
glob that anchors only the first character (twice — Task 3 fixed it, then
Task 6 reproduced it because my dispatch pointed the implementer at the
older file it was modelled on); "neither assertion subsumes the other," which
is backwards; a comment asserting the push is "gated on TWO facts" when both
guards were unreachable; a signpost recipe echoing to stdout while telling
the implementer to match a sibling that uses stderr throughout; a merge-subject
check grepping for `^Merge branch` against a chamber that merges a raw SHA
into a detached HEAD, where git's default is `Merge commit '<sha>' into HEAD`
— a guard written against a situation that does not occur; and a Task 10
sweep list I got wrong in both directions, inside a plan step whose job was
preventing exactly that kind of drift.

Two of those are guards that could never fire, and one is a check for a
situation that cannot arise. That is the family this project has tracked
since The Timekeeper, arriving again from the same place it always arrives
from.

The existing rule — *ground a plan's snippets on a real repo helper and cite
it by file and line* — is necessary and did not help here, because most of
this code had no existing analogue to ground on. It was new machinery. The
rule that actually emerged, and that every implementer should be told
explicitly in its dispatch:

> **A plan's reference code is a sketch to be reviewed by the implementer,
> not transcribed.** Plan-authored code is the least-reviewed code in a
> campaign.

Both implementers who treated it that way improved on the brief. One
replaced an argument I asked it to validate with a value derived from the
commit itself — an argument can diverge from what it describes; a derived
value cannot.

A related failure of my own, cheap this time: I dispatched Task 4 naming a
brief file I had never generated, while instructing the implementer not to
read the plan. It fell back to the plan's own section, applied the
corrections, and flagged the discrepancy rather than improvising. Generate
the brief *before* writing the dispatch, not after — a dispatch that names a
missing requirements file and forbids the only other source can leave an
implementer working from nothing but the prompt.

## 2. Three rounds of one task, three times the test diverged from production on exactly the axis the fix was about

Task 4's chamber has an injectable-phases design so its behaviour can be
driven against a scratch repository. The seam is good and it was also the
mechanism of the failure, three times running:

- **Round 0 — tracked versus untracked.** Every phase writes the timings
  ledger, a *tracked* file, into the chamber. `git clean -fd` removes only
  untracked files, so the modification survives and the mutation audit,
  which refuses on a dirty tree, fails on every real run. The scratch repo
  has no tracked timings ledger, so the same write created an *untracked*
  file there and `clean -fd` removed it. The one axis the bug lived on was
  the one axis the seam simplified.
- **Round 1 — the hook path.** The fix's own commit inside the chamber ran
  the repository's commit hook, which runs a gate, whose timing wrapper
  appends to the same tracked ledger *after* the staging step. The scratch
  repo sets no hooks path. Same divergence, one layer down.
- **Round 1 also — a signal handler that could hold the machine forever.**
  The round-0 bug released the claim too early; its fix could hold it
  permanently, because the handler sent one signal and then waited with no
  deadline and no escalation, and this queue has no force override.

The rule this suggests is not "write more tests":

> **A test seam that is simpler than production in the way that hides the
> bug is worse than no test, because it produces evidence.** The scratch
> fixture must be configured like production in every respect the fix
> touches, and the fix's own test must be shown red against that
> configuration before the fix lands.

I also overstated the third one in two dispatch prompts. The wedge needs a
surviving process *group*, not a signal-trapping direct child; in the test
the direct child dies normally and the unbounded wait returned in a tenth of
a second. Recorded because the correction came from the re-review, not from
me.

## 3. A prompt is not a control: an agent force-pushed over the trunk

While building an adversarial case for a fix round, an implementer wrote a
standalone diagnostic to probe how bash resolves quote splitting in a
`git p''ush -f origin HEAD:refs/heads/main` form, and ran it with the working
directory defaulting to the real checkout. It executed against the remote:
`+ 21847b08...fb71dd2e HEAD -> main (forced update)`.

The dispatch had named that prohibition as *the single most important
constraint*, in a boxed warning at the very top, with three sub-rules
including "print `git remote -v` and confirm it points at your temp bare
repo."

Handling was correct and is why this cost thirty minutes rather than a day:
the agent halted immediately, attempted no remediation, reported precisely,
and said it could not choose between two candidate restoration SHAs and
wanted direction. Nothing was ever unreachable — the displaced tip remained
referenced on the remote under another branch — and restoration used a lease
pinned to the exact bad value, so it would have refused if anything had
moved.

The lesson is not "be more careful," and it is not about this agent:

> **Prose in a prompt is not a control.** The agent was not careless about a
> rule it had read. It wrote a *diagnostic* — a category its own mind did
> not file under "the push tests" the rule spoke about.

The control that replaced it is environmental: `scripts/hooks/pre-push`,
which refuses a force-push or a delete to a real remote, verified against
the real remote. Two stronger controls remain candidates and are in the
follow-ups: a `PATH` shim for `git` that refuses a push to any non-`file://`
remote without an explicit opt-in, or running such tasks in a clone whose
origin *is* a scratch bare repository, making the real remote unreachable by
construction.

This is the same lesson the campaign kept relearning one level up — a guard
described in prose that nothing enforces is not a guard — and it has now
cost a force-push to the trunk, which is a considerably more expensive way
to learn it than the four times it appeared in code review the same day.

## 4. A static detector over a language is only as good as its model of that language

The same task carried a test-only lint that scanned one file for pushes to a
real remote. It was defeated three times, and **not once by a missing
pattern**:

1. A denylist that could not enumerate shell syntax.
2. A hand-rolled word-boundary regex that matched nothing at all.
3. A line-continuation joiner that disagreed with bash about whitespace. It
   wrote `line="$buf $line"`, inserting a space where the continuation was;
   bash *deletes* the backslash-newline and concatenates directly. So
   `git pu\` / `sh -f origin …` really executes as a force-push while the
   joiner produced `git pu sh -f origin …` and the auditor reported zero
   violations. The inserted space went unnoticed precisely *because*
   whitespace runs were normalised for the comparison, which hid the
   modelling error in the common case.

It was deleted rather than patched a fifth time, and the reason is worth
recording: it had acquired a **worse security property than the thing it
protected**. An unbalanced brace in the audited file closed the test wrapper
early, so content ran at `source` time — a proof of concept created a file
and attempted a real push — while the audit reported clean.

The implementer did not merely comply with the delete instruction. It
searched for something the lint caught that the runtime hook does not, found
one candidate (an ordinary non-force push to the wrong destination), and
confirmed the chamber's own tests already cover it; the re-reviewer then
verified that claim independently against the real assertion. That mattered:
a comment crediting coverage that did not exist would have recreated, in
prose, the fault this campaign found five times in code.

## 5. The campaign's own success metric was wrong, and the correction is the finding

The spec's section 3a originally committed the campaign to ending with
**fewer lines of process machinery** than it started with, on the reasoning
that a merge queue costing more scaffolding than the lane it replaces has
relocated the payment rather than removed it.

Nathan corrected it mid-flight: more code does not mean less simple, and the
real change is *one session managing one machine* instead of many sessions
coordinating another one. Both numbers belong here, and neither is
suppressed:

| measure | before | after |
|---|---|---|
| `make` targets in the landing path | 12 | 5 |
| …write path only | 7 | 3 |
| dispatches per stage checkpoint | 4 | 1 |
| job ids / logs to read per checkpoint | 4 | 1 |
| machines a campaign reasons about | 2 | 1 |
| scripts in the request path | 3 | 1 |
| durable records that a job existed | 2 (after it finished) | 1 (from enqueue) |
| lines deleted | — | 950 |
| lines, all of `scripts/*.sh` + `Makefile` | — | **+2,789 net** |

Of the added lines, 1,910 are `scripts/test-sluice.sh` and a large share of
the chamber's 652 are comments carrying four review rounds' reasoning. The
line count is the wrong instrument and it is reported anyway, because the
campaign publicly committed to it and a suppressed number is worse than a
refuted one.

One residue counts honestly against it: the sub-floor chunking fix
(`Makefile:subfloor-run` plus `scripts/subfloor-run-chunked.sh`, 91 lines)
was out of brief, was kept anyway at a G5 stop because without it no Rust
commit can be made from lefford at all, and is **not** on the deletion list.
It is permanent new machinery this campaign added.

## 6. The campaign lived its own thesis before it could benefit from it

Task 0 fixed a claim file that the status command could not parse — so
`census-run.sh status` reported "no heavy run in progress" for the entire
duration of *every* lane job, on the exact command the contributor guidance
tells you to run before consuming the box.

The fix was green on the branch and **inert**. The dispatch path runs the
script from the primary checkout, which tracks the trunk, so every job kept
executing the old code. A fix that is green on a branch has no effect on
reality until it lands, and nothing gated the landing. The campaign spent
its first task fixing a bug it then could not benefit from, for exactly the
reason it exists.

## 7. Where I was wrong, and who caught it

Kept as a list because the shape repeats and flattening it would make the
record read as though everything was understood the first time.

- **"The chamber solves the sub-floor roster problem for free."** I
  ledgered this after Task 4 and it was false: `ci-record` refuses whenever
  any claim is held and the chamber always holds one. Task 12 then found my
  *correction* was also wrong, in the other direction — there had been no
  path at all, because the lane held a claim too, so the copy-out built to
  rescue the file was gated on a change that never happened. The file has
  one commit in its entire history. Two published explanations, both
  downstream of the real cause, which was one step earlier than either.
- **"`gate-suite-run` rewrites the tracked type-audit report."** A
  reviewer's claim; I repeated it in this campaign's ledger and in two
  dispatch prompts before anyone checked. It regenerates into a temporary
  file and only diffs it. The underlying finding was real with two tracked
  writers, not three — a finding can be right about the bug and wrong about
  the evidence, and it took both reviewers to get it straight.
- **"Neither assertion subsumes the other."** A clean working tree strictly
  implies a clean diff over a subset of it, so the drift check I specified
  was unreachable. It was deleted rather than left as decorative protection,
  and the chamber now documents that it cannot detect a phase silently
  failing to regenerate an artifact.
- **The census tripwire is fitted to a point, not a distribution.** I set a
  900-second ceiling against three observations of 949, 920 and 882 — fitted
  to the lowest — and framed the thinness as a feature ("a live tripwire,
  not a formality"). The better framing came from the campaign whose merge
  it held: *a threshold is only a decision rule if the instrument can
  resolve it.* The spread is ±4% with no code change between runs.
- **A wrong claim of mine is still on the shared board.** A peer post
  asserts the audit's dirty-tree residue is a *tracked* modification, and
  therefore that the previous campaign's fix did not close it. That is
  backwards — `clean -fd` removes only untracked files and demonstrably
  works, which is itself the evidence the diagnosis was "untracked." I
  flagged the post as stale hours before and never refuted this specific
  claim, and it sits on the channel every session reads at start.

## 8. What held up

**The review loop found what neither the implementer nor I did, repeatedly,
and the reviewers corroborated rather than echoed.** One reviewer reached the
BSD-`sed` critical independently of my own finding, having been dispatched
without being told what to look for. Another reproduced the queue-corruption
critical rather than reasoning about it. A third proved a hermeticity test
was real by asserting the exact string only the intended repository can
produce, rather than merely "not the decoy."

**Naming the property instead of prescribing the patch produced better
fixes.** For the dirty-tree critical I named the constraint — every phase
starts clean *and* legitimate regenerated artifacts still reach a commit —
and refused to prescribe the mechanism. For the joiner I named the
property (reproduce bash's semantics: concatenate with nothing between) and
offered an explicit escape hatch: say a line-oriented detector cannot be
made correct for this, rather than producing a fourth round of patches. The
implementer took the hatch, correctly.

**Mutation as a standing dispatch line kept paying.** Six tests found in this
campaign could not fail; three of the six were caught by the implementing
agent itself. Every absence-shaped assertion in the chamber's harness now
carries a mutation demonstration — including "a stage request never
pushes," proved by deleting the gate and watching the mutant push.

**Two findings came from running the thing rather than reading it.** The
chamber was committed non-executable, `100644` against four `100755`
siblings — harmless today only because it is invoked as `bash <script>`, and
invisible to all 131 tests because every one of them invokes it that way.
Thirty seconds against a clean checkout found it. And the timings ledger
turned out **not to be chronological**: sixty-plus timestamp inversions from
merges interleaving parallel branches, so the census tripwire reading "the
last line" could have read a stale row as the latest — green on an old fast
run while a slow new one sat above it. The check enforcing this campaign's
own rule could have been silently defeated by the exact phenomenon the
campaign exists to address. An append-only ledger is only chronological if
nothing ever merges into it.

**Sequencing the deletion after the first real merge was right.** The lane
was the only way to run anything expensive until the queue demonstrably
worked, so a queue that had not landed a merge did not get to delete its
predecessor. It held on its first candidate, which is precisely when that
ordering mattered.

## 9. The gap accepted deliberately

Spec §5.3.3 — *a behavioural fix is committed to the candidate's branch,
never amended into the merge commit* — has **no mechanical enforcement**. It
is an operator rule enforced by review. Mechanising it would mean the
chamber inspecting the provenance of its own commits, which is more
machinery than the risk warrants at three and a half merges a day. It is a
known hole, recorded here rather than left implicit, and it is the decision
to revisit if the rule proves hard to hold: the `--no-ff` topology is what
makes it a rule rather than a property, and under the fast-forward topology
that was given up it would have been true by construction. Note that history
is not retroactively fixable, so revisiting means changing topology going
forward, not repairing what landed.

## Follow-ups

Promoted from the campaign's own register before teardown.

**Queue behaviour**

- **A/B attribution for queue reds.** The first real merge held an innocent
  candidate for a defect pre-existing on the trunk (`UNGUARDED
  ledger_day_of_bake_year` at `windows/worldgen/src/person_promote.rs:303`,
  untouched by the candidate — since closed at `a12cfbb7`). The queue should
  re-run a failing phase against the trunk alone and exonerate the candidate
  when the finding reproduces. It already holds both trees, so it is the only
  thing here positioned to do this automatically.
- **A decision number wants assigning at merge time.** The no-gaps and
  no-collision invariants on `docs/decisions/` are mutually exclusive under
  parallel campaigns, and the gap check pushes an author into the collision
  the uniqueness check exists to catch. The queue is the only mechanism that
  could assign at merge.
- **The queue's request id collides** for two adds of the same SHA inside one
  second (`req-<sha12>-<second-resolution timestamp>`). Harmless in
  production; id lookups are not uniquely keyed.
- **No on-demand single-set dispatch survives.** `make lane SET=artifacts`
  after an absorption has no replacement but an operator on the box running
  that set's own command. Accepted in 0140; the first person to want it will
  notice.
- **A stage checkpoint now costs a queue slot**, and campaigns used to fire
  the old one fairly casually.

**Artifacts and instruments**

- **Two heavy-tier artifacts cannot go red.** `occupancy.csv` and
  `repose-exposure.csv` assert against `include_str!`; `the-history` and
  `the-sounding` only `fs::write`. They went stale through a green 80/80 run.
  If a test authors an artifact it must also assert it.
- **The census epoch label is wrong.** `tools/census/history.sh` reads the
  first-parent subject, and the chamber's artifact commit sits above the
  merge. Pinning the merge subject's shape is necessary and not sufficient.
- **The census tripwire's ceiling sits inside the instrument's noise**
  (882–949 s, no code change). Deferred by Nathan pending settling into the
  workflow; it is currently at a temporary 1050 with an expiry. Options: keep
  it, widen it to clear the spread, or denominate against `cpu_ratio` so
  contention and regression separate.
- **`docs/timings.md` rows are keyed by run ref, not by the commit a campaign
  cites**, so "most recent successful row" and "the census this campaign ran"
  are different identifiers.
- **`history-myth-hop-median` is 19.2% of every census sweep** from one
  column, quadratic in a lineage tree that now has ~900 nodes, and its
  registered cost claim understates the measured figure by ~18×. A `children`
  adjacency map and a `depth` map built once at construction removes the
  quadratic factor; the output sorts before returning, so byte-identity is
  provable. Nothing re-checks a registered metric's cost claim.
- **Host-keyed budgets for `cli/tests/session_cost.rs`.** This campaign
  raised `TURN_BUDGET_MS` 8.0 → 9.0 on measurement, which is a compromise.
  The file already computes `bases_apply` and uses it to suppress the ratio
  verdict, then asserts Mac-derived absolute ceilings unconditionally — the
  same error, half-corrected. lefford measures ~1.9× the Mac here, so one
  global constant cannot serve both hosts.

**Controls and hygiene**

- **An environmental control against pushing to the real remote** (§3), beyond
  the `pre-push` hook now in place.
- **Nothing notices an unpushed branch.** Eighty-nine commits sat on lefford's
  disk with no remote copy, because implementers had been pushing their own
  work and the controller's direct commits had no such habit. The `pre-push`
  hook guards what you *do* push. Caught only because Nathan asked. Candidate:
  a `post-commit` warning when the branch is N ahead of upstream.
- **A bash-4+ denylist lint** (`mapfile`/`readarray`/`declare -A` in
  `scripts/**`) is ~30 minutes and would have caught one of Task 0's two
  criticals. The GNU/BSD `sed` divergence is not statically detectable —
  identical text, different runtime behaviour — so only a narrow "sed
  replacement contains `\n`" special case is buildable.
- **A nested `census-run.sh` deletes the enclosing claim.** Pre-existing;
  it is why the chamber refuses `census` as a phase outright.
- **`make shellcheck` is red at `origin/main` and nothing runs it** — two
  pre-existing SC2119 infos. It is in no set, so it has been red unobserved.
- **The hermeticity scrub unsets only `GIT_DIR`/`GIT_INDEX_FILE`**, not the
  wider set `tools/board/src/git.rs` scrubs. Worth widening the queue scripts
  together.
- **A peer's operational claim can be stale in the direction that flatters
  the campaign**, and `git status` is host-specific — a cross-session report
  about "the primary checkout" may be about a different machine. Check
  `hostname` and the path before acting on it.
- **Registry rows owed** once the uncommitted `PROC-merge-queue` row lands on
  the trunk: re-score it from `raw`, and note that `TOOL-lane-supersession`'s
  prescribed ancestry-coalescing fix is implemented for the queue but its own
  scope (the lane's dispatch path) closed by deletion rather than by repair.
