# The Penstock — retrospective

**Merged:** 2026-08-23, `132854255`. Stage 1 of the derived-working-set
program (`docs/superpowers/specs/2026-08-22-the-penstock-metaplan.md`).
Process lessons only; what shipped is in the chronicle.

## The shape of the campaign was decided by one measurement, taken early

The brainstorm's first instinct was to design the cache. Instead the session
built a throwaway probe and measured, and the numbers reordered the entire
plan before a line of it was written: the largest available win was a
**call-site fix** (1,088× at 1M facts), not a subsystem, and the cache
machinery competed for the gap between 17.8 ms and 44 ms. Stage 1 became a
bug fix plus instruments, and the cache moved to stage 2 behind a
measurement gate.

**Keep doing this.** The cost of the probe was twenty minutes. It changed
what five tasks were.

## Four ways I verified the wrong thing

These are one lesson with four instances, and it is the lesson of this
campaign.

1. **I checked that functions *take* a roster, not that they are *called*
   per creature.** §6.2 named `hazard_memory_memo` and `alarm_field_memo` as
   the O(A²) suspects. Both are hoisted to once-per-tick, and
   `liveness.rs:4670` says so in capitals — a comment I had read earlier in
   the same session. `grep 'roster: &[Npc]'` answers a *different question*
   from the one I claimed to be answering, and I reported it as
   "structural support". Caught by the whole-branch review, as a Critical.

2. **A single-line grep asserted completeness.** The plan said five matches,
   two production. The real count was fourteen. `dispatching-hornvale-
   subagents` says verbatim that "a brief that enumerates call sites is
   asserting completeness, and completeness is what enumeration gets wrong" —
   I had read that skill the same day.

3. **The correction was itself wrong, and propagated three levels.** The
   review said "twelve, of which two were fixed" while listing twelve that
   *excluded* the fixed two. I copied it into a fix brief; the fix wave
   copied it into the spec, the plan, and a commit message. Four artifacts,
   one off-by-two, caught only when a re-review re-derived the count from
   scratch instead of reading the prose.

4. **I ran an hour of tests in the wrong directory.** A tool result said
   "Shell cwd was reset to …/hornvale" — the main checkout — and I did not
   register it. Every post-edit timing test executed main's *unmodified*
   copy of the script I had just fixed, so I concluded my own fix had failed
   and went hunting a second bug that did not exist. `dispatch-preamble.md`
   makes subagents echo `pwd` as their first action precisely against this.
   **Nothing imposes that discipline on the controller.**

**The generalisation:** each time, a cheap check was run and its result was
mapped onto a claim it did not support. The remedy is not more checking, it
is naming the proposition first — *what exactly would this command prove?* —
and only then choosing the command.

## Three diagnostic traps, all of which cost real time

- **`ps | grep -E 'cargo|rustc'` is the wrong liveness probe** when the work
  is bash. It reported "nothing running" while a bash process burned 100% of
  a core for an hour, and I diagnosed a parked subagent. **The right probe
  was `docs/timings.md`**: `timed.sh` writes only on completion, so one grep
  distinguishes "gate still running" from "gate finished and something else
  is wrong." Nothing else visible from outside the agent makes that cut.
- **Piping a long-running command through `tail` hides all of it.** `tail`
  buffers until stdin closes, so a run killed on timeout prints nothing —
  and I twice read that silence as "it hung before the first line." Redirect
  to a file and poll it.
- **A killed background command can leak.** A `ps … args` sweep found a
  1h37m 100%-CPU process from an earlier backgrounded run that nothing had
  reaped. Sweep after any kill.

## macOS bash 3.2 is a defect *class*, not a handful of defects

Three independent failures in one session, from the same source, all
**clean under shellcheck** and all fine on lefford's bash 5:

- `scripts/test-worktree-freshness.sh` does not *parse* under 3.2 (a U+2026
  in a single-quoted awk body), so the guard that catches stale-name
  contamination is dead on the only box that runs it. **Still unfixed** —
  see the registry row.
- `scripts/subfloor-run-chunked.sh` was superlinear (~O(n³)) and
  locale-sensitive (12× between `LC_ALL=C` and UTF-8) on a 229 KB string, so
  `git commit` on a Mac took minutes to hours **in bash, with no compiler
  running**. Fixed in this campaign.

And the third surfaced during this very close, which is how the count went
from two to three inside an hour: `scripts/hooks/post-merge:67` calls `mapfile`, which is bash 4+, so the hook dies mid-body
on darwin. It always exits 0, so the artifact-staleness advisory it exists to
print has simply never printed on a Mac and nothing said so.

The prior fix round of that same file reasoned correctly about which
constructs bash 3.2 *supports* and never measured what one of them *costs*.
**"bash 3.2 supports this" and "this is usable at our input sizes on bash
3.2" are different claims, and only the first is cheap to check.**

## A green gate is not evidence about a new test

A test with no recorded baseline duration is excluded from `gate-commit` by
design, so the new tests in Tasks 1, 2 and 4 could all have shipped
unverified behind a green gate. Each was run explicitly. This is documented
in root CLAUDE.md and is easy to forget precisely because the gate is green.

## What went right, and is worth copying

- **Implementers reported inconvenient results rather than tuning them.**
  Task 3's implementer said its harness could not discriminate — a defect in
  *my* brief. Task 4's reported a flat rate that falsified the program's
  hoped-for outcome. Both were told in their dispatches that a falsifying
  result is a success; both behaved accordingly.
- **Measuring the fix caught a half-fix.** Rewriting only the separator step
  in awk still took 8m24s. Declaring victory after the plausible fix would
  have shipped a 500× regression against the real one.
- **The reviews earned their cost.** The whole-branch review found a
  Critical that three prior passes missed; a scoped re-review then found two
  defects the *fix wave* introduced. Each layer caught what the one before
  could not see.

## Process defects to fix

- **The close was not part of the merge.** `closing-a-campaign` puts DoD
  artifacts on the branch (step 3) *before* submitting (step 6). This
  campaign submitted first, so the chronicle and this retrospective need a
  second merge. Nathan called it out. Guidance updated in
  `submitting-to-the-sluice` so the merge path names the precondition
  instead of leaving it to be remembered.
- **Board notes must contain no backticks and no double quotes** — `make
  board-post` re-expands the recipe through a second shell, so backticks
  become command substitution (one ran `mv`) and double quotes word-split
  into bogus `key=value` args. CLAUDE.md documents the `FIELDS`/`PATHS`
  quoting hazard but not this one.
- **Reviewer dispatches carried no no-subagents clause.** One reviewer
  dispatched its own research subagent — harmless, but an unbudgeted seat.
  The clause was in implementer dispatches only.

## Deferred, with homes

- Task 2's landed commit message says `facts_about` where the code says
  `facts_of`. The number it cites genuinely came from `facts_about` (the
  §4 baseline comparator), so the attribution is right and the sentence is
  still misleading. Not worth a history rewrite. **Shipped as-is.**
- Ten unindexed `find(pred).filter(subject == e)` call sites remain, several
  hotter than the two fixed. Registry row `TOOL-remaining-subject-scans`.
- `scripts/test-worktree-freshness.sh` still does not parse under bash 3.2.
  Registry row `TOOL-bash32-script-class`.
