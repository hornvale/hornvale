# The Stilling — decision ledger

A census delivery commit is gated on a measurement of itself. `census_duration.rs`
reads `docs/timings.md` from the WORKING TREE; a census writes its own duration
row there before the delivery commit; so a run over `CENSUS_REFUSAL_SECS`
refuses its own delivery, strands its goldens staged in the SHARED census
worktree, and the next attempt `reset --hard`s that worktree — destroying the
previous attempt's output — and refuses again.

Observed, not theorised: campaign/the-tidemark's 1898.859 s run on 2026-09-13
was refused, and its 109 moved goldens were destroyed by the following run,
at ~30 minutes of canonical-box time per attempt.

---

#1 [Q] — **Is this a defect or the guard working as intended?**
· **Decision:** a defect. · **Why:** `scripts/hooks/pre-commit` already stands
down the OTHER half of this same tripwire for a delivery, by name, and its
comment block describes this exact hazard and its cost — *"Two censuses blocked
in fourteen hours (campaign/the-plat 1019.1 s, campaign/the-weft 1188.9 s with
70 goldens stranded) before it was diagnosed."* The alarm was excluded for
precisely this reason; `the_latest_census_is_under_the_refusal_ceiling` was
added later (The Sluice), reads the same latest row of the same file, and was
never added beside it. Root CLAUDE.md's rule adjudicates it: *"a delivery
SATISFIES every check a regeneration remedies and DEFERS only what needs a
human re-statement."* · **Alternatives discarded:** "the ceiling is meant to be
a hard stop, leave it" — rejected, because the stop is not removed, only moved
to the merge of the delivery branch, which is the deal the other three
deferrals already make. · **ideonomy passes / overturns:** 1 / 1. The pass
overturned the framing: the first reading was "a slow census cannot deliver",
and the sharper one is **the deadlock's only exit is a guard change, so it
manufactures on demand exactly the shape decision 0016 exists to prevent** —
the campaign that needs the threshold raised is the campaign that raises it.
· **Capture:** board post `5aba5980e7ef`; this entry.

#2 [Q] — **campaign/the-tidemark raised the duration pair to escape it. Is that
the abuse this predicts?**
· **Decision:** no, and the record says so in public. · **Why:** I read
`4d46c14e8` before objecting. It names the doctrine forbidding the easy version
(*"raising because a run went red is the flap-hiding move this file has already
refused once"*), explicitly refuses to derive from the red reading (*"Explicitly
NOT set from 1898.859: one reading is not a normal"*), derives instead from the
established five-run series (1366–1624 s, cpu_ratio flat at 29.9–31.9, so work
not contention), sets RED inside the documented band, names the cause, and
carries 44 lines of derivation plus 52 of ledger. That is the sanctioned path,
done unprompted by a campaign with every incentive to do the cheap thing.
· **Why it still matters:** a correct instance does not make the structure safe.
The next campaign in that position may not write that commit.
· **ideonomy passes / overturns:** 1 / 0. · **Capture:** correction and credit
posted to the board (`9b6b7e92f51a`); this entry.

#3 [G1] — **Mechanism: how should the second check be deferred?**
· **Decision:** widen the existing `docs_tests_exclude` to carry a nextest
FILTER EXPRESSION naming both tests, rather than adding a fourth stand-down
branch. · **Why:** the variable held a bare test name, and `not test($VAR)` can
express exactly one exclusion — which is the mechanical reason the ceiling
could not join the alarm beside it. An expression costs the same and has no
arity. It also keeps `scripts/test-census-guard.sh`'s "exactly three stand-down
branches" assertion green *for the right reason* (there really are three), not
by editing the expectation. · **Verified, not assumed:** `cargo nextest list`
with the new expression selects the two instrument tests and excludes the two
threshold verdicts; without it, all four appear. · **Alternatives discarded:**
excluding the whole `census_duration` module (over-broad — it would defer two
checks a delivery CAN satisfy, the failure mode on the other side of the rule);
a fourth stand-down branch (same effect, more surface, and it would have
required editing an assertion that was correct).
· **ideonomy passes / overturns:** 1 / 0. · **Capture:** this entry.

#4 [G1] — **What stops this recurring a third time?**
· **Decision:** derive the required stand-down set FROM THE SOURCE rather than
hard-coding two names: every test in `census_duration.rs` that compares against
a `CENSUS_*_SECS` threshold must be named in the hook's exclusion, and every
name in the exclusion must still exist. · **Why:** the defect has now occurred
once and a hard-coded pair would sit green forever while a THIRD threshold test
walked into the identical trap. The distinguishing property is comparison
against a threshold constant, not use of the `latest_by_timestamp` helper —
`the_chronologically_latest_row_wins_even_when_it_is_not_last_in_the_file` calls
that helper on synthetic rows and is correctly NOT excluded, so the obvious
derivation over-selects. · **Both directions are held:** under-deferral (the
original bug) and over-deferral (standing down a check a delivery can satisfy).
· **Mutation-tested in both directions rather than asserted:** restoring the
exact pre-fix exclusion reddens with the named test; adding an instrument test
to the exclusion reddens the over-broad check; restoring leaves it green. So
this guard would have caught the original defect.
· **ideonomy passes / overturns:** 1 / 1 — the pass overturned "assert the two
names" into "derive the class from the source".
· **Capture:** this entry; `scripts/test-census-guard.sh`.

---

## Follow-ups

- **Refused deliveries are still perishable.** This fixes the duration deadlock,
  but any OTHER refusal of a delivery commit still leaves goldens staged in the
  shared census worktree for the next run to destroy. `sluice-census.sh` should
  push a rescue branch on refusal rather than leaving work staged; not done
  here, to keep this change to one subject.
- `make sluice-drain` has no passthrough for `--allow-orphan`, so the Makefile
  target cannot express the flag a standing drainer actually needs. Found by
  using it: the consolidation on 2026-09-13 had to call the script directly.
- The duration pair on main (1320/1650) is now behind campaign/the-tidemark's
  re-derived pair (1630/2040), which lands with their delivery. Nothing to do,
  but a reader comparing the two before that merge will see a discrepancy.

## Follow-up found by using the tooling (2026-09-13, after the commit above)

**`box_is_busy()` in `scripts/sluice-drain.sh` has a blind spot, and it is the
one that function exists to close.** It asks `census-run.sh status` and greps
`running:` — which reads the CLAIM FILE at `/tmp/hv-census.claim`. But a census
DELIVERY holds the box lock while re-authoring the gnomon arms and deliberately
writes no claim file for that window; its own log says so: *"holds the box lock
for the arms after 0s queued (no claim file — see the header)"*.

So during the arms phase the box is busy and every claim-file-based check says
free. Observed twice within ten minutes on 2026-09-13:

- a probe waiting on `census-run.sh status` started a heavy build at 20:37
  while campaign/the-trencher's census was still in its arms phase, and had to
  be stopped by hand;
- the merge drainer claimed `tooling/the-stilling`, marked the row `running`,
  and then sat in `flock -w 7200 9` — which is exactly the ghost-`running`-row
  unreadability `box_is_busy` was written to prevent.

Nothing is corrupted by this: the `flock` still serializes, no row runs twice,
and `sluice-run.sh` touches the shared worktree only under the lock. It is a
reporting-honesty defect, not a safety one.

**The fix is to ask the LOCK, not the file** — the same correction this
campaign's sibling made about drainers (a lock cannot go stale; a file can be
absent or left behind). A non-blocking `flock -n` probe on
`/tmp/hv-census.lock` answers "is the box busy" for every holder, claim file or
not. Deliberately not done in this commit: it is a third subject, and the
function is advisory — being wrong costs readability, never correctness.

**The transferable half:** I wrote `box_is_busy` from the interface that was
there (`census-run.sh status`) rather than from the invariant I wanted (is the
lock held). The interface answers a narrower question than its name suggests,
and the narrowing is documented in a log line nobody reads until they are
already confused.
