# The Warden — decision ledger

A candidate can change the gate that judges it, and nothing said so.

The chamber runs MAIN's programs against the CANDIDATE's content:

| what | read from | so a candidate edit |
| --- | --- | --- |
| `scripts/sluice-run.sh`, `sluice-drain.sh`, … | main | is inert this run |
| `scripts/lane-sets.tsv` (the roster) | main | is inert this run |
| phase list (`merge_phases=`) | a literal in main's script | cannot be changed at all |
| the Makefile target a roster line names | **the candidate** | **takes effect immediately** |
| `scripts/hooks/` | the candidate (relative `core.hooksPath`) | takes effect immediately |

---

#1 [Q] — **Should a campaign be able to change its own gate?**
· **Decision:** yes, and it already can — through the Makefile target. Nathan's
call, 2026-09-14: *"a campaign ultimately needs to be able to change its own
gate, but it's up to us to determine how reasonable that request is."*
· **Why the mechanism was left alone:** the split above is coherent — main owns
the frame (which phases, in what order, what command invokes each), the
candidate owns the content. `sluice-run.sh`'s own comment shows the same
reasoning applied deliberately to the phase list (*"THE TWO PHASE LISTS ARE
LITERALS, AND THAT IS A CHOICE, not an oversight"*). Pointing `lane_sets_file`
at the worktree would let a candidate rewrite the command that invokes its own
gate, which is strictly more power than it needs: everything a campaign
legitimately wants is reachable from the target.
· **ideonomy passes / overturns:** 1 / 1. The pass overturned my own first
reading — I had called the roster-from-main behaviour an unexamined consequence,
and verifying `repo_root`, the phase-list literals and the `cd "$wt"` showed a
consistent design I had under-credited.
· **Capture:** this entry.

#2 [G1] — **Then what makes the request reviewable?**
· **Decision:** report it in `sluice-vet.sh`, at the moment a human decides;
do not enforce. · **Why:** a gate change can be a repair, a tightening or a
retreat, and only a reader can tell which — the same reason every other line of
that script reports rather than gates. Nathan named the difficulty exactly:
*"I know that's hard to enforce."* It is; so do not try. What was missing was
not judgement but VISIBILITY. · **Alternatives discarded:** a ratchet on the
phase list (the list is already a literal in main — nothing to ratchet); a
default-deny on gate paths (a gate change is legitimate and common, so the
violation set would be large and the check would be waived into uselessness);
blocking on any gate diff (would have blocked the container fix, which is the
repair we want). · **ideonomy passes / overturns:** covered by #1.
· **Capture:** this entry; the GATE MACHINERY section.

#3 [Q] — **Report only roster-named targets, or follow the call graph?**
· **Decision:** one level of expansion. · **Why:** measured, not assumed. The
roster names `clients-check-run`, an AGGREGATE whose recipe fans out to six
targets via `$(MAKE)`. campaign/the-coherence changed `visual-check-run`, which
the roster never mentions — so the first cut of this section **missed the very
change that motivated it**, and reported only their inert roster edit. One level
catches it. · **Not more than one level:** the control test exists because the
opposite failure is worse — a section that reports every changed Makefile target
is scrolled past, and then reports nothing at all.
· **ideonomy passes / overturns:** 0 — this was forced by a failing test, not
selected from alternatives. · **Capture:** this entry.

## Follow-ups

- The section knows two ways a gate is reached (a roster target and one level
  of `$(MAKE)` under it). A gate weakened through a third path — a script a
  target calls, a fixture a test reads — is invisible to it. It says so in its
  own DIRECTION line rather than implying coverage it lacks.
- `scripts/hooks/` is reported as TAKES EFFECT on the reasoning that
  `core.hooksPath` is a relative path resolved per worktree. Verified by reading
  the config in the chamber worktree, not by reasoning about git.
