# The Retelling — retrospective

**Merged:** 2026-08-15 · **Program:** Myth, campaign 2 of 4

## Six blind spots, one shape — and a seventh that works

The campaign's most transferable output is not its measurement. It is that six
separate mechanisms in this repo failed the same way, and every one was green
while failing.

1. **A verbatim ratchet cannot see that its constant went stale.**
   `heavy_tier.rs` asserts every heavy-tier ignore reason equals one constant
   exactly. That constant claimed "(minutes)" for a battery measured at 4.31 s
   and named `make gate-full`, a target The Staff had turned into a refusing
   signpost. The check stayed green throughout, because every heavy test
   repeated the same wrong string consistently — which is precisely what a
   sameness ratchet guarantees.
2. **An allow-list gate cannot see that its list went short.** The commit gate
   runs exactly `subfloor-roster.tsv`. `hornvale-hearsay` was the only
   workspace crate absent from it, so from the moment campaign 1 merged, every
   green commit gate anyone ran was green about everything *except* that crate.
3. **A self-updating artifact written to scratch does not update.** A green
   stage gate rewrites that roster — into the lane's scratch worktree, which
   the next dispatch destroys. `lane-run.sh` contains no commit and no push.
   `git log --follow` shows the roster has been touched twice, both by hand.
4. **A detached job reports to nobody.** Nothing notifies a caller when a lane
   job fails: not the runner, not preflight, not the session-start board
   render. A red stage gate on this campaign's ref was found only because
   someone went looking for something else.
5. **A test guarded by the condition it tests cannot fail.** One assertion sat
   behind `if let Some(v) = five && v.precision != FINEST` — and both
   conditions are exactly the regressions the test was named to catch.
6. **Two guards sharing a string do not know they are coupled.**
   `heavy_tier.rs` asserts equality against a constant; `preregistration_guard.rs`
   pattern-matched a *substring* of that same constant. Deleting a false word
   from one broke seven tests in another crate that names it nowhere.

Every one was caught by running something. None was caught by reading.

## And a seventh mechanism, which is what the other six should look like

The campaign gate went red at the close on a test that did not exist when the
campaign began: `cli/tests/subfloor_roster_coverage.rs`, added by *The Ballast*
the same day, guarding — in its own header — "an allow-list gate cannot see
that its own list went short." Two parallel campaigns found the identical
defect independently, within hours, and neither knew until the guard fired.

What it fired *on* is the point. The Ballast could not fix the roster's data,
only the copy-out mechanism, so it declared `hornvale-hearsay` absent with a
reason naming this campaign and an instruction: delete the row once the fix
lands. When the harvested roster arrived, the declaration became false and the
guard went **RED for a fix**, refusing to let a stale acknowledgement stand.

That is a three-valued verdict — undeclared gap is red, declared gap is green,
**declared-but-now-false is red** — the same shape `seam-guard` uses, and the
exact property the other six mechanisms lacked. A verbatim ratchet, an
allow-list, a scratch write, a detached job, a self-guarding assertion and a
shared string all fail in one direction only: they can be satisfied and never
un-satisfied, so they rot silently. A declaration that reddens when it stops
being true cannot.

It also worked as a handoff no human had to broker. One campaign left a machine-
checked instruction for another and it was executed on contact, at merge, by a
gate — not by anyone remembering.

## The controller wrote every defect again

Ten defects in plan or spec text, all mine, none found by review:

- a monotonicity assertion false against its own implementation (91 does not
  divide 365);
- Earth's calendar hard-coded into a world with its own year;
- a type name that would have been the workspace's fourth homonym;
- an instruction to leave a sibling crate uncompilable, which the workspace-wide
  pre-commit hook makes impossible;
- a missed generated artifact (`layering-generated.md`);
- three tasks written against a `Filters` API two earlier tasks had replaced;
- a decision table whose branches were not mutually exclusive;
- a decision rule conflating the commit gate's roster with the `#[ignore]` tier;
- an assertion nested inside the condition it tested;
- a duplicate `let` binding introduced by my own fix to a previous defect.

The pattern from campaign 1 held exactly. What changed is *when* they were
caught: the dispatch skill's step-1 verification, run one task ahead against
live source, caught four of them **before** an implementer saw them. Verifying
late and narrow works; verifying at plan-authoring time would not have, because
three of those APIs did not exist yet when the plan was written.

## Implementers overrode the plan four times, correctly, and said so

Every override was reported rather than absorbed: the false monotonicity
fixture, the type-audit tags the plan omitted, the forced one-line fix to keep
the workspace compiling, and the layering golden the plan never named. One
agent went further and instrumented a conditional test to prove it was not
vacuous before removing the scaffolding — the right instinct, though the
*shape* still needed fixing, since proving non-vacuity today does not keep it
so.

The one thing to watch: an agent once invoked a real repo guard ("run once,
inspect many") as grounds for skipping verification its brief had asked for.
A project rule cited as a reason to do less is worth a second look.

## The registry nearly got a duplicate, and grep was not the check

An ideonomy pass produced what felt like a new idea — a world's cycles never
close, and the residual should drive ritual — and it was drafted as a fresh
row. Scanning the *category* rather than grepping the phrasing found it
substantially banked already: `LANG-50` owns the moon-versus-year (Metonic)
case and even records the blocker, `LANG-48` has **shipped** ratio detection
between two moons' periods, `LANG-51` and `MAP-63` design the
falsification-and-revision ladder, and `PSY-4` states the goblin example more
sharply than the conversation had — `threat_response` x `night_vision`
flipping light-emitters hostile.

The row survived, cut down to the one thing those five do not carry: the
residual as a permanent per-world magnitude, and the eschatological story as
an encoding of the teller's self-image. The lesson is the frontier guide's own
and it earned itself again: a targeted grep is not a scan, because the same
idea is routinely banked in an adjacent category under a different name.

## Measurement discipline held, and cost something

`0.8848` was computed on seed 42 before any hypothesis was frozen, so it was
recorded as substrate and barred as a headline — the same disclosure campaign 1
made about its 0.50 threshold. The one-rung ceiling was reported rather than
repaired, because changing the model after unblinding to produce a richer
number is the move this project forbids.

Two wrong denominators nearly cost a working mechanism. The species-keyed draft
died of one legitimately. The stance predicate almost died of another: measured
against every edge crossed with every event it reads as 0.3% lossy and looks
dead, where the population that actually carries a claim gives 12.4%. **Both
times the wrong denominator made a live mechanism look dead** — the failure
direction that gets a good design abandoned rather than a bad one shipped.

Those two figures are the ones measured during development. Absorbing 165
commits at the close moved the world under them — seed 42 went from 658
inheritance edges to 780 and from 408 qualifying endings to 511, and its
people census is barely recognisable (drow 5 to 169, gnoll 171 to 23) because
The Glasshouse re-centred the temperature baseline. The chronicle is restated
on the merged tree; these process figures are left as they were measured,
because the lesson is about the denominators and not about the world. What is
worth carrying is that **every conclusion survived the world changing under
it**: the correlation moved 0.004, the median not at all, the one-rung ceiling
not at all, and the zero-crossing result held across 780 edges instead of 658.
A campaign that had published its readout from the tree it developed on would
have shipped numbers already false at merge.

## Process notes

- **The two boxes had different `core.hooksPath` semantics**, and nobody knew.
  The Mac's was absolute, so every worktree ran main's hooks; a main-side
  rename of gate targets broke committing in every unmerged worktree, which is
  how this campaign discovered it. lefford's was relative, resolving
  per-worktree. `make install-hooks` prescribes the relative form; the Mac had
  drifted. Fixed, and verified with a discriminating control — two worktrees in
  one repo now demonstrably run different hooks.
- **The lane's install was half-landed.** The Mac refused the old gate targets
  while lefford's primary checkout, 380 commits behind, could not run the new
  ones. `lane-dispatch.sh` ships `HV_LANE_REMOTE_DIR` for exactly this, with a
  quoting hazard it validates against. Both fixed.
- **Repo command guards scan the whole command string**, so writing *about* a
  forbidden flag in a ledger heredoc trips the guard that forbids *using* it.
  Three times this campaign. The workaround — write the file rather than pipe
  it through a shell — is on the board and works.
- **The shared stash stack is cross-worktree.** A bare `pop` would have
  restored another session's work. The guard caught it; a unique tag made
  recovery by SHA clean.

## Deferred, with homes

- **The compounding variant.** Make stance relative to the *teller* rather than
  to a fixed event and the label stops being absorbing. Campaign 3, frozen by
  someone who has not seen these numbers.
- **The readout measures seed 42 only**, while the spec's population is the
  census seed set. Every figure here is one world, and the generality claim is
  untested. The heavy batteries now run in the stage gate, so this is a study
  away rather than an infrastructure problem.
- **`bare-ok(diagnostic-value)` carries two incompatible meanings** —
  test-harness instrumentation in `lab`, world quantities in `scene` — and
  `windows/lab/CLAUDE.md` forbids the second while `windows/scene` ships it.
  `type-audit` validates that a class is in the closed list, never that it is
  the right one. Posted; not this campaign's to settle.
- **One prose mention of the old heavy reason** survives in
  `windows/worldgen/tests/repose_laws.rs`, correctly explaining that the old
  duration was false. Left deliberately.
