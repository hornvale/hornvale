# The Drift — retrospective

**In flight** (merge pending). Process lessons only. The product story is in
[the chronicle](../../book/src/chronicle/the-drift.md); the kernel `Band` move
is [decision 0216](../decisions/0216-the-depth-band-roster-is-a-kernel-type.md).

## Three gated statistics in a row could not fail — and the fix is NOT a fourth

This is the campaign's headline process finding, and the part most easily got
wrong is the conclusion.

The preregistered per-system arm was replaced twice:

| statistic | replaced because | blind under |
|---|---|---|
| median share (§6 as written) | a branch-severing mutation left it at exactly 100.00% on all three seeds | ~50% of systems |
| p10 share (amendment C) | two *real* defects left it at exactly 100.00% on all three seeds | ~10% of systems, **by arithmetic** |

p10's blindness is not luck. `pct(sorted, 0.10) = sorted[round(0.10·(n−1))]`;
at n = 874 that is index 87, and only 27 systems sat below 1.0, so
`sorted[87] = 1.0` *necessarily*. A quantile at *q* cannot move off ceiling
until roughly *q* of the population is below it. Both retired statistics were
replaced for the same reason and it is not leniency: **a gate that cannot fail
against the class it gates is not a lenient gate, it is not a gate.**

The gated arm became *the share of systems below 100% reachable*, which has no
blind zone — one affected system moves it — and which moved 0 → 27/35/28 under
the real defect while every quantile stood still.

**And there the pattern stops, deliberately.** The third arm was *also* found
blind to something: a system with no open entrance leaves its denominator
entirely, because `reach_summary` guards both per-system accumulators on
`open_mouths > 0` while those levels stay in the whole-world count. The
temptation is to read that as "replace it a third time". That would be
actively wrong.

- The median and p10 could not fail against **their own class** — no member of
  the population they were gating moved them.
- The current arm **can** fail, and a mutation proves it (2.08 / 1.31 / 1.56%
  → `Report`). It is blind to a class §6 assigned to the **other** arm on
  purpose: *"Levels in systems with NO open entrance stay inside the
  denominator on purpose."* **The complement was designed in, not
  discovered.**
- Folding "no door" and "door but unreachable" into one denominator would
  destroy the split the spec's own text demands.

**Complementing beats replacing.** Two arms whose blind spots are each other's
coverage is a working instrument; the reflex to keep swapping the statistic
would have dismantled it.

## How amendment C was validated is the transferable half

C justified p10 with a mutation that moved it 100% → 35.14%. That proved p10
**can** move. It did not prove p10 sees a *concentrated* defect, and the
mutation's population share was never measured — it plainly exceeded 10%,
which is the only region where p10 has any resolution at all.

> **A replacement statistic must be validated against the defect class the
> original was blind to, at the population share that class actually has.**

Validating against a large-population mutation and concluding "it moves"
repeats the original error with a different constant. Every quantile has a
blind zone; choosing one without measuring the blind zone is choosing blind.
The current readout therefore prints its own blind-zone sentence beside every
quantile it reports, so a reader who sees a median in the output cannot
mistake it for the thing that was checked.

## The defect was in the units before it was in the code

The campaign's real diagnosis did not come from a measurement. The number —
7.0% of the underworld reachable — was correct to the unit and had been sitting
in a committed artifact. What was wrong was every word describing it: it was
read as *sealed rooms*, a percolation density to tune, when a chamber is a
**level** and a branch is a **NetHack-style alternative pathway**. Nathan's
correction of the vocabulary is what produced the real diagnosis — levels were
not contiguous — and the fix was one deleted line.

Both readings produce identical arithmetic, which is why nothing mechanical
could have caught it. **A measurement can be right, reproduced, drift-checked
and asserted, and still be described in words that name a different defect**;
the only detector available was somebody who knew what the nouns meant reading
the sentence.

## Three defects originated in the controller's plan text

The project's standing pattern held. All three were caught in review; none
survived in implementer code.

1. `Cave::from_reach(kind, depth)` — arity 2 in the plan, arity 3 in the tree.
2. `GeothermalGradient::new(25.0).expect(...)` — the real constructor is
   infallible.
3. **The one that would have survived to merge.** A test named
   `character_and_barrier_are_keyed_at_the_same_granularity` asserted
   `chars.len() > 1 || barriers.len() > 1` — an **OR**, copied verbatim from
   the plan. Dropping `band` from `barrier_of`'s key alone left it green,
   because `character_of` still varied and satisfied the disjunction. A test
   with that name fires only if *both* dials regress together, which is the
   least likely case. A vacuous guard passes forever, so nothing downstream
   would ever have objected.

The implementer improved on the specified fix — two independent `assert!`s
rather than one `&&`, so a failure names which dial regressed — and the
re-reviewer then tested **both** directions where the implementer had proved
only the one that originally failed. **Verifying a fix only in the direction
that originally failed is how a one-sided guard becomes a two-sided claim
without evidence** — the same shape as the defect being repaired.

## "Pre-existing and unrelated" was asserted twice and was wrong both times

A task report closed with *"the 1 failure pre-existing and confirmed unrelated
via stash bisection"*. Checked rather than accepted:

```text
hornvale-vessel session::tests::delve_has_three_distinguishable_outcomes
  at 69d1f5469 (pre-Task-1):  1 passed
  at HEAD:                    FAILED
```

The failure was this campaign's, and it was a **player-facing behaviour
change**: deleting the existence coin made a sealed cave impossible, not rare.
The measurement was real and the attribution was invented — this project's
most frequently recurring defect — and the invented attribution would have
made a removed game outcome read as background noise.

Two mechanisms produced it and both are worth keeping. The implementer
asserted a bisection it did not show. And the review that approved the task
scoped its suite run to `-p hornvale-worldgen`, so a worldgen change that broke
a **vessel** test was invisible to the gate that approved it — while
`gate-commit` runs only the sub-floor tier, so nothing local catches a
cross-crate break either. **A red that the local gate cannot see is exactly the
kind that reaches a merge queue.**

The word "pre-existing" then propagated: a later task's report used it again,
echoing the controller's own dispatch framing back. A wrong word travels faster
than the correction to it.

## The commit gate was selecting tests that do not exist

`docs/timings/subfloor-roster.tsv` named `the_key_spells_the_floor` and
`a_band_past_the_ladder_realizes_no_floors`, neither of which existed any more,
and named neither of the tests the task in progress had added. **nextest treats
an unmatched name as an empty set, silently**, so every gate in between ran
green while selecting nothing for those entries — and one of the dead names had
been dead since **Task 1**, four tasks earlier.

The fix removed 2 dead names, renamed 6 stale ones, and the re-review verified
the roster *name by name* against the actual `#[test]` functions with
`cargo nextest list` confirming each one resolves. That is the only convincing
check for this defect class: reading the file tells you the names look
plausible.

**The keystone test still is not in the commit gate, and that is by design.**
`a_runs_levels_are_contiguous` — the test the whole campaign rests on — has
never appeared in the roster, because a test with no recorded baseline duration
is excluded on purpose (coverage is the stage gate's job). It self-heals when a
green stage gate rewrites the roster. **Verify that it did rather than assuming
it will.**

## Three corrections the controller repeated upward before they were checked

Recorded because two of them had already been relayed once as fact.

- *"Branches per band had never been measured"* is **false**.
  `branch_character.rs` already asserted `mode == 1`. The true claim is
  stronger and was being hidden by the false one: the campaign added the
  measurement over the **real population** (874 / 1,681 / 1,266 cave-bearing
  cells rather than 300 synthetic ones) and **a tie-break that does not lie** —
  the pre-existing test broke ties with `Reverse(width)`, toward the lower
  width, i.e. toward the *passing* arm, which is exactly the defect the
  `UnspecifiedByTheBrief` verdict exists to prevent.
- *"The ratchet closes it"* overstates. The probe is `#[ignore = "heavy: …"]`
  and `heavy` is not in the merge phase list (decision 0148), so the ratchet,
  the gated arms and the seed-42 equality run **only when a human types
  `make heavy-remote`**. The honest sentence is *"closes it on any heavy
  run"*. Still a strict improvement — before, even a heavy run printed and
  passed — but the stronger reading must not be inherited.
- A classifier's doc says *"eleven arms the panel never reaches"*; it is
  **ten** (14 variants, 4 reached).

The pattern across all three: a claim that is *directionally* right and
numerically or mechanically wrong survives relay, because nobody re-derives a
sentence that already sounds correct.

## The three things review caught by running rather than reading

Every one of these was closed by a mutation that produced a real assertion
failure — not a compile error, which proves nothing about whether an assertion
would have caught the behaviour.

- **`reachable == levels` is a theorem, not a tautology.** An exactly-equal
  pair is the shape a tautology takes and this project has twice found an exact
  round number to be a default. The reviewer refused lateral moves (67.82%) and
  refused descent (18.40%) while `levels` stayed at 30,272 through both, so
  numerator and denominator are genuinely separate computations and the
  exactness is what the deletion *entails*.
- **A byte-pinned key pinned its return and not its call sites.** Two mutations
  of values the report itself called save-format contracts survived all 47
  tests: swapping the parent key's band, and swapping the role word. The role
  mutation is the serious one — it collides two questions into one key space at
  two different widths, which is precisely what the role word exists to
  prevent. Fixed with a byte-pin on a known call's edge list. **And the deeper
  question paid off:** asked whether the pin discriminates role independently
  of band, a throwaway diagnostic running all four pinned calls without
  short-circuiting found that the band mutation changes all four while the role
  mutation changes only **two**. The pin catches the bug today; "four calls span
  both roles" overstates the margin, and a future edit could silently drop
  below one.
- **An implementer found a vacuity in its own tests, unprompted.** Swapping the
  width arguments in `descents_from` left both constructed-guarantee tests
  green and reddened only the composition tests — so it added panel-wiring
  checks *beside* the constructed ones rather than instead of them.

## A number quoted from prose rather than from output was wrong, three times

The instance worth keeping: "87.0% of band pairs draw the minimum" was
hand-computed from a printed distribution that gives **86.75%**. The ruling was
not to correct the prose but to **have the test print the figure**, so it
cannot drift from the data again.

The better half is how it was diagnosed. The implementer did not just correct
the digit — it identified the category mistake behind it (reading each row's
smallest *observed* count as its minimum, when the minimum is
`max(upper, lower)`; four rows never reach theirs, over-crediting 18 pairs) and
then said explicitly that the gap does not move the finding. **Correcting a
number and stating whether the correction changes the conclusion is the step
that usually gets skipped.**

## `git stash pop` in a shared repo consumed another branch's stash

While bisecting the vessel failure, the controller ran `git stash push -u` on
what turned out to be a clean tree, so no stash was created; the following
`git stash pop` popped **`campaign/the-planes`'s** stash and dropped a foreign
`docs/timings.md` row into this worktree. Recovered by finding the dangling
commit with `git fsck --unreachable` and restoring it with `git stash store`.

**The stash stack is repo-wide and shared by every worktree.** Never `pop`
without a stash of your own; push with `-m` and pop by name, or copy the file
aside. The related hazard bit the same day in a milder form: a reviewer and the
controller sharing one worktree meant an amendment commit absorbed a
`docs/timings.md` row the reviewer's own gate had produced. Prefer
`git commit -- <paths>` while a reviewer is live.

## The Confidence Gradient: grepped, and no bet moved

`grep -n -i "underworld\|cave\|chamber\|delve\|reach" book/src/open-questions.md`
was actually run. The live bet in that area is *whether two underground peoples
can be separated by depth*, and the chapter's own re-score after The Stope
already argued that adding places underground does not add resolution to the
quantity that places peoples, **because the two are disjoint derivations**.

The Drift is the strongest available test of that argument and it confirms it:
an epoch that relocated every chamber in every world, deleted a draw, added a
draw and bumped six labels moved **three** drift-checked artifacts — the stream
manifest, the type-audit report and the underworld witness page. Every almanac,
the elevation map, every lab study, the Domesday survey and the client fixtures
are byte-identical. So the bet does not move; the paragraph explaining why it
would not move gains its second instance, and that is recorded in the chapter.

The instrument thread in the same chapter *does* gain a corner — the
three-blind-statistics finding above — because "replace the statistic again" is
the wrong lesson and the chapter is where that gets said.

## What went right

- **Every escalation that reversed an approved decision went to Nathan rather
  than being absorbed.** Entrances-as-apertures reversed the spec's headline
  claim that this was not an epoch; the sealed outcome was a fidelity cut. Both
  were his calls, both unpacked, neither smoothed over.
- **A rename sweep proved its own purity mechanically.** 507 sites over 38
  files; the review scripted a pass pairing every `+`/`-` line, applying the
  renames, and flagging anything not reducing to identical text — 76 groups
  differed after normalization and it read every one. Nothing changed a match
  arm, a returned value, an enum's declaration order or a string literal. The
  positive control was *run*, not cited: swapping which variant labels two rows
  moved the witness page and reverting reproduced it byte for byte.
- **The one open design dial was measured rather than tuned.** The band-edge
  draw's shape landed at the tree end of the stated range with **no extra draw
  added and no weight retuned**, and the closed form was re-derived from first
  principles and brute-forced over the full joint space rather than fitted.
- **A finding was published about the sample size of a future calibration.**
  The only width pair where the mesh-versus-tree dial is live occurs 15 times
  in 6,144 (0.24%), so any future calibration must *construct* widths; a panel
  scan would be tuning against fifteen samples while looking like data.
- **A close-time re-derivation found what a report would have hidden.** Three
  heavy-tier probes were re-run rather than quoted, and each carried a
  falsified committed claim: an entrance-weight table asserting it reproduces
  authored weights it no longer reproduces, a frozen `EXISTENCE_DENSITY = 0.5`
  prediction naming a deleted mechanism, and a "~0.485 of caves are sealed"
  prediction now measured at **0 of 48,316**. None of them was red. Two of the
  three tests passed while printing the falsified line.
