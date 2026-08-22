# The Stope — retrospective

**In flight** (merge pending). Process lessons only. The product story is in
[the chronicle](../../book/src/chronicle/the-stope.md); the epoch is
[decision 0160](../decisions/0160-the-chamber-address-gains-a-floor-and-sunless-becomes-nadir.md).

## Two preregistration defects, and the second is the first one level up

The spec recorded one at the time (amendment B.8): section 4.1's branch table
had a hole between 15% and 25%, seed 42 landed in it at 24.49%, and the table
was left as written rather than repaired knowing the answer. That is the
discipline working.

**The second is worse, and it is the same family one level up.** Amendment B.7
froze `P(Nadir access | branch reached Underdeep)` with an intended arm of
0–10%, as though something gated access. Amendment **B.6, in the same
document**, deferred every *effect* of the barrier to a later campaign — "this
one ships the dial, not the noise it makes". The shipped access predicate is
`chamber_exists`, and it calls neither `bands_of` nor `barrier_of`;
`barrier_of` has no production consumer at all. The question could not have
landed on its `0-10%` arm however the draws fell.

The lesson generalises past this campaign: **a preregistered table can be
internally well-formed, correctly measured, and still measure nothing, because
a sibling section of the same document removed the mechanism it assumes.** A
freeze is checked for internal consistency and against the tree; nothing checks
it against the rest of its own spec. Both defects were reported rather than
smoothed over by choosing a denominator that gives a comfortable number, which
is the part that went right.

## Eight instances of one defect family, in one campaign

**A bound derived from something other than the thing it witnesses.** Every
instance is the same shape: a loop or a scan takes its limit from a value that
cannot disagree with the thing being tested.

- The chamber witness walked each run's floors `0..drawn`, where `drawn` is the
  *same value* the existence gate compares against — so the gate could never
  fire on any address the instrument asked about. Deleting the gate outright
  produced a byte-identical artifact. An instrument that derives its own loop
  bound from the draw it is meant to witness is self-consistent by construction.
- A local `HABITATION_BANDS = 5` in the readout while its sibling bounds came
  from the chamber module: a sixth delve rung would have compiled the module
  unchanged and silently dropped that band's chambers from every tally, with
  the by-band sums still agreeing because both sides undercounted.
- `0..5u8` in a junction test helper — written *one commit family after* a fix
  round that had spent twenty lines warning against exactly this.
- And the eighth in **shipped** code: `root_floor_of` walked a literal `0..5u8`
  to find which bands a parent main line realizes.

**The asymmetry that made some of these silent is worth carrying.**
`rung_of_rank(5) == None` is a live tripwire, so a sixth rung reddens *first* —
nobody was ever going to add one silently. But that tripwire tells the next
person only that the ladder grew, and then leaves them to find every hard-coded
bound by hand. **A tripwire that fires on the cause is not a substitute for
bounds that move on their own**, and the difference is invisible until the
cause actually occurs. Eleven more such bounds live in `chamber.rs`'s own test
module; they are the standing follow-up this campaign did not take.

## Three instances of a second family

An `is_ocean` guard sitting *after* a successful `cave_at`, which refuses ocean
cells itself — a branch no input can reach, with a doc clause beside it
asserting ocean as an independent gate. Same shape as the above in that the
code is self-consistent and says something false about the world.

## Review found something in every single task it ran on

Tasks 2b, 3, 3b, 4, 5, 6 and 7 — seven for seven. That is not a statement about
the implementers; the work was good and most findings were in claims rather
than behaviour. It is a statement about how much a per-task review is still
earning at this stage of the project.

**The two Criticals are the ones to remember, and they are the same defect.**
In Tasks 6 and 7 the finding was *a test that stayed green under a mutation
breaking the property it named*. Task 6's `a_junction_is_derived_not_drawn`
built a local stream, advanced it a thousand times and asserted two calls
agreed — but a stream is a plain value type and there is no global state, so
the loop influenced nothing, and a `junctions_at` that *did* draw would derive
a fresh keyed stream per call and be equally invariant. Review proved it by
adding a coin flip: **all three junction tests and the whole 300-test crate
stayed green.** Task 7's headline rate had no assertion at all; mutating the
existence density moved it 4.4× and the probe exited 0.

Both were closed by making the test assert the rule rather than a consequence
of it — an independent re-derivation of the predicate compared as a set, and a
ratchet band around the rate — and in both cases the *same mutation was
re-applied afterwards to prove the new test fails*. That last step is what
separates a fix from a hope, and it should be the default closing move for any
finding of this shape.

## The measurement was reliable and the attribution was not

Task 7's numbers reproduced to the digit across a review round: every gated
table byte-identical, the reachable/open-mouth pairs matching an independently
written witness exactly on all three seeds. **Both of its explanations were
wrong.** The headline was a conditioned count printed under the word
*unconditional*, understated 2.2×. The mechanism was attributed to a closed
form — percolation against the existence coin — which two ablations then
refuted: the pure spine-descent route the form describes delivers exactly zero,
and the elasticity runs 1.75 → 4.41 → 9.37 where the form demands a constant
above 10.

This is the project's named repeat failure mode (*right measurement, wrong
attribution*) and it recurred here on the campaign's own headline. What caught
it was that the wrong account was **checkable** — it made a numerical
prediction. The repair was not just to correct the sentence but to make the
evidence *print on every run*, so the explanation is re-tested rather than
re-read. **A mechanism stated in prose beside a number is an unasserted claim
sitting inside an asserted one.**

## A clean auto-merge is still not evidence

Absorbing main at the Task 6/7 boundary auto-merged with zero conflicts, and
`docs/audits/type-audit-report.md` was still wrong: another campaign had added
two tags, and a text merge of two independently regenerated aggregates took
this side's counts (651/315 where the tree held 653/317). **Second campaign
running for that same file.** An aggregate artifact must never be text-merged;
it must be regenerated on the merge product. The earlier absorption on this
same branch hit it too, with six rows disagreeing.

A related near-miss the same commit records: the post-merge hook flagged a
*different* generated directory, which regenerated byte-identically — confirmed
by mtime rather than by an empty diff, because an empty diff against a path
nothing wrote is vacuous.

## An artifact can be committed without being its generator's output

Task 6 committed the underworld witness page with a stray blank line the
generator does not produce. Nothing runs the drift check for anyone, so the
first symptom would have been a red `make rebaseline` in an unrelated
campaign's close. **Regenerate before committing a generated file, even when
the edit was to its generator's prose.**

## Restoring a mutation with `git checkout --` destroyed a change set

A fix round reverted a probe mutation with `git checkout -- <file>` on a file
it was *actively editing*, and silently lost the whole in-progress change. The
correct discipline — copy the file aside, mutate, restore from the copy — was
already written down, in Task 0's own commit message on this very branch:
*"Both restored from a cp backup, never a git checkout of the file."*

**A discipline recorded in a commit message does not travel.** It was right,
it was on this branch, it was three weeks of reading away, and the next
implementer reinvented the failure. Anything worth this much belongs in a
directory-level guide, not in history.

## What went right, and is worth repeating

- **Three spec premises were corrected before the plan inherited them**, each
  found by checking a claim in the spec against the tree rather than by
  reviewing prose, and **all three made the campaign smaller**: the epoch label
  was already at v2 so the new one is v3; a descent-shaped level generator
  already shipped, so "an engine generates the floor's map" was not new work;
  and cave kind was not unread after all. The cheapest review in this project is
  still "grep the claim".
- **The falsified prediction was falsifiable by grepping.** The spec called the
  deep-rung termination rate "measurable before anything is built" while it was
  already *measured* — published to the unit in the delve module's own
  documentation since the previous campaign. Task 0 reproduced it through the
  public API and, more usefully, **asserted** it, so a later change that moves
  it reddens instead of quietly rewriting a paragraph.
- **A task was added mid-campaign for the right reason.** Task 2's review found
  the drift check structurally blind to the chamber lattice — no declared path
  carried one byte of chamber content — which made the "regenerate and see what
  moved" step of three later tasks vacuous *as written*. The witness was
  inserted before Task 3 rather than deferred to the close, and every task after
  it had a real artifact to move.
- **A cost claim was withdrawn rather than defended.** An A/B measured +2.6 s
  for the witness; six same-tree regenerations spread 10.2 s of user CPU, so the
  figure was inside its own noise floor. Re-measured directly, the three-seed
  panel is 1.634 s.

## The keystone refreeze was DISCHARGED, not skipped, and the distinction was nearly lost

The close's keystone-refreeze step was about to be justified as "the branch is
zero commits behind main, so it collapses into `make rebaseline` plus the
generated-paths drift check". **That reasoning is false in a checkable way.**
The keystone fixture is `cli/tests/fixtures/world-seed-42.json`; it is not in
`docs/generated-paths.txt` and not written by `scripts/regenerate-artifacts.sh`
— it is a byte-golden refreshed by a different target entirely
(`make rebaseline-goldens`). Neither instrument can refreeze it, so a step
resting on them would have been a silently skipped step wearing a
justification.

The true discharge is a measurement: the fixture's only three consumers —
`cli/tests/suite/lens_purity.rs`, `cli/tests/suite/repose_byte_identity.rs` and
`windows/worldgen/tests/suite/deep_realm_rehome.rs` — pass unchanged across the
epoch, seven tests in 2.7 s, and the reason is substantive rather than lucky: a
chamber is never stored, so relocating every chamber in every world moves no
serialized byte. **A DoD step is discharged by naming the artifact and the
check that covers it, never by naming a condition that makes the step feel
unnecessary.**

## One git hazard, unchanged and still biting

Four commits cherry-picked onto a fresh branch, all clean, and **six registry
rows landed where two belonged**: git's three-way merge silently reinstates
rows sitting in an applied hunk's *context*. The commit list looks exactly
right while the tree does not. Recorded as "third time this session" at the
time, and nothing mechanical catches it — the rows were dropped on merit, by
reading the spec for which ones it actually cites.
