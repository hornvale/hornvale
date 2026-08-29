# The Governor — retrospective

Cost and gating for the heavy tier. Process lessons, not product; the product
is in [the chronicle](../../book/src/chronicle/the-governor.md).

**Merged:** 2026-08-28

## The headline: correcting is the least-audited act in the workflow

**Nine** defects this campaign were introduced **by corrections fixing
defects**, and the mechanism is specific rather than general carelessness:

> **A correction tends to add a justifying restatement beside the number it
> repairs, which doubles the surface that can drift.**

The chain, in order:

1. The count of heavy tests in files with zero assertion macros went
   **3 → 2 → 5 → "at least 9" → 6**. Every step repaired the previous step's
   mechanism and broke a new one at a *different layer*: a body-scoped regex
   (scope), then a file-wide scan that verified only a ranked prefix
   (sampling), then an exhaustive scan whose regex counted `panic!` as an
   assertion (predicate), then a correctly exhaustive scan applied to the
   wrong unit — tests with no assertions *of their own* in files that have
   some (unit). The fourth wrong value was produced by an agent explicitly
   warned that the number had been wrong twice and told to count
   exhaustively. It did count exhaustively.
2. The heavy tier's count of committed-artifact writers went **three → two →
   one**. The three-writer claim was true at file level and restated at test
   level; the correction that fixed the staleness hardened the category error
   and propagated the wrong count to five places, one of them append-only.
3. Decision 0426 accumulated four in a row. A `--since` filter selected the
   entire lifetime of the trailer it filtered on, so a twelve-day sample was
   divided by two months and every per-month cost was ~5.3x low — in the
   direction that flattered the option being chosen. The repair introduced a
   wrong row count. The repair for *that* used a ratio **the controller
   supplied**, in the same message that named this pattern, borrowed rather
   than derived and wrong by a rounding. The repair for *that* stated the same
   ratio in two forms in one sentence, which disagreed.
4. **The fix-wave brief — the repair for the whole final review — added two
   more, and this item was written by The Errata because the section could not
   see them.** The brief said the superseded `1550` figure appeared in **three**
   sites; it appeared in **six**. And it carried `~29%` where the arithmetic the
   same brief supplied gives `~30%`. The implementer caught both and returned
   them rather than propagating them, which is why neither reached the tree —
   the campaign ledger records them as "instance eight and nine". They count
   under this section's own rule all the same: a defect introduced inside a
   repair counts whether or not it lands, which is how the borrowed ratio in
   item 3 was already counted.

**Nine, derived here rather than asserted, because this section's own remedy
demands it** — and because this number has now been wrong twice in the same
place, first as *six* and then as *seven*, which is the same defect one and
then two layers up. Only a step whose defect was *introduced by a
repair* counts; the value that started each chain was an ordinary original
defect. Item 1 contributes **three** (the chain has four wrong values —
3, 2, 5, "at least 9" — of which 3 was the original, so 2, 5 and "at least 9"
are correction-introduced). Item 2 contributes **one** (the "two" that fixed
the stale "three" and hardened the category error; "three" was original).
Item 3 contributes **three of its four** (the vacuous `--since` denominator
was the original defect; the wrong row count, the borrowed ratio and the
two-forms-in-one-sentence disagreement each arrived in a repair).
Item 4 contributes **two**, both original within their own brief but that
brief was itself a repair, which is the rule item 3's borrowed ratio already
answers to. 3 + 1 + 3 + 2 = **9**.

**And it fired once more, inside the correction, in the sentence that states
it.** The Errata's own repair of this section edited
`docs/retrospectives/README.md`, where the campaign's one-line index carries
*two* of these figures in a single sentence: the headline count and the
implementer-refusal count. The repair fixed the first, left the second reading
"five times", and its commit message asserted the index "stated the same
derived quantity in a second form and is corrected with it" — true of one
number and false of the one beside it. A reviewer re-deriving both caught it;
re-reading would not have, and had not. So the tenth instance of this
mechanism occurred in the act of writing the mechanism down, in the file
`CLAUDE.md` names as the place to grep before reopening an area. **The count
above is nine because it counts the campaign; this one belongs to its errata.
The pattern does not respect that boundary, and neither should a reader.**

**The seven was not merely stale, it was stale for the section's own reason.**
The correction that raised this count from six to seven and the two defects
that make it nine were produced in the *same* fix wave, hours apart: the
paragraph was repaired and invalidated by one act. Nothing in the campaign
could have caught that, because the retrospective was written before the wave
finished and never re-read after. A count of defects introduced by corrections
is the one number that cannot be finalised while corrections are still being
made — which argues for stating the derivation, as this paragraph does, rather
than the total, so a later reader can extend it instead of replacing it.

Two things came out of it that are worth carrying:

**The structural remedy beats the arithmetic one.** Changing `60.9` to `60`
would have fixed the instance and left the class. What was applied instead:
**state a derived quantity once, next to its inputs, never in a second form.**
A sweep for that class found four more live disagreements in the same record
(a day count at two precisions in five places, an hours-per-month figure in
six, a per-landing cost in seven sites across a decision, two scripts, a test
and a skill file, and "about one landing in six" glossing 17.6%). It also
recorded a deliberate *non*-collision — 53% appears twice for two unrelated
quantities — so a later reader does not "find" it.

**The review instruction that finally caught it was not "check this fact".**
It was: *re-derive every count, date and range you added, and say explicitly
if they all hold.* That found three further defects, one of them the
controller's. Seventeen other claims were re-derived and held — and recording
an audited-clean result is the point, because assuming it is exactly how this
pattern propagates.

A ratified decision record is the worst place for this to land, since
append-only makes the repair cost an amendment forever. 0426 was still
unmerged, so it could be edited directly. That was luck, not process.

## Naming a cost honestly is not the same as judging it correctly

The adjudication demoted `occupancy_readout_is_current`, then wrote down, at
length and in a ratified decision's amendment, that doing so left a committed
fixture with **no automated witness at all**. The cost was found, stated
precisely, and accepted — and the accepting was wrong; the final whole-branch
review reversed the verdict, not the cost note. The disclosure was doing the
work of a decision. It reads as diligence, which is exactly why nothing in
three subsequent reviews stopped on it: a paragraph that already concedes the
downside looks like the concession has been priced. **When a verdict's own
justification has to name a cost that large, that is evidence against the
verdict, not evidence that it was reached carefully.**

## Bisecting this repository needs `--first-parent`

A manual bisection over the full `git log` between two SHAs produced an
**incoherent PASS/FAIL sequence** rather than a clean flip. The cause is
topological: a full log interleaves commits from parallel campaign branches
that do not dominate each other in the DAG, so "earlier in the list" does not
mean "ancestor of". The tell is concrete — **a PASS at a later list position
than a FAIL**, confirmed non-ancestral with `git merge-base --is-ancestor`.

`git log --first-parent --reverse` collapsed 1,489 commits to 384 mainline
ones and bisected cleanly to a single merge; bisecting *inside* that branch
then separated a raid-timing code change (still passing) from the stream-epoch
bump that actually moved the value. With seventy-plus branches a month through
this repository, this is a standing trap and it was not written down anywhere.

## A fix to a dispatch script cannot be exercised by dispatching

Task 7 diagnosed why twenty-seven heavy runs never reached the cost ledger —
the dispatcher runs in a scratch worktree, and the ledger path resolves to
*that* worktree, so the row lands in a file nothing commits and the next
dispatch's `reset --hard` discards it — and fixed it by mirroring the row into
the canonical checkout.

The fix did not fire on the campaign's own final run, and the reason is
structural rather than a defect: **`make heavy-remote` runs the canonical
checkout's copy of the script, which is main's.** The dispatched ref only ever
reaches the scratch worktree, after the script has already started. So a fix
riding on a branch cannot be exercised by dispatching it. Verified empirically
after a real run (the canonical `docs/timings.md` unmodified; `grep -c mirror`
on that checkout returning 0), not inferred.

The implementer flagged the residual honestly and the reviewer named the
settling test — "the next dispatch of a post-fix ref". That test was run and
could not settle it, for a reason neither could have known without running it.
Any change to the heavy, census or queue dispatch scripts inherits this.

## A docs-only commit skips the commit gate and can break a Rust test about docs

Adding a path to `docs/generated-paths.txt` broke
`generated_paths::every_declared_generated_path_is_written_by_the_regeneration_script`
— which is in the sub-floor roster, so the commit gate does run it. It survived
because the commit was **docs-only** and the pre-commit hook prints "no
Rust-relevant paths staged — skipping". It then survived four subsequent
reviews of adjacent work before a reviewer caught it by reading the criterion
rather than the diff.

The hook's notion of "Rust-relevant" and the roster's contents disagree, and a
test *about* documentation is exactly the crate of test that falls in the gap.
(The declaration itself was also a category error, and removing it was the
repair: the file's own criterion is "a path belongs here when the regeneration
script produces it", and a hand-authored audit table has no producer, so the
drift check on it was permanently vacuous.)

## Reasoning from the layering diagram instead of the manifests

The controller did this twice, and both times a diagram was substituted for a
fact a manifest already held.

- Spec §8.1 argued that a helper hosted in the composition root could not reach
  an in-module unit test in a sibling window crate, and that reaching every
  candidate would therefore require the kernel — constitutional ground. False:
  that crate declares the composition root under `[dependencies]`, not
  `[dev-dependencies]`. All thirteen candidates were reachable and the kernel
  was never needed. Found by an implementer checking the manifest and
  confirming with a scratch compile rather than taking the table's word.
- A conditional gating predicate keyed on the layering would have exempted
  exactly the changes most able to break the tier: seven of the 64 heavy tests
  live in the crate holding the tier's own harness, and the harness sits
  outside the world-generating layers entirely.

**A layering diagram says what *may* depend on what. It never says what does.**

## Implementers declined a controller's inherited claim seven times, and were right every time

A relayed campaign attribution for two probes (their own module docs named
different campaigns); a relayed follow-up list of four stale locations that was
actually five, with the fifth in the most-read file in the repository; a
relayed pointer naming the wrong task; a section misattribution ("your rows"
when the rows belonged to another task's section); a borrowed ratio; a count of
seven unconverted `map_seeds` candidates that was ten; and finally the fix-wave
brief's two — "three sites" for a figure that sat in six, and a `~29%` its own
arithmetic put at `~30%`. In each case the implementer read the source rather
than the relay.

**Five was the count at the moment this section was written and it went on
rising afterwards** — the last two arrived in the fix wave that repaired the
final review, which is the same reason the headline count above moved from
seven to nine. Both numbers are the campaign's, and the campaign was still
producing instances of them while it wrote them down.

The transferable half: **handing someone a task forces contact with the thing;
handing them a claim invites agreement.** Every one of the five was caught
because the recipient had to go and do something with the claim.

A related asymmetry, from the one time it went right: **a controller's doubt
routed to a verifier costs one command; routed to an implementer it can cost a
correct answer.** A false tension about a witness's ancestry was raised as a
question *to the reviewer* rather than as a finding *to the implementer*, and
dissolved under a single `git merge-base --is-ancestor`. Sent the other way it
would have reopened a correct attribution — the same shape that nearly wrote
the previous campaign's name onto nine failures it did not cause.

## Two tooling traps

- **The `Write` tool resolved an absolute path under the worktree's
  `.superpowers/` into the *main checkout's* copy**, and reported success. The
  file was written; it was not written where it was asked for.
- **`git add -A <paths…>` with one stale pathspec stages nothing at all** — the
  whole invocation aborts — while `gate-commit` reads the *working tree*, not
  the index, so it stays green and gives no signal that the commit is
  incomplete.

## Four more, recovered from the scratch before it was swept

Added by The Errata. Each of these was established during the campaign, lived
only in `.superpowers/sdd/2026-08-28-the-governor/`, and would have gone with
the worktree. Standing guidance says to promote findings before teardown; four
were not, which is itself the point — the ones that get left are the ones that
belong to no task's deliverable.

- **`cargo fmt` can silently un-waive a `// lexicon:` comment.** The guard's
  waiver marker is a trailing comment, and fmt will relocate it off a
  brace-opening line, after which the guard no longer sees it and the site is
  un-waived — with no error from either tool. Task 6 happened to be fmt-stable,
  so the hazard was reasoned about rather than suffered. **Re-check the guard
  after `cargo fmt`, not merely after adding the comment.** The same shape
  threatens any convention that binds meaning to a comment's *position*.

- **A harness hook dropped a heredoc, so a measurement's control arm ran the
  other arm's config.** Task 1's A/B on nextest ordering wrote arm B's config
  with a heredoc and ran it in one invocation; the command-safety hook
  pattern-matches on `cargo` text regardless of cwd, intercepted the
  invocation, and swallowed the heredoc — so arm B re-ran arm A. It was caught
  only because the completion index did not move, and it would have produced a
  clean, plausible, *identical* pair of measurements otherwise. Same family as
  `git add -A` staging nothing: **the tool ran, the input never arrived**, and
  the exit code says nothing about which. Rewriting the config with a separate
  `Write` call fixed it.

- **`timed.sh`'s `user` seconds include the cargo build, so `user / cores` is
  not a packing floor.** Dividing the heavy tier's pre-campaign `user = 22,138 s`
  by 40 cores gives 553 s and reads like an achievable lower bound. It is not:
  the tests themselves summed to **12,280.4 s** of wall — 0.55 of that `user`
  figure — so the test-only floor was ~307 s, and the campaign landed at
  440.269 s against it. The corrected sum reached the chronicle; the trap that
  produced the wrong one did not, and the wrong number is the easy one to
  recompute. **A per-test wall sum and a process-wide `user` total are not the
  same quantity and only one of them is a packing floor.**

- **When bumping a save-format stream epoch, grep for every test pinning a
  bake-derived number — not only the ones your own study touches.** The
  Granary's `history/bake/v3` bump (`eeaa011fd`, 2026-08-24) deliberately moved
  when raids fire within a year. Its close re-pinned several affected witnesses
  and missed
  `disposition_calibration::the_sub_floor_raider_reading_is_pinned_as_a_witness`,
  which then read `(10, 60)` against a pin of `(12, 60)` for four days inside a
  tier nothing was dispatching. The Granary passed every gate it was asked to
  pass. The campaign that inherited the red spent a task bisecting it and
  nearly attributed it to the wrong campaign. **An epoch bump's blast radius is
  every committed number derived from that stream, and the campaign bumping it
  is the only one that knows the bump happened.**

## Two shapes an escalation and a repair can take, and did

- **The escalation was correct AND incomplete.** Task 1's implementer escalated
  a change to the shared helper `pinned_filter_names_for_class` rather than
  assuming — correct, and the reason given was that the **return values** were
  unchanged, which was true. What the change actually removed was a
  **uniqueness guarantee**: duplicating the scatter-sweep table with no
  `priority` on the copy went from a refusal to 6/6 PASS. Checking the values
  and checking the property are different questions, and the escalation asked
  the easier one. The reviewer found it by mutation rather than by argument.
  **An escalation that names its own reasoning invites agreement with that
  reasoning; the thing to re-derive is what the change *removes*, not what it
  returns.**

- **The obvious repair for the Task 8 blocker would have inverted its purpose.**
  `the_serialization_pin_names_exactly_the_batteries_that_scatter_their_sweeps`
  asserts a two-way equality between `.config/nextest.toml`'s scatter-sweep
  filter and every test calling `seed_sweep::map_seeds(`, so parallelising a
  test would have reddened it. The naive fix — add each newly-parallel test to
  the `"num-cpus"` roster — turns the guard green and creates **one full-drain
  barrier per converted test**, in a campaign whose largest single saving
  (-345 s) came from front-loading the barriers that already existed. A bounded
  `# class: sized-sweep` was written instead, with `num-cpus` hard-rejected on
  it. **The blocker was found by reading the guard before running into it**; had
  it been hit mid-conversion, the repair that makes a red gate green is exactly
  the one that undoes the campaign.

## Do differently next time

Absorption ran at the Stage 0/1 boundary rather than only at close, which is
the previous campaign's stated "do differently" and it was worth it: the merge
was content-clean and still broke the gate, because it silently added a test to
the commit gate's selection for the first time and this branch had already
grown that test's frozen token counts past its baseline. Two independent
changes, neither wrong alone. Nothing in the merge diff pointed at it.

The ruling that pulled the absorption forward gave a decisive reason that did
not fire — the heavy roster was unchanged across those 26 commits — and it is
recorded as a null rather than banked as a win. The ruling was still right; the
justification given for it was not what paid.
