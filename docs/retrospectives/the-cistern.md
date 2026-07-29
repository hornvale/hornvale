# The Cistern — retrospective

Process lessons, not product. A three-task performance campaign that shipped
the fix its predecessor had specified, and hit its preregistered number.

## Measure both arms in one process, not one arm in two runs

The task brief said to run the profiler and compare its output against The
Sextant's recorded reference. That comparison would have been worthless. The
box was carrying roughly eighty runnable threads of a parallel session's
`gate-full` when the measurement was taken, and the unfixed code re-measured
at 902.3 ms/tile against the 702 ms The Sextant had recorded for *the same
work*. (Not literally the same lines: by then the `&World` forms were
one-line wrappers delegating to their `_in` halves, so the arm being
re-measured was semantically equivalent to what The Sextant timed, not
textually identical to it.) Twenty-eight percent of the "improvement" would
have been the machine, in whichever direction the load happened to sit — and
the sign is unknowable after the fact, which is worse than the magnitude.

Making the profiler run both arms in one process fixed it structurally. Same
build, same world, same box, same second: the only surviving difference
between the two columns is the code. The ratio came out at 11.1× and 10.8× on
two consecutive runs — holding to within 3% — while the unfixed arm those
ratios are taken from sat 28.5% above what The Sextant had measured for the
very same code. The absolute drifted between campaigns; the ratio did not.

The same trick paid twice. The cost battery's five metrics include two this
campaign did not touch — world genesis and the four terrain-free documents —
and they re-measured within noise of the earlier basis (6318.6 against 6442.8;
2.7 against 2.6). Those two rows were not planned as a control; they became
one. That agreement is what licenses attributing the other three rows' fall to
the code rather than to a quieter machine, and it cost nothing because the
metrics were already there.

**Lesson:** a before/after taken in two runs measures the code *and* the box.
Put both arms in one process. And when a battery already measures quantities
your change should not affect, say so out loud and read them — an untouched
metric that reproduces is the cheapest control available.

## A ratchet that stops measuring what it moved is not a ratchet

The brief said: lower the ceilings to ~2× the new measurements. Followed
literally, that would have taken the 638 ms derivation out of every per-call
ceiling and put it under no ceiling at all — the campaign's numbers would have
improved while its coverage got worse, and a future regression inside terrain
or climate derivation would have tripped nothing on that surface.

So a ceiling was **added** during a ratchet-down, which is not what "ratchet"
suggests and needed its own argument in the constant's doc comment. Cost does
not disappear when you move it; a budget mechanism has to follow it.

**Lesson:** when an optimisation relocates a cost, check where the guard went.
"Every number went down" and "every number is still guarded" are different
claims, and only the second one is a ratchet.

## Ask what a checksum failing would mean before it fails

The campaign inherited eleven pre-refactor checksums and a warning that one of
them — the serialized world — might have been invalidated by an unrelated
kernel change absorbed from main mid-campaign. That framing was the useful
part. The line did fail, the diff was twelve lines of a new epoch stamp on a
five-and-a-half-megabyte file with all 26,309 facts intact, and attribution
took one command.

The counterfactual is what makes it a lesson. Without the warning, a red line
in a determinism check at close is exactly the moment to either panic or —
worse — quietly regenerate the baseline and record eleven OKs. Predicting the
failure in advance, and naming the commit that would cause it, converted a
scary result into a cheap confirmation. And ten green lines beside one
explicable red is better evidence than eleven green ones, because it
demonstrates the check can go red.

**Lesson:** when absorbing main mid-campaign, predict which of your baselines
it invalidates *before* re-running them, and write the prediction down. A
baseline you regenerate without understanding why it moved has stopped being a
baseline.

## A test can go tautological without failing or being edited

The campaign's first artifact was a byte-equivalence test written red, before
the refactor: build a context, drive the `_in` entry points, assert the
documents match the `&World` path. It was genuine evidence when written.

By the end it was not. Every `&World` form had become a one-line delegation to
its `_in` form, so the test compares a function against itself and asserts only
that context construction is deterministic. Nothing failed. Nobody edited it.
The refactor it was written to guard is exactly what hollowed it out.

It survived only because the Task 1 reviewer noticed and said so in the ledger,
and because a second, absolute net had been added on the same reviewer's
recommendation: a committed region golden, so the campaign's evidence lived in
the repository rather than in `/tmp`, where it would die at the next reboot.

**Lesson:** after a refactor, re-read the tests that justified it and ask what
would still have to be true for each one to fail. A test whose two sides have
converged is worse than a missing one, because it reports confidence.

## Measure binary size at a fixed path

The reported wasm size delta was wrong twice, by roughly 4× in one direction
and then in the other, before anyone found the cause: the binary embeds source
file paths — `#[track_caller]` panic locations survive `strip = true` — so its
size depends on how deep the checkout sits on disk. A build from a repository
checkout and a build from a git worktree are different sizes for identical
code. The correct figure (+1,569 B, +0.171%) came from holding the build path
constant.

**Lesson:** binary-size comparisons need a fixed build path, the same way
timing comparisons need a fixed machine. A campaign that runs in a worktree —
which is every campaign here — cannot compare its binary against one built in
the primary checkout.

## Follow-ups

Two coverage gaps found by the whole-branch review and deliberately **not**
closed here: both are widenings of the region path's byte net, which is a
larger job than a fix wave, and neither is a defect in what shipped.

- **The region golden is partly degenerate.**
  `windows/scene/tests/fixtures/region-seed-1-f0-l3.json` is an all-ocean
  tile — `water` all `0`, `drainage` all `0.0`, `waterfalls` empty. So three
  of the region document's branches have *no* byte coverage at all: the
  water-kind classification, drainage, and `tile_contains`'s waterfall
  filter. A refactor could move any of them and no committed golden would
  notice. The fix is a second golden on a coastal or river-bearing tile,
  chosen so all three branches are non-trivial; that is a fixture-selection
  problem (finding an address with water *and* relief) more than a code one.

- **`temperature_grid_region` has no in-repo byte pin.** Its only net inside
  this repository is `temperature_grid_region_commutes_with_the_evaluator`
  (`windows/scene/src/region.rs`), which checks the grid against an
  independent rebuild of the same three additive terms at 1e-9 *relative*
  tolerance. That is a relation, not an absolute: a change moving both sides
  together passes. The absolute golden this repo *generates*
  (`examples/region_temperature_golden.rs`) is committed in the **orrery**
  repo, so a cross-repo change is required to see it move. An in-repo
  absolute golden would put the pin next to the code it pins.
