# The Timekeeper

Decision 0040 adopted nextest in July to bring the commit gate "under four
minutes," and recorded the number that justified it: **234 seconds**. On a
quiet Mac on 29 July, `make gate` took **934.5 seconds**. Six of those seconds
were contention; the rest was simply growth — 2,548 tests, each a little
heavier than the ones before, and no single one of them at fault.

The project already owned a ledger for exactly this. `docs/timings.md` opens
by explaining why it exists: "a whole suite creeping 65 s → 43.5 min lived
unnoticed until it hurt." It had rows for censuses, rows for rebaselines, rows
for scene profiles. It had **zero rows labelled `gate`**. The instrument was
built, committed, and never wired to the most-run expensive command in the
repository.

## What the campaign built

A `ci` nextest profile emitting `libtest-json-plus`, so every test reports its
own `exec_time` as JSON — no XML parser, no new crate, `serde_json` already on
the allowlist. A committed per-host baseline at
`docs/timings/test-baseline-<host>.tsv`. Two alarms: a per-test one that fires
when a test takes at least five seconds **and** more than twice its recorded
value, and a whole-suite one that fires when the total shifts by more than
25 %. One entry point, `make ci`, that persists its raw output instead of
leaving it in whoever's terminal ran it.

The per-test alarm is the obvious half and the weaker one. The gate's 234 s →
934.5 s creep was not any single test doubling; it was death by a thousand
cuts, and a per-test threshold is structurally blind to that. Only the suite
total can see it. That alarm compares the *intersection* of test ids present
on both sides rather than the raw totals — otherwise it would fire on ordinary
test-count growth, and an alarm that fires during normal development gets
tuned out.

## The alarm that could never fire

Plan step 4 said: divide a baseline row by ten, run the suite, require the
alarm to go **red**. It came back green.

The recipe ran the baseline recorder *before* the alarm test. Every run
therefore overwrote the reference with the current run and then compared the
current run against itself. The alarm was structurally incapable of firing,
and every artifact of a working system was present — the profile emitted
durations, the parser read them, the baseline file filled with plausible
numbers, the tests passed, `make ci` printed a tidy summary naming every file
it had written.

That step existed only because it was added during the plan's own self-review.
Without it the campaign would have shipped a duration alarm that never alarms,
into a repository whose thesis that week was checks that never fire.

## Three more of the same shape

The final whole-branch review — the one that runs the thing rather than
reading it — found five more, and the two most serious were the same failure
again, now inside the shipped wrapper.

`make ci` exited **0 on a completely failing suite**. A `|| true` in the recipe
discarded nextest's status so that the alarm could still run against the
partial output; nothing downstream ever recovered it.

Worse, a contended run was silently green — the suppression notice went to
stderr, which `cargo test` swallows without `--nocapture` — and the recorder
then wrote the contended durations, inflated 5.2×, as the new baseline. A
one-way ratchet. One busy afternoon would have permanently raised the
reference beyond anything a real regression could exceed, and every future
alarm would have been quietly dead. The fix is now a rule with a name: **a red
run never becomes the new baseline**, so re-recording a regression is a
deliberate act taken in the commit that caused it.

And the contention gate itself had been wired **backwards**. The design read
"enforce only when this run holds the box claim," but `make ci` never acquires
a claim. So a quiet machine — the overwhelmingly common case, and the one time
timings are trustworthy — suppressed the alarm, while a claim being present,
meaning some other heavy job was running right now, was the one condition that
turned enforcement *on*. The task reviewer had passed it as correct, and was
right to: it faithfully matched the plan. The plan contradicted its own stated
rationale. Spec-compliance review cannot see that, by construction.

## Making the diff readable was a design problem, not a polish problem

The whole review loop rests on one sentence: `git log -p` on the baseline file
*is* the archaeology. Nathan had ruled out dashboards in a single line, and
that ruling makes the committed file load-bearing rather than convenient.

It did not survive measurement. Across two consecutive quiet runs, **2,405 of
2,578 rows changed** on nothing but ordinary scheduler jitter — median 16.9 %
across all tests — and none of that movement could ever cross the alarm's
five-second floor. A file that rewrites 93 % of itself every run is not
archaeology; it is a wall.

Two changes fixed it, both derived from the measured jitter rather than
chosen. Every test below one second folds into a single reserved
`<below-floor>` row carrying both its summed seconds and the count of tests
folded in — the count because a test *crossing* the floor is itself
information. And stored values move only when a new measurement differs by
more than 20 %, comfortably above the ≥ 1 s tests' measured 3.8 % median
jitter and far below the alarm's own 2× threshold, so it can never mask a
real shift. The stored number now means "the last significantly different
measurement," which also stops the baseline ratcheting upward on noise.

The file went from 2,578 rows to **535**: 534 individual tests at one second
or more, plus one aggregate holding 77.7 s across 2,064 folded tests — 0.83 %
of the suite's 9,317 s of accumulated test time, so the whole-suite total
stays accurate. Churn fell from 2,405 rows to a few dozen.

## Sixteen instances, and where they came from

The Siding, the week before, had named the shape: *a check is worth only the
configurations it actually runs in.* It found five. Counting both campaigns
the tally is sixteen — a census stale for 139 commits, a benchmark's wall-clock
timings sitting inside a strictly drift-checked tree that nothing reconciles, a
gate running at four times its documented budget with zero rows in the ledger
built to catch that, an alarm that recorded its own regression as the new
baseline, a contention guard wired backwards, and a `make -n` that turned out
not to be a dry run.

Two facts about that tally are worth more than the tally. **Eight were
authored by the controlling session's own plan text** — not inherited debt,
not legacy, written that day by the agent that had just finished explaining
the pattern. And **four were inside the machine built to detect them.**

Two practices caught what ordinary review did not: a mutation step that
*requires* the alarm to go red on command, and a final review that ran the
system instead of reading it. Neither is expensive. Both were nearly omitted.

One of the sixteen is still open, written down rather than fixed. The
contention guard asks only whether a census claim is held. During this
campaign's own hysteresis runs the Mac sat at a load average between 42 and 63
on ten cores — parallel agent sessions, not a census — and the guard saw
nothing at all. A guard that does not fire in the configuration where
contention actually occurs, shipped by the campaign about guards that do not
fire in the configuration where they are needed. The pattern was still
producing instances from inside the machinery built to end it, on the last day
of the campaign that named it.
