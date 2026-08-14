# The Sexton

**August 2026 · outcome: merged — seventy-seven hours of waiting had been
recorded and never added up, and the sum said the campaign was aimed at the
wrong thing**

## What was attempted

The brief was to keep the project's test coverage — the census included — while
cutting the wall-clock time and effort of verifying a change. The obvious
target was the census: a thousand worlds, two hundred metrics, and a refresh
that had recently taken five hours and twenty minutes to move three columns.

The first useful result was learning that the census is not the problem.

`docs/timings.md` had been accumulating for a month: 368 rows for `gate`, 247
for `rebaseline`, 34 for `census`, 9 for `ci`. Nobody had ever summed it.
Summing it took one line of `awk` and inverted the campaign's premise:

```
  label         runs   total_wall_h   share
  gate           368          43.89   57.0%
  census          34          13.50   17.5%
  rebaseline     247          12.22   15.9%
  heavy            4           5.77    7.5%
  ci               9           1.66    2.2%
  TOTAL                       77.04
```

Seventy-seven hours in a month, and the census was the third-largest line. A
second aggregation, by host, was worse:

```
  MacBookPro   368 runs   34.18 h   10 cores
  ambrose      190 runs   15.77 h   12 cores
  lefford       70 runs   22.86 h   40 cores   <- 3.1% of the window
```

Fifty hours of waiting happened on laptops while the forty-core machine idled
at three percent. And decision 0125 had deleted CI on the grounds that runner
minutes are metered — but compute was never the scarce resource. What had been
deleted along with the runner was the *scheduler*, and the scheduler was the
half with no substitute. CLAUDE.md had accumulated six separate paragraphs
telling humans to remember what a timer used to do: *nothing runs it for you*,
*nothing syncs the board for you*, *nothing rebuilds its binary for you*.

So the campaign became three limbs, after the sexton who rings the bells,
sweeps the yard, and buries the dead: **See** (make the invisible costs
visible), **Sweep** (recover the accidental ones), **Bells** (restore
scheduling).

## What shipped

Nine tasks, measured on `ambrose`:

```
  make rebaseline    125.2 s -> 33.9 s        cpu_ratio 1.65 -> 5.69
  seven commands     no ledger rows -> all seven recording
  make ci            9 runs/month -> the alarm rides all 368 gates
  census sentinel    ~15 CPU-s, ~0 marginal gate wall
```

`regenerate-artifacts.sh` was sixty-two sequential `cargo run` invocations at a
measured `cpu_ratio` of 0.72–2.11 on a twelve-core box — effectively serial, on
sixteen percent of all measured waiting. Nobody had decided that; it fell out
of the file being a shell list. Restructured into a dependency DAG with a
bounded fan-out, it runs byte-identically at a quarter of the wall time. The
byte-identity is a real falsifier rather than a hope, because all forty-six
committed output paths have exactly one writer each — checked before the work
began, which is what made ordering provably irrelevant.

`prewarm` had **zero** ledger rows against seventy-three branches in a month.
Seven commands now record; the ledger can finally see a class of cost that had
never appeared in any decision about cost.

The duration alarm built by The Timekeeper had run nine times against the
gate's 368. It now rides every gate, because every gate already computed the
durations it needed and threw them away.

And two instruments were built whose value is not in their cost:

- **The census sentinel** runs three worlds against the committed census inside
  the commit gate, in about fifteen CPU-seconds. Within hours of shipping it had
  verified three other campaigns' byte-identity claims — The Millrace's, The
  Fathom's, The Holdfast's — on the census path, which is precisely the path
  those campaigns cannot cheaply check for themselves. Before it, the next proof
  of such a claim was a five-hour census at somebody's campaign close.
- **The defect ledger** records which test failed, on what change, on every red
  gate. Its first real entry arrived during the campaign's own fix wave.

## What the campaign found by accident, and it was the larger result

The instruments caught things while being built.

**Decision 0130's channel work had added 467 CPU-seconds to the commit gate and
nobody knew.** The first gate run under the new instrumentation alarmed on nine
tests; `hearth_population_calibration` had gone from 9.6 s to **187 s**,
becoming the slowest test in the workspace by more than double. It went
unnoticed because `make ci` — the only thing watching — had run nine times in
that window. The campaign's thesis, demonstrated by the campaign's own first
gate.

Reading that re-record required a distinction worth keeping: **in a baseline
diff, a broad uniform shift across unrelated tests is the machine; a local
shift concentrated in related tests is the code.** This diff had both at once —
467 CPU-s worse across nine lab and vessel tests, 637 CPU-s better across three
hundred and fifty unrelated ones — and reporting either half alone would have
misattributed the other.

**And the campaign's own headline decayed six-fold inside a working day.** The
spec opened with a measurement: world generation is five percent of census
cost, metric extraction ninety-five, so worldgen optimisation aims at the wrong
five percent. By that evening, after absorbing The Millrace, extraction had
fallen 8.25× while the world build had not moved, and the split was 32/68. The
conclusion the spec drew from its own number was no longer true. The spec now
carries both measurements and the cause rather than corrected digits, and the
registry row that depended on it says *re-measure before speccing* instead of
inheriting the figure.

## The shape of nearly every defect

Fifteen-odd plan defects were caught before reaching production behaviour, and
they were almost all one shape: **a sentence asserting something about the
world that nobody had executed.**

- `git checkout -- . --quiet` places `--quiet` *after* the pathspec separator,
  so git reads it as a pathspec, exits 1, and the defensive `2>/dev/null || true`
  swallows it — leaving the tree dirty in a job whose only purpose there was
  leaving it clean.
- A defect-ledger extractor grepped `"name"` before `"event"`; nextest emits
  `event` first, so the pattern matched **0** of 3,449 real events. Its failure
  mode was an empty ledger, which reads as *no test has ever caught anything*.
- A script that recycles working checkouts enumerated candidates with `find`.
  A directory git does not have registered answers `git -C` from the
  **enclosing repository**, so a leftover directory reported the main branch,
  scored as reusable, and the script would have switched the primary checkout
  onto a campaign branch.
- A status read from a file reported **green** when the file was empty, because
  `[ '' -eq 0 ]` is a syntax error and `if` reads an errored test as false.

Three of the four were mine. The fourth was found by an implementer who checked
`git status` instead of trusting `exit=0`.

The lesson that generalises past this campaign is narrower than "test your
code": **the defensive `|| true` that makes a script robust is exactly what
hides its failures**, so every suppressed failure needs a paired positive check
of the state it was supposed to produce.

## The instruments that lied

Three times the broken thing was not the code but the tool measuring it, and
each produced confident, specific, wrong output:

- a concurrency sampler counted its own `grep` as a running job, showing a
  throttle exceeding its cap;
- a `grep` for a landed fix returned a false negative through the author's own
  quoting;
- a planted "shellcheck violation" was not a violation — `echo $x` after `x=1`
  is not flagged — so a working gate looked vacuous.

Each was caught only by asking whether the instrument fired on a positive
control. A negative result means nothing unless the control fires; the campaign
re-learned that four times, twice at its own expense.

## Where it landed short

The scheduler is written and **not installed**. `~/Projects/hornvale-scheduled`
does not exist on lefford, the systemd timers have never run, and the
seven-consecutive-nights criterion is deferred. CLAUDE.md says so in
conditional language: the census refresh at campaign close is unchanged until
the timers run and are observed. The most dangerous line in the campaign's diff
was an earlier version of that paragraph, which described the nightly census in
the present tense and told readers *if the diff is empty you are done* — while
the diff was, by construction, always empty.

Four smaller things are recorded and unfixed: `gate-fast` now mixes scoped and
full runs under one ledger label; a documentation test is brittle to prose
rewrap; the nightly cleanup still removes a human's untracked files; and
`make board-post`'s `NOTE` is macro-expanded by GNU Make, so `$(shell …)` in a
note **executes** — the two scheduled scripts sanitise, the mechanism does not.
