# The Fast Gate

**July 2026 · outcome: complete — the commit gate runs in half a minute, and
the test system gained five instruments and a ledger of twenty more**

## What was attempted

The day began with a complaint that had nothing, apparently, to do with the
test system: builds and tests on campaign branches kept outrunning a
ten-minute tool timeout. The obvious suspects — cold builds in fresh
checkouts, missing cache sharing, debug information bloat — were each
measured, and each acquitted. The whole workspace compiles from nothing in
sixteen seconds; sharing target directories buys almost nothing (cargo keys
workspace artifacts to their absolute source path) and costs a build lock
that serializes parallel sessions; trimming debug info saved two seconds.
What the stopwatch actually found was a test binary: seven calibration
assertions over the thousand-seed family sweep, recomputing the sweep live
on every run — two hundred and nine seconds, seventy-eight percent of the
entire gate, sitting exactly where the previous census elephant had sat
before the fixture treatment removed it. The elephant was given the same
treatment the same day, on main. What remained was the question this
campaign answers: if two days of measurement could find that much, what
else does the test system want?

An ideonomy session produced twenty directions — a spectrum of how many
seeds a test examines, a graph of what trusts what, a tree of test kinds
with conspicuously empty branches — and the campaign recorded all of them
in the idea registry and landed the five with the best leverage-to-effort
ratio.

## What landed

**Release-grade speed inside the debug gate.** Four crates do almost all of
the suite's computational work: language evolution, terrain, climate, and
the worldgen composition root. Each now carries `opt-level = 2` in the dev
profile — measured one crate at a time, keeping only what paid (astronomy
was tried twice and excluded). The ignored family-sweep guard fell from 343
to 52 seconds; the full workspace gate from 267 seconds, at the day's
start, to about 29. The optimization is safe precisely because the project
already proves, in CI, that opt level cannot perturb a derived world —
the fixtures the fast tests compare against are release-built.

**Probes that sweep instead of staring.** The staleness probes had always
regenerated each census's first three seeds, so a physics change touching
only later seeds slipped past them to CI. Each probe now also regenerates a
rotating three-seed window whose position is a pure function of the
committed fixture's bytes — no clock, no git state, nothing but the
artifact itself deciding where to look next. Every regeneration moves the
window; over time the scan covers the sky.

**A black box for failing worlds.** When a swept-seed calibration fails, the
lab now rebuilds the offending world exactly as the sweep built it and
writes it to `target/failures/`, beside a one-line command that reproduces
it — the flight-recorder pattern, aimed at the half of test cost that no
compiler flag touches: the time a person spends finding out what actually
went wrong.

**The finite corner, enumerated.** Where pin space is discrete and small —
two skies, two rotations, six neighbor stars, clustered or scattered
continents — sampling is a false economy. All forty-eight combinations are
now built, built again, and byte-compared, in under four seconds. The
measured result (all forty-eight build; refusals are reported, never
presumed) is a statement about the whole corner, not a sample of it.

**An affected-only gate for iteration.** `make gate-fast` maps changed
paths to the crates they can affect and tests only those, falling back to
the full gate for anything it cannot bound. It over-approximates by design
— the one window-to-window dependency edge in the workspace, worldgen's
use of the almanac, is handled explicitly and documented as the pattern to
extend. The full gate remains the only thing that gates a commit.

The other fifteen directions — a suite that watches its own wall time, a
thousand-seed hash net, mutation testing, a constitution coverage matrix —
wait in the registry as `raw` rows, each one a sentence someone can pick up
without re-deriving the reasoning that produced it.
