# 0317. A pre-genesis sky answers for the instant asked

**Status:** Accepted (2026-08-27) · **Decider:** Nathan · **Campaign:** The
Foliot · **Supersedes:**
[0187](0187-a-pre-genesis-sky-query-is-clamped-to-genesis.md) ·
**Relates:** [0126](0126-fact-day-is-a-typed-world-time.md),
[0190](0190-a-reachability-trace-is-not-closed-by-finding-one-funnel.md),
[0316](0316-a-local-day-is-a-whole-number-of-ticks.md)

In the context of `GeneratedSky::t` — this domain's one crossing from exact
kernel ticks into continuous time — clamping a pre-genesis instant to genesis,
we decided that **the clamp is removed and the sky answers for the instant it
was asked about**, negative or not.

## Why 0187 clamped, and why that reason is gone

0187 is worth reading before this record, because it did not choose the clamp
out of conviction. It says so itself: an earlier draft justified it by calling
a pre-genesis sky "not a physical question," and 0187 **retracted that** on
the strength of its own sibling task — `Calendar::local_day` was deliberately
fixed to answer correctly for negative time on decision 0126's precedent, and
the sky and the calendar sit on the same axis. If the calendar may answer, the
sky cannot claim the question is meaningless.

What 0187 fell back on was structural, and it was true at the time:

> the public API cannot receive a negative `StdDays` to answer honestly in the
> first place, so the choice is between clamping (a total function with no
> case to defend) and defending a case the type system already forecloses.

`StdDays::new` refused a negative, so no caller could hand one in. The clamp
was not a judgment about physics; it was the only thing the funnel could do.

The Foliot's instant/duration split removed that constraint. `StdInstant` is
signed, a negative instant is constructible at the public boundary, and the
clamp became a free choice for the first time. This record makes it.

## Two things established before the line changed, in that order

**The negative branch is reachable by an ordinary user.** 0187 left this
open, guessing no caller could reach it; 0190 then refuted the trace that
guess rested on. The answer is yes, and trivially: `cli/src/repl.rs`'s `sky`
command does

```rust
let day = argument.and_then(|a| a.parse().ok()).unwrap_or(0.0);
match WorldTime::from_std_days(day) { ... }
```

`WorldTime` is signed, so `sky -5` parses, converts, and reaches the funnel.
Anyone at a REPL prompt can produce it.

**The arithmetic actually holds down there — after one fix.** A sweep of all
seventeen instant-taking `Calendar` methods across instants from −100,000 to
+100,000 days found a live defect: `year_phase` computed
`(t / year + offset).fract()`, and `fract` returns a *negative* fraction for a
negative operand, so `year_phase(-100_000)` read **−0.49** where a phase must
lie in `[0, 1)`. `season_phase` delegates to it and inherited the fault;
`moon_phase` had the same shape independently. Both now use `rem_euclid(1.0)`,
which for a non-negative operand agrees with `fract` exactly — `trunc` and
`floor` coincide there — so the repair is byte-neutral for every world that
exists.

The order matters and is the point: removing the clamp first would have
shipped a path returning negative phases and called it a fix. The sweep is
what makes this decision an informed one rather than a preference.

## What removing it also fixed

A pre-genesis query was **incoherent**, and 0187 does not mention it because
nobody had looked. `sky_report_from` assembles one report from two sources:
the sky through the clamping funnel, and the weather through
`climate.weather_at(vertex, day)`, which takes the raw `f64` and clamps
nothing. So `sky -5` returned **genesis's sky under weather from five days
before the world existed** — one report whose two halves disagreed about what
time it was.

Measured on seed 42 before the change: `sky 0`, `sky -5` and `sky -100000`
returned three different descriptions, and the difference was entirely the
weather. Removing the clamp makes both halves read the same instant.

## What it costs

Nothing committed moves. No artifact and no golden queries a negative instant,
verified by regenerating everything and diffing the declared paths. The
behaviour change is confined to a caller that deliberately asks for
pre-genesis time — today, exactly the REPL's `sky <negative>`.

A caller can no longer distinguish "the sky at genesis" from "the sky before
genesis" by getting the same answer for both, which is the property 0187
accepted as a cost and this record hands back.

## The alternatives, and why not

- **Keep clamping, correct only the rationale.** Defensible and
  behaviour-preserving; rejected because it preserves the incoherence above
  and keeps a total function whose totality was an artifact of a type
  limitation that no longer exists.
- **Clamp the weather too, so both halves say genesis.** Also coherent, and it
  matches what 0187 claims the behaviour already is. Rejected because it
  reaches into `domains/climate`'s public sampling API to enforce a decision
  made at astronomy's funnel, and because it answers a question the caller did
  not ask in order to avoid answering the one they did.
