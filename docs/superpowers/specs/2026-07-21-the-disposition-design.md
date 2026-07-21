# The Disposition — consolidating the arbitration signature

**Status:** shipped (merged 2026-07-21) — see the
[chronicle](../../../book/src/chronicle/the-disposition.md) and
[retrospective](../../retrospectives/the-disposition.md). Byte-identical (zero
artifact drift); the ~22 test call sites go through a thin `arb` adapter (retro).
**Date:** 2026-07-21
**Campaign:** The Disposition (a pure, byte-identical refactor)
**Precedes:** implementation plan → execution → merge

## Problem

Six drives in, `arbitrate` — the heart of the drive layer — takes **nine
arguments**:

```rust
pub fn arbitrate(
    view: &Perceived, home: &RoomAddr, drives: &[&dyn Drive],
    latency: f64, horizon: f64, helpless: bool, awake: bool,
    incoming: Mode, budget: usize,
) -> Resolution
```

It carries a `#[allow(clippy::too_many_arguments)]`, and every call site (three
real — `decide`, `affect_of`, the tick — plus ~23 tests) spells out nine
positional arguments, where a stray `true`/`false` or a swapped `latency`/
`horizon` is an easy, silent mistake. The existing code comment already names the
fix: bundle the loose scalars into a `Disposition`/`MindState` struct. Every
campaign since The Temperament has flagged it; this one pays it down before the
reserved engine seams deepen the arbitration further.

## Design

Introduce one struct — **the creature's disposition going into a decision**: its
psychology dials plus its momentary mental state.

```rust
/// How a creature is disposed to decide right now — the psychology dials that
/// weight its drives and the momentary states that gate them. The same
/// perception and drives yield different decisions through this.
pub struct Disposition {
    pub latency: f64,   // deliberation_latency: grab (loudest) ↔ weigh (sum)
    pub horizon: f64,   // time_horizon: myopic ↔ foresighted (anticipation)
    pub helpless: bool, // learned helplessness — the survival drive given up
    pub awake: bool,    // the wake-gate state (The Slumber)
}

pub fn arbitrate(
    view: &Perceived, home: &RoomAddr, drives: &[&dyn Drive],
    disposition: &Disposition, incoming: Mode, budget: usize,
) -> Resolution
```

Six arguments — under clippy's threshold, so the `#[allow]` is **removed**.

**What is bundled and what is not.** The bundle is the "how this mind decides"
group: the two psychology dials (endowment, from `PsychVector`) and the two
per-tick derived states (`helpless`, `awake`). Left as arguments: `view` and
`drives` (what the creature perceives and wants), `home` and `budget` (the world
frame and the search bound), and `incoming: Mode` (the hysteresis carry — it is
paired with the *returned* mode, an in/out flow distinct from disposition). A
considered alternative — splitting the stable dials (`Psyche`) from the volatile
states along the *polarity* axis — was set aside as over-structuring a refactor
(two structs, more boilerplate, no readability gain).

**Type-audit.** The four bundled primitives' verdict tags move from arbitrate's
signature onto the `Disposition` struct doc (`bare-ok(ratio: latency)`,
`bare-ok(ratio: horizon)`, `bare-ok(flag: helpless)`, `bare-ok(flag: awake)`) —
the same struct-field-tag convention `Perceived` uses. arbitrate's own tag
reduces to `bare-ok(count: budget)`.

**`decide` (the byte-identical Stage-0 seam).** Unchanged behaviour: it builds a
default `Disposition { latency: 0.0, horizon: 0.0, helpless: false, awake: true }`
— exactly the literals it passes today — and calls arbitrate. The single-drive
thirst-only decision stays byte-for-byte.

## Determinism

- **Byte-identical.** Only the argument *packaging* changes — never a value, a
  default, or an evaluation order. `arbitrate`'s body is untouched (it reads
  `disposition.latency` where it read `latency`, etc.). Same seed → same worlds,
  almanacs, and — this time — the same possession galleries too (no behavioural
  change whatsoever). The artifact drift check must show **zero** drift.
- No stream draw, no epoch, no new predicate, no save-format contact.

## Test plan

- **The existing suite is the test.** All ~23 test call sites are updated to
  construct a `Disposition` and pass it; every one must pass unchanged (same
  assertions — the behaviour is identical). This is the refactor's proof: if a
  single arbitration test changes its expected value, the refactor was not
  byte-identical and is wrong.
- **Zero artifact drift** (`regenerate-artifacts.sh` → `git diff` clean) — the
  strongest byte-identity check.
- No new tests: a pure refactor adds no behaviour to test.

## Deferred (captured)

Nothing new — this campaign *closes* a standing followup rather than opening one.
The reserved engine seams (PSY-9 chronobiology, PSY-10 trophic/predation, PSY-11
fear, PSY-12 affiliation) are untouched and remain the content backlog; this
refactor makes each of them cleaner to build atop `arbitrate`.
