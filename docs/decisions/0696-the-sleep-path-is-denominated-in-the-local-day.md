# 0696. The sleep path is denominated in the local day, closing 0587's inversion

**Status:** Accepted (2026-09-03) · **Decider:** Nathan (autopilot) ·
**Campaign:** The Pallet · **Relates:**
[0587](0587-a-physical-span-is-denominated-in-the-local-day.md) (the rule this
completes), [0586](0586-every-authored-constant-declares-its-axis-of-variation.md)
(the audit that published the four findings), [0186](0186-an-instant-is-an-exact-tick-count.md)
(the standard day these were denominated in)

## Context

[0587](0587-a-physical-span-is-denominated-in-the-local-day.md) ruled that a
constant modelling a **physical duration** is denominated in the world's own
day, converted `REST_BOUT`, and published the four constants on the sleep side
of the same code as *findings* rather than conversions. It then shipped a
measured inversion knowingly: with the rest side scaled by `L` and the sleep
side still capped at one standard day, a rest on a 100-standard-hour world in
permanent night ran 104,166 ticks against a sleep of 100,000 — a rest outlasting
a sleep, on a legally pinnable world.

That inversion was held open by a **running** test asserting the wrong ordering,
so that the day someone converted the sleep path the tree would go red.

## Decision

The four sleep-side quantities are converted, and 0587's rule now holds across
the whole sleep path with no exceptions:

| quantity | was | is |
|---|---|---|
| `SLEEP_BOUT` | `TICKS_PER_STD_DAY * 2 / 5` | `local_day * 2 / 5` |
| `WAKE_SCAN_STEP` | `TICKS_PER_STD_DAY / 20` | `(local_day / 20).max(1)` |
| `SCAN_LIMIT` | `TICKS_PER_STD_DAY * 3 / 2` | `local_day * 3 / 2` |
| `ONE_DAY` | `TICKS_PER_STD_DAY` | `local_day` |

The four `const` declarations survive at file level as the **`L = 1` anchors**
the runtime computation reduces to on a standard-length world and on any terrain
reporting no calendar of its own — the idiom `REST_BOUT` established.

The inversion is closed at its source rather than papered over: both sides of
the comparison now scale with `L`, so their ordering is `L`-invariant and holds
at every legally pinnable world.
`a_rest_still_outlasts_the_sleep_scans_give_up_fallback_at_the_100_hour_legal_extreme`
is **deleted**. Deleting it is the success condition, not collateral — its own
doc says so.

## Consequences

- **Every one of the four carries a dedicated property witness**, not a
  byte-golden. Each was mutated back to its retired standard-day form and a
  named vessel test reddened for it. The property that discriminates
  `WAKE_SCAN_STEP` is worth stating because it is the same sentence the
  constant's own doc had been asserting for months: the step is the scan's own
  *resolution*, so it can only matter where the retired 5,000-tick grid and the
  converted finer grid **bracket an actual wake transition**. A diurnal window
  is half the day, too wide for either grid to miss; a **crepuscular** window is
  `TWILIGHT_DEG` = 6°, about 2,124 ticks, narrower than the retired step.
- **`WAKE_SCAN_STEP` gains a `.max(1)` floor.** `local_day / 20` is `0` for any
  `day_ticks()` in `1..=19`, and a zero step never advances the scan. The old
  fixed step made that structurally impossible; the conversion made it merely
  outside today's genesis bound, which is declared in a different crate. The
  floor restores the structural guarantee rather than resting it on a distant
  invariant.
- **The live numbers moved out of the audit's reach.** `20`, `3 / 2` and `2 / 5`
  are now `let` bindings inside `next_awake_day` and `act_span`, and
  `tools/plumb`'s walk overrides only the three `const` visitors — so
  `plumb report`'s header claim that *every* authored constant declares its axis
  is false for the numbers that actually govern rest and sleep spans. Recorded
  as an idea-registry row rather than fixed: auditing every integer literal in a
  function body has an obvious false-positive problem and is its own campaign.
- **Byte-goldens moved and the artifact machinery could not see it**, exactly as
  0587 recorded. Golden tests are run by name after every behaviour-changing
  task in this campaign, never inferred from the artifact diff.

## See also

`windows/vessel/src/liveness.rs` (`next_awake_day`, `act_span`, and the four
constants' own docs);
[The Pallet chronicle](../../book/src/chronicle/the-pallet.md).
