# 0287. A local day is a whole number of ticks

**Status:** Accepted (2026-08-26) · **Decider:** Nathan · **Campaign:** The
Foliot · **Relates:**
[0186](0186-an-instant-is-an-exact-tick-count.md),
[0188](0188-quantize-still-governs-magnitude-time-leaves-it.md),
[0189](0189-a-pre-flip-world-file-does-not-load-and-that-is-the-point.md)

In the context of `windows/vessel` maintaining a **second tick lattice** —
its own `BASE_TICKS_PER_STD_DAY`, its own `Ticks` type, and a `days_of`
crossing into the kernel's lattice on every action — we decided that **a
world's rotation period is stored as an exact tick count**
(`Rotation::Spinning { day: TickSpan }`), so a local day divides the kernel
lattice exactly and the second lattice has no reason to exist.

## Why there were two lattices at all

Not carelessness. The Escapement (0186) made `WorldTime` an exact tick count
at 100,000 ticks per **standard** day. But `ActivityCycle` needs a whole
**local** day to be a whole number of ticks — otherwise every dawn rounds and
the error beats against the day cycle over a long run — and a world's day
length was a drawn `f64`. So `d * 100_000` had a fractional part, and vessel
derived its own lattice (`round(d * 100_000)` ticks per local day) to get the
integer day the sim needed.

The two lattices therefore differed by up to half a tick per day, and every
charge crossed between them.

## What the crossing actually cost, measured

**The crossing was correct, not a bug**, and this record exists partly to stop
that being rediscovered as one. This campaign's opening claim was that a
vessel tick simply *was* a kernel tick and the `f64` bridge was pure loss.
That is false: one vessel tick is `d*B / round(d*B)` kernel ticks, so
`days_of`'s conversion was doing real work and replacing it with the identity
would have **introduced** an error while claiming to remove one.

Measured before anything changed:

| probe | result |
| --- | --- |
| reachable costs × admitted rotations | 438 differing pairs, each by exactly 1 tick |
| wider sweep, direction | 213 losses, 211 gains, **net −2 ticks** |
| `round(d*B)` vs `d*B` | above 651×, below 650×, exact 140× |

Symmetric noise. Nothing accumulates. The idea registry's
`TOOL-vessel-clock-duplicates-the-kernel-tick-lattice` calls it "one
`Session::charge` rounding from observable"; that **overstates** it, and The
Escapement retrospective's "not wrong, just collidingly named" was closer.

Two sampling errors are recorded because either alone gives a wrong answer.
Sampling action costs only up to `base_cost`'s 10,000 finds **zero**
witnesses and reads as "no defect" — the error is `t·ε / round(d·B)`, so a
differing tick needs `t` on the order of a whole local day. Sampling up to
`MASS_BAND_KG`'s 100,000 kg overstates it the other way: no authored species
exceeds 6,000 kg, so the reachable maximum is 121,709 ticks, not 246,000.

## The decision, and why not the alternatives

- **Quantize the day at the draw (chosen).** Removes the *reason* for two
  lattices instead of managing the conversion between them. After it, a
  vessel tick genuinely is a kernel tick — verified over 85,716 round trips
  across every lattice-aligned day length, zero inexact — so `Ticks`,
  `days_of`, the local-tick rescale and the queue's `scale` factor all
  collapse to identities and are deleted rather than renamed.
- **Vessel-only unification (declined).** Define vessel's day as
  `round(d·B)` kernel ticks and leave astronomy continuous. Cheaper and
  contained, but it leaves vessel's day boundary drifting from astronomy's by
  half a tick per day — roughly a full day per 200,000 days. It relocates the
  discrepancy rather than removing it.
- **Leave it (declined).** The noise is harmless, but the two lattices are a
  standing invitation to exactly the misreading this campaign made, and the
  name collision (`Ticks` vs the kernel's tick) is real.

## What it costs

**An epoch: every world regenerates.** The derived day length moves by at
most half a tick (0.432 s). Measured on seed 42, the committed world changes
by **exactly one number** — `0.87987998` → `0.87988` standard days, which is
87,987.998 ticks snapping to 87,988.

The eclipse scene moves only in ground-track longitudes, and only as that
predicts: 0.0008° at day 85 growing to 0.017° at day 1908, because a ground
track's longitude *is* the world's rotation phase and the phase error
accumulates linearly. 1908 days × 0.0017 s/day ≈ 3.3 s of rotation ≈ 0.017°.
Days, tick counts, latitudes and durations are byte-identical. One vessel byte
golden moves by one leaf: `sun_altitude_deg`, 79.467033 → 79.467029.

**The draw itself does not change**, and this is the fact that keeps the
change small. `anchor.rs` takes the same two `next_f64()` values from the same
streams in the same order; the quantization happens *after*, where the
`Rotation` is constructed. So **no seed label takes an epoch suffix**, the
pin-isolation tests pass unmodified, and 0189's "a world written before the
flip does not load" does not apply — an existing world file still loads, it
simply re-derives a day length 0.0017 s different.

`Calendar::day_length()` keeps returning `Option<StdDays>` as an exact derived
conversion, so its 31 call sites did not move; `Calendar::day_ticks()` is
added beside it for callers doing integer time arithmetic.

## What this does not do

It does not convert `windows/vessel/src/liveness.rs` to integer time. That
layer's internal currency is `f64` days end to end, and converting it was
attempted and reverted — see the campaign retrospective. The lattice
unification and the liveness conversion are separate pieces of work; this
record covers only the first.
