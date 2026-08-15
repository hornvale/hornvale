# 0134. A partition statistic refuted by its own mechanism is retired, not rescued

**Status:** Accepted (2026-08-15) · **Decider:** Nathan · **Relates to:**
[0016](0016-studies-preregister-hypotheses.md),
[0131](0131-refuted-is-a-seventh-registry-status.md)

In the context of `windows/lab/tests/disposition_calibration.rs`'s PRIMARY
sign claim (`separation > 1.0`) having been red on `main` since 2026-08-10,
diagnosed and deliberately left standing by The Radiation
(`cfc028b8`), we decided that **the claim is refuted rather than repaired: the
min-versus-max order statistic it computes is retired from assertion, the
SECONDARY whole-roster Spearman correlation it was always a proxy for stays
asserted unchanged, and nothing about the mechanism, the thresholds, or the
seed panel is retuned.**

## Context

The battery preregisters that a raiding people (`threat_response >=
RAID_DISPOSITION_MIN`) re-seats its genesis flagship more often than a
non-raiding one. The Assize (2026-08-08) already stripped this claim down
once, from a fitted ceiling/floor/ratio to a bare sign claim — `separation >
1.0`, the weakest raider's re-seating rate over the strongest abstainer's —
because the original bound encoded a pre-Tolerance mechanism that a
per-settlement draw (`Bake::takes_the_initiative` comparing a drawn value
around the authored mean, not the mean itself) had already invalidated once.

The Radiation's fifteen-people roster (six elves added since The Assize)
falsified even the sign claim: measured on lefford's heavy-tier run at
`581468bb`, seeds 1..=60,

```
weakest raider      high-elf    0.317
strongest abstainer  snow-elf   0.466
separation                      0.680   (was 1.30 at nine peoples, 2.55 pre-Tolerance)
```

Two findings from that investigation matter more than the failing number
itself:

1. **It is not a roster-size accident.** The same measurement restricted to
   the pre-Radiation nine peoples already reads 0.383 / 0.367 = 1.045 — inside
   5% of failing before the elves existed. High-elf sits at `threat_response`
   0.60, exactly `RAID_DISPOSITION_MIN`; snow-elf at 0.50, one tenth below it.
   The claim did not need six new peoples to break; it needed one authored
   near the gate on each side, which the elves happened to supply.
2. **The direction the claim was written to test survives, and survives
   better than before.** Spearman's rho of authored `threat_response` against
   re-seating rate is **0.840** over all fifteen peoples (up from 0.831 over
   nine) — the battery's own SECONDARY assertion, unmoved and unfalsified.
   The rate is still a monotone increasing function of the authored mean, on
   the new peoples as strongly as on the old.

The mechanism is structural, and this file's own module doc predicted it in
advance, before The Radiation's roster existed: once `takes_the_initiative`
compares a per-settlement *draw* around a people's authored mean rather than
the mean itself, two peoples close in mean have overlapping behaviour by
construction — high-elf clears the gate on 50% of its draws, snow-elf on 31%.
A min-versus-max comparison over such a partition is comparing the two
innermost order statistics of two overlapping distributions, which is a
statistic that gets weaker as authored means cluster, not one that tracks
whether the underlying mechanism still orders anything. Its survival at nine
peoples was luck; 1.045 is what that luck looked like just before it ran out.

**Nothing was retuned at any stage of this chain.** Not at The Assize (which
deleted `NONRAIDER_MAX` and `SEPARATION_FACTOR` rather than raise them), not
at The Radiation (which diagnosed and left the assertion firing, red, exactly
as ADR 0016 requires of an honest preregistered result), and not here.

## The ruling

**The PRIMARY sign claim (`separation > 1.0`) is retired from the always-run
battery.** It no longer executes as part of
`non_raiding_peoples_hold_their_genesis_flagship_far_longer_than_raiders`,
which keeps its two claims that still hold — the `RAIDER_MIN` floor (every
raiding people clears 0.30; weakest raider at fifteen peoples is 0.317, still
comfortably above it) and the SECONDARY `spearman(threat_response, rate) >
0.0` claim, which is the actual carrier of the mechanism this file exists to
test.

**The retired claim is preserved as a standing, re-runnable falsification
record**, not deleted outright: a dedicated test,
`the_weakest_raider_beats_the_strongest_abstainer_primary_claim`, still
computes and asserts it, `#[ignore]`d under this repo's
`PREREGISTERED, not met:` idiom and citing this decision directly (the guard
at `windows/lab/tests/preregistration_guard.rs` demands a sanctioned
reason — a cost or a decision cite — for any `#[ignore]` inside a
`tests/*calibration*.rs` file, and a registry-slug-only citation, the form
this repo's five earlier `PREREGISTERED, not met:` pins all use, does not
satisfy it because none of those five live inside a file the guard's glob
matches). Anyone who wants to confirm the falsification has not silently
reversed itself can run it by hand with `--ignored`.

**The registry carries the finding as `refuted`, not `raw`.** Per decision
0131's admission rule — the row's own central claim was tested and found
false, and no artifact ships from it — this is a clean `refuted` case: the
claim's successor already exists and is already asserted (the SECONDARY
rho), so there is no future work item to file, unlike the `raw`-status
`PREREGISTERED, not met:` pins that await a successor axis nobody has built
yet. `TOOL-min-vs-max-separation-compares-an-overlap` records it.

## What was rejected

**Raising, re-fitting, or re-deriving a new threshold to rescue the sign
claim** — considered and set aside for the reason The Assize already gave
when it deleted `NONRAIDER_MAX` and `SEPARATION_FACTOR` rather than move
them: moving a preregistered bound to rescue a prediction after the physics
under it changed is exactly what this project's method forbids (decision
0016). The honest reading is that a two-set partition of a continuum stopped
being a partition at The Tolerance, and The Radiation's roster is the delayed
consequence arriving, not evidence the disposition mechanism stopped
ordering anything — the rho reading says the opposite.

**Widening the partition to a rung-weighted or continuous statistic** —
considered, because it is the shape of remedy `BIO-rung-weighted-concentration`
took for an unrelated falsification (a stronghold-only axis needing a
rung-weighted successor). Rejected here because the successor already exists
and is already shipped: the SECONDARY Spearman correlation *is* the
continuous statistic a min-vs-max comparison approximates, so building a new
one would duplicate an assertion this file already carries rather than fill
a gap.

## Consequences

- `windows/lab/tests/disposition_calibration.rs`'s always-run battery goes
  green again: it asserts the `RAIDER_MIN` floor, the `MIN_RATE_SPAN` guard,
  and the SECONDARY rho claim, none of which this decision touches.
- A new standing, `#[ignore]`d test carries the falsification forward,
  re-runnable by hand, so a future silent reversal (the rate ordering
  recovering separation on its own) is discoverable rather than assumed.
- `cli/tests/heavy_tier.rs`'s `EXPECTED_UNTOKENISED` roster gains the new
  test's reason string — adding an untokenised `#[ignore]` reason is a
  reviewed roster change by that file's own design, not a silent one.
- `book/src/frontier/idea-registry.md` gains
  `TOOL-min-vs-max-separation-compares-an-overlap`, status `refuted (0134)`.
- `docs/digest/` is regenerated in the same commit as this record, per the
  root `CLAUDE.md`'s generated-artifact discipline.

## See also

`cfc028b8` (The Radiation's falsification, with the full separation table and
the pre-Radiation-nine control); [The Radiation chronicle](../../book/src/chronicle/the-radiation.md);
`windows/lab/tests/disposition_calibration.rs` module doc, "THE RADIATION'S
FALSIFICATION OF THE PRIMARY CLAIM" and "THE PRIMARY CLAIM, RETIRED"; decision
[0016](0016-studies-preregister-hypotheses.md); decision
[0131](0131-refuted-is-a-seventh-registry-status.md).
