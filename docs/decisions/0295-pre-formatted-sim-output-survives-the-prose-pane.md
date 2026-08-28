# 0295. Pre-formatted sim output survives the prose pane

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (the founding defect
report) · **Relates:**
[0294](0294-a-mode-gesture-is-not-a-fetch.md) (which removed the most common way
of hitting this), [0022](0022-sim-emits-data-clients-render.md);
[The Quadrat](../../book/src/chronicle/the-quadrat.md)

In the context of a prose pane that split paragraphs on `\n` and rejoined them
on `split_whitespace()` — collapsing every run of spaces and left-flushing any
picture that passed through it — we decided that **a line that fits the pane is
preserved verbatim, and a line that exceeds it is clipped or horizontally
scrolled, never re-flowed.**

## Context

Reproduced, not inferred: the committed seed-42 chart reference rendered through
the real pane came out left-flushed with every run of spaces collapsed.

```
plate (correct)          prose pane (mangled)
     +                   +
  + +   +                + + +
+ +   + +   +            + + + + +
+ +   @ +   +            + + @ + +
```

**Direction is the whole rule.** Prose may grow downward; a picture may not grow
at all. So the fix is not "wrap what does not fit" — an earlier draft said
exactly that, and would have word-wrapped an over-wide chart line, reproducing
the defect at a wider pane. The in-repo precedent for the scrolling half is the
map strip's own marquee.

**The classification is per LINE, not per block**, because the wire carries no
marker distinguishing a pre-formatted block from prose. A prose line long enough
to exceed the pane is wrapped (correct); a picture line long enough to exceed it
is clipped (correct); a prose line that happens to be short is preserved verbatim,
which is identical to wrapping it.

## Consequences

- **No SGR reaches the prose channel.** `strip_sgr` is applied per line **before
  the width is measured**, and that ordering is load-bearing: an escaped row
  costs 17–23 bytes per glyph, so measuring first would clip a *fitting* picture
  down to its opening escape bytes. (The escape path is live but latent — seed 42
  turn 0 reports "0 tinted, 31 withheld" — so this guards a defect that has not
  yet been seen rather than one observed in the wild.)
- **The stripping is a client-side remedy, and the sim-side fix is registered
  rather than shipped.** Asking the sim for an escape-free lens was measured
  unimplementable four ways: `Session::map` picks the lens itself with no per-call
  parameter and no lens argument in the grammar; `PossessOpts.eyes = Off` yields
  terrain but drops `scene.sight`; rewriting `map` to `!map` bypasses the body's
  own refusal check; and the driver cannot re-render into the snapshot at all,
  because the pane reads a JSON string and `serde_json` is a dev-only dependency
  there. A per-call chart lens is additive and moves no byte, but touches 17
  exhaustive `PossessOpts` constructions.
- **What we give up:** an over-wide picture is not shown in full. It is clipped
  or scrolled, and the reader must widen the pane or use the map focus. That is
  strictly better than showing it re-flowed, which shows something false.

## See also

Spec §4.2; `clients/game/core/src/entry.rs` (`wrap`, `strip_sgr`).
