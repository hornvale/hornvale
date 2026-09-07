# 0906. The walk band draws the compass rose, not a projection of it

**Status:** Accepted (2026-09-07) · **Decider:** Nathan ·
**Relates:** [0141](0141-compass-navigation-is-an-overlay.md) (a heading is
an overlay resolved against adjacency — this is the picture catching up with
that), [0507](0507-every-lattice-in-the-project-is-eight-connected.md) (the
eight neighbours a rose names),
[0510](0510-compass-input-is-four-way-primary-and-eight-way-capable.md) (the
arrows this makes exact),
[0117](0117-the-client-re-derives-nothing-the-sim-emits.md) (the client
consults the sim's own movement rule rather than deriving a second frame),
[0287](0287-a-zoom-rung-is-a-mesh-depth.md) (the rung the map keeps);
`docs/superpowers/ledgers/2026-09-06-the-newel.md` (R13, R14),
`docs/superpowers/ledgers/2026-09-06-the-sett.md` (S1-S22)

In the context of a bug report — *"I press the left arrow, sometimes I move
southwest"* — and of a walk-band map that was a clamped Mercator raster while
movement was an edge of the facet adjacency graph, we decided that **at the
walk rung the picture is the movement rule iterated outward from the
observer: box `(j, k)` is the facet reached by `k` steps along the
observer's own `heading_rose` north/south chain and then `j` along that
facet's east/west chain**, accepting a fold at the eight cube corners and at
the two poles, and a redraw that consults the mesh where it used to consult
a cache.

The map keeps Mercator at every rung. Mercator is the map; the graph is the
walk.

## What the report was

Not a malfunction in any component. Measured over 3,456 facets at walk rung
13, each neighbour resolved through the shipped `heading_rose` and projected
through the shipped `mercator::project`:

```text
              equatorial faces      polar caps
  W lands in the box to the left    83.0%          17.7%
  neighbours landing on the observer's OWN box, equatorial:  1.13%
  W lands at  (-1, 0) 83.0%   (-1,-1) 8.9%   (-1,+1) 8.2%
```

One left-press in six did not go to the box on the left, and 1.13% of moves
landed on the box the observer already occupied — press a key and the map
does not move at all. The compass word, the movement and the drawing were
each correct; together they surprised the player.

## Why the rose and not the lattice

The predecessor campaign measured two candidates and chose the
**lattice-transport** raster. Re-measuring its own frozen prediction as
written (see [0907](0907-a-preregistered-check-must-name-the-axis-it-could-fail-on.md))
separated them, and a third candidate — following the compass chains rather
than the lattice — dominates on every axis. Over 288 facets at a 41x21
plate:

```text
                                        lattice     rose chain
  arrow lands in its box, equatorial      100.0%       100.0%
  arrow lands in its box, POLAR CAPS       66.7%       100.0%
  picture unchanged under one step, caps   66.7%       100.0%
  up-error, caps (mean / max, degrees)  28.5/58.9     8.8/24.4
```

And the picture's own coherence — from a box that is **not** the observer's,
does the right arrow reach the box drawn to its right:

```text
  rose chain, polar caps    95.95% row   76.55% column
  lattice,    polar caps     1.49% row    1.40% column
```

The two are the **same raster on the four equatorial faces** — the lattice
frame and the compass rose agree there at 18,432 of 18,432 words — which is
why the third candidate went unnoticed: every equatorial measurement returns
an identical number for both. They part only on the polar caps, which are a
third of the world.

Decisively, the rose raster **changes nothing sim-side**: `heading_rose` is
untouched, its measured error ceiling is untouched, no movement verb is
added and no compass word changes meaning. Binding the arrows to the lattice
instead was measured and rejected — the worst word would point **58.9
degrees** off its own bearing on the caps, against a pinned ceiling of
34.578.

## What is accepted

**A fold at eight points and two poles.** At a cube corner the rose refuses
one bearing, so a chain ends; the boxes past it are blank and are never
filled by a repeat, a draw or a substitute. The neighbourhood is 10 facets
wide on an equatorial face and 28 on a polar one, out of 402,653,184 facets
in the band. At each pole the meridian chains converge and the picture
repeats — 456 boxes of 861 at the pole itself, **no blanks** — clearing 16
facets (0.176 degrees) out.

**This is better than what it replaces, at the same places.**
`mercator::project` returns `None` past 85 degrees and
`centre_window_on` propagates it, so today an observer within **5 degrees**
of a pole cannot have the walk view centred on them at all — 1.00% of the
polar caps, against the rose fold's 0.176 degrees: 28x the radius, ~800x the
area, and a refusal rather than a repeat.

**A redraw that costs more.** The graph raster has no chart-tile
coordinates, so it cannot use the tile cache. Measured, and reported as a
falsified prediction rather than retuned: 0.380 ms per keypress before,
9.712 ms after — of which **65.3% was one un-memoised call** that the tile
cache had been hiding in the old path too, and which is now memoised.

## What was refused

- **Disclosure instead of a fix** — naming the true bearing in the endpaper.
  A player pressing left into a wall is irritated by the outcome, not
  un-irritated by an explanation.
- **Cross-track correction** — still correct, and it repairs a promise the
  game need not make once the picture draws the graph. It would cost carried
  state and a decision superseding a shipped one.
- **Binding the keys to whichever neighbour is drawn leftmost** — makes the
  key agree with a picture that is itself wrong, and leaves two neighbours
  in one box unreachable.
- **A height-matched Mercator.** It does not exist: the height that makes one
  row equal one facet edge at the equator is the height already shipped. The
  misses are not a pitch error — a lattice row is not a line of constant
  latitude.
