# 0292. Centre on arrival, anchor on gesture

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (autopilot; reached by
the campaign's reviewer adjudicating against the controller's own earlier
ruling, and adopted) · **Relates:**
[0291](0291-the-point-under-the-cursor-is-invariant-across-a-zoom-step.md)
(the guarantee this one scopes); [The Quadrat](../../book/src/chronicle/the-quadrat.md)

In the context of a map view that must both start somewhere sensible and hold
still under the reader's own gestures, we decided that **the view centres on the
observer when the reader ARRIVES at the map, and anchors on the cursor for every
GESTURE thereafter** — never both, and never centring on a zoom.

## Context

An earlier ruling in this campaign centred band B on the observer's own facet,
because the alternative was worse by a wide margin: origin `(0,0)` at the finest
rung is roughly 11,800 rows and 2,200 columns from the fixture's observer — the
arctic corner of a 23,245-column chart, a position nobody chose.

The implementer then removed observer-centring from the zoom path, contradicting
that ruling. Asked to adjudicate, the campaign's reviewer judged on the merits
without deferring to either party and agreed with the implementer, on an argument
the original ruling could not answer: **the centring rationale was about the
state at `start`**, and it does not reach a zoom, because after an anchored zoom
the window is never arbitrary. Holding the centring at a zoom into band B would
throw a reader from Antarctica to their own rainforest on a `+` press — which is
the founding complaint in the same shape.

## Consequences

- **The earlier ruling is amended, not overruled.** Centring on arrival stays and
  is correct; it simply does not extend to a gesture.
- **`enter_map`'s centring acts at any rung**, not only at band B. This was
  forced: with centring gone from the zoom path there was no way home at a coarse
  rung at all — the observer is not drawn off band B, so the reader could not
  even see where they were, and the only route back was `+` to the ceiling and
  then re-entering the map. Before the change, "zoom out one, zoom in one" had
  been an accidental home gesture.
- **What we give up:** a reader who deliberately scrolled somewhere and then
  re-enters the map loses that position to the centring. That is the accepted
  cost of having a way home at all, and the residual — an observer marker at
  coarse rungs — is registered rather than shipped.

## See also

Spec §4.1; `clients/game/bin/src/driver.rs` (`enter_map`, `apply_zoom`,
`centre_band_b_on_the_observer`).
