# 0436. The startup is a frame with pluggable views, not a screen

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0022](0022-sim-emits-data-clients-render.md),
[0055](0055-external-clients-consume-a-versioned-wasm-catalog.md) ·
[The Overture](../../book/src/chronicle/the-overture.md)

In the context of world generation costing **3,054 ms today** with the terminal
not opened until it returned — and Nathan's standing intent that genesis grows
toward a minute or more on slower hardware — facing a choice that looked like
"which loading screen", we decided the deliverable is **a frame with pluggable
views**: the frame owns the chrome, the progress substrate and the `space`
cycle, and a view owns the middle of the screen and nothing else. We accept
that the frame renders every speaking view at every rung rather than only the
current one.

**Context.** The candidate designs — a checklist, a prologue, a drawn world —
looked like alternatives and are not. Each is a *view*; the interesting
artifact is the contract between them and the frame
(`clients/game/bin/src/overture/view.rs`). That reframing is what makes the
next generator additive: when a music or literature generator exists it
registers a view and the frame does not change.

**Consequence.** Rendering the non-current views costs something on every rung
boundary. It buys the property [0437](0437-a-view-shows-what-exists-and-one-that-cannot-speak-is-skipped.md)
demands — pressing `space` can never land on an empty region, because there is
no rung at which a speaking view has no grid. The reason it must work this way
is a borrow: `RungArtifacts` hands the observer references that live only for
the duration of the callback, and a `GeneratedTerrain` is far too large to
clone per view, so rendering happens *during* the observation and the frame
keeps the resulting grids. `space` and the slideshow are then pure switches
between grids already in hand, needing no world at all.

**See also.** Spec `docs/superpowers/specs/2026-08-28-the-overture-design.md`
§2–§3; `clients/game/bin/src/overture/mod.rs` (`Frame::observe`).
