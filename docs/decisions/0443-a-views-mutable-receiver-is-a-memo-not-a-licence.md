# 0443. A view's mutable receiver is a memo, not a licence

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0092](0092-derivation-at-named-sites.md),
[0436](0436-the-startup-is-a-frame-with-pluggable-views-not-a-screen.md),
[0437](0437-a-view-shows-what-exists-and-one-that-cannot-speak-is-skipped.md) ·
[The Overture](../../book/src/chronicle/the-overture.md)

In the context of a frame that renders every speaking view at every rung,
facing a view that must build an expensive derived structure to draw at all, we
decided `View::render` takes **`&mut self`** so a view may memoise what its
arguments imply — and that the mutability is **a memo, not a licence**: two
calls with the same arguments must return the same grid.

**Context.** The atlas needs a nearest-vertex index over the geosphere, which
costs ~200 ms to build and can only be obtained from `artifacts.terrain`, i.e.
from inside the render call. Under an `&self` signature it would be rebuilt at
every rung it speaks at — roughly 600 ms on a 3,054 ms build, a ~20% regression
in the one path this campaign exists to improve. The doctrine is not new: this
codebase already states that such an index "is built ONCE by the caller and
passed in, never rebuilt per call".

**Why not a `OnceLock` under `&self`.** It works, and it is deliberately not
the answer: it hides mutation behind an immutable signature and makes every
view reinvent the same escape hatch. Naming the mutability in the trait puts
the rule where a reader of the trait will find it.

**Consequence.** Rendering must stay a pure function of
`(world, rung, artifacts, w, h)` as far as its **output** is concerned. The
mutability exists to cache what those arguments imply, never to carry state
between renders that changes what is drawn. A view that violated this would be
undetectable from the frame's side, so the constraint lives in the contract's
own doc comment and in the views' tests rather than in a mechanism.

Relatedly, no view may re-derive terrain or climate inside a render
([0092](0092-derivation-at-named-sites.md)): the artifacts the build already
holds are handed over precisely so a second construction site is never opened,
and doing so would cost ~199 ms + ~69 ms per render for data the view was
given.

**See also.** Spec §3; `clients/game/bin/src/overture/view.rs`;
`clients/game/bin/src/overture/atlas.rs`; `clients/game/bin/src/plate.rs`.
