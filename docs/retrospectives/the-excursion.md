# Retrospective — The Excursion

Real pan/zoom camera controls for the Orrery's flat Map view, backed by a
same-face neighbor-tile ring (hot ring + warm cache halo), the map's dormant
zoom-driven symbol rung finally wired up, and a leftover ocean-wave sprite
removed. Seven tasks, subagent-driven, orrery-only, no wasm/producer change.
Closes a followup The Vantage named at the start of the four-campaign
view-remake program and left open through all four of its campaigns.

## What worked

- **Pushing back on the controller's own first framing surfaced the real
  scope.** The natural first answer to "how do we handle panning past the
  tile's edge" was a hard clamp. Nathan's objection — "we're on a globe, how
  is there empty space?" — wasn't really a disagreement about the clamp, it
  was catching that the premise was wrong: the void wasn't a property of the
  world, it was a property of only ever having fetched one tile. Surfacing
  the actual code constraint (one mounted tile, no adjacency table for
  crossing a cube face) before re-asking the question turned a defensive
  feature (stop the camera from going somewhere bad) into the real one (give
  it somewhere real to go).

- **Two ideonomy passes, not one, and the second wasn't a formality.** The
  first pass (cycle organon: open→orient→roam→saturate→depart) produced five
  concrete folds — eager ring fetch, the stable coordinate frame, recenter
  hysteresis, center-tile-only symbols, cache-clear on a genuine region
  change. Asking "have we converged?" and running a second, differently-timed
  pass (cross-domain re-instantiation into a river/floodplain analogy) found
  two more that the first pass's own frame couldn't see: the pan clamp needs
  to be an *active* constraint, not an assumed consequence of nothing being
  there to render, and the cache needs a genuine upper bound, not just
  delayed eviction. Both became real, load-bearing parts of the shipped
  design. Convergence is a real state, not just "we did one pass" — the
  second pass earned its keep.

- **The origin/center split, stated as an explicit invariant before any code
  was written, held across seven separately-reviewed tasks.** Every one of
  the seven tasks' individual reviews re-verified this rule by name and found
  it uncompromised in its own scope; the final whole-branch reviewer verified
  it end-to-end across all seven at once and reached the same conclusion.
  Naming the invariant in the spec, not just discovering it during
  implementation, is what let seven different subagents build against it
  without ever needing to rediscover it.

- **A plan-writing self-review caught two real bugs before a single
  subagent touched the code.** Re-reading the draft plan against the spec
  found a fragile position-reverse-engineering path (recovering a tile's
  address from its rendered world-space position via rounding, instead of
  just storing the address) and a construction-time crash waiting to happen
  (`MapControls`'s constructor unconditionally touches its DOM element's
  style property; the plan's first draft would have passed `undefined`).
  Both were fixed at the desk, for the cost of reading `three.js`'s own
  source once, rather than at the cost of a failed subagent dispatch and a
  re-plan.

## What bit

- **Every per-task test ran from the same starting condition, and the one
  real bug lived exactly in what that condition couldn't reach.** Seven
  tasks, seven clean reviews (one with a same-task fix loop for a coverage
  gap) — and the campaign's one substantive defect survived all of them,
  because every test, in every task, started from a freshly-opened region
  where the camera's remembered look-at point and the tile grid's coordinate
  origin happen to be equal. The bug only exists on the path where they
  diverge: switch style after panning, or leave the map and come back having
  panned before. No single task owned that path — Task 3 set up the origin,
  Task 5 added the camera and the panning, Task 5's own `setStyle` change
  touched the seam between them — and no single task's reviewer had the
  whole picture to see it. This is the same shape of lesson prior campaigns
  have already named from the product side (a green test proves the
  mechanism, not the look); here it proved the mechanism, from a state the
  mechanism was never asked to be in. The final whole-branch review is not
  a formality layered on top of seven good reviews — it is the only point
  in the process with the standing to ask "does this hold from every
  starting condition," and it should stay mandatory even when every prior
  gate came back clean.

- **The bug's own fix needed a second look before it was actually fixed.**
  The first attempted fix corrected the camera's aim point but not its
  position, leaving a smaller, subtler version of the same class of bug (an
  ~11° view skew instead of a wrong-tile recenter) — caught only because the
  fix was re-reviewed rather than accepted on the fixer's own report of
  green tests. A regression test passing is not the same claim as "the bug
  is gone"; a fix for a spatial/geometric bug needs the same skepticism the
  original finding did, not less, because the two halves of one bug can look
  like two different bugs from the fixer's seat.

- **The controller, not a subagent, has to be the one who looks.** Every
  functional claim in this campaign — the ring loads, the clamp holds, no
  wave sprites remain — could be verified by a subagent reading test output.
  Whether a rendered scene actually looks *right* (does the ring seam
  cleanly? does a zoomed-out view read as one continuous landscape rather
  than a nine-tile jigsaw?) could not, and the one real visual defect found
  this campaign — a voxel elevation cliff at a tile boundary, invisible on a
  single mounted tile because there was never a neighbor to disagree with —
  was found exactly this way: a person driving a real browser and looking,
  not a subagent reading a diff.

## Follow-ups

- **Voxel cross-tile elevation seam** (visual-pass finding) — two
  independently-built ring tiles at different elevations show a visible dark
  gap at their shared boundary, because the voxel wall-builder only ever
  draws a wall between cells inside one tile's own grid, never on a tile's
  outer edge. Never in this campaign's scope (no cross-tile wall-stitching
  was ever requested); only visible now that a neighbor tile exists at all
  to disagree with. A natural next campaign, building on this one's ring.
- **The `beginRegion`/`setRegion` sibling of the camera-anchoring bug** — a
  smaller residual the final review's *second* fix round surfaced and
  explicitly deferred: re-entering the map after a prior pan session can
  leave the camera's position (not just its aim) stale. Cosmetic, bounded,
  worth a ticket rather than a third fix round on this campaign.
- **Cross-cube-face-boundary panning** — stitching the ring across a face
  seam (not just a tile seam within one face) needs a 24-case adjacency
  table this codebase has never built, on either the client or the sim
  side; corners (three faces meeting) are a strictly harder case than edges.
  Named explicitly rather than attempted.
- Doc-comment sweep: a few stale "wave" mentions and one misplaced doc block
  survived individual tasks' scope; aggregate cosmetic cleanup, not urgent.
- Tightened test coverage: no test currently asserts recenter *direction* or
  camera *position* directly, only that a recenter/request happened — a
  sign-inverted implementation could theoretically still pass. Verified
  correct by hand at every review round; a reasonable hardening pass, not a
  blocker.
- Map URL-addressability (a Vantage-era followup this campaign did not
  touch) remains open.
