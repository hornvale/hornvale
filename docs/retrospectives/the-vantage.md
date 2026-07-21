# Retrospective — The Vantage

A small, focused campaign — explicit view switching — and the first of a
four-campaign program (view-switching → voxel globe → voxel map → pixel-art
rework). Process notes:

## What worked

- **The ideonomy lift named the keystone in one pass:** two orthogonal axes —
  discrete *which view* and continuous *how close* — were overloaded onto one
  input (the wheel). Decoupling them is the whole design, and it *removed*
  complexity rather than adding it (the dolly-limit detection and handoff intents
  deleted outright). When a change makes the code smaller, that's usually the
  right cut.

- **Doing the foundation first.** Nathan had five directions; the substrate
  (how you move between views) was chosen before the render-style work that sits
  inside the views. Settling navigation first means the voxel/pixel campaigns
  don't have to also redesign the view model.

- **The e2e was the real gate.** For a navigation change, the unit tests can't
  see the behaviour that matters — that the dropdown switches and the wheel does
  NOT. The e2e round-trip (System→Globe→Map→System) and the explicit
  wheel-no-longer-switches test are what actually prove the campaign's thesis;
  they caught a stale test referencing the removed view button, too.

## What bit

- **A parked subagent.** The e2e implementer started the suite in the background
  and "waited for a notification" — its detached job dies with the turn, so it
  hung with no result. Un-parking with a foreground-poll instruction recovered
  it. The standing rule holds: dispatched agents run long commands in the
  FOREGROUND and paste what happened, never what they intend to wait for.

- **A stale worktree wasm masqueraded as 15 failures.** The fresh orrery client
  expects the freshwater scene fields, but the worktree had been seeded with the
  pre-freshwater `hornvale_world.wasm` (copied from the main checkout, which was
  never refreshed). Swapping the v12 wasm in dropped the failures to zero. When
  setting up an orrery worktree against a client that reads new scene fields,
  seed it with the CURRENT wasm, not the main checkout's stale copy.

- **A type-hole in `e2e/`.** The e2e helper had a missing-argument bug that
  `npm run build`'s type-check never saw, because `tsconfig`'s `include` is
  `["src"]` — `e2e/` isn't type-checked at build. Only the actual test run caught
  it. Worth knowing: e2e code is validated by running, not by `tsc`.

## Follow-ups
- Rename the now-misnamed `setViewButtonFor` helper (it sets the dropdown, not a
  button) — cosmetic.
- In-map pan/zoom; Map URL-addressability (deferred here).
- The queued program: voxel globe + deeper LOD, voxel map, pixel-art rework.
