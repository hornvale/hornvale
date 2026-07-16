# The Lens — retrospective

Process lessons, not product. One page. Campaign: a client-side lens registry
(six lenses + a wind overlay) draining four unrendered `scene/tiles/v1`
layers; shipped in the orrery repo, zero producer work.

## The headline: four defects shipped with every gate green, and all four came from the plan

The implementers transcribed the plan faithfully. The plan was wrong four
times, and the CLI gates could not see any of it:

- **An invisible, unclickable HUD** (Task 7). Built from my plan's CSS
  snippet, without the base `hud` class — the only rule granting
  `position: absolute` — so the panel sat in static flow and painted behind
  the canvas. `elementFromPoint` resolved to the canvas; only programmatic
  clicks reached the buttons. 229 tests + `tsc` green throughout.
- **A beige temperature map** (Task 9). Spec §7 chose a light midpoint for
  headroom under the terminator — correct about dimming, wrong about
  *tinting*: the globe's directional light multiplies vertex colours, so a
  near-white midpoint under a warm G-class star renders tan. Compounded by a
  ±40 °C clamp that parked a habitable world's tiles at that midpoint.
- **A water-veiled data globe** (Task 9). The spec gated ice to `natural` but
  never the ocean, so ~73% of every data lens sat under translucent blue —
  hiding real data (sea temperature, moisture, sub-oceanic plate id, unrest).
  The spec missed it because ice's *mechanical* reason (it blends into base
  vertex colours) does not apply to a separate mesh, so the *presentational*
  reason was never surfaced.
- **An ice test that asserted nothing** (final review). Proven by mutation:
  `const icy = false`, killing ice everywhere, passed 240/240 — including the
  test named "blends ice under natural".

The common cause is that **jsdom has no layout, no paint order, and no
hit-testing**, and the dataviz validator checks a colour against a
*background*, never against a *multiplier*. For a rendering campaign, "render
it and look at it" is not ceremony; it is the only gate that sees the
deliverable. Three of the four are test-design defects in my own plan — the
gate ladder can only be as good as the assertions someone wrote.

**And a report is not a look.** The Task 9 verification subagent reported
"every lens recolors correctly, legends match" and described "a clean jagged
boundary between a green plate and a blue-gray plate" — that was the
*coastline*. The controller caught both real defects by opening the PNGs.
When the deliverable is visual, the verification artifact must reach the
person judging it; a subagent's prose verdict about pixels is a summary of a
look, not evidence of one.

## The counter-lesson: the loop worked wherever a reviewer measured

Every place the process caught something real, it caught it by measurement,
not assertion:

- **ΔE 2.5** — all 28 palette pairs measured against the app's real
  background found blue↔violet effectively identical under protanopia. Safe
  in canonical slot order, catastrophic on a map. The first 6-slot pick was
  eyeballed and failed the validator outright.
- **99 vs 6** — a reviewer's symmetric-rounding "fix" was declined on
  measurement: on the real palette the diverging arms sit 99 and 6 units from
  the midpoint, so mirror symmetry is not a property this palette *has*, and
  a 1/255 tie-break is noise against it. The same review correctly found the
  symmetry test itself was degenerate — comparing a computation to itself.
- **`icy = false`** — mutation, not inspection, proved the vacuous test.
- **The v3 distribution** — seed 42 measures median −9 °C, 23% of tiles
  pinned at the cold pole, 0% reaching the hot one. Kept ±30 anyway (a fixed
  clamp is what buys cross-world comparability), but now on a number.

Measurement is also what made the G4 plan self-review worth its cost: it
found that every test block in my plan referenced helpers that **do not
exist** (`tilesFixture()`, `buildGlobe`, `tilesDoc()`), written from memory of
the pattern rather than against the real test files. A plan whose fixtures do
not compile burns a subagent's entire context rediscovering the right names.

## What to carry forward

- **Re-verify a blocking external fact at the gate where it blocks.**
  Absorption was deferred (ledger #12) because the `world-wasm-v3` release did
  not exist — verified true when written, stale hours later when presented at
  G6. Nathan caught it, not the controller. A cross-campaign blocker verified
  once can expire silently while a long session runs; "is this *still* true?"
  belongs at the gate, not only at discovery.
- **A green `npm test` does not mean the types check.** vitest/esbuild strips
  types without checking them: adding two required `TilesScene` fields broke
  `tsc --noEmit` in five fixture literals while the suite stayed green. The
  plan was amended mid-flight to require both gates before every commit.
- **A rendering-debt check belongs in `closing-a-campaign`.** The debt table
  (shipped / parsed / evaluated / rendered) took an explicit enumeration to
  surface and had accumulated silently across three producer campaigns. One
  line — "does a consumer render this?" — would catch the next one at merge
  instead of a campaign later.
- **Plans that quote test code must be written against the real test files.**
  See above; three of four shipped defects were test or CSS snippets authored
  from memory.

## Follow-ups

Registered in the campaign's follow-up ledger; the load-bearing ones:
ice as an independent overlay (would retire the `natural` identity checks,
alongside a `decorated`/`showsIce` lens property); the lens is absent from
URL state (share a link, the recipient opens on `natural`); plate boundary
ink is only marginally legible at globe zoom, which weakens the secondary
encoding the 8–12 CVD floor band leans on — possibly retired by tightening to
the all-pairs-≥12 four-colour subset if greedy never exceeds 4 across the
census seeds (**measure first; one seed is not a distribution**); the ±30 °C
clamp deserves a multi-seed temperature distribution before anyone retunes
it; wasm-backed tests sit at 4.5–6.4 s against vitest's 5 s default with no
margin, which a `beforeAll`-shared fixture would retire as a class.

## Registry

One new row: **RENDER-9** (`raw`) — the client has no ground-truth view, only
registered lenses, and the HUD says which one you are wearing. Surfaced by
the G2 abstraction-lift and deliberately not filed until close, since it is a
book/vision claim rather than a build item.
