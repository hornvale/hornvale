# The Portolan, part II — the world map

**Status:** DRAFT — awaiting G3 review. **Campaign:** The Portolan (extended).
**Branch:** `campaign/the-portolan`. **Extends:**
`docs/superpowers/specs/2026-08-19-the-portolan-design.md`.

A Mercator chart of the whole planet, scrolled and zoomed, with the cursor
already built pointing into it and a status strip that carries the whole
containment chain.

---

## 0. Why this extends rather than closes

The Portolan's first four tasks shipped the **mechanism**: look mode, a cursor
that is the terminal's own hardware cursor, a plate-width strip, declared
per-class salience, a load-time cell index, and cursor-tracked resolution.
All reviewed, all mutation-proved.

**And it is nearly inert, because there is nothing to point at.** Measured:
the walk band spans **~3.92 km** (`distance_rad` max 6.149372e-4 rad at a
~6,371 km radius) against **~110 km** terrain-cell spacing — 28x smaller. Every
cell in view belongs to the same landmass, the same catchment, the same
volcano. The only reachable transition is named-feature → unnamed terrain.

The original spec's §2 band table asserted the world plate existed and the
others did not. **The reverse is true**: `Spatial` carries exactly
`Walk { chart }` and `Chamber { plan }`, and no world view exists anywhere in
`clients/game`. That was an error in the spec, discovered by implementation.

Nathan's ruling: *"No point in merging something we can't present."* So the
campaign extends to build the thing the mechanism was for.

---

## 1. What this produces

- **A world band in the plate**: an ASCII Mercator chart of the planet.
- **Zoom and scroll**, with a principled ceiling (§4.3).
- **The cursor resolving against it** — the index and salience already exist.
- **A status strip carrying the full containment chain with type labels**, and
  **scrolling when the content exceeds the strip** (§5).

### What it does not produce, and each is its own campaign

- **Glossed landform names.** Nathan's target line reads
  `Vngashngatva ("Old Flaming Man", an active volcano)`. **The translation
  does not exist**: `NameKind::Landform` draws a bare 2-3 syllable stem
  (`draw_syllables(stream, 2, 3, false)`), and `GeneratedName` carries
  `roman`/`ipa`/`espeak` and **no gloss**. Settlements *are* glossed, through a
  separate `glossed_name` that returns `(GeneratedName, String)` — but it needs
  a `SiteConcepts` vocabulary that exists only for settlement sites, and
  routing landforms through it is a **different stream draw and therefore an
  epoch**, moving every landform name in every world including the Gazetteer's
  committed artifact. It is the largest of the three and it is not this one.
- **Mountain ranges.** The target line reads
  `in Flurbesagt ("World's Spine", a mountain range)`. `FeatureClass` is
  `Volcano / Landmass / Sea / SaltLake / River`. A range needs **relief
  clustering**, which the Gazetteer put explicitly on the far side of its
  scope line ("graph traversal ships, clustering waits"). Its natural campaign
  also carries bays, capes, straits and peninsulas.
- **Common as the primary name.** A rendering choice that only becomes
  meaningful once glosses exist.

---

## 2. The architecture, and the constraint that chooses it

**`hornvale-game-core` depends on no hornvale crate.** It is a pure
`vessel/session/v2` → character grid renderer, and that purity is worth more
than any convenience.

The world map needs terrain for the *whole planet*. The snapshot does not
carry it and must not: `vessel/session/v2` is a cross-repo contract, and a
world-terrain channel would put a planet in every per-turn document.

So the split is the same one Task 3 already established, one rung wider:

- **`bin`** (which depends on `hornvale-worldgen` and `hornvale-terrain`,
  in-process) renders the Mercator plate into a `Grid` and hands it over.
- **`core`** composes that plate into the spread exactly as it composes the
  chart and the plan, and knows nothing about terrain.

**Consequence, declared:** a caller-supplied plate is content `core` cannot
verify — the same category as `Source::Look`, whose doc already says its
honesty is a caller discipline. The world plate's cells carry a `Source` that
says so, and §7 F2 settles which.

**No schema change.** `Spatial` does not gain a variant. The world view is a
*lens on the world*, not a band the character occupies — the character is at
the walk band the whole time.

---

## 3. Why Mercator

A **constant-bearing course draws as a straight line** on Mercator, and this
project's navigation model is a dead-reckoned rhumb course (The Rhumb).
Equirectangular would bend the very courses the sim computes. That is the
reason, and it is not decorative.

The spike's renderer already exists at
`windows/worldgen/examples/portolan_spike.rs` and routes through the kernel's
`math::ln`/`math::tan` — the libm-backed path, so it is cross-platform
deterministic. **Reuse its projection; do not write a second one.**

### 3.1 The projection's central line is derived from the world's physics

**A fixed geographic graticule is wrong for this world model, and the failure
is not hypothetical.** Tidal locking is pinnable (`--rotation locked`), commits
a `TIDALLY_LOCKED` fact, and drives a genuinely different climate:
`domains/climate/src/temperature.rs` organises a locked world's temperature as
*"a substellar cosine, hottest at `+x` and floored on the night side."*

The geometry, verified rather than assumed — `kernel/src/geosphere.rs:235-236`
gives `latitude = asin(z)`, `longitude = atan2(y, x)`, so **`+x` is exactly
(lat 0°, lon 0°)**, matching the convention `domains/astronomy` states twice
(*"the substellar point sits on the prime meridian"*).

So on a locked world the substellar point is on the equator, and the habitable
band is the **terminator** — the great circle 90° away, which runs **pole to
pole** through longitudes ±90°. A geographic Mercator puts **half the habitable
ring inside the polar clamp**: on the one world type where the poles are the
only livable ground, the clamp discards it.

**Therefore the projection is oblique, and its central line comes from the
world:**

| world | central line | what falls in the clamp |
|---|---|---|
| spinning | the geographic equator | the polar ice caps |
| tidally locked | the **terminator** | the substellar desert and the antistellar ice |

The clamp then always discards the least interesting 10°, which is what a
clamp is for. On a locked world the projection's two poles land precisely on
the substellar and antistellar points — the two places nobody lives.

**Note a property worth preserving:** a great circle's pole is 90° from every
point on it, so the terminator's pole *is* the substellar point. Making the
terminator the projection's equator therefore means putting the projection's
pole at `+x` — an axis swap, not an arbitrary rotation. Task 1 derives the
exact form; the spec fixes the requirement, not the matrix.

The central line is computed **once at world load** from committed facts, and
does not move as the player does (§3.2).

### 3.2 Re-centring is a command, not a behaviour

Nathan's original proposal was to roll the projection continuously so the
cursor's position is always undistorted — a true oblique Mercator, and
cartographically sound. It is **not** the default here, for three reasons:

- **The map would reflow every keypress.** Landmarks move while the player is
  navigating by them, which defeats the one thing a map is for.
- **It destroys the cacheable layer.** `RENDER-three-channels-three-clocks`
  records that the glyph layer is geological and the most cacheable thing on
  screen; a per-cursor projection makes it per-keypress.
- It pays transcendental math over every plate cell, constantly, for a benefit
  that is already present near the central line.

So: **stable by default, re-centrable on demand.** An explicit command rolls
the projection to put the cursor on the central line. The player gets the
oblique view when they ask for it and a map that holds still when they do not.

### 3.3 The clamp is disclosed

Mercator diverges at its poles, so the projection's latitude is clamped —
±85° in the **projection's own frame**, not the geographic one. **The caption
states the clamp and names the central line**, because on a locked world "what
is missing" is a different pair of places than a reader would assume. This is
decision 0142's rule that a lost axis is declared, applied to lost *area*.

---

## 4. Zoom and scroll

### 4.1 The plate is a window onto a larger chart

The plate is 40 columns at the floor. The whole planet at 40 columns is ~9° a
character — around nine terrain cells wide. So zoom is not resampling the
plate; it is **choosing how much of a larger virtual chart the plate shows**,
and scroll is choosing *which part*.

### 4.2 Scroll follows the cursor

The cursor already exists and is already clamped to the plate. **Scrolling is
what happens when it reaches an edge**: the window moves, the cursor stays.
No new keys, no separate mode, and the cursor remains the single thing the
player moves. Longitude wraps; latitude stops at the clamp.

### 4.3 Zoom stops where the data stops — a principled ceiling

**Maximum zoom is one character per terrain cell.** Below that the map would
be drawing detail the world does not have, and decision 0123 already governs
this: *disclose a resolution rather than refine a field.* At the ceiling the
caption states the resolution rather than the map inventing texture.

At level 6 (~110 km cells, ~40,962 of them) the equator is roughly 364 cells
around, so the ladder runs from "the planet in 40 columns" to a virtual chart
about 364 columns wide. **§7 F1 measures the real number rather than
inheriting this estimate.**

Minimum zoom is the whole planet — there is nothing further out to see.

---

## 5. The strip carries the chain, and scrolls

The first spec cut the strip to **one name** because the full chain overflowed
40 columns 12% of the time. **That cut is withdrawn.** Two things changed:

1. H1 measured max name length at **9 characters** — the chain usually fits.
2. Nathan's ruling: the strip's *content* may exceed its *width*, scrolling or
   marqueeing.

Once content may exceed width, **the width constraint disappears entirely**
rather than being relaxed, and with it the fidelity cut. The strip carries:

```
Vngashngatva (a volcano), on Kxsokxkxzhakx (a landmass)
```

— every feature in the containment chain, most specific first (§3.2 of the
first spec), each with its class named in prose.

**Scrolling is a client behaviour and must not become a clock.** It is driven
by the same redraw the rest of the client uses; it introduces no timer, no
wall-clock, and no animation loop. §7 F3 settles how.

---

## 6. Refusals

- **No schema change** (§2). `Spatial` gains no variant.
- **No second projection.** The spike's is reused, or moved somewhere both can
  call. Two copies of a projection is how a chart and a cursor come to
  disagree — this campaign has already fixed that exact defect once, in
  `chart.rs`'s `boxes_of`.
- **No new external dependency.** `clients/game` carries `crossterm` and
  nothing more.
- **No polar fabrication.** Above the clamp, nothing is drawn and the caption
  says so — naming the central line, since the clamped pair depends on it.
- **No per-cursor reflow by default** (§3.2). Re-centring is an explicit
  command; the map holds still unless asked to move.
- **No detail below the cell floor** (§4.3).
- **The 80×24 floor is inherited and may not be weakened.** If the world plate
  does not fit at the floor, the plate changes, not the floor.

---

## 7. What is unverified, and how each is settled

**F1 — the zoom ladder's real extent.** §4.3 estimates ~364 columns at the
ceiling from a ~110 km cell spacing. **Settled by:** Task 1 measures the
actual equatorial cell count at level 6 and reports the ladder's real rungs.
Every zoom constant comes from that measurement, not from this section.

**F1b — the central line's derivation, and its exact form.** §3.1 fixes the
requirement (projection pole at the substellar point for a locked world, the
geographic pole otherwise) and deliberately does not fix the rotation.
**Settled by:** Task 1 derives it, states whether it is an exact axis swap as
§3.1 suggests, and confirms it is transcendental-free if so — a rotation that
needs no `sin`/`cos` is one fewer cross-platform surface.

**F2 — what `Source` does a caller-supplied plate cell carry?** `Source::Look`
established the precedent for content `core` cannot verify. **Settled by:**
Task 2 chooses, and its doc states plainly that the world plate is supplied by
the caller and unverifiable here — the same honesty `Source::Look`'s doc now
carries. **If it reaches for `Chrome`, that is wrong**: the plate is
world-derived, and `Chrome` means declared-inert.

**F3 — does the strip's scrolling introduce a clock?** It must not. **Settled
by:** Task 4 states what drives it. **If it needs a timer, that is a STOP** —
`Instant` is banned workspace-wide, `clients/game` has no animation loop
today, and adding one is a design decision, not an implementation detail.

**F4 — does the world plate fit the 80×24 floor?** The plate is 40×20 there.
**Settled by:** Task 2 renders at exactly 80×24 and shows the result. The
floor does not move.

**F5 — resolution cost at world zoom.** A cursor at the coarsest zoom covers
~9 terrain cells; resolution must still answer for exactly one. **Settled by:**
Task 3 states which cell a coarse character resolves to and why that choice is
honest rather than arbitrary.

---

## 8. Preregistered measurement

**H1 — the map is legible at the floor.** At 80×24, with the whole planet in
40 columns, land and water are distinguishable and the shape of the largest
landmass is recognisable against the Gazetteer's committed `elevation_ascii`
rendering of the same world. *Falsified if* the 40-column planet is
indistinguishable noise — which would mean the minimum zoom is too coarse to
be a view, and the ladder needs a floor above "whole planet".

**H2 — the cursor resolves consistently across zoom.** A cursor pointing at a
given feature at one zoom resolves to that same feature after zooming in and
re-pointing at it. *Falsified by* variation, which would mean the
screen→cell mapping is zoom-dependent in a way the resolver does not model.

**H3 — scroll and cursor stay coherent.** After scrolling, the cell under the
cursor is the cell the plate draws there. *Falsified by* drift between the
window's offset and the resolver's — **the exact defect class this campaign
has already hit twice** (a stale cursor, then a hardcoded plate height). A
test that only runs at one scroll offset cannot see it.

**H4 — a locked world's habitable band is not in the clamp.** Generate a world
with `--rotation locked`, and confirm the terminator band renders inside the
drawn area rather than inside the clamped one. *Falsified if* the habitable
ring is clipped, which would mean the central line is not being derived from
the rotation regime at all — the defect this whole section exists to prevent,
and one that a spinning-world test cannot see.

**H3 is the one at real risk.** Both prior instances were "a wrong name
indistinguishable from a right one", and both were introduced by a fix that
changed what a value depended on. Scroll adds an offset to every screen→cell
computation, which is precisely that shape a third time.

---

## 9. Out of scope, carried forward

- **Glossed landform names** — an epoch (§1).
- **Mountain ranges, bays, capes, straits** — a clustering campaign (§1).
- **Common as the primary name** — waits on glosses.
- **The walk/chamber cursor resolvers.** The cursor exists at those bands and
  answers "nothing here yet"; wiring them is separate.
- **Mouse support.** `crossterm` offers it; the key-driven cursor is the
  primitive.

---

## 10. Flagged for Nathan at G3

- **The fidelity cut from the first spec is WITHDRAWN** (§5). He authorised
  showing one name; the measurement and his marquee ruling both removed the
  reason. Un-cutting is his call and this records that he made it.
- **Three campaigns are deferred**, and one is an epoch (§1). His target
  status line is not reachable until all three land, and the spec says so
  rather than implying this campaign delivers it.

---

## 11. Decision to promote

> **A map's frame is a fact about the world, not about the graticule.** The
> line a projection holds true is derived from the world's own physics — the
> equator where a world spins, the terminator where it is tidally locked — so
> that what a projection must discard is always the ground nobody occupies.
>
> **A view of the world is a lens, not a band.** The character occupies a
> band; a map is something they consult. So a whole-world view adds no variant
> to the session schema and carries no per-turn payload — the client renders it
> from the sim it already has, and declares that the content is its own rather
> than the wire's. Zoom stops where the data stops: a map may disclose its
> resolution but may never invent detail below it.

---

## 12. Task outline

**Stage 1 — the chart**
1. Measure the zoom ladder (F1) and derive the central line (F1b). Reuse the
   spike's projection; no second copy. **Test on a `--rotation locked` world,
   not only seed 42** — a spinning-world fixture cannot see H4's defect.
2. The world plate rendered in `bin`, composed by `core`; F2, F4, H1.

**Stage 2 — moving in it**
3. Zoom, cursor-driven scroll, and the explicit re-centre command (§3.2);
   F5, H2, H3, H4.
4. The strip carries the chain and scrolls; F3.

**Stage 3 — close**
5. Chronicle, retrospective, registry rows, decision, freshness sweep.
   Delete all three spike instruments. **Absorb main and regenerate at every
   stage boundary, not only here.** Write the retrospective LAST — the
   Gazetteer's was authored early and under-reported itself by three findings.
