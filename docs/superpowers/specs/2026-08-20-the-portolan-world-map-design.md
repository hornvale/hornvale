# The Portolan, part II — the world map

**Status:** DRAFT — awaiting G3 review; reshaped by **Amendment 1 (2026-08-23)**, which is part of the same G3 package and changes what is *labelled*, not what is *drawn*. **Campaign:** The Portolan (extended).
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
`windows/worldgen/examples/portolan_spike.rs` (deleted at this campaign's
close, once ported; see git history at `0292de87f^`) and routes through the
kernel's `math::ln`/`math::tan` — the libm-backed path, so it is
cross-platform deterministic. **Reuse its projection; do not write a second
one.**

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

---

# Amendment 1 (2026-08-23) — the discovery layer

**Status:** drafted for G3 alongside the body above. **Ruling by:** Nathan,
2026-08-22. This amendment is additive: it does not supersede any section
above except where it says so explicitly, in §A2.

## A0. Why an amendment and not a rewrite

The campaign paused for 275 commits, during which four campaigns landed on
`clients/game` (The Stylus, The Chroma, The Wick, The Stride). The
reassessment that reopened it first proposed replacing the design outright
with a *remembered* map — geometry dead-reckoned from the possession's own
traverse. **Nathan refused that for the player** and the refusal restored the
body of this spec rather than replacing it:

> "This sounds like an excellent concept for NPCs but not necessarily the
> player, who counts on predictability and consistency to navigate the game
> world."

Once the map's geometry is complete and true, every load-bearing argument
above holds unchanged: §2 (`bin` renders the plate, `core` composes it, no
schema change), §3 (why Mercator), §3.1 (the central line derived from the
world's physics), §4 (zoom and scroll), §5 (the strip), and F2
(`Source::Look`'s caller discipline is the precedent for a caller-supplied
plate). This amendment changes what is *labelled*, not what is *drawn*.

**The framing defect worth recording**, because it is the transferable half:
the reassessment reasoned on a ONE-axis fidelity spectrum, from "pure belief"
to "ground truth". The design needs TWO axes — geometry-fidelity and
naming-fidelity are independent — and Nathan's ruling pins them at opposite
ends. That combination was not a rejected band on the spectrum; it fell
*between its lines* and was therefore invisible. A one-dimensional organon
cannot represent a two-dimensional decision, and its missing options do not
announce themselves.

## A1. The ruling

- **Geometry is complete from the first turn.** The whole planet is drawn,
  truthfully, at every zoom rung. Nothing about the terrain is withheld,
  fogged, distorted, or deferred. The player navigates by this map and it
  must be predictable and consistent.
- **Naming is earned.** A feature's NAME appears only once the possession has
  discovered it.
- **The unreliable map is re-homed on NPCs** and is not built here. Captured
  as `CLIENT-npc-dead-reckoned-map`, `CLIENT-loop-closure-collapses-map-error`
  and `CLIENT-hearsay-placed-features`.

## A2. What this amendment changes above

| section | change |
|---|---|
| §1 "What this produces" | gains a fifth bullet: labels are discovery-gated (§A3) |
| §8 H1 | **restated** — it was written as a monochrome legibility question one day before The Chroma landed (§A5) |
| §6 Refusals | gains three (§A7) |
| §12 Task outline | **replaced** by §A9 |
| everything else | unchanged |

## A3. Two visibility classes, and neither needs suppression machinery

The distinction falls out of what is already rendered:

- **Terrain-borne landmarks** — a volcano's cone, a river's course, a
  coastline, a salt lake's basin — are *in the elevation and biome render*.
  They draw whether or not they are known. Only the name is gated. This is
  Nathan's "some major landmarks would be visible but not labelled", and it
  costs one branch: do not write the label.
- **Point sites** — settlements, ruins, cave mouths — are not in the terrain
  render at all. They are invisible until discovered because they are simply
  **not drawn yet**, not because anything suppresses them.

**No feature is ever drawn and then hidden.** That refusal matters: a
suppression pass is where a client learns to lie about what it knows, and it
is also where an omniscient read leaks through a rendering bug.

**Addendum, final review (2026-08-23): the claims above are mechanism
claims, and each is true — but silent on a question they invite a reader
to assume is settled.** "Not in the terrain render at all" and "not drawn
yet" both describe what GATES a point site's glyph; neither says whether,
once the gate opens, the plate's own sampling actually PAINTS that glyph
anywhere on screen. It very often does not: only 1.1% of terrain cells are
ever an `area_majority` representative at the 80x24 floor (5.7% at the
design plate's own coarsest rung), so a discovered settlement can sit
undrawable at every zoom this client ships — measured, for the seed-42
flagship's own starting settlement, as true at every rung up to a
resolution more than 3x `MAX_VIRTUAL_WIDTH`. This is not a defect in the
discovery gate §A3 specifies; it is an entirely separate limit this
amendment never had reason to state, recorded as
`MAP-vertical-axis-undersamples-the-mesh` and
`MAP-settlement-glyph-may-be-unreachable-at-any-shipped-zoom`. §11's own
"a map may disclose its own resolution but may never invent detail below
it" was built for the strip (F5) and not for the plate's own content —
this amendment's silence on that gap was a choice, made explicit here
rather than left to be rediscovered.

## A4. Two different questions, and conflating them was the defect

Nathan's F6 and F7 rulings (2026-08-23) share one rule, and this amendment's
first draft broke it by asserting that "a feature is discovered when the
possession has walked a cell it occupies."

> **CO-LOCATION IS NOT DISCOVERY.** *"Just being in the area where a thing was
> buried doesn't imply any knowledge of the buried thing any more than going to
> Paris means you've visited the Catacombs or going to southwest Colorado means
> you've visited Mesa Verde."*

So the map answers **two** questions, by two mechanisms, and they must never be
wired to each other:

### A4a. Where have I been? — a property of CELLS

Nathan, on F6: *"consider the cell visited but not the village. As zoom levels
increase, you should be able to pick out what you have and have not visited
accurately."*

Visitedness propagates **upward only**. Walking a room marks that room and
every coarser cell containing it — you were physically inside all of them — and
marks **no sibling**, so zooming in resolves the true, fine-grained set of
ground actually covered rather than blurring it.

**This is exactly the shipped predicate and needs no new code.**
`windows/vessel/src/purview.rs` promotes a cell to `state = "remembered"` when
`w.path[..addr.path.len()] == addr.path[..]` for some walked room `w` — an
ancestor test, upward-only by construction. Its module doc states the overlay
"WRITES NOTHING", so a possession that draws the chart is byte-identical to one
that never does; determinism is untouched.

**This corrects a registry row that said otherwise.**
`CLIENT-remembered-map-asymmetry` claimed `"remembered"` had "no writer ever";
it has had one, with tests, since before that row was written.

### A4b. What do I know is there? — a property of FEATURES

A feature is discovered by **encountering the feature**, never by standing on
ground that contains it. The encounter condition follows from what kind of
thing the feature is, and the two kinds are the ones §A3 already names:

| kind | the feature is… | encountered by |
|---|---|---|
| **extent features** — landmass, sea, river, salt lake, volcano | the ground itself; its extent IS a set of cells | entering any cell of its extent — standing on a volcano *is* meeting it |
| **point sites** — settlement, cave mouth, ruin | a thing standing *at* a cell, not the cell | encountering the thing: entering the settlement, entering the cave |

**The asymmetry is not a special case, it is the rule applied twice.** For an
extent feature the ground and the feature are the same object, so co-location
and encounter coincide. For a point site they are different objects, and
co-location buys nothing.

**Visitedness never implies discovery.** A cell may be visited with every point
site in it still unknown, and that is the normal case at coarse zoom where one
character spans ~9°.

## A5. Colour (The Chroma drift)

The Chroma landed **the day after** this spec was written, so the body above
mentions colour exactly zero times while the client it targets now renders in
truecolor.

**The plate tints**, from terrain-derived substance, honouring `NO_COLOR` and
preserving the monochrome floor, reusing The Chroma's `Ink`/`Ink::from_wire`
gate rather than building a second colour path.

**The provenance question this opens, stated rather than skipped:** The
Chroma's colour is *wire-carried* — it read fields already on
`vessel/session/v2` that had zero readers. A plate rendered in `bin` from
`hornvale-terrain` carries colour that is **not on the wire**. That is the
same shape as `Source::Look` and is admissible on the same terms: a declared
caller discipline, stated in the doc, not a crate-enforced guarantee.

**Colour may not carry the epistemic channel.** The obvious rendering —
discovered in colour, undiscovered in grey — is refused: decision 0142 and The
Illumination assign epistemic to WEIGHT, `CLIENT-bold-means-perishable` builds
on that, and The Chroma assigns colour to substance. One channel, one meaning.
Recorded as `CLIENT-colour-cannot-carry-epistemic`.

**H1 is restated accordingly:**

> **H1' — the map is legible at the floor, in both renderings.** At 80×24 with
> the whole planet in 40 columns, land and water are distinguishable and the
> largest landmass is recognisable against the Gazetteer's committed
> `elevation_ascii` of the same world — **and this must hold under `NO_COLOR`
> as well as in colour**, since monochrome is the floor. *Falsified if* the
> 40-column planet is indistinguishable noise in either rendering. The
> monochrome arm is the one at risk and it is the original H1; the coloured
> arm is new and expected to be easier.

## A6. Focus and the zoom keys (The Stylus / The Stride drift)

The body above says "no new keys, no separate mode" (§4.2), written when the
cursor lived in `Mode { Normal, Look }`. Decision **0159** superseded that:
focus is the client's one input mode. Today `Focus { Walk, Cli, Map }`, `Map`
entered by submitting bare `map`, `Esc` returning `Map → Walk`.

The design lands *better* than specced, and the landing site is already marked
in the source: `input.rs:120-121` binds `-`/`+`/`=` to `Action::Zoom(i8)`, and
`driver.rs:548` accepts-and-ignores it with a doc naming "The Portolan part II"
as where it gets implemented.

**Those keys drive the plate's zoom ladder (§4), not the session's.** The sim
has its own ladder — `map out N` → `purview(zoom_out)` — and it is a different
instrument: `PURVIEW_RADIUS = 4` BFS rings, so that chart is ~9 cells across at
*every* rung. It is a biome-diversity readout, not a map, and it is not a
substitute for this campaign. Letting the snapshot reach it remains its own
rung (`CLIENT-snapshot-chart-cannot-zoom`), explicitly out of scope here.

## A7. Refusals added

- **Nothing is drawn and then hidden** (§A3). Undiscovered point sites are not
  rendered; undiscovered landmarks render as terrain and go unnamed.
- **Co-location is not discovery** (§A4). Visitedness is a fact about cells and
  may never be wired to a feature's label. A campaign that "fixes" a sparse map
  by letting a visited cell disclose its contents has broken this rule.
- **No third copy of the delve roster.** `hornvale_terrain::delve::DelveRung`
  owns the ladder and its derivation; `hornvale_climate::underworld::DelveZone`
  mirrors the roster because the layering forbids climate importing terrain,
  and `cli/tests/suite/delve_roster_mirror.rs` guards the pair (decision 0094:
  the roster is shared, the derivation is not). If this campaign ever names a
  depth, it reuses one of those two. **A third mirror is a STOP.**
- **Colour may not carry the epistemic channel** (§A5).
- **No new site kinds.** This campaign ships the mechanism against the world
  that exists (§A8) and does not widen the discoverable roster.

## A8. What there is to find, measured

Scope rests on this, so it was measured rather than assumed. Three worlds
generated at `--seed 42/7/1337`, spinning:

```
occ-function    agrarian: 625 / 801 / 584      (ZERO variation)
occ-notability  common:   625 / 801 / 584      (ZERO variation)
```

**Every settlement in every world is the same kind of settlement.** No
castles, towers, forts, temples or capitals exist as kinds. Of the roster
Nathan named — "caves, towers, castles, towns" — caves and towns exist;
towers and castles do not.

The discoverable roster, seed 42 (spinning):

```
kind                        count  kind (A4b)  discoverable HERE?
--------------------------  -----  ----------  --------------------------
settlement cells              221  point site  YES -- enter the settlement
natural feature classes         5  extent      YES -- enter any cell of
  volcano/landmass/sea/                         its extent
  saltlake/river
caves                          --  point site  YES -- enter the cave
                                                (derived from the
                                                 stratigraphic column)
distinct ruin cells           170  point site  NO -- no artifact exists
  under a live settlement     127                to encounter (below)
  ABANDONED (no live site)     43
```

**RUINS ARE NOT DISCOVERABLE TODAY, and Nathan's own F7 rule is what removes
them.** An earlier draft of this section promoted the 43 abandoned ruin cells
to first-class discoverables and called them the best content in the world.
They remain the best *material* — each carries a recorded cause (`fled` /
`migrated` / `famine`), a founding date, a people, and a tech level — but under
§A4b a point site is discovered by encountering the thing, and **there is no
thing**. Verified, both halves:

- **The 43 abandoned cells have nothing standing on them.** `Brief.built` is
  `Terrain::is_built`, whose real implementation (`liveness.rs:769`) is
  membership in the injected **settlement-territory** set — live settlements
  only. An abandoned ruin cell is unbuilt, so `structure_at` yields no
  structure and there is nothing to enter.
- **The 127 buried cells have structures, but none can belong to the dead
  occupation.** `Brief`'s four occupation fields are each scoped, in their own
  docs, to "the **alive** occupation"; nothing carries `cause`, `ended_by`, or
  any occupation layer. So Nathan's own example — *"if a goblin village has a
  single tower from an ancient hill dwarf settlement, then enter the tower and
  the ruins are discovered"* — has no tower to enter, because no chamber can be
  attributed to the earlier settlement.

The gap is already named and already licensed: `CLIENT-ruin-signature` (raw,
high confidence) describes what each `cause` should leave behind, and
`brief.rs`'s own doc says "the campaign that first needs `cause` adds one field,
with no save-format consequence and no epoch." **That campaign is not this
one.** Ruins leave the discoverable roster here; the residue is recorded as
`PLAY-ruins-have-no-artifact`.

That the roster is thin is a known and accepted condition, not a finding to
act on here — Nathan, 2026-08-22: *"we have a metric butt-ton of features to
add in the world for it to be worth exploring… let's ship the mechanism
against what exists, and we'll add those features in time."* The bound is
recorded as `PLAY-site-kinds-are-constant`.

## A9. Revised task outline (replaces §12)

**Stage 1 — the chart**
1. Measure the zoom ladder (F1) and derive the central line (F1b). Reuse the
   spike's projection; no second copy. **Test on a `--rotation locked` world,
   not only seed 42** — a spinning-world fixture cannot see H4's defect.
2. The world plate rendered in `bin`, composed by `core`, **in colour with a
   monochrome floor** (§A5); F2, F4, H1'.

**Stage 2 — moving in it**
3. Zoom (the already-routed `-`/`+`/`=`), cursor-driven scroll, and the
   explicit re-centre command (§3.2); F5, H2, H3, H4.
4. The strip carries the chain and scrolls; F3.

**Stage 3 — discovery**
5. Cell visitedness from the shipped fog predicate (§A4a) and feature discovery
   by encounter (§A4b), kept as two mechanisms that are never wired together;
   the two visibility classes (§A3). Ruins are OUT (§A8). F6', F7', F9, H5, H6,
   H6b.

**Stage 4 — close**
6. Chronicle, retrospective, registry rows, decision, freshness sweep. Delete
   all three spike instruments. **Absorb main and regenerate at every stage
   boundary, not only here.** Write the retrospective LAST.

## A10. What is unverified in this amendment

**F6' — RESOLVED by Nathan, 2026-08-23.** The question was malformed: it asked
whether *discovery* should propagate upward, when visitedness and discovery are
different properties (§A4). Visitedness propagates upward and only upward,
which is the shipped predicate unchanged; discovery does not propagate at all.
The "one visited village lights a cell containing dozens of unvisited ones"
worry dissolves — the cell is lit because you were in it, and none of the
villages inside it are named. **Residual, a rendering question rather than a
semantic one:** at the coarsest zoom a lit cell says "you have been somewhere
in here." Task 5 states whether that reads honestly at 40 columns; if it does
not, the fix is in how visitedness DRAWS, never in what it means.

**F7' — RESOLVED by Nathan, 2026-08-23, and it removes ruins from scope.** A
ruin is discovered only by encountering an artifact of it (§A4b), and no such
artifact exists in either the abandoned or the buried case (§A8, both verified
against the code). **Settled — nothing left for a task to decide.**

**F9 — what counts as "entering" a settlement or a cave?** §A4b names the
encounter but deliberately not its threshold. **For a cave the answer is
already owned:** the delve ladder leads with `Surface` and `Undercroft` is "cave
mouths and the first few tens of metres of worked rock", so entering a cave is
the `Surface -> Undercroft` transition `Session::delve` already performs
(`session.rs`, setting `self.underground`). **For a settlement it is open:**
`structure_at`'s existence predicate and the `enter` verb are the two
candidates. **Settled by:** Task 5 names the existing predicate it reuses, for
each kind, and cites it. **Minting a new arrival test is a STOP.**

**F8 — does the strip's chain change under the gate?** §5 has the strip carry
the whole containment chain. **Settled by:** Task 4, which must state what the
strip shows when an *outer* member of the chain is undiscovered and an inner
one is not (standing in a named valley inside an unnamed landmass).

## A10b. H1 is RETIRED as mis-specified (Nathan, 2026-08-23)

**H1 and all three of its framings are withdrawn, not re-framed.** The
hypothesis asked whether the whole-planet plate shows *one coherent largest
landmass*. Seed 42 does not have one, and that was established by instruments
independent of the plate:

```
                                          largest   2nd largest
shipped plate, 104x52, Mercator,
  area-majority                             27.6%       25.0%   (14 components)
independent probe, 300x150,
  equirectangular, plain sampling           27.3%       22.4%   (86 components)
Gazetteer elevation_ascii, 72x24,
  nearest-cell (H1's own reference)         49.9%       13.9%
```

The probe is five times finer than the plate, uses a **different projection**
and a **different sampling strategy**, and agrees with the plate rather than
with the reference. **The reference H1 benchmarked against was itself
under-resolved on the exact property H1 tested**, merging two genuinely
separate landmasses into one.

**The sequence, recorded in full because the sequence is the lesson:**

| framing | method | result |
|---|---|---|
| H1' | nearest-cell, 40x20 | falsified |
| H1'' | area-majority, 40x20 | non-decisive (~10% fewer speckles; the two largest components identical in size under both samplings) |
| H1''' | area-majority, expanded plate 104x52 | falsified — and diagnosed |

Each re-framing followed seeing the previous result, and each was defensible on
its own. Three of them in a row is what retuning-to-pass looks like from the
outside, which is why the campaign set a stop at three and honoured it: the
question went back to the owner rather than being framed a fourth time.

**Retiring is not a fourth framing.** A fourth framing would keep asking whether
the map shows one continent until some configuration said yes. This abandons the
claim as unanswerable-as-posed — the world has no such property to render — and
reports instead a *different* property the same data already measured (§A11's
H8). The distinction is the whole point, and the chronicle must make it.

**Residue, not acted on here:** `elevation_ascii` is a committed artifact that
misrepresents the world's landmass structure at its current resolution, and has
presumably been read that way. Recorded as `MAP-elevation-ascii-merges-landmasses`.

## A10c. H2 — undisposed at G6, disposed at final review (2026-08-23)

**H2 — "the cursor resolves consistently across zoom" — was never measured,
tested, documented, or retired anywhere on this branch, despite appearing
three times in this spec (§8, §12, §A9) and being mapped by the plan's own
self-review table to "Task 3 Step 5" beside H3.** H3 got a real test; H2 got
none. This is the *second* loss from that same table — the retrospective
records the first (§3.3) — and it was found the same way the first one was:
by a reviewer re-auditing the table against the spec rather than trusting the
mapping.

**Disposition: not measured, and here is what is known instead of a
measurement.** The final review computed, rather than guessed, a datum that
bears directly on H2's likely answer: at zoom 0 (the coarsest rung, the whole
planet in the design plate's width), only **10.9%** of terrain cells are ever
an `area_majority` representative — the same undersampling
`MAP-vertical-axis-undersamples-the-mesh` measures at other rungs. Since a
cell that is never a representative at one zoom cannot be "the same cell" the
cursor resolved to at another, **H2 is likely FALSE at the cell level** for
the large majority of cells, purely as a consequence of that undersampling,
independent of whether the resolver's own logic is otherwise consistent.
H2 is more plausible at the *feature* level — a landmass or sea is usually
large enough that pointing "at it" survives a change of representative cell
even when the specific cell does not — but that is also unmeasured.

**Owner: the next campaign that revisits `plate.rs`'s vertical resolution**
(the same one `MAP-vertical-axis-undersamples-the-mesh` and
`MAP-settlement-glyph-may-be-unreachable-at-any-shipped-zoom` are deferred
to). H2 should be measured after that fix, not before it, since the current
number is dominated by an already-known and already-registered defect rather
than by anything specific to cursor resolution.

## A11. Preregistered measurement added

**H8 — the plate is faithful to the world's own structure** (replaces H1, §A10b).
At the design size, the rendered plate's connected-component structure matches a
finer, independently-projected probe of the same terrain. *Measured 2026-08-23:*
the two largest land components are 27.6% / 25.0% of land on the shipped 104x52
Mercator plate against 27.3% / 22.4% on a 300x150 equirectangular probe using
plain sampling — agreement within 2.6 percentage points, across a 5x resolution
change, a different projection and a different sampling strategy. **CONFIRMED.**
*Falsified by* a render whose component structure departs from a finer probe's,
which would mean the plate is inventing or destroying coastline connectivity.

**H5 — the map is useful before it is complete.** From a cold start on seed 42,
the world plate at minimum zoom is navigable — coastlines and the largest
landmass legible — with **zero** features named. *Falsified if* an unlabelled
plate is unreadable, which would mean labels were carrying the legibility that
§A3 assumes terrain carries.

**H6 — discovery is monotonic.** A feature named after an encounter stays named
for the rest of the session, and no feature is named before it is encountered.
*Falsified by* either direction; the second is the leak that matters, because it
is an omniscient read escaping through the renderer.

**H6b — co-location does not disclose (§A4).** Walk a cell containing a point
site *without* meeting the site, at every zoom rung, and the site stays unnamed
and undrawn while the cell reads as visited. *Falsified by* the site appearing,
which would mean visitedness and discovery got wired together — **the single
defect this amendment exists to prevent**, and one a test that only ever walks
straight into things cannot see. The fixture must walk PAST something.

**H7 — the gate costs nothing in the ledger.** A possession that opens the
world map and walks is byte-identical to one that never opens it, exactly as
`purview`'s overlay is. *Falsified by* any ledger difference, which would mean
the map became a writer.

## A11b. Why a thin roster is the right trade — the owner's reasoning, recorded

Approving the reduced roster (2026-08-23), Nathan gave the reason, and it is a
design principle rather than a concession, so it is recorded rather than
summarised:

> "We're going to need to invest a lot of time in building things to hang from
> this skeleton, but I think that's better than building a lot of things that
> we then need to design a skeleton for."

The discovery layer is the skeleton. It is specified against three feature
kinds because three is enough to prove the joints — an extent feature, a point
site, and a point site reached by a band transition (§A4b) — and every later
feature is hung on the same rule rather than renegotiating it. The failure mode
this avoids is the opposite order: a large accumulated roster whose structure
must be retrofitted, where the retrofit is constrained by content that was
authored without it.

**This is also why `PLAY-ruins-have-no-artifact` is a good outcome and not a
loss.** It is the skeleton refusing to hold a bone that does not exist yet, out
loud, instead of quietly disclosing ruins by co-location because the roster
looked thin.

## A12. Flagged for Nathan at G3

- **The fidelity ruling is his and is recorded as such** (§A1). It reverses a
  proposal this session made, and the reversal is the reason the body of the
  spec survives.
- **The roster SHRANK after his F7 ruling, and the size of it is worth his eye**
  (§A8): ruins are out, because the rule is right and the artifact does not
  exist. What remains is settlements (221, one kind), five natural feature
  classes, and caves. Restoring ruins means `CLIENT-ruin-signature` plus one
  `Brief` field — licensed by `brief.rs`'s own doc, no epoch — and that is a
  campaign, not a task.
- **The discoverable roster is thin and he has accepted that** (§A8) —
  `PLAY-site-kinds-are-constant` is the row that unblocks the rest, and it is
  deliberately not this campaign.
- **No save-format, epoch, or determinism-contract decision is taken by this
  amendment.** The gate is a pure read (§A4) and H7 preregisters that claim.
- **One schema-adjacent NON-change:** the reassessment proposed an additive
  `vessel/session/v2` channel to carry a remembered world. Nathan's ruling
  removed the need for it, so §2's "no schema change" stands and no cross-repo
  contract moves.
