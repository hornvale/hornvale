# The Portolan, part II — the world map

Part I gave the terminal client a cursor and a strip that named whatever it
pointed at — and almost nothing to point at, because the only thing on
screen was the walk band's local terrain plate, ~28 times smaller than the
gap between one named feature and the next. This campaign builds the thing
part I's mechanism was waiting for: a Mercator chart of the whole planet,
scrolled and zoomed, with that same cursor and that same strip now pointing
into a world instead of a room's worth of it.

The spine of what shipped is one sentence, and it takes a caveat to stay
honest: **the map's geometry is complete and the knowing is not.** What is
complete is the geometry itself and its truthfulness — nothing about the
terrain is fogged, distorted, invented, or withheld by knowledge; a locked
world's substellar desert draws exactly like a spinning world's tropics,
whether or not the possession has ever set foot there. What is *sampled* is
which of those cells actually earns a character on screen. At the 80×24
floor the plate is 40×20 — 800 characters standing in for a planet of
40,962 terrain cells — and measured on seed 42, only about one cell in
ninety (1.1%) is ever the representative a screen character draws. So a
discovered point site is very likely **not drawn at all**, whatever its
label would say: the worked case is the flagship's own starting
settlement, undrawable at every zoom rung this client ships, appearing
only past a resolution more than three times the client's own ceiling. The
campaign's own ratified rule (decision 0196's second clause) is that a map
"may disclose its own resolution but may never invent detail below it,"
and this campaign built exactly that disclosure for the strip — F5's "one
character stands for roughly N terrain cells at this zoom" — and never
built the matching disclosure for the plate's own content. It honoured the
second half of its own clause and not the first. What a feature is
*called*, once it does draw, is a separate question again, answered only
once the possession has actually met it. And the frame the map holds still
by is not an arbitrary grid — it is read off the world's own physics.

## The equator is a fact about the world, not the graticule

A fixed geographic Mercator is the wrong instrument for this world model,
and not hypothetically: a world can be pinned `--rotation locked`, and a
locked world's habitable ring is not around its equator at all — it is the
**terminator**, the great circle running pole to pole through the twilight
band between the scorched substellar face and the frozen antistellar one. A
geographic projection's polar clamp would discard half of the only place
anyone can live.

So the projection's central line is derived from the world's committed
rotation regime rather than fixed: the geographic equator on a spinning
world, the terminator on a locked one. The two cases turn out to be the same
rotation, seen from a different pole — a locked world's terminator has the
substellar point as its own pole, ninety degrees from every point on it, so
holding the terminator level is an **axis swap**, not an arbitrary rotation.
No `sin`/`cos` needed to derive it, one fewer cross-platform surface to keep
identical.

Deriving that rotation surfaced a real, pre-existing bug in the spike this
campaign ported from: its `project` function measured longitude on `[0°,
360°)` while its `unproject` measured `[-180°, 180°)` — a systematic ~180°
error that a labelling spike, which never round-trips a coordinate back
through itself, could not see. Porting the pair together, with a property
test asserting `unproject(project(x)) ≈ x`, is what caught it. A *second*
implementation of the locked-world rotation — plausible, and wrong — passed
every test written against it before being discarded; what actually
established the shipped rotation as correct was an independent probe run
after the fact, sampling round-trip error over a dense grid at six poles
(max error ~8×10⁻¹³°, great-circle distance preserved to ~10⁻⁹). Two
seam-level tests could tell a working rotation from a broken one; neither
could tell a working one from a merely plausible one.

## A screen given more room drew a worse map

The plate was built to a fixed 40 columns, matching the 80×24 floor this
project has always designed against. Height did the opposite: it grew to
fill whatever the terminal offered. Put those two together on a large
terminal and the result inverts the point of a bigger screen — a 210×56
window handed the plate 40 columns by 52 rows, stretching the one shape a
Mercator chart cannot use (a planet is roughly twice as wide as it is tall)
into the one shape it is worst at. The fix makes the plate's width, like its
height, a value computed once per redraw from the terminal's own size while
the world view is active — the same discipline the height axis already
followed, applied to the axis that had been silently exempted from it.

## Entering the map, and holding it still

**Map focus has to be entered first** — submitting the bare command `map`
(The Stride's own routing, unchanged by this campaign) — because
`Action::Zoom` is only ever routed while `Focus::Map` is current; "press
`-`" is not by itself a complete answer to how a player reaches the world
map. Two gestures reach the map from there, and neither is a new
keybinding — both reuse keys the client already routed for a different
purpose, on the reasoning that a letter key would break `Focus::Map`'s
deliberately total routing table (every unclaimed letter already types
itself into the command line), so the only room left for a new verb was in
the punctuation this campaign had already claimed for zoom.

**Zooming out past the walk band's own finest rung enters the world view,
at its coarsest rung; zooming in past the world view's own finest rung
leaves it, back to the walk-band chart.** The `-`/`+`/`=` keys were already
bound to zoom before this campaign — part I reserved them and did nothing
with them — so the world map's entry and exit are simply what happens when
that one ladder is walked past either of its old ends, rather than a
separate mode switch. A typed `world` command was considered and rejected:
the client's entry pane has no reply channel of its own, since its prose is
carried on the wire from the sim, and a client-only command would have
nowhere to answer from.

**Re-centring — rolling the projection so the cursor's own position becomes
the new central line — is bound to `.`, a punctuation key alongside those
same three.** The same reasoning applies twice over: `.` joins an already-
established pattern (punctuation as map verb) instead of starting a new one,
and a typed `recentre` command would have had the identical no-reply-channel
problem. The gesture costs no reply either way — the map simply redraws
around the new centre, which is the whole acknowledgement a keypress needs.

Both bindings are gestures chosen on Nathan's behalf rather than specified
by him directly, and both are cheap to move if they read wrong in play: one
line in the input router, one in the test that pins it.

## H1 asked a question this world does not have an answer to

The original hypothesis was simple: does the whole-planet plate show one
coherent largest landmass? Seed 42 answered three times, each answer forcing
a different framing of the same question — and the sequence is worth
recording in full, because only the full sequence shows why the fourth
framing that suggests itself was refused instead of tried.

| framing | method | result |
|---|---|---|
| H1′ | nearest-cell sampling, 40×20 | falsified |
| H1″ | area-majority sampling, 40×20 | non-decisive — ~10% fewer stray cells, but the two largest landmasses are identically sized (34 and 29 cells) under either sampling |
| H1‴ | area-majority sampling, the expanded 104×52 plate | falsified, and diagnosed |

Each re-framing followed directly from seeing the one before it — first the
sampling method changed, then the plate's own size — and each was
defensible taken alone. Three defensible re-framings in a row is exactly
what retuning a result to pass looks like from the outside, so the campaign
had set a stop at three in advance and honoured it: at H1‴, the question
went back to the owner instead of being reframed a fourth time.

H1‴'s diagnosis is what made the retirement possible rather than just
convenient. Two instruments — the shipped 104×52 Mercator plate itself, and
a five-times finer 300×150 equirectangular probe using plain sampling,
sharing no machinery with it — agreed with each other (largest landmasses
27.6%/25.0% and 27.3%/22.4% of land, respectively) and disagreed with the
one thing H1 had been benchmarked against the whole time: the Gazetteer's
committed `elevation_ascii` rendering, at 72×24 nearest-cell sampling, which
shows 49.9%/13.9% — a reference coarse enough to have quietly merged two
real, separate landmasses into one. **The instrument H1 was measured against
was itself under-resolved on the exact property H1 was testing.**

Nathan retired H1 as mis-specified rather than reframing it a fourth time,
and the distinction is the whole point: a fourth framing would have kept
asking whether the map shows one continent until some configuration
answered yes. Retiring instead abandons the claim as **unanswerable as
posed** — this world simply has no single dominant landmass to show — and
reports a different property the same data already measured: **H8, that the
plate's connected-component structure is faithful to a finer, independently
projected probe of the same terrain**, confirmed within 2.6 percentage
points across a five-times resolution change, a different projection, and a
different sampling strategy. The map was never blurry about where the
coastlines are; it was only ever being asked the wrong question about what
lay behind them.

`elevation_ascii` itself is left exactly as it was — a committed artifact
that has now misled two campaigns about this world's landmass structure,
recorded rather than fixed here.

## Co-location is not discovery

*"Just being in the area where a thing was buried doesn't imply any
knowledge of the buried thing any more than going to Paris means you've
visited the Catacombs or going to southwest Colorado means you've visited
Mesa Verde."* — Nathan, ruling on the discovery layer.

The map answers two different questions, and this campaign's central
discipline is keeping them answered by two mechanisms that are never wired
to each other. **Where have I been** is a property of *cells*: walking a
room marks that room and every coarser cell containing it, upward only, so
zooming in always resolves the true, fine-grained shape of what was actually
covered. The walk band's own chart has drawn exactly this, unchanged, since
before this campaign — `windows/vessel`'s fog predicate. The world map's
own bookkeeping is new code that reuses that predicate's *shape* rather
than a second invention of visitedness, and it is currently write-only:
nothing yet renders a cell differently for having been visited, an open
item for a future campaign rather than this one's to draw. **What do I know
is there** is a property of *features*, and it is earned only by encounter, not
by proximity: a terrain-borne landmark (a volcano's cone, a river's course,
a coastline) is visible on the map from the first turn because it *is* the
ground, but it draws unnamed until the possession has entered its extent; a
point site (a settlement, a cave mouth) is not drawn at all until it has
been entered, because there is nothing rendered to hide — it simply is not
there yet. Nothing the map ever shows is drawn and then taken away.

The asymmetry between the two feature kinds is not a special case grafted
on; it is the same rule applied twice. For a landmark the ground and the
feature are the same object, so standing on a volcano *is* meeting it. For a
point site they are different objects, and standing near the settlement
buys nothing — you have to walk into it.

One casualty of that rule, decided rather than discovered: **ruins are not
part of the discoverable roster this campaign ships.** A ruin is committed
history — a cause of death, a founding date, a people, a tech level — but
under "discovered by encountering the thing," there has to be a *thing*, and
today there is not one: an abandoned ruin cell has nothing built on it to
enter, and a buried ruin's chamber belongs entirely to whatever settlement
lives there now, with no field anywhere recording that an earlier occupation
ever stood in the same place. A committed field already carries each ruin's
own cause of death — fled, migrated, burned, plague, famine — and the
project's own notes on it describe exactly what closing this gap would take
and at no cost to a saved world; closing it is a future campaign's work, not
a quiet exception carved into this one's discovery rule.

## The strip discloses what it does not know

At the coarsest zoom, one character on the plate stands for dozens of real
terrain cells, and the cursor's status strip has to answer for exactly one
of them. Rather than pick silently, it says so: at any zoom coarser than one
character per cell, the strip appends its own resolution — *"one character
stands for roughly N terrain cells at this zoom"* — the same discipline
decision 0123 already applies to a chart drawn finer than its underlying
field. The resolved cell is guaranteed to agree with what is actually drawn
there (the strip can never name water while the map shows land under the
cursor), but agreement with the picture is not the same claim as certainty
about the ground, and the strip no longer lets a reader mistake one for the
other.

The same caption also carries the projection's own disclosure from §3.1:
which line the map is holding level — the equator or the terminator — and
which latitude band is clamped off the top and bottom of the chart, since on
a locked world the missing pair of places is not the pair a reader used to a
spinning world would assume.

## What this does not build

Glossed landform names, mountain ranges (and the bays, capes and straits
that come with the clustering pass a range needs), and a dead-reckoned,
partial map for NPCs to carry their own errors on remain out of scope, each
its own campaign — the player's map stays geometrically complete and
predictable, by Nathan's explicit ruling, precisely so that the eventual NPC
map can be allowed to lie without the player's ever doing so.
