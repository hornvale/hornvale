# A Possession, Live

The [transcript on the previous page](./possession-seed-42.md) was recorded
by the native binary. The pane below is not a recording. When you press
*possess*, your browser derives the world — sky, tectonics, climate,
settlements, the goblin you inhabit — from nothing but the seed in the box,
by running the same crates the native simulation runs. The prose is
byte-identical to the native binary's: the portable-transcendentals
decision (0041) holds on wasm unchanged, and CI walks this module against
the committed transcript to prove it.

Nothing is consulted but the seed. The module *cannot* consult anything
else — its WebAssembly imports object is empty: no network, no clock, no
DOM. Seven exports, memory in, prose out — and beside the prose, since [The
Snapshot](../chronicle/the-snapshot.md), the same turn's structured emit, which
is what the transcript you read below is now rendered from.

Since [The Panes](../chronicle/the-panes.md) there are **two** panes, and the
second is the reason the first one's emit was worth building. A map stands
beside the prose and redraws itself every turn — the chart of the country when
you are out of doors, the floor plan of the building when you are inside it,
switching on its own as you `enter` and step `out`. Both panes are pure
functions of one snapshot: the sim sends *cells*, never a picture, so nothing
in the browser can show you something the sim did not say, and nothing in the
sim knows how anything is drawn. There is no second source for the two panes
to disagree about. The map is also not a capability the prose lacks — every
pane can do only what a verb can do, so what you see beside the transcript is
what `map` would have printed had you typed it.

Since [The Beholding](../chronicle/the-beholding.md) the chart is **coloured,
through the eyes of whoever you are possessing**. Colour is not a property of
a cell: it is the product of the surface's reflectance, the light actually
falling on it at your latitude and hour, and the sensitivity curves the
possessed species' perception vector implies — so the pane carries a caption
naming the eye, its channel count, and what its projection to a
three-channel screen *preserves*. A dichromat's map is captioned as one,
because a two-chromatic-channel signal on an RGB screen is a false-colour
mapping and the honest place for that admission is the caption rather than
the picture. Type `eyes kobold`, `eyes human`, `eyes own` or `eyes off` to
change the eye without changing anything else. Since
[The Illumination](../chronicle/the-illumination.md) the tint is the ground's
**surface cover** — vegetation, litter, snow, sand or silt over the mineral
blend, varying room to room — rather than the bedrock underneath it, which is
not what anyone standing there would see. It is still withheld wherever the
glyph is drawing water, a mark, or you, because a river painted meadow-green
is a false claim and the honest response to one is to withhold it. The chart
is drawn **north-up** now, with each cell placed by its own bearing and
distance from you, so the cells across a face seam of the underlying
icosahedron draw rather than being disclosed as unplaceable.

Since [The Lantern](../chronicle/the-lantern.md) the **floor plan** is
coloured too, and by a different light. Indoors there is no sun: a cell's
colour is the fabric its walls are built from — stone, timber, cob or thatch,
derived from the bedrock, the biome and the depth of soil under the
building — lit by whatever sources actually reach it. Those are a hearth at
its wall, an open doorway onto the daylight outside, and the 1900 K flame the
possession is taken to be carrying, which is why a room reads amber rather
than white. Light travels by the same symmetric field-of-view calculation that
decides what you can see, so a wall blocks light exactly as it blocks sight,
and a cell nothing reaches carries no colour at all rather than a black one.
A threshold is uncoloured on purpose: an opening is not a material.

## The Demo

<div id="casement"></div>
<noscript><p>The casement needs JavaScript — the world derives in your
browser, and something has to run it.</p></noscript>

<p class="casement-docent">Genesis takes a few seconds — everything above
derives from the seed, every time. The verbs: <code>look</code>,
<code>map</code> (and <code>map out N</code> for a coarser rung — the chart
of the locales around you, with ground you have walked but cannot currently see
drawn from memory; indoors the same verb draws the floor plan of the building
you are standing in, since a plan has no coarser rung — The Blocking),
<code>go n|ne|e|se|s|sw|w|nw</code>,
<code>dive</code> and <code>surface</code> (descend and rise a layer of the
water column, afloat — The Column),
<code>delve</code> and <code>climb</code> (descend into the cave beneath the
cell you stand on, if the rock admits one, and come back up — The Deep Realm),
<code>enter</code> (step inside what is built where you stand, then
<code>enter further in</code> to go deeper — a chamber is the same address
space nine refinements down, ~3.3 m rather than the walk band's ~1.7 km — The
Lintel) and <code>out</code>,
<code>examine</code> (anything either the prose or the chart named; indoors,
anything the chamber's prose or the floor plan's legend named),
<code>back</code>,
<code>wait</code> (the world moves too — a derived NPC keeps its own daily
route, departing and returning), <code>knows</code>,
<code>ask</code> (ask the body you are wearing how it feels; it answers in its
own tongue, and what it says is <em>not</em> what its arbitration computed — it
cannot perceive what that arbitration suppressed, and its culture may have no
word for the state it is in, in which case it reaches for the nearest word it
does have and says so — The Confidant. Whether it answers at all is also not
guaranteed: what its people believes you are and what you have made this body
ignore can leave it refusing to say, claiming to feel fine when it does not, or
answering truthfully and naming what it cost the host to say so — The
Reticence),
<code>sleep</code> (lie down and let go of the day; the body stops obeying
until its own cycle wakes it — The Deed),
and <code>release</code>, which ends the possession rather than doing anything
in the world.
All but the last are <em>in-character</em> verbs: the body does them, so the
body's own state can refuse them, the ones that move it cost it time, and the
world remembers the ones that change it.
A leading <code>!</code> selects the <em>out-of-character</em> namespace, which
bypasses the body but never the world — the operator instruments
<code>!whoami</code>, <code>!npcs</code>, <code>!why</code>, <code>!help</code>
and <code>!eyes</code> (bare, it reports whose eyes you are seeing colour
through and what their projection drops; <code>!eyes own|human|off</code> or
any species name switches them — The Beholding), and an objective half of each
dual verb — <code>!map</code>, <code>!look</code> and the rest — which still
answers while the body cannot act. None of the instruments has a bare spelling;
<code>!help</code> lists the lot. This terminal
is
<a href="https://github.com/hornvale/hornvale/tree/main/clients/vessel">clients/vessel</a>;
the world it derives is the same
<a href="https://github.com/hornvale/hornvale/tree/main/windows/worldgen">worldgen</a>
the CLI runs.</p>

<style>
  #casement {
    border-left: 3px solid #b8860b;
    padding: 0.25em 1em;
    margin: 1em 0;
  }
  .casement-transcript {
    max-height: 24em;
    overflow-y: auto;
    margin-bottom: 0.75em;
  }
  .casement-transcript p { margin: 0.4em 0; }
  .casement-meta, .casement-echo, .casement-status, .casement-docent {
    font-family: var(--mono-font, "Source Code Pro", monospace);
    font-size: 0.8em;
    opacity: 0.75;
  }
  .casement-map {
    font-family: var(--mono-font, "Source Code Pro", monospace);
    font-size: 0.8em;
    opacity: 0.75;
    white-space: pre;
  }
  .casement-echo { opacity: 0.6; }
  /* The map pane's EPISTEMIC channel: a remembered cell draws the same
     glyph as a sensed one, fainter (spec §2 — weight is a modulator, not a
     second alphabet). Expressed here rather than inline so the page decides
     how faint faint is, as it already does for .casement-echo. */
  .casement-dim { opacity: 0.55; }
  .casement-error { color: #b3554d; }
  .casement-controls, .casement-promptrow { margin: 0.5em 0; }
  .casement-seed, .casement-input {
    font-family: var(--mono-font, "Source Code Pro", monospace);
    background: var(--bg);
    color: var(--fg);
    border: 1px solid var(--theme-popup-border, #888);
    border-radius: 3px;
    padding: 0.15em 0.5em;
  }
  .casement-seed { width: 12em; }
  .casement-input { width: min(28em, 90%); }
  .casement-possess {
    font-family: var(--mono-font, "Source Code Pro", monospace);
    background: var(--theme-hover, #e8e4dc);
    color: var(--fg);
    border: 1px solid var(--theme-popup-border, #888);
    border-radius: 3px;
    padding: 0.15em 0.75em;
    cursor: pointer;
    margin-left: 0.5em;
  }
  .casement-possess:disabled, .casement-input:disabled { opacity: 0.5; }
  .casement-promptmark { font-family: var(--mono-font, monospace); opacity: 0.75; }
  .casement-status { min-height: 1.5em; }
  .casement-panes { display: flex; gap: 1rem; align-items: flex-start; }
  .casement-transcript { flex: 1 1 60%; min-width: 0; }
  .casement-mapview {
    flex: 0 0 auto; margin: 0; font-family: monospace;
    line-height: 1.05; white-space: pre; overflow-x: auto;
  }
  @media (max-width: 640px) { .casement-panes { flex-direction: column; } }
</style>
<script type="module" src="./vessel.js"></script>

*(Reading this locally and the pane says the casement is dark? The wasm is
deploy-built, never committed — run `make wasm-vessel`, then serve the book
again.)*
