# The Lot, Live

A *lot* is one life drawn from a world's own demography: a birth year taken
off the births curve, a place taken off the population map for that year, a
life course run against the mortality the community actually had, and then
two dozen questions asked of the committed ledger about the person who lived
it. The rule the whole thing runs on is that **a tile is absent, never
invented**. Where the record has no answer, the page says so and says why —
in the sim's own words, not a dash and not a plausible substitute. Four of
the silences are declared: no world here models sex, family, work or
letters, and those four are folded into a single note so they read as one
statement about the world rather than four failures of its record.

The sim draws; the page shows (decision 0022). Every number below is a field
of a payload the WebAssembly module produced — `lot/life/v1`, `lot/curve/v1`
and `lot/places/v1` — and the browser computes no demographic quantity of
its own. What the browser *does* own is the theatre: the log/linear switch
on the births graph, the year that spins before it settles, the crosshair
hopping to the site, the timeline assembling itself. The permalink is the
key, so two readers of one link see one life; a life is drawn by the module
from `(seed, index)` and the pins, never re-rolled here. The byte-stable
text of lots 0–9, as the native binary prints them, is on
[Ten Lives of Seed 42](./generated/the-lot-seed-42.md).

Reading this locally and the page is dark? The exhibit's own wasm is
deploy-built and never committed (decision 0052), and since decision 0125
retired CI the book has no deploy to build it — run `make wasm-lot`, then
serve the book again. Genesis then takes a few seconds in your browser,
every time, because everything you see derives from the seed.

<div id="lot"></div>
<noscript><p>The exhibit needs JavaScript — the world derives in your
browser, and something has to run it.</p></noscript>

<style>
  #lot {
    border-left: 3px solid #b8860b;
    padding: 0.25em 1em;
    margin: 1em 0;
  }
  #lot h2 {
    font-size: 1.1em;
    margin: 1.2em 0 0.4em;
    text-transform: uppercase;
    letter-spacing: 0.08em;
    opacity: 0.7;
  }
  .lot-status, .lot-hint, .lot-caption, .lot-bydesign, .lot-silences, .lot-tile-refs {
    font-family: var(--mono-font, "Source Code Pro", monospace);
    font-size: 0.8em;
    opacity: 0.75;
  }
  .lot-status { min-height: 1.5em; }
  .lot-heroline { font-size: 1.25em; margin: 0.4em 0; }
  .lot-bigyear {
    font-family: var(--mono-font, "Source Code Pro", monospace);
    font-size: 2.4em;
    font-variant-numeric: tabular-nums;
    margin: 0.2em 0 0;
  }
  .lot-button {
    font-family: var(--mono-font, "Source Code Pro", monospace);
    background: var(--theme-hover, #e8e4dc);
    color: var(--fg);
    border: 1px solid var(--theme-popup-border, #888);
    border-radius: 3px;
    padding: 0.15em 0.75em;
    margin-right: 0.5em;
    cursor: pointer;
  }
  .lot-button:disabled { opacity: 0.5; cursor: default; }
  .lot-axis-on { background: #b8860b; color: #fff; }
  .lot-actions, .lot-axisrow { margin: 0.6em 0; }
  .lot-plot, .lot-track { width: 100%; height: auto; display: block; }
  .lot-plot-area { fill: rgba(184, 134, 11, 0.25); }
  .lot-plot-line { stroke: #b8860b; stroke-width: 1.5; }
  .lot-plot-mark { stroke: #b3554d; stroke-width: 1.5; stroke-dasharray: 3 3; }
  .lot-plot-tick { stroke: currentColor; opacity: 0.4; }
  .lot-plot-label {
    fill: currentColor;
    opacity: 0.6;
    font-size: 11px;
    font-family: var(--mono-font, monospace);
  }
  .lot-picking { cursor: crosshair; outline: 2px dashed #b3554d; }
  .lot-map { width: 100%; height: auto; display: block; background: rgba(120, 140, 160, 0.08); }
  .lot-track-axis { stroke: currentColor; opacity: 0.35; }
  .lot-track-mark circle { fill: #b8860b; }
  .lot-track-ending circle { fill: #b3554d; }
  .lot-track-mark text {
    fill: currentColor;
    font-size: 12px;
    font-family: var(--mono-font, monospace);
  }
  .lot-untimed {
    font-size: 0.85em;
    opacity: 0.7;
    margin: 0.2em 0 0.6em;
  }
  .lot-tiles {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(15em, 1fr));
    gap: 0.5em;
    margin: 0.6em 0;
  }
  .lot-tile {
    border: 1px solid var(--theme-popup-border, #888);
    border-radius: 3px;
    padding: 0.4em 0.6em;
  }
  .lot-tile-label {
    display: block;
    font-size: 0.72em;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    opacity: 0.6;
  }
  .lot-tile-value { display: block; }
  /* An ABSENT tile — the record had no answer. Faint and italic rather
     than empty: the silence and its reason are the content. */
  .lot-absent { border-style: dashed; opacity: 0.65; }
  .lot-absent .lot-tile-value { font-style: italic; }
  .lot-disclaimer { font-style: italic; margin: 0.4em 0 0.8em; }
  .lot-sources { font-size: 0.85em; margin: 0.8em 0; }
  @media (max-width: 640px) { .lot-tiles { grid-template-columns: 1fr; } }
</style>
<script type="module" src="./lot.js"></script>
