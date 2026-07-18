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
DOM. Five exports, memory in, prose out.

## The Demo

<div id="casement"></div>
<noscript><p>The casement needs JavaScript — the world derives in your
browser, and something has to run it.</p></noscript>

<p class="casement-docent">Genesis takes a few seconds — everything above
derives from the seed, every time. The verbs: <code>look</code>,
<code>go n|ne|e|se|s|sw|w|nw</code>, <code>examine</code>, <code>back</code>,
<code>wait</code> (the world moves too — a derived NPC keeps its own daily
route, departing and returning), <code>npcs</code>, <code>why</code>,
<code>whoami</code>, <code>knows</code>, <code>release</code>. This terminal
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
  .casement-echo { opacity: 0.6; }
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
</style>
<script type="module" src="./vessel.js"></script>

*(Reading this locally and the pane says the casement is dark? The wasm is
deploy-built, never committed — run `make wasm-vessel`, then serve the book
again.)*
