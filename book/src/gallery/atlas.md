# The Atlas of Seed 42

Every pixel below is read, in your browser, from the same committed scene
document the simulation emitted — [`scene-tiles-seed-42.json`](./scene-tiles-seed-42.json),
a `scene/tiles/v1` description of seed 42's surface (see
[the schema reference](../reference/scene-tiles-v1.md)). The simulation
publishes what each tile *is*; everything you see — the colors, the
markers, the zoom — is this page's own choice of presentation
(decision 0022: the simulation emits data; clients render).

Switch layers, hover to inspect any tile, drag to pan, scroll to zoom.
Settlements are ringed dots; the flagship village is gold.

<div id="atlas-holder">
  <div id="atlas-layers" style="margin-bottom: 0.5em;"></div>
  <canvas id="atlas-canvas" width="1024" height="512"
    style="width: 100%; height: auto; border: 1px solid var(--theme-popup-border, #888); cursor: crosshair;"></canvas>
  <p id="atlas-readout" style="font-family: monospace; min-height: 1.5em;">loading…</p>
</div>

<script type="module" src="./atlas.js"></script>
