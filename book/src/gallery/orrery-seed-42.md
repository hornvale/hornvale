# The Orrery of Seed 42

The seed-42 system, computed live in your browser from the data the simulation
emitted — [`scene-system-seed-42.json`](./scene-system-seed-42.json) (the orbital
elements) and [`scene-tiles-seed-42.json`](./scene-tiles-seed-42.json) (the real
terrain). The star is drawn from its class, the moons show their true phase, and
the world is its own globe — its actual continents, spinning at its day rate,
tilted at its obliquity, with the day/night line sweeping across.

The orrery, like the atlas, renders committed scene data; [A Possession, Live](./possession-live.md)
goes further and runs the simulation itself in the page.

<div id="orrery-holder">
  <canvas id="orrery-canvas" width="640" height="640" style="max-width:100%"></canvas>
  <div id="orrery-controls" style="font-family:monospace;margin-top:.5em"></div>
</div>

<script type="module" src="./orrery.js"></script>

---

*Generated deterministically: this seed always yields this system.*
