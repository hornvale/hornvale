# The Orrery of Seed 42

A year in the life of seed 42's system: the world circling its star, the moons
turning through their synodic phases. Generated deterministically — this seed
always yields this animation.

### Single-width Unicode

<div id="orrery"></div>

### Emoji (experimental — needs an emoji-capable, two-column font)

The same schematic on a two-column-per-cell grid, its moons the phase emoji
`🌑🌓🌕🌗`. Emoji are unconditionally double-width, so the grid is built for it;
whether it aligns depends on the player's font.

<div id="orrery-emoji"></div>
<script>
  document.addEventListener('DOMContentLoaded', function () {
    AsciinemaPlayer.create('./orrery-seed-42.cast', document.getElementById('orrery'), { loop: true, autoPlay: true, cols: 61, rows: 31 });
    AsciinemaPlayer.create('./orrery-emoji-seed-42.cast', document.getElementById('orrery-emoji'), { loop: true, autoPlay: true, cols: 122, rows: 31 });
  });
</script>

[Download the Unicode .cast](./orrery-seed-42.cast) · [Download the emoji .cast](./orrery-emoji-seed-42.cast)

---

*Generated deterministically: this seed always yields this animation.*
