<!-- GENERATED FILE — do not edit. Regenerate with `hornvale streams`. -->

Labels are permanent save-format contracts; regeneration uses epoch suffixes (e.g. `settlement/name/v2`), never renames.

### hornvale-astronomy

| Label | Meaning |
|---|---|
| `astronomy` | root stream for sky genesis |
| `astronomy/star-mass` | main-sequence star mass draw |
| `astronomy/anchor-mass` | anchor world mass draw |
| `astronomy/rotation` | rotation regime and period draw |
| `astronomy/orbit` | anchor orbital distance draw |
| `astronomy/obliquity` | axial tilt draw |
| `astronomy/moon-count` | how many moons |
| `astronomy/moons` | per-moon mass/distance draws (sequential attempts) |
| `astronomy/neighbors` | notable-neighbor class/distance draws |

### hornvale-climate

*(no seed-derivation streams)*

### hornvale-culture

*(no seed-derivation streams)*

### hornvale-religion

| Label | Meaning |
|---|---|
| `religion` | root stream for religion generation |
| `religion/epithet` | deity epithet pick |

### hornvale-settlement

| Label | Meaning |
|---|---|
| `settlement` | root stream for settlement generation |
| `settlement/name` | per-settlement generated name |
| `settlement/placement` | per-settlement population against carrying capacity |

### hornvale-terrain

| Label | Meaning |
|---|---|
| `terrain` | root stream for tectonic genesis |
| `terrain/plate-count` | how many plates |
| `terrain/plate-seeds` | per-plate seed positions on the sphere |
| `terrain/plate-kind` | continental fraction and per-plate continental rolls |
| `terrain/plate-motion` | per-plate Euler pole axis and rate draws |
| `terrain/maturity` | per-plate orogenic maturity draws |
| `terrain/hotspots` | hotspot count, positions, and strengths |
| `terrain/ocean-fraction` | target ocean fraction draw |

### hornvale-kernel (internal)

| Label | Meaning |
|---|---|
| `octave-{n}` | per-octave noise streams derived inside fbm (n ≥ 1) |
