<!-- GENERATED FILE — do not edit. Regenerate with `hornvale streams`. -->

Labels are permanent save-format contracts; regeneration uses epoch suffixes (e.g. `settlement/name/v2`), never renames.

### hornvale-astronomy

*(no seed-derivation streams)*

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
| `settlement/name` | candidate village names |
| `settlement/name-pick` | which candidate survives refinement |
| `settlement/population` | village population draw |

### hornvale-terrain

*(no seed-derivation streams)*

### hornvale-kernel (internal)

| Label | Meaning |
|---|---|
| `octave-{n}` | per-octave noise streams derived inside fbm (n ≥ 1) |
