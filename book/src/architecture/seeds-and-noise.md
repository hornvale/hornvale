# Seeds & Noise

## Hierarchical seeding

All randomness in Hornvale flows from a single world seed, but never
directly. Seeds *derive* children by label: the world seed derives an
`"astronomy"` seed, which derives a `"moon-count"` seed, and so on, forming a
tree of named streams. Derivation is a hash — deterministic, cheap, and
stable forever (the constants are a save-format contract; changing them would
orphan every saved world).

The property this buys is subtle and load-bearing: **adding a new consumer
never perturbs existing streams.** If Campaign 7 adds a `"tides"` stream to
astronomy, every already-generated moon stays exactly where it was, because
moons draw from `astronomy/moon-count`, not from a shared sequence whose
allocation shifted. Systems that instead pull from one global random sequence
suffer butterfly effects on every code change; label-derived streams are
immune. This is what makes a multi-year project's worlds *stable under
development*.

A derived seed can open a **stream** — an iterator of random values (integers,
unit-interval reals, ranged picks) used for choices: how many moons, which
name, what population.

## Coherent noise

For continuous properties over space — elevation, temperature, eventually
"political pressure" — streams are the wrong tool; we need functions over
coordinates. The kernel provides *value noise* and its fractal sum (fBm),
chosen for three properties, in priority order:

1. **Determinism** — same seed and coordinates, same value, every platform.
2. **Random access** — the value at any point is computable in constant time
   without evaluating neighbors. This is the miracle that makes unbounded
   worlds possible: nothing is generated until someone looks, and looking
   anywhere costs the same.
3. **Local coherence** — nearby points correlate; zooming in reveals
   consistent finer detail (octaves), so the world looks intentional at
   every scale.

One implementation detail promoted to a principle: each octave of the fractal
sum draws from its own *derived* seed, so octaves are independent named
streams like everything else. See the Gallery's
[First Light](../gallery/first-light.md) for what seed 42's noise actually
looks like.
