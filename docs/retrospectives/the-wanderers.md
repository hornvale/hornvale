# The Wanderers — retrospective

Process lessons, not product. One page.

## What worked

- **Topology before decoration prevented a retrofit.** The binary question
  was asked before ephemerides and scene output existed. Keeping the stellar
  root explicit meant close binaries could use a barycentric frame while
  wide binaries used a circumprimary frame, without teaching every later
  consumer that a former single-star field had secretly changed meaning.
- **Append-only streams preserved the old world.** Topology, binary
  parameters, and wanderer phases were appended rather than inserted. The
  pin-isolation and seed-42 checks made the compatibility promise executable
  instead of leaving it as prose.
- **The producer contract stayed authoritative.** Native scene generation
  and world-wasm both consumed the same Rust scene producer. The byte-identity
  smoke caught the boundary that mattered and kept the client from becoming a
  second orbital-physics implementation.
- **The almanac fixture exposed the expected consequence.** Regeneration
  moved only the locked-world almanac's event prose and timing rows. The
  diff is evidence that the Task 2 event layer is visible to readers, not an
  accidental unrelated fixture shift.

## What to carry forward

- **Generated-artifact scope still needs an explicit diff review.** `make
  artifacts` is broader than this campaign's declared outputs and includes
  live studies. The closeout kept the run because its final write set moved
  only the expected locked almanac and timing ledger; future campaigns should
  make that classification immediately after the command.
- **Baseline failures must be separated from contract failures.** The scene
  worker encountered worldgen observation-count drift. It was resolved as a
  named rebaseline commit (`5eb03bc48`), while the scene field-order and
  native/wasm identity assertions stayed intact.
- **The first useful orbital model is not the final one.** Circular,
  bounded topology is a good deterministic instrument, but eccentricity,
  inclination, transits, and arbitrary N-body dynamics each deserve their
  own evidence and registry boundary rather than being smuggled into this
  contract.

## Deferred at close

- `ORRERY-ellipse-truth`: true elliptical rendering and Kepler-correct
  anomaly.
- `SKY-transits`: wanderer transits and occultations.
- `SKY-figures-per-species`: perception-specific figure catalogs.
- `SKY-multi-star-nbody`: arbitrary multi-star dynamics.
- `SKY-binary-planet`: full binary-world promotion remains a separate,
  spine-breaking program.

The canonical census and sluice gates were intentionally not run here.
