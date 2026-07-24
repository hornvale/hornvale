# The Bearing — retrospective

**Completed:** 2026-07-23 (spec `docs/superpowers/specs/2026-07-23-the-bearing-design.md`,
plan `docs/superpowers/plans/2026-07-23-the-bearing.md`, four tasks: 2-D grid
with a provable coverage bound → all-levels equality test → artifact-drift
verification → close). Third geosphere-perf campaign of the session, after The
Lookup (shipped) and The Commons (parked at G3). Ran under campaign-autopilot.

**The find only existed because we profiled the observation surface, not
genesis.** The whole session's perf work had measured world *generation* (the
`new` and census paths). `NearestCellIndex::nearest` never showed up as a top
cost there — it's 10.5% of the census, real but not dominant. It took profiling
a *renderer* (`map`, `scene tiles`) to see it was 78% of the map. The lesson
that generalizes: profile the thing you actually run, and enumerate the *kinds*
of thing you run. A hotspot can be modest in one entry point and dominant in
another the profiler never visited.

**The stale-profile trap struck the queue, not just a measurement.** #2 (the
`annual_mean_insolation` loop-hoist) was queued as a promising lever off the
*first* flamegraphs — which were taken on a stale pre-arc checkout. On current
main, insolation wasn't hot, and the hoist was perf-neutral. The Bearing exists
because we then re-profiled on the correct tree and found the real lever. Cheap
spikes are the defense: #2 was disproven for two builds' worth of effort rather
than a whole campaign.

**Byte-identity by construction, defended at three depths.** A rendering change
that moves a committed pixel is a corruption, so the campaign proved
non-movement three ways, each catching what the last couldn't: (1) an
**all-levels equality test** vs the band scan over a dense sweep — which
*caught two real bugs* the spike's levels-3-6 test had missed; (2) the seed-42
world sha (weak — genesis barely uses the lookup); (3) **artifact drift** — the
committed maps regenerating byte-for-byte, the only check that exercises the
real render grids end to end. Depth (1) is where the design was actually forced
correct.

**The two bugs the all-levels test caught are the whole story.** The spike was
"fast but occasionally wrong," and only the strict all-levels bar exposed how.
(a) **Tie-break:** the fast bucket-jump visits cells in a different order than
the band scan, so on an exact-equidistant tie it chose a different cell. Fixed
not by sorting (too slow) nor by iterating the full band to preserve order
(*slower than the original* — measured 2.0 s, a salutary dead end) but by an
explicit `(band, CellId)` tie-key that names the band scan's winner in any visit
order. (b) **Pole coverage:** the `cover / cos(lat)` longitude window
*linearizes*, and near the poles a cell 180° away in longitude is only a few
degrees away angularly — so the window wrongly excluded the true nearest. Fixed
by a near-pole full-ring guard. **Lesson: a perf optimization's correctness
bar must be the exact contract at every input regime, not a convenient subset —
the subset is exactly where "fast but subtly wrong" hides.**

**Provable beats empirical for a kernel change.** The spike used a hand-picked
24° coverage constant that happened to pass. Execution replaced it with a bound
*measured from the mesh's longest edge*, so coverage is a property of the code,
not of the test's luck — and it auto-adapts (coarse levels, with big edges,
fall back to full-ring). The all-levels test then confirms the bound is
sufficient rather than being the only thing standing between the code and a
silent wrong answer.

**Scope notes.** No `open-questions.md` bet moved; no idea-registry row (infra,
though A4 below is frontier-adjacent). Independent of The Commons (different
file, different mechanism) — they compose but landed separately.

## Follow-ups (promoted from `.superpowers/sdd/followups.md`)

- **B1: precompute the pixel→cell table per (level, resolution)** — the render
  grid is fixed + seed-independent; byte-safe by construction; the regen's ~8
  maps would share one table. Stacks on The Bearing; the obvious next render win.
- **Tighter latitude windowing** — `nearest` is still ~45% of the map render
  because it sweeps three full latitude bands; a 2-D window on latitude too
  would push further.
- **A2/A3 (mesh-hierarchy descent / warm-start neighbor-walk)** — O(log N) /
  O(1) amortized but byte-fragile (tie-breaks); only if B1 + latitude windowing
  are insufficient.
- **A4 (frontier): HEALPix / S2 direction→cell-id** — the field's canonical O(1)
  answer; a large rework against the committed geosphere cell IDs.
- **Profile `possess`** — the interactive walk, still unprofiled; `room.rs`
  resolves cell corners via `nearest_to_position`.
- **The Commons** (parked at G3) — Arc-share the cached mesh; its own follow-up
  is the climate-provider clone.
