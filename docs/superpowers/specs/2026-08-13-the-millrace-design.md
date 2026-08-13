# The Millrace — design

**Campaign.** Make the laboratory's read path over the river network cheap
again, without moving a single committed world.

**Branch.** `campaign/the-millrace`, cut from `main` at `36a8f383`.

**Predecessor.** The Rill (decision 0130) made the network ~16x denser and the
census 11.2x dearer. This campaign pays that bill. Its inheritance is
`docs/retrospectives/the-rill.md` follow-ups 1, 2 and 9, and the `TOOL-24` row
of `book/src/frontier/idea-registry.md`.

---

## 1. Four premises of the brief that measurement or reading refuted

The campaign was handed a brief. Four of its claims did not survive contact
with the code, and each changes the design rather than merely sharpening it.
They are stated first because a reader who skips them will re-derive them.

**1.1 `NearestCellIndex` is *not* threaded into `nearest_line`'s query path,
and cannot answer the question an index needs.** Its entire public API is
`new` / `nearest` / `nearest_to_position` (`kernel/src/geosphere.rs:354-486`) —
**single-nearest only, no radius query**. `cover_deg` and `grid` are private,
and `Geosphere` exposes **no edge-length accessor at all**
(`domains/terrain/tests/channel_properties.rs:31-33` says so in as many words).
`ChannelNetwork` stores neither a `Geosphere` nor an index; `nearest_line`'s
signature is `(&self, position: [f64; 3])`.

Two consequences. Any index must be **owned by `ChannelNetwork` and built in
`build`**, because adding a parameter ripples through `provider.rs:500`, five
`windows/locale` call sites and `metrics.rs`. And the brief's implied
"expand rings from the nearest cell" scheme has **no sound stopping rule
available** — the mesh publishes a *max*-edge bound, privately, and no
*min*-edge bound. §3's construction is preferred because it needs neither.

The brief's claim was true of a *different* function: `rill_reading` does take
`index: &NearestCellIndex` (`domains/terrain/src/branch.rs:805`). The two were
conflated.

**1.2 `rill_reading` is a contract deliverable, not a cost one — and more
strongly than the brief allows.** It considers exactly `here` plus
`geo.neighbors(here)` — seven candidates, bounded, not a network scan. Beyond
that, it has **zero call sites in the lab read path**: exactly one registered
metric reaches `hornvale_locale` at all (`cold-built-room-share`,
`metrics.rs:4684`), and its chain runs `temperature_at_cached` ->
`temperature_with_weights`, never entering `describe_with_weights`
(`windows/locale/src/lib.rs:752`), which is the sole holder of the
`rill_reading` and `bank_reading` calls. `rill_reading` is a real cost on the
**player's** walk loop, which is production, not the laboratory.

So this campaign owes `rill_reading` an **assertion, not an index** (§5).

**1.3 `channel-connectivity` is 23% of per-world cost, not 7.5%.** The 7.5%
figure is a share of the *pre*-memoisation total of 72.03 CPU-s/world. The
Rill's own memoisation then took the total to 23.93 without touching
connectivity, so its 5.50 CPU-s/world is now **23.0% of the total and 34.3% of
the lab-read half**. It is already the largest single identified lab-read term.
Its size *and* its slope are the problem.

**1.4 The brief's "400 probes per vertex" is a level-5 test figure.** The
`metrics.rs:9168` comment describes the `steps: 400` argument a
`#[cfg(test)]`-only positive control passes to `lab_channel_transect_width`
(`metrics.rs:7046`, `:9202`), on a `Geosphere::new(5)` world. The shipped sweep
is **98 probes per taken vertex**, and the census grid is level 6
(`GLOBE_LEVEL = 6`, `domains/terrain/src/lib.rs:82`). Level-5 commentary
scattered through the tests understates the read path by ~4x; none of it may be
carried into a budget.

---

## 2. Where the cost actually is

The lab read path is **two functions over one shared primitive**. Nothing else
in the registry touches the network: `channel-land-fraction` integrates
`band_edges` directly and calls none of the scan primitives.

| term | per-world calls into `nearest_line` | site |
|---|---|---|
| `lab_band_transects` (memoised; feeds 3 columns) | ~49,400 | `metrics.rs:7298-7369` |
| `lab_channel_connectivity` | `7 x H`, `H` unmeasured | `metrics.rs:7170-7227` |

One `nearest_line` call scans all 3,606 polylines / ~11,000 segments, at 1-3
`libm::acos` per segment. The transect sweep alone is therefore
**~5.4 x 10^8 segment evaluations per world**, and a full refresh of both
committed censuses pays it **2,000 times** (`the-census` 1,000 worlds;
`census-of-the-meeting` 500 seeds x 2 pin sets).

### 2.1 The preregistered cost prediction, stated as a decomposition

Frozen before the code, per decision 0016. All figures are CPU-s/world on The
Rill's attribution harness — **not census wall seconds**; the census runs at
`cpu_ratio` ~36.5 on 40 cores, so translating assumes that parallel efficiency
holds. Every number below is **reconstructed from The Rill's published figures,
not measured by this campaign**; the campaign takes its own before-arm at
`git merge-base main HEAD` and these are the predictions that arm is scored
against.

```
                                      CPU-s/world     note
  build (unchanged by this campaign)        7.88      The Rill null 1
  lab reads                                16.05
      channel-connectivity                  5.50
      band transects + remainder           10.55
  ------------------------------------------------
  total, post-memoisation                  23.93

  pre-Rill baseline                        ~7.83      build dominates it
```

Predicted landing, by lever, each falsifiable on its own:

| lever | predicted effect | mechanism |
|---|---|---|
| L0 duplicate-scan removal (§4) | band transects **/2** | one `bank_reading` replaces two scans |
| L1 connectivity suffix-memo (§6) | connectivity hops `walks x depth` -> `L` | shared trunk suffixes |
| L2 the index (§3) | remaining `nearest_line` cost **/k** | `k` = the measured candidate-set factor, **unknown** |

`k` is the campaign's one genuinely unknown quantity, and the honest statement
is that the brief's ~1.1-1.3x target brackets two different worlds:

```
  to reach 1.1x pre-Rill : lab reads must fall 16.05 -> 0.79   (20.4x)
  to reach 1.3x pre-Rill : lab reads must fall 16.05 -> 2.36   ( 6.8x)
```

**Prediction P1.** With L0 + L1 + L2, the census lands at **1.05x-1.45x** the
pre-Rill 1,718.995 s — i.e. **1,800-2,500 s**. The brief's 1.1-1.3x sits inside
this and is not itself predicted; the interval is wider deliberately, because
`k` is unmeasured and the campaign will not narrow an interval it cannot
justify.

**Prediction P2.** Define `k = L / |candidates(p)|` — the factor by which the
candidate set shrinks the 3,606-line scan, so **larger is better**. Over the
real census query population:

```
  median query          k >= 8      (examines <= ~450 of 3,606 lines)
  95th-percentile-worst k >= 2      (examines <= ~1,800 lines)
```

Stated as a **distribution, not a mean**, because the win is structurally
heterogeneous: the search radius scales with the answer distance, so a probe
beside a headwater wins hugely and a mid-ocean probe degrades toward the full
scan. A mean would be dominated by whichever tail is larger and would hide
exactly the failure mode that matters. **P2 is the falsification hinge for the
entire keystone**, and §3.3 requires it measured *before* the index is built.

---

## 3. The keystone: indexing `nearest_line` without moving a world

### 3.1 What the contract actually is

```rust
pub fn nearest_line(&self, position: [f64; 3]) -> Option<(usize, f64)> {
    let mut best = f64::INFINITY;
    let mut best_line: Option<usize> = None;
    for (i, line) in self.polylines.iter().enumerate() {
        let d = line.signed_distance(position);
        if d.abs() < best.abs() { best = d; best_line = Some(i); }
    }
    best_line.map(|i| (i, best))
}
```

The winner is the lexicographic **argmin over `(|d_i|, i)`** — minimise `|d|`,
break ties to the lowest index. `SphericalPolyline::signed_distance`
(`kernel/src/polyline.rs:53-108`) returns the minimum *unsigned* great-circle
distance over the line's segments, multiplied by the winning segment's side, so
`|signed_distance|` **is** the geometric minimum. Beyond a segment's endpoints
the distance is to the nearer endpoint. A degenerate segment borrows its side
from the nearest index-adjacent segment with a defined normal.

That tie-break reaches the **serialized sign** of `bank_signed_distance`
(`channel.rs:869-888`), which is why this is a determinism-contract change and
not a refactor.

### 3.2 The design, and why its correctness is one inequality

**The index narrows the candidate *line set* only.** For each candidate we call
the **unchanged `line.signed_distance(position)`** and apply the **unchanged**
`d.abs() < best.abs()` in **ascending polyline index**. Nothing about the
arithmetic moves: the returned `f64` is bit-identical because it is produced by
the same function on the same inputs, and the sign is never re-derived.

Everything therefore reduces to: **is the true winner in the candidate set?**

Bucket **vertices** (payload: the polyline index) into a spherical grid at
build time. Query at `p` with a working best `D`, gathering every polyline with
a vertex inside the cap of radius `rho = D + L_max/2`. For a segment `[a,b]` of
arc length `L` whose closest point to `p` lies at distance `d`, the two
sub-arcs sum to `L`, so `min(|qa|,|qb|) <= L/2`, and by the spherical triangle
inequality:

> **min( angle(p,a), angle(p,b) ) <= d + L/2 <= d + L_max/2**

So **every segment within `D` of `p` has an endpoint inside the cap**, hence its
polyline is in the set, hence the winner is. `L_max` is the only quantity that
must be bounded — and it is **measured exactly in one O(V) pass at build time**,
never assumed.

Independently, `L_max <= 1.5 * E_max` in theory: consecutive run cells are mesh
neighbours (`downhill` targets come from `geo.neighbors`), and meander
displacement is exactly `atan(|offset|) <= 0.25 * cell_spacing` — the offset is
applied purely tangentially, `MEANDER_AMPLITUDE_RATIO = 0.25`
(`channel.rs:177`), `confinement` is clamped to `[0,1]` (`:300`) and
`meander_field` lies in `[-1,1)`. The confluence repair moves a mouth onto the
trunk's vertex **for the same cell** (`:731-747`), so the bound survives it. The
build-time measurement is the contract; this paragraph is the tripwire on it.

Iterate: if `D + L_max/2 > rho`, re-gather at the larger radius (`D` decreases
monotonically, so it terminates). If `rho >= pi`, fall back to the full scan. A
non-empty fallback is **mandatory** — the original returns `Some` whenever
`polylines` is non-empty.

Dedup candidates with an epoch-stamped `Vec<u32>` over polylines, not a
`BTreeSet`: this is the hot path, and a long line lands in many buckets.

### 3.3 The measurement that comes before the code

**P2 is measured first, on the shipped tree, with no index built.** Instrument
the candidate-set size a cap of radius `D + L_max/2` *would* have produced, over
the real census query distribution, and report the distribution. If the median
`k` falls below 8, the lever is falsified before it is built, and the campaign
says so and stops at L0 + L1. This ordering is deliberate: it is the cheapest
possible falsification of the campaign's most expensive task.

### 3.4 The oracle survives, permanently

The linear scan **stays in the tree** as `nearest_line_reference`, and a
property test asserts `index == reference` across levels and a wide position
sample — including polar positions and positions beyond every line's endpoints.

This is The Bearing's precedent copied deliberately
(`docs/retrospectives/the-bearing.md`): its all-levels equality test against the
unindexed reference caught **two real bugs, a tie-break inversion and a
near-pole coverage hole**, and both failure modes are live here. The near-pole
one specifically returns if the bucket grid uses a `cover/cos(lat)` longitude
window, which must now be recomputed **per query** from `rho` rather than from a
fixed constant.

An index whose reference implementation is deleted is an index nobody can ever
re-verify.

---

## 4. L0 — the duplicate scan, and why it goes first

`metrics.rs:7332-7338` runs the all-lines scan **twice per probe**:

```rust
if net.nearest_line(q).map(|(k, _)| k) != Some(i) {   // :7335  scan #1
    still_own = false;
}
let band = net.transverse_at(q).0.index();            // :7338  scan #2
```

`transverse_at` -> `bank_reading` -> `nearest_line` (`channel.rs:805, 850`).
`bank_reading` already returns **both** answers in one `BankReading`, and
`transverse_at` is *defined* as `Transverse::from_band(band(reading
.signed_distance, &reading.band_edges))` over exactly that reading. One
`bank_reading` call replaces both — **byte-identical by construction, not by
argument**: the two values are read off the same struct the second call was
already building.

**It ships first, before the index.** It is provable without measurement, and
landing it first means the index is measured against a tree that no longer
contains an obvious 2x — so the index is credited only with what it actually
buys. That is the attribution discipline The Rill's §5 was written about.

---

## 5. The contracts, stated and held

Success criterion 3 of the brief. Both tie-breaks are documented today and
**held by nothing**.

- **`nearest_line`** — argmin over `(|d|, index)`, strict `<` keeping the lowest
  index, and the winner's sign is what `bank_signed_distance` reports.
- **`rill_reading`** — candidates are `here` then `geo.neighbors(here)` in the
  geosphere's yield order, strict `<` keeping whichever was offered first
  (`branch.rs:782-798`). Its serialized path is
  `rill_reading -> grounded_wetness -> micro.wetness`.

Each gets an assertion that **fails when the tie-break is inverted**, proven by
mutation. Two rules govern that proof, both learned at The Rill's close:

- **A mutation must prove it mutated** — assert the target text exists before
  substituting it.
- **Verify a revert by re-running after `touch`, never by grepping the source.**
  `mv file.bak file` can restore an mtime *older* than the compiled binary, so
  cargo re-runs the stale binary. One direction gives a false red; the other
  gives a false **green**, which is the direction that silently invalidates a
  mutation proof.

---

## 6. `channel-connectivity`: cost, vacuity, and a foot-gun

### 6.1 The vacuity is real; its magnitude is unmeasured

The metric's continuation test asks `WaterKind::River` (`metrics.rs:7187-7192`)
while `ChannelNetwork::build`'s reach predicate is
`!Ocean && downhill.is_some()` (`channel.rs:560-562`) — strictly wider, since
`River` additionally requires `drainage >= RIVER_MIN_DRAINAGE` (15.0,
`water.rs:71`) and rivers are ~6.7% of seed-42 land. A tributary ending on a
sub-threshold trunk breaks the walk **before** its join and scores `intact`
untested.

**Prediction P3.** The fraction of walks whose first continuation test is false
is measured, seed 42 at level 6, and reported as a finding. **No prior figure
is inherited** — The Rill's review estimated ~3,500 of 3,606 and correctly
refused to assert it.

### 6.2 The repair makes the metric *more* expensive, and the memo is not
union-find

After the repair a walk breaks only where nothing owns the cell, so **walks
chain to the sea instead of stopping at the first sub-threshold trunk**. The
loop's own bound is `0..=net.polylines.len()` = 3,607, giving a worst case of
`3,606 walks x 3,607 hops x 7 probes x O(11,000)`. **Landing the repair without
a memo would make the term the campaign was sent to fix dramatically worse.**

Walks share suffixes — the loop ends each iteration with `line = trunk`
(`:7220`) — and the per-hop test depends only on the hop, never on the walk's
history. So intactness is a pure **suffix property**:

```
intact_from[line] = hop_ok(line) && intact_from[trunk_of(line)]
```

Memoising collapses `walks x depth` to **`L` hops evaluated once** and is exact
by construction. The downhill graph is acyclic (`:7183-7184`), so no visited set
is needed.

**This displaces the brief's union-find suggestion.** Union-find answers *graph*
connectivity with no geometry — but the metric's content **is** geometric:
whether seven interpolated points across a join stay inside the `Channel` band.
A graph formulation answers a different question, and would be a different
metric with a different column, not a cheaper version of this one. It is
captured as a registry row, not adopted.

**Constraint: the repair and its memo land together or not at all.**

### 6.3 Whether a column moves is decided by measurement, not now

The confluence repair relocates *every* tributary mouth onto its trunk's vertex
for the same cell, with **no water-class filter** (`channel.rs:730-747`). So
after the predicate repair `from == to` exactly at a join, all seven probes
collapse onto that point, and `band(0.0, edges) == 0 == Channel`. The repair
therefore very likely converts a **vacuous 1.0 into a tested 1.0** rather than
moving the value.

If so, adding a second column carrying an identical 1.0 is noise. So the
decision is a rule resolved by measurement:

```
Run both arms over the-ford-probe's 64 seeds, same build:

  values DIFFER on any seed   -> ship as a NEW additively-named column; the
                                 existing column keeps its definition, and its
                                 census_history series stays interpretable.
  values IDENTICAL on all 64  -> repair IN PLACE. No column added, no census
                                 value moves; the census diff must then be
                                 byte-identical on all 203 columns.
```

Either branch moves three committed artifacts, because the published `doc:`
literal (`metrics.rs:3473-3520`) flows into
`book/src/laboratory/generated/the-census/schema.json`,
`.../census-of-the-meeting/schema.json`, and `book/src/domesday/hydrology.md`.

### 6.4 Two consequences to carry into the plan

- **The drop-out arm goes structurally dead.** After the repair, `continues` and
  the `owner[last_cell]` lookup are the same expression, so
  `else { good = false }` (`:7197-7202`) becomes unreachable. It must be
  **re-expressed**, not silently deleted — it is the only signal that a walk
  fell out of the network.
- **`owner` is a per-world rebuild a published accessor already answers**, and
  substituting it is the same "one implementation" argument
  `bank_signed_distance` makes. But they are **not identical**:
  `ChannelNetwork::trunk_vertex` (`channel.rs:458`, public at `:781`) keeps the
  **first** claiming run (`:758`); `lab_run_owner` (`metrics.rs:7075-7089`)
  keeps the **last**. They agree only because the relation is asserted
  functional by R-4. The substitution is safe **only on R-4's strength and must
  say so.**

---

## 7. Acceptance criteria

1. **Byte-identity is the criterion, not a nice-to-have.** Full artifact regen
   plus a census diff. A green suite is not evidence: the columns at risk live
   in `#[ignore]`d probes the gate never runs.
2. **`nearest_line` == `nearest_line_reference`** across levels 4-7 and a wide
   position sample including poles and beyond-endpoint positions (§3.4).
3. **Both tie-breaks held by an assertion**, each mutation-proven under §5's two
   rules.
4. **The census diff is either byte-identical on all 203 shared columns, or
   additive** — demonstrated by the shared-column diff, never by line counts
   (The Ford's technique: 5 columns added rewrote all 1,000 rows, and the
   evidence that nothing moved was 0 diffs across the 198 shared columns).
5. **A named census cost figure**, measured the same way as the 19,207.751 s
   baseline, on `lefford`, recorded in `docs/timings.md`.
6. **P1, P2 and P3 reported against their preregistered statements**, falsified
   or not. A falsified prediction is a finding; no constant is retuned to rescue
   a number after unblinding.

---

## 8. Non-goals

- **Indexing `rill_reading`.** It is already bounded and absent from the lab
  read path (§1.2). It receives an assertion only.
- **A union-find / graph-formulated connectivity metric** (§6.2) — captured as a
  registry row.
- **Pushing per-probe results down from `build`** instead of pulling per query.
  Coherent, but it changes what the transect metric measures — the transect
  exists to find where *other* lines interfere. Registry row.
- **A warm-start cursor threaded through call sites.** Byte-safe (a bound only
  ever prunes) and the map-matching literature's standard move, but it requires
  signature changes the index does not. Held as an optional second lever, to be
  built only if L0 + L1 + L2 miss P1. A per-cell seed line would get most of the
  benefit with no signature change.
- **Any new dependency.** The allowlist is `serde`, `serde_json`, `libm`
  (decisions 0004 / 0041). The index is hand-rolled.

---

## 9. Constraints

- No `HashMap`/`HashSet`; `BTreeMap`/`BTreeSet`/`Vec` only. Float ordering via
  `total_cmp` with deterministic tie-breaks. No wall-clock time, including
  `std::time::Instant` in test code.
- Measurement on matched arms over a **scratch study**, never the census;
  the before-arm taken at `git merge-base main HEAD`, never at `main`.
  `/usr/bin/time -l` for wall *and* peak RSS, `uptime` either side, `--release`.
  A contended measurement is a contention datum, not a cost datum.
- The census runs on `lefford` only, once at the pre-merge close, and
  **regeneration is an explicit-authorization carve-out — ask Nathan.** Budget
  from `docs/timings.md` (`grep '| census |' docs/timings.md | tail`), never
  from a doc comment.
- `make gate` is ~8 min and three other campaigns are live on this box
  (`the-docket`, `the-fathom`, `the-repose`) — stagger gates.

---

## 10. Task outline

Sequenced so that each task's measurement is honest about what precedes it.

| # | task | gate |
|---|---|---|
| 1 | L0: collapse the duplicate scan (§4) | byte-identical artifacts; before/after arms |
| 2 | Measure P2's candidate-set distribution on the un-indexed tree (§3.3) | **falsification hinge for task 4** |
| 3 | Contract assertions for both tie-breaks (§5) | mutation-proven red |
| 4 | The index + the permanent oracle (§3) | acceptance 2; byte-identical artifacts |
| 5 | Connectivity: quantify P3, memoise, repair (§6) | the §6.3 branch table decides the column |
| 6 | Close: census refresh, chronicle, retrospective, `TOOL-24` levers | acceptance 4, 5, 6 |

Task 2 gates task 4. Task 5's memo and repair are one commit. Task 1 precedes
everything so the index is measured against a deduplicated tree.
