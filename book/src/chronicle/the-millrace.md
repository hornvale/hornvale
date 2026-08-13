# The Millrace

[The Rill](./the-rill.md) drew the fourteen fifteenths of the drainage tree
that had never been rendered, and the river network went from roughly two
hundred polylines to **3,606**, from 890 vertices to 14,606. Nothing about the
world got slower. What got slower was *looking* at it: the census — the
thousand-world fixture the Laboratory measures itself against, plus the
five-hundred-seed census of the meeting — went from **1,718.995 seconds to
19,207.751**, five hours and twenty minutes, an 11.2× wall and 12.7× CPU
regression against the run immediately before it.

That is a measurement instrument becoming too expensive to run, which is a
slower and more corrosive failure than a wrong number: an instrument nobody can
afford to point at the world stops being pointed at it.

This campaign pays the bill. It is a performance campaign in a codebase where
the acceptance criterion for a performance campaign is not a benchmark but a
**byte-identity proof** — the seed is the world's identity, and the function
being made fast decides a serialized sign.

## The result

| | wall | against pre-Rill |
|---|---:|---:|
| pre-Rill census (2026-08-12) | 1,718.995 s | 1.00× |
| The Rill's census (2026-08-13) | 19,207.751 s | 11.17× |
| **The Millrace's census** | **949.579 s** | **0.55×** |

The census is now **1.81× faster than it was before the river network got
sixteen times denser**, and 20.2× faster than The Rill left it. On the lab's
own read path over the network — five channel metrics plus a terrain-depth
world build, measured over sixty-four worlds on one matched harness — the cost
went from **28.74 to 0.30 CPU-seconds per world**.

**Nothing moved.** Two thousand world evaluations across both censuses produced
a byte-identical diff: no metric value, no schema, no row. The only artifact
the refresh wrote was the timing row recording itself.

## Prediction P1 was falsified, in the favourable direction

The campaign preregistered its landing point before any code was written:
**1.05×–1.45× the pre-Rill baseline, i.e. 1,800–2,500 seconds.** The interval
was deliberately wider than the brief's own 1.1–1.3× guess, because the one
quantity it depended on — how much an index would actually narrow the candidate
set — was unmeasured.

It measured **949.579 s, 0.55×**. That is not inside the interval, and it beats
the optimistic end by a further 1.9×. The honest report is that the prediction
**missed**: the campaign did not predict that the census would end up cheaper
than it was before the network was densified at all, and restating the target
around the result afterwards would convert a miss into a fabricated hit.

Two mechanisms account for the gap, and both were visible only after the fact.
The interval's floor assumed the world build (~7.9 CPU-s/world, unchanged by
this campaign) would dominate whatever was left; in practice the *census's*
per-world build is cheaper than the harness the interval was reconstructed
from, so the floor was anchored too high. And the interval was expressed
against a cost model reconstructed from The Rill's published figures — figures
whose measuring harness **was never committed and no longer exists**. A
baseline whose instrument cannot be re-run cannot be re-measured, so it was
demoted from baseline to historical context mid-campaign, and every later arm
was taken on one harness this campaign owns.

## What made it safe: one non-strict inequality

`ChannelNetwork::nearest_line` answers *which river line is nearest this
point, and on which side*. It was a linear scan over all 3,606 polylines,
returning the lexicographic argmin over `(|d_i|, i)` — smallest absolute
distance, lowest index on a tie. That tie-break is not cosmetic. The winner's
own signed distance is what `bank_signed_distance` serializes, and two lines
tied at exactly equal distance sit on **opposite** sides of a query point, so
an index that returned *a* nearest line rather than *the lowest-indexed*
nearest one would flip a committed sign and then drift-check green forever.

The index is a vertex-keyed spherical bucket grid, and the whole correctness
argument reduces to one set-membership claim. For a segment `[a,b]` of arc
length `L` whose closest point to `p` lies at distance `d`, the two sub-arcs
sum to `L`, so

```
min( angle(p,a), angle(p,b) )  ≤  d + L/2  ≤  d + L_max/2
```

Every segment within `d` of `p` therefore has an **endpoint** inside the cap of
radius `d + L_max/2`. Bucket the **vertices**, gather that cap, and the winner
cannot be absent. Bucketing whole polylines instead would have been useless at
this density — a level-6 run spans many cells, so its bounding cap covers most
of a continent and excludes nothing. The distance function itself is called unchanged, so the
returned `f64` is bit-identical by construction rather than by tolerance — the
index narrows *which* lines are compared and never *how*.

Three things make that argument load-bearing rather than decorative:

**`L_max` is measured, not assumed.** It is computed in one `O(V)` pass at
build time, over the network as actually assembled — after the confluence
repair that relocates tributary mouths. Independently, theory bounds it above
by `1.5 × E_max` (consecutive run cells are mesh neighbours; meander
displacement is a quarter of a cell spacing at most), and the campaign asserted
that bound rather than trusting it: measured `L_max = 0.021732976` rad against
`E_max = 0.020673412` rad, a ratio of **1.051** against a ceiling of 1.5. The
derived bound survives its own tripwire with thirty percent of headroom, and
the tripwire is a test rather than a comment.

**Ties survive because the pruning inequality is non-strict.** The loop exits
when `|best| + L_max/2 ≤ ρ`. A line tied at exactly `|best|` has a vertex
within `|best| + L_max/2 ≤ ρ`, so it *is* in the cap, and the ascending scan
over the gathered candidates then takes the lowest index — the same winner the
linear scan chose. Change that `≤` to `<` and the determinism contract breaks
on precisely the case the contract exists for.

**The linear scan is still in the tree.** It survives as
`nearest_line_reference`, byte-for-byte the loop that shipped before the index,
never deleted, with a property test asserting `index == reference` across mesh
levels and a wide position sample. An index whose reference implementation has
been deleted is an index nobody can ever re-verify.

The pole-crossing case earns its own note. The gather widens a latitude/longitude
window recomputed per query from `ρ`; below `ρ = π/2` the explicit pole test is
redundant, and at or above it the test is load-bearing, because `sin(ρ)` folds
back and the longitude half-width would silently under-cover. And the `ρ ≥ π`
exit to the full scan turns out to be **provably unreachable**: `π` is the
supremum of `d + L_max/2` and never an attained value, since a segment of
length `L` puts every point within `π − L/2` of an endpoint. The campaign
measured how tight that is — the short-arc antipodal case reaches
`3.1415926535897927` against `π = 3.141592653589793`, **short by exactly one
ULP** — and then kept the exit anyway, pinning the near-breaking geometry
rather than contriving a test for a branch that cannot fire.

## The free halving nobody had found

Before any index, the lab's transect sweep was running the all-lines scan
**twice per probe**: once to ask which line owned the probe position, once to
classify which band it fell in. The second call's own implementation already
computed the first call's answer and threw it away — one `BankReading` carries
the signed distance, the band edges, the cell and the line, and the band
classification is a pure read off that struct.

Replacing two scans with one is byte-identical *by definition* rather than by
argument: the two values are read off the same struct the second call was
already building. It landed first, deliberately, so that the index would be
measured against a tree that no longer contained an obvious factor of two and
would be credited only with what it actually bought. Measured: **28.74 → 16.93
CPU-s/world, a delta of −11.81**, with an empty artifact diff across every
committed path.

## Prediction P2 held, and holding is the finding

P2 was the campaign's declared falsification hinge — the one measurement
allowed to kill the expensive task before it was built. It defined
`k = L / |candidates|`, the factor by which the index shrinks the 3,606-line
scan, and required a **median `k ≥ 8` and a 95th-percentile-worst `k ≥ 2`**,
measured on the real query population with no index in existence.

It held with two orders of magnitude to spare: median `k` of 1,803, fifth
percentile 1,202, minimum 601 over 41,027 queries.

**And the hinge could not have swung.** P2 needs two inputs, and both were
already committed to the repository before the campaign started. `L_max ≤ 1.5 ×
E_max` is derivable from the meander constants without measuring anything. And
the query population's own offset `D` is sub-cell — the widest channel is under
a tenth of a cell edge, and both probe generators offset by small multiples of
band widths, so `D` contributes under 3% of the search radius. Together these
force `ρ < ~0.8` cell spacings for **every** query *independently of `L`*, so
the candidate set is the runs incident to one cell's neighbourhood: a constant.
Hence `k ≈ L / O(1)`, growing linearly in network size. Turned around, `k = 8`
would have required roughly 450 distinct polylines to pass through a cap of
0.58 cell spacings — about 450 runs through one cell of 40,962. Structurally
unreachable, not merely unlikely.

The transferable form is a defect in the preregistration, not in the
measurement:

> **A floor preregistered on a *ratio* whose denominator is the quantity under
> study is a floor on that quantity in disguise.** `k = L / (something
> independent of L)`, so "median `k ≥ 8`" was "the network has at least 450×8
> lines" wearing a costume, and it was satisfied by the densification the
> campaign was responding to.

The discriminating threshold was on the **candidate count** — the numerator's
denominator, the thing the index actually delivers: *95th-percentile candidate
count ≤ 4, maximum ≤ 12*. Observed maximum was 6, and a confluence-denser
network could genuinely violate it. Better still, on the delivered count from a
*specified* bucket grid, because `ρ` being sub-cell means the grid's cell size —
a design choice the campaign had not yet made when P2 was frozen — is what sets
the candidate set. P2 as written was insensitive to every choice that remained
to be made.

## The metric that was measuring nothing

`channel-connectivity` asks whether a tributary's join with its trunk stays
inside the channel band — seven interpolated probes across the junction, one
walk per polyline. Its continuation test asked whether the next cell was
classified `WaterKind::River`, while the network's own reach predicate is
`!Ocean && downhill.is_some()` — strictly wider, because `River` additionally
requires drainage above a threshold that only about 6.7% of land clears. A
tributary ending on a sub-threshold trunk therefore broke its walk **before**
the join and scored `intact` without ever testing one.

P3 preregistered that this fraction be measured and reported, inheriting no
prior figure — The Rill's review had estimated ~3,500 of 3,606 and correctly
declined to assert it.

**Measured: 2,987 of 3,606 walks — 82.83% — vacuous on seed 42; 0.749 to 0.850
across sixty-four worlds (249,778 of 312,093).** The refused estimate was 17%
high, which vindicates the refusal: a number nobody measured is worth exactly
what it cost.

The repair asks the network's own reach predicate instead of re-deriving one
from water class — a deduplication rather than a better rule, on the same
argument `bank_signed_distance` already makes for delegating to `nearest_line`:
a second implementation is a second chance to disagree. Joins actually crossed
went from **853 to 2,333** on seed 42 (90,537 → 219,763 over sixty-four
worlds).

And the column's *value* did not move. It reads 1.0000 under both rules, on all
sixty-four worlds, because the confluence repair relocates every tributary
mouth onto its trunk's vertex for the same cell, unconditionally — so at a join
`from == to` exactly, all seven probes collapse onto one point, and a signed
distance of zero bands as `Channel`. The campaign predicted this branch before
running it and preregistered a decision rule for both outcomes: differ on any
seed and ship a new additively-named column; identical on all sixty-four and
repair in place. It was identical, so the repair landed in place and the census
diff stayed byte-identical on all 203 columns.

**A vacuous 1.0 became a tested 1.0.** That is the entire visible change, and
it is worth stating plainly: the census value is unchanged and the census
*claim* is not.

## The foot-gun that was not there

The campaign's own spec predicted that the repair would make the metric
dramatically more expensive: after it, walks chain to the sea instead of
stopping at the first sub-threshold trunk, giving a worst case of `3,606 walks
× 3,607 hops × 7 probes` over an `O(11,000)` scan each. It required the repair
and a suffix memo to land in one commit for exactly this reason.

**Measured, the deepest walk over all sixty-four worlds crosses 10 joins**, and
the mean is **0.704 joins per walk**. On seed 42 the depth histogram is
`{0: 2,028, 1: 1,035, 2: 378, 3: 125, 4: 33, 5: 7}` — and `Σ k·n_k = 2,333`,
reproducing the crossing count independently. The quadratic was never
approached. The mechanism is that `build` claims trunks in ascending cell
order, so a tributary generally joins a trunk that is already running to the
sea.

The cost of the repair is therefore a **null**: 0.314 → 0.317 CPU-s/world,
inside the within-arm spread of 0.012. Without the memo it is +0.012, also
negligible.

The memo was kept anyway, and the reason is a correction to the campaign's own
first explanation. "Ascending claim order" gives acyclicity and a bound on the
*number of runs*; it does **not** bound the *depth* of a chain. So the
quadratic is not structurally unreachable — it is empirically absent on
sixty-four worlds and nothing in the code prevents it. The memo is exact, costs
one pass, and removes the possibility rather than the observation.

## The connectivity problem dissolved rather than being solved

`channel-connectivity` was the campaign's second named cost lever, reconstructed
at 23% of per-world cost and identified as the term whose *slope*, not size,
would dominate. After the index landed, **all five channel metrics together
cost about 0.31 CPU-s/world**, and there was no connectivity cost left to
recover. The superlinearity was real in principle and never materialised in
practice.

So the campaign's cost result is **one lever, not two** — and the second
lever's own preregistered analysis was correct about the mechanism while being
irrelevant to the outcome. A term that is 23% of a cost you are about to divide
by fifty-six is not a term.

## What the byte-identity evidence actually is

It is worth being exact about this, because "no drift" is a claim that can be
made vacuously.

The evidence is a **full census refresh on the canonical host, at the merge
candidate, producing an empty diff** — 1,000 worlds in `the-census` plus 500
seeds × 2 pin sets in `census-of-the-meeting`, 2,000 world evaluations, across
203 metric columns, with `book/src/domesday/` (a pure read over that census)
also unmoved. The only file the run changed is the timing row it wrote about
itself. It is corroborated in-gate by an `index == reference` property test
across mesh levels, by a gather-coverage test over 1,376 position/radius pairs,
and by the fact that every intermediate task also produced an empty seven-path
artifact diff.

Two limits travel with it. The strength of the census as an oracle rests on it
*containing* the query classes at issue, and the measured query population is
near-channel **by construction** — both lab metrics probe near lines — so the
mid-ocean query the design named as its own worst case is *absent from the
measurement rather than disproved by it*. The property test supplies
far-from-network and polar positions itself, precisely because the census
population would not.

How near-channel it is shows up in the shape of the result: across 41,027
queries only **four distinct candidate-set sizes occur** — 1, 2, 3 and 6 —
because the search radius is effectively constant at 0.575 mean cell spacings
and the query's own offset contributes under 3% of it. The probe is therefore
not measuring a per-query quantity at all; it is measuring local channel
density at a fixed radius. Two consequences follow. The re-gather step — widen
and look again when the answer lies outside the cap — is a **correctness
requirement that this population essentially never exercises**, so the property
test is its only coverage. And none of this transfers to the world-walking
path: in `windows/locale` the distance to the nearest river is many cells, the
radius grows with it, and the sub-cell argument that makes the census result so
lopsided simply does not hold. **The claim is scoped to the lab's read path**,
which is what the campaign was for.

And the radius policy — the `L_max/2` term, the opening radius, the re-gather
condition — is guarded more thinly than it looks. Deleting the pruning bound
outright makes **exactly one probe in 6,175** disagree with the reference,
because the grid's bucket edge already exceeds `L_max` at every mesh level, so
quantisation supplies more margin than the bound covers for. The campaign
measured this, hypothesised two strengthenings, built and measured both, and
recorded both as nulls rather than shipping tests that caught nothing. It then
pinned the *mechanism* directly instead. The gap is documented as open in both
tests' doc comments, with the explicit instruction that a reader finding two
green tests must not conclude the policy is covered.

## What this campaign did not do

Four things were considered with a measurement attached and deliberately left,
rather than dropped:

- **Sizing the grid's latitude bands from `L_max`** rather than from the vertex
  count would make the analytic bound binding, tightening the gather *and* the
  equality test at once. It trades a performance characteristic for
  testability, which is a decision, not a fix round.
- **A union-find / graph formulation of connectivity** would answer the
  question with no geometry at all — but the metric's content *is* geometric
  (do seven interpolated points stay inside a band), so the graph version is a
  different measurement wearing the same name.
- **Pushing per-probe results down from `build`** instead of pulling them per
  query changes what the transect metric measures: the transect exists to find
  where *other* lines interfere.
- **A warm-start cursor**, or a per-cell seed line giving every query an
  achieved distance for free, was scoped as an optional second lever to be
  built only if the first missed. It did not miss.

---

*Campaign: The Millrace. Five implementation tasks, one pre-merge fix wave, one
census refresh. Census 19,207.751 s → 949.579 s; lab read path 28.74 → 0.30
CPU-s/world; zero goldens moved across 2,000 world evaluations. Predictions P1
falsified (favourably), P2 held but undiscriminating, P3 measured at 82.83%.
The spec's own foot-gun premise falsified at 10 joins deep against a predicted
3,606.*
