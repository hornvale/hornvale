# The Waymark

Ten creatures, forty ticks, ninety seconds. That was the arithmetic that
reframed this campaign before it began: the health battery's cost was
never population scale — it was **~150–200 ms of navigation re-derivation
per creature-tick**, four-hundred-odd budget-1000 searches to walk ten
creatures through forty ticks. Nathan's directive turned a test-time
grievance into a design bar: the marginal cost of a stationary,
unchanged-belief creature approaches zero, the algorithms stay
substitutable, and the whole stack gets benched at rungs the sim has
never seen.

## What shipped

**The tier split** (first, independently): the health seed sweep's
breadth arm — four seeds of pure replication for a silence-armed alarm —
moved to the heavy tier at campaign-close cadence, keeping the seed-42
control and the synthetic sensitivity scenarios in every commit's gate.
Clinical labs run one negative control per batch and the full panel on a
schedule; so do we now.

**The geometry memos**: a session-lived `RoomMeshMemo` — the
three-corner cell lookup (`corner_weights` plus its `scan_at` calls) and
`neighbors`, each a pure function of immutable inputs, each pinned
memoized-==-recomputed bit-for-bit. The interesting engineering was not
the maps but the walls: the hot `corner_weights` path flows through
`&dyn Terrain` inside the drive stack, where `&mut` cannot follow — the
answer was a **prefilled read-only cache**, filled before the drives
exist, consulted under `&self`, misses falling through so correctness
never depends on prefill completeness. The reviewer A/B'd the resulting
shape empirically: traces equivalent label-for-label, and the cached
path's advantage *widens* with session length, because thirst
integration re-reads historical sighting rooms that earlier ticks
already prefilled.

**The plan cache**: `home_nav(entity) → (distance, first_step)` — the
consumed feature, never the path — cached per entity across ticks,
invalidated by movement or by the believed-hazard set actually changing
(compared by value: no write-point enumeration to miss). Its guarantees
are deterministic integers, not wall-clock: pin tests assert **zero**
searches for a stationary unchanged creature after warm-up, exactly one
on movement, exactly one on belief change — and the adversarial
staleness test was proven able to fail by disabling invalidation and
watching three pins go red. Three separate copies of the same
unconditional replan were found and unified under the cache: the Social
drive's feature, the affect sampler's, and `arbitrate`'s
no-active-drive fallback — the path an ametabolic creature always takes.

**The fold hoist** (ledger #8, found by review, sized by measurement):
`Thirst` and `Hunger` re-ran an identical full search per candidate
action per fold pass under an unchanged view — ~25 identical searches
per thirsty creature-tick, 28 % of all searches in the health battery.
The fix computes each view-invariant affordance once per `arbitrate`
call, lazily, in a caller-owned slot. Water searches: **1 632 → 130**.

**The solver seam**: `Solver` over the existing `SearchSpace` — A*
verbatim behind the trait (the delegation pinned by test), the mesh memo
threaded to the one boundary that cannot leak into edge costs
(`RoomAddr::neighbors`, isolated by a shared `edges_from`), and a second
live implementation to prove substitutability is real.

**The bench**: `nav_bench`, four backends × six rungs, ten subjects to
one million, on a real seed-42 mesh. The numbers that matter: the memo
backend flat at ~36 µs/query across six orders of magnitude; the cached
mix at **~13 µs/query at N = 1 000 000** — a million walkers' worth of
navigation in ~13 s of single-core time, memory peaking at 229 MB. The
marginal-cost story Nathan asked for, measured (the spec's N=10-vs-30
probe ships as a strict subset of this ladder).

## The nulls, kept

Two predictions died honestly. The **shared reverse field** — one
Dijkstra from the common home serving every settled creature — was
disproven by its own gating property test: 52 of 346 rooms diverge on
`first_step` because A*'s tie-break is root-relative, and a home-rooted
field cannot reproduce every start-rooted search's choices. The field
ships disabled, its builder surviving as the solver seam's second
implementation, where the bench then quantified the second half of the
null: at 35–55× the per-query cost, its amortization cannot even be
expressed through a single-start solve signature. And Task 4's
**"tick()'s second walk is cache-blind" hypothesis** was refuted by the
close-out flamegraph — both walks reach the memo about equally; the
remainder is the room-family searches themselves (32 %) and the
`RoomAddr` comparison churn (~7.7 %), which is now a specced follow-up
with a falsifiable first claim: `RoomId` (a packed `u64`) exists, and
whether its numeric order is isomorphic to `RoomAddr`'s lexicographic
`Ord` is the question a packed-address campaign must answer first.

## The numbers

Health null-control 60.7 → **47.5 s** (the < 45 s target missed by two
and a half seconds, shipped un-retuned with the flamegraph that names
the remainder); heat-hastens-thirst 48.3 → **4.7 s**; the sky-follows
walk 32.9 → **24.5 s**; the synthetic band test 53.8 → **21.5 s**; the
breadth sweep 280 → 167 s in its heavy tier. The absorb mid-campaign
took The Watershed's sonority epoch cleanly — zero conflicts, and the
sim goldens rode through unmoved, which is itself evidence: drawn names
do not feed navigation. Byte-identity was proven the strong way at
close: world and possession transcripts regenerated from `origin/main`'s
own binary, compared against ours, identical.

One process scar worth its chronicle line: a constraint was silently
traded mid-campaign (a `RefCell` where the plan said caller-owned) and
reported as "no concerns" — the review caught it, the exception was
denied, and the reshape kept the laziness without the interior
mutability. The correction that followed was symmetrical: the
controller's own mandated every-hit assertion turned out to cancel the
optimization under the gate's build profile, and the implementer's
honest measurement caught *that*. The red-run discipline ran in both
directions this campaign.
