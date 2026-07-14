# Eclipse Seasons — Design

**Date:** 2026-07-14
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-05-campaign-2-the-sky-design.md` (§5 model card
governs; Constitution §2 binds)
**Registry rows:** ships `SKY-eclipse-seasons`; refines `SKY-6`; leaves named
seams for `SKY-transits`, `SKY-tidal-braking`, and a new
standstills-with-prerequisite-shipped row.
**Provenance:** Brainstormed 2026-07-14 from the SKY-eclipse-seasons registry
row, whose opening claim ("moons have no orbital inclination") went stale when
SKY-6 shipped; the surviving core — eclipses as *events in time* rather than a
statistical rate — is this campaign. An ideonomy pass (timeline organon;
cross-domain re-instantiation) pulled in three additions: lunar eclipses, the
event-time total/annular decision, and the recurrence ladder as readings.

> **Reconciliation (2026-07-14, same day).** A parallel session approved *The
> Long Count* (`2026-07-14-the-long-count-design.md`, committed @e4f7570,
> thirteen minutes before this spec) claiming the same `moon-nodes` stream,
> nodal regression, and dated solar/lunar event core. Nathan adjudicated:
> **Eclipse Seasons owns the eclipse core**; The Long Count re-scoped to
> secular brightening, the alignment ground half, and the SKY-23 batteries
> (see its matching scope amendment). This spec adopts the Long Count plan's
> naming for the shared surface so its written eclipse tasks transfer
> verbatim: the module is `eclipses.rs` (plural, the `moons.rs` convention)
> and the drawn field is `Moon.node_longitude_deg`. Standstill *facts* ride
> this campaign only as the committed `moon-node-period-days` beat; the
> standstill instrument stays the deferred follow-up row.

---

## 1. Goal

SKY-6 shipped eclipses as a *rate*: `eclipse_chance` (provider.rs) dilutes the
synodic month into a mean recurrence interval, and the phenomenon's
`period_days` is `synodic / chance`. Nothing can answer "is there an eclipse
on day t?" or "where does its shadow fall?" — the moon has an inclination
*magnitude* but no node *position*, so eclipses are uniformly smeared through
the year instead of clustering into the real structure: two eclipse seasons
per eclipse year, drifting through the civil calendar as the node regresses.

This campaign turns the rate into events:

1. **One new draw** — each moon's ascending-node longitude.
2. **Derived node motion** — first-order solar-perturbation regression.
3. **Closed-form event enumeration** — solar eclipses at conjunctions, lunar
   eclipses at oppositions; no scanning, no search.
4. **A positional moon ephemeris** — ecliptic (λ, β) as functions of
   `WorldTime`; the moon has a place in the sky for the first time.
5. **Ground tracks** — where the shadow falls, so eclipse rarity-where-you-
   stand becomes true from held quantities.
6. **The recurrence ladder as readings** — season parade, saros, exeligmos,
   series birth/death, multi-moon coincidences — all falling out of the
   enumeration, none of it new machinery.

The shipped statistical phenomenon is untouched and becomes the coarse tier;
a guard test binds the fine tier to it (coarse constrains fine).

**Deliberately out of scope:** standstills (the 18.6-year moonrise-azimuth
extreme) — deferred to a follow-up registry row whose hard prerequisite (the
positional ephemeris) this campaign ships. The deep-time secular arc
(SKY-tidal-braking) stays out, but §3's evaluate-at-t rule is the seam it
later slides under without an epoch.

## 2. Architecture and data flow

One new module, one new field, no layering changes.

```
streams.rs                 moons.rs                      eclipses.rs (NEW)
MOON_NODES ── drawn ──▶ Moon.node_longitude_deg ──▶ node_at(t), regression rate
(appended after         (0–360°, drawn          moon_ecliptic(t) → (λ, β)
 MOON_INCLINATIONS)      post-admission)        conjunctions t_k   — SOLAR
                                                oppositions t_k+s/2 — LUNAR
calendar.rs ── synodic months ─────────────▶    eclipse_at(t) → Event
forcing.rs ── eccentricity(t) → sun size ──▶    ground_track(Event)
                                                next_eclipse(t)
                                                readings: parade / saros /
                                                exeligmos / series / overlaps
                                                     │
        ┌──────────────┬──────────────────┬──────────┴──────┬──────────────┐
        facts.rs       provider.rs        windows/almanac   windows/lab
        node + regr.   placed, timed      Eclipses section  census metrics
        facts per moon prose (devoured /  (next events +
                       bitten / blood     recurrence ladder)
                       moon)
```

- `eclipses.rs` holds every derived quantity. It reads `moons`, `calendar`,
  and `forcing`; it is read by the four consumers, none of which learns
  anything about the others (phenomena protocol).
- The `Moon` struct gains `node_longitude_deg: f64` (degrees, matching
  `inclination_deg`'s convention, `type-audit: pending(wave-1)`). When the
  kernel's Angle family lands (the datum retrospective's named next step),
  both fields convert together — this campaign does not pioneer `Angle`.
- The shipped coarse tier (`eclipse_chance` → rate phenomenon) stays
  byte-identical.

## 3. Physics model card

Every quantity classified derived / approximated / drawn, with Luna check
values (Earth–Luna–Sol inputs) where one exists. One rule binds all of them:
**angular sizes and rates are evaluated as functions of t at the event, never
cached** — the seam SKY-tidal-braking later slides under.

| Quantity | Class | Formula / source | Luna check |
|---|---|---|---|
| Node longitude | drawn | uniform [0, 360)°, stream `moon-nodes` | — |
| Nodal regression rate | approximated | Ω̇ = −(3/4)·(n_s²/n_m)·cos(i); n_s from year length, n_m from sidereal period | node period ≈ 18.6 yr (few-percent tolerance, documented) |
| Moon ecliptic longitude | approximated | λ_m = λ_sun(t) + 360°·synodic phase (uniform circular motion; moons carry no eccentricity, declared) | — |
| Moon ecliptic latitude | approximated | β = i·sin(λ_m − Ω(t)) | — |
| Event candidates | derived | solar at conjunctions t_k = t_0 + k·synodic; lunar at oppositions t_k + synodic/2; exact enumeration | — |
| Solar eclipse condition | derived | \|β\| < (sun_ang(t) + moon_ang)/2 + `ECLIPSE_PARALLAX_DEG` (the shipped threshold, sun size now read at t) | — |
| Total vs annular | derived | total iff moon_ang ≥ sun_ang(t) at the event's own t; borderline moons are hybrid across the year | Sol's angular size varies ±1.7% with e = 0.0167 |
| Lunar eclipse condition | approximated | \|β\| < (umbra_ang + moon_ang)/2; umbra from shadow-cone geometry with anchor radius ∝ mass^(1/3) at Earth-like density (declared) | umbra at Luna's distance ≈ 1.4°, ≈ 2.6 lunar radii |
| Ground track | approximated | shadow latitude ≈ solar declination(t) + β projected to surface; longitude from rotation angle swept over the event duration (duration from relative angular rates). A band-with-a-path, not umbral cartography; documented precision, heliacal-style | — |
| Draconic month | derived | from synodic and node rates | 27.212 d |
| Saros | derived | best rational approximation of synodic : draconic | 223 synodic ≈ 242 draconic ≈ 6585.3 d |
| Exeligmos | derived | 3 saros (longitude closure: each saros shifts the track ≈ 120° west) | ≈ 54 yr 33 d |
| Series lifetime | derived | per-return β residue drifting across the threshold width | order 1,200–1,500 yr |
| Season parade rate | derived | civil year − eclipse year | ≈ 19 d/yr |
| Multi-moon coincidences | derived | season-window overlap across moons | — |

## 4. Determinism and save format

- **One new stream:** `streams::MOON_NODES = "moon-nodes"`, published via
  `stream_labels()` into the generated manifest. Drawn per admitted moon
  *after* the inclination draws (the SKY-6 append pattern), so every
  pre-existing draw — count, masses, distances, admission redraws, sort,
  inclinations — stays byte-identical. No epoch bump: pure addition; nothing
  drawn becomes derived.
- **Pin isolation:** the pinned-moons path consumes node draws identically to
  the unpinned path (asserted in `genesis_properties.rs`).
- **Nothing new is serialized** except the one drawn field. All derived
  quantities are pure functions of (system, calendar, forcing, t) through
  kernel `math` only — no platform transcendentals, no wall clock, no
  `HashMap`. Quantization stays at the emit boundaries. The Lorenz guard-rail
  does not bite: no chaotic integrator; everything re-derives closed-form
  from the lossless seed.
- **Blast radius:** pre-campaign worlds re-derive identically (the new field
  draws from a fresh stream, consuming nothing old). Committed artifacts
  change only where consumers add content: three seed-42 almanacs,
  concepts/streams dumps (one local `SKIP_CENSUS=1` rebaseline), and the
  census schema (pre-merge AWS regen, with warning to Nathan first).

## 5. Consumers

- **Ledger facts** (`facts.rs` genesis, the SKY-15 pattern): per moon,
  `moon-node-longitude-degrees` (the draw) and `moon-node-period-days` (the
  derived regression period), committed quantized; predicates registered in
  the concept registry under the domain's naming conventions.
- **Phenomena** (`provider.rs`): the solar rate phenomenon is untouched. It
  gains a sibling — a coarse lunar-eclipse phenomenon (venue NightSky,
  period = synodic / lunar chance, reusing `eclipse_chance` with the umbral
  threshold; "the moon drowns in shadow and rises ember-red"). Salience sits
  below solar-total; the "nothing rivals the sun" invariant holds.
- **Placed, timed prose** (possess/day-sky path): on a solar event day —
  inside the track band, "devoured whole" / "the burning ring"; elsewhere on
  the day side, "the sun is bitten" (the partial). On a lunar event night the
  entire night hemisphere gets the blood moon — no band, which is the
  experiential asymmetry that makes lunar eclipses the common omen. The
  vessel consumes all of this through the existing observation seam: no
  `windows/vessel` source changes.
- **Census metrics** (`windows/lab`, metrics-are-code, decision 0011): four —
  `eclipse-year-days`, `solar-eclipses-per-century`,
  `lunar-eclipses-per-century`, `coincidence-days-per-century` (zero for
  0–1-moon worlds). Schema churn lands at the pre-merge AWS regen.
- **Almanac** (`windows/almanac`): a new Eclipses section — next solar (date,
  kind, band latitude) and next lunar per moon, the current season window,
  and one line each for the parade rate, the saros → exeligmos ladder, and
  the active series' drift.

## 6. Testing

- **Check values** (the `matches_the_luna_check_value` style): node period
  ≈ 18.6 yr; eclipse year ≈ 346.6 d; saros ≈ 6585.3 d; umbra ≈ 2.6 lunar
  radii; parade ≈ 19 d/yr; series lifetime order 10³ yr. One per §3 formula.
- **Property battery** (`genesis_properties.rs`): every admitted moon draws a
  node in [0, 360); node stream pin-isolated; all pre-existing draws
  byte-identical to the pre-campaign format across a seed sweep.
- **The coherence guard** (the campaign's constitutional test): enumerate a
  few centuries of events per seed; the long-run solar event rate must
  reproduce `synodic / eclipse_chance` within a documented tolerance;
  likewise lunar. Coarse constrains fine, mechanically. Tier placement
  (normal vs `heavy:`) decided by measuring runtime, not guessed.
- **Event-shape invariants:** solar events only at conjunctions, lunar only
  at oppositions; events cluster into ≤ 2 season windows per eclipse year;
  every total-kind event has moon_ang ≥ sun_ang(t) at its own t; lunar
  events are visible from the whole night hemisphere while solar visibility
  is band-gated.
- **Determinism:** same seed → byte-identical almanacs including the new
  section; no new clippy exceptions.

## 7. Close (Definition of Done, beyond `make gate`)

- Chronicle entry; campaign retrospective (decision 0020).
- Registry sweep: `SKY-eclipse-seasons` → shipped, with its stale first
  clause corrected in passing; `SKY-6` row gains the refinement note; new
  row for standstills (prerequisite now shipped); `SKY-transits` and
  `SKY-tidal-braking` rows updated to name the seams this campaign leaves
  them.
- Confidence Gradient re-score if a bet moves (decision 0030).
- Artifact rebaseline as §4; census regen warned before the merge to main.
