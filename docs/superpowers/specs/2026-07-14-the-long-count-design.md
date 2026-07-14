# The Long Count — Design

**Date:** 2026-07-14
**Status:** Approved (brainstorming session)
**Campaign:** The Long Count (slug-named per decision 0026)
**Provenance:** The close-out of the astronomy residuals — the four leftovers
on shipped registry rows identified in the 2026-07-08 sky audit and its
successors: SKY-6's undated eclipses (folding in SKY-eclipse-seasons' nodal
half), SKY-1's ageless star, SKY-stale-alignments' ground half, and SKY-23's
thin star/anchor/neighbor verification. Everything here *finishes shipped
machinery*; nothing opens a new regime (SKY-16 binaries, SKY-moon-world, and
kin stay open rows). The name is the theme: eclipse dating, alignment dating,
and stellar aging are all the sky working as a long clock.

> **Scope amendment (2026-07-14, post-approval).** A parallel session
> approved *Eclipse Seasons*
> (`2026-07-14-eclipse-seasons-design.md`, committed @83efb1f, thirteen
> minutes after this spec) claiming the same `moon-nodes` stream, nodal
> regression, and dated solar/lunar event core — and going deeper there
> (positional moon ephemeris, shadow ground tracks, the saros recurrence
> ladder) while deferring standstills. Nathan adjudicated the collision the
> same day: **Eclipse Seasons owns the eclipse core.** The Long Count
> re-scopes to the non-overlapping residuals — secular brightening (§4, with
> the §5 migration decline), the alignment ground half (§6), and the SKY-23
> batteries (§7, minus the node/eclipse rows) — plus their §10 metrics and
> almanac surfacing. The standstill facts move to a follow-up riding Eclipse
> Seasons' explicitly-deferred standstills seam. §3 is retained below for
> the record but **does not govern**; this campaign draws no new stream.

---

## 1. Goal

Four residuals, one campaign, four tasks:

1. **The eclipse package (SKY-6 close-out + SKY-eclipse-seasons).** Eclipses
   today are a *rate* — `period_days = synodic / chance` — never a date. Each
   moon gains a drawn ascending-node longitude; nodal regression is *derived*
   from elements already held; dated solar **and lunar** eclipse occurrences
   are computed by scanning syzygies. Eclipse seasons, the eclipse year, and
   the standstill cycle (the 18.6-year analog) fall out of the same geometry.
2. **Secular stellar brightening (SKY-1 close-out).** The star's luminosity
   becomes a slow, derived, draw-free function of `WorldTime`, anchored so the
   present is untouched. Semi-major-axis migration — the row's other half —
   is **declined with reason** (§5) and the row retires.
3. **Stale alignments, ground half (SKY-stale-alignments).** The sky half
   shipped with the night-sky instrument (`star_equatorial_at` reads
   precession). This ships the ground half: solstice-rise azimuth as a
   function of time (the drift *is* the obliquity wobble the forcing already
   models), a dating inverse, and a `founding-solstice-azimuth` fact per
   settlement committed by the composition root.
4. **The verification batteries (SKY-23 close-out).** Star, anchor, and
   neighbors get invariant batteries "on the scale moons enjoy," and every
   piece of new machinery above ships with its own battery from day one.

Consumers land in all three registers (per the brainstorm decision): the
instrument (a dated eclipse table and alignment lines in the almanac),
religion/culture hooks (a lunar-eclipse phenomenon, standstill-period and
node facts), and lab ground truth (cadence, totality, standstill, brightening
and drift-rate metrics over the census).

## 2. Design principles

1. **One draw; everything else derives.** The campaign's single new random
   quantity is the per-moon node longitude Ω₀ — an initial condition with no
   physical prior, exactly what a draw is for. Regression period, eclipse
   dates, seasons, standstills, brightening rate, and azimuth drift are all
   pure functions of quantities already held. No guesses wearing physics'
   clothes: where the model approximates, the model card says so.
2. **The migration guard is the labeled-stream property.** The node draw
   lives on its own stream (`moon-nodes`), derived independently from the
   seed, consumed after every existing draw — star mass, orbit, moons,
   inclinations, forcing, wanderers, starfield all consume exactly what they
   consume today, byte-identically. A battery enforces it (§7).
3. **Anchored at the present.** `luminosity_at(0) = L₀` exactly; the node
   position at `t = 0` is the genesis draw; solstice azimuth at `t = 0` is
   the genesis obliquity's. What changes at day 0 is only what is *added*
   (new facts, new almanac sections) — the single re-baseline absorbs it.
4. **Tier 0 keeps its whole claim.** Nothing here rivals the sun: eclipse
   events are transient structure beneath it, and `tier_refinement.rs` gains
   an assertion saying so.
5. **Consumers stay cause-blind.** Religion reads an eclipse phenomenon, not
   a node longitude; the almanac reads dated events, not the scan that found
   them.

## 3. Task 1 — The eclipse package

### 3.1 The node draw

`Moon` gains `node_longitude_deg: f64` — ecliptic longitude of the ascending
node at genesis, drawn uniform `[0, 360)` from a new stream:

```text
streams::MOON_NODES = "moon-nodes"
```

one draw per admitted moon, in distance-sorted order, mirroring the
`moon-inclinations` pattern (drawn after admission and the distance sort, so
every pre-node draw is byte-identical to the pre-campaign path). Committed as
a `moon-node-longitude-degrees` fact per moon (quantized at commit). No new
pin: the node is unpinnable this campaign — none of the shipped sky pins
constrain it, so pin isolation holds by construction, and the battery checks
it anyway.

### 3.2 Derived node motion

Nodal regression period, from the standard lunar-theory leading term
(declared approximation, model card):

```text
P_node = (4/3) · Y² / (P_sid · cos i)        [standard days]
Ω(t)   = Ω₀ − 360 · t / P_node               [degrees, regresses westward]
```

Earth check: Y = 365.25 d, P_sid = 27.32 d, i = 5.14° gives P_node ≈ 17.9 yr
against the true 18.61 yr — the right magnitude and scaling, honestly carded.
`P_node` **is** the lunar-standstill period; it is committed per moon as a
`standstill-period-days` fact — the monument-scale ritual beat, and the
number MAP-18 calendrics will one day score cultures against.

### 3.3 The moon's ecliptic latitude, and dated events

The moon's ecliptic longitude reuses shipped phase machinery — at any `t`,
`L_moon = L_sun + 360 · moon_phase(t, index)` (exact at the syzygies, which
is where it is evaluated; declared approximation elsewhere). Its ecliptic
latitude is the inclined-orbit small-angle form:

```text
β(t) = i · sin(L_moon(t) − Ω(t))             [degrees]
```

A new module `domains/astronomy/src/eclipses.rs` (the `heliacal.rs`
precedent: derived events, computed on demand, never stored) exposes:

```rust
pub struct EclipseEvent {
    pub day: StdDays,        // the syzygy
    pub moon: usize,         // distance-sorted index
    pub body: EclipseBody,   // Solar | Lunar
    pub kind: EclipseKind,   // Total | Annular (solar); Total (lunar)
}
pub fn eclipse_events(cal: &Calendar, system: &StarSystem, from: StdDays, until: StdDays) -> Vec<EclipseEvent>
```

Syzygies are closed-form (the synodic phase is linear in `t`), so the scan
visits each new and full moon exactly, no sampling:

- **Solar**, at each new moon: event iff `|β| < θ_solar`, where `θ_solar` is
  the *shipped* threshold from `eclipse_chance` (half-sum of discs +
  `ECLIPSE_PARALLAX_DEG`), unchanged. Kind by the shipped disc rule: total if
  the moon's disc covers the sun's, else annular.
- **Lunar**, at each full moon: event iff `|β| < θ_lunar`, with
  `θ_lunar = SHADOW_FACTOR · θ_solar`. `SHADOW_FACTOR` is a declared
  approximation calibrated on Luna–Sol to ~1.5 umbral lunar eclipses/year
  (the value is tuned during implementation and carded). Lunar kind is
  `Total` only this campaign; partiality grading for both bodies is deferred.

Eclipse seasons and the eclipse year `Y·P_node / (Y + P_node)` (Earth:
346.6 d) are consequences of the geometry, not code; the lab measures them.

### 3.4 Surfacing

- **Phenomena:** the shipped rate-based solar-eclipse phenomenon is
  untouched. One new phenomenon per eclipsing moon: kind `eclipse` in
  `Venue::NightSky`, "the full moon darkens to a bloodred coal", salience
  0.8, `period_days = synodic / chance_lunar` (the same statistical form the
  solar phenomenon uses, with the wider threshold). Religion takes the omen
  or doesn't — consumers stay cause-blind.
- **Facts:** `moon-node-longitude-degrees`, `standstill-period-days` per
  moon (predicates registered by astronomy; naming per the concept-registry
  conventions).
- **Almanac:** a new "reckoning of eclipses" section — every dated event in
  the next two years (capped), with day, moon, body, and kind. A world whose
  moons never eclipse says so in one line.
- **Consistency guard:** a battery asserts the dated scan's empirical
  cadence over a long window agrees with the phenomenon's statistical
  `period_days` within tolerance — the rate model and the date model are two
  views of one geometry and must not drift apart.

## 4. Task 2 — Secular stellar brightening

`star.rs` gains (both `type-audit`-tagged, model-carded):

```text
b = 0.10 · M^2.5                              [fractional brightening per Gyr]
luminosity_at(star, t) = L₀ · (1 + b · t / GYR_DAYS)
insolation_rel_at(star, anchor, t) = luminosity_at(star, t) / a²
```

Sol-calibrated (~10%/Gyr) and mass-scaled by the main-sequence lifetime
(t_MS ∝ M⁻²·⁵ — heavier stars age faster). Draw-free: "mass drawn,
everything else derived" stays the star's whole story. Anchored:
`luminosity_at(star, 0) == star.luminosity` exactly, so no present-day byte
moves except what is added.

- The habitable zone remains a genesis-epoch derivation from L₀ — documented
  where `HabitableZone` is derived. On the kiloyear scales the sim inhabits,
  brightening is honestly negligible (10⁻⁵ per 100 kyr); it exists so deep
  time has the *right* slope to read, not to move the present.
- **Fact:** `brightening-per-gyr` on the star entity.
- **Almanac:** one deep-time line in the sky section (the sun's slow
  brightening, stated at the Gyr scale it lives on).
- **Lorenz guard-rail:** untouched — `luminosity_at` is a pure function of
  the lossless genesis state, never integrated forward.

## 5. Migration: declined with reason

SKY-1's "semi-major-axis migration" half is **declined**: disk-driven
migration ends before a stable habitable world exists, and post-genesis
secular drift of an isolated planet's semi-major axis is effectively zero on
every timescale the sim can express. Adding it would be invention, not
derivation — a guess wearing physics' clothes, the exact failure mode SKY-3
criticizes. Tidal orbital decay is real physics but belongs to
SKY-tidal-braking (an open row with its own arc: day-lengthening, the moon's
outward spiral, the death of total eclipses) and is out of scope here. A
decision-log entry records the decline so the registry row can retire
without the question silently reopening.

## 6. Task 3 — Stale alignments, ground half

`calendar.rs` gains three derived functions (all `None`/empty on locked
worlds, which have no sunrise):

```text
solstice_rise_azimuth_at(latitude, t) -> Option<f64>
    cos(az) = sin(ε(t)) / cos(φ)     — summer-solstice sunrise azimuth,
                                       degrees clockwise from north;
                                       None where |sin ε / cos φ| > 1
                                       (polar day/night) or the world is
                                       locked. Refraction and horizon dip
                                       ignored (declared approximation).
alignment_drift_deg(latitude, t0, t1) -> Option<f64>
    azimuth(t1) − azimuth(t0)        — how far a sightline cut at t0 is off
                                       by t1. The drift IS the obliquity
                                       wobble: bounded by obliquity_amp,
                                       periodic on P_OBLIQUITY.
alignment_epoch_of(azimuth_deg, latitude, t_now) -> Option<StdDays>
    the dating inverse: the most recent epoch at or before t_now whose
    solstice azimuth matches. Multi-valued over deep time (the wobble
    oscillates) — nearest-past is the contract, documented; None when the
    azimuth is unreachable at that latitude.
```

The stellar half needs no new code: which *stars* rise on a founding
sightline already drifts via the shipped precession machinery, and
`precession_offset_deg` is the dating clock for that half.

**Founding facts.** The composition root (`windows/worldgen`) — the one
place domains meet — reads each settlement's committed latitude, evaluates
the azimuth at the founding epoch (genesis, day 0), and commits a
`founding-solstice-azimuth-degrees` fact per settlement (quantized;
predicate registered by astronomy; skipped where the function returns
`None`). Real entities, no new entity kind — the monument entity stays an
open idea.

**Almanac:** the flagship settlement's sightline and its drift *rate*
("drifts a finger's width in N centuries") — at day 0 the accumulated drift
is honestly zero, so the rate is the story the instrument tells.

## 7. Task 4 — The verification batteries (SKY-23 close-out)

Extends `domains/astronomy/tests/genesis_properties.rs` (and the
pin-isolation tests already there), at the moons' existing scale — a few
hundred seeds, genesis-only, fast-tier (no full world builds; no `heavy:`
additions):

- **Star battery:** mass in [0.6, 1.4] always; luminosity, radius-derived
  angular diameter, and HZ bounds monotone in mass; class boundaries exact;
  brightening rate positive, finite, anchored at `t = 0`.
- **Anchor battery:** orbit inside the HZ always; obliquity in range; every
  rotation regime reachable across the sweep; locked worlds never have a
  solar hour; year consistent with the orbit (the Kepler relation the domain
  derives).
- **Neighbors battery:** count in 2–5; positions, distances, and classes in
  range; regeneration byte-identical.
- **Node/save-format battery:** node longitudes in [0, 360); the draw
  consumes nothing before existing streams — every pre-campaign draw
  byte-identical with and without the new stream consumed (§2, principle 2);
  pinned worlds (`--moons`, `--sky`, …) draw nodes exactly as unpinned ones.
- **Eclipse battery:** every dated solar event falls at a new-moon phase and
  every lunar at full (to solver tolerance); `|β|` inside its threshold at
  every event; a Luna–Sol configuration reproduces ~2.4 solar and ~1.5
  lunar eclipses/year; the scan's cadence matches the phenomenon's
  statistical period (§3.4); a flat orbit eclipses every syzygy.
- **Alignment battery:** azimuth mirror-symmetric across the equator; drift
  bounded by `2 · obliquity_amp`'s azimuthal image; the dating inverse
  round-trips (`alignment_epoch_of(azimuth_at(t), lat, now) ≈ t` for `t` in
  the nearest half-cycle).
- **Cross-tier:** `tier_refinement.rs` untouched except one added assertion:
  eclipse events never outrank the sun (salience < 1.0 in every regime).

## 8. Composition root and dataflow

`windows/worldgen` changes are additive: node draws happen inside
`generate_moons`' successor path in `domains/astronomy` (the domain, not the
root); the root gains only (a) committing the new astronomy facts it already
commits siblings of, and (b) the founding-alignment fact pass after
settlement placement (§6). No domain learns of another; the almanac window
reads the new derived functions through the provider surface it already
holds. Adding all of this edits no existing domain other than astronomy.

## 9. Save format, determinism, and the re-baseline

- **New stream label:** `moon-nodes` (permanent contract), published via
  `stream_labels()`; the streams manifest artifact regenerates.
- **New predicates:** `moon-node-longitude-degrees`,
  `standstill-period-days`, `brightening-per-gyr`,
  `founding-solstice-azimuth-degrees` — registered, contradiction-checked,
  quantized at commit like every serialized float.
- **No epoch bumps, no formula changes:** every shipped constant, stream,
  and physics formula is untouched; the eclipse threshold reuses the shipped
  `eclipse_chance` geometry.
- **Model card additions (Campaign 2 spec §5):** nodal regression
  (approximated), the syzygy-longitude form (approximated), `SHADOW_FACTOR`
  (approximated, Luna–Sol-calibrated), brightening rate (approximated,
  Sol-calibrated), solstice azimuth (derived; refraction ignored).
- **Re-baseline:** the three seed-42 almanacs, registry/manifest dumps, lab
  studies, and `golden_seed_42.rs` pins re-pinned in the drifting commit
  (standing practice), locally with `SKIP_CENSUS=1`. **Census fixtures
  drift** (new facts change the ledger): one AWS regen
  (`make regen-remote`), pre-merge, with warning to Nathan first — the
  standing census rule.
- **Type audit:** every new pub-boundary primitive tagged
  (`bare-ok`/`pending(wave-N)`); the audit report regenerates.

## 10. The Lab

New metrics (code, per decision 0011), joining the census study:
`eclipse-cadence-solar` and `-lunar` (events/year over a long scan window),
`totality-fraction` (total vs annular among solar events),
`standstill-period-days` (innermost eclipsing moon), `brightening-per-gyr`,
and `alignment-drift-rate` (deg/kyr at the flagship settlement's latitude).
All ride the astronomy rung of the build-depth ladder except
`alignment-drift-rate` (settlements rung). Census columns change ⇒
`schema.json` bumps with the same AWS regen.

## 11. The book

Definition of Done, beyond the merge:

- **Chronicle entry** for The Long Count.
- **Registry re-scores:** SKY-6 → fully shipped (residual language removed);
  SKY-eclipse-seasons → folded into SKY-6, shipped; SKY-1 → shipped, with
  the migration decline noted against the decision; SKY-stale-alignments →
  shipped (both halves); SKY-23 → shipped. SKY-tidal-braking,
  SKY-wanderer-calendar, and kin stay open and gain "enabled-by" notes where
  this campaign feeds them.
- **Freshness sweep** of the sky chapters (the almanac gallery pages show
  the new sections); Confidence Gradient re-scored if any bet moved.
- **Decision-log entry:** the migration decline (§5).
- **Retrospective** (decision 0020) at close.

## 12. Success criteria

1. `hornvale almanac` on seed 42 prints a dated eclipse table; the dates are
   byte-identical across platforms and regenerations.
2. Every pre-campaign draw is byte-identical to before (battery-enforced);
   a pre-campaign world JSON reloads and re-derives cleanly.
3. Luna–Sol numbers reproduce ~2.4 solar and ~1.5 lunar eclipses/year; the
   dated cadence agrees with the rate phenomenon.
4. `standstill-period-days`, node, brightening, and founding-alignment facts
   appear in the seed-42 ledger, quantized.
5. The star/anchor/neighbor batteries run in the fast tier and pass over
   their seed sweeps; `make gate` stays near its ~4-minute budget.
6. The census regenerates once on AWS with the new columns; the book and
   registry updates land with the merge.

## 13. Explicitly deferred

- **Partiality grading** for solar and lunar eclipses (binary in/out this
  campaign, matching the shipped chance model's granularity).
- **Saros extraction as a first-class fact** — the lab measures recurrence
  quality; naming and committing saros series is a culture-layer question.
- **Monument entities** and any new world furniture (the alignment facts
  ride settlements).
- **Path-of-totality geography** — events are dated, not localized beyond
  the shipped "somewhere on the world" parallax allowance.
- **Tidal braking / lunar recession** (SKY-tidal-braking, its own row).
- **Apsidal precession** and any orbital-element motion beyond the shipped
  Milankovitch triad plus nodal regression.
- **M dwarfs / stellar variability** (SKY-13) and everything structural
  (SKY-16 binaries, SKY-moon-world, SKY-galactic-situation).
