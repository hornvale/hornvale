# The Night Sky Instrument — Design

**Date:** 2026-07-13
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-05-campaign-2-the-sky-design.md` (§5 deficit audit). The Constitution's determinism rules and the astronomy model card govern every formula this spec commits.
**Provenance:** Five registry rows — SKY-9 (wanderers), SKY-12 (constellations), SKY-seasonal-night-sky, SKY-heliacal-risings, SKY-circumpolar — form one coherent cluster: the fixed-star sky becomes a calendar and navigation instrument, the third naked-eye calendar family (stellar, beside the shipped solar and lunar), feeding MAP-18 calendrics. An ideonomy pass (combination × tree-finding operators, spectrum organon, materiality/symmetry/scope prompts) reshaped the design in six recorded ways (§6): the observer-dependence spectrum became the trace-protocol law (§5), the morning/evening-star duality and the ecliptic figure flag were added, heliacal visibility was pinned to SKY-17's twilight machinery, the rotation-regime × obliquity test matrix became explicit, and two deferred registry rows were identified.

**Known parallel campaign:** `2026-07-13-the-self-describing-sky-design.md` (SKY-15) also touches `domains/astronomy/src/facts.rs` and commits per-neighbor facts. No semantic conflict — that campaign commits drawn values, this one derives events and views from them — but every stage boundary of this campaign absorbs main per house process, and the facts-file merge is the expected collision point.

---

## 1. Goal

Make the night sky an instrument: something a placed observer can navigate by, date the year with, and eventually build calendars on.

1. **Stage 1 — The Placed Sky.** Give the sun a place among the fixed stars; derive the seasonal night sky, heliacal risings, and the circumpolar/pole-star verdict at an observer latitude. Zero new draws.
2. **Stage 2 — The Wanderers.** Sibling planets as real `StarSystem` members with Kepler orbits: elongation-bound inner wanderers (with the morning-star/evening-star duality), retrograde-looping outer ones. One new pin: `--wanderers N`.
3. **Stage 3 — The Figured Sky.** A derived background starfield and deterministic figure-grouping, descriptively identified, with the ecliptic band flagged.

**Non-goals** (named to bound the campaign): proper names for figures or wanderers (names belong to observers — the culture/language layer names the sky when MAP-3/MAP-18 arrive); the wanderer-synodic calendar and wanderer transits/occultations (deferred registry rows, §8); orrery/scene rendering of wanderers (ORRERY-sibling-planets stays open — this campaign builds the bodies, not the lens); any physical effect of wanderers on the anchor (declared approximation, like moon masses); lunar/planetary occultation machinery.

## 2. Stage 1 — The Placed Sky (pure derivation)

The enabling computation: **the sun's celestial position**. Solar ecliptic longitude λ is the year phase the calendar already computes; with obliquity ε (shipped, time-varying via forcing), the sun's equatorial coordinates are

```
alpha = atan2(sin(lambda) * cos(epsilon), cos(lambda))   # right ascension
delta = asin(sin(lambda) * sin(epsilon))                 # declination
```

consistent with the neighbor convention (celestial equator = the anchor's rotational equator, spec §3). Lives beside the shipped solar-geometry functions in `calendar.rs`; draws nothing.

On top of it, three latitude-parameterized derivations (the placed observer, SEQ-5/SKY-8, finally pays off at night):

- **Seasonal visibility** — a derived view (§5): which neighbors are visible in tonight's sky at (latitude, t). A star is night-visible when it is above the horizon at some moment while the sun is below the twilight threshold; the dominant term is its RA offset from the sun's RA, so the answer swings with year phase — winter stars and summer stars. Returned by a function, committed nowhere.
- **Heliacal risings** — dated **phenomena** (§5): for each neighbor visible from the latitude, the first day of the year on which it rises far enough ahead of the sun to be glimpsed in morning twilight. The visibility threshold **reuses SKY-17's twilight machinery** (the sun's altitude bands that already color the sky) rather than inventing a bare altitude cut; brighter classes surface earlier in brighter twilight (a per-class arcus-visionis table in the model card). The almanac gains a stellar-calendar section: each bright star's rising date, the year's sharpest annual events.
- **Circumpolar verdict** — at latitude φ, stars with |dec| > 90° − φ (same-sign hemisphere) never set; their mirror set never rises. Hemispheres see genuinely different skies. **Pole star:** when a neighbor sits within a model-card threshold of either celestial pole, commit a fact (`pole-star-north` / `pole-star-south`, value = separation in degrees) — world-side, a property of the drawn declinations. On a retrograde world (SKY-22) the night prose notes the sky wheels the other way about the pole.

## 3. Stage 2 — The Wanderers (new draws, appended)

**Streams** (appended to `streams.rs`, permanent contracts; no existing draw moves):

```
WANDERER_COUNT = "wanderer-count"   # count draw: 0..=4
WANDERERS      = "wanderers"        # per-wanderer params, sequential
```

**Draws per wanderer:** semi-major axis (log-uniform over an inner band and an outer band around the anchor's orbit, excluding a Hill-flavored neighborhood of the anchor — exact bands in the model card), size class (rock / giant), and albedo. **Derived, never drawn:** orbital period from Kepler's third law with the star's drawn mass (`P² = a³ / M`, the anchor's own formula); apparent brightness from size class, albedo, and geometry; inner wanderers' maximum elongation (`asin(a_w / a_anchor)`); outer wanderers' opposition brightening and retrograde-loop cadence (synodic period `1/|1/P_w − 1/P_anchor|`). Higher fidelity refines, never contradicts: the kinematics are honest now so the orrery inherits no fakes.

**Structure:** `StarSystem` gains `wanderers: Vec<Wanderer>` (innermost first). Worlds serialize as seed + ledger, so no world-format migration; scene/ephemeris JSON changes rebaseline in the same commit that drifts them, per house rule.

**Facts** (world-side → ledger, parallel-list pattern like moons): per-wanderer `wanderer-orbit-au`, `wanderer-period-days`, `wanderer-class`; count as a functional fact. **Phenomena:** a wanderer among the stars is the night sky's salience anomaly ("a star that wanders"); inner wanderers get the **morning-star/evening-star duality** — their heliacal risings and settings via stage 1's machinery, appearing as two dated event families the observer has no reason to unify.

**Pin:** `--wanderers N` (0–4) forces the count. The `WANDERER_COUNT` draw always happens (then is overridden), and per-wanderer draws follow the effective count; because both live on their own streams, no other subsystem's draws can move — the pin-isolation battery proves star/anchor/moons/neighbors/forcing bytes are identical pinned vs unpinned. Unsatisfiable pins fail loudly with the physical reason; no retries across seeds.

## 4. Stage 3 — The Figured Sky (derived catalog)

**Starfield:** ~100–300 faint background stars (count drawn once, then per-star RA/dec via the sphere-uniform transform, brightness class) from one new stream:

```
STARFIELD = "starfield"             # count + per-star position/brightness
```

Generated **on demand** by a pure function of the astronomy seed — never serialized, never committed per-star, not part of genesis's stream-consumption order (its own derivation, like a field). The catalog is the night sky's texture; neighbors remain the named actors.

**Figures:** deterministic single-link clustering over the unified sky (neighbors + starfield) by angular separation with a brightness floor; iteration order fixed by `(total_cmp on brightness, then RA, then dec)` tie-breaks; thresholds are model-card constants. Figures are **descriptively identified** — "a chain of five low in the winter sky" — from their structural properties (member count, shape class, sky region, season of visibility). No proper names (non-goal).

**The ecliptic flag:** a figure whose angular span crosses the sun-and-wanderer road is marked `on-ecliptic`. Those figures are the calendrically loaded ones — the zodiac band exists for the cost of one boolean.

**Facts** (minimal, world-side only): functional `figure-count`; per-figure `figure-members`, `figure-region`, `figure-on-ecliptic`, committed in deterministic order (descending member count, then RA). Everything interpretive (descriptions, seasonal timing) stays derived.

## 5. The trace-protocol law (the observer-dependence spectrum)

The campaign's members span a spectrum from world-side to mind-side, and their position dictates their kernel protocol. This is the design's law; every later addition to the night sky should place itself on the spectrum first:

```
position on spectrum   member                protocol
---------------------  --------------------  ---------------------------
world-side             wanderers             Facts (orbit/period/class)
world-side             pole-star verdict     Fact (drawn-declination property)
event-side             heliacal risings      Phenomena (dated, salient)
event-side             morning/evening star  Phenomena (two event families)
view-side              seasonal visibility   derived view (no commitment)
mind-side              figures               minimal world-side Facts only;
                                             descriptions stay derived
```

Consumers never learn which system produced a phenomenon (constitutional); the figures' descriptive identifications flow through prose/almanac, not through the registry.

## 6. Degenerate regimes

Handled, not assumed away — each formula degrades continuously or returns the honest constant:

- **Locked rotation:** the night hemisphere's sky is frozen — seasonal machinery returns the constant answer, heliacal events are absent (like tier 0's aperiodic sun: an absence, not an error). The instrument becomes a map, not a calendar.
- **Zero/low obliquity:** the sun's declination flattens toward 0; seasonal night-sky variation collapses continuously (RA still moves — heliacal risings survive; only the *daylight* seasons vanish). No special case; the formulas flatten.
- **Retrograde spin:** wheeling direction flips in prose only; no period or date changes (SKY-22's shipped rule).
- **Extreme latitude:** polar observers see one unchanging hemisphere; the circumpolar verdict covers it with no special case (at φ = 90°, everything visible is circumpolar).

## 7. Testing

Property batteries in the `genesis_properties.rs` / `tier_refinement.rs` house style:

- **Determinism:** every new draw and every derived catalog byte-identical across two generations; clustering deterministic across platforms (all transcendentals via `kernel::math`, quantization at emit boundaries only).
- **Pin isolation:** `--wanderers N` leaves star/anchor/moons/neighbors/forcing untouched (the save-format contract battery).
- **Regime × feature matrix:** {spinning, locked, retrograde} × {zero, normal obliquity} over a seed sweep — every stage-1 derivation returns the regime-honest answer (locked → no heliacal events; zero obliquity → risings survive, daylight seasons vanish; retrograde → dates unchanged).
- **Cross-tier refinement:** a sky with wanderers keeps tier 0's whole claim — exactly one top-salience day-sky sun, nothing rivals it, "the sun" never retracted (extends the SKY-23 battery).
- **Kepler consistency:** outer wanderers slower than inner; period monotone in semi-major axis; elongation < 90° for inner, opposition defined only for outer.
- **Geometry properties:** circumpolar sets partition correctly at sampled latitudes; heliacal dates fall within the year; seasonal visibility at the equator covers every star over a full year.
- **Fixtures:** golden seed-42 extends; almanac/artifact drift check covers the new sections; goldens re-pinned in the drifting commit. Seed sweeps sized for the commit gate; anything heavier carries a `heavy:` ignore reason.

## 8. Book, registry, and close obligations

- **Registry flips at close:** SKY-9, SKY-12, SKY-seasonal-night-sky, SKY-heliacal-risings, SKY-circumpolar → `shipped` with result summaries.
- **New deferred rows filed:** the wanderer-synodic calendar (the fourth calendar family, Venus-calendar territory) and wanderer transits/occultations (composite of wanderers × the solar disc / moons).
- **Definition of Done:** chronicle entry, freshness sweep (the almanac chapter, the sky chapters, the concept-registry chapter for new predicates), Confidence Gradient re-score if a bet moves, campaign retrospective in `docs/retrospectives/`.
- **Model card additions:** the arcus-visionis table, wanderer orbit bands and albedo/brightness model, clustering thresholds, pole-star threshold — each labeled drawn / derived / approximated.

## 9. Stage boundaries

Each stage merges green through `make gate`; `make preflight` + absorb main at every stage boundary (the self-describing-sky campaign is the known collision, at `facts.rs`). Stages 1 and 2 are independently shippable; stage 3 detaches cleanly if the campaign runs long.
