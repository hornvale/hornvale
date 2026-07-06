# Campaign 2: The Sky — Design

**Date:** 2026-07-05
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-05-hornvale-longterm-plan-design.md` (Constitution §2 governs)
**Vision chapter:** hornvale-book, `005_astronomy` (*Cosmos* and Tatooine)

---

## 1. Goal

Give the sky reality: replace the tier-0 constant sun with a **generated
star system** — anchor-first, experiment-pinnable, back-of-the-envelope
physical — and the observables that flow from it: day and night, seasons,
moon phases, a derived calendar, and a night sky with notable stars. Exit
demo: two worlds identical except for a pinned astronomical difference
produce **legibly different religions** — an early draft of the year-one
enrichment-thesis demonstration.

## 2. Design principles specific to this campaign

1. **Anchor-first generation.** Habitability is not a filter applied after
   generation; it is the generation *order*. The habitable anchor world is
   placed by construction; everything else decorates around it and is
   locally redrawn (bounded) if a decoration would break the anchor.
2. **Pins, not filters.** Every generated parameter is either **drawn** from
   a seeded stream or **pinned** by the experimenter; downstream generation
   conditions on pinned values identically to drawn ones. An unsatisfiable
   pin combination fails loudly, naming the conflicting constraint — the
   generator never silently bends.
3. **Envelope physics with a model card.** Real formulas where cheap and
   load-bearing; declared approximations everywhere else (§5). Promoting a
   "drawn" quantity to "derived" in a later tier is an **epoch bump**
   (the ratified regeneration mechanism), never a silent change.
4. **Tiers coexist.** `ConstantSun` (tier 0) remains untouched and usable;
   the new provider is additive.

## 3. Phase 0 — ratified opening chores (from the 1b final review)

- **Provider centralization:** `world_builder` becomes the single place
  providers are constructed (currently triplicated across
  `observed_phenomena`, `almanac_context`, and `repl.rs`). Precondition for
  swapping in the new provider; REPL reduces to presentation over
  `world_builder` accessors (`sky_report(world, time)`,
  `climate_report(world)`).
- **Kernel ergonomics:** a public `NAME` predicate constant;
  `Ledger::text_of(subject, predicate) -> Option<&str>` (or String) to
  replace the per-crate `text_of` helpers; `Debug`/`Clone`/`PartialEq`
  derives on all report structs (`SkyReport`, `ClimateReport`, `PlaceInfo`,
  `VillageInfo`, `Belief`).
- **Stream-manifest discipline:** every seed-derivation label used anywhere
  is declared as a constant in one reviewed module per crate; a generated
  **stream manifest** page joins the book's reference section, produced by a
  `hornvale streams` subcommand and drift-checked in CI exactly like the
  concept registry. (Alias/indirection layer stays deferred to the
  lazy-generation campaign; forced regeneration uses epoch suffixes, e.g.
  `settlement/name/v2`.)
- CLI lib extraction for WASM: deferred unless the stretch goal (§11) is
  taken.

## 4. System genesis

New module in `hornvale-astronomy`; runs during world genesis; all streams
derive from `seed.derive("astronomy")` with labels declared per Phase 0.

Generation order:

1. **Star.** Main-sequence only this campaign. Mass drawn within the range
   that permits a habitable zone compatible with the anchor constraints
   (roughly K-through-F); luminosity derived (mass–luminosity relation);
   habitable-zone bounds derived (√L scaling). Color/character derived from
   class.
2. **Anchor world.** Orbital distance placed *inside* the habitable zone by
   construction (drawn within the zone, or derived from a pinned year
   length via Kepler III — with a loud failure if the pinned year has no
   in-zone orbit). Rotation: period drawn (or pinned), or the
   **tidally-locked regime** (pinned or a small drawn probability), in which
   case no local solar day exists. Obliquity drawn 0°–35° (or pinned,
   including 0 = no seasons); seasonal amplitude derived from obliquity.
3. **Moons.** Count drawn 0–3 (or pinned). Each: mass and orbital distance
   drawn, then period (Kepler III), angular diameter, and relative tidal
   strength (∝ M/d³, normalized so Luna-at-Luna-distance ≈ 1) derived.
   Acceptance requires the stability inequalities: above the Roche limit,
   inside the anchor's Hill sphere with margin, mutual spacing between
   moons above a Hill-radius multiple, and an anchor-safety cap on combined
   tidal stress. Bounded local redraws (fixed attempt budget per moon);
   exhaustion with a pin in force = loud failure naming the pin.
4. **Stellar neighborhood.** A handful (drawn 2–5) of **notable neighbor
   stars** — the objects that would dominate a night sky, not a starfield
   (constellations are tier 3). Each: spectral class from a weighted
   distribution (giants rare but present), distance drawn above a
   plausibility floor, apparent brightness and color derived
   (luminosity-by-class, inverse square). Observational only: no
   gravitational or radiative effect on the anchor (declared approximation).
   One showpiece neighbor is pinnable by class.

### Pins shipped this campaign

| Pin | CLI flag | Values |
|---|---|---|
| Moon count | `--moons N` | 0–3 |
| Rotation regime | `--rotation normal\|locked` | or pinned period via `--day-hours N` |
| Obliquity | `--obliquity none\|N` | degrees, 0–35 |
| Year length | `--year-days N` | local days; must admit an in-zone orbit |
| Showpiece neighbor | `--neighbor none\|blue-giant\|red-giant\|white-dwarf` | forces one; rest drawn |

Unpinned = drawn. Pin validation errors are user-facing CLI errors with the
physical reason ("a 4000-day year places the anchor outside the habitable
zone of this star").

## 5. The model card

Maintained in the spec and mirrored in the book's astronomy chapter. Every
quantity appears in exactly one column.

**Derived (real physics):** stellar luminosity (mass–luminosity), habitable
zone (√L), all orbital periods (Kepler III), moon angular diameters,
relative tidal strengths (M/d³), neighbor apparent brightness (inverse
square), day/night geometry from rotation + obliquity + season.

**Approximated (declared):** circular orbits; no orbital evolution or
resonance; no N-body effects; seasonal daylight variation by a smooth
sinusoid in obliquity and year phase; neighbors observational-only; no
eclipses this campaign (angular diameters exist, so tier 3 can derive them).

**Drawn from seed (or pinned):** star mass; anchor orbital distance (within
zone); rotation period/regime; obliquity; moon count, masses, distances;
neighborhood size, classes, distances.

## 6. Two clocks

- Kernel `WorldTime.day` is **ratified as absolute time in standard days**
  (a fixed unit, decoupled from any planet's rotation). Doc-only
  sharpening; no schema change; tier-0 worlds unaffected; `Fact.day`
  untouched.
- Astronomy gains a **calendar layer**: from the generated system, translate
  absolute time to — local day index and fraction (absent if tidally
  locked), day/night state and daylight fraction (obliquity- and
  season-modulated), year day and season phase, and each moon's phase.
  Worlds without a given cycle have calendars without that column,
  truthfully. Month structure = each moon's period expressed in local days,
  with the non-integer year/month ratios stated plainly (intercalation is a
  future culture problem, not ours).

## 7. The tier-1/2 provider and observables

New provider `GeneratedSky` (name is a descriptive placeholder, per the
naming deferral) alongside the untouched `ConstantSun`:

- `sky_at(time)` becomes time-dependent: sun up/down and elevation
  character, visible moons with phase descriptions, night-sky presence of
  notable neighbors. Tidally locked worlds describe the permanent day and
  night faces.
- **Phenomena with real periodicities and time-varying salience:** the sun
  (period = 1 local day; absent for locked worlds — replaced by the
  fixed-sun-of-the-day-side phenomenon), each moon (its period; salience
  scaled by angular diameter), the seasonal cycle (period = 1 local year;
  salience scaled by seasonal amplitude; absent at obliquity 0), each
  notable neighbor (aperiodic but **night-gated** — a new phenomenon shape;
  salience by apparent brightness).
- Religion consumes all of this **unchanged** — cyclic deities appear the
  moment periodic phenomena exist. Any change to religion's code this
  campaign is a design smell.

## 8. Persistence and surface

- A **world entity** is minted at genesis. Pins are committed as facts about
  it with provenance `scenario`; the generated system's headline parameters
  (star class, day length, year length, moon count and periods, obliquity,
  notable neighbors) as facts with provenance `astronomy`. `why`-style
  interrogation of world configuration works like any other query; saves
  carry their experimental setup inside the ledger. Predicate names follow
  the ratified naming conventions and are reviewed at campaign close.
- REPL: `calendar` command; `sky [time]` gains the richer report;
  `phenomena [time]` now visibly varies with time of day.
- Almanac: **The Calendar** section (year, months per moon, season
  structure) and a night-sky paragraph under The Sky.
- `hornvale new` gains the pin flags (§4); provider selection defaults to
  the generated sky, with `--sky constant` preserving tier-0 worlds,
  recorded as a fact.

## 9. Exit criterion and artifacts

- **Exit demo:** `hornvale new --seed 42 --moons 0` vs `--moons 2` (same
  seed, one pin different) produce almanacs whose religions **legibly
  differ** — eternal-watcher vs cyclic-deity tenets — and whose calendars
  differ for stated physical reasons. This is the enrichment thesis's first
  real data point.
- **Gallery:** the paired almanacs, committed and CI drift-checked like
  seed 42's; optionally a third variant (`--neighbor blue-giant`).
- **Book:** astronomy domain chapter rewritten for tiers 1–2 (with the
  model card); chronicle chapter; stream-manifest reference page (Phase 0);
  comprehension gate as always.

## 10. Testing

- Determinism: same seed + same pins → byte-identical system facts,
  calendar output, and almanac. Different pins, same seed → different
  worlds with the pinned cause visible.
- Property tests over many seeds: every generated system passes all
  stability inequalities; anchor always in habitable zone; derived periods
  satisfy Kepler III round-trips; calendar invariants (daylight fraction in
  [0,1]; phases advance monotonically mod period; locked worlds have no
  local-day column).
- Pin tests: each pin honored exactly; each documented unsatisfiable
  combination fails loudly with the physical reason.
- Provider-contract tests: `GeneratedSky` and `ConstantSun` both satisfy
  the phenomena-source contract; the almanac renders both without special
  cases.

## 11. Deferred / stretch

- **Tier 3 (later campaigns):** binaries, eclipses (angular diameters
  already exist), constellations and the full starfield, precession,
  orbital resonance, physical obliquity.
- **Stretch, only if the campaign runs ahead:** CLI lib extraction + the
  first WASM gallery widget (type a seed and pins, read the sky). Not in
  the exit criterion.

## 12. Risks

- **Scope**: system genesis + calendar + provider + pins is the largest
  campaign yet. Mitigation: Phase 0 is mechanical; the physics is ~a dozen
  formulas; the plan should sequence system-genesis-with-tests before any
  observable, so an aborted campaign still leaves a tested generator.
- **Tuning ranges** (moon mass/distance draws that mostly pass stability;
  salience scalings that feel right) will need iteration; property tests
  catch validity, only the almanac's read catches *taste* — Nathan reads
  the paired almanacs before the campaign closes (comprehension gate doubles
  as the taste gate here).
- **The locked-world calendar** is the design's most novel corner (no local
  day: what does the almanac even say?); if it fights back, it ships as
  explicitly minimal ("time is measured in standard days; the sky does not
  move") rather than blocking the campaign.
