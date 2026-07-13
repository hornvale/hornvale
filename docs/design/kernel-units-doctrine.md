# Kernel Units Doctrine

**Status:** Draft design note (2026-07-12). To be ratified as the
**`shared-units-live-in-the-kernel`** decision — numbered at merge (decision 0043
restored numbered decision records; the next free number after 0043) — when the
successor "Kernel Units" campaign lands. **Pilot:** The Datum
(elevation) — `docs/superpowers/specs/2026-07-12-the-datum-design.md`.

How Hornvale types physical quantities and where those types live. This grew out
of The Datum (the elevation pilot) but governs *every* future unit family, so it
is recorded on its own. It refines decision 0008 (coherent quantities are
newtypes) with the questions 0008 left open: *where* a unit lives, *how* it is
classified, and how it behaves at each boundary it must cross.

## Placement — where a unit lives

**Coherent physical quantities that cross domain boundaries live in the kernel;
single-domain quantities stay in their domain.** A quantity belongs in the
kernel when either (a) more than one domain speaks it, or (b) it originates in a
kernel type (e.g. `GeoCoord`'s latitude). This preserves the existing split by
example: the kernel holds the substrate (`WorldTime`); a domain holds its
flavored view (`StdDays`/`LocalDays` over `WorldTime`; `Au`/`SolarMasses` are
astronomy's scaled views over kernel Distance/Mass). The `elevation-convention`
waiver is reclassified as *temporary* — the kernel type retires it;
`crust-km-convention` follows in its own wave.

## Proactive, not reactive

The planned domains share a common physical vocabulary, so the kernel unit
*vocabulary* is built ahead of demand — units are a closed, knowable domain, so
this is not speculative generality. Each type's *richer surface* (accessors,
conversions) stays reactive: added when a consumer needs it. The Datum is the
pilot; the shared core is then built as a *series* of small, anchored,
byte-identical family campaigns (see Roadmap), not one mega-campaign.

**Each family needs a real anchor.** Proactive applies to the *doctrine* and to
families with a genuine current consumer (or a near-certain planned one) — not to
speculative SI completeness. This is the guardrail that keeps a units effort out
of the graveyard of built-but-unused abstractions: elevation (retires a live
waiver; enables room-scale radius), temperature (dedups the live
`paleoclimate::Celsius` wart), and angle (kills live degree/radian
hand-conversions in `GeoCoord`) all clear the bar today; energy, mass, and
vectors wait for a consumer.

## Promotion — classify the quantity, then type it

### Scale of measurement (Stevens) — the spine

A quantity's scale decides whether it is a unit at all; the levels form a lattice
from most to least structure:

- **ratio** (true zero; +,−,×,÷) — energy, mass, distance, radius, duration,
  absolute (Kelvin) temperature, and every *delta* → a shared kernel unit type
  with full arithmetic.
- **interval** (arbitrary zero; differences meaningful, ratios not) — elevation,
  Celsius/Fahrenheit temperature, calendar date, longitude-as-position → a unit
  type **that carries its zero-convention (datum)**, whose *difference is a ratio
  quantity*: its companion delta (elevation's `Sub`-delta, `Celsius` →
  `TempAnomaly`, instant → duration). The code grew these pairs blind
  (`Celsius`/`TempAnomaly`, sea-level/`SeaLevelChange`); the pattern is now
  deliberate — **an interval type is promoted together with its ratio delta.**

  *The same quantity can sit at different levels by representation:* absolute
  (Kelvin) temperature is ratio; Celsius temperature is interval. So the
  **canonical-unit choice sets the stored Stevens level**, trading ratio's clean
  true-zero (no datum) against interval's smaller-magnitude quantization
  precision (see Evolution boundary). A *genuinely* interval quantity (elevation
  — no true zero exists) has no such choice: its datum is load-bearing meaning,
  not a representation convenience.
- **ordinal** (order only — Mohs, Beaufort; a future sim's faith, reputation,
  danger) and **nominal** (categories — biome, species; the rubric's
  `identifier-text`) are **not units**: ordered enums / identifiers.

**Additivity is a related but distinct property.** Whether a type impls `Add`
(and therefore needs a companion delta) tracks *extensive vs intensive/positional*,
which correlates with — but is not identical to — the Stevens level. *Extensive*
ratio quantities add (mass + mass, energy + energy). *Intensive* ratio quantities
(temperature, density, pressure) and *all* interval quantities do **not** add —
they carry a ratio delta and support only `value − value → delta` and
`value ± delta`. Temperature is the case that proves the distinction: ratio (a
true zero exists) yet non-additive.

### Two rules from the lattice

- **Level is chosen at birth.** Structure only flows *down* the lattice (ratio →
  interval → ordinal → nominal); it is never recoverable upward. A quantity that
  will ever need arithmetic must be *born* interval/ratio — an ordinal stat
  cannot be "upgraded" later without re-modelling.
- **Interval types carry their datum.** An interval value is meaningless without
  its zero-convention recorded, and that convention travels with it across every
  boundary — which is why `ReferenceElevation` names its datum (`Celsius`'s zero
  is the freezing convention; a calendar's is its epoch).

### Orthogonal axes (independent of scale)

- **Multiplicity** — *singular* (one meaning, compares everywhere) → one shared
  type; *plural* (meanings that must not intermix: length → elevation vs
  crust-thickness vs distance) → semantic newtypes (`ReferenceElevation`,
  `CrustThickness`), never a bare shared `Length`.
- **Cyclicity** — *cyclic* quantities wrap (longitude, phase — modular arithmetic
  + normalization); *bounded* clamp (latitude ±90); *linear* do neither. An angle
  type is **not** a re-skin of the linear elevation pattern.
- **Rank** — the types built now are *scalars*. Rank-1 vectors (position,
  velocity, gradient — today bare `[f64; 3]`) are a **deferred category** that
  *decomposes* into scalar quantities + a direction (magnitude is a ratio scalar;
  direction is a cyclic angle), so nothing built now forecloses them.

### Derived units

No dimensional algebra — it is monolithic (every unit coupled through one
exponent system), which 0008 declined and which would violate "adding a quantity
must never edit an existing one." A relation between units is a **named-law
constructor** (`Speed::over(distance, duration)`, `Energy::kinetic(mass, speed)`)
that documents the physical law; an operator impl (`impl Div<Duration> for
Distance`) is added only where the operation is unambiguous and frequent. Derived
units are materialized on demand (most base×base combinations are meaningless);
magnitude (km vs m vs Mm) is an accessor, never a type.

## Boundaries — rich inside, contract at the edge

A unit is a rich type in the compute path. At every *dumb boundary* it degrades
to a value + an external contract, stored in a boundary-specific registry. Units
are one instance of a pattern the kernel already runs:

```
boundary           rich type becomes      contract lives in                 exists?
serialize (save)   quantized f64 bytes    #[serde(transparent)] + quantize  yes
cross-platform     8-sig-dig f64          QUANTIZE_SIG_DIGITS spec          yes
trace envelope     Value::Number          predicate def (name + registry)   yes
presentation       formatted string       the observer's frame (a window)   partial
evolution          a new representation    regenerate-from-seed + review     yes
```

**Units are compute-only.** No unit type is *self*-serialized today: the World is
`seed + ledger`, derived fields are recomputed, and a unit reaches the ledger (as
a bare `Value::Number`) and artifacts (as bare `f64`) via `.get()` — the trace
and presentation rows. The `serialize` row's `serde(transparent)` is therefore
*conditional*: it applies only if a future serialized struct holds a unit
directly. **And a validating unit must never derive transparent `Deserialize`** —
it would bypass `new()` and admit the very values the type exists to forbid; a
unit that ever needs deserialization uses a validating custom impl through
`new()`.

### Trace boundary

A unit crosses the deliberately-dumb `Fact`/`Value` envelope as its
*canonical-unit* `Value::Number` — the envelope stays untyped, so no consumer
learns a phenomenon's producer. The unit contract lives on the **predicate**:
visible in its name (`_c`/`_m` — `identifier-text`) and declared in the registry,
where the `Ledger` enforces the Value *kind* (unit-correctness stays a producer
discipline against that single declared source). One canonical trace unit per
quantity (no `_c`/`_k` proliferation; accessors are display-only); an *interval*
quantity's predicate declares its **datum** as well as its unit.

### Presentation boundary

The display unit belongs to the **observer**, not the quantity — so formatting
never lives on the type (**no `impl Display` for units**); the type offers the
canonical value plus reactive accessors (`.celsius()`, `.metres()`), and a window
chooses the frame. Presentation is a **(quantity × observer-frame) matrix** owned
by the presentation layer; adding a frame is a *column*, never a type edit. It is
a **leaf** (nothing depends on windows), so the irreducible messiness of
formatting (°C/°F, m/ft, 24-hour vs bells) is quarantined there. It is the one
boundary that is **regenerable, not frozen**: display is not serialized into the
world, so — unlike a byte-identical world — an almanac format may change
*intentionally* (regenerate, review the drift, commit). Far future: measurement
conventions are cultural artifacts (a civilization generates its own units, like
names); frame-parameterize the presentation layer now so that future is an added
column, not a refactor.

### Evolution boundary

Unit changes sort by blast radius (≈ semver):

- **Additive** (a new accessor, companion delta, or derived-unit constructor) —
  no break (minor).
- **Representation-preserving** internal refactor (serde form unchanged) —
  invisible.
- **Canonical-representation change** (Celsius → Kelvin; isostatic → radius) —
  changes serialized bytes (major), but worlds **re-derive from seed**, so it is
  a *regenerate + review-the-drift*, **not** a stream-label epoch. Serialized
  unit values are a *cache*; the seed is the source (the Lorenz guard-rail), so
  Hornvale needs no migrations or reader-dispatch — the existing
  artifact-regeneration workflow absorbs it.
- **Validation tightening** — safe only if the current physics cannot produce an
  out-of-range value.
- **Physics/formula change** — *not a unit change*; that is the model-card /
  stream-epoch mechanism.

**The one exception: authored vs derived values.** Derived unit values regenerate
freely; **authored** ones (CLI pins, constants, config) must pin their unit in
the config schema, or an old config is silently misread. That is the only place a
unit "version" must be explicit.

**Precision couples to the canonical unit and datum.** Under 8-significant-digit
quantization, a canonical unit whose typical magnitude is *large* burns
significant digits on the offset (Kelvin ~300 keeps coarser absolute precision
than Celsius ~25 for the same 8 figures). So an interval quantity with a *distant*
zero-convention is precision-hostile — **put the datum near the data** (the
isostatic reference sits near typical elevations; a far calendar epoch would burn
figures on the fractional day). Datum placement is a determinism-precision
decision, not only a semantic one.

## Fences (recorded so the library stays clean)

- **Vector/tensor quantities** (rank ≥ 1) — position, velocity, gradient (today
  bare `[f64; 3]`). A deferred *category* built later from scalar quantities + a
  direction; the scalar library is not "the whole library."
- **Conventional/ordinal scales** — faith, reputation, danger; Mohs, Beaufort:
  **not units.** They are ordered enums / ordinal scales, kept out of the units
  library. If one will ever need arithmetic, model it interval/ratio at birth
  (level is chosen at birth).

## Roadmap — the family queue

The doctrine spawns a *series* of small, anchored, byte-identical family
campaigns (not one mega-campaign), ordered by anchor and risk:

1. **Elevation** — The Datum (the pilot). Ready to execute (kernel freed when The
   Room Mesh shipped, 2026-07-12).
2. **Temperature** — next: re-uses the elevation mechanism on a simpler quantity
   (no datum, no cyclicity). Singular, ratio-but-intensive, carries `TempAnomaly`.
   Anchor: dedups `paleoclimate::Celsius` and the worldgen cross-domain borrow.
   Spec: `docs/superpowers/specs/2026-07-12-temperature-design.md`.
3. **Angle** — its own campaign (new cyclic machinery: `Longitude` wraps,
   `Latitude` clamps; shortest-angular-distance). Highest blast radius
   (`GeoCoord.latitude`); migrates astronomy's `Degrees`.
4. **Deferred until anchored** — `GeocentricRadius`/`Distance` (room-scale 3D),
   `CrustThickness` (retires `crust-km-convention`), `RelativeElevation<H>` (the
   Underdark), `Energy`/`Mass`/vectors (a computing consumer).

Per-family recipe: classify per this doctrine → spec → byte-identical plan →
origin-first migration → retire a waiver / dedup a wart → golden drift test.
