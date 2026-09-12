# The Planetarium source qualification

**Status:** Task 1 source inventory and geometry qualification document. The
physical-radius prerequisite is implemented; the evaluated geometry endpoint
and its executable conformance suite belong to Task 2.

**Authorization:** Nathan approved “Include a physical-radius prerequisite” on
2026-09-10. The addition is a derived astronomy observation over the existing
anchor mass. It adds no stored `Anchor` field, seed draw, stream consumption,
terrain rescaling or dynamics feedback.

## Physical anchor radius

`earthlike-rocky-zeng2019-linear/v1` assumes a spherical, Earth-like rocky
interior of 32.5% Fe and 67.5% MgSiO3. It linearly interpolates Hornvale's
frozen covering subset of Li Zeng's published numerical curve and converts
Earth radii with a 6371 km volumetric mean Earth reference radius. The source
curve uses Earth-mass and Earth-radius axes:

- Author's model page: <https://lweb.cfa.harvard.edu/~lzeng/planetmodels.html>
- Numerical curve: <https://lweb.cfa.harvard.edu/~lzeng/tables/massradiusEarthlikeRocky.txt>
- Retrieved curve: 49 rows; SHA-256
  `dcc5080f2186983b7e36200373878dc06a8d8083ec21ce1c4f670659c0404b38`
- Earth radius reference: NASA volumetric mean value, 6371 km,
  <https://nssdc.gsfc.nasa.gov/planetary/factsheet/plutofact.html>

Hornvale supports evaluation only on 0.5–2 Earth masses, the existing anchor
generator's range. Inputs outside it return a `UnitError`; no extrapolation is
performed. Exact tabulated knots are preserved and values between knots use
Hornvale's declared linear interpolation. The model does not assert that
Hornvale simulates interior composition, an atmospheric envelope, oblateness,
composition diversity or radius feedback into orbital, rotational, climate or
terrain models. The 2016 analytic power law was rejected for this interval
because its published applicability begins at one Earth mass:
<https://arxiv.org/abs/1512.08827>.

## Reproducible candidate inventory

The inventory was generated from commit `d61ad3f995342f705b1269271a44f29c80bd4619`
plus the Task 1 radius worktree changes, using the existing CLI:

```sh
work=$(mktemp -d)
target/debug/hornvale new --seed 42 --out "$work/world.json"
target/debug/hornvale scene system --world "$work/world.json" > "$work/system.json"
target/debug/hornvale scene moons --world "$work/world.json" > "$work/moons.json"
shasum -a 256 "$work/world.json" "$work/system.json" "$work/moons.json"
```

The actual temporary path was
`/var/folders/_0/j0_zkq_d3jn0klz033gq33cc0000gn/T/tmp.nR4e8KDykt`.
The hashes were:

- `world.json`: `77168f2bc1a8db9c01b37b31b66ac4757e1133862f8249aa8d80bb0194285bf8`
- `system.json`: `a82a0c95aedb6211ad802dca662c2bbe0812322c8fd2eab35506266b920d32da`
- `moons.json`: `11d6cd46c2882d17ec889eb6158588702583faeb20d836d2f89be84955cc1108`

Seed 42 is a single-star system with a 0.97164647 AU anchor orbit, a
368.05357-day year, a 0.87988-day prograde rotation, 0.95930567° mean
obliquity and genesis orbital phase 0.20941868 turns. It has two moons:

| Moon | Distance (Mm) | Sidereal days | Inclination | Node at genesis | Radius (km) | Formation |
|---:|---:|---:|---:|---:|---:|---|
| 0 | 307.74439 | 15.993805 | 4.6667409° | 1.5976041° | 2274.7776 | giant-impact |
| 1 | 494.27358 | 32.555 | 117.27724° | 193.38776° | 1599.1254 | capture |

It also has two outer wanderers: a giant at 4.9626269 AU and a rock at
7.2662626 AU. This satisfies the candidate requirement without an alternative
seed search. No seed in 0–63 besides 42 was attempted. Seed 42 remains a
candidate, not the final visual selection; the final seed, pins and represented
time interval are frozen only after the first actual look-development output.
That selection is compositional and makes no prevalence claim.

## Coordinate and orientation contract to qualify in Task 2

The astronomy sources use two related right-handed frames. In the ephemeris
system frame, `+x` is the anchor's genesis phase-zero axis, `+y` is one quarter
turn ahead and `+z` is the orbital north normal. `Calendar::year_phase` uses
the same phase, but `Calendar::solar_equatorial` describes the sun in the
calendar's solar/equinox frame: at phase zero its vector is `+x`, while the
native anchor is at system `+x` and the anchor-to-orbital-center sightline is
system `-x`. These values are intentionally opposed rather than directly
equal.

Task 2 converts the exact calendar vector into the native system frame with
`Rz(π) · Rx(-obliquity_at(t))`. `Rx(-obliquity)` removes the calendar's
equatorial tilt and `Rz(π)` supplies the explicit center-sightline half-turn.
For orbital longitude `λ`, the converted vector is
`(-cos λ, -sin λ, 0)`, exactly the unit sightline opposite the native anchor
position `(cos λ, sin λ, 0)`.

Task 2's executable `planetarium_geometry` suite must cover these cases:

1. At negative, zero and positive instants, the public anchor-position wrapper
   equals the native circular evaluator in AU. Its orbital-center sightline,
   agrees with `Calendar::solar_equatorial` only after the explicit
   `Rz(π) · Rx(-obliquity_at(t))` calendar-to-system conversion. The resulting
   anchor basis columns are unit length, mutually orthogonal, and satisfy
   `x × y = z`.
2. The same basis checks hold for a tilted prograde anchor, a retrograde
   anchor, and a tidally locked anchor. Spin direction changes surface rotation
   direction; it does not reverse the orbital frame. A locked world keeps the
   substellar body longitude fixed at the prime meridian. Its orientation is
   not inertially fixed: it turns synchronously as the anchor advances around
   the orbital center.
3. For each moon, negative, zero and positive instants agree with
   `moon_ecliptic_longitude_deg`, `moon_ecliptic_latitude_deg` and
   `node_longitude_at`. The suite includes both a prograde inclination and the
   seed-42 captured moon's 117.27724° retrograde inclination.
4. Single, wide-binary and close-binary fixtures exercise
   `stellar_positions_at` and `stellar_illumination_at`. In a single system the
   orbital center is the primary. A wide binary retains a circumprimary
   anchor. A close binary uses the barycenter and resolves both stellar bodies.

Two existing distinctions are part of the contract. `Calendar::solar_equatorial`
uses exact spherical declination; `solar_declination` and the private
`solar_geometry` deliberately use a coarse sinusoidal approximation. Geometry
conformance uses the exact equatorial producer and must not require agreement
with the coarse tier away from cardinal phases. Also, the calendar's solar
reference is the orbital center, while `stellar_illumination_at` resolves each
star in a binary. Binary source directions are therefore checked individually;
they are not substituted for the calendar center direction.

If these independent producers disagree, Task 2 must narrow the source
contract or fix the owning astronomy implementation. Camera placement cannot
absorb the discrepancy.

## Eclipse avoidance

The film's time mapping is intentionally not frozen before look development,
so no final interval is qualified here. Once the candidate and exact inclusive
source interval are selected, the reproducible check is:

1. Require a single-star candidate.
2. Call the existing `eclipse_events(system, calendar, from, until)` export and
   record every returned event.
3. At every final frame's exact `StdInstant`, inspect the emitted anchor, moon
   and stellar-source directions. Check source/body angular alignment as well
   as the dated event list, including the interval endpoints.
4. Reject the interval if either check indicates an occultation or shadow, or
   if a required body dimension is absent. Select and record another interval;
   do not fill a missing dimension with presentation geometry.

This procedure is avoidance evidence for the film. It does not validate or
extend the independent eclipse model.
