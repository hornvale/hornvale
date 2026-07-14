# The Night Sky Instrument

**July 2026 · outcome: merged — the fixed stars gain a placed, epoch-honest
position; two new kinds of body move against them; and the sky becomes
something a placed observer can actually use**

## What was attempted

Astronomy had, by Firm Ground II, everything a sky needs to *drift*:
obliquity, eccentricity, and axial precession all live as functions of
absolute time. But `precession_at` had no reader — computed every query,
discarded every time — and the sky itself held only fixed stars: no
constellations, no calendar built from a star's own rising and setting, no
wandering planets, nothing an observer standing at a real latitude could
use to actually navigate or keep time. This campaign built the instrument
those draws were always capable of supporting: a placed, epoch-honest
night sky, real wandering siblings on Kepler orbits, and a figured
background a culture could eventually name.

## The derivation trio

Three pieces land as one coherent read of the same underlying geometry.
`sky_position.rs` gives `precession_at` its first consumer: a star's
*apparent* equatorial position is its genesis coordinates carried through
the genesis ecliptic, drifted by however far the equinox has precessed
since epoch 0, and re-projected through the *current* epoch's obliquity —
not the genesis one. Two star charts drawn twenty thousand years apart
genuinely disagree, by design, because a real sky would. `night_sky_at` is
the unified derived view a placed observer's night now holds in a single
query: which neighbors are visible tonight, which never set (circumpolar)
or never rise at all, and whether a pole star currently exists — a
system-level fact, independent of the observer's latitude, so it can
retire and crown itself as precession turns the pole across a starfield
that never moves the stars themselves. And `heliacal_events` recovers the
oldest naked-eye instrument there is: the day a star first reappears in
the pre-dawn twilight after a season lost behind the sun's glare, and the
evening it is last glimpsed before vanishing again. The threshold a star
must clear to be seen is class-graded — a blue giant surfaces through
brighter twilight than a red dwarf tolerates — and the gap between rising
and setting is the star's *absence*, the seed of a stellar calendar family
standing beside the solar and lunar ones.

All three read the same shared `SkyBand` (Day / Twilight / Night, split at
a fixed depth below the horizon) that the ordinary day/night prose already
used — one twilight definition, never duplicated, never allowed to drift
between consumers.

## The wanderers

Zero to four sibling worlds now orbit the anchor's star on real Kepler
orbits, drawn on their own stream (or pinned exactly via `--wanderers`, up
to the legal maximum of four). Each carries a semi-major axis, a derived
period (Kepler III, from the axis and the star's mass), a class (rock or
giant), an albedo, and — for inner wanderers only — a maximum elongation
bounding how far from the sun they can ever appear. Inner wanderers swing
through the oldest planetary spectacle there is: lost in the glare near
conjunction, then a morning star low before the sun, then (after crossing
behind it) an evening star chasing the sunset — the almanac now narrates
exactly that swing. Outer wanderers loop retrograde near opposition, a
purely geometric fact independent of visibility. The phase driving all of
this is a declared approximation, not a second orbital integration: each
wanderer's synodic phase reuses the genesis year-phase offset already on
the stream, rotated by an arbitrary per-wanderer constant (`+ index ×
0.37`) that draws nothing new — cheap, deterministic, and honestly
documented as a stand-in rather than smuggled in as physics.

## The figured sky

A derived starfield — 100 to 300 faint background stars, dim-heavy like a
real sky, drawn on their own stream and never committed to the ledger —
gives the notable neighbors company. `figures.rs` then clusters neighbors
and bright field stars alike by angular separation (single-link, a 7.0°
threshold, a magnitude-4 brightness floor for admission, a minimum of
three members) into star figures: no proper names, just structural facts
— member count, centroid, angular span, brightest class, and whether the
figure straddles the ecliptic band. That last flag is the zodiac, reduced
to one boolean: a figure a wanderer or the sun could plausibly pass
through, versus one that never sees anything but the fixed stars.

The clustering thresholds were not picked; they were **calibrated**. A
1000-seed lab study (`census-of-figures.study.json`) swept the parameter
space before freezing `FIGURE_SEPARATION_DEG = 7.0`, `FIGURE_MAGNITUDE_FLOOR
= 4`, and `FIGURE_MIN_MEMBERS = 3` against the readout: a median of 6
figures per sky, 6.4% of worlds with no figure at all (a real, honest
possibility rather than an artifact of a bad threshold), and 66.5% of
worlds with at least one figure crossing the ecliptic. The figure catalog
is a **reference-observer convention**, stated as such in the model
card — a real culture's asterisms depend on its own visual acuity and
tradition, and a per-species catalog (crossing this against each species'
own perception model) is named as future work, not silently assumed away.

## The observer-dependence spectrum, as trace-protocol law

What this campaign actually demonstrates, underneath the individual
features, is a spectrum of how facts depend on the observer, made legible
by forcing every derivation to be honest about which end of it a quantity
sits on. A wanderer's orbital period depends on nothing but the star and
the wanderer — the same number from every latitude, every epoch. A pole
star's *identity* is a system-level fact — the same from every latitude,
but it drifts across epochs as precession turns the pole. Whether a
*particular* star is visible tonight depends on latitude, season, and the
hour, all three. And a heliacal rising depends on all of that *plus* which
class of star is asked about, since the twilight threshold it must clear
scales with brightness. None of this was new machinery — it is the trace
protocol's read side, applied with more discipline than before — but this
campaign is the first place the astronomy domain had enough moving parts
at once to make the spectrum itself visible, and the regime battery
(`night_sky_regimes.rs`) pins it directly: a locked world's sky freezes
completely (no heliacal events, no sky band, nothing to be honest *about*)
rather than returning a stale or approximate answer; spin direction flips
which way the sky wheels but never touches a single heliacal date; and
precession moves the epoch-referenced half of the sky while leaving every
orbital-mechanical quantity — periods, the year itself — exactly where it
was, because those quantities take no epoch as an argument at all.

## The Confidence Gradient

No bet in the Confidence Gradient chapter names the night sky's
regime-honesty or the observer-dependence spectrum specifically, so
nothing there was re-scored — the same call The Self-Describing Sky made
for its own closing campaign. This campaign's work is recorded here in the
chronicle instead.

## What this leaves open

Wanderers have no calendar of their own yet — the synodic-period family
heliacal risings founded for fixed stars has an obvious wanderer analog
(Venus-calendar territory) nobody has built. Transits and occultations
(a wanderer crossing the solar disc, or a moon) are a composite of
machinery that all now exists but has never been crossed against itself.
Figures are one reference observer's catalog, not a per-species one. And
position oscillates everywhere in this sky — moon phase, wanderer
elongation, precession drift — while brightness itself never does; a
variable star is the shipped starfield's obvious next degree of freedom.
All four are named rows in the idea registry, not silent gaps.
