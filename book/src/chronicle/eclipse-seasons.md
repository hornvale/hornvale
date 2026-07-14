# Eclipse Seasons

**July 2026 · outcome: merged — the sky's shipped statistical eclipse rate
becomes dated events: a positional moon ephemeris, closed-form syzygy
enumeration, shadow ground tracks, and the full saros-to-parade recurrence
ladder, all read off one new draw**

## What was attempted

An earlier campaign shipped eclipses as a *rate*: each moon's orbital inclination diluted
the synodic month into a mean recurrence interval, and the almanac could say
how often the sun was swallowed but never *when*, nor where the shadow fell.
The reason was structural, not an oversight — a moon had an inclination
*magnitude* but no node *position*, so its eclipses were smeared uniformly
through the year instead of clustering into the shape real eclipses actually
take: two eclipse seasons per eclipse year, drifting slowly through the
civil calendar as the node regresses. This campaign turns the rate into
events. One new draw — each moon's ascending-node longitude — feeds a
derived first-order nodal regression, which feeds a positional ecliptic
ephemeris (the moon has a place in the sky, longitude and latitude both, for
the first time), which lets `eclipses.rs` enumerate every solar eclipse at
conjunction and every lunar eclipse at opposition closed-form — no scanning,
no sampling, exact syzygies. Everything downstream — ground tracks, sight
tiers, the recurrence ladder — falls out of that one enumeration; none of it
is new machinery. The shipped rate phenomenon stays byte-identical as the
coarse tier, with a constitutional guard test binding it to the newly dated
fine tier (coarse constrains fine, mechanically, not just as a stated
principle).

## The same-day collision, and how it resolved

Thirteen minutes before this spec landed, a parallel session had committed
*The Long Count*, claiming the identical `moon-nodes` stream and the same
dated-eclipse core from a different angle (a long-clock audit rather than an
ideonomy pass on a stale open-questions entry). Two campaigns
implementing one save-format stream with independent draw semantics would
have been exactly the kind of collision the Constitution's determinism
clause exists to prevent. Nathan adjudicated within the hour: Eclipse
Seasons owns the eclipse core, because it went deeper on the contested
ground — ground tracks, sight tiers, and the full recurrence ladder that The
Long Count's audit had not scoped. The Long Count re-scoped to the
non-overlapping residuals (secular stellar brightening, the alignment ground
half, the domain's verification batteries) and handed the standstill cycle to
a follow-up riding this campaign's deliberately deferred seam. In the
reverse direction, this spec adopted the Long Count plan's naming for the
shared surface — the module is `eclipses.rs` (the `moons.rs` plural
convention) and the drawn field is `Moon.node_longitude_deg` — so its
already-written eclipse tasks transferred verbatim rather than being
rewritten from scratch. Both specs record the adjudication; both plans
carry a hands-off constraint on the other's surface. It cost one spec
amendment each and no wasted implementation.

## The recurrence ladder

Once syzygies are enumerable, six more readings fall out for free. The
draconic month (a moon's period against its own regressing node) and the
eclipse year (the sun's return to the node line, Luna check ≈ 346.6 days)
set the two beats an eclipse season is built from. A closed-form search over
synodic:draconic ratios up to 300 months finds each world's own saros-analog
— fed true Luna periods it recovers the historical 223 synodic ≈ 242
draconic ≈ 6585.3 days exactly, though a generated world's own *derived*
node period (the approximation trades accuracy for a closed form; more
below) may legitimately land its best cycle on a different, shorter
commensurability. Three saros make an exeligmos, shifting the ground track
back to nearly the same longitude after triple the interval. A series'
lifetime — order 10³ years — falls out of how fast the node-phase slip per
return walks the ecliptic latitude across the eclipse threshold. And the
parade rate, the eclipse seasons creeping backward through the civil
calendar at a Luna check of ≈ 19 days a year, is simply the year length
minus the eclipse year. None of the ladder is bespoke: every rung is the
same handful of formulas from the model card, composed.

## Ground tracks and rarity made true

The positional ephemeris also answers a question the shipped rate could
never pose: where does the shadow fall? A solar eclipse's ground track is a latitude
band — center at the sub-solar latitude displaced poleward by the
event's own ecliptic-latitude miss, half-width a declared constant — swept
across longitude as the world turns beneath it over the crossing's duration
(hours, at Luna scale, the check value the tests pin against). Sight tiers
partition the globe honestly from that geometry: inside the band on the day
side, the sun is devoured whole or reduced to a burning ring; elsewhere on
the day side, merely bitten (the partial); the night side sees nothing.
Lunar eclipses get no band at all — the anchor's shadow falls on the whole
night hemisphere at once, which is the experiential asymmetry that makes
lunar eclipses the common omen and solar eclipses the rare, place-bound one.
"Rarity where you stand" was always the intuitive shape of an eclipse; this
campaign is what makes it a computed consequence of held quantities rather
than an assumption baked into prose.

## The saros-honesty point

The literal Luna check — 223 synodic months ≈ 242 draconic months ≈ 6585.3
days — only holds when the search is fed *true* lunar-theory periods. This
campaign's derived nodal-regression rate is a declared approximation, the
lunar-theory leading term, which yields a node period near 17.9 years
against the Moon's true 18.61. That gap is small in absolute terms but large
enough to shift which synodic:draconic ratio best minimizes drift, so a
generated world's own recurrence ladder may honestly find an octon-class
cycle (about 8 years) rather than a saros. The test suite draws the line
precisely: one test pins the *search function* against true inputs and
asserts it recovers 223/242 exactly; a separate test pins the *derived
pipeline* only to the weaker claim that it always yields some cycle whose
family survives at least ten returns. Asserting literal saros numbers
against approximated inputs would have encoded a false precision the model
card doesn't earn — the kind of mistake the domain's "declared
approximation, not hidden physics" discipline exists to catch before it
ships.

## The phenomenon that wasn't supposed to notice anything

The lunar eclipse's coarse phenomenon — the night-sky sibling of the shipped
solar rate, "the moon drowns in shadow and rises ember-red," salience 0.8,
below solar-total so "nothing rivals the sun" still holds — is a genuinely
new, always-present input to every mooned seed's phenomena list. Religion
mints its pantheon off the salience-ranked phenomena by design, with
consumers constitutionally forbidden from learning which system produced an
observation. That design promise was tested for real here: adding one
phenomenon re-derives every mooned seed's pantheon, deity names included,
without a hand-written line of religion code changing. The campaign's
decision ledger considered and declined a naming epoch for this — the
precedent set by the libm migration already established that a genuine
physics-driven world reshape rebaselines without one, and epochs exist for
stream-label reuse, not for legitimate new inputs changing what a cause-blind
consumer derives. The reshuffle is not a bug tolerated at close; it is the
architecture working exactly as specified, watched happen for the first time
at this scale.

## What the close carries forward

The census schema gained four metrics — eclipse-year length, solar and
lunar eclipses per century, and coincidence days per century (zero by
construction for worlds with fewer than two moons) — joining The Long
Count's two in the same schema bump. Following this project's standing
practice, the thirty-two census-fixture drift tests stay red on this branch
until the one shared AWS regeneration both campaigns' schema changes
warranted; that lag, not a full local rebuild, is the chosen trade
(censuses regenerate only on the spot box, with Nathan's authorization, never
on a local gate). Deliberately left out of scope: standstills, the 18.6-year
moonrise-azimuth extreme that gives monument-scale ritual astronomy its
alignment story — this campaign ships the standstill cycle's hard
prerequisite (the positional ephemeris and the committed
`moon-node-period-days` fact) without building the instrument itself, a seam
named in the registry rather than built here. The deep-time secular arc
(tidal braking spiraling a moon outward, thinning total eclipses to annular
over deep time) stays out too, but the model card's evaluate-at-t rule —
angular sizes and rates are functions of the event's own time, never
cached — is exactly the seam that arc will later slide under without an
epoch.
