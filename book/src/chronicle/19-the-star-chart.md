# Campaign 19: The Star Chart

**July 2026 · outcome: complete, merged — a confirmed physics defect fixed
before a single star was drawn, and the almanac's oldest unpaid promise
finally paid**

## What was attempted

Since the founding spec, the Constitution has promised the almanac "star
charts." For seven campaigns the sky delivered a neighborhood — two to five
notable stars, each with a class, a distance, a brightness — and never a
picture. Designing that picture surfaced two gaps a chart cannot honestly
paper over. First, a neighbor star had a distance and a brightness but no
place: nothing on `Neighbor` said *where* in the sky it hung, so there was
nothing to plot. Second, and more serious, the moon's illumination cycle had
been computed on the wrong period all along — a confirmed defect, not a
missing feature. This campaign set out to fix the defect, give every
neighbor a fixed position, and only then draw the chart, in that order,
because a chart drawn over an unfixed defect would have rendered a
confidently wrong picture.

## What landed

**The moon corrected.** A moon's orbit and a moon's *appearance* run on two
different clocks. The orbital period — how long the moon takes to circle the
world once, measured against the distant stars — is the *sidereal* period,
and it is what Kepler's third law derives from the moon's distance. But an
observer on the ground never watches the moon against the stars; they watch
it against the sun, and the sun moves too, so the interval from one full
moon to the next — the *synodic* period — runs longer than the orbit itself.
For a Luna-like moon in a 365.25-day year, a 27.32-day sidereal orbit
stretches to a 29.53-day synodic cycle:

```
P_syn = P_sid × Year / (Year − P_sid)
```

The calendar had been feeding the sidereal period straight into illumination
phase and into the count of months in a year, an error of about eight
percent — enough that the almanac's own hand-written fixture prose, which
had always described "29.5 days" and "12.4 months" in the way a human
narrating a sky would, was quietly describing a different number than the
code beneath it computed. The fixture prose had the physics right; the code
did not. The fix keeps a moon's orbital period exactly as it was — that
quantity is correct and untouched — and introduces the sidereal-to-synodic
conversion exactly where an observed cycle is read off: the calendar,
`synodic_month`, guarded against the degenerate case where a moon's orbit is
not safely shorter than the year it sits in (the stability bounds that admit
a moon at all keep this from arising in practice, but the guard returns
cleanly rather than a nonsense negative period). Every committed almanac and
every study that counts months in a year shifted down a bin to match: fewer
of the longer synodic months fit inside a year than fit the shorter sidereal
one.

**A place in the sky for every star.** With the calendar honest, the second
gap could be closed without inheriting its dishonesty. Each neighbor star
now draws a fixed declination and right ascension at the moment the world is
born — declination from a uniform draw over the surface of a sphere (so
stars cluster the way a real, unbiased sky does, not thin toward the poles
the way a naive uniform-angle draw would), right ascension uniform around
the full circle. The celestial equator these coordinates are measured
against is the world's own rotational equator, the same frame a future
solar-position or latitude feature will need — this campaign spends nothing
extra to leave that alignment ready. The one hard constraint was that this
addition could not move a single star in a world already shipped: the draw
consumes a stream of its own, separate from the stream that has always
supplied a neighbor's class, distance, and brightness, so every world
generated before this campaign renders byte-identical neighbors with newly
visible positions layered on top, and a property test proves that pinning a
neighbor's class moves no position.

**The chart, in two media.** With a corrected clock and a real position for
every star, the almanac's promise could finally be kept. A 72-by-24
equirectangular chart renders each star as a single digit — its
brightness rank — on a field of space, right ascension running left to
right and declination top to bottom, with the celestial equator traced as a
dashed line underneath. It reads like this, for seed 42:

```text
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
      2                                                                 
                                                                   3    
                                                                        
- - - - - - - - - - - - - - - - - - - - - - -4- - - - - - - - - - - - - 
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
  5             1                                                       
                                                                        
                                                                        
                                                                        
```

Alongside it, a 256-by-128 planisphere pair renders the same sky in color: a
north disc and a south disc, each an astronomer's classic view straight
down the pole, star brightness as dot size, star class as dot color, and
every dot labeled with its index in a small hand-drawn digit font. Both
charts carry a companion strip for every moon — its full synodic cycle laid
out as sixteen characters, `oo))))))OO((((oo`, new through waxing through
full through waning and home again — deliberately timeless, tied to no
particular day, because a chart is a map of the sky's structure, not a
snapshot of one night. The whole page is reachable from the command line as
`hornvale star-chart`, and a world still running the tier-0 constant sun —
one motionless golden sun and no notable neighbors at all — is refused
loudly rather than handed an empty page: there is nothing there to chart.

## What was learned

- **Fixing the clock before drawing the picture pays for itself.** Ordering
  the work so the synodic correction landed before a single star was
  plotted meant the chart never rendered a wrong phase for even one commit
  — there was no interval in which the two pieces of the almanac disagreed
  in a way a reader could catch. A campaign that drew the chart first and
  patched the moon phase after would have shipped a season of pictures it
  then had to admit were wrong.
- **A plan's list of what regenerates is itself worth checking.** The
  synodic correction's ripple was enumerated up front — the almanac
  fixtures, the calendar's own tests — but one committed study of month
  counts across many seeds was missed in that enumeration, and the same
  study was independently missing from the automated check that regenerates
  every committed artifact and fails the build on drift. A second look
  before merge caught both gaps together; neither would have caused a
  silent wrongness, but both would have left a stale number sitting in the
  repository, patiently wrong, until someone happened to look.
- **A committed reference table is exactly the kind of thing worth a second
  pair of eyes.** The chart's digit labels come from a small hand-drawn
  bitmap font, five digits at a handful of pixels each — the sort of table
  that is tedious to write and easy to get one row wrong in. One digit's
  bitmap was malformed in an early draft and would have rendered a
  misshapen numeral onto every future chart that needed it; a review pass
  caught it before it ever reached the gallery.

## Deferred, deliberately

No constellations and no star names — every star in the chart is still
index-labeled, and grouping them into named figures or handing them to a
people's own mythology is real work for a future campaign, not a quiet
extension of this one. No full background starfield: the chart shows
exactly the handful of notable neighbors the model already holds, because a
denser field would be decoration standing in for data the world does not
actually track. No observer either — no latitude, no horizon, no rising or
setting; the chart is the whole fixed sphere seen from nowhere in
particular, not any one night's view from any one place. And the sky's
oldest remaining timing debt, the moment all worlds are born already
misaligned in their very first dawn, stays exactly as deferred as it was
before this campaign — a known, named gap, not a silent one.

## Artifacts

[The Night Sky of Seed 42](../gallery/star-chart-seed-42.md) — the ASCII
chart, the legend, the moon strips, and the full-color planisphere pair, all
regenerated and drift-checked on every build. [The Astronomy
chapter](../domains/astronomy.md) — the model card now records a star's
drawn position alongside its class and distance, and the chart as the
domain's committed artifact. `hornvale star-chart` — the CLI command that
renders the page, and refuses cleanly on a world with no generated sky to
chart.
