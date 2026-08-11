# The Ford

A river in Hornvale was about a hundred and ten kilometres wide. It was that
wide on the global map, that wide in the deep-zoom regional tile whose whole
purpose is to sample below the global lattice, and that wide in a room
twenty-seven metres across — where it reported the water class of a cell four
thousand times the room's own width.

The number is not a rounding error or a missing level of detail. It is the
edge length of the canonical geosphere cell, and it was the width of every
river because a river was *stored as a cell*.

## River-as-area is a type error

The useful reframing is not "the resolution is too coarse." It is that the
carrier of a hydrographic feature should match the feature's dimensionality,
and for one of the three carriers it did not:

| mesh element | feature | correct? |
|---|---|---|
| vertex | spring, pond | unmodelled |
| **edge** | **river** | **no — carried as a face** |
| face | ocean, lake, playa | yes |

Ocean-as-face is right; a salt basin really is an area. River-as-face is the
category mistake, and a hundred and ten kilometres is merely how large the
mistake is.

This distinction decides what the repair has to be. If the problem were
resolution, the repair would be a refinement rule — a way of subdividing a
cell that provably never contradicts the coarse answer above it — and that
rule would have to be designed, calibrated and defended. It is worth being
precise about why no amount of subdivision could have worked. The existing
room-level blend is an integer-weighted mean over the three corner cells of
the containing triangle: linear barycentric interpolation. **A linear
interpolant on a simplex attains its extrema at its vertices.** There is no
interior minimum anywhere below the cell floor, and therefore no sub-cell
valley for a river to occupy. Detail beneath the floor must be *added*; it can
never be recovered by interpolating what is already there.

Changing the carrier dissolves the problem instead of solving it. **A polyline
carrying a width function is the same object at a hundred and ten kilometres
and at twenty-seven metres.** "Coarse constrains fine" is satisfied vacuously,
because there is only one representation and every scale evaluates it
directly. Consistency between the map, the locale and the walk comes from
shared *derivation*, not shared storage: all three call one function of
position.

## One scalar, and the bands are a reading of it

Underneath, the whole model is a signed distance and four thresholds.

The kernel gained a primitive that knows nothing about water: given a polyline
on the unit sphere and a point, it returns the **signed** distance from the
point to the nearest segment, and a second function bands that scalar into
ordinal zones against a non-decreasing edge list. A river, a coastline, a
scarp and a treeline all band the same way. Water is the primitive's first
instance, not its owner.

The sign is load-bearing, and the campaign's name is the reason. A crossing is
precisely the path `bank → channel → bank` in which the sign flips; a model
carrying only `|d|` cannot distinguish fording a river from walking to the
water and turning back. It also cannot express cut-bank against point-bar —
the outside of a bend erodes and the inside deposits — which is nothing more
than `sign(d)` read against the sign of meander curvature. And symmetric bands
are simply wrong about rivers: real floodplains are frequently one-sided, and
an unsigned formulation would impose a symmetry the world does not have.

```
   terrace    floodplain   bank | channel |  bank   floodplain    terrace
 ------------------------------\|~~~~~~~~~|/--------------------------------
 |d| :   > V/2      < V/2    > w/2      0      < w/2    < V/2      > V/2
```

## Every width is an angle, because there is no length scale

`domains/terrain` works on the unit sphere. Cell area is `4π / cell_count`, a
dimensionless solid angle, and **no planet radius exists anywhere in this
codebase.** "Channel width in metres" therefore has no defined meaning here.

This is not a gap to paper over. The claim the campaign actually makes is
about the **ratio of channel width to cell width**, and that ratio is what a
dimensionless model states directly. A window that wants metres may multiply
by a reference radius it declares; introducing a world radius is its own
decision and not something to smuggle in through a river.

So the canonical level-6 cell edge — measured, not assumed, at **0.018886
radians** as the mean neighbour separation — is the unit everything is quoted
in.

## The network is constructed to satisfy known flux, not simulated

Drainage is already computed per cell, so every cell has a known discharge
`Q`. The downhill graph gives every cell a known exit. A channel therefore
needs *routing* between a known entry and a known exit, not simulating: runs
are traced downhill from headwaters, each cell contributing a vertex, until
the run reaches the sea, a sink, or a cell some earlier run already claimed.

Width follows downstream hydraulic geometry, `w = a·Q^b`, with `a` expressed
as a fraction of the local cell spacing so the result stays angular:

- `b = 1/2` is the textbook value and not a free parameter. It is what makes
  width-per-unit-discharge fall off as a river grows, which is what keeps a
  mainstem from being a hundred times its headwaters' width.
- `a = 8.5e-4` was **calibrated once, in the open**, against a single point:
  the widest channel seed 42 actually produces on the canonical globe is one
  hundredth of a cell edge across. The grid nearly cancels out of that fit —
  solving `a·edge·√Q = edge/100` gives `a ≈ 1/(100·√Q_max)` — so the
  coefficient is essentially a statement about the largest discharge the
  terrain produces, not about the mesh.
- The exponent, not the coefficient, is the dynamic-range lever. Real drainage
  spans about 15 to 180, so `√Q` spans only ~3.5× across the entire
  headwater-to-mainstem range. Widening that range by moving `a` is not
  possible: it scales every channel equally.

Outward from the centreline the bank border sits at `w/2 + w/2`, and the
valley border at `V/2`, where `V` carries the **confinement** term — a gorge
has no floodplain. Confinement falls linearly from 1 on the flat to 0 at a
gradient of `4.0e4` metres of fall per radian, and the threshold is a measured
quantity: across three seeds it is the 95th percentile of the river-cell
gradient distribution, and it is within 15% of itself at level 5, so it is a
property of the terrain rather than of the grid. That choice is what makes the
term discriminate. Normalizing on the maximum gradient instead would compress
the whole population into confinement between 0.7 and 1.0 and the law would be
a near no-op; normalizing on the median would make gorges of over half the
world's rivers.

Meander displaces each interior vertex perpendicular to its run, sampled from
a **position-continuous** noise field on the sphere — four octaves at spatial
frequency 24, so a meander wavelength is about 0.04 radians, a couple of
cells. It is emphatically not sampled by room address. Address-hashed noise is
exactly what makes the existing room `wetness` axis unable to form a connected
watercourse: sibling rooms are independent by construction, so a band edge
drawn from it would speckle and flip a walker in and out of "bank" room by
room. Amplitude scales with confinement, so a low-gradient reach wanders and a
steep one runs straight — real hydrology, and free, since the gradient is
already in hand.

The whole field costs one new seed-derivation label, `terrain/channel-meander`,
and no new draws: it is hash-noise sampled by position, and every other input
— discharge, gradient, the downhill graph — is committed state the world
already held.

## What was predicted, and what was measured

Three of the campaign's five hypotheses could be measured at this stage; the
other two need consumers this stage deliberately did not move. All were frozen
in the specification before the code that could move them, each with a floor
*and* a ceiling.

**Channel area — confirmed.** The fraction of seed 42's land area classified
`channel` was predicted to fall in `[0.005%, 0.5%]`. It reads **0.025931%**,
and every one of the 64 probe worlds lands between 0.02% and 0.05%. The
confirmation is a population result rather than a single-world one, and it
would have confirmed at the pre-calibration coefficient too — which is the
point of fitting on a *point* statistic and predicting an *aggregate* one.
Fitting the widest channel's width does not determine the integral over the
whole discharge distribution, and that independence is what made the
prediction worth stating.

The denominator deserves naming, because a different defensible estimator
gives a different answer by two orders of magnitude. Sampling at cell centres
reads **3.29%** of land on seed 42 — 127× higher, and above the stated
ceiling. That reading is a sampling artifact rather than a measurement: the
polylines are built *through* cell centres, so a per-cell sample lands the
sample point on the very feature whose area it estimates. The reported figure
integrates the channel tube instead — each segment's arc length times its
width — and is cross-checked against the shipped band predicate by stepping
across it, agreeing to within 3%.

A consequence worth stating plainly: `water_kind == River` and
`transverse_at == Channel` disagree for nearly half of river cells **by
design**. On seed 42 only 364 of 700 river cells read `channel` at their own
centre. A polyline runs *through* a cell, not *across* it.

**No speckle — confirmed.** A straight transect across a channel was predicted
to yield a monotone band sequence, with no band re-entry, in at least 99% of
samples. It reads **1.0000** on all 64 worlds: zero re-entries, which is the
positive result the position-continuous noise field was chosen to produce.

**Longitudinal connectivity — falsified.** Walking downstream from a sampled
headwater, at least 95% of walks were predicted to reach a sink or the sea
without leaving the channel band. It read **0.9236** on seed 42 and **0.8983**
on seed 7, and only **4 of 64** worlds cleared the floor.

The mechanism was neither the width law nor the network dropping runs — zero
walks left the network at all. It was an **anchoring asymmetry**. A run's
first and last vertices are placed at their cells' undisplaced positions (a
source and a mouth are anchored), while interior vertices are
meander-displaced. A tributary *ends* on its confluence cell, so its mouth is
anchored; the trunk carries that same cell as an *interior* vertex, so the
trunk's copy is displaced. Two runs joined in the drainage graph were
separated in space by a median of 1.78e-4 radians — about **4.5 channel
half-widths**. A walker crossing that gap is out of the water at the midpoint,
and for 12 of 15 joins was out of the water *at the mouth vertex itself*, so
no cleverer routing rescues it. The rivers were connected in the graph and
discontinuous on the ground.

## The repair, and that it came after unblinding

**The falsification above is the reading the measurement produced, and it
stands.** What follows is a design change made *after* seeing that result, on
an explicit authorization, in its own commit with its own before-and-after.
The pre-repair numbers are kept — here, in the retrospective, and in the
metric's own documentation — because a number that moved after unblinding is
only readable next to the one it replaced.

The change is one assignment: a tributary's terminal vertex takes the trunk's
*displaced* position for the shared cell rather than the cell's base position.
The two polylines then meet exactly, not merely nearby.

| axis | before | after |
|---|---|---|
| channel area, seed 42 | 0.025931% | 0.025938% |
| connectivity, seed 42 / seed 7 | 0.9236 / 0.8983 | 1.0 / 1.0 |
| worlds clearing the 0.95 floor | 4 of 64 | 64 of 64 |
| band monotonicity | 1.0000 | 1.0000 |

That the area moved by less than three parts in ten thousand — entirely the
confluence segments' changed length — is the evidence that no width, band or
confinement constant moved with it. It was checked directly: hashes of every
non-terminal vertex and every band edge are bit-identical between the two arms
across three worlds.

**And the resulting 1.0 means less than it looks like.** Connectivity is now a
constant rather than a measurement, because both of its failure branches are
unreachable. The join crossing is degenerate by construction: the two lines
are exactly coincident, so there is no gap to fall through. The second branch
— a walk continuing into a cell no run owns — *never could have fired*, before
the repair or after, because a cell with a river downhill is always a
non-final vertex of some kept run. It measured zero in both arms.

A preregistered axis satisfied by construction is a different result from one
satisfied by the world behaving well, and the two are indistinguishable in a
table of numbers. Saying so is worth more than the 1.0.

## A correct measurement, read wrongly, twice

The monotonicity result carries a companion reading that counts *any* band
re-entry rather than stopping at the transect's own channel. Before the
repair, 17 of 64 worlds cleared 0.99 on it; afterwards, all 64 do.

The interesting part is what the violations were. The measuring pass
instrumented every one and found that each coincided exactly with the
**nearest polyline changing** — and concluded that the transect ray was
entering a *neighbouring* river. An independent reviewer reproduced the
instrumentation and confirmed that reading.

All 14 violations across three worlds actually attribute to the originating
line's own **confluence partner**.

The measurement was never wrong. A different line does win, and it is never
the line the transect started from — every digit reported was correct. The
interpretation conflated *a different line wins* with *an unrelated river*,
and at a confluence those are exactly the two things that are not the same:
the different line **is** the trunk this one flows into. Two independent
parties running the same correct instrument arrived at the same wrong reading,
because the instrument reports which line won and cannot, unasked, tell a
stranger from a relative.

It surfaced only because a repair moved the number.

## Where this stops

This is the first of several stages, and it moved no consumer. The room schema
still reads `locale/room/v2`; both scene schemas still read `v1`; the map, the
locale and the walk all still answer the old cell-scale question. What exists
now is the producer: a network on the globe, a band predicate any position can
be handed to, and three instruments in the laboratory watching it.

Still ahead: the room reading its transverse position, and the epoch that
implies; gallery forest conditioned on *bank* and *floodplain* rather than
drawn two-in-seven by dice — and never on the channel, since a forest planted
in the water is not a gallery forest; and the network reaching the scene
schemas additively. Two hypotheses wait on those consumers: whether the ford
actually exists at a useful rate, and whether riparian conditioning leaves the
global formation fractions alone, as a sub-cell descriptor change should.

The lake is deliberately untouched. Through-flow lakes still classify as
`River`, so the naive reading of this campaign — rivers become polylines,
therefore `River` becomes a polyline — would have converted every lake into a
line. The network carries rivers only. Ocean, playa and lake remain faces,
which for them is the right carrier and always was.
