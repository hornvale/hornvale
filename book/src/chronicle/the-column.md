# The Column

A question, asked plainly: can a ship pass above while sea elves dwell below?

The honest answer was no, and not because the sea was unmodelled. The world
knew the depth of every ocean cell, knew which zone that depth fell in, and
after The Shoal could describe each zone in its own words. What it could not do
was let a coordinate be **more than one place**. Standing at sea, a walker was
told they stood *in* a coral reef — a reef that might be a thousand metres
below them.

## Walking to the sea

The first thing to establish was whether anyone could get there at all, since a
mechanism nobody can reach is the defect The Tare had just finished naming.

A first attempt suggested they could not: four hundred movement attempts
visited six rooms. That reading was wrong, and wrong in an instructive way. The
walk cycled through eight compass directions on a mesh where every room is a
triangle with exactly **three** exits, so it spent most of its steps refused and
the rest oscillating between the same handful of rooms. Three hundred of the
four hundred moves had actually succeeded; they simply went nowhere.

Biasing the attempts — west, north-west, south-west, over and over, letting the
refusals fall harmlessly — the walker crossed twelve hundred rooms and arrived
at the coast. The line it printed on arrival is worth quoting, because four
campaigns are visible in it at once:

> You stand in coral reef — a coral head sunlit over a trough — in the lands of
> Qvooshtvoagootao. The sky above: Night. The sky is a low grey rain-deck.

The Shoal supplied *a coral head sunlit over a trough* where there had been
"broken terrain sun-warmed dry on a rise". The Occlusion supplied a rain-deck
belonging to that coordinate rather than the capital's, with the stars correctly
absent beneath it. The Formations is why a reef can be a community *at* a depth.
And a bare `w` moved the walker at all.

It also shows the defect exactly: *stand in*.

## A second way of being somewhere

The repair reuses a mechanism that shipped a week earlier. The Lintel gave
possession a **band** — `inside`, a state in which the ordinary rules of the
outdoors are suspended, entered by `enter` and left by `out`. A water column is
the same shape. The session gains `submerged`, entered by `dive` and left by
`surface`.

How deep the column goes is the sea floor's business. Fifty metres over a reef
holds only the sunlit water; three thousand holds three layers. The strata a
diver can reach are exactly those from the surface down to the one the floor
sits in — so diving past the bottom is refused by *naming* the bottom, since a
bare "you cannot" reads as a parse failure rather than the floor of the sea.

Above the floor there is only open water. A community lives on the bottom, and
floating a thousand metres over a reef is not being at the reef — so the
expression seen from a stratum keeps the cell's own formation only at the floor,
and reads `OpenWater` everywhere above it. That single rule is what makes one
coordinate into several places.

## The verb follows the medium

A walker does not *stand* in the sea. The stance now follows the medium and the
band together, which is the same category error The Shoal fixed one clause
lower down:

> **You float on** open water — open blue water sunlit swept by a current over
> a trough — …
>
> `> dive`
>
> **You hang in** coral reef — a coral head sunlit over a trough — …
>
> `> surface`
>
> You break the surface.

Three states, one coordinate: on it, under it, and on the ground when there is
ground. The ship and the sea elves are finally in different places.

## What is deliberately not here

Swimming. Lateral movement is refused while submerged, exactly as it is indoors,
and for the same reason — a step that carried the band sideways would leave the
session holding a depth that belongs to a different sea floor. The refusal is
diegetic rather than a shrug: *Not while you are under. Surface first, then
swim.* Making the column traversable is its own campaign, and it will want the
floor depth of the destination before it can be honest.

No world byte moved. The column is derived at observation time and stored
nowhere, which is why a change this structural touches no save and no census.
