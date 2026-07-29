# The Effacement

For most of this project's life, every room a possession entered opened with
the same three words:

> **You stand in** tropical seasonal forest — buttressed canopy — …

The Column had just made that sentence worse by making it more specific. At
sea, a walker floated on open water or hung in a coral reef, chosen by a
three-way match on medium and depth band. The obvious next move was more cases:
swimming, walking the sea floor, flying when a sky realm arrives, sitting in a
boat.

The obvious next move was wrong, and the reason is worth stating plainly.

## An unsourced claim

Ask where "stand" comes from. Not from the world: the renderer knows the
medium and the band and nothing else. Not from the creature: nothing in the
body layer says whether this thing has legs, fins, wings, or is asleep.

"You stand" is a **posture asserted by a template that computed nothing about a
body.** Adding "you swim" and "you walk the floor" would not have fixed that; it
would have made the unsourced claim more specific and therefore more confidently
wrong. A description cannot be made contextual by enumerating contexts it has no
way to detect.

The convention that tabletop read-aloud text must never tell players what their
characters are doing exists for exactly this reason, and screenplay slug lines
and naturalist field notes arrive at the same place from different directions:
describe the place; the body belongs to whoever owns it.

So the narrator withdrew.

> Tropical seasonal forest — buttressed canopy — in the lands of
> Qvooshtvoagootao. The sky above: Night. The vast moon is a smear of light.

> Open water — open blue water, sunlit, over a trough — in the lands of
> Qvooshtvoagootao. The sky above: Night. The sky is a low grey rain-deck.

The place opens the sentence. The clauses became a list rather than a run-on,
because a descriptor that no longer trails a verb has to stand as prose on its
own.

**What did not withdraw:** answers. `dive` still replies "You break the
surface", and it should — that is a response to something the player did, and
the command is its source. The line the campaign draws is between a narrator
answering an action and a narrator asserting a posture nobody asked about.

A sourced stance is still possible, and the seam is marked. It wants the
liveness layer to supply a real activity, which is a different campaign from
guessing.

## Ice is not dry

Two smaller honesties.

Permanent ice is land, so it drew the ordinary land clauses, and an ice sheet
read "sun-warmed, dry" — the same category error the sea had one medium over,
surviving because ice had been correctly classified as land and land clauses
are correct for land. Ice now reads its own micro-field: relief is the
surface's own shape, aspect is glare rather than warmth, and wetness is how
much snow the wind left.

> a crevasse field, glaring, scoured bare

## The invariant that said no

The last item was the almanac's `Visible bodies:` line, which listed the sun at
midnight beneath a flat overcast. The obvious fix was to filter the list by
what the occlusion had left.

A test refused it: `the_sun_never_leaves_the_visible_bodies_list`.

That test is not incidental. Its battery enforces **"coarse constrains fine"** —
the Constitution's rule that a refined tier may add structure beneath a coarse
one but never contradict it. Tier 0's `ConstantSun` claims there is a sun and it
is in the sky's bodies at every hour. `SkyReport.bodies` is therefore a
**roster**, and a superset the generated sky may only add to.

Filtering it would have been a retraction — a constitutional violation dressed
as a prose fix, and it would have shipped had a test not been standing there.

The list was right. The **label** was the lie. It now reads:

> This sky holds: the sun, moon 1, moon 2.

Which is true at midnight, true under a storm, and true at tier 0.
