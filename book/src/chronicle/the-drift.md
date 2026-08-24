# The Drift

*A drift is the horizontal passage that follows the vein. It is the part of a
mine that connects.*

[The Stope](./the-stope.md) gave the underworld extent — a band stopped being
one interior-less point per column and became a drawn run of floors, a column
became a branch with its own character and its own barrier, entrances became
plural. Its closing premise check for the next campaign asked how much of that
a player can actually reach, and the answer on seed 42 was:

```text
open entrances                        511
levels reachable from them          1,496
levels that exist                  21,328
                                    ---
reachable per open entrance          2.93
```

You open a door into a cave system and reach 2.93 of its two dozen levels.
**7.0% of the underworld the campaign had just built.**

## The defect was a misreading of the units before it was a defect in the code

The first account of that number was wrong, and the shape of the error is why
this campaign exists rather than a tuning pass.

It was read as *the underworld is full of sealed rooms* — a percolation
problem, a connectivity density to be dialled up. Every word of that is a
category error. Nathan's correction: **a chamber is not a room, it is a
level** — a screen-filling map with its own interior, the thing
`windows/vessel/src/underworld_level/` already generates. And a **branch** is
not a parallel corridor; it is a NetHack-style branch, an *alternative
pathway* — the Gnomish Mines beside the Dungeons of Doom, not a second hallway
on the same floor.

The measurement was right to the unit. Every word describing it was wrong, and
the wrongness was invisible because both readings produce the same arithmetic.
With the units corrected the diagnosis is not percolation and it is not
tuning:

> **A branch is levels 1..n. There is no level 7 that does not exist.**

Levels were not contiguous. That is a different defect from the one anybody
was looking at, and it has a different fix.

## Five gates specified a contiguous dungeon and a sixth punched holes in it

`chamber_exists` gated in six steps. The first five are structural and each
has a reason: the lattice's width, the lattice's height, the drawn branch
count, how deep the rock allows, and this run's drawn length. Together they
describe a fully determined, contiguous dungeon — branches `0..drawn`, bands
`0..deepest`, levels `0..drawn length`.

Step six was:

```rust
chamber_stream(seed, addr).next_f64() < EXISTENCE_DENSITY   // 0.5
```

**A 50% coin, punching random holes through a shape five other gates had
already specified correctly.** A branch of twenty-four drawn levels realized
about twelve of them, scattered, and a descent needs an unbroken chain of
survivors down a column. That is the whole of the 2.93.

Nothing in the passage graph was at fault, and the campaign checked before
concluding: `passages_from` offers up to four neighbours and every pair of
adjacent *existing* levels is connected automatically. No passage is ever
drawn or withheld. The only randomness in the entire connectivity story was
whether a level exists — so there was nothing to tune in the graph, and one
spurious draw to delete.

The fix was the deletion of that line. `EXISTENCE_DENSITY` is gone, not
lowered. Reachability on seed 42 went from **7.0% to 91.4%** in that one
commit, and the shape of the movement is what confirms the diagnosis: levels
roughly doubled (21,328 → 42,820) while reachable levels rose twenty-six-fold
(1,496 → 39,140). The coin had not merely been removing levels. It had been
severing the graph.

## Entrances were private worlds, and that had to be settled before anything else

The baseline probe found something the spec had been written without seeing.
Every lattice draw carried the entrance, and `chamber_exists` gated on
`branch_count_of(seed, cell, addr.entrance)` — so **each entrance realized its
own private sublattice**. The tree said so and called it deliberate. Seed 42's
21,328 levels were a sum over four disjoint worlds; entrance 0 alone held
14,976.

The model the campaign was designed from has a Blacksmith's Cellar and a Cave
under the Well — two Undercroft entrances — descending into the *same* Spider
Cave. Per-entrance realization cannot express that. Worse, it made the
campaign's central promise weaker than it read: connectivity guarantees proved
inside an entrance's private world say nothing about the system.

So `entrance` left the lattice entirely. It survives only as *which aperture
you came in by* — how many a system has, and where each one lands. The address
now reads `(cell, band, branch, level)`, and a system has one lattice with
several doors into it. Every world's underworld relocated, which the campaign
accepted because it was already regenerating every world for the deleted coin:
the epoch buys the correct model on a regeneration that was happening anyway.

**The epoch's mechanism was not where the campaign first put it.** The obvious
carrier was `chamber_key`, which formats the whole address into a stream
label — and with the coin deleted, `chamber_exists` draws nothing, so
`chamber_key` derives nothing. Its one remaining production caller *prints*
it. Changing it relocates precisely nothing. What carries the epoch is the
legs that really are consumed: `run-floors`, `branch-count`,
`branch-character`, `branch-barrier`, `entrance-count` and `entrance-mouth`
all to v2, `branch-root` retired, `band-descent/v1` new. **`chamber` stays at
v3 and was deliberately not bumped**, because bumping a label nothing derives
from mints an empty epoch — a label recording a discontinuity that never
happened through it, which is exactly the case a monotonic label check cannot
catch.

That correction also caught a committed artifact falsified without anyone
touching the file. `docs/audits/underworld-lattice-seed-panel.md` called its
`key` column *"the real derivation key"*, three times in generated legend text
and once in hand-authored prose. As of the deletion it was a formatted address
that derives nothing: a string presented as a derivation key, in a
drift-checked artifact, deriving nothing. The campaign's own standard turned
on itself.

## The descent verb lost an outcome, and that is a player-facing cut

A cave was **sealed** when its chambers lost their existence coin flips.
`hornvale-vessel`'s `delve_has_three_distinguishable_outcomes` passed before
the deletion and failed after it, and its panic named the loss exactly: *no
sealed cave found in seed 42's terrain*.

With the coin gone, every cave in shape realizes chambers. This is not
rare-now; it is **impossible**-now, and the probe had already proved it
without anyone reading it that way — `systems_with_open_mouth == systems` on
all three panel seeds, 874/874, 1681/1681, 1266/1266. Re-measured at close
over thirty worlds: **0 of 48,316 caves are sealed**, against the ~0.485 a
committed prediction still named.

Nathan's ruling was to accept two outcomes and build restricted passage later.
The descent verb therefore ships with two distinguishable outcomes rather than
three, the test was **amended rather than deleted** — it now scans every
cave-bearing cell in the fixture and reddens if a sealed cave ever returns
while it still claims two — and the owed work is filed. **A campaign that
removes a player-facing outcome and does not write that down has misreported
itself**, so it is written down here: locked doors, collapses that magic can
clear, boss encounters and the rare chamber that stays lost with something
worth finding are now *owed* rather than merely nice to have. The campaign's
non-goals list had already named restricted passage; what it did not know was
that it was removing something the game already had.

## Bands connect by drawn edges, and the guarantees hold by construction

Deleting the coin makes each band's runs contiguous. It says nothing about how
one band reaches the next. The old rule descended on a fixed sequence; the new
one reads a drawn edge set, `chamber/band-descent/v1`, with two guarantees
that hold **by construction rather than by a repair pass**:

- every branch in the upper band draws at least one child below, so nothing
  descends into a dead end;
- every branch in the lower band draws at least one parent above, so nothing
  is unreachable.

Take the union of those two surjections; sort and dedup. A draw-then-patch
loop was rejected outright, and not on grounds of elegance: an order-dependent
repair is a determinism hazard as well as a correctness one.

The key spells `cell / branch / band / role` with role in `{child, parent}`,
because one lattice place answers two independent questions. A single stream
with two ordered draws would reintroduce exactly the order-dependence the
design rejects, and would degrade at the Nadir where there is no child draw.

The campaign's one open dial was *how many edges beyond the guaranteed
union* — zero extra is a tree, many is a mesh. Measured over 3 seeds × 512
cells × 4 band pairs = 6,144 pairs, with **no extra draw added and no weight
retuned**:

```text
mean out-degree per upper branch      1.3648
mean edges per band pair              2.1702
band pairs drawing the MINIMUM        86.75%   (max(upper, lower))
1x1 pairs with exactly one edge       36.1%    (the modal transition is one staircase)
closed form                           E[edges] = upper + lower - 1
```

The union alone lands squarely at the tree end of the range. The closed form
was re-derived from first principles and brute-forced over the full joint
space rather than fitted: child edges are pairwise distinct by their first
coordinate and parent edges by their second, so duplicates can only be
cross-set, and each index contributes at most one coincidence. Predicted
13,329 edges against 13,334 observed — 0.3 σ, which is itself evidence the two
draws are independent, which is what the role word is for.

**The finding that will matter later is the sample size, not the mean.** `4×4`
is the only width pair where mesh-versus-tree is genuinely live, and it occurs
**15 times in 6,144 — 0.24%**. Any future calibration of that dial has to
*construct* widths; a panel scan would be tuning against fifteen samples while
looking like data.

## The last two percent, and the guarantee that was vacuous where it mattered

Rewiring descent onto the drawn edges took reachability to 99.60 / 99.59 /
99.67%, with 3.09 / 2.08 / 2.21% of systems short of complete. The residue was
not noise, and the discriminator is exact: of seed 42's 120 unreached
top-band levels, **120 sit in branches no open mouth landed on and 0 in
branches that have one.**

The gap was in the guarantee *set*, not in either guarantee. "Every branch has
a parent above" is **vacuous at the top band**, which has no band above it — so
an Undercroft branch that no entrance names and no lower branch links back to
is simply orphaned, while both guarantees continue to hold over all sixteen
constructed width pairs. The spec's own list was incomplete, which is a
different and quieter failure than a rule being violated.

The third guarantee closes it: **every branch in the top band is named by at
least one entrance.** Achieved the same way as the other two — the aperture
set is sized `max(free_draw, top_band_width)` and the aperture-to-branch map
is a bijection by counting, so surjectivity is structural rather than
patched — and it took the residue to **zero on all three seeds**.

```text
                        before        after §4.1      after descent     final
whole-world reachable      7.0%          91.4%            99.60%       100.00%
systems below 100%          --              --             3.09%         0.00%
unreached levels by band    --              --        120 @ rank 0       EMPTY
```

`levels` did not move across that last step (30,537 / 59,227 / 48,294), so the
numerator rose and the denominator stood still: the rise is not an artifact of
the measurement. And the zero is live rather than saturated — two independent
mutations of the construction (removing the width floor, breaking the
bijection's disjointness) put systems-below-100% back at 3.32 / 1.90 / 2.69%
and 0.92 / 0.42 / 0.47% respectively, each reddening both the constructed-width
test and the probe's gated arm with a populated band histogram.

One asymmetry died with `root_floor_of`, and it was not the edge case it was
first called. A side entrance used to size its branch pick against one band's
width and then land in a different band with a different width, so a mouth
could name a branch its landing band never realized. Measured across the
panel, that was **47.8 / 53.0 / 51.9% of every drawn side-branch mouth** — the
majority case, diluted to invisibility in the aggregate by the head mouths
that are exempt. It now reads 0 of 543 / 1,065 / 749, correct by construction
rather than by a stand-in, and mutating the landing band one rung deeper
reintroduces it at 67–71% and reddens two instruments.

## The Stope's headline finding, resolved

The predecessor campaign's central result was that the deep underworld
essentially could not be walked to:

```text
                                        The Stope    The Drift
reaches the Nadir BY WALKING              0.359%      28.68%
reaches the Underdeep BY WALKING          0.440%      35.21%
```

The Stope also established, by route ablation and a density sweep, that the
0.359% was *not* percolation against the coin: the pure spine-descent route
delivered exactly zero branches, and the Nadir was reachable at all only
through a side entrance whose root band was picked uniform over the parent's
realized bands — roughly one door in five opening directly into the bottom.
That door is gone: every entrance now lands at level 0 of the top band, and the
deep is reached by walking down to it.

Nearly a third of the underworld's branches can now be walked from the surface
to the bottom of the ladder, against about one in three hundred before. **This
campaign resolved the previous campaign's headline finding**, and the mechanism
is the one The Stope's ablations had already identified as the lever.

## The epoch moved one artifact

Every chamber in every world relocated, six seed-derivation labels took an
epoch suffix, one retired and one is new. Across the whole campaign, exactly
three drift-checked artifacts moved: the stream manifest (the labels
themselves), the type-audit report (a handful of signatures), and the
underworld witness page. The three seed-42 almanacs, the elevation map, every
lab study, the Domesday survey and the committed client fixtures are
**byte-identical**.

That is The Stope's demonstration repeated with a stronger premise, and it is
worth stating because it is the design paying out: a chamber is never stored.
Existence and content are pure functions of an address, so moving all of them
moves no serialized byte — and the surface world, which reads the depth ladder
and the rock column but never asks whether a chamber exists, does not notice
that the underworld beneath it changed completely.

## What this is not

Restricted passage is owed, not shipped — the campaign made a system
structurally connected and *restricting* passage across an edge is a better
layer for having something to restrict. There is no signage: the per-branch
character axis is the hook and it exists, but naming was not this campaign.
There is no content in the branches — no Duergar Kingdom, no Drow Empire; this
ships the structure that makes them addressable. And the three-way terminology
rotation is half-done on purpose: the depth ladder is now one kernel `Band`,
the rock ladder is parked on `Horizon` — a soil-science word doing a crustal
job — and the correct end state, `Band` / `Stratum` / `PelagicZone` across some
750 sites, is filed rather than built.
