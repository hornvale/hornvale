# The Margin

*The follow-up batch, and what a campaign's leftovers turn out to be worth.*

The Purview shipped a coarse chart of the rooms around a possessed player and
left a list behind: six carried Minors, two owner calls, and two questions its
own reviews had raised and deliberately not answered. This is that list, worked
through. Nothing here is a feature. It is worth a chronicle entry anyway,
because three of the items turned out to be about honesty rather than tidiness,
and one of them was a lie the previous campaign had just finished telling.

## Two nouns for one thing

The chart's legend named the biome by its internal slug —
`tropical-seasonal-forest` — while the prose named it `tropical seasonal
forest`. A player standing in one place could examine two different nouns for
the same ground and get two different answers, which is a small wart on its own
and a large one given what the campaign had claimed. The Purview's thesis is
that map and prose are *two grains of one lens*, and its sharpest test is that a
noun surfaced at both grains resolves to the same datum. The most obvious thing
on the chart was the one thing that test could never fire on, because the two
grains spelled it differently.

The legend now carries the human name and the machine catalog keeps the slug,
which is the split that should have existed from the start: `biome_legend` is an
index contract for consumers, and `legend` is text for a person. The biome is
now a shared noun, and the thesis test asserts it specifically.

## A refusal that named a bound it did not have

Asking the chart to zoom out further than the world allows should say so. The
Purview's own review had already caught one version of this — an internal error
about the canonical grid reaching the player — and fixed it. This batch fixed
the *next* version and then, in review, was caught inventing a third: told to
zoom out by more than four billion rungs, the chart replied that it "tops out at
4294967295 rungs coarser," a number that is true of `u32` and false of the
world, and that the very next command disproved by refusing at eight.

The lesson is not about integer parsing. A message that names a bound is making
a claim, and a claim invented to be more helpful than "no" is still a claim that
can be wrong. The refusal now declines to name a number it does not own and lets
the real ceiling — `depth − globe_level` — produce the real message.

## Where a drift check can and cannot reach

The Purview's committed chart was strictly drift-checked by CI while its
siblings were excluded, and nobody had written down why the siblings were
excluded. The reason turns out to be sharp: quantizing floats at every emit
boundary makes *quantities* portable across platforms, but a biome is not a
quantity. It is a branch taken by comparing a float to a threshold, and two
libms that differ in the last ULP can take opposite branches. Quantizing after
the comparison cannot undo it.

So the rule, now recorded as [decision
0078](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0078-thresholded-classification-artifacts-are-platform-local.md):
an artifact that *is* classifications is drift-checked platform-locally, by a
byte pin in the producing crate that runs on every commit; an artifact where
classifications are incidental to a broader signal stays in the CI diff and
accepts the risk. What the exclusion gives up is the cross-platform comparison,
which was never sound for that data. What it keeps is the check that actually
mattered — that a field order, a noun, or a band boundary has not moved.

## The showcase that showcased nothing

The chart's public face was a neighbourhood of seed 42's flagship settlement:
thirty-one cells, every one of them river, shelf, and tropical seasonal forest.
Correct, deterministic, and a uniform field of `+`. It demonstrated the shape of
the lattice and nothing about the world.

The gallery now shows three observers — the flagship kept, because it is where a
possession actually starts and it carries the settlement mark; a coastline where
ocean meets forest and the boundary reads; and one that crosses a base-face
seam, where a third of the neighbourhood has no honest place on a flat chart and
the caption says so. Reaching them meant making the render addressable outside a
possession, so `hornvale scene surrounds --render ascii` now draws any room.
Honest accounting: one of the three charts fixes the stated problem. The seam
chart is a uniform field of `~` whose payload is its caption, and the flagship
one is the same uniform field the complaint was about — kept deliberately, and
labelled as such.

The batch also untracked a file that should never have been tracked: a prior
campaign's scratch decision-ledger, committed past its own gitignore by a close
that staged by path instead of by status. It had already cost something real —
The Purview's ledger was overwritten by it during an absorption, because two
campaigns' scratch had collided at one tracked path.
