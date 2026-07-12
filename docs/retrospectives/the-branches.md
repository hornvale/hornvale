# The Branches — retrospective

**Merged:** 2026-07-10

**The re-baseline was staged deliberately, and that was the right call.**
Lifting goblin's proto-roots to the family level and adding two new
species were never going to land as one clean commit — the moment
hobgoblin and bugbear existed, nineteen workspace tests broke at once:
byte goldens, hard-coded two-species assertions, and six calibration
studies, all downstream of a world that had quietly assumed exactly two
peoples since the roster was invented. The controller's call was to
accept a red workspace across the species-authoring and composition-root
commits and do the actual re-baseline once, after the wiring settled the
world's final shape, rather than re-pin every intermediate state twice.
That held. The alternative — re-baselining after species landed and
again after wiring — would have meant deriving throwaway "truths" about
a world that was about to change again, work whose only output is a
diff nobody needs. Staging the messy middle and re-baselining once
against the *final* shape is the pattern worth repeating whenever a
change is known in advance to touch the same fixtures twice.

**A design defect surfaced from the inside, not from a review.** The
plan scoped hobgoblin and bugbear as data — two more rows in a species
registry, wired through machinery three years of prior campaigns had
already proven. What it did not anticipate, because nothing before this
campaign had ever placed more than two peoples on one globe, is that the
first authored draft of hobgoblin's psychology scored better than
goblin's on every term the settlement-suitability formula reads at once:
not a stronger competitor, a strict dominator, leaving goblin and bugbear
zero settlements across every seed tried. This was caught by simply
running the world and looking at the numbers, not by a review of the
authored vectors in isolation — a reminder that some defects are only
visible in the composed system, and a species vector that looks
reasonable line by line can still be a trap in aggregate. The first fix
(re-authoring the psychology to cede real niches to each people) resolved
the domination but not the whole problem: the placement algorithm's
spacing rule could still box a nominally-niched people down to a single
founding settlement, which took a second design pass — reframing the
placement problem itself as a resource-allocation question rather than a
per-cell fitness contest — before a scoped, deterministic fix (a founder
floor guaranteeing every people its own best cell before the competitive
fill runs) actually unblocked the campaign. Two passes to find the right
layer to fix at is not a failure of the first pass; a Pareto-dominance
defect and a spacing-induced boxing-out are genuinely different bugs that
happened to present as the same symptom.

**The defect earned a frontier entry instead of a silent workaround.**
The founder floor is deliberately the narrowest fix that unblocks this
campaign — it says nothing about whether four, or forty, peoples sharing
one world is a solved problem in general. That wider question (a
placement algorithm optimizing one cell's suitability at a time
*manufactures* monoculture; the realistic target is diversity that
survives, not fitness that maximizes) was captured as its own frontier
essay rather than left as an unexplained constant in the composition
root. Recording the general problem the specific fix stands in front of
is cheaper the moment it's found than it will ever be later, when the
context for *why* the founder floor exists has faded and someone has to
reverse-engineer the reasoning from a diff.

**Calibrations were re-measured, not defended.** Every calibration this
campaign's re-baseline touched was re-run against the four-people world
and re-pinned to what actually came back, per the project's standing
rule that a preregistered claim gets measured, never tuned to keep
passing. Two findings are worth naming because they are genuinely new
behavior, not restatements of old ones under new numbers: the seed-swept
family battery's clean-outgroup and divergence-ordering claims both came
back true only in the aggregate (998/1,000 seeds; a population-level mean
ordering that holds on 588/1,000 individual seeds), and a pre-existing
calibration — "a spinning world's first-minted belief is never eternal" —
now fails on 9 of 477 spinning worlds in the 500-seed drift population,
because bugbear sorts alphabetically before goblin and the founder floor
guarantees it a flagship on every seed, so bugbear's pantheon commits
first and can occasionally crown an aperiodic night-star instead of the
sun. Nothing here is a bug; it is exactly what the family and the founder
floor were built to do, observed for the first time because this
campaign is the first thing that ever gave bugbear a seat at the table.
Each finding is pinned as measured, with the mechanism explained in the
test that pins it, rather than either forced to the old, cleaner-looking
number or quietly loosened until it passed.

**A spec's own prose can overcommit a mechanism, and it takes a close
final read to catch.** The campaign's earliest planning pass described
voice loudness as biasing *which* sound-change rules a lineage's cascade
draws — a tidy, symmetrical story (loud peoples fortify, quiet peoples
lenite) that turned out not to be what the code does at all: cascade
rule selection is uniform and loudness-blind; loudness's only lever is
the phoneme inventory a lineage draws, which then gates how much of a
cascade's proposed output survives and how much falls to nativization.
The discrepancy was flagged early, during the first task's own review,
and carried forward explicitly as a note for this closing pass rather
than fixed in place and forgotten — which worked, but only because it
was written down as a tracked line item rather than trusted to memory
across ten more tasks and a mid-campaign merge from main. A prose
description of a mechanism is a claim like any other; it deserves the
same "measure, don't assume" discipline the code itself gets, and a
one-line note at the moment of discovery is cheap insurance against
losing the thread by the time someone is finally in a position to fix
the words.

**Freshness sweeps have a bigger blast radius than the file list
suggests.** This closing pass was scoped to one domain chapter, but
regenerating the seed-42 artifacts as part of the standard close
revealed that a world-wide re-baseline — the same one this campaign
staged so carefully in code — had quietly gone unaddressed in prose:
hand-authored gallery essays quoting "verbatim" almanac excerpts, and
two other domain chapters citing the same worked example, all still
carried names, counts, and one entire worked claim (a locked-world
pantheon's cult form) from before hobgoblin and bugbear existed. None of
this is caught by the CI drift check, which only re-diffs the files its
own regeneration commands overwrite — a hand-quoted excerpt of a
regenerated file is invisible to a tool that only checks whether the
regenerated file itself changed. The lesson generalizes past this
campaign: whenever a change is known to alter a canonical example (a
seed-42 flagship name, a worked demonstration), the search for what else
quotes that example needs to run wider than whatever file list a task
brief happened to enumerate in advance — the same "blast radius is a
claim about completeness" lesson prior retrospectives have already
recorded once, observed here one level up, in prose rather than code.
