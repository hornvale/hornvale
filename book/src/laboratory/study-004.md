# Study 004: The Census of Faiths

Ten thousand worlds, unpinned — the same seeds 0 through 9,999 every earlier
census has walked, each now carrying a flagship pantheon derived from its
observed phenomena and structured by its emergent society (Campaign 5). The
metric registry grows by four: how many deities a pantheon holds, whether
its cult is organized or folk, whether it is ranked or flat, and whether its
head deity's tenet is eternal or cyclic. As with every census before it, this
one asks the known-answer questions first — because an instrument that lies
about what it already knows cannot be trusted about what it doesn't — and
then the genuinely unknown ones.

**A note on scale and provenance.** This chapter's headline numbers come
from a 10,000-seed run of `studies/census-of-faiths.study.json`, executed
once by hand at author time, exactly as Studies 001 through 003 report their
own 10,000-seed headline runs. It is **not** committed and **not** part of
CI — regenerating it is this chapter's responsibility, by hand, the same
arrangement `census-of-skies`/`census-of-lands`/`census-of-peoples` use. The
charts embedded below are a different, smaller run: `census-lands-drift`,
the 500-seed sibling study CI reruns and diffs on every build
(`book/src/laboratory/generated/census-lands-drift/`), which now carries
these four faith metrics alongside the sky, land, and people metrics it
already tracked. Small numeric differences between a percentage quoted in
this prose and a bar in a chart below are sampling variance between a
10,000-seed and a 500-seed draw, not drift — only the `census-lands-drift`
charts are drift-checked.

Three worlds in the 10,000-seed sample hold no pantheon at all — the same
three seeds Study 003 already flags as placing no settlements at all: two
reaching exactly 0% habitable fraction, and (new since the Campaign Y2-0
placement fix) a third whose land is technically 0.01% habitable but too
fragmented for even one settlement to clear the placement floor. A world
with nothing to stand a religion on stands none; the remaining 9,997 worlds
carry a pantheon of at least one deity, as the no-cap floor guarantees.

## The calibrations hold

**Verticality is not authored per world; it is a threshold on a number the
census already had.** A pantheon is ranked — its top-salience deity flagged
`high-god`, presiding over the rest — exactly when the flagship's emergent
structure (Campaign 4b) reaches four or more castes; anything leaner gets a
flat pantheon of co-equal spirits. This is not a hypothesis about the
census, it is a fact about `genesis`, so `pantheon-verticality` must equal
that re-derivation from `flagship-structure-size` in every world —
`windows/lab/tests/calibration.rs` asserts it row by row over the 500-seed
`census-lands-drift` sample on every CI build, the fourth member of the
calibration family after belief⇔lock, band-count⇔rotation, and
subsistence⇔biome. Across the full 10,000-world census it holds without a
single exception, though the Campaign Y2-0 placement fix has flipped which
side of the split is now the majority: **7,729 worlds (77.3%) are ranked,
and every one of them has a structure of size 4, 5, or 6; 2,268 worlds
(22.7%) are flat, and every one of them has a structure of size 2 or 3.**
Zero mismatches, over 9,997 worlds with a pantheon — the same shift Study
003 already reported in `flagship-structure-size` itself (size 5 now the
overwhelming mode) propagates exactly through this threshold, as the
re-derivation guarantees it must.

{{#include generated/census-lands-drift/census-lands-drift-default-pantheon-verticality.svg}}

**The head deity's tenet is not authored either; it is read straight off
the world's rotation regime.** The most salient phenomenon a spinning world
observes is a returning sun, so its pantheon's head is cyclic; a tidally
locked world's sun never sets, so its head is eternal — the pantheon-head
extension of tier 0's original belief⇔lock calibration, now checked against
the whole pantheon rather than a single tier-0 belief. It holds exactly
across the full census too: **457 worlds (4.6%) yield an eternal head
deity, and all 457 are tidally locked; the remaining 9,540 (95.4%) yield a
cyclic head deity, and none of them are locked.** That 457 is not a
coincidence of rounding, and it is unchanged by the Campaign Y2-0 placement
fix (sky and rotation are upstream of settlement placement) — it is the
identical count Study 002 already reports for tidally locked worlds in this
same seed range, reached here by an entirely different metric (a pantheon's
head-deity tenet, not a rotation flag), the same fact seen from theology
instead of astronomy.

{{#include generated/census-lands-drift/census-lands-drift-default-head-deity-periodicity.svg}}

## The first unknown number: how many gods

Nobody knew, before this census, how large a generated pantheon would grow,
or how the no-cap floor would spread that size out. **Mean pantheon size:
3.05 deities (median 3, standard deviation 1.07), ranging from 1 to 7.** The
distribution is unimodal and left-leaning: 6.4% of worlds seat only their
single most salient phenomenon (a pantheon of one), 25.2% seat two, 35.8%
seat three — the mode — 23.7% seat four, and the no-cap tail thins fast
after that: 7.9% seat five, 1.0% seat six, and just five worlds in ten
thousand (0.1%) seat seven, the richest skies this generator currently
produces, thick enough with salient phenomena to crown a pantheon most
worlds never approach. That five-world tail is the esoterica the design
predicted: no ceiling was authored, and the census is what shows the ceiling
the *sky* imposes instead.

{{#include generated/census-lands-drift/census-lands-drift-default-pantheon-size.svg}}

## The second unknown number: cult form and verticality are separate axes

`cult-form` and `pantheon-verticality` are both read from the same
`SocietySummary`, but they are not the same question, and the census is
what makes the difference legible instead of assumed. Overall, **84.7% of
pantheons (8,472 worlds) are tended by an organized priesthood and 15.2%
(1,525) are folk animism held by the whole community** — organized cults
grew far more common with the Campaign Y2-0 placement fix, tracking the
same shift toward higher `flagship-structure-size` Study 003 reports, since
priesthood asks only "does a shaman caste exist," a coarser threshold than
verticality's four-strata bar that a lean structure clears more easily.

{{#include generated/census-lands-drift/census-lands-drift-default-cult-form.svg}}

Crossing the two axes shows exactly how much room separates them: every
flat pantheon in a folk cult (15.2% of the census) behaves as expected, and
every ranked pantheon is organized (77.3%, with zero ranked-but-folk
exceptions in the sample) — a stratified enough society to crown a high god
always has a shaman to tend the cult. But **7.4% of the census (743 worlds)
is organized *and* flat**: almost the entire size-3 population (743 of 744),
a shaman caste presiding over a three-rung structure (worker, shaman,
chief) that never reaches the fourth stratum verticality requires. An organized priesthood, in other words, is
still not a leading indicator of a ranked pantheon in this generator — even
after the Campaign Y2-0 placement fix pushed most flagships toward the
five-rung structure that verticality rewards, roughly one in eleven
organized cults (8.8% of them) still tends a flat pantheon of co-equal
spirits, a combination the design's two independent thresholds (strata ≥ 4
for rank, a shaman caste for organization) permit and the census is the
instrument that keeps counting it.

## What the census does not yet say

Every pantheon in this sample belongs to the flagship alone — the scatter's
other settlements are named, populated, and structured, but raise no
pantheon of their own, a scope limit of the composition root (spec §13),
not a claim that a Census of comparative religion across a whole scatter
would be uninteresting. The pantheon draws only from celestial and ambient
phenomena; earth gods from tectonic unrest and sea gods from tidal fields
are explicitly deferred substrate, not yet a source religion reads. Theology
here is static — computed once at genesis and never revised — so this
census cannot yet ask how a pantheon would drift under a religious history,
a question that waits on the Year-2 event ledger. And the whole population
is single-species: goblin, the same restriction every earlier census
carried, unrelaxed until the species-psychology substrate arrives. For now
the fourth leg of the census stands next to the sky, the land, and the
people: built, calibrated against two facts the code already guaranteed,
and honest about the boundary between what it measures and what it has not
yet been asked to explain.
