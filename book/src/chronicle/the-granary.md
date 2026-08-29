# The Granary

The deep-history bake knew everything about a community's year except when
things happened in it. It grew populations by logistic rates, resolved raids
by dominance margins, folded stores into raiding strength — and dated every
event it committed to the whole-year step in which its cause had fired. A
founding, a raid, a collapse: all of them landed on the year line like beads
on a rod, no two distinguishable by anything but their place on it.

The Granary gave the year an inside. An authored harvest curve — phase
derived from the latitude machinery climate already runs (southern cells
harvest half a year out of phase with northern ones), amplitude keyed on the
coarse biome class each cell already carries — turns food from a yearly lump
into a rate over the year. The granary stops being a year-boundary scalar and
becomes a running stock: production accrues across twelve sub-year phases,
consumption bleeds across the same twelve, and what a community owns in
month five is not what it owned in month one. Raid checks sample that curve
at every phase. The raid rule itself is untouched arithmetic; only the
question *when is it asked* changed — and the answer turned out to be the
whole difference.

## What the finer clock bought

Events now carry sub-year stamps: a founding is dated by the day its cause
fired, a raid by the day its threshold crossed. Two consequences were
preregistered and measured.

**The founder handle does not trim.** The Ell had left the founder-handle
question honestly open: the identity key collides wherever a raided founding
is succeeded same-year same-site by its twin, and only post-founding facts
(`ended`, `peak_population`) separated them — fields whose recomputation
forces a rename epoch. The hope was that day-grain stamps would separate the
twins naturally. Measured over seeds 0–2999: **2261 worlds still collide,
5039 founders lost.** The twinning is *same-phase*: the successor is founded
in the same resolution pass as the raid that displaced its predecessor, so
the finer stamp is identical too. The tail stays, now knowingly rather than
provisionally.

**Raids are seasonal — but not for the reason we guessed.** The
preregistered hypothesis held that raids cluster on the hunger side of the
year. Half of it passed emphatically: raid stamps concentrate in the annual
cycle at mean circular concentration **0.914** (n = 984 worlds, z = 542
against uniformity). The other half failed cleanly: the depleted-half
fraction sits at 0.5066, indistinguishable from chance. Raids cluster in
time because raider and victim share latitude and therefore season — and the
model prefers to strike when targets' stores are *full*, not when anyone is
hungry. Opportunity, not desperation. Shipped as a finding per decision
0016; nothing was retuned to rescue the prediction.

## What moved, and what it cost

The campaign's epoch bill was paid in full and on purpose:
`history/bake/v2` → v3 (committed history changed, stream consumption order
did not); seed-42 galleries and history fixtures regenerated; the history
showcase repointed to cell 10626 — twelve kobold steadings stacked on one
another's ruins across eighteen centuries, a clearing nobody ever chose but
everyone fled to; founder handles renamed. The Gnomon injection battery was
re-authored locally as a pilot under Nathan's authorization (its H1 recall
assertion resumes when `gnomon-injection.sh` re-runs on lefford), and its
witness pin re-read a fifth time: 70/120 = 0.5833, −0.37 SE — the fifth
reading of one unchanged report, all five within one standard error of the
bar. The instrument remains underpowered; the diagnosis holds.

## What comes next

The rethink this campaign began has two more layers registered: inter-annual
weather memory (bad-year runs integrating across years into cross-year raid
flurries — new draws, a v3-stream consumption change) and full event-level
resolution. And beneath both, out-of-band, the world's clock itself is
moving to integer seconds; this campaign deliberately kept f64 days-of-bake
so that migration lands on a quiet field. When it does, the calendar becomes
a function of the type rather than a convention, and the last float-key
sentinels in the history domain retire.
