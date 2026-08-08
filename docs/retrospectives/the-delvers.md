# Retrospective — The Delvers

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-delvers.md): three dwarves, one
family, the first occupants of the paced life schedule — and the discovery that
the realm gate places a kind at a cave mouth rather than underground.

This campaign's scratch ledger ran to 36 entries. It is git-ignored and dies with
the checkout, so what is worth keeping is promoted here.

## The headline: every measurement held, and every unmeasured mechanism failed

Five mechanisms were proposed during this campaign without measuring them first.
**All five were wrong.** Meanwhile every number the campaign actually measured —
the bind shares, the pairwise correlations, the cave-depth percentiles, the
before-and-after of the diet correction, the twelve-seed history panels — has
held, including under a re-run after an absorption.

1. **"Dwarves inherit the bind result from their mass class."** The spec
   generalised a three-kind measurement (*elevation binds on 100 % of land for
   goblin, gnoll and human*) to a roster it never covered. Hobgoblin is 74.8 kg —
   human's mass class — and binds on elevation on 74.77 % of seed 42's land, not
   100 %. **Mass sets the floor; the authored devotion decides the bind.** Same
   shape as `measure-the-population-you-apply-to`. Caught because the bind audit
   was built and run *before* any dwarf trait value was authored.

2. **"`MINERAL` is the right axis for a people that mines."** It is a *trophic*
   axis whose only other holders are the roster's two lithovores. The claim
   zeroed the xorn's measured stronghold, and the natural-looking repair was to
   invert a correct test in order to protect a false claim — a sibling assertion
   had even predicted this campaign would move it. Dwarves mine rock; they do not
   eat it, and this model has no extraction economy.

3. **"The survivors will crowd the contested food axes after the cut."** Refuted
   by arithmetic before it was tested: cutting two kinds *reduced* roster load on
   every axis (detritus −70 %, animal prey −41 %, plant forage −13 %).

4. **"Deleting a species reshuffles the other species' draws."** Refuted by
   reading `kernel/src/seed.rs`. `Seed::derive` is an FNV-1a hash of a label, not
   a sequential stream, so removing a species *cannot* shift another's draws. The
   codebase is built specifically to prevent the thing being proposed.

5. **"Long-lived peoples squat the map and quiet it."** The one explained to
   Nathan at length, with a tidy musical-chairs story and a table that fit it.
   Refuted by a twelve-seed spread: the nine-people world is *healthier* on
   median — +36 % tribute, +33 % occupations.

**The failure mode is identical every time: reading a before/after difference as
a cause, in a campaign that changes several things at once.** A difference
attributes to your change only if nothing else moved with it, and here something
else always did, because the campaign authored a whole family in one epoch.

The standing check the campaign adopted, and the transferable part of this entry:

> **Before asserting any result, ask which authored value would have produced
> this anyway.**

The worst instance is the one the check was invented for. Duergar rooted more
toponyms than any other people at seed 42 — marsh, spring, valley — and this was
written up as an emergent finding about a fungal food web wanting damp ground. It
survived a full implementer report, a controller review, and a written status
summary. **Nathan's one-line question dissolved it**: duergar had been authored at
an elevation optimum of 300 m to mean *deep*, and depth below the surface is not
height above sea level. The curve selected lowland marshes because it says to.

**And the pattern kept recurring after it was named.** While editing the coverage
tables, the controller's first draft of a comment invented an ecology ("a fungal
food web whose yield is lumpy") for a generosity trait whose authored rows say
something else entirely — a windfall too large to keep, a find too small to fight
over, a settled surplus. That is writing a plausible mechanism instead of reading
the authored value, **in the campaign whose headline lesson is exactly that,
while editing the file the lesson came from.** Naming a failure mode does not
install a guard against it.

## What ordering bought: instruments before kinds

The single highest-value process decision was task ordering, and it was made for
one reason and paid off for a different one.

The bind audit and the pairwise-distinctness probe were built and validated
**before any dwarf existed**. The stated reason was preregistration hygiene — an
instrument built after the kinds cannot be shown not to have been tuned to the
null it will report. What it actually bought was catching a false premise before
five kinds were authored on top of it (mechanism 1 above), and catching it at a
point where the correction was free.

The distinctness probe carried a two-directional discrimination control from the
start: it must separate two known-different kinds *and* return exactly 1.0 for a
kind against itself. A probe whose only assertions are "these are identical" is
indistinguishable from one that computes nothing.

The same habit ran again mid-campaign and saved a spec claim. The depth
coordinate had been folded in on the stated justification that it would separate
the two cave kinds. Measuring the cave-depth distribution first — fourteen
seconds of compute — showed the median depth over habitable caves is **0.0 m on
every seed**, so the subtraction would move almost nothing. Without that probe the
campaign would have shipped a spec asserting the depth read resolves the
degeneracy and a readout asserting a separation, which would then have failed
after five kinds were authored, with no diagnosis available.

## The plan text was wrong in three places, and something executable caught each

Not one of the three was caught by re-reading the plan.

1. **The lab's three "daughter" constants are three different objects.** The plan
   said "extend both". Read against *intentionality* they split cleanly: the
   goblinoid daughter list is a deliberate family-membership claim (dwarves are
   not goblinoid; adding them falsifies monophyly by construction), the
   all-daughters list is an *accident* whose doc claim had been false for two
   campaigns, and the per-species inventory instruments were deliberately minted
   once by an earlier campaign and are not extended by every new people. Two
   assumptions in that plan sentence were also measured false: deriving the
   goblinoid list would **not** have been byte-equivalent (the synthetic twin
   roster carries the goblinoid family label, so a derived list is non-empty where
   the authored one is empty, turning a null control's metric from absent into
   trivially true), and deriving the all-daughters list was not a free swap
   (the derived population includes kinds whose cascade regime is not settled,
   which the consumer had hardcoded).

2. **A proposed repair that would have been vacuous.** A coverage table's
   allometric row read "every kind"; the plan said it "must become an explicit or
   filtered list". The filtered version is the *identical computation* the
   assertion is checking, so it would have compared a value to itself and passed
   for any roster whatsoever. Spelled out explicitly instead. **This is the
   seventh time a plan sentence has offered a green-but-empty guard as a repair**
   — the class named in `the-beholding` and `the-benchmark`, arriving again.

3. **A constant nothing pinned.** The paced factor of 4.0 produces every dwarf
   lifespan in the world. Grepping the schedule workspace-wide found its *variant*
   audited in three places and its *value* in none: it could have been retyped to
   anything with a fully green suite. That is `the-vigil`'s shape — a verified
   claim unpinned by any failing test — and it is closed here.

Two further plan defects were caught by the implementer rather than by the
controller's own plan self-review: a bespoke heavy-tier ignore reason (the
canonical string is compared by **equality, not prefix**, and the memory entry
recording that already existed), and a type attributed to the wrong crate. The
generalisable observation is about what a self-review can do: **it checks the
plan's internal consistency; it does not check the plan's claims about the
codebase against the codebase.** Only something executable does that.

There is a smaller companion. Two of five sovereignty floors the controller
computed by hand were wrong in the fourth decimal, and one guessed API signature
was caught by the compiler within minutes of reading the rule that says to read
the signature. Harmless in both cases because a machine checked them — but the
same guess inside a spec sentence travels.

## A derived population made a latent bug reachable, and only the census saw it

Replacing a hardcoded four-species list with one derived from the roster was
correct: human and gnoll had been silently unmeasured for two campaigns because
nobody remembered to add them. An authored sampling frame under-covers silently,
**because the instrument still returns a number**.

The derivation exposed a cross-roster read. A metric took its species from the
view's *own* component set and handed it to an entry point that re-assembled the
**canonical** set and resolved against that — a species from one component set
resolved against another, which is precisely what the roster-threaded naming
convention exists to prevent. It failed inside a panicking function, so the worker
died rather than returning the recoverable error the caller already handled.

Three things about this are worth keeping:

- **It crashed the census on the canonical box and was invisible to the entire
  local suite.** The synthetic-roster study only runs during a census, and nothing
  in the workspace built the full metric registry against the solo rosters. A
  metric-registry change could therefore reach the canonical host before anything
  went red.
- **The blast radius was measured, not assumed** — every registered metric, all
  three study rosters, two seeds. Zero cells moved on the canonical census (there
  the view's component set *is* the assembled canonical set, so the change is a
  no-op), and the five cells that moved on the solo roster are all repairs of
  readings taken against the wrong roster.
- **The standing guard is deliberately NOT in the heavy tier.** It runs the whole
  metric registry against all three study rosters, collects every panic instead of
  stopping at the first, and asserts a specific metric is *measured* rather than
  merely non-panicking — because a guard that only asks "did nothing crash" is
  satisfied by a metric that quietly stopped measuring. It costs ~21 s standalone
  and 36.6 s under the gate's parallelism, against a suite whose slowest tests run
  three to six minutes. **Deferring it to the heavy tier would reproduce the exact
  blind spot it exists to close.**

## An artifact class nobody enumerated

`make rebaseline` regenerates the phonology page but cannot author the audio the
page references — voicing is a separate step with external tooling, and a test
asserts the committed clip set is exactly the page's references in **both**
directions.

So a roster campaign that adds three speaking kinds silently owes twelve audio
files, and the debt surfaces as the first red test in the gate rather than
anywhere near its cause. An earlier campaign paid the same debt in its close
commit. This plan did not enumerate the artifact class at all.

The lesson generalises past audio: **enumerate the artifacts a change owes, not
just the artifacts the regeneration script writes.** The two sets are not the
same, and the difference is invisible until the gate is red.

## A showcase page went empty, and the established test was re-applied

The deep-history gallery page was pinned to a cell carrying sixteen layers of
settlement stratigraphy. Three new settling peoples re-decide placement on every
seed, and that cell now carries none — the page was about to ship telling readers
the feature "never settled here". The guard caught it as designed.

What went right is that the **established** question was asked before the camera
moved: *is the world empty, or only this cell?* Scanning the seed-42 world found
another column carrying eleven layers, so the repointing is legitimate. Had the
world itself gone quiet, repointing would have been the wrong move and would have
hidden a real finding. That the deepest column fell from sixteen to eleven is
reported, with no mechanism claimed — the standing check, applied by default.

The same failure exposed a dormant gap: a documentation guard's people-name loop
is an *authored* list checked against an unauthored roster, so it goes stale in
the safe-looking direction. A missing people makes the guard check *less* rather
than fail. Its own comment already recorded the previous omission as "dormant,
not red", which is a warning that had been written down and not acted on.

## Two red history gates became report-only panels, and no floor was lowered

Two history gates went red. The diagnosis is that they sample a distribution
spanning three orders of magnitude (7.055 – 6936.212) **exactly once**, at seed
42, and were already failing 3 of 12 seeds on main before this campaign touched
anything. One of them sat one unlucky reshuffle from red the whole time.

Measured across twelve seeds on both rosters, the pass counts are **identical**;
only the failing members moved. Nathan's call: panel, report-only. Both gates now
run a twelve-seed panel in the heavy tier, print the full distribution plus how
many clear the historical floor — reported, never asserted — and assert only
non-inertness, which is what both tests' own text says they are for. A cheap
single-seed smoke stays in the commit gate, labelled as one sample.

**No floor was lowered and no quantile was invented from an afternoon's data.**
Both test files carry a prominent block saying the panel is deliberately the shape
the project is moving away from, that the assertion is weak on purpose, and — the
important part — **do not tighten it from panel data; wait for the census**. A
percentile chosen from twelve worlds is a percentile chosen from noise.

Two adjacent facts came out of the same investigation. An earlier campaign had
already reached this conclusion and re-based the *sibling* gate onto a multi-seed
spread; this one simply never followed, so the fix is two campaigns late — an
instance of floors eroding unseen. And a third constant is eroding identically:
its doc cites a value clearing its floor by ~6×, it now measures 2.3×, and it is
still green and still asserted. Same clock, one line below the floor that just
crossed.

## Main moved six times, and once only the local ref moved

Six absorptions from main landed on this branch. The plan mandated the drift check
as a literal command — the fix a previous retrospective had asked for — and **it
named the wrong ref.**

An earlier campaign had merged into the *local* `main` in the primary checkout
(about twenty commits) without pushing. `git log HEAD..origin/main` was empty and
main had nonetheless moved. **A literal command is only as good as the ref it
reads**, and a drift check that consults only `origin/main` reports "main has not
moved" while twenty commits sit in the checkout next door. Both refs are checked
now.

Two things worked. Absorptions were taken at **plan-stage boundaries**, and the
last one deliberately landed before the readout ran — the rule forbids absorbing
mid-measurement, and absorbing *after* the readout would have meant re-running it.
And after the mid-campaign absorption both instruments were re-run even though the
file diff over the relevant source trees was empty: the diff proves the inputs did
not change textually, not that the numbers did not move. It cost 51 seconds and
the numbers were identical to six decimals. A confirmed prediction has a shelf
life measured in merges.

The semantic check was also done by hand, since the mechanical preflight only
covers the checkable half. The campaign landing alongside derives an observer's
eye from a species' perception row; this one adds three perception rows. Reading
its chronicle rather than its diff is what settled in a minute that the two do not
collide — a pure function of an authored row gives three new rows three eyes for
free.

## The controller committed to main, because the shell's directory persists

Worth a numbered entry in the ledger and worth repeating here, because it was the
*controller* and not a delegated agent.

A commit of 42 lines landed on `main`, and two ledger entries were written into
main's checkout instead of the campaign's. The cause is mechanical: the Bash
tool's working directory **persists across calls**. Several read-only navigations
that afternoon were repo-wide questions answered from the primary checkout, which
sits on `main`, and the next `git add && git commit` inherited that directory.

This is the exact wrong-tree failure the dispatch preamble exists to prevent — the
one that got a model banned for all Hornvale work. That preamble, with its literal
`cd <path> && pwd && git branch --show-current` first step, was pasted into five
dispatches the same day. **The rule is enforced on delegated agents and on nothing
else. The controller has no preamble, and the controller is the one holding a
persistent shell.**

The repair was ordered so that content never lived in one place only: recover the
ledger entries into the campaign checkout, cherry-pick the commit onto the branch,
reset main, delete the stray scratch file. Four preconditions were verified before
touching anything — the commit was unpushed, it was the tip, its parent was
`origin/main`, and main's tree was clean.

**The generalisable fix is not "be careful."** Every mutating command in a
multi-checkout session should carry its directory explicitly (`git -C <path>`,
absolute paths in redirects) rather than relying on an inherited working
directory. A read-only navigation elsewhere is invisible and silently re-arms the
trap for the next write.

Near-miss worth recording: the ledger is git-ignored, so nothing reddened. Without
a state audit at the session handoff, two entries would have died in the wrong
checkout and a stray commit would have merged into whichever campaign absorbed
main next.

## Registry-first paid for itself immediately

Three defects were ready to file as new registry rows after a design question from
Nathan. **Two were already registered, and better framed than the new versions** —
one of them Nathan's own earlier note, which had already named the flat decomposer
supply constant and stated the general form ("makes scavengers viable where
photosynthesis is not — the general form of an arid specialist starving in its own
desert"). A third existing row had already asked for the pairwise niche-distance
readout this campaign had just built.

Filing three new rows would have duplicated two and orphaned the evidence for the
third — the failure mode that once minted a duplicate identifier which then
travelled through a spec, a plan, a study file and a decision. What shipped:
three existing rows amended with this campaign's witness (one clause each, with
headroom against the length cap checked), and exactly one genuinely new row.

The lift that produced the new row is worth the method note. Reading the resource
basis past its own vocabulary: it is a vector space whose referent is a food web,
so a trophic *level* cannot be expressed without a *link*. And the same lift
exposed a second dependency-compiled-into-a-literal: monophyly is a property of a
*family*, and the existing metric is named after what was, until this campaign,
its only possible subject. **A quantity with one instance cannot be distinguished
from a quantity hard-coded to that instance**, which is the same shape as the
diversity ceiling below. The dwarf family is the second instance that made both
visible.

## Preregistration held under pressure, including where a bound was re-derived

Three thresholds were frozen before the code that would move them, and **none was
moved**. One prediction was refuted (the third capacity pair) and shipped as the
headline; the refutation is pinned as a *witness* so a later change that separates
the pair reddens rather than silently inheriting the finding — re-pin a witness,
never a claim.

Two predictions were withdrawn and replaced **in the open**, at the point a
measurement falsified their premise, rather than reinterpreted afterwards. When a
question could not be predicted honestly, both readings were frozen in advance
with their interpretations attached, so neither branch could be rationalised after
unblinding. In the event the frame itself failed — the two-branch reading did not
discriminate — and *that* was reported rather than one branch being declared.

One preregistered bound genuinely was changed after unblinding, and it is called
out here because that is the move the project forbids by default. A diversity
ceiling of `3.0` broke on the enlarged roster. Its own documentation justified it
as "comfortably below undifferentiated oatmeal sharing, where the statistic tends
to the species count" — and the species count was four. **`3.0` was never an
absolute quantity; it was 75 % of the roster size, compiled into a literal.** The
repair re-derives the bound's *rule* (three-quarters of the peopled count) rather
than fitting its *value*, and is a **no-op at the original roster of four**, which
is the strongest available form of this change. The floor stays absolute because
monoculture drives the statistic to one whatever the roster size. The competition
temperature it interprets is untouched. The honest cost — a scaling ceiling is a
weaker discriminator on a large roster — is stated at the test.

One near-confound is worth its own note because **no assertion could have caught
it**. An implementer authored one dwarf's resource vector summing to 0.60 against
every other kind's 1.00, arguing the case explicitly in-comment (which is why it
was catchable). It was reverted for two reasons: it puts the scarcity in the wrong
object — a desert is poor because the *cell* supplies little, not because the
people extract badly — and it would have confounded that kind's entire purpose as
the climate demonstrator, since a uniform 40 % supply cut leaves its difference
from its neighbour partly attributable to supply. **Pearson correlation is
scale-invariant, so the readout would have been green and the interpretation
wrong.** Only reading the authored values against the roster's convention finds
that shape of defect.

## Sequencing: the gate cannot be green before the census, and that is structural

A new family metric plus the roster change reddened three dozen tests at once —
**34 stale census-fixture tests** across the calibration batteries at the point it
was measured. Those read committed census fixtures, which `make rebaseline` does
not write: only the census opt-in path writes them, and only on the canonical
host.

So the plan's ordering was wrong in a way no local work fixes: the true order is
readout → rebaseline and goldens → **census on the canonical box** → final green
gate. One task anticipated this; the task that scheduled the gate did not. Rather
than assume, the implementer diffed the fixture header against the live registry
and confirmed it differs by **exactly one column** and nothing else, which is what
made "these 34 reds are expected" a verified statement rather than a hope.

**Plan for this explicitly in any campaign that adds a metric or a settling
people**: there is a window in which the workspace is legitimately red, its size
is knowable in advance, and knowing the number is the difference between waiting
and debugging.

## An assertion-logic change, reviewed and accepted

This campaign produced the first observed tie in a per-culture naming guard (two
peoples each naming exactly 13 of 23 settlements with a simplex shape). The
implementer changed the guard to **skip** exact ties rather than count them.

Accepted, and the reasoning is the transferable part. Relaxing the strict
comparison to a non-strict one would let a tie **bank itself as a confirmation**
and prop up the "did we compare anything" guard, so a world in which every
separated pair tied would pass while demonstrating nothing. Skipping is strictly
safer: if every pair tied, the comparison count falls to zero and the guard fires.
Falsification power is unchanged.

One gap was closed on review: that guard's zero now has **two** causes — no pair's
predicted shares separate, or every separated pair observed a tie — and its
message named only the first. **A guard whose message describes one of its two
triggers reads as total to the next person.**

## Follow-ups

- **The underworld should become a place**, not a coordinate. A cave's biome is a
  function of its kind and the band its void reaches, both already committed, no
  new draws; a kind declaring which biomes it can live in generalises the realm
  gate, the one mechanism measured to *select* rather than modulate. Registered,
  with this campaign's cave-depth measurement as its input.
- **The two deferred kinds are blocked on that**, not on authoring effort. Both
  were authored and withdrawn; their traits are recoverable from this branch's
  history.
- **The decomposer supply axis is spatially constant**, which is one concrete
  reason the supply term reads kind-independent here. Deriving it from the biota
  above it is small and tempting, and was refused inside a roster epoch because it
  would move every cave-dwelling creature's capacity in the same change as three
  new peoples.
- **The residual correlation between the climate-selected dwarf and its
  elevation-selected neighbour is unestablished.** Supply was measured and ruled
  out as its explanation; the magnitude claim about supply is neither confirmed
  nor discharged, because a scale-invariant statistic cannot speak to magnitude.
- **The twelve-seed history panels are a stopgap.** They should become census
  tests once the campaign moving "build N worlds and count how many pass"
  batteries onto the thousand-world census lands. Recorded in both test files'
  module docs, where whoever edits them next cannot miss it.
- **A constant one line below the floor that just crossed is eroding on the same
  clock** — doc cites ~6× headroom, it measures 2.3×, and it is still asserted.
- **The diversity-ceiling re-derivation was measured on the five-dwarf roster**
  (a peopled count of eleven) before the cut to three. The bound is derived, so it
  is correct at any roster size, but the measured table quoted in its
  documentation describes a roster that did not ship.
