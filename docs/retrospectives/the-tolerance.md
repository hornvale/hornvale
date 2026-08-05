# The Tolerance — retrospective

Process lessons. The product is in the chronicle
(`book/src/chronicle/the-tolerance.md`).

## What worked

**A hypothesis was withdrawn at spec review, before any code existed.** H4
predicted that raiding would track the interannual variance of local resource
supply. Two facts checked against source rather than assumed killed it: the
supply field takes no time parameter, and the one available daily trajectory is
a periodic year by construction, so it carries seasonal amplitude and not
unpredictability. This is the fifth time this program's probe-validity ladder
has bitten and the **first time it bit at spec review** rather than after the
measurement came back flat. The cost of catching it there was one paragraph.
The cost of catching it later would have been a task.

**The highest-risk decision was tested before it was implemented.** Keying a
per-settlement draw on an entity identifier is the obvious implementation and is
catastrophic — sequential minting means one inserted entity reshuffles every
community's psychology. The plan wrote the id-independence test *before* the
implementation for exactly that reason, and the reviewer independently ran the
mutation and got a red with materially different vectors. A rule ratified by an
earlier campaign (an identifier is stored, compared and looked up, never read
for its value) prevented something for the first time instead of merely
describing something.

**Every review round discharged a risk item independently rather than accepting
the report's account of it.** Three examples worth keeping: the reviewer
recomputed every row of both clamping-disclosure tables exactly; it re-derived
the clamp table analytically and matched the measured second moments to four
decimals; and it reproduced the readout's committed table to ten digits from a
second, independently written harness. Two harnesses agreeing is the strongest
available check on a reconstruction, and neither report mentioned it — the
reviewer found it.

## What went wrong

**A mutation proof passed while every people carried a fabricated parameter.**
Three tests established that zeroing a people's dispersion collapses its
between-settlement variance to exactly zero. The reviewer then handed *every*
people a fabricated dispersion of 0.15 — a value no author ever wrote — and all
three tests **passed**, while the proof's own output printed "authored sigma =
0.3500" beside worlds built with a different sigma.

The generalisable rule: **a mutation that perturbs the derivation proves the
function reads its argument; it does not prove the pipeline delivers the
authored value as that argument.** The two are different claims and the second
is the one an acceptance criterion usually means. Closing it required extracting
the configuration assembly into something a unit test could reach — the seam did
not exist, which is why the hole did not either — and requiring a red against
exactly that mutation. This belongs beside the standing "require RED" lesson: it
is a case where a red *was* required and obtained, against the wrong input.

**A stale-prose family took three sweeps, and the first two were aimed at a
phrasing.** **Fourteen** committed sites asserted that a named people is an
extreme of an authored axis. Recounted from the commit record rather than from
the round reports: round 0 found **6** on the claim's own wording; round 1
(`efe5e0a5`) found **4** more on the arithmetic a stale premise leaves behind, a
vocabulary absent from all six; round 2 (`18da834b`) found **4** more phrased
with no number in them at all, one contradicting a line eleven rows above it
inside the block round 1 had just edited. All fourteen descend from a single
sentence in a *previous* campaign's plan, which is why they propagated verbatim.

**This retrospective originally said ten, from 6 + 4 + 2, and the review caught
it.** A miscount inside the lesson about miscounting is as on-the-nose as this
campaign gets, and it has the same cause as the defect it describes: the figure
was carried forward from a round report instead of recounted from the source.
The correction is not "be careful with addition" — it is that **a count is
scoped to the paragraph that produced it**, and re-deriving one from the record
costs a minute.

The fix, stated as a rule because this is the third instance in this project's
memory: **sweep on the invariant a stale premise asserts, not on its wording and
not on the arithmetic you have already seen.** Round 2 mechanised exactly
that — a script derived ground truth from the authored registries and printed
the true minimum and maximum beside every hit, so each was adjudicated against
the source rather than against memory. 1066 hits on the bare shape narrowed to
120 with authored-axis vocabulary and then to 4 live and stale; an independent
re-sweep by the reviewer (3334 blocks → 88 → 44 read by hand) found nothing
further. The mechanised sweep is the artifact worth reusing, not the word list.

**A fifteenth site surfaced at the close, on a different invariant.**
`book/src/domains/species.md` ended a paragraph with *"never a distribution of
its own"* — the campaign's own premise stated abstractly, carrying no people's
name and no number, so no sweep keyed to the extremal-claim invariant could
reach it. Two invariants went stale here, not one, and finding the second cost a
separate reading pass. **Enumerating the invariants a campaign falsifies is
itself a step**, and this campaign did it once.

**And the prose was already stale before this campaign touched it.** The claim
that four values were reachable on one axis was correct when written, went stale
at five when a *previous* campaign added a people with a shorter time horizon
than the incumbent extreme, and is now stale again. Confirmed against both the
registry and git chronology. **A campaign that widens a roster silently
invalidates every extremal claim about it**, and nothing in the gate notices.

**An instruction I issued was wrong on floating-point grounds, and the
implementer was right to decline it.** I asked for a blanket assertion that the
zeroed variance equals zero for every people. That is true only for the two
peoples whose authored location is 0.5 — a power of two. For the other four the
battery goes red at around 10⁻²⁷, because a mean computed as a sum divided by a
count does not round-trip for a non-dyadic location over four thousand terms.
**The residue is in the estimator, not in the draw.** The re-reviewer wrote a
standalone reproduction and matched the failure digit-for-digit. What shipped
asserts exact per-draw equality against the authored location plus a variance
bound, which proves the point-collapse claim directly rather than inferring it
from an aggregate statistic carrying summation noise — strictly stronger than
what I asked for. The lesson is not "check my arithmetic": it is that **an
aggregate statistic is a lossy witness for a per-element claim**, and asking for
the aggregate when the per-element form is available weakens the test twice
over.

**A report softened a confirmation.** The readout misquoted the baseline task's
victim-side variance as this run's post-dispersion numbers, which erased a
roughly fourfold rise in two peoples' victim variance. The unusual thing is the
direction: reporting errors are expected to flatter, and this one *understated*
a result in the campaign's favour. The fix was not simply to restore the real
numbers but to state that the comparison **crosses a merge boundary** and so
cannot be attributed to dispersion alone — the same merge grew one people's
population 44 % and another's 70 % while shrinking four others 21–40 %. A
calibrated correction, not a correction in the other direction.

**A committed disclosure went stale because a later task changed the population
under it.** The code accepting the draw-key collision rate justified itself on
the ground that the hypotheses would be measured over settlements alive at the
end of the bake, where the key is unique. The readout then measured all 19,996
records rather than the 6,041 alive, and the premise became false. Nobody
changed the disclosure; the world under it moved. The resolution was to correct
the disclosure rather than to narrow the readout to fit it.

**A heavy-tier fixture re-pin arrived at the close instead of in the drifting
commit — and it was not this campaign's drift.** `occupancy.csv` failed its own
drift check, whose assertion message states the standing rule inside itself:
*"if this is intended, rewrite the fixture in the SAME commit as the change that
drifted it."* The rule was broken, but the diagnosis handed to me with it was
wrong, and checking it is the lesson.

The reasoning offered was that this campaign moved seed-42 occupations 919→459
and the readout covers seeds 1..=30, so the drift is ours. **Occupations and
occupancy are different things.** The readout renders
`per_species_suitability(geo, terrain, climate, obliquity, insolation, regime,
biosphere)` — the carrying-capacity field. It takes no history, no settlements
and no disposition; `build_world` is called only so terrain and climate can be
reconstructed. There is no path by which a settlement's drawn temperament
reaches it. The decisive check is the diff rather than the argument: over
`kernel/src`, `domains/{terrain,climate,astronomy,demography}/src` the branch's
diff against the merge-base is **empty, zero files**, and
`domains/species/src/lib.rs` has **zero deletion lines**. Every input to the
readout is byte-identical between merge-base and HEAD, so the branch *physically
cannot* have moved the fixture.

**And then I got the replacement attribution wrong too.** I named The Keeping's
land-mask decomposition as *the* cause. It is *a* cause. Of the thirty new rows,
**eleven are a people that did not exist in this fixture at all** — a roster
addition from a different campaign — and **twenty are one biome opening**, the
two sets overlapping in exactly the one row that needs both. Two campaigns
drifted this fixture, and it was already stale before the one I blamed touched
it.

So the lesson has a third instance, and it is in the correction to the
correction. **An inherited diagnosis is a hypothesis** — and so is the one you
write to replace it. The first arrived attached to a correct instruction
(regenerate the fixture) and a correct rule citation, which is the shape that
gets a wrong cause written down unchecked; the second arrived attached to
genuine evidence that happened to be *sufficient for the conclusion I was
defending* (not this branch) and *insufficient for the one I actually wrote*
(therefore that campaign). Checking that a cause is real is not checking that it
is the only one. The counting discipline this campaign already learned twice —
recount from the source, and enumerate rather than generalise — is the same
discipline, applied to causes instead of sites.

Beneath both: the re-pin rule was violated by the campaigns that drifted the
field, and it could be violated because this artifact is **heavy-tier and
therefore invisible to `make gate`** — the standing trap, and the reason two
campaigns' drift could stack unnoticed.

**Refreshing the fixture exposed what it froze, which is the reason to check.**
The regenerated readout carries a preregistered verdict from the campaign that
authored it. The verdict survives — one of three target regions still gains a
top-ranked occupant — but two statements beneath it did not. One went stale (a
people authored for desert had *zero* desert occupancy; it now holds 3793
desert cells, because the land-mask change opened the biome). The other **was
never right**: the same paragraph named that people's largest share as
temperate-forest when it was tropical-seasonal-forest in the very fixture the
sentence was written against. A drift check pins output against *change* and has
no opinion about whether the output was ever *right*, and this is a clean
instance: the wrong sentence sat green through every regeneration.

## Two mechanical notes

**Absorbing main mid-campaign forced a baseline decision.** The project forbids
absorbing during a measurement, and main moved fourteen commits between the
baseline task and the readout. The resolution was to treat the mutation task's
**zero-dispersion arm as the re-derived baseline on merged physics**, making the
comparison a matched pair on identical physics rather than a before/after across
a merge boundary. The pre-merge numbers stay in the record as the pre-merge
record. This is worth generalising: a campaign that ships a zeroed-parameter arm
gets a merge-proof baseline for free, and should notice that it has one.

**Twelve seed-42 artifacts regenerating byte-identically after a merge is
indistinguishable from a silently dropped semantic merge.** It was settled by a
matched-pair control on the merged tree rather than by argument: reverting the
incoming change left one world byte-identical while moving another by 168 facts,
which proves the incoming change is live and simply does not reach the first
world. A byte-identical regen is evidence of nothing until you have shown the
change *can* move something.

**Two dispatch frictions worth recording, because both cost real time and
neither is in any commit.** The heavy-tier dispatch failed twice with exit 128
before it could start, because an **orphaned heavy worktree** was parked on the
canonical box at an old campaign's commit with four uncommitted regenerated
artifacts from a run killed hours earlier. The shared regeneration worktree is
not where you left it; verify its HEAD and sweep orphans before dispatching
rather than after two failures. Separately, the **type-audit report needed a
regen** after a task that touched a public boundary, and what caught it was the
pre-commit hook, not the task brief — any task adding a `pub` item should expect
`docs/audits/type-audit-report.md` to drift and should say so in its own brief.

## Owed at the close

- **The census refresh.** Deferred by owner ruling so that one run happens on
  the final tree after this branch absorbs main, rather than two.
- **`occupancy.csv` awaits confirmation on the canonical box.** It was authored
  here on the Mac, but `heavy-run.sh` names it an authored artifact, diffs it,
  and is host-guarded to lefford (decision 0079 — goldens are authored on one
  enforced host). Decision 0090 measured cross-platform byte-identity on a
  40-world all-metric probe, so this is very likely fine, and *very likely* is
  not the standard for a committed golden. **The final heavy run on the
  canonical box is the confirmation**; treat a byte difference there as the
  golden's, not as a regression.
- **The heavy-tier calibration battery is RED and that is the campaign's owned
  finding**, not a pending question: goblin re-seats its flagship on **52.1 %**
  of worlds against a 0.25 bound (predicted ~38 %; the measurement came in half
  again as high). The bound was deliberately not retuned, because the test's
  *premise* — that a non-raiding partition exists — is what this campaign
  destroyed. One further heavy failure, a climate readout's conductance null,
  has **genuinely open attribution**: green before this branch existed, never
  touched by us, but another campaign landed in between and changed the
  placement gate, and the data does not separate them. Two more (a scene-cost
  ceiling 0.8 % over, and a census-fixture probe) were **already red on main**
  and are not ours to carry.
- **`hostname -s` flaps on this box** between `MacBookPro` and `Greyjoy`, which
  macOS does unprompted. The owner has ruled that no time is to be spent on it;
  **do not rename anything.** The consequence must be known rather than fixed:
  `make ci`'s duration baseline is keyed on `hostname -s`, so it forks and
  re-forks, and the first run under whichever name is current finds no baseline,
  records silently, and **cannot alarm**. A quiet `make ci` on this box is
  *unproven*, not green. This is The Timekeeper's documented second blind spot,
  live and permanent rather than hypothetical.
- **`scripts/heavy-run.sh:72` records the wrong ref.** It writes the *caller's*
  `git rev-parse HEAD` into `runs.tsv` rather than the ref it actually ran — the
  ledger recorded a main commit for a run whose worktree was verifiably at this
  branch's Task 5 tip. **Every historical row is wrong the same way**, so the
  heavy-run ledger cannot currently be used to attribute a result to a tree.
- **`fixture_staleness.rs`'s failure message is stale**: it still directs the
  reader to "Regenerate on the AWS box: `make regen-remote`", abandoned by
  decision 0063 and superseded by `scripts/census-run.sh` (decision 0081).
- **The raid gate still reads a defensive axis.** Only one of the design's three
  warlikeness terms was built; the structural term is unreachable from where
  history is baked, and the quadrant term is documentary only.
- **One residue inherited from main**, deliberately not edited inside a merge
  commit: `windows/worldgen/src/lib.rs` still cites a field name that main's own
  campaign deleted. Identical on `origin/main`.
