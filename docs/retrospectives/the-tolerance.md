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

**A stale-prose family took three sweeps, and each sweep was aimed at a
phrasing.** Ten committed sites asserted that a named people is an extreme of an
authored axis. Round one found six by one phrase; round two found four more by a
phrase absent from all six; round three found two more carrying no number at
all, one of them contradicting a line eleven rows above it inside the block the
previous round had just edited. All ten descend from a single sentence in a
*previous* campaign's plan, which is why they propagated verbatim.

The fix, stated as a rule because this is the third instance in this project's
memory: **sweep on the invariant a stale premise asserts, not on its wording and
not on the arithmetic you have already seen.** Round two mechanised exactly
that — a script derived ground truth from the authored registries and printed
the true minimum and maximum beside every hit, so each was adjudicated against
the source rather than against memory. 1066 hits on the bare shape narrowed to
120 with authored-axis vocabulary and then to 4 live and stale; an independent
re-sweep by the reviewer (3334 blocks → 88 → 44 read by hand) found nothing
further. The mechanised sweep is the artifact worth reusing, not the word list.

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

## Owed at the close

- **The census refresh.** Deferred by owner ruling so that one run happens on
  the final tree after this branch absorbs main, rather than two.
- **The heavy-tier calibration battery** partitions the roster from authored
  means and was deliberately not retuned; if it reddens, that is this campaign's
  finding.
- **The raid gate still reads a defensive axis.** Only one of the design's three
  warlikeness terms was built; the structural term is unreachable from where
  history is baked, and the quadrant term is documentary only.
- **One residue inherited from main**, deliberately not edited inside a merge
  commit: `windows/worldgen/src/lib.rs` still cites a field name that main's own
  campaign deleted. Identical on `origin/main`.
