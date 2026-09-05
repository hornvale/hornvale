# Campaign The Tenon — retrospective

**Implementation complete:** 2026-09-04 · **Ledger:**
`docs/superpowers/ledgers/2026-09-04-the-tenon.md` (37 entries through close reconciliation) ·
**Decisions:** 0726–0730

Process lessons only. The product — a factorized kind-to-kind rest edge, three
natural surfaces, kind-graded recovery, and the measured distribution — is in
the chronicle.

Shape first: nine tasks, seven implementation commits before close, two
measurement passes, and one deliberate epoch. The ledger records more defects
in plans, briefs, and corrections than in implementation. That is not a reason
to trust implementation by default; it is evidence that a live preflight and a
specific discriminating check were doing the work prose could not.

## 1. A correction has the same blast radius as the claim it replaces

The same failure happened three times. A source-count correction changed
`103` to `95` in one paragraph and left the larger number elsewhere. A Task 7
correction removed one closed documentation dispatcher and left another brief
still instructing an implementer to recreate it. A caller count was re-derived
as “exactly two” while the module's own header, already read earlier, said
three.

None was a failure to discover the truth. The truth had been found, then the
correction was applied to the place under attention rather than to every copy
of the claim. A correction is an edit with a dependency graph. Its unit of work
is not the sentence being fixed but every surviving statement that depends on
the old value.

The practical remedy is mechanical: after correcting a named claim, search the
tree for its distinctive number, identifier, and phrasing, then classify every
hit. That is why this close corrects the frontier essay, four rest rows, the
Confidence Gradient, the concept-registry introduction, and the probe header
together. “Book freshness” is this dependency walk, not a final spelling pass.

## 2. A green filter is not evidence until it selected a test

Task 3's written red command used `-- substrate`. It selected zero tests and
printed green against a stub. Later, an exact byte-golden filter named the
module rather than the actual test target and again selected nothing. The
failure is especially dangerous because the command exits successfully and
looks cleaner than a real run.

Every filtered-test claim therefore has two outputs:

1. the exit status; and
2. the runner's `running N tests`, with `N > 0`, from the intended target.

This is not clerical decoration. A zero-match filter and a perfect test have
the same shell status. The verification report at this close records the
selected counts, and byte-goldens are invoked by their real suite paths rather
than inferred from `make rebaseline` or `gate-commit`, neither of which owns
them.

## 3. Compile the question before arguing about the answer

Three type claims were settled faster by the compiler than by inspection. A
stub field on `ObjectTraits` enumerated exhaustive construction sites and
showed that a grep count included declarations and functional updates that
would not break. A proposed `KindId(String)` lookup was genuinely ill-typed
and had a working `get_by_label` sibling two lines away. Conversely,
`&str == &String` was suspected not to compile and did compile through a
blanket `PartialEq` implementation.

The transferable rule is narrower than “always prototype.” When the dispute is
about what Rust accepts or what a field addition breaks, make the smallest
temporary edit and ask the compiler. A search result estimates syntax; a clean
or enumerating build answers the type question. Revert the probe completely,
then write the plan from the output.

## 4. Cross-world identity must include the world, or aggregate by sum

The baseline probe kept correct per-world sets of walked room addresses and
then unioned them across seeds. A `FacetId` identifies a room inside a world,
not across worlds; the same packed address in two seeds collapsed. The code's
comment said to sum the per-seed cardinalities, but there was no parallel
scalar doing so.

The corrected historical rerun printed the same four totals as the broken
method. That equality was luck, not vindication. A synthetic control made two
worlds reuse one packed address and forced union and sum apart; only that
control proved the repair had power.

This yields a reusable aggregation test: before combining identity sets across
parents, ask whether the identity carries the parent. If it does not, either
key by `(parent, identity)` or reduce each parent to a scalar and sum. Then
construct a collision deliberately. Reproduction of old output cannot validate
a method whose counterexample was never present in the sample.

## 5. Measure the grammar before choosing a distribution

The first surface design filled built-cold, built-warm, and wild rooms with an
ungated pattern. The plan spoke as though the pattern grammar sampled optional
features. It does not: every admissible locale pattern composes, always. The
draft would have put a surface in every room of every world and made bare
ground unreachable, contradicting the campaign's governing rest rule.

A four-cell table over the grammar's actual gates exposed the defect before
implementation. The shipped set fills three cells and deliberately leaves
wild-warm bare. The live measurement then found Bare at `0.3978`, which is a
positive control on the omission, not an accidental residue.

The lesson is to measure the distribution operator before authoring its rows.
“Add an ungated pattern” means nothing probabilistic in this system. A design
that depends on rarity must point to the mechanism producing rarity, and stop
if none exists.

## 6. A null can localize a problem without becoming a tuning request

Rushes reached 34 walked rooms in 11 of 24 worlds and appeared in zero
committed `SLEPT_ON` facts. Reachability and selection were measured separately,
so the null cannot be dismissed as “the pattern never spawned.” It localizes
the question above composition: who entered those rooms, which other surfaces
co-occurred, and when sleep was chosen.

No value was changed after unblinding. The preregistration made P2 and P4 stop
conditions, not a preferred shape for P6, and the null is therefore a product
of the campaign rather than a defect to hide. This is the useful role of a
tuning indicator: it narrows where to look next while withholding permission to
retune in the same measurement.

## 7. A ledger cell that reads "Resolved" is an assertion, and nothing checks it

Task 8's independent review found one minor: the probe header named two private
reconstructions after a third had been added. The code and measurement were
correct, which made the omission easy to defer and easy to lose.

**The close then recorded that minor as fixed, twice, without fixing it.**
Ledger entry #29 read "**Resolved:** `rest_site_census.rs:34-39` now names all
three without source line numbers", and this very section read "This close
updates the header to enumerate all three". Neither was true. The header still
opened "**It carries two private reconstructions**" and still named exactly
two; the third, `RECONSTRUCTED_FIT_FLOOR`, had arrived in Task 8's own commit
`869395c9f` and was never added. The real fix landed at the G6 absorption a day
later, along with this correction.

Three things make that worth a section rather than a line. First, the assertion
was **specific** — it cited a file, a line range, and named the remaining
site-local warnings by line — and specificity is exactly what makes a claim
read as verified. Every one of those numbers was wrong too: the header sits at
35-46 and the bolded sites at 673 and 722, against the cited 34-39, 658 and
707. A citation is only evidence if someone opened it.

Second, it survived the entire close. The DoD walk's step 2B says to read the
ledger for deferred minors and name where each one landed; the entry named
where it landed, so a reader checking that the item had a *home* would tick it.
What catches this is not reading the ledger but reading the ledger **against
the code**, which is a different and more expensive act, and one nothing in the
walk currently demands.

Third — and this is why it is filed here rather than as a curiosity — the
section that made the false claim was the section titled *"Deferred
documentation is still a close obligation."* The campaign wrote the correct
lesson and then failed it in the same paragraph. The lesson is not that
deferred documentation gets lost; the campaign knew that. It is that
**recording the discharge of an obligation is not discharging it**, and a
retrospective is as capable of carrying an unaudited claim as any other
authored text.

The same pass promoted the pre-existing `offered_by` rebuild cost to the idea
registry, narrowed the stale locale-grade row, and gave every discarded edge
shape and campaign follow-up a durable disposition in the ledger. Scratch
progress is coordination; it is never the last address of a finding.

## 8. The measured cost was in a sibling function, not the new one

The plan warned against rebuilding an object registry inside the fatigue fold.
Temporary counters over 12 bouts and two rooms found the existing path already
did worse: `offered_by` rebuilt the whole store per anchor, eight builds, while
the shipped borrowed-roster path built once.

The campaign removed that cost from the fold without widening into a general
affordance refactor. Two production paths and the lab probe still use the
rebuilding wrapper. Recording that residue matters because “the fold is fixed”
can otherwise decay into “the cost is fixed.” The new registry row names the
remaining callers and the boundary of the improvement.

The close is therefore not a claim that every adjacent issue was solved. It is
a claim that none remains known only to the conversation that found it.

## 9. Stage-boundary absorption was missed, so close paid the whole semantic bill

This branch first met current `main` only at close, 243 commits after its merge
base. That missed the repository's stage-boundary absorption cadence. The cost
was not merely conflict volume: The Housemark had already claimed accession
epoch 20, appended three inventory patterns, and added a required pattern field,
while Tenon's finished prose still described the old 18-kind and 17-pattern
baselines. Git marked the adjacent rows but could not mark the missing field in
Tenon's newly added patterns.

The reconciliation therefore re-derived the live inventories rather than
combining the old numbers: 22 thing kinds, 23 patterns, 56 referenced pattern
slots, and Tenon's surface cohort at epoch 21 after `bench`. It also exposed a
semantic conflict Git could only report as a missing struct field: The Weft's
overhang already carried `SupportsRest`, while Tenon requires every such marker
to carry a calibrated `RestSurface`. Nathan placed it beside the ledge as a
hard natural surface (`offer = 0.7`, `hardness = 0.85`), preserving both
campaigns instead of weakening either one's accepted contract. Absorbing at
each stage boundary would have put those corrections beside the task that
introduced the affected rows, instead of concentrating source, generated-
output, historical-prose, and cross-campaign calibration repair at close.
The commit gate then found the same lesson in a measured test constant: making
the screen conditional on an inward housemark moved the merged 1,024-case
grown-placement corpus from 28 to 29 unfaithful placements. A one-field
counterfactual restored 28 exactly, so the final ratchet records the merged
inventory rather than concealing it as test noise.
