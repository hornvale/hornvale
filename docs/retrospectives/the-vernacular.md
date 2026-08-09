# Retrospective — The Vernacular (parts 1–2)

Process lessons only. The product is in
[the chronicle](../../book/src/chronicle/the-vernacular.md).

## The headline: nine confident, checkable, wrong claims

Across two plans, **nine claims were asserted with confidence, were checkable
by one command, were not checked, and were wrong.** Five were the controller's.

| # | Claim | Disproved by |
|---|---|---|
| 1 | Task 1's fixture ordering was "the brief's own verified ordering" | The values failed the codomain assertion the moment referents became load-bearing |
| 2 | Artifact drift was "deferred by standing campaign convention" | `git show --stat` — both prior tasks re-recorded it in their own commit |
| 3 | Spec §4: `Phenomenon.description` served a fourth "dedup key" role | One grep — the dedup is on `SkyReport`, in a test asserting prose *should* differ |
| 4 | The 77-vs-73 delta came from predicates The Watershed added | Building a worktree at the older commit and rerunning — all six predicates predated it |
| 5 | Followup #11 described a production prose-read | It is inside `#[cfg(test)]` |
| 6 | `origin/main` was fully absorbed | Local `main` was 113 commits ahead, unpushed |
| 7 | `exposure_from` draws its universe from the packs | Its body iterates `world.registry.concepts()` |
| 8 | A guard "proves the invariant directly" | Deleting the exclusion left all 588 tests green |
| 9 | Spec §3.1: 23 `Gap` uses are absorbing `Unnamed` cases | Sampling 8 found 7 correctly `Gap` |

**Two reached source comments.** #7 was written into `accession.rs` as the
justification for an epoch cohort — the misapprehension that caused a bug,
preserved on disk in the file the next engineer opens. #3's sibling landed on
`Phenomenon` itself, the type the campaign is named after. Both were caught by
reviewers, neither by the author.

### The rule worth keeping

> **A causal claim spanning two commits is verified by rerunning the
> measurement at the older commit, not by reasoning about what changed.**

That one sentence would have caught #4 and #9 outright, and #1, #2 and #7 by
the same reflex. It is cheap: build a worktree at the old commit, rerun, read.

### The rule that is new, and less obvious

> **Ledgering a correction is not propagating it.**

#7 was found by a reviewer, recorded correctly in the decision ledger — and
then the *next* task's brief was written without it. So the brief predicted the
wrong fixture would change and omitted the surface where the defect actually
shipped, which cost a fix round. A correction must be carried into every later
brief, not merely into the record. The ledger is a memory aid, not a
distribution mechanism.

## What the review machinery caught that the work did not

The subagent loop earned its cost, and specifically at scopes the work could
not see itself:

- **A defect only whole-branch scope could find.** `Void::Unnamed` did not
  survive save/load; every guard the campaign wrote built its world in-process,
  so four task reviews were structurally blind to it, and a *published* page
  shipped the wrong claim as live output.
- **A vacuous guard reported as a proof** — found by deleting the thing it
  guarded and watching nothing red.
- **A latent species-dependent divergence** no test could have caught, found by
  reading control flow rather than running anything.
- **A golden snapshotting a broken path**, not merely a superseded one: 22
  duplicate proto forms under a doc claiming to mirror a page it no longer
  mirrored.

The technique that did the work, four times over: **break it, watch it red,
revert, prove a clean tree.** Adopted as the default way to verify any guard
this campaign added.

### Reviews can be confidently wrong too

Findings #1 and #2 were *reviewers* explaining away real defects with plausible
premises. A review is not an oracle. The dispatch prompts were amended
mid-campaign to say: *if you are about to accept something because a convention,
a decision record, or a prior review says so, run the command first.* Reviews
after that amendment found more, and cited evidence rather than authority.

## What went right, structurally

- **Splitting by what moves.** Part 1 moved zero facts by construction; part 2
  likewise; part 3 (the `star-class` value change) is separated precisely
  because it moves them and owes an epoch. Every task had a crisp numeric gate,
  and "zero facts moved" was verifiable at every step rather than at the end.
- **Preregistration worked as designed.** The prediction was frozen in the spec
  before the code, and the readout was re-measured as a *matched pair* on one
  tree rather than comparing a pre-merge number to a post-merge one — which is
  how #4 was caught at all.
- **Two subagents improved on their instructions and said why.** One replaced a
  requested "hoist above the pack loop" with a final unconditional overwrite
  (strictly stronger — a hoist remains vulnerable to later inserts); one
  declined to invent a conversion that could not compile and reported the
  brief's error instead. Both were right. Dispatch prompts should keep inviting
  that rather than demanding literal compliance.

## What to do differently

1. **Write the "verify at the older commit" rule into the dispatch preamble.**
   It is currently tribal knowledge held by whoever read this page.
2. **A brief's file list is a hypothesis, not a specification.** Twice a list
   missed the surface where a defect actually shipped. Ask implementers to grep
   for siblings before finishing, as the later dispatches did.
3. **Gate wording must match what the code can do.** The plan demanded a
   seed-42 world `diff` printing `IDENTICAL` for a task that *registers
   concepts* — impossible, because `World` serializes the registry. The right
   gate is "no fact moved," and it took a task failing its own stated gate to
   notice.
4. **Guards that build their world in-process cannot see the save boundary.**
   Any invariant on registry-derived state needs a round-trip assertion. This
   campaign's Critical is the standing example.

## Absorption cadence

Main was absorbed three times mid-campaign (17, 113, and 1 commits) at
plan-stage boundaries, never mid-measurement. The 113-commit absorption moved
**zero** committed facts, which is worth recording as evidence that the
stage-boundary cadence keeps semantic drift next to its cause. One artifact —
`scene-tiles-region-seed-42.json` — was found stale *on main*, its schema having
gained two fields without a regeneration; surfaced by this campaign's rebaseline,
not caused by it.

## Part 3b — process lessons

### The controller was the least reliable source in the loop

Three claims I asserted into dispatch prompts as verified context were wrong,
and each would have cost a single `grep`:

| Claim | Reality | Cost |
|---|---|---|
| `exposure_from` draws from the packs | It iterates the registry | Reached a source comment before a reviewer caught it |
| `windows/almanac` may depend on `hornvale-worldgen` | Worldgen depends on almanac — that direction is a **cycle** | Shipped a renderer with no noun-class marking and three dead `Speaker` fields; cost a whole extra task to correct |
| `phenomenon_line` is `pub(crate)` | Already `pub`, re-exported with a doc and a tag | Implementer wasted effort evaluating a promotion that was unnecessary |

The pattern is not carelessness about facts I had no way to know. It is
confidence about facts that were one command away. The existing rule ("verify
generated-artifact and tool-behavior claims") covers tool output; it does not
cover **claims about the repository's own structure**, which is where all three
landed. Extend it: a dispatch prompt asserting a dependency direction, an item's
visibility, or a function's data source needs the command output beside it, at
drafting time.

### A guard that fails open on the controller's error is not a guard

Task 1's dispatch preamble carried the wrong branch name (`the-vernacular-3`;
the branch is `campaign/the-vernacular-3`). The preamble's first instruction is
a branch check whose stated failure mode is to reply `BLOCKED` and stop. The
agent printed a non-matching branch and **proceeded anyway**.

No damage — the commit landed correctly and main never moved — but the control
exists because subagents have twice committed to `main` from the wrong tree. It
protects against the *subagent's* error and failed open on the *controller's*.
Candidate fix: have the preamble instruct the agent to derive the expected
branch from the worktree rather than trust a supplied literal, or make the
mismatch reply mandatory regardless of which value is wrong.

### Reversing a decision mid-plan needs a grep, not a memory

`CommonGap` was removed from the design after the registry measurement showed
Common could be total. I edited the sections I remembered writing — the
interfaces block and the test — and missed Step 3's prose and the step's commit
message, both of which still specified `Result<String, CommonGap>`. The brief
therefore contradicted itself, and the implementer had to adjudicate. It chose
correctly and flagged the discrepancy.

**Rule:** when a type or concept is cut mid-plan, grep the plan for its *name*
before re-extracting briefs. Editing from memory finds the sections you wrote
most recently, not the ones that mention it.

### A wrong world-fact propagated through four documents unchallenged

Tasks 5, 6 and 7's reports and my own dispatch preambles all named seed 42's
flagship people "goblin". It is **bugbear**. No test named the species, so
nothing reddened; the error survived until Task 8 regenerated the artifact and
read it. I relayed it to Nathan as fact more than once.

Worth noting *why* it survived: each task inherited the claim from the previous
report rather than from the artifact. A chain of agents citing each other
converges on a shared belief with no external check. The artifact was the
authority the whole time and cost one `grep` to consult.

### The scope grew twice, both times correctly

The plan went 5 → 7 → 8 tasks. Neither growth was scope creep: the first came
from Nathan rejecting a design (Common taking a literal string) and the second
from correcting my layering error. Recording this because a plan that grows is
usually a planning failure, and these were not — the first was a design
improvement caught at review, the second a defect caught by an implementer.

What *would* have been cheaper: the registry measurement (191 concepts, 93
hyphenated) that overturned my "hyphenated id = no word" rule took one command
and happened only after I had written that rule into a plan and a spec. **Measure
the corpus before writing the rule that classifies it.**

### The measurement that refused to be single-sampled

§4.2 froze a ≤1.25× wall-clock prediction. Two runs at the *same commit*, three
minutes apart, gave 1.38× and 1.05× — one fails, one passes. CPU time gave 1.02×
and 1.00×, agreeing to 1.5 s. The chosen baseline was the fastest rebaseline ever
recorded on this host, and the pre-campaign history shows an 8% wall swing at an
*unchanged* commit.

The prediction was answerable only because the implementer sampled twice.
Preregistering a wall-clock threshold on a shared machine preregisters the noise
along with the effect; **freeze the CPU-time metric, or freeze a sample count.**

### Reviews earned their cost, and the missing one was noticed

Task 3's review found that a validating constructor I had described as "proof of
totality" had a public `Default` bypass and validated well-formedness rather than
word quality — a better framing than mine, adopted. Task 4's review mechanically
ran 191 ids through the resolver to check a collision claim. The whole-branch
review regenerated five artifacts and diffed them byte-identical rather than
trusting the reports, and corrected the campaign's headline claim from "text no
longer exists in the sim" to the narrower and true "the phenomenon channel is
text-free."

Task 6's per-task reviewer never reported back. That was caught only because the
ledger tracked which reviews had returned; the whole-branch review was then told
to weight that commit. **A review that silently never lands looks exactly like a
review that passed** — the ledger is what distinguishes them.

### The absorption found convergence, not collision

Main gained The Namesake and The Salt while 3b ran. Both touch
`domains/language`; the merge was textually clean and the full gate passed on
the merged result, so there was no collision to fix.

What the semantic read found instead is worth more than a conflict would have
been. The Salt shipped `cli/tests/no_entity_id_values_in_prose.rs` — a scoped
source scan forbidding an `EntityId`'s *numeric value* from being read into a
derived-prose path. It was designed independently, in a different campaign, for
a different identifier type, and it is **the same guard 3b built twice**:
`a_referent_never_carries_prose` and `common_is_total` forbid a *concept* id
from reaching reader-facing text.

Three identifier types — `EntityId`, `ConceptId`, `KindId` — have now each
acquired a bespoke, hand-written guard against the same failure, because none of
them is a type the compiler can distinguish at the prose boundary. The Salt's
module doc says so itself, at length, explaining why a `clippy.toml` ban was
unavailable and a source scan was the backstop.

That is the strongest available argument for `LANG-typed-text` (part 3c), and it
did not come from either campaign's own reasoning — it came from reading the two
side by side at an absorption. **The stage-boundary absorption cadence is
usually justified as conflict avoidance; here it paid as design evidence.**
