# The Stile — retrospective

**Merged:** 2026-08-28

## The headline: two corpora authored independently found what neither could alone

The ladder was drafted from the typological literature — 150 rungs, 139
tokens, each rung one capability with its dependencies. The flood-watch corpus
was authored separately, by the project owner, as five scenes of investigative
dialogue, and annotated against the ladder's vocabulary. Neither author saw
the other's work while writing.

The cross-check between them found **56 tokens the corpus demanded and the
ladder did not name** — 56 of the corpus's 149-token vocabulary. Two are worth
keeping because they are the same error at two scales:

- **An entire ellipsis and anaphora family collapsed into two tokens.** The
  draft carried `ellipsis` and `discourse-anaphora`; fragmentary dialogue needs
  eleven. Worse, the draft's `ellipsis` rung *was a fragment answer* under the
  general name, while the corpus used `ellipsis` in its general sense 41 times.
  A ladder can under-describe exactly the way a hand-written demand list can,
  and this is what it looks like one level up.
- **`proper-name` appeared 23 times with no rung at all.** The ladder had
  `named-entity-list` — three names coordinated — and nothing for one name in
  an argument position. The same inversion produced four more gaps
  (`manner-adverb`, `pp-modifier`, `infinitival-complement`,
  `attributive-adjective`), and the generalization is the transferable part: **a
  ladder derived from the literature reaches for what has a name and an argument
  attached, and the unmarked member of an opposition frequently has neither.**

The ladder was revised to 214 rungs and 199 tokens **before anything was
measured against it** — before the spec was written, before the plan, before
any task ran. That ordering is the whole point. Had the ladder been frozen
first and then cross-checked, the same 56 tokens would have arrived as a
measurement of the ladder's inadequacy rather than as its revision, and
revising it afterwards would have been retuning an instrument after
unblinding.

**The design lesson is about instrument independence, not about corpora.** The
project already knew that a measurement needs a control; what this campaign
adds is that two instruments built by different processes over the same
subject will disagree in a way that names a defect, where one instrument
checked against itself cannot. Neither author was wrong about their own
artifact. The corpus is a faithful record of one author's ear for
investigative dialogue; the ladder is a faithful reading of the typology
literature. The 56 tokens live in the gap between those two faithfulnesses,
and only a comparison could reach them.

## The defects were in controlling-session text again, and commands found them

The pattern The Scarf, The Reticence, The Inquest, The Mortise and The Offer
each named held again. Every substantive defect this campaign found
originated in the controlling session's own spec, plan, or dispatch prose,
and none was found by re-reading.

1. **A false factual claim committed in prose, inside a paragraph asserting it
   had been re-derived.** Task 2's doc comment explaining the `r183` diamond
   said its two shared ancestors were reached "both by way of `classify`";
   `r002` introduces `intransitive-frame`, not `classify`. The diamond itself
   was real — `r181`'s and `r095`'s closures overlap at exactly `{r001, r002}` —
   so the conclusion stood and the attribution did not. It earned a fix round
   rather than a shrug for two reasons: it was the **exact claim the review was
   dispatched to audit** ("re-derived directly from the committed JSON"), so a
   plausible detail rode through a re-derivation claim into permanent prose; and
   `intransitive-frame` is the ladder's own headline finding about this project's
   grammar — `Valence` has only `Nominal` and `Transitive`, so *"the woman
   sleeps"* has no construction — which a comment calling `r002` `classify`
   erases at the one place a reader would meet it.

2. **A controller review brief that attributed an implementer's reply content
   to its report file.** I told a reviewer that an implementer's report "ends by
   disclosing" a removed false claim. It does not — the disclosure was in the
   implementer's *reply to me*, and it described removing that claim *from* the
   report, so the report correctly contained neither. This is the same shape as
   the defects the campaign kept finding: a claim about **where** something is,
   asserted rather than verified. The reviewer checked the report, found
   nothing, and flagged the discrepancy as a possible stale expectation rather
   than silently ignoring it, which is the only reason it is recorded here.

3. **A vacuous drift check that predated the campaign, found by pre-dispatch
   verification rather than by any gate.** `docs/audits/sentence-coverage.md` is
   written only under `HV_SENTENCE_REBASELINE=1`, and **nothing set that
   variable** — not the Makefile, not `regenerate-artifacts.sh`, which did not
   mention the report at all. `docs/audits/` *is* in `generated-paths.txt`, so
   the path was covered; but the check is `git diff --exit-code`, which can only
   fire if something changed the file, and with no writer in the regeneration
   path nothing ever did. `make rebaseline` did not regenerate this artifact and
   reported no drift, which reads as a resolution. **A remedy is a claim:
   "run X to fix Y" asserts that X writes Y.** Closed by wiring the generator
   into `regenerate-artifacts.sh`, and the wiring was proved rather than
   assumed — a marker spliced into the report's title, the full script run, the
   marker confirmed present with no other declared path moved, then reverted and
   re-regenerated to byte-identical. A wiring that silently does nothing is the
   same defect in a new coat.

4. **An implementer catching itself writing a verification claim for work it
   had not performed.** Task 4's earlier draft claimed it had additionally
   inverted its control's expectation to re-verify it. It had not, and it
   removed the claim before finalizing — the exact shape defect 1 took, caught
   by the implementer this time instead of by a second reviewer. The hazard had
   been named explicitly in that task's dispatch. One instance is not evidence,
   but it is the cheapest available hypothesis: **naming a campaign's own
   failure shape in a dispatch appears to propagate it as a self-check**, and it
   costs two sentences to test again.

**What caught them:** a command, every time. The reviewer that ran the
forbidden speaker-inference mutation itself and reproduced the exact 4/8
split. The reviewer that swapped the *expected* refused set to
`{contraction-elision, wrong-token}` to confirm `assert_eq!` was a genuine set
comparison rather than a size check that would tolerate a swap — a third
mutation neither the implementer nor I had thought of. The reviewer that
confirmed the mutation's **premise** before trusting the mutation (`r001`
really does introduce `classify`, and `classify` really is in the corpus's
vocabulary), so the test targets a load-bearing case rather than a token that
was already absent.

## Naming the eleven rungs to avoid, without naming the one to use

The plan deliberately did not name a rung for the transitivity test.
Prescribing a probe from outside the code is this repository's documented
anti-pattern: the implementer is the one who can see the graph.

But a pre-dispatch computation found something the plan could honestly carry.
**203 of the 214 rungs discriminate between a one-level and a full transitive
closure; 11 do not** (`r001 r002 r003 r005 r006 r007 r011 r013 r014 r083
r171`). A closure test built on any of those eleven passes under a shallow
implementation and proves nothing at all.

So the dispatch named the eleven to **avoid** rather than the one to use. That
is a constraint, not a prescription, and it kept both properties: the
implementer still chose its own probe from inside the code, and it could not
choose a vacuous one. The chosen rung, `r183`, goes from 3 tokens shallow to
22 transitive, and happens also to be a diamond — one rung witnessing reach
and path reconvergence together.

A related judgment was settled by data instead of opinion. Asked whether a
second, hand-picked rung was needed to witness reconvergence, the implementer
scanned the graph: **140 of 214 rungs contain a diamond somewhere in their
closure**, so reconvergence is the norm rather than a rare structure needing a
dedicated witness. Without that scan the answer would have been a guess in
either direction.

## Two deferred minors, closed here

**Task 6's rot-proofing was partial, and is now mechanical.** The headline
test's name states the score — `merchant_coverage_is_five_of_twelve` — so it
is renamed every time the score moves, and Task 6 found a doc comment still
pointing at `merchant_coverage_is_zero_of_twelve`, a test that had not existed
for two campaigns. It fixed that reference, but four live intra-doc links to
the *current* name remained, and each would dangle on the next move by exactly
the same route. Nothing mechanical guards it: an unresolved intra-doc link is
a rustdoc diagnostic, invisible to nextest, clippy, the doctest runner and
`gate-commit` (`TOOL-rustdoc-links`).

Prose discipline was the obvious repair and it is the one that had already
failed, so this campaign added a test instead —
`every_reference_to_the_headline_test_names_a_function_in_this_file` — a
narrow text scan resolving `merchant_coverage_is_*` links against `fn`
definitions in the same file. It was demonstrated red under the mutation it
exists to catch (rename the definition, leave the doc comments; the failure
names both the dangling target and the new definition), with the file restored
byte-identically afterward. It is not a general intra-doc link checker and
does not stand in for one; `TOOL-rustdoc-links` still names the missing
instrument.

**Task 1's empty-diff control verified only half of what it looked like it
verified**, and the report said so rather than being caught saying otherwise.
Regenerating `docs/audits/sentence-coverage.md` and showing an empty diff is a
real control for the merchant path — but `sentence_coverage_report` never
calls `read_derived`, so that control could not exercise the derived reader at
all. Recorded here because a scoping sentence in a task report dies with the
worktree, and the general form is worth keeping: **a control's reach is the
set of code paths the regenerated artifact actually traverses, not the set the
campaign changed.** Task 2 owned the derived reader's proof by design, and
supplied it by mutation.

## Two smaller findings worth the file

**A heuristic guard false-positived on an unrelated identifier.**
`cli/tests/suite/claim_shape.rs`'s seed-loop guard (decision 0093) fired on a
`.map(|s| s.to_string())` closure over a fixed two-element slice — nothing to
do with seeds. `seed_shaped()` is purely lexical (`lower == "s" ||
lower.contains("seed")`), so a bare `s`-named closure parameter is
indistinguishable from a seed variable to it. Renaming the parameter to
`token` was the correct minimal fix and masks nothing, but the cost is real
and belongs on the record rather than being absorbed as noise: **bare
`s`-named non-seed closures are a known false-positive shape for that guard.**

**The plan's one ruling that shaped execution was about where a file may be
split.** Six of seven tasks edited one test file, 883 lines at the plan commit.
Splitting was *permitted in exactly one task, with the roster edit in the
same commit*, rather than mandated: `docs/timings/subfloor-roster.tsv`
selects tests by exact name, so a split spread across tasks means several
roster edits and several chances to drop a test silently from the commit
gate — which is how a predecessor carried a red test for nine tasks. No task
took the permission, and the file closed at 2,057 lines. That is the accepted
cost; the alternative risked a commit gate running a subset of what it
claimed.
