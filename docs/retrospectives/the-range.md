# Retrospective — The Range

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-range.md): a biome-keyed habitat
mask, both preregistered predictions confirmed, a probe-validity ladder that
gained a rung, and a fifteen-seed existence claim converted to a census rate
under decision 0097.

## The spine: seventeen defects, every one in controller-authored text

This campaign ran four implementer tasks, five review passes, one fix wave and
two canonical censuses. **Seventeen defects were found. All seventeen were in
text the controller wrote** — plan steps, briefs, spec clauses, dispatch
instructions, recommendations to the owner. Zero were in implementer code that
survived its own review, and zero were reviewer findings that turned out to be
wrong.

The count is not the lesson; the *distribution* is. Nothing here says the
implementers were unusually good or the reviews unusually thorough. It says the
controller was producing a different kind of artifact from everyone else: prose
that asserts things, dispatched to agents who then execute against it. An
implementer's mistake meets a compiler within minutes. A controller's mistake
meets nothing until someone happens to check the premise.

**Sixteen of the seventeen were claims that were never executed.** A
representative sample, in the order they were found:

- A plan step whose before/after recipe was **mathematically incapable** of
  showing what it claimed. The bake's roster admits only settled kinds and both
  subterranean kinds are fauna, so the unmutated diff it demanded was always
  going to be empty. Followed literally, it would have manufactured a false red
  on the campaign's own founding prediction.
- Two briefs whose **`git add` / file lists were incomplete**, both times because
  widening a function by one parameter rippled to eleven and then eighteen call
  sites the list did not enumerate.
- Test code in a brief that **could not compile** — eight arguments in one call,
  nine in the next.
- A test that, after the obvious mechanical repair of that bug, **could not
  fail**: both sides of the comparison became identical, so it asserted only
  determinism while its name and doc comment claimed it proved absence was a
  no-op.
- A **parser that silently dropped 2 of 33 rows**, including the primary subject
  of the table it was building. It looked complete and five of six spot-checks
  matched.
- A **recommendation drawn from a `CLAUDE.md` line the enforced guard
  contradicts** — the doc said the census refresh was local, the guard refuses
  on any host but the canonical one. The doc was the defect; it is fixed in this
  campaign.
- A **cost estimate off by its own sign**, multiplying a per-world figure that is
  almost entirely world-build cost by a thousand, against a census pass that
  already pays that cost.
- A tally designed at a build depth that **places settlements but commits no
  founder**, so the measurement would have read zero rather than erroring — the
  most dangerous shape on the list, because it produces a plausible number.
- A **proposal that relitigated a ratified decision** which names the very test
  being relitigated as its own worked example.

The uniform diagnosis: each was a statement about the codebase, the tooling or
the cost model that would have taken between ten seconds and two minutes to
check, and none of them was checked. Not one was a reasoning error downstream of
a correct premise.

## The seventeenth, and why it is worth the page

The other sixteen share a cure — execute the claim. The seventeenth was
**executed**, and was still wrong.

A `census_claim` test went red across an absorption. It was diagnosed as a
consequence of a parallel campaign that had turned on optimization workspace-
wide. The diagnosis was not asserted from prose. It was:

- **reproduced in isolation**, red repeatedly on its own;
- **bisected** to a commit range, green on one side and red on the other;
- **cross-checked on a second host**;
- consistent with four separately verified facts — the test is timing-sensitive,
  its file carries a platform split, the merge resolution could not reach that
  module, and the profile change was real.

It was still wrong, and the parallel campaign is exonerated. The machine was at
**load average 615** on ten cores. Four hundred and forty-two leaked ripgrep
processes, every one parented to a single editor extension host that had been up
for five days, were spawning continuously and never being reaped, at about one
percent CPU each. The test asserts a wall-clock bound across four subprocess
spawns. At that load four spawns from a bare shell cost 1.177 s; once the leak
cleared they cost 0.031 s, a factor of 38. The test went from 3.5–4.1 s and
failing to 0.04 s and passing seven times out of seven — **same commit, same
binary, nothing edited**.

Two lessons, and the second is the durable one.

1. **"Reproducible in isolation" is not "independent of the environment."** A
   single-process run in a quiet shell still competes with four hundred
   strangers.
2. **A before/after that straddles an invisible load spike is not a
   before/after.** The bisect was clean and it was measuring a confound. This is
   the *right measurement, wrong attribution* failure the memory index already
   names as the top one — committed while narrating that very failure mode to
   the owner.

What broke it was the owner's one-line correction that the second host was the
same class of machine as the first. That killed the platform story, and with the
story gone nothing was left to explain the split except the box.

**The retraction was appended, not applied.** The ledger's original reasoning is
left standing above it, wrong, with the correction beneath. Editing it would
have produced a tidy document in which no one had ever believed the false thing,
and the shape of the error — four true facts and one unchecked inference,
assembled into a confident cause — is the part worth keeping.

One real defect survives the retraction and is **not** repaired here: the
elapsed time that test measures is started before the claim structure is built,
and building it shells out four times. The test is named "takeover must not
wait" and its message says so, but the quantity it checks is time spent
constructing a claim; on a quiet box it passes for the wrong reason. It was
never broken by this campaign, and repairing it here would read as silencing the
thing that had been red. Recorded for its owner.

## The owner's questions found the two most expensive defects

Two one-line questions, ten minutes apart, overturned two controller
conclusions:

- *"Why is this a seed sweep and not a census test?"* — which found a ratified
  decision prescribing exactly that conversion and naming the test in question.
  Three options had been put to the owner and **none of them was the decided
  one**.
- *"Why would this add any time whatsoever to the census?"* — which inverted the
  sign of a cost estimate that had nearly killed the right decision.

**Neither required knowing the code.** Both tested a premise rather than a chain
of reasoning, and both took seconds to answer once asked. The controller's
reasoning from its premises was sound in both cases; the premises had never been
examined. That is a cheap habit to adopt and it is not adopted by working
harder — it is adopted by asking, of each load-bearing sentence, *how would I
know this?*

## Absorbing once, at the close

The branch absorbed main **once, at the very end**, and met **31 commits** it
had never seen — a workspace-wide profile change, a new metric-classification
axis requiring two fields on every metric literal, and a generated book tree
that is a pure read over the census this campaign had just twice rewritten. The
reconciliation was clean, but it was clean by luck of file placement rather than
by design, and it is where the false `census_claim` diagnosis was minted.

The stage-boundary absorption cadence exists precisely for this. It was skipped
because the campaign had a legitimate reason to hold still mid-measurement, and
then the reason expired and the habit did not resume. The narrow rule worth
writing down: **the mid-measurement exception ends when the readout lands, not
when the campaign does.**

## The recurring shape, which is the campaign's real product

Every finding of this campaign, from the founding measurement to the last review
note, is the same sentence: **the check sits one level away from the thing it
protects.**

- The realm gate was wired to a **readout** whose only consumer is a report,
  while the path that decides worlds took no realm parameter.
- The canonical-host guard lives on the **wrapper script**, not on the writer. A
  committed artifact authored by an ordinary ignored test can be written on any
  box. (Regenerating it on the canonical box and diffing proved byte-identity
  here, so the gap is latent rather than live — but it is a gap, and it was
  found by a subagent asking the question, not by any check.)
- A test's **name and its measurement** were different quantities — twice, in
  unrelated files.
- An **anti-vacuity guard counted comparisons** rather than discriminating
  between them, and so passed a comparison of a value against itself. A guard
  that counts is not a guard that discriminates.
- A **correction had itself gone stale**: prose "fixing" a claim about which
  biome leads a kind's share, where the claim had been true when written,
  reversed by a later regeneration, and had quietly become true again by the
  time it was corrected.
- A **rung tag is a claim about what a metric needs**, and nothing checks it: two
  existing census metrics declare a shallow build rung and read facts emitted
  past it, correct today only because every study that selects them selects
  everything.

The generalisation is not "add more checks". It is that a check inherits its
authority from its *distance to the decision*, and that distance is invisible
in a green run. The ladder amendment this campaign shipped is the one durable
form of the lesson: score a mechanism by whether perturbing it changes the
committed world, not by whether some readout notices.

A method limit fell out of this too, and it is worth naming because the campaign
leaned on the method throughout. **Mutation-based review is structurally blind
to a test that cannot fail.** A reviewer applied exactly the right technique to
three tests and got three right answers; the fourth was never mutated, because
there is no mutation that *should* turn an absent-affinity test red, so nothing
prompts the question of whether it *can*.

## Contaminated measurements, and a ledger that cannot mark them

Several `make gate` timings from the leak window are now committed to
`docs/timings.md` as ordinary rows: a 1573 s gate at a `cpu_ratio` of 0.56, a
1238 s gate reported to the owner as drift against the baseline, and a 20.6 min
gate flagged much earlier as a possible real regression. A one-hour build
timeout in the same window was blamed on a cold `target/`. **None of these mean
anything** and nothing in the file says so.

Two structural facts made that possible. `docs/timings.md` **has no `rc`
column**, so a red gate's wall time — which stops at the first failing test —
lands beside a green one's and reads as a dramatic speed-up; this campaign found
that separately, relabelled its own rows, and recorded the proper fix without
doing it. And the contention suppression that protects `make ci` asks only
whether a *census claim* is held; ordinary load, including four hundred stray
processes, is invisible to it. `CLAUDE.md` already documents that blind spot for
`make ci`. It applies identically to `make gate`, and nothing enforces it for
either.

## What went well

- **Every implementer challenged its brief where the brief was wrong**, and each
  was right to. The first task's implementer refused a recipe that would have
  manufactured a false red; the census-conversion agent refused to write a
  confidence band from fifteen seeds and fixed its rule before seeing the
  numbers instead; the fix-wave agent declined to add a validating constructor
  on the grounds that public fields make it advisory, and that "advisory
  enforcement is worse than none in a campaign about guards that cannot fail."
  Dispatches explicitly invited this after the first task, and it paid every
  time.
- **A withdrawn instruction was verified before being obeyed.** When the
  relitigating fix was pulled mid-flight, the agent read the ratified decision
  itself and confirmed the withdrawal was correct before standing down.
- **Reviews re-executed rather than inspected.** Byte-identity claims were
  reproduced from scratch, not read off reports; mutations were run, pasted red,
  reverted, and confirmed green. One re-review caught an arithmetic slip in the
  fixer's report about fixing errors.
- **Attribution was built into the design, not argued afterwards.** The second
  occupant was chosen as fauna precisely so exactly one peopled kind's placement
  could move, and its inertness is asserted over whole placement lists on three
  seeds rather than assumed from its social form.
- **The world-moved regen was proved additive by shared-column diff**, in its
  cheapest form: the schema changed by exactly one line, its own content hash,
  so the thousand-row rewrite is content and not text.

## Follow-ups

- **A guard on the wrapper is not a guard on the artifact.** The canonical-host
  refusal lives on the census and heavy-tier scripts; at least one committed
  artifact is authored by a plain ignored test that will write it on any box.
  Cross-host agreement for that artifact is now *measured* rather than inferred,
  which lowers the urgency and does not close the hole.
- **`docs/timings.md` wants an `rc` column.** Red-gate rows are indistinguishable
  from green ones, and this campaign committed several.
- **The `make ci` contention guard cannot see ordinary load**, and the same blind
  spot applies to `make gate`. A load-average suppression is the obvious
  candidate and remains unbuilt.
- **A metric's build-rung tag is an unchecked claim.** Two existing census
  columns declare a shallower rung than the facts they read, and are correct only
  by accident of study composition. A study selecting either alone would report
  a silently empty column rather than failing.
- **A test's absent-affinity check is relative, not absolute** — it would survive
  both resolution sites breaking to the same wrong constant. Named at the test.
- **The staple re-witness sweep fell from eleven qualifying pairs to three**, its
  first fall on record. One more roster-scale campaign could leave none, and the
  witness is regenerated by a documented sweep that assumes the population is
  ample.
- **The `census_claim` timing test measures claim construction, not waiting.**
  Untouched here, deliberately.
