# The Culvert — retrospective

Process, not product. The product is in
[the chronicle](../../book/src/chronicle/the-culvert.md), the sixteen rulings
are in [the campaign ledger](../superpowers/ledgers/2026-09-05-the-culvert.md),
and the full readout with every load average is in the spec's §11.

## The defect shape this campaign produced three times: a value where a predicate was meant

Three separate defects, all in my own text, all the same underlying move — a
specification written as an **observed number** where a **property** was
intended.

**R9.** Task 4's brief specified a test population by its measured figures
("55 of 83 unreachable at band 10") instead of by the property the test needs
("contains at least one unreachable pair"). The implementer matched the numbers,
correctly, and rejected a cheaper construction as wrong — it was wrong for
matching my figures and right for the job. Cost: a 129.337 s test in every stage
gate and every merge, forever, and a second copy of it one task later, because
nextest is process-per-test. Restated as a property, 34.154 s.

**Entry #15, and this one is the deepest.** Task 4's witness was defined as
`calls = sum of the remembered-room set sizes`. That sentence was **true before
the memo and false after it** — the memo's whole purpose is to break the
identity between "candidate rooms" and "route searches". I wrote it as a
definition, so the witness measured the definition. It could go red, and did,
at exactly the predicted 529. Its **green was unreachable by construction**:
it never called the function under change at all, so it would have reported its
pre-fix number forever, and that red reads exactly like "the memo did not work".
Three passes read it — implementer, task review, me — and none asked *can this
test ever pass*, because a red witness before the fix is what everyone was
looking for. The repository's standing rule is that a check which can never fire
is worse than an absent one. This is its mirror image, and it needs its own
name.

**R15.** The set-aside-runs paragraph said the lowest contended reading "would
have read as a 9.8× **fall**". The magnitude (312.98 / 31.838) and the tree
attribution were both right and the **direction was inverted**, which turned
"noise nearly manufactured alarm" into "noise nearly manufactured a pass" — the
stronger claim, and the false one. I had already relayed it verbatim upward.

None of the three was caught by re-reading. The arithmetic is correct in all
three; only the mapping from number to claim is wrong, and arithmetic checks
nothing about that. The correction is left visible in the spec rather than
quietly applied, because a campaign whose own record is the artifact cannot
silently repair the record.

## A slot check, adopted mid-campaign, caught the next instance before dispatch

After R9 I began running each task brief past three explicit slots — what does
this **require**, what has been **observed**, what does it **cost** — before
dispatching. On Task 5's brief it caught two clauses in under a minute, and both
were R9 recurring one task later in text I had already written and re-read:

- an unpriced provenance clause that would have re-introduced the exact 129 s
  construction I had just removed, into the task immediately after the one that
  removed it;
- a `require` phrased as an observed value — "bounded and comparable to the
  home-anchored 83". That is not a decidable predicate, and it decides by
  magnitude a question that is about shape. A population of 200 that has stopped
  growing is safe to memoize; a population of 40 still climbing is not.
  Saturation is the property; the number is only how you see it.

The check is cheap, mechanical, and it fires without suspicion, which is the
whole reason it works. It is the single most transferable thing this campaign
produced.

## Deferred minors went to git-ignored scratch and had to be backfilled

Seven deferred minors were recorded contemporaneously in
`.superpowers/sdd/…/progress.md` — which is git-ignored, per-worktree, and dies
with the worktree. **None reached the committed ledger until the closing walk
went looking.** The autopilot skill names exactly this split (task state to
scratch, rulings and deferred minors to the committed ledger), and I applied it
correctly to all sixteen rulings and incorrectly to every minor.

The closing walk caught it, so the backstop worked. That is not the same as the
practice working: the same worktree recycle that would have cost nothing on the
rulings would have destroyed the entire minors table. The Cartulary records
promotion-at-close as the practice that has failed five times; this is a sixth
in miniature, and the improvement worth wanting is that the closing walk should
not have had to.

The same failure very nearly took a whole task's deliverable: Task 5's 1,077 s
sixty-wait curve and its verdict lived **only** in scratch until the task review
found it, which is the review catching a Cartulary violation rather than a code
defect.

## A moved hash constant is the instrument working, not an alarm

Both campaign-time constants moved on the closing absorption. The response that
looks like diligence — re-record the numbers, note that main moved — produces
the same two values and no knowledge, and is indistinguishable from quietly
accepting a real regression.

What was done instead was one experiment: neutralise the memo's cache lookup so
it behaves exactly as the pre-memo code did, and re-take both hashes **on the
same tree**. Identical in both rows. That attributes the movement in one run,
where a bisect over the absorbed range would have found the same commit at far
greater cost and only after assuming the answer lay in main at all.

The transferable form: a campaign-time constant's *value* is disposable and
expires at close. The **control that attributes its movement** is the thing
worth building, and it must be designed before the constant is needed, not
after it moves.

## The load rule earned its keep in one direction and not the other

Three of **nine** timed runs were set aside on a one-minute load average above
ten, leaving six valid. (This paragraph said "three of six" until the final fix
round — six is the count that *survived*, not the population, so as a
denominator it claimed half the runs were discarded when it was a third. The
spec's own §11.5 table, which lists all nine, was three lines away from the
sentence in every draft.)
Their readings scatter over an order of magnitude in both directions around the
valid runs, and the lowest — taken on a *before* tree — reads as a 9.8× rise in
the measured slope: an apparent catastrophic regression the campaign did not
cause.

The asymmetry is worth stating because the first draft of this lesson got it
backwards. The criterion is a before/after ratio, so contention could **not**
have manufactured a pass: the largest spurious fall available across every
pairing in the set, including set-aside against set-aside, is 1.92× against a
tenfold floor. Contention on this box manufactures alarm, not success. That is a
weaker hazard than the one I first wrote down and a stronger argument for the
rule, since a spurious alarm is the failure mode that costs a campaign a day.

## Two smaller things

**A constraints block is the reviewer's attention, and naming one direction
spends it.** My Task 1 review brief asked, verbatim, whether the new guard would
go red if a predicate were **removed** from the roster. The reviewer answered
that question correctly and reported the guard live-fireable. The defect that had
occurred, twice, was an **addition**. The guard could not fire on it. I posed the
wrong half of a two-directional property and got a correct answer to it.

**A stated preregistered reason can be right in direction and wrong in
mechanism.** The timing criterion was preregistered as the weaker instrument,
with a stated reason — a residual map lookup remains, so the slope stays small
but positive. That reason assumed a **warm** memo, and the probe deliberately
holds a cold one. The prediction's direction was right and its mechanism was
not, and only a same-tree control arm separated them. A preregistered
explanation is a claim like any other; being right about the outcome does not
license the explanation.
