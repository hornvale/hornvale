# Retrospective — The Tithe (Tribute)

*Living-community program, campaign 4, slice 2. Process lessons, not product.*

## What went right

- **Preregistration is what made the fifth amendment worth making.** By the
  time the wounded-patron mechanism was proposed, the campaign had amended its
  own spec four times, three of them after a disappointing measurement. The
  cumulative shape of that is metric-chasing whatever each local justification,
  and the useful move was not to refuse the amendment — it was defensible on
  realism independently of any metric — but to **name the pattern to the owner
  and write the predictions into the spec before any of the code existed**,
  with both branches made informative in advance. The spec said in so many
  words that revolts firing while the shape stayed geometric would be a
  *stronger* falsification than the standing null. That is exactly what
  happened, so the campaign's headline is an adjudicated prediction rather than
  a number that was then explained. **A late amendment is safe in proportion to
  how much it commits to in advance.** The disclosure that carries this now
  lives in three places — the spec, the test module's own documentation, and
  the chronicle — because a reader who meets the final histogram alone has been
  misled.

- **Budgeting an investigation instead of a recalibration bought the
  campaign.** A stage shipped, the world thinned by 62%, and a gate reddened.
  The cheap move was to tune a constant. Instead the stage's own stated
  explanation was **falsified** by measurement, and the investigation found
  that the ratio of assessment to visible capacity took *exactly one value*
  across all 2258 relations — there was no patron-side term anywhere in the
  mechanism, and a bust-out and a Roman census were indistinguishable in
  principle. Every later amendment, and the campaign's only unambiguous win,
  came out of that finding. It also overturned the red gate's *stated*
  mechanism, which had been wrong. **A disappointing measurement is worth an
  investigation before it is worth a constant.**

- **Opposed knobs shipped in separately measured stages, and it paid.** The
  plan required a seed-42 readout between every stage precisely because
  assessment raises extraction while concealment lowers it. That discipline is
  what let a later readout notice that concealment **lowered the rate and
  raised the total** — a longer relation life more than compensating a smaller
  integrand — and flag in advance that an attribution reading total tribute
  alone would mis-sign the term. A counter separating rate from volume was
  added on the spot.

## What to do differently

- **The metric-chasing pattern needs an earlier tripwire than the fifth
  amendment.** Five amendments, four post-measurement. Each one was reviewed on
  its own merits and each passed; nothing in the process looks at the
  *sequence*. The lesson is not "amend less" — three of the five fixed genuine
  errors that would have adjudicated the wrong model — but that the count
  should be surfaced by the process rather than by an agent happening to
  volunteer it. **Proposed rule: the second post-measurement amendment in a
  campaign triggers the preregistration requirement**, rather than the fifth
  triggering it voluntarily.

- **A parallel campaign landed a people inside this one's mechanism, and it was
  discovered only at absorption.** The Vacancy added a fifth settled people
  whose authored `time_horizon` is 0.2 — shorter than any of the three horizons
  this campaign's strategy family was designed, measured, and characterised
  around, and therefore **the most extractive patron in a family that had never
  seen it**. Every count roughly doubled; the verdict survived unchanged, which
  is luck as much as design. Ancestry checks cannot see this: no file
  conflicted, and neither campaign's physics disagrees with the other's. The
  checkable half of collision detection is mechanised; the semantic half is
  still a human reading of what the other branch *means*. **When a campaign
  indexes behaviour on an authored per-people scalar, the roster is part of its
  blast radius** — enumerate the peoples the way a consumer enumeration is done
  by class, and re-check at every absorption, not only at the last one.

- **A heavy-tier gate was red on `main` and nothing reported it.** `make gate`
  structurally cannot see the heavy tier, so a cost gate stayed red until a
  task happened to run the battery. The same structural blindness bit twice
  more in this campaign: a committed heavy-tier artifact (the history study's
  rows and summary) was stale because `regenerate-artifacts.sh` cannot see it
  either, and it surfaced only when the batteries ran at absorption. The
  recurring shape is that **the everyday gate's silence is not evidence** about
  anything the heavy tier owns. Until the tiers are reconciled, a campaign that
  touches the bake should run `make gate-full` at least once per stage, not
  once at the close.

- **A clean textual merge created a duplicate registry ID.** Main landed a
  600-character cap on registry rows and compacted one row to satisfy it; this
  branch carried the pre-compaction text of the same row. Git auto-merged the
  file **without raising anything** and emitted both versions — a duplicated
  ID, caught by the drift-check rather than by the merge. The file is a
  line-oriented table, which is exactly the shape that merges cleanly and
  wrongly. **Any campaign editing the registry should re-run the drift-check
  immediately after absorbing, before assuming a clean merge is a correct
  one.** The same absorption exposed a second registry defect for free: this
  campaign's own row had been written at 5752 characters *before* the cap
  existed, and because the waiver list is append-never the sanctioned fix was
  to compact it to a 587-character index entry, not to waive it.

- **Four non-binding assertions and three green-and-unreddenable tests, again.**
  The predecessor found four; this campaign found the same class repeatedly.
  The most instructive: an assertion that "tribute must milk, never kill" could
  not be reddened by *any* tribute defect, because starvation was unreachable
  in its fixture by construction and tribute only ever lowers pressure. It was
  replaced by a per-vassal between-epoch population floor over the real bake —
  and **the evidence for the replacement is not that the new test passes, it is
  that the old test stays green under the very mutation the new one reddens.**
  Later, a discount-rate change made a *previously binding* test unreddenable
  by making the take a function of two variables alone; the negative result was
  kept and the test renamed and repointed rather than quietly left in place. A
  test's binding-ness is not a property it has once — **it decays as the
  mechanism under it changes**, so mutation-verification belongs at every
  review that touches the mechanism, not only at the review that introduced the
  test.

- **The whole-branch review found 22 committed facts dated before the entity
  they named existed.** Every per-task review was green, and correctly so: the
  defect is an *interaction* between amendment 5's relation continuity and the
  emit path's dating, and neither task owned both halves. It was confirmed
  against the committed seed-42 world before the fix and pinned afterward by an
  invariant that reads the emitted ledger rather than the in-memory records —
  so the check cannot agree with itself while the emitted pair disagrees. **The
  final whole-branch review earns its cost even when every task-scoped review
  passed**, which is now the third campaign to record that.

## Tooling notes

- **The census path changed mid-campaign** (decision 0081). The refresh is
  `scripts/census-run.sh`, **not** `HV_CENSUS=1 scripts/regenerate-artifacts.sh`
  — only the former serializes against another heavy writer on the box and
  records the run in the timing ledger. Both are guarded; only one queues. The
  plan was written before the decision landed and named the old path; nothing
  reddens if the wrong one is used, which is exactly why it needs saying.

- **The golden-pin tripwire fires wider than the tripwire reports.**
  `make census-check` was green on the SQL pins alone while **five** Rust-side
  pins in two sibling calibration files were still stale, and only the full
  workspace suite catches those. The close re-pinned thirteen literals where
  the dispatch had predicted three. Related: this file's per-pin comment trails
  and its module-header table are two conventions that had both accumulated in
  it, so the last line of several trails named a value that was not the literal
  beside it. The re-pin used both conventions deliberately, so the trails are
  internally consistent again from here forward.

- **A pre-existing cross-platform test failure can masquerade as a merge
  regression.** Four tests failed on this machine after absorbing, all in
  another campaign's claim machinery, which probes process liveness through
  `/proc` — Linux-only, and this box is Darwin. The proof that it was not this
  branch's was mechanical (`git log … --not origin/main -- <path>` empty, and
  the file byte-identical to main), and it is worth doing that check *before*
  investigating, not after. The behaviour is deliberately Linux-only, so the
  *tests* were gated to Linux rather than the probe being given a Darwin path,
  which would be a design call and not a merge resolution. The wider point is
  that `make gate` could not go green on the Mac **on main either**, and
  nothing said so until a close happened to run the full suite there.
