# The Parley — retrospective

**Merged:** 2026-08-18 · **Program:** Myth, campaign 4 of 4

## The freeze leaked, and it leaked through the reading the handoff asks for

Campaign 3 asked that this campaign's preregistration be frozen by a session
that had not read its exploratory readout. The controller read it anyway, on its
**second command**, and disclosed immediately.

The mechanism matters more than the incident. The freeze named **spec sections**
— and the payload also sat in an unmarked table inside the **chronicle**, which
is a file the handoff instructs you to read for orientation. It arrived through
a heading grep run *before* reading anything, while mapping the predecessor's
structure. **A freeze that requires the reader to know where the boundary is
before reading anything cannot survive an orientation pass**, because the pass
that would tell you where the boundary is *is* the pass that crosses it.

The remedy is structural and cheap: **future freezes put the frozen content in a
separate file that is not opened.** Not a marked section, not a warning banner —
a different file, named so the handoff can say "do not open this one".

The contamination was scoped and the scoping held: nothing in that table speaks
to the transmission graph's topology, to stance geometry, or to the clock, and
the unit fix it *does* bear on was excluded from this campaign entirely and left
frozen for a session that has not read it.

## Every defect was in controller text, and every one was caught by running

The fourth campaign running. Four are worth carrying:

1. **A fixture that would have proved nothing.** The planned round-trip fixture
   made the claim under test the ending of the very occupation the raider
   destroyed — but the witness rule already seeds a named attacker as a hop-zero
   witness, so the raider's line *already held that account under descent* and
   the contact edge would have added exactly nothing. Caught by verifying the
   brief against the live source before dispatch. The corrected fixture puts the
   seam on a *later* raid against a descendant, where descent genuinely cannot
   reach.
2. **A preregistered control that was unreachable by construction.** The
   seed-level positive control asked for a world where the mechanism is "absent
   at hop 0 but present as a graph". No world can be in that state: a contact
   edge joins a victim to its own named attacker, so the graph's edges **are**
   the hop-zero seam. The control was chosen for sharpness, against a false
   model of the mechanism. **Before freezing a seed-level control, check the seed
   can reach the outcome the prediction requires.**
3. **The spec stated a wrong mechanism for a correct number.** A draft said the
   raider's child is forced into the bystander label because it can never
   descend from the subject — false wherever the attacker itself descends from
   the subject, and the measurement says 576 of 3,694 land on the victim line.
   The 100.00% is right; the reason given for it was not. The weaker statement
   (a child of the attacker can never *be* the attacker) is sufficient.
4. **A first instrument overstated its own data 5.8×.** Comparing two peoples'
   *sets* of remembered days gave 111 of 138 unequal. The sharper question —
   does each side hold a day the other holds nowhere — gives **19**. Set
   inequality was measuring one-sidedness and calling it disagreement.

One counter-example is worth recording against the pattern: the controller
warned an implementer that two asserted values in a brief were transcribed from
a doc comment rather than a run, and told it the code wins. They matched on the
first try. **The warning cost nothing and would have saved a round had it been
right**, which is the correct price for that kind of hedge.

## Three parties propagated one claim nobody checked

A reviewer asserted that the relaxation terminates because `hops` rises on every
edge. The controller accepted it without checking and wrote it into a fix
instruction. The implementer applied it faithfully. **It was false**: the
ordering key is a tuple compared lexicographically with **width first**, so a
falling width shrinks the key however many hops have accumulated.

It was caught only by a fourth party that **instrumented** the question rather
than arguing it: a relaxation counter on the cycle fixture gives **5**
relaxations under the shipped non-decreasing rule and **1,078** under a
width-halving one, the latter halting only on floating-point underflow. That is
exhaustion, not termination.

Three competent readings of the same sentence, none of which ran anything. The
claim read as reasoning and needed an instrument, which is the shape of every
other defect here.

## A correction leaves copies standing — grep the CLAIM, not the file

Three instances in one campaign:

- The false termination claim existed **twice inside one file** — once in a doc
  comment and once, in miniature, in a match-arm comment. Repairing the first
  left the second. The implementer found it and said so.
- The inflated count ratio (below) was corrected in the section that reported it
  and **survived in the section written to feed the chronicle** — the one place
  whose whole purpose is to propagate.

Correcting the file is not correcting the claim. The remedy that actually works
is a grep for the *assertion* — a number, a phrase — across everything, not a
re-read of the file that carried it.

## Right measurement, wrong attribution — caught two documents from publication

The non-preregistered secondary reported that endings carrying *some*
mutually-exclusive pair of peoples rise **19.46× / 33.48× / 15.54×** under
contact. Those are bare **count** ratios, and the eligible population grows
**7.50× by construction** (548 → 4,112 endings reaching two or more peoples)
because contact only ever adds holders. As **rates**: 6.75% → 17.51%, 6.02% →
26.87%, 12.59% → 26.07% — **2.59× / 4.46× / 2.07×**. Roughly a fourfold
inflation.

It was headed for the chronicle and an idea-registry row. The frozen verdict was
never at risk — its population is pinned at 548 on both arms and all three rules
— but the number a reader would have remembered was wrong, and the controller had
already repeated it once before catching it.

## Tests that looked like coverage and were none, found only by mutation

Twice, in the same campaign, and both times the discovery was a mutation rather
than a review:

- Against a self-raid mutation, an `edges() == 0` assertion is **vacuous**:
  deduplication collapses the doubled self-entry and integer division gives
  `1/2 == 0`. The test survives only because it *also* asserts the peer list is
  empty. One half was load-bearing and the other was decoration that read as
  proof.
- The first round-trip fixture could not detect a wrong-route regression at all,
  because the tree route won on **every** component of the ordering key — so
  only reversing the comparison flipped it, and that hangs rather than reddens.
  The implementer found this itself, deepened the tree line so the two routes
  genuinely disagree, and confirmed that blanking the key's width component now
  reddens exactly one test in the crate.

A third of the same shape, found by the deliberate mutation step: the contact
edge's day filter was **unguarded** — removing it produced zero failures across
the whole crate. It is now pinned by a fixture *pair* differing only in the order
of the two days, which is a negative control rather than a second positive one.

## Two rulings that paid for themselves, and one implementer correcting a reviewer

- The clock's orphaning mechanism was asserted in a fixture rather than left
  implicit: an entity that never ends, and therefore passes the aliveness test
  on its own account, collapses out of the holder set because its only teller
  was dead. That is the whole of the clock hypothesis, exercised by a fixture
  that said nothing about it until the assertion was added.
- An implementer found a **reviewer's** suggested assertion false on the fixture
  it was suggested for — the seam route accumulates 37.0 days against a rung at
  41.7 — and asserted the measured fact instead, with the arithmetic. Reviewers
  are not oracles, and the correct response to a suggestion is to check it.

## The instrument split was the single most expensive available mistake

Two walks exist in this window. Only one honours the contact seam; the other
honours the clock and not the seam. A readout that varied the contact arm
through the *wrong* walk would have printed identical columns per arm and read
as **"contact does nothing"** — a wrong attribution presented as a null, on the
campaign's headline. It was named as the trap in the dispatch, verified in code
afterwards (one call site, inside the stance block, under descent), and the
battery's module doc states the split at the top so the next reader cannot
repeat it.

## An empty drift on two audits is evidence in neither direction

Task 7 moved six idea-registry rows (all `KNOW-*`) and checked the generated
audits that cite the registry. `docs/audits/system-matrix.md` and
`docs/audits/system-coverage-wolverson-2021.md` came back byte-identical — and
that fact says **nothing about whether the row edits were correct**, for two
independent reasons, neither of which is "the drift check is broken".

1. **Neither artifact reads the registry when it renders.** `systems report`
   calls `systems::render(&corpus, path)`, whose signature does not even take
   `RepoFacts` — the corpus alone decides every byte. `systems matrix` calls
   `render_matrix(corpora, facts)` and touches only `facts.subsystems`, the
   source tree. The registry *is* read, by `RepoFacts::gather`, and consumed by
   `systems::audit` — whose findings surface through `hornvale systems check`
   and `cli/tests/system_coverage.rs`, and reach no committed artifact.
2. **No corpus item anchors a `KNOW-` row anyway.** The report's twelve
   `registry:` anchors are `CLIENT-*`, `MAP-*`, `MAT-*`, `MEM-8` and `PLAY-*`.
   Every row this campaign touched was invisible to the corpus before the render
   question arose.

The final reviewer corroborated the shape from a third path:
`docs/digest/intent-vs-reality.md` contains **zero** `KNOW-` tokens, so its
byte-identity cannot report on a `KNOW-` row edit either.

The reusable part: **an empty diff on a generated path is a positive result only
if that path's render reads what you changed.** Check the renderer's signature
before reading its silence as agreement — here two of three artifacts could not
have moved under any registry edit at all, and the third could not have moved
under this one. What actually verifies a registry flip is
`cargo test -p hornvale --test docs_consistency` (form, cap, uniqueness, links)
and `cli/tests/system_coverage.rs` (anchors), and a future campaign reading an
empty drift on those audits as reassurance would be reading noise.

## Process notes

- **The panel size was measured, not guessed**, as in the two predecessors: a
  three-seed pilot cost 2.07 s/seed against a 5 s/seed decision bar written
  before it ran, so the forty-seed panel stood. The full battery then measured
  90–92 s.
- **The readout re-derives the substrate probe's own published figures**, because
  its twelve-seed prefix is a strict prefix of the forty-seed panel: 5,913
  endings, 138 foreign attackers, and 19 mutually-exclusive events, all now
  **asserted** rather than printed. The third is discriminating rather than
  coincidental — the same prefix gives 10 under the additive rule and 12 under
  quadrature, so a wrong rule index reddens. A red there means the world bake
  moved and must be re-derived in the commit that moved it, never rebaselined to
  go green.
- **An interpretive conclusion was printed unconditionally beside numbers that
  could have contradicted it.** It is now gated on the condition it asserts, with
  an explicit branch for the other outcome. A sentence that prints either way is
  not a finding.
- **Nothing was retuned to rescue a prediction.** Verified by commit ordering
  rather than by diffstat: the spec was last touched 22 commits before the
  readout, and the two derivation files two commits before it.

## Deferred, with homes

- **The readout could be a committed, drift-checked artifact** instead of stdout
  from a heavy battery — the same complaint the last two campaigns filed about
  their own numbers being pasted into prose rather than re-derived. **Not
  adopted here**, because the authoring path for such an artifact is unverified
  and this campaign had no budget to verify it. Recorded so the third filing does
  not read as the first.
- **The pooling mechanism is named but not measurable.** A claim does not carry
  the witness it originated from, so "each side keeps the other's least-corrupted
  telling" is an inference from the counts rather than something the model can
  show directly.
- **The equal-key tie-break** between two distinct routes of equal cost is
  decided by frontier expansion order — deterministic, and unstated.
- **The argument that a route through another witness can never win** assumes
  every people's ladder shares a rung-zero span. True on this panel; the social
  ladder sorts durations by length, so a sub-day generation would break it.
- **The unit erratum stays frozen** for a session that has not read the
  exploratory column — unchanged from where campaign 3 left it, and untouched
  here on purpose.
