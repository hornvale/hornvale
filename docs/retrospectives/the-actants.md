# Retrospective — The Actants

One page of process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-actants.md); the shipped
mechanism is the reverse audit over species and acts, and the naming behind it.

## What worked

- **Asserting derivation instead of population.** Both audits report sets this
  campaign then emptied. A test demanding "12 orphan species" would have had
  to be weakened or deleted at Stage B — by the same author, in the same
  campaign, which is exactly how a guard quietly stops guarding. Asserting
  that the line is *computed from the roster* survived the transition
  untouched: the only edit to `concepts.rs` between Stage A and Stage B was
  one `kind_kebab` match arm, and that fact is the proof, checked as a success
  criterion rather than asserted in prose.

- **Reading the golden's own instructions instead of rebaselining on the exit
  code.** The `solitary_tongue` fixture's failure message names two causes and
  insists on a diff: a *changed* word means the phonology moved and is a bug;
  *insertions only* means nothing moved. Running that test explicitly — every
  committed line still present verbatim — turned three scary red goldens into
  three verified pure insertions (0 of 320 lexicon lines changed, all 76
  proto-roots byte-identical, the world fixture registry-only). A campaign
  that rebaselines three goldens is either the most dangerous kind or the
  safest; the difference is entirely whether someone ran that check, and the
  message was written by a past campaign precisely so someone would.

- **The tripwire earned its asymmetry.** `Action::all()` is held exhaustive by
  a destructure with no wildcard arm, so a fifth act cannot compile until it
  is named. Species cannot have that guard — its roster is data — and saying
  so plainly in the spec was better than pretending the two audits are equally
  strong. Knowing which half of a guard is weaker is worth more than a uniform
  claim.

- **The unaudited direction is on the page.** Naming the prose-vocabulary gap
  as UNAUDITED, with its reason, costs nothing and converts an unknown into a
  known-unknown. The alternative — leaving it out because it was out of scope —
  would have reproduced in miniature the exact failure the campaign existed to
  fix: an absence with nowhere to be written down.

## What to carry forward

- **I shipped an over-tight test one day and broke it the next.** The
  Accession's cohort test pinned `EPOCH_COHORTS.len() == 1` — true the day it
  landed, wrong on the first legitimate append, which was this campaign. The
  invariant is cohort 0's *size* (editing it would re-sort assignments); the
  number of cohorts above it is *meant* to grow. Writing an assertion over
  something designed to change is the same error class as The Accession's own
  headline finding, committed by the person who had just written that finding
  up. **When adding a guard to a structure whose growth is the point, assert
  the frozen part explicitly and leave the growing part alone.**

- **A spec claim about the codebase is not evidence, even in a careful spec.**
  §4 said all four acts were orphans. Three were; `eat` had been a Swadesh
  entry since The Words. Nothing was harmed — the code derived the right
  answer and the audit reported three — but the spec had asserted a fact about
  the registry without querying the registry, in a campaign whose entire
  subject was registry/reality mismatch. The house rule (run the command
  before writing the claim) was applied rigorously to *generated output* and
  not at all to *a count of existing concepts*, because the latter felt like
  something one simply knows.

- **The cheap half of a campaign can be the wrong half to start with.** The
  original plan was "small parity fix now, big naming later." Pricing the
  naming first is what found the ordering defect, which was worth more than
  either stage, and which reversed the cost of the stage that had been
  deferred for being expensive. Spiking the *expensive* item first — purely to
  price it — should be the default when a plan's shape depends on that price.

- **The remaining reverse directions are known and unmeasured.** Predicates →
  concept has never been counted. Prose → concept is bounded but needs a
  design line first. Both are in the followup register; neither should be
  discovered a third time by accident.

## Confidence Gradient

No re-score, same reasoning as The Accession's close: the nearest bet in
`open-questions.md` (*what the world can already check itself on*) is already
high-confidence, and two more self-audits are confirmatory rather than moving.
Recorded per decision 0030 so the absence is a finding rather than an
omission.
