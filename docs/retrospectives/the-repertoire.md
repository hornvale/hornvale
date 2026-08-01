# The Repertoire — retrospective

Process, not product. The product is in
[the chronicle](../../book/src/chronicle/the-repertoire.md).

## Every substantive review finding was a defect in the plan, not its execution

This is the campaign's one real process signal, and it is unusually clean.
Across Tasks 2, 3 and 4 the reviews raised one Critical, nine Important and
twelve Minor — counted **as adjudicated**, which is what the ledger's four
per-task summary lines record and which includes one finding a reviewer filed
Minor and the controller elevated because it fed a published count. Counted as
the reviewers first filed them it is one, eight and thirteen. **Not one
Important-or-worse was a transcription error** under either convention. Every
one of them was already present, verbatim, in the plan's own code listings:

- **The Critical (Task 3).** The leverage table ranked bundles under a heading
  reading "Missing bundles by fan-in", but the loop gated on the *situation*
  being blocked, so a blocked situation contributed every bundle it requires —
  including seven the world already fully satisfies. Thirty-eight rows where
  thirty-one belong. The spec makes the ranked misses the deliverable, so this
  was the worst available place for a wrong number, and it was written into the
  plan.
- **The fail-open (Task 4).** The ratchet tested `env::var("REBASELINE").is_ok()`,
  which accepts `REBASELINE=0` and `REBASELINE=` (empty). Both were verified to
  pass a tampered artifact *and* silently rewrite it. `kernel/src/golden.rs`
  already has the deliberate guard; the plan reimplemented it worse.
- **The inverted default-deny (Task 2).** An unknown bundle name expanded to an
  empty token list, so a situation whose requirements were all dangling
  resolved *stageable* — the exact inversion of the spec's posture. Live data
  was clean, so no number moved, but a hand-authored corpus plus a typo would
  have softened the ratchet.

The implementers transcribed faithfully throughout; three of them ran
verification the plan did not ask for. The review effort that paid was **review
of the plan's listings against the spec**, and it was paid late — at
implementation time, three times, by three different agents. All three were
resolved by amending the plan first and re-dispatching, rather than escalating
as a plan-versus-rubric conflict, because in each case the plan was simply
wrong relative to the spec and the spec governs.

**Do differently:** a plan whose tasks are mostly literal code listings should
have those listings read against the spec's own invariants *before the first
dispatch* — as a distinct pass, not as part of writing them. Two of the three
above are one-line predicate errors that a reader asking "does this listing
implement default-deny? does this one rank what the heading claims?" would
catch in minutes.

The corollary, worth keeping: **the fail-open was found by mutation, not by
reading.** So was the leverage filter's exactness — the re-review checked the
fix in both directions, confirming that the thirty-one kept rows each have an
unheld token and the seven dropped are each fully satisfied, because an
over-aggressive filter would have produced an identical row count. This is the
Confidence Gradient's standing floor firing again: the only thing that reliably
distinguishes a check that fires from one that does not is making it fail on
command.

## Commit-message prose must never pass through a shell

Two incidents this campaign, in opposite directions, from one root cause.

1. A commit message composed in a heredoc contained backticks. They expanded as
   **command substitution and ran a real gate**, whose stdout landed in the
   message. Repaired by amend — but not completely: `772cf3ee`'s message still
   claims to fold in a gate timing row when what it carries is two rebaseline
   rows. The gate row arrives one commit later in `88b11b45`, whose message
   says so plainly. Permanent history therefore contradicts itself, and it is
   left that way deliberately: re-amending published history to tidy a
   description is the worse trade, and the successor commit already records
   the truth for anyone who follows the file.
2. Separately, the repo's bash guard **blocked commits three times** because
   the message *prose* named a test command. Nothing was wrong with the commit;
   the guard was reading the command text and correctly refusing something that
   only looked like an invocation.

Both parties converged independently on the same fix: **write the message to a
file and use `git commit -F <file>`.** Prose that never reaches the shell can
neither execute nor be blocked. Filed as `PROC-commit-message-via-file`.

## This box is not one of the two decision 0086 names

Two related observations, both first-of-kind:

- `docs/timings.md` now carries rows from host **`ambrose`** (12 cores), the
  first for that host. Decision 0086 names only the Mac and lefford. Harmless
  for the commit gate, which writes no baseline.
- `docs/timings/` has **no `test-baseline-ambrose.tsv`**. The per-host duration
  baseline has therefore forked: the first `make ci` run here will find no
  file, record silently, and be unable to alarm. That is CLAUDE.md's documented
  blind spot (2) firing for real rather than hypothetically — first-run-never-
  fails is deliberate, but spending the free pass unnoticed is not.

## Estimate deltas

Five tasks, all landed. Task 1 and Task 2 took one fix round each; Task 3 took
two (the Critical plus a prose round); Task 4 took one. No task exceeded its
shape — the cost was concentrated entirely in review-and-fix, which is what the
plan-defect pattern above predicts. The campaign touched no world state, drew
no seed, and needed no census.

Preregistration paid its rent on schedule. P3 — the corpus-validity kill — was
scored the day the corpus froze, before a line of resolver code existed. Had
the lattice come back flat, nothing built afterwards could have rescued the
instrument; that ordering is the whole point and it is worth stating that it
was honoured rather than assumed.

## The freshness sweep moved no bet

Stated explicitly rather than skipped silently (decision 0030). None of the
Confidence Gradient's three open bets — refinement at scale, emergent
economics, historiography worth reading — was resolved or moved by this
campaign. The instrument scores *representability* only, and says so in its own
provenance section; whether an agent could plan or recognise a situation is a
different bet and remains untouched. What the sweep did add is a third instance
to the preamble's running account of checks that cannot fail, because the two
found here came from a campaign with no world state at all, which narrows the
diagnosis from "measurement code" to "plans written as literal code listings".

## Follow-ups

Promoted here in full, because the scratch register dies with the worktree.

- **F1 — `excluded_by` is read with `.first()` and silently drops a second
  reason.** Exactly one excluded situation with one reason today, so nothing is
  lost. The chronicle's P4 finding makes this live: if the belief-mediated
  reading of #31 is accepted, that situation grows a second reason and the
  `.first()` must become a join.
- **F2 — the third verdict may be earning its place zero times.** #31 is the
  only situation carrying an exclusion *and* the only situation requiring
  `divine-agency`, whose two predicates the registry does not hold. The
  resolver short-circuits on exclusion, so the artifact cannot show that #31 is
  also structurally blocked. The next corpus revision owes an answer; it was
  deliberately not answered here, because changing the corpus after unblinding
  would move the score P4 was made about.
- **F3 — the Supply caveat buries its own point.** Four sentences of hedging
  with spec citations up front; "read it as *unrequired by this catalogue*, not
  *unused*" is third. Declined at close rather than trimmed: it is prose inside
  a byte-ratcheted artifact, so the fix drifts the artifact and its generator
  for ordering alone, and the chronicle now carries the point. Fold it into the
  next change that touches the renderer.
- **F4 — the Supply section is half of what the spec asked for.** Tokens no
  situation requires *and no readout consumes* was the ask; only the first
  conjunct is implemented, which is why the list contains `predicate:is-a`. The
  spec's Goodhart guard cannot serve until the second half exists.
- **F5 — the ratchet is not a direction check.** A whole-file byte compare is
  strictly stronger than the spec's per-situation ratchet given identical
  accept semantics, but it reddens on improvement exactly as on regression, and
  a reader coming from the spec will expect otherwise.
- **F6 — two Provenance lines in the artifact exceed 76 columns** (338 and 78).
  Out of scope for the wrapper by instruction; recorded so a future wrap sweep
  knows they were seen and not missed.
- **F7 — `hornvale tropes check` resolves both `--corpus` and the committed
  artifact path relative to the working directory.** It fails loudly with a
  clear message and the gate test sets `current_dir`, so no action is needed
  unless a script ever calls it from a subdirectory.
