# 0139. Main advances only through the lock

**Status:** Accepted (2026-08-16) · **Decider:** Nathan · **Amends:**
[0132](0132-three-gates-named-for-the-campaign-moment.md)'s rung table,
[0133](0133-nontrivial-checks-run-in-one-serial-lane.md)'s placement table

In the context of a project where every gate ever run has tested a tree that
is not the tree that becomes `main` — a branch tip, never the merge product —
we decided that **every commit on `origin/main` is the tip of a tree that was
gated as itself, by a serial merge queue, immediately before it was pushed**;
that the merge product, not the branch tip, is the object the queue proves
things about; and that a commit landing on `main` outside the queue is a
detected fault, not a silent weakening of the guarantee.

## Context

**The incomparable-guarantees argument.** `lane-run.sh` does
`checkout --force "$ref"` followed by `reset --hard "$ref"`, so the tested
tree is exactly a campaign branch tip by construction. What actually lands on
`main` is that branch merged into whatever `main` is at merge time, and
nothing has ever built, let alone gated, that object. A gated branch tip and
an after-the-fact red `main` are *incomparable* — neither implies the other.
The weakest guarantee that implies both is "the merge product was gated," and
it has never existed in this project.

**The 0134 collision.** Two campaigns each minted decision `0134` against the
`main` they branched from. The slugs differ, so the merge raised no conflict
and both files simply coexist; `docs/digest/` renders one line per file, so
the duplicate reads as a normal entry. `no_gaps_in_the_decision_log`
(`cli/tests/docs_consistency.rs`) names "a collision with a number that
arrived on main" as a cause of gaps in its own doc comment, and cannot catch
one, because a duplicate creates no hole. The branch was green throughout.
The merge itself was never tested. `no_two_decision_records_share_a_number`
closes that specific hole; it does not close the class — the same defect
resurfaced a second time inside this very campaign (Task 10's own number
check), avoided only because someone looked instead of trusting `main` alone.

**Wasted serial capacity.** In the lane's first 27.4 hours (`jobs.tsv`,
2026-08-14T19:38 → 2026-08-15T23:02): 46 jobs, 21.2 h total wall time, of
which 14.2 h (67%) was queue wait and 7.1 h was actual compute; 13 distinct
refs were gated, and 4 of those 13 never landed in `main` — 15 jobs, 33% of
all lane work, spent proving things about trees nobody would ever run. The
mechanism is named in the idea registry as `TOOL-lane-supersession`: a
re-dispatch queues *behind* the dispatch it replaces, so a fast-moving
campaign starves the lane with its own obsolete work. Head-of-line waste
compounds this structurally, not incidentally — `gate-campaign` was six
separate lane dispatches, so it paid the queue wait six times over, with mean
queue wait per job across the six campaign-rung sets ranging 903–1823 s.

## The ruling

**`main` advances only through the lock.** A serial merge queue on the
canonical box constructs the merge commit first, gates it as itself, and
pushes it unchanged — the object that carries the guarantee is the object
that lands. This is a lock, not a gate: a gate tests an object it does not
change, and every domain that has already solved this shape (a canal lock, an
airlock, blood crossmatching) tests the *combination*, not either side alone.
Testing the donor branch in isolation is exactly the defect this replaces.

Two corollaries the design protects:

- **Tested SHA == pushed SHA.** No commit is created after the last green
  result; the merge commit gated is the merge commit pushed.
- **`main` advances only through the queue.** The guarantee is inductive —
  each merge builds on an already-proven `main`, which is what lets the queue
  prove only the delta. Anything landing out of band breaks the induction
  silently unless the queue itself detects it.

## Consequences

- **`gate-campaign` retires.** It gated a branch tip, and nothing ever built
  the object that actually lands; the merge queue gates the merge product
  directly and pushes the exact SHA it tested. `make gate-campaign` is now a
  refusing signpost naming the replacement, the same shape decision 0132 gave
  `make gate`/`make ci`.
- **`gate-stage` and `gate-commit` are unchanged.** `gate-stage` buys author
  confidence at plan-stage boundaries and is cheap relative to the merge
  queue's integration set; `gate-commit` stays local, seconds-scale, and
  host-unguarded. Neither carries the merge-product guarantee, and neither
  needs to — that guarantee is specifically about what lands on `main`.
- **The census stays outside the guarantee.** It is not in the queue's
  automatic path: a census regen requires explicit authorization under
  `campaign-autopilot`'s carve-out, and its cost has historically spanned 20x
  (882 s to 19,207 s). A merge can land with a census that predates it; a
  15-minute tripwire (a fixed 900 s ceiling read against the last `| census |`
  row in `docs/timings.md`) flags when that gap is worth closing, but the
  queue does not close it automatically.
- **Head-of-line blocking is accepted deliberately, with `hold and fix` as
  the policy.** The queue stops on red; the operator diagnoses; the campaign
  behind it waits. At a measured 3.5 merges/day this leaves room for an hour
  of debugging, not four, and the accepted cost is that a broken campaign
  blocks every campaign behind it in the queue, not only itself — the same
  trade every strictly serial resource makes. The mitigations are that the
  hold is loud (status reporting, a board notice with
  `polarity=hold-off`), that triage is classified against named evidence
  rather than guessed, and that manual eviction stays available as an
  explicit operator action, never a default.
- **Out-of-band landings are a detected fault.** The queue records the SHA it
  pushes; at the mouth of the next request, `origin/main` must equal that
  SHA, or the queue says so loudly instead of quietly resuming with a weaker
  guarantee than it advertises.
- The lane's dispatch machinery for the retired `gate-campaign` path is
  deleted along with it, not left dangling — dispatch to the merge queue
  replaces dispatch to the campaign rung of the lane.

## See also

`docs/superpowers/specs/2026-08-15-the-sluice-merge-queue-design.md` §1-6;
decision [0132](0132-three-gates-named-for-the-campaign-moment.md);
decision [0133](0133-nontrivial-checks-run-in-one-serial-lane.md);
decision [0081](0081-one-heavy-writer-per-box-claimed-at-the-write-seam.md).
