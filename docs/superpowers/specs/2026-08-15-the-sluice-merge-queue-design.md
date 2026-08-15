# The Sluice — a merge queue that gates the merge product

**Status.** Spec, awaiting G3 review.
**Campaign.** The Sluice (`campaign/the-sluice`).
**Supersedes in part.** Decision 0132's `gate-campaign` rung; decision 0133's
placement of campaign-rung sets.

---

## 1. The defect

Every gate this project has ever run tests a tree that is **not** the tree that
becomes `main`.

`lane-run.sh` does `checkout --force "$ref"` followed by `reset --hard "$ref"`,
so by construction the tested tree is exactly `$ref` — a campaign branch tip.
What lands is that branch **merged into whatever `main` is at merge time**, and
nothing has ever built, let alone gated, that object. The two things this repo
does gate — a branch tip, and `main` after the fact when someone notices it is
red — are *incomparable*: neither implies the other. The weakest guarantee that
implies both is "the merge product was gated", and it has never existed here.

This is not theoretical. Three witnesses, all from the last week:

**The 0134 collision.** Two campaigns each minted decision `0134` against the
`main` they branched from. The slugs differ, so the merge raised **no
conflict** and both files simply coexist; `docs/digest/` renders one line per
file, so the duplicate reads as a normal entry. `no_gaps_in_the_decision_log`
(`cli/tests/docs_consistency.rs`) names "a collision with a number that arrived
on main" in its own doc comment as a cause of gaps — and cannot catch one,
because a duplicate creates no hole. The branch was green. The merge was never
tested. `no_two_decision_records_share_a_number` closes that specific hole; it
does not close the class.

**Wasted serial capacity.** In the lane's first 27.4 hours (`jobs.tsv`,
2026-08-14T19:38 → 2026-08-15T23:02):

```
  46 jobs
  21.2 h total wall time
  14.2 h of that is QUEUE WAIT (67%)
   7.1 h is actual compute
  13 distinct refs gated
   4 of those 13 never landed in main  (15 jobs — 33% of all lane work)
  20 of 46 jobs returned non-zero (43%)
```

A third of a strictly serial resource was spent proving things about trees
nobody will ever run. The mechanism is named in the idea registry as
`TOOL-lane-supersession`: a re-dispatch queues *behind* the dispatch it
replaces, so a fast-moving campaign starves the lane with its own obsolete
work.

**Head-of-line waste is structural, not incidental.** A campaign gate is six
separate lane dispatches, so it pays the queue wait six times. Mean queue wait
per job across the six campaign-rung sets ranges 903–1823 s.

## 2. The invariant

> **Every commit on `origin/main` is the tip of a tree that was gated as
> itself, by the queue, immediately before it was pushed.**

Two corollaries the design must protect:

- **Tested SHA == pushed SHA.** No commit is created after the last green.
- **`main` advances only through the queue.** The guarantee is *inductive* —
  each merge builds on an already-proven `main`, which is what lets the queue
  prove only the delta. Anything landing out of band breaks the induction
  silently, so the queue must detect it (§6.4).

## 3. It is a lock, not a gate

A *gate* tests an object it does not change. Every domain that has solved this
shape says otherwise: a canal lock equalises a vessel to the destination level
and *then* passes it; an airlock will not open the inner door until pressure
matches; transfusion medicine does not test donor blood or recipient blood but
**crossmatches** them and watches what happens when they are mixed. Testing the
donor alone is exactly the present defect.

Calling the missing instrument "a fourth gate" is part of why the hole stayed
invisible. It is a lock: the chamber transforms the candidate into destination
terms before producing any evidence about it.

Two moves come back from those domains and are adopted below:

- **Turn away at the gate, not inside the chamber** (§5.2) — mergeability is
  checked *outside* the serial claim, because a candidate that cannot merge
  must never consume the box.
- **The lock-keeper is not the crew** (§6) — the operator triages and decides;
  it does not become the campaign's implementer.

## 4. Scope

**In scope.** A serial merge queue on lefford; the `integration` lane set; the
retirement of `gate-campaign`; out-of-band-landing detection; a census duration
tripwire.

**Out of scope, deliberately.**

- *Speculative/parallel batching.* At a measured 3.5 merges/day (104 merges
  into `main` in 30 days) it is pure complexity. Revisit at ~3× the rate.
- *Retiring `gate-stage`.* It buys author confidence at plan-stage boundaries
  and is cheap relative to the integration set. Unchanged.
- *Retiring `gate-commit`.* Unchanged; local, seconds, every commit.
- *Automatic census.* See §7.

## 5. Architecture

Two halves, split by whether the work needs judgment.

### 5.1 The request

`scripts/sluice-request.sh <branch> <full-sha>` is the caller's side and runs
on **any** machine — the same shape as `lane-dispatch.sh`: validate locally,
`ssh`, print a request id, return. It never blocks.

The durable queue is `~/.local/state/hornvale/sluice/queue.tsv` on lefford,
beside the lane's own `jobs.tsv`, for the reason `lane-run.sh` already gives
for that location: it is the only record that a request ever existed, and
`/tmp` does not survive a reboot.

A wire message to the operator session is a **nudge, never the request**. The
request is durable on lefford before any session hears about it; a session that
dies between the message and the merge loses nothing.

**Ordering.** FIFO by enqueue time, with per-branch coalescing: enqueueing a
SHA whose queued predecessor is its own ancestor **replaces** that entry and
ledgers it `superseded` rather than dropping it silently. `git merge-base
--is-ancestor` is the whole test — ancestry, not branch name, so it survives
rebases and detached refs. This is `TOOL-lane-supersession`'s prescribed fix,
applied here first because the queue is where a superseded request is cheapest
to discard.

### 5.2 The mouth — checks that run OUTSIDE the claim

Cheap, read-only, and before the box is consumed. Any failure escalates to the
operator without ever taking the lane claim:

1. The SHA is pushed and reachable (`lane-dispatch.sh` already validates this).
2. It is not already an ancestor of `origin/main` (already merged → drop).
3. `origin/main` is exactly the SHA the queue last pushed (§6.4).
4. **The merge is clean**, computed with `git merge-tree --write-tree` — no
   worktree, no checkout, no claim. Verified available: lefford runs git
   2.39.5, and `--write-tree` needs 2.38+.

Step 4 is the canal-lock move and the single largest efficiency item in this
spec: a conflicting candidate is turned away in milliseconds instead of after
acquiring a resource whose mean queue wait is measured in the thousands of
seconds.

The mouth also computes and reports two risk signals it gets for free, neither
of which gates anything:

- **Age** — `git rev-list --count $(git merge-base main $sha)..main`, how far
  behind the branch's merge base is. High values predict the semantic-drift
  class (The Tumult and The Waterline collided with a clean preflight GO).
- **Scope** — which layers the diff touches (`kernel/` → `domains/*` →
  `windows/*` → `cli/`, plus `clients/` and docs-only). `gate-commit`'s own
  cost already tracks this (kernel 470.8 s, domains ~84 s, cli 17 s).

### 5.3 The chamber — one claim, one job

The queue runs as a new lane set, `integration`, dispatched through the
existing `lane-run.sh` so it takes the **same shared claim** as the census and
the heavy tier (decisions 0081/0086/0133). One job, not six: the queue wait is
paid once.

It runs in its **own dedicated worktree**, not the lane's shared scratch tree.
Two reasons, both load-bearing: the lane's tree is `reset --hard`ed by every
dispatch, which is what destroys the sub-floor roster copy-out today; and a
dedicated tree always builds `main`-plus-a-delta, so its warm `target/` stays
hot and the 771 s cold build is paid once, ever, instead of once per campaign
the way `worktree-take` pays it.

Sequence inside the claim:

1. Fetch; check out the candidate branch; `git merge origin/main`.
2. On conflict → **hold** and escalate (§6). The queue does not guess.
3. Run the phases (§5.4), `git clean -fd` between each.
4. After each authoring phase, commit any artifact drift onto the branch as a
   single `chore(artifacts)` commit.
5. Assert `git status --porcelain` is empty and the declared drift check is
   clean: `git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt |
   grep -v '^$')`.
6. Fast-forward `main` to the branch tip; push `origin main`; push the branch.
7. Record the outcome; post a `notice` to the board; release the claim.

### 5.4 Phase order

```
  artifacts -> outboard -> gate -> seam-guard -> clients -> heavy
     254s       186s       554s      491s        439s      1678s
```

Mean execution per set, measured from `jobs.tsv` (wall minus queue wait);
~3602 s total, about one hour.

Ordered by **expected time-to-red**, not by tree hygiene. `git clean -fd`
between phases makes cleanliness free — never `-fdx`, because `target/` is
gitignored and `-x` destroys a 15 GB warm cache, a note `lane-run.sh` already
carries. Freed from the hygiene constraint, the order optimises for detecting
the class that is *distinctive to a merge product*: interaction with `main`,
which only `gate` and `artifacts` see. `seam-guard`, `clients` and `outboard`
detect campaign-local defects the branch's own stage gate already had a chance
to catch. `heavy` is last because at 1678 s it is 47% of the set.

Cleaning between phases also closes by construction the bug The Ballast half-
fixed: `seam-guard` returned `rc=2` "refusing to run on a dirty working tree"
in five of its first six lane runs, because earlier sets in the same campaign
dispatch dirtied the shared tree. Its `rc=2` reads as "found survivors", so the
breakage looked like a finding for a month.

### 5.5 The queue authors the artifacts

The queue is the only actor that knows the final merged tree, so it is the only
correct place to regenerate committed artifacts. This collapses a diamond that
has cost this project repeatedly: *tests green* and *artifacts regenerated*
have two common children — green-with-stale-artifacts and
green-with-fresh-artifacts — and nothing today orders them. Hence the standing
folklore about regenerating "after the LAST absorb", and the recurring
inherited-artifact debt that surfaces at someone else's close and gets
attributed to the wrong campaign.

Decision 0079 is satisfied rather than strained: the queue runs on lefford, the
enforced canonical host, so goldens are authored where they must be.

## 6. Failure handling

**Policy: hold and fix** (Nathan's call, taken with the cost stated). The queue
stops on red; the operator diagnoses; the campaign behind it waits.

The accepted cost is head-of-line blocking: today a broken campaign blocks only
itself. At 3.5 merges/day against a 24-hour budget there is room for an hour of
debugging and not for four. Three mitigations, none of which is "evict by
default":

1. **The hold is loud.** `make sluice-status` reports what is held, since when,
   which phase failed, and the log path. A `notice` with `polarity=hold-off`
   goes to the board, because a held queue means `main` is frozen for everyone.
2. **Triage is classified, not vibed.** Four classes, and the evidence that
   distinguishes them: *campaign-local defect* (the branch's own stage gate
   would also be red — re-run it), *interaction with main* (branch green,
   merge product red — the class this campaign exists to catch), *stale
   artifact* (drift check red, tests green), *infrastructure or contention*
   (`cpu_ratio` in `jobs.tsv` — the diagnostic CLAUDE.md already names as the
   one that separates contention from a real regression).
3. **Manual eviction stays available** as an explicit operator action, never a
   default.

**The operator does not become the crew.** A fix belongs on the campaign
branch, and the topology already puts it there: the queue merges `main` *into*
the branch, so anything committed in the chamber lands on the branch and
survives. Where the defect needs the author's knowledge, the request is bounced
back with the evidence rather than fixed in the chamber.

### 6.4 Out-of-band landings

The inductive guarantee breaks the moment something lands on `main` outside the
queue. Detection is exact and costs nothing: the queue records the SHA it
pushed; at the mouth of the next request, `origin/main` must equal that SHA. If
it does not, something landed out of band and the queue says so loudly instead
of quietly resuming with a weaker guarantee than it advertises.

## 7. The census

Not in the automatic path. It stays a separately authorized dispatch, for two
reasons: `campaign-autopilot`'s carve-out requires explicit authorization for a
census regen, and the cost has historically spanned 20× (882 s to 19,207 s).

This is the one place the queue does **not** close the artifact-freshness
diamond, and the spec says so rather than implying otherwise: a merge can land
with a census that predates it.

**The 15-minute tripwire.** Nathan's rule — if a census exceeds ~15 minutes,
profile it rather than budget for it — becomes a check with a fixed ceiling of
**900 s** against the most recent `| census |` row in `docs/timings.md`,
printing the last five rows so the margin is visible. Read from the ledger, not
from prose:

```
2026-08-13T19:01:49Z  census   949.579   cpu_ratio 28.56
2026-08-14T00:56:09Z  census   920.212   cpu_ratio 29.43
2026-08-14T15:36:53Z  census   882.487   cpu_ratio 32.10   <- latest, 14.7 min
```

Green today, and **thin**: two of the last three runs would have tripped it.
That is the point — it is a live tripwire, not a formality. A ratchet against
recent best was considered and rejected; it would arm at 882.487 s and fire on
ordinary variance, and this repo's own rule is that a check which is always red
is ignored as fast as one that is always green.

## 8. What retires

`gate-campaign` becomes a **refusing signpost**: it prints the replacement and
exits non-zero. This is decision 0132's own established pattern — `gate`, `ci`,
`gate-fast` and `gate-full` were each retired this way rather than aliased,
because silently repointing a target changes what hundreds of existing calls
meant. The same argument applies here and the same mechanism is reused.

`gate-stage`, `gate-commit`, `make lane SET=… REF=…`, `preflight` and the
census dispatch are all unchanged.

## 9. Prerequisites and risks

**P1 — lefford cannot push today. Hard blocker.** Verified:
`git config --get-all credential.helper` is empty, `origin` is HTTPS, and
`ssh -T git@github.com` returns `Permission denied (publickey)`. A push would
prompt for a password and hang in a non-interactive session. `gh auth status`
shows an authenticated `ndouglas` token carrying `repo` scope, so the
capability exists and is merely unwired: `gh auth setup-git` is the fix. It is
a task in the plan, not an assumption, and it changes a machine's git config —
so it is called out here for G3 rather than done quietly.

**P2 — the heavy tier's state on `main` is unverified.** A board post from The
Repose reports the heavy tier red on `main` with one of two failures
attributable to contention; a later lane job (`bfd21abc`, 2026-08-15T22:23:56)
returned `rc=0` for `heavy`. These are not reconcilable from the ledger alone.
The queue cannot bootstrap against a red `main`, so establishing a green
baseline is task one, and if `heavy` proves flaky rather than red the campaign
must decide whether it gates or merely reports — that decision is **not** taken
in this spec.

**R1 — a held queue freezes everyone.** Accepted, mitigated per §6.

**R2 — the integration set is ~1 h, so a late-phase failure is expensive.**
Mitigated by the §5.4 ordering; not eliminated.

**R3 — flakiness meets a blocking policy.** 43% of the lane's first 46 jobs
returned non-zero. If even a fraction of that is flake rather than signal, a
hold-and-fix queue stalls often. §6.2's classification is the instrument; if
the flake rate proves high, the honest response is to fix the flakes, not to
weaken the queue.

## 10. Testing

`scripts/test-sluice.sh`, in the shape of `scripts/test-lane.sh` (which pins
flock's ordering property rather than merely asserting a lock file exists).
Properties to pin, each of which must be shown to fail when mutated:

- FIFO order, and per-branch coalescing by ancestry.
- A conflicting candidate is refused **without acquiring the claim** — assert
  on the claim, not on the error message.
- The push is fast-forward only. A force-push must be impossible, not merely
  absent.
- An out-of-band landing is detected (§6.4).
- Tested SHA == pushed SHA.
- The census tripwire fires above 900 s and not below.

Two rules from this repo's own hard-won practice apply. A mutation test must
**prove it mutated** — assert the target text exists before substituting it, or
a no-op mutation manufactures a green that looks like robustness. And a RED
from a compile error proves nothing about an assertion.

## 11. Decision to ratify

**0137 — `main` advances only through the lock.** That every commit on `main`
is the tip of a tree gated as itself; that the merge product, not the branch
tip, is the gated object; and that an out-of-band landing is a detected fault
rather than a silent weakening.

## 12. Open, flagged for G3

1. **Merge topology.** Adopted: merge `main` into the branch, fast-forward.
   The discarded alternative (`--no-ff` onto `main`) yields one first-parent
   entry per campaign — 104/month instead of the measured 993 — and history
   topology is the least reversible choice here.
2. **P1**, wiring push credentials on lefford (§9).
3. **P2**, the heavy tier's baseline, and whether it gates or reports.
4. The census remains outside the guarantee (§7).
