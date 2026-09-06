# The Spillway — decision ledger

Campaign: `campaign/the-spillway` · Spec:
`docs/superpowers/specs/2026-09-06-the-spillway-design.md` · Decision block:
**0836–0845** (reserved under `the-spillway`, the name the branch submits from;
The Warp's unused 0780–0785 belongs to The Warp and is not touched).

Registry row: `TOOL-census-delivery-and-injection-fixtures-deadlock` (raw, high;
filed by The Warp, ledger #12). The Warp's census at `4a419e996ef7` grew the
registry by 32 columns and its delivery commit was refused by the commit gate
twice; the goldens landed only through an ungated intermediate object and a
by-hand re-authoring on the canonical box. This campaign makes a delivery that
grows the registry deliver on its own.

---

### #0 [Q, session] — which of The Warp's open items this session takes

**Question.** The handoff named three candidate tasks: brainstorm the
successor for `SURF-a-knowledgeable-body-reads-the-warp`; profile the census
(item 1); fix the census-delivery deadlock (item 2).

**Decision.** Item 2. Item 1 is owned by Nathan by the handoff's own words.
The successor brainstorm is a design campaign whose G3 needs Nathan's taste on
a world-facing verb; the deadlock is a bounded tooling defect with a written
record, and it blocks every future census refresh that registers a metric —
which is most world-touching campaigns' closes.

**Ideonomy passes / overturns:** none — a session-scoping call, not a design
choice; recorded so the choice is visible.

**Capture:** this entry. The visual companion was NOT set up this session: no
visual question arose (the subject is a shell delivery path and two gate
tests), and the session runs unattended under autopilot with no one to look
at a browser tab. Recorded as an omission with its reason rather than left
silent.

---

### #1 [G1] — the delivery re-authors the evidence its own gate reads

**Question.** Three ways to break the deadlock were on the table. (A) The
delivery (`scripts/sluice-census.sh`) re-authors the eight Gnomon arms itself
before it commits, with `scripts/gnomon-injection.sh`'s clean-tree guard
narrowed to the paths that can reach a build, and the column-COUNT witness
(`domesday::anomaly::tests::evaluable_columns_measured_surface_on_the_<N>_column_census`)
stood down for the delivery commit only. (B) Stand every census-shaped
witness down under the existing `HV_CENSUS_DELIVERY` opt-out and leave the
re-authoring to the owning campaign by hand. (C) Make the witnesses read a
delivery in progress — compare the fixtures against the STAGED census.

**Decision.** A, with four enrichments from the ideonomy pass (below).

**Why (precedent cited).** The registry row's own "Fix:" clause names A
first. `HV_CENSUS_DELIVERY` already exists as a scoped, named opt-out of ONE
check per commit (the golden-pins guard, then the yellow-census alarm — both
in `scripts/hooks/pre-commit`), and the rule that licenses each is the same:
the check demands something that CANNOT exist before the run finishes. That
rule separates the three legs cleanly. The fixture-columns test
(`anomaly_injection::the_fixture_columns_match_the_census`) demands
regenerated evidence, which a machine on the canonical box can produce in the
same sitting — so the delivery must SATISFY it, not defer it. The count
witness demands a human re-measurement with narrative ("DO NOT SIMPLY UPDATE
THESE INTEGERS"; its NAME carries the count and the sub-floor roster selects
it by exact name) — so it can only be DEFERRED, and the merge of the delivery
branch re-demands it exactly as it re-demands the calibration pins today.
Decision 0139 (main advances only through the chamber) and the delivery's
own header (never push main; the branch is gated as an ordinary merge) are
what make deferral safe: nothing stood down here can reach main un-gated.

B was rejected because it reproduces The Warp's by-hand labour on every
growing census — an ssh, a checkout of the delivery branch on lefford, an
eight-minute script, a commit — the exact sequence ledger #12/#13 recorded as
the cost of the deadlock. C was rejected because the count witness's purpose
is a human's re-measurement; a witness that reads its own answer from the
staged census is a tautology (memory: a guard the type system guarantees is
not a guard), and the fixture-columns test already reads the committed census,
which on the delivery commit IS the staged one — C changes nothing for it.

**Alternatives discarded within A.** (i) Re-author unconditionally on every
delivery — rejected: ~8 minutes of the box per null census for evidence that
did not change. (ii) Author in a SECOND clean worktree at the ref so the guard
stays untouched — rejected: a worktree lifecycle inside the delivery and
either a cold build or a shared `CARGO_TARGET_DIR`, for a guard whose stated
rationale (mutated source restored by `git checkout --`; the manifest's `sha`
is a claim about SOURCE) is met exactly by narrowing it. (iii) Have the
delivery re-pin the count witness mechanically (rename the test, rewrite the
roster line) — rejected: it satisfies the assertion while destroying what the
assertion is for; the witness's message forbids it in capitals.

**Ideonomy passes / overturns.** One picked tuple (negation × scale ×
connectivity, direction), then implication-mining in prose. No overturn.
Four enrichments adopted, each now in the spec:

1. *Scale — how much a delivery commit may defer.* Zero (the deadlock) … the
   three checks it now stands down … everything (`--no-verify`, forbidden).
   The line between them is a RULE, not a list: **satisfy every check whose
   remedy is a regeneration; defer only a check whose remedy is a human
   re-statement.** Written into the decision record so the next census-shaped
   witness is placed by the rule rather than by precedent-matching.
2. *Direction — an unrecorded cost accumulates.* The re-authoring is "an
   eight-minute job" by the retrospective's word and by nothing else; The
   Governor's Task 7 exists because 27 heavy runs wrote rows nothing
   committed. The delivery runs it under `scripts/timed.sh` with its own
   label, so its cost lands in `docs/timings.md` beside the census's.
3. *Connectivity — the lock.* `census-run.sh` releases the shared flock when
   it exits, so the re-authoring would otherwise run on 40 cores unclaimed —
   ledger #13 accepted exactly that once, as a recorded exception. The
   delivery takes the same flock around the re-authoring instead. It writes
   no claim file (the eight-field format belongs to `census_claim.rs` and a
   second writer is the drift `lane_sets.rs` exists to fail on), so for those
   minutes `census-run.sh status` reports nothing while flock-takers wait —
   stated in the spec as the accepted gap.
4. *Negation of the trigger.* "Re-author when the column set differs" was the
   first draft. Negating it: the arms are evidence about the WORLD at a SHA,
   and every canonical reading of the recall witness (eight so far) was taken
   on arms re-authored at that census epoch. A census whose VALUES moved with
   no new column would leave the arms describing an older world. The trigger
   is therefore **any census golden moved, OR the column set differs** — the
   second arm catches arms left stale by an earlier epoch on a census that
   itself moved nothing.

**Capture actions.** This entry; the spec; decision 0836 (to be minted at
execution). The "fully connected" extreme — the delivery regenerating EVERY
fixture frozen against the registry — is one family today (verified:
`find windows/lab/tests/fixtures -maxdepth 1` shows `injection/` as the only
directory holding `rows.csv`/`schema.json` pairs), so no registry of such
fixtures is built; the spec names the rule and the single member.

---

### #2 [Q] — where the count-witness exclusion lives

**Question.** The delivery commit's gate runs the sub-floor roster through
`scripts/subfloor-roster.sh` → `scripts/subfloor-run-chunked.sh`. The chunker
splits the filter on a literal ` | ` and its header states it is lossless ONLY
because the roster is flat ("a future roster shape with real nesting … would
need a smarter splitter"). Wrapping the filter as `(roster) and not test(…)`
would break it.

**Decision.** The exclusion REMOVES terms from the flat expression in
`subfloor-roster.sh` (an awk skip on the test path) rather than wrapping it,
under `HV_SUBFLOOR_EXCLUDE`, mirroring `HV_DOCS_TESTS_EXCLUDE` on the
`docs-tests` target (Makefile, "narrows this roster for ONE caller only").
`pre-commit` sets it only inside its `HV_CENSUS_DELIVERY` block. A pattern
that matches no roster line is a silent no-op, and that is the SAFE
direction: the witness then runs, reds, and the delivery is refused loudly —
never a silent green.

**Why (precedent cited).** `HV_DOCS_TESTS_EXCLUDE` (The Nettle fix wave,
`48aa9373b`) is the same shape for the same caller;
`scripts/test-census-guard.sh` already executes the hook under
`HV_CENSUS_DELIVERY=1` and asserts the stand-down line AND a control without
it — the new test copies that pair.

**Alternatives discarded.** Reading the env var inside the Rust test (worse:
any caller can set it, and the roster script is at least the gate's own
text). A nextest `--skip`-style flag (nextest's `-E` is the only selector
the recipe uses; adding a second mechanism doubles the surface).

**Ideonomy passes / overturns:** one (implication-mining on the chunker's
flatness invariant), which is what turned "wrap the filter" into "remove the
term" — an overturn of the first draft.

**Capture:** this entry; spec §3.3.

---

### #3 [Q] — the delivery locks the box with `flock`, which the Mac lacks

**Question.** `flock(1)` is `/usr/bin/flock` on lefford and absent on this
Mac (`which flock` → nothing). `sluice-census.sh` runs only on lefford in
production (`census-run.sh`'s host guard fails first anywhere else), but
`scripts/test-sluice.sh`'s census block drives it on whatever host runs the
tests, with the census stubbed.

**Decision.** The lock step requires `flock` and FAILS CLOSED without it
(exit 4, goldens left staged, nothing pushed). **Corrected in the same
sitting, before any code:** the first draft of this entry had the step
degrade to unlocked with a printed line, on the belief that the test harness
runs on the Mac. It does not — `scripts/test-sluice.sh`'s first guard is
`command -v flock || exit 0 (SKIP)`, read after the draft was written. With
no Mac caller to protect, degrading would have been a second locking story
for a host that never delivers; refusing costs nothing. The positive control
for the lock is therefore unconditional.

**Why (precedent cited).** In production the step is unreachable without
`flock`: `census-run.sh` took the same flock on the same host minutes
earlier, so its absence would have failed the census before a golden moved.
`test-sluice.sh`'s own SKIP guard is the precedent that the census path is
canonical-box-only end to end.

**Alternatives discarded.** Degrade to unlocked with a warning (the first
draft; unnecessary once the harness's guard was read). A second lock
primitive for macOS (no caller). An env-driven bypass (a guard a variable
can satisfy).

**Ideonomy passes / overturns:** one (inversion: "what if the lock is
absent?" → "then the census could not have run") — no overturn of the
unreachability argument; the CONSEQUENCE drawn from it was overturned by
reading the harness, from "degrade" to "refuse".

**Capture:** this entry; spec §3.2 step 3, §4.2.

---

## Follow-ups

- Refs that predate this campaign's merge carry the OLD `gnomon-injection.sh`,
  whose whole-tree guard refuses the delivery's dirt. A census of such a ref
  that moved goldens will fail at the re-authoring step and report it;
  recovery is The Warp's by-hand path. Recorded in the spec (§5); no row —
  it expires as branches absorb main.
- `windows/lab/src/domesday/anomaly.rs` line ~867 says of the count witness
  "this test is not in the sub-floor tier a local gate runs"; the committed
  roster (`docs/timings/subfloor-roster.tsv:1273`) selects it, and The Warp's
  delivery log shows it failing INSIDE `make gate-commit`. Stale comment;
  corrected by this campaign's re-statement of that witness's comment.
