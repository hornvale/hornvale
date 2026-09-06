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

---

## Task 1 — complete

**What shipped.** `scripts/gnomon-injection.sh` gained a `check` subcommand
(no arguments; runs the host guard and the tree guard, builds and mutates
nothing, exits 0/1/2) and its tree guard was narrowed from "the whole tree
outside the fixture directory must be clean" to "the tree must be clean
everywhere a build or the source mutation can see" — `git status --porcelain
-- . ":!$FIXTURES" ":!book" ":!docs"`, excluding `book/` and `docs/` in
addition to the pre-existing fixture-directory exclusion. This is spec §3.3,
task 1 of decision 0836's plan: the delivery (`scripts/sluice-census.sh`,
later tasks) stages its regenerated census goldens under `book/` and needs to
re-author the Gnomon injection arms afterward without the old guard reading
its own staged dirt as a reason to refuse.

**The test's arms** (`scripts/test-gnomon-injection.sh`, 10 assertions, run
against a scratch repo carrying only the script and its two sourced files —
never the real repo):
- the host guard, exercised for real (passes clean on the canonical box
  without `HV_GNOMON_PILOT`; refuses and names the host off it);
- clean tree: `check` passes;
- a MODIFIED `book/` file is allowed;
- an UNTRACKED `book/` file is allowed;
- STAGED `book/` and `docs/` dirt together is allowed;
- dirt inside the fixture directory is allowed;
- CONTROL: a modified tracked source file (`domains/terrain/src/strata.rs`)
  still refuses and is named;
- CONTROL: a staged new file outside `book/`/`docs/` (`scripts/new.sh`) still
  refuses and is named;
- `check` with extra arguments is a usage error (rc=2);
- anti-vacuity: `check` never authors a manifest or builds a `target/`.

The two CONTROL arms are load-bearing in the same sense the campaign's own
ideonomy note (spec, and ledger entry #1's enrichment 1) makes explicit:
without them every "book/docs dirt is allowed" arm above would pass equally
well for a guard that had been deleted outright rather than narrowed.

**A ruling made while implementing.** The brief's `check` wired the host
guard's refusal path through `HV_GNOMON_PILOT` exactly as written, and this
session is running off the canonical box (`lefford`), so the test's
"off-canonical-box" branch is the one actually exercised here — confirmed by
running the RED test first (see task-1-report.md) and observing the host
guard's real, unmodified refusal text before any script change landed. No
change to the brief's shape was needed; recorded because the controller's
context block called this out as the expected reading and it is worth
confirming it held rather than assuming it did.

**Ideonomy.** None run for this task — the brief specifies the guard
predicate, the subcommand's exit codes, and the test text verbatim, so there
was no design fork for this task to explore; the design work (the rule for
what a delivery may satisfy vs. defer, and the trigger for re-authoring) was
already done in ledger entry #1's ideonomy pass for the campaign as a whole.

**Files changed:** `scripts/gnomon-injection.sh` (USAGE block, guard-rationale
paragraph, guards section), `scripts/test-gnomon-injection.sh` (new),
`scripts/lane-outboard.sh` (registered the test in the `outboard` set), this
ledger.

**Concerns.** `make shellcheck` reports one PRE-EXISTING failure unrelated to
this task: `scripts/sluice-census.sh:216`'s `release_census_row` trips
SC2329 ("function is never invoked") under the shellcheck version installed
in this worktree, even though it carries a `# shellcheck disable=SC2317`
directive immediately above it (SC2317 and SC2329 are different checks for
the same underlying shape, and this shellcheck version added SC2329 after
that directive was written). Verified pre-existing by stashing this task's
changes and re-running `make shellcheck` against unmodified HEAD: the same
single failure appears, byte-identical. Not touched by this task; flagged
for a later task or a separate fix, since `scripts/sluice-census.sh` is
outside this task's file list.

---

## Task 2 — complete

**What shipped.** Two library functions under `scripts/sluice-census.sh`'s
existing `HV_CENSUS_LIB=1` seam, inserted after `census_golden_count`'s
closing `}` and before the `HV_CENSUS_LIB` early return: `census_schema_columns
<schema.json>` (the sorted column names of a lab `schema.json`, printing
nothing for a missing file) and `injection_arms_stale <worktree>` (one line
per Gnomon injection arm whose column set differs from the census's,
following spec §3.2 step 1's second trigger from ledger entry #1's enrichment
4 — a census that moved goldens re-authors the arms, but so does a census
whose columns drifted from an earlier epoch with no golden movement this
run). `injection_arms_stale` always exits 0; its stdout is what Task 3
reads.

**The one judgement call: the indent rule.** A lab `schema.json` is serde's
pretty print, and the function distinguishes a column's `"name"` from the
study's own `"name"` (which sits at indent 2, one level shallower) purely by
indent — `^ {4,}"name": "`, indent >= 4. This is a structural assumption
about serde's pretty-printer output rather than a JSON parse, and it is safe
in exactly one direction: **a mis-parse here can only cause a NEEDLESS
re-authoring, never a missed one.** If the indent rule ever miscounts a real
column as the study name (or vice versa), the two sides of the comparison
(census vs. arm) both go through the same function, so a spurious extra or
missing name shows up as a spurious diff and the arm is marked stale when it
was not — costing an unneeded re-author, not skipping a needed one. The rule
was checked against the two real files it will run on before trusting it:
the committed census and `baseline-a`'s fixture each report 285 `"name"`
keys total and 284 at indent >= 4, the one exception in each being the
study's own name line — confirming the split holds on real data, not just
the brief's synthetic fixtures.

**TDD evidence.** RED: `bash scripts/test-sluice-census.sh` before the
functions existed — `FAIL: HV_CENSUS_LIB=1 did not expose
injection_arms_stale`, three `command not found` errors from the shell
sourcing a function that did not exist yet, `11 passed, 4 failed`, exit 1 (see
task-2-report.md for the full transcript). GREEN: the same command after
implementation — `15 passed, 0 failed`, exit 0.

**Ideonomy: none, no fork.** The brief specifies both functions' exact code,
the test text verbatim, and the insertion point; there was no design
question left open for this task to explore — the design work (the rule for
when to re-author, stated as "any census golden moved, OR the column set
differs") was already made in ledger entry #1's ideonomy pass for the
campaign as a whole.

**Files changed:** `scripts/sluice-census.sh` (the two functions),
`scripts/test-sluice-census.sh` (the new test arms),
`docs/decisions/0836-a-census-delivery-regenerates-the-evidence-its-gate-reads.md`
(new — see ruling below), `docs/digest/decisions-in-force.md`
(regenerated), this ledger.

**Ruling: decision 0836 was minted here, in Task 2, not in Task 5 as the
plan's Global Constraints originally staged it.** The brief's Step 3 code
comment cites `decision 0836` verbatim, but the plan's own constraint ("A
Rust comment may not cite a decision number until the record exists … Task 5
mints 0836 before any comment cites it") assumed the citing gate only reads
`.rs` files. It does not: `cli/tests/suite/docs_consistency.rs::
decision_cites_in_sources_resolve` scans `scripts/` among its source
directories, and `pre-commit` runs that suite (via `make docs-tests`) on
THIS commit, because a commit touching only `scripts/` and docs is not
Rust-relevant and takes the fast path — so the cite is checked at Task 2's
own commit, not deferred to whenever a `.rs` file next cites it. Session
flagged this as a blocker (NEEDS_CONTEXT); the controller's ruling was to
mint 0836 now rather than strip the citation, using Task 5's exact record
text (`docs/decisions/0836-a-census-delivery-regenerates-the-evidence-its-gate-reads.md`),
confirming the four `Relates:` targets (0079/0133/0139/0514) exist first,
regenerating `docs/digest/decisions-in-force.md` (moved by exactly one
entry) and `docs/digest/intent-vs-reality.md` (unmoved, as expected — the
registry row does not flip until Task 6), and re-running
`decision_cites_in_sources_resolve` (and the rest of `docs_consistency`)
green before committing. Task 5 will need to be read against this when its
turn comes: minting is already done, so Task 5's remaining work is Step 2
onward (the Rust comments in `anomaly.rs`/`anomaly_injection.rs` and the
prose sweep) rather than Step 1.

**Concerns.** `make shellcheck` reports the same single PRE-EXISTING failure
Task 1 flagged and left untouched: `release_census_row`'s SC2329 finding,
now at `scripts/sluice-census.sh:256` (shifted from line 216 by this task's
38 inserted lines landing earlier in the file; confirmed the same function,
same finding, by diffing against `HEAD`). Not touched here, per the
controller's resolution — Task 3 owns it.
