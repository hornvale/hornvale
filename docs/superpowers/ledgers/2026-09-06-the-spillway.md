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
  roster (`docs/timings/subfloor-roster.tsv:1279`) selects it, and The Warp's
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
the brief's synthetic fixtures. [Corrected at close — this measurement is
impossible: at indent >= 4 both depths match, so the count is 285; see the
Fix wave section. Left in place because the ledger is append-only.]

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

---

## Task 3 — complete

**What shipped.** `scripts/sluice-census.sh`'s delivery flow now re-authors
the Gnomon injection arms itself, between staging the census's own diff and
the `HV_CENSUS_DELIVERY=1` commit. Trigger: `n_goldens -gt 0` (the world
moved) OR `injection_arms_stale "$wt"` is non-empty (an arm's column set has
drifted, even on a null census). On trigger it runs the REF'S OWN
`scripts/gnomon-injection.sh check` first (a fast, lock-free refusal for a
ref that predates The Spillway and would refuse the delivery's own staged
goldens as dirt — spec §3.2 step 2), then takes `HV_CENSUS_LOCK`
(`flock -w "${HV_CENSUS_WAIT_TIMEOUT:-2700}"`, default `/tmp/hv-census.lock`,
the same claim every expensive job takes), runs the real authoring under
`scripts/timed.sh gnomon-injection --`, stages the re-authored arm files with
the goldens, and releases the lock before the commit. Every arms failure
(missing script, refused check, lock timeout, authoring failure) exits 4 —
the same code `COMMIT REFUSED` already used — leaving the goldens staged in
`$wt` for by-hand recovery (The Warp, ledger #12) and pushing nothing. The
arms' outcome (`Gnomon arms re-authored at <ref12> (<n> arms).` or `Gnomon
arms unchanged (census moved nothing; columns match).`) is echoed to the log,
appended as its own line in the delivery commit message, and echoed again in
the final DELIVERED report — three separate call sites per the brief's Step
3, not one shared helper. Folded in alongside the arms wiring: `SC2329` added
to the `# shellcheck disable=SC2317` directive above `release_census_row`
(Task 1's flagged concern, now closed), and `LC_ALL=C` pinned on
`census_schema_columns`'s `sort` (paired `comm` needs matching collation on
both sides).

**Corrections applied, both from the controller's brief review.**

1. **Arm (c)'s reset target.** The brief's own text used
   `reset --hard HEAD~1`, which lands one commit short once the delivery's own
   commit sits on top of the "stale arm" commit. Fixed by recording
   `pre_c="$(g -C "$cen_wt" rev-parse HEAD)"` *before* the stale-arm commit and
   resetting to that captured sha afterward, rather than counting commits.

2. **Arms (d) and (e) never entered the arms path as literally written**, and
   this was caught by running the RED test and finding these two returned
   `rc=0`/`rc=3` — wrong exit codes but crucially *not* the failure text the
   brief predicted, which is what sent this to investigation rather than
   straight to "not implemented yet". Root cause: after the first `moves`
   delivery the worktree golden already reads `42,2`, and `write_stub moves`
   always writes the same literal `42,2` — a genuine no-op the second time — so
   `n_goldens` read 0 and, with the arms otherwise matching, the delivery
   skipped them entirely, running past the two failure-path assertions on an
   empty diff. Fixed exactly as directed: before arm (d), the golden is written
   back to `42,1` and committed in `$cen_wt`, so `moves` moves it again; arm
   (e) inherits that same `42,1` HEAD from (d)'s bare `reset --hard`, so
   `moves` (still baked from (d)'s `write_stub`) moves it there too. Both
   arms' `if` conditions gained the brief-specified additional assertion —
   `grep -q 're-authoring the Gnomon injection arms'` — as the positive proof
   the arms path was actually entered, not merely that *some* rc=4 fired.

**A third, undirected fix, found the same way (RED, then investigate rather
than assume "not implemented").** Even after correction 2, the FIRST arm
("arms rode along" — the main `moves` test) still failed all five of its own
assertions on the first green attempt, with the log reading `NO GOLDENS
MOVED` despite the golden file genuinely differing. Traced to
`census_golden_count`: it reads `docs/generated-paths.txt` off the worktree,
falling back to `HV_SLUICE_REPO_ROOT`'s copy — and this test's `$cen` fixture
had never declared that file at all, for any test in this section, at any
point before this campaign. `sluice_path_author` (`scripts/sluice-phases.sh`)
tolerates the missing file by returning an empty author rather than failing
loudly, so `census_golden_count` has silently read 0 for every `moves` run
this file has ever driven — invisible until now because nothing before The
Spillway asserted the *count*, only the delivered branch's eventual content
(which stages unconditionally on `git add -A`/`add -u`, independent of
`n_goldens`). The arms' own trigger (`n_goldens -gt 0`) is the first thing in
this file's history to actually depend on the count being right. Fixed by
adding a `docs/generated-paths.txt` to the `$cen` fixture, mirroring the real
committed file's two rows for this exact path (`.../the-census/schema.json
artifacts` then `.../the-census/ census`, exact-row-beats-directory) — purely
additive to the fixture, so no pre-existing assertion in this file changed
behaviour; confirmed by the full-suite run reporting the same pass count for
every pre-existing arm before and after.

**Plan-shape change not directed by either correction.** The brief's Step 1
text for `c_log`/`af_log`/`ar_log` reads
`ls -t "$tmp/cen-state"/census-*.log | head -1`, which `make shellcheck`
flags (SC2012). The file already carries a house idiom for the identical
job two lines above (`t_log`/`still_log`: a `for _f in glob; do [ -e "$_f" ]
&& x="$_f"; done` loop, picking the lexicographically-last match, which is
chronological given the `%Y%m%dT%H%M%SZ` suffix) — used it for consistency
and a clean `make shellcheck` rather than a new directive.

**TDD evidence.** RED, on lefford (`ssh lefford … bash
scripts/test-sluice.sh`), against `scripts/test-sluice.sh` with its new arms
but `scripts/sluice-census.sh` still at `HEAD` (pre-Task-3): `254 passed, 9
failed`, all 9 failures exactly the new arms-related assertions (`a moving
census did NOT run gnomon-injection.sh`, `the arms were authored with the
lock FREE`, `the delivered branch does not carry the re-authored arm`, `the
commit message does not name the arms`, `no gnomon-injection row …`, `null
census: ran=no, log lacks 'Gnomon arms unchanged'`, `stale-arm census: rc=0
ran=no …`, `failed authoring: rc=3 …`, `refused check: rc=0 …`); every
pre-existing assertion green. GREEN, same host, same command, after the
implementation and both fixture fixes above: `263 passed, 0 failed`, wall
27.844s (`real 0m27.844s user 0m6.768s sys 0m17.032s`, measured with `time`).
All 18 census-block assertions read `ok`, including the five new "arms rode
along" checks, the null/stale-arm pair, and the failed-authoring/refused-check
pair. Confirmed again from the pushed commit (below) with a final run.
`bash scripts/test-sluice-census.sh` (15 passed, 0 failed) and `bash
scripts/test-gnomon-injection.sh` (10 passed, 0 failed) both stayed green
throughout — unaffected by this task's changes, run locally on the Mac since
neither needs `flock`. `make shellcheck` is clean (no output, exit 0) —
Task 1's flagged SC2329 concern on `release_census_row` is now closed by the
directive update above.

**Ideonomy: none.** The brief specifies the arms block's exact code, the
header prose, the commit-message line, the report line, and the test text
verbatim; the two corrections and the fixture fix above are bug fixes
surfaced by actually running RED and reading what failed, not design
choices — there was no open design question for this task to explore.

**Files changed:** `scripts/sluice-census.sh` (header prose, the arms block,
the commit-message line, the report line, the two folded minors),
`scripts/test-sluice.sh` (the census-block stub/arms per the brief plus the
`docs/generated-paths.txt` fixture addition and the `ls -t` → `for` loop
change), this ledger.

**Concerns.** None outstanding. The controller's Step 6 (stage gate) is
explicitly out of scope for this task per the dispatch brief and is left for
the controller to submit after review.

## Task 4 — complete

**What shipped.** The census's column-count witness
(`domesday::anomaly::tests::evaluable_columns_measured_surface_on_the_<N>_column_census`)
now stands down for a delivery commit, the third scoped opt-out under
`HV_CENSUS_DELIVERY`. `scripts/subfloor-roster.sh`'s final `awk` line gained
an `HV_SUBFLOOR_EXCLUDE` ERE parameter: a roster line whose test path (the
part after `$`) matches is OMITTED from the flat filterset rather than
wrapped in `and not (...)`, because `scripts/subfloor-run-chunked.sh` splits
the filterset on a literal `' | '` and is lossless only while it stays flat.
Empty or unset leaves the output byte-identical, which is the default for
every caller but one. `scripts/hooks/pre-commit`'s final block now sets
`subfloor_exclude='evaluable_columns_measured_surface_on_the_[0-9]+_column_census'`
only inside its `HV_CENSUS_DELIVERY` branch and passes it through as
`HV_SUBFLOOR_EXCLUDE` to `make gate-commit`. No machine may re-pin the
witness — its name carries a count that does not exist in the tree until
the census does, so the obligation is DEFERRED, not discharged: the merge of
the delivery branch runs the full suite and demands the re-pin.

**TDD evidence.** RED, `bash scripts/test-census-guard.sh` against the
appended tests with neither script change made: `17 passed, 4 failed` —
`could not read subfloor_exclude from scripts/hooks/pre-commit`, `the
exclusion omitted 0 term(s), want 1`, `with the escape the witness is STILL
selected`, `HV_CENSUS_DELIVERY stand-down branches: 2, want 3`; all other
(pre-existing plus the two controls) arms green, exactly as the brief's Step
2 predicted. GREEN, same command, after implementing the roster exclusion
and the hook block: `21 passed, 0 failed` — `the exclusion omits EXACTLY one
roster term (4093 -> 4092)`, `with the escape the witness is not selected`,
`an exclusion matching nothing leaves the filterset byte-identical`, `the
excluded filterset is still FLAT`, `HV_CENSUS_DELIVERY stands down exactly
three checks`.

**Shellcheck.** One deviation from the brief's verbatim test text: the
appended `grep -cF 'if [ -n "${HV_CENSUS_DELIVERY:-}" ]' ...` line trips
SC2016 (expressions don't expand in single quotes) even though the quoting
is deliberate — it is a literal `grep -F` pattern, not a shell expansion.
Added a `# shellcheck disable=SC2016` comment above it, matching the
project's existing convention for the identical situation
(`scripts/test-pre-push.sh:351,428`, `scripts/test-sluice.sh` in several
places). `make shellcheck`: clean, exit 0.

**Byte-identity control.** Per the controller's resolution, a bare `git
stash` was refused by this repo's shared-stash-stack guard (its message
recommends a scoped `git stash push -u -m '<tag>' -- <path>` plus `apply
<sha>` rather than `pop`, independent of the unrelated `docs/timings.md`
modification already in the working tree). Ran the scoped form instead:
`git stash push -u -m "task4-spillway-byte-identity-control" -- \
scripts/subfloor-roster.sh`, captured `bash scripts/subfloor-roster.sh` to a
scratch file, `git stash apply <sha>` (not `pop`) to restore the change, then
`git stash drop stash@{0}` to remove only that entry (never touching
`docs/timings.md` or any other worktree's stash). Final comparison:
`bash scripts/subfloor-roster.sh | cmp - "$sp/roster-before" &&
echo IDENTICAL-BY-DEFAULT` printed `IDENTICAL-BY-DEFAULT` — the default
filterset (4093 terms) is byte-for-byte unchanged by this task, the positive
proof that nothing but the escape moved.

**Ideonomy: none.** The brief specifies the roster `awk` line, the hook
block, and the test text verbatim; the shellcheck directive and the stash
workaround are mechanical fixes surfaced by actually running the commands,
not design choices.

**Files changed:** `scripts/subfloor-roster.sh` (the `awk` line and its
header comment), `scripts/hooks/pre-commit` (the final `HV_CENSUS_DELIVERY`
block), `scripts/test-census-guard.sh` (the appended arms plus one
`shellcheck disable=SC2016` comment), this ledger.

**Concerns.** None outstanding.

## Task 5 — complete

**Step 1 (verify, not mint).** Decision 0836 was minted inside Task 2
(commit `7f7333b72`) by controller ruling, because
`decision_cites_in_sources_resolve` scans `scripts/` as well as Rust and a
script comment citing 0836 could not land before the record existed.
Verified here: `ls docs/decisions/0836-*` shows the file, its text matches
the brief verbatim, and all four `Relates:` filenames
(`0079-census-goldens-are-authored-on-one-enforced-host.md`,
`0133-nontrivial-checks-run-in-one-serial-lane.md`,
`0139-main-advances-only-through-the-lock.md`,
`0514-a-census-refresh-needs-no-authorization.md`) exist under
`docs/decisions/`. Re-ran both `render` commands
(`decisions`, `delta`); `git diff --exit-code docs/digest/` was empty for
both — the digest was already current from Task 2's mint, so this task's
regeneration produced no new diff. Nothing to commit under `docs/digest/`.

**Step 2 — Rust comments and one message.**
`windows/lab/src/domesday/anomaly.rs:867` — replaced the `// masked it (this
test is not in the sub-floor tier a local gate runs).` comment with the
brief's correction (it IS in the sub-floor tier; cites decision 0836).
`windows/lab/tests/suite/anomaly_injection.rs` — replaced the branch-table
bullet above `the_fixture_columns_match_the_census` (the "Task 7 carries
this" bullet) with the queued-delivery description, and extended the
`assert!` message to name `make sluice-census` and decision 0836, keeping
the file's `\` line-continuation style.

**Step 3 — prose.** `windows/lab/tests/fixtures/injection/README.md` — the
`## Regenerate with the script, and only with the script` section (through
"...adjudicates nothing.") replaced with `## Regenerated by the census
delivery, or by the script by hand`, naming `make sluice-census` as the
ordinary author and the indented by-hand `ssh lefford` block as the
exception. `windows/lab/CLAUDE.md` — the paragraph beginning "That last
sentence no longer describes what happens, and the replacement is QUIETER,
not louder" replaced: only `the_fixture_columns_match_the_census` reds now
(the movement control is green with a PREDATES line), and since The Spillway
the census delivery itself re-authors the arms. `scripts/CLAUDE.md` — added
a bullet after the `sluice-census.sh each claim their own row` bullet,
naming the arm re-authoring, the `gnomon-injection` timing row, the
no-claim-file gap, and the three-check `HV_CENSUS_DELIVERY` stand-down rule.
Root `CLAUDE.md` — added a block after "...check whether its test is
exercising the case that actually occurs before concluding nobody wrote
one." stating the rule (satisfies what a regeneration remedies, defers what
needs a human re-statement) and the three deferred checks.

**Step 4 — gate.** `cargo fmt --check`: clean (rc=0). `cargo nextest run -p
hornvale --test suite -E 'test(docs_consistency)'`: 41 passed, 0 failed,
280 skipped — including `decision_cites_in_sources_resolve` (the new
comment cites 0836, which now resolves) and
`a_decision_records_title_matches_its_filename`. `make gate-commit`: 1282
tests run, 1282 passed, 4378 skipped; subfloor chunks all green;
`wall=50.614s user=111.040s sys=28.944s cpu_ratio=2.77 rc=0`.

**Self-review.** Every prose site the brief's Files list and spec §3.5 name
is touched: the decision record (verified, not re-minted), the digest
(verified unchanged), the two `anomaly.rs`/`anomaly_injection.rs` Rust
sites, the README section, the `windows/lab/CLAUDE.md` paragraph, the
`scripts/CLAUDE.md` bullet, and the root `CLAUDE.md` block. Nothing else in
those files was touched. `docs/timings.md` (modified by earlier gate runs,
not by this task) was left unstaged, per the brief's `git add` list.

**Ideonomy: none.** The brief specifies every replacement text verbatim;
this task's only judgment calls were where exactly to splice the new
`scripts/CLAUDE.md` bullet and root `CLAUDE.md` block, both resolved by the
brief's own anchor text.

**Files changed:**
`docs/digest/decisions-in-force.md`,
`docs/digest/intent-vs-reality.md` (both re-verified, no diff produced),
`windows/lab/src/domesday/anomaly.rs`,
`windows/lab/tests/suite/anomaly_injection.rs`,
`windows/lab/tests/fixtures/injection/README.md`,
`windows/lab/CLAUDE.md`,
`scripts/CLAUDE.md`,
`CLAUDE.md`,
this ledger.

**Concerns.** None outstanding.

---

## Close — the digest after G3, for the G6 package

**The rule this campaign exists to state** (spec §3.1, decision 0836), which
leads the G6 package:

> A delivery satisfies every check whose remedy is a regeneration, and defers
> only a check whose remedy is a human re-statement.

### What shipped, in five commits plus two absorptions

| commit | what |
| --- | --- |
| `b21423406` | `gnomon-injection.sh` gains a `check` subcommand; its tree guard narrowed to what a build or the source mutation can see (`book/`, `docs/` excluded). New `scripts/test-gnomon-injection.sh`, 10 arms including two CONTROLs, registered in the `outboard` set. |
| `7f7333b72` | `census_schema_columns` and `injection_arms_stale` under `sluice-census.sh`'s `HV_CENSUS_LIB=1` seam; **decision 0836 minted here** by ruling. |
| `21ebd79ee` | The delivery re-authors the arms: trigger, ref's-own-`check` pre-flight, the shared `flock`, `timed.sh gnomon-injection`, staging, the commit-message and report lines. Every arms failure exits 4 with the goldens left staged. |
| `817d50f19` | `HV_SUBFLOOR_EXCLUDE` on `subfloor-roster.sh` (term removal, keeping the filterset flat) set by `pre-commit` inside its `HV_CENSUS_DELIVERY` block; the count witness stands down for the delivery commit only. |
| `6e8748a36` | Decision 0836 verified; the two Rust comment sites, the fixtures README, `windows/lab/CLAUDE.md`, `scripts/CLAUDE.md` and root `CLAUDE.md` brought current. |

`1f13da1af` and `dbb76e843` are absorptions of `main` (the second, 60
commits, carried six campaigns and two census-schema changes).

### Rulings made during execution (from the task ledger, promoted here)

1. **Task 2 — decision 0836 minted three tasks early.** The plan staged the
   record for Task 5 on the reasoning that a Rust comment cannot cite an
   unminted number. `docs_consistency::decision_cites_in_sources_resolve`
   scans `scripts/` as well, and Task 2's commit was scripts-and-docs, so the
   cite was checked at Task 2. Ruling: mint 0836 then, from Task 5's exact
   text, after confirming its four `Relates:` targets exist. Cost if wrong: a
   decision record a day early whose content the approved spec had fixed.
   Task 5 Step 1 became a verification, and verified clean.
2. **Task 3 — arm (c)'s reset target.** The brief's `reset --hard HEAD~1`
   lands one commit short once the delivery's own commit sits on the stale-arm
   commit. Ruling: record `pre_c` before the stale-arm commit and reset to
   that SHA.
3. **Task 3 — arms (d)/(e) never entered the arms path.** After the first
   `moves` delivery the worktree golden already read `42,2`, so a second
   `write_stub moves` was a no-op, `n_goldens` read 0, and both failure arms
   ran past their assertions on an empty diff. Ruling: write the golden back
   to `42,1` before (d); (e) inherits it through (d)'s `reset --hard`. Both
   arms gained a positive `grep -q 're-authoring the Gnomon injection arms'`.
4. **Task 1 → Task 3 — the pre-existing SC2329 finding** on
   `release_census_row` (local shellcheck newer than lefford's) was folded
   into Task 3's edit of that file rather than left. Closed.
5. **Task 3 — `LC_ALL=C` on `census_schema_columns`'s `sort`**, folded into
   the same commit.

### Findings

- **Task 3, latent and fixed in-task.** `scripts/test-sluice.sh`'s census
  fixture had never carried a `docs/generated-paths.txt`. `sluice_path_author`
  tolerates a missing file by returning an empty author, so
  `census_golden_count` read **0** on every moving-census run that file has
  ever driven — three arms passing vacuously, invisible until the arms'
  trigger became the first thing to depend on the count. Fixed by adding the
  file to the fixture, mirroring the real committed rows; every pre-existing
  arm's pass count was unchanged before and after.
- **Close: found, and fixed in this campaign's own fix wave before merge.**
  `census_schema_columns` did **not** skip the study-name line on the real
  files. The extractor's rule was `^ {4,}"name":` on the stated belief that a
  study's own name sits at indent 2; in serde's output it sits at indent 4,
  nested under `"study"` — a distinct depth from a column's indent 6, but
  `>= 4` catches both, which is the whole bug. Measured at this tip before the
  fix: `injection_arms_stale` reported all eight arms stale, the difference on
  each being exactly `the-census` versus `gnomon-injection`. Consequences: the
  "arms unchanged" branch was unreachable, so every delivery re-authored; it
  failed SAFE (a mis-parse can only cause a needless re-author, which is the
  design's own stated safety argument) and the arms WERE current at this tip
  (`manifest.json sha=d2bd513f12072120d7f749f4d63a8da88b8fd3c9`, The Lot's
  delivery); and the test arm written to catch it passed, because
  `write_schema` in `scripts/test-sluice-census.sh` emitted the study name at
  indent 2 — the shape the belief predicted rather than the shape the
  pretty-printer produces. **Fixed in the fix-wave commit**: the extractor now
  anchors on exactly six spaces (`^ {6}"name": "`), `write_schema` now nests
  the study name where serde nests it (a `"study"` object at indent two, its
  `"name"` at indent four), and a fixture-shape control, a real-tree control,
  and a positive control were added — see `## Fix wave` below. A census of
  this tip is now expected to report `Gnomon arms unchanged` on a null.
  Recorded in the retrospective and named in the chronicle, both updated to
  say found-and-fixed.
- **Stage gate 1 (`req-1f13da1aff69`) went RED at `outboard`** on a single
  shellcheck style finding (SC2002, `scripts/test-gnomon-injection.sh:42`).
  This Mac's shellcheck 0.11 does not report SC2002; lefford's 0.9.0 does. So
  `make shellcheck` was clean locally, and `make gate-commit` does not run
  shellcheck at all — a scripts-only change is unlinted until the chamber
  takes the box. Fixed by the controller as an exact-content change and
  verified against lefford's version; the gap is filed on the registry as
  `TOOL-out-of-workspace-crates-have-no-local-lint`.

### Figures re-derived at close, not restated

The spec's §3.2 verification block records 285 name keys / 284 columns; that
is a **drafting-time measurement** and is labelled so. At this tip, computed
from the tree:

| figure | value at close | command |
| --- | --- | --- |
| census CSV columns | 290 | `grep -c '"kind":' book/src/laboratory/generated/the-census/schema.json` |
| census metric columns | 287 (290 less `seed`, `pin_set`, `refusal`) | the witness's own comment |
| count witness's name | `evaluable_columns_measured_surface_on_the_287_column_census` | `docs/timings/subfloor-roster.tsv:1279` |
| Gnomon arms | 8 | `ls -d windows/lab/tests/fixtures/injection/*/` |
| arms' manifest ref | `d2bd513f1…` (The Lot's delivery) | `windows/lab/tests/fixtures/injection/manifest.json` |
| sub-floor terms, default vs excluded | 4095 → 4094 (exactly one omitted) | `scripts/subfloor-roster.sh` with and without `HV_SUBFLOOR_EXCLUDE` |
| `HV_CENSUS_DELIVERY` stand-down branches | 3 | `grep -cF 'if [ -n "${HV_CENSUS_DELIVERY:-}" ]' scripts/hooks/pre-commit` |

### Deferred minors carried to the retrospective

Every `minor (deferred)` line from the task ledger is listed in
`docs/retrospectives/the-spillway.md`'s deferred-minors table with its
disposition: the two closed in Task 3 (SC2329, `LC_ALL=C` on the sort), two
more closed in the fix wave below (`comm` collation, same-second log
collisions), the one discharged in review (timings-label additivity), and
the five still accepted as close minors (the swallowed `git add` failure and
unbounded `add -u`, the unguarded `exec 9>`, duplicated rationale prose, a
ledger line's 38-versus-40, a punctuation difference). The column-extractor
Critical defect is tracked separately, as its own row, not among these
minors — see `### Findings` above and `## Fix wave` below.

### Freshness sweep

`grep -rn 'gnomon-injection' book/src docs/*.md docs/retrospectives` (excluding
this campaign's own files) returns seven hits and **none is stale**:

- `book/src/frontier/idea-registry.md:1789` — the deadlock row itself,
  flipped to `shipped` by this commit.
- `book/src/chronicle/the-granary.md:59`, `docs/retrospectives/the-gnomon.md`,
  `the-winze.md`, `the-glasshouse.md` (×2), `the-granary.md` — dated
  historical records of what those campaigns did. A chronicle or a
  retrospective is a record with a date; none of them describes the by-hand
  re-authoring as the *ordinary path today*, and none is rewritten.

The prose that *did* describe the by-hand path as ordinary — the
`sluice-census.sh` and `gnomon-injection.sh` headers, the fixtures README,
`windows/lab/CLAUDE.md`, `scripts/CLAUDE.md` and root `CLAUDE.md` — was
brought current in Task 5 (`6e8748a36`), which is why the sweep finds nothing
left. No stale sentence was found and none was fixed at close.

### Confidence Gradient

N/A, no bet moved. `book/src/open-questions.md` is untouched: this campaign
changes what a delivery commit carries, not what the world is or what is
known about it.

### Success criteria (spec §6) at close

1. The four shell test files pass with every §4 arm present; `make shellcheck`
   clean **locally** — see the stage-gate finding above for what that did and
   did not prove.
2. **Not yet demonstrated.** The census of this tip is the controller's Step 5
   and had not run when this section was written. The close's own finding was
   fixed before merge in the fix wave (`## Fix wave` below), so the
   expectation reverts to the design's original one: a delivery at this tip
   with no golden movement and no stale arm should report `Gnomon arms
   unchanged` and the null verdict, not an unconditional re-author.
3. `HV_CENSUS_DELIVERY=1` stands down exactly three checks; pinned at 3 by
   `scripts/test-census-guard.sh` and re-derived above.
4. The registry row reads `shipped`; decision 0836 is in force; the §3.5 prose
   no longer describes a by-hand re-authoring as the ordinary path.

## Fix wave

One Critical defect plus three deferred minors, closed together in one
commit before merge (this commit), so the census-block prose above and in
`book/src/chronicle/the-spillway.md` and `docs/retrospectives/the-spillway.md`
now say found-and-fixed rather than open. Four changes:

1. **`scripts/sluice-census.sh`, `census_schema_columns`** — anchors on
   exactly six spaces (`^ {6}"name": "`) instead of `^ {4,}`. The comment
   above the two functions is corrected: the study's own `"name"` sits at
   indent 4, nested under `"study"`; column names sit at exactly indent 6,
   one per `"kind"` — verified at this tip: `grep -c '"kind":'` and
   `grep -c '^      "name":'` both read 290, on the census schema and every
   one of the eight injection arms. The comment now says the earlier
   `^ {4,}` rule kept the study name and read every arm stale, found at the
   campaign's close by re-deriving these figures.
2. **`scripts/sluice-census.sh`, `injection_arms_stale`** — both `comm`
   calls now carry `LC_ALL=C`, matching the `sort` that already had it
   (deferred minor, closed).
3. **`scripts/test-sluice-census.sh`** — `write_schema` now emits the real
   shape (a `"study"` object after the `columns` array, its `"description"`
   and `"name"` nested inside), so the "ignores the study's own name" arm
   can actually discriminate. Three controls added: a FIXTURE-SHAPE control
   pinning the fixture to the real committed `book/src/laboratory/generated/
   the-census/schema.json` (1 name@indent4, `grep -c '"kind":'` ==
   `grep -c '^      "name":'`); a REAL-TREE control asserting
   `injection_arms_stale` over the actual checkout (`$root`) prints nothing —
   the same fact `anomaly_injection::the_fixture_columns_match_the_census`
   asserts in Rust, so this cannot be vacuous; and its positive half, which
   copies the real census schema and one real arm schema into a scratch
   tree, deletes one column object from the copied census schema with a
   small `python3` script (asserting the `"kind"` count dropped by exactly
   one), and asserts `injection_arms_stale` reports that arm with
   `0 column(s) the census has and the arm lacks, 1 the arm has and the
   census lacks`.
4. **`scripts/test-sluice.sh`, census block** — the stub schema files
   written into the stub census tree (the two `printf` calls that used to
   write `"name": "the-census"` / `"name": "gnomon-injection"` at indent 2,
   plus the stale-arm schema further down) now use the real shape too.
   `newest_census_log` finds the current newest `census-*.log`; the
   failed-authoring and refused-check arms record its path and byte size
   BEFORE calling `run_census`, then grep only `tail -c +$((off + 1))` of
   that log for the corroborating `re-authoring the Gnomon injection arms`
   line (two runs of the same ref inside one second share a log via
   `exec >>`, so the old whole-file grep could match a neighbouring run's
   bytes) — deferred minor, closed. The primary assertions (rc, branch
   count, `git diff --cached`) are unchanged.

**Verification.** `bash scripts/test-sluice-census.sh`: 19 passed, 0 failed
(was 15). `bash scripts/test-gnomon-injection.sh`: 10 passed, 0 failed.
`bash scripts/test-census-guard.sh`: 21 passed, 0 failed. `make shellcheck`:
clean on this Mac (shellcheck 0.11.0). On lefford (shellcheck 0.9.0):
`shellcheck scripts/*.sh scripts/hooks/*` clean, `bash scripts/test-sluice.sh`
263 passed, 0 failed, every census-block arm `ok`. Re-run after pushing the
commit, from `origin/campaign/the-spillway` on lefford: unchanged.
`cargo nextest run -p hornvale --test suite -E 'test(docs_consistency)'`
green over the close-prose edits.

## Fix wave 2

The final whole-branch review's findings: prose that still asserted claims
this campaign's own close had disproven, plus one small guard widening.
Nine changes, one commit:

1. **`scripts/gnomon-injection.sh` header** — "nothing regenerates these
   fixtures automatically, and nothing should" was true when written and is
   false now: since this campaign (decision 0836) the queued census delivery
   IS the fixtures' ordinary automatic author. Corrected loudly, in place;
   the residual truth — the ARTIFACT SWEEP must never author them, because
   that would mutate tracked source on its way past — is kept. The
   "WHERE IT RUNS" `ssh lefford` line is marked as the by-hand path, now the
   exception rather than the ordinary caller.
2. **`scripts/gnomon-injection.sh` guard** — widened to
   `":!$FIXTURES" ":!book" ":!docs" ":!clients"`; the comment gains a fourth
   bullet (`clients/` is outside the cargo workspace and no `lab run` reads
   it, but the census's own artifact sweep regenerates
   `clients/game/core/tests/fixtures/`, declared `artifacts` in
   `docs/generated-paths.txt`, so a delivery's dirt can include it). The
   `check OK` message now names all four exclusions.
   `scripts/test-gnomon-injection.sh` gained a scratch
   `clients/game/core/tests/fixtures/x.json` and a new arm asserting a
   MODIFIED `clients/` file is allowed, placed with the other allowed-dirt
   arms; the two CONTROL arms are unchanged.
3. **`scripts/sluice-census.sh`** — the pre-flight refusal message ("a ref
   that predates The Spillway refuses the delivery's own staged goldens as
   dirt") is softened to point at the refusal actually printed above it
   first, naming the predates-the-merge case as the common one rather than
   the only one. The `HV_CENSUS_DELIVERY=1` comment is corrected from "a
   scoped, named opt-out of ONE check" to naming all three it stands down
   (the golden-pins guard, the yellow-census alarm, the column-count
   witness), matching `test-census-guard.sh`'s pinned count of 3.
4. **`docs/superpowers/specs/2026-09-06-the-spillway-design.md`** — §3.2
   step 1 gains a loud inline correction (not a silent rewrite): the
   study's name sits at indent 4 under `"study"`, so `>= 4` kept it and read
   every arm stale; the shipped rule is exactly six spaces, verified at
   close on all nine real files (`"kind"` 290, name@indent6 290,
   name@indent4 1, name@indent2 0); fixed in `81968faee`. §9's matching risk
   row gets the same correction. §3.3's dirt enumeration gains
   `clients/game/core/tests/fixtures/**` and the predicate now reads
   `":!clients"` too. §5 gains a caveat: on a ref predating the merge,
   `check` is parsed as an ARM NAME by the old script (clearing the fixture
   directory before printing "unknown arm"), unreachable in production
   because that ref's whole-tree guard refuses first, and self-healing on
   the next `reset --hard`. The stale `subfloor-roster.tsv:1273` citation in
   §1 is corrected to `:1279` (this tip's line for the count witness).
5. **This ledger** — Task 2's impossible measurement sentence ("285 name
   keys total and 284 at indent >= 4") gets a bracketed correction appended
   in place (append-only), and the Follow-ups section's
   `subfloor-roster.tsv:1273` citation is corrected to `:1279`. This section.
6. **`windows/lab/tests/fixtures/injection/README.md`** — "reproducible from
   `manifest.json` by a human running the script, never by the artifact
   sweep" gains "by the census delivery or" before "by a human", since the
   delivery is now the ordinary author.
7. **`scripts/CLAUDE.md`** — the `subfloor-roster.sh` bullet gains one
   sentence naming `HV_SUBFLOOR_EXCLUDE`'s one caller (`pre-commit` under
   `HV_CENSUS_DELIVERY`, decision 0836) and restating that it omits rather
   than wraps, so the chunker's flat split holds.
8. **`docs/retrospectives/the-spillway.md`** — "Estimate deltas"' `tasks` row
   now names the two fix waves instead of "no fix rounds"; the header's
   ledger description gains the two fix-wave sections. One accepted item
   added to the deferred-minors table: `test-sluice-census.sh`'s dependency
   on `python3` (the fix wave's positive-control script), precedented in
   `test-census-path.sh` and `test-pre-push.sh`. **A second item this task
   was briefed to add — that commit `f22c7c083` carries no `Claude-Session`
   trailer — was checked against the actual commit object
   (`git cat-file -p f22c7c083`) and is false: the trailer is present. Not
   added; flagged instead of propagated.**
9. **`book/src/chronicle/the-spillway.md`** — the guard-exclusion paragraph
   gains `clients/`, attributed to this review. Its indentation account
   (already correct — indent 4 study, indent 6 columns, exactly-six rule)
   was checked and needed no change.

**Verification.** `bash scripts/test-gnomon-injection.sh`: 11 passed, 0
failed (was 10; the new `clients/` arm). `bash scripts/test-sluice-census.sh`:
19 passed, 0 failed (unchanged — this wave touches only comments and a
message string in this file). `bash scripts/test-census-guard.sh`: 21
passed, 0 failed (unchanged). `make shellcheck`: clean on this Mac
(shellcheck 0.11.0). `cargo nextest run -p hornvale --test suite -E
'test(docs_consistency)'`: 41 passed, 0 failed, 280 skipped. `docs/timings.md`
picked up local mtime/duration churn from running these suites and is
intentionally left unstaged.
