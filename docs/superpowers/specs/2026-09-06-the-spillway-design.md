# The Spillway — a census delivery regenerates the evidence its own gate reads

**Status:** draft for G3 · **Branch:** `campaign/the-spillway` · **Decision
block:** 0836–0845 · **Registry:**
`TOOL-census-delivery-and-injection-fixtures-deadlock` (raw → shipped at close)
· **Ledger:** `docs/superpowers/ledgers/2026-09-06-the-spillway.md`

A spillway is the channel that lets a full reservoir deliver past the dam
instead of backing up behind it. The Warp's census filled the reservoir and
the dam held it: 136 goldens sat staged in the canonical worktree for an
evening while a human broke the deadlock by hand.

## 1. The problem, as it happened rather than as it was described

`make sluice-census` at `4a419e996ef7` (The Warp, 2026-09-05) ran the census
in 1,145 s and moved 136 goldens. Its delivery commit was then refused by the
commit gate, and the refusal is in the queue's own log
(`~/.local/state/hornvale/sluice/census-4a419e996ef7-20260905T213324Z.log`
on lefford):

```
sluice-census:    docs/audits/type-audit-report.md                   |   16 +-
...
pre-commit: HV_CENSUS_DELIVERY=1 — skipping the golden-pins guard for this census delivery.
...
        FAIL [   0.549s] hornvale-lab domesday::anomaly::tests::evaluable_columns_measured_surface_on_the_249_column_census
...
pre-commit: 'make gate-commit' failed — fix fmt/clippy/type-audit/tests before committing.
sluice-census: COMMIT REFUSED — the census ran and its output is NOT delivered.
sluice-census: 144 staged path(s) (136 golden) remain in /home/nathan/Projects/hornvale/../hornvale-census-wt; nothing was pushed.
```

Three facts in that log are the whole problem, and the registry row names
only two of them.

**Leg 1 — a gate test compares the fixtures' columns to the census's.**
`windows/lab/tests/suite/anomaly_injection.rs::the_fixture_columns_match_the_census`
is in the sub-floor roster and asserts that every Gnomon injection arm's
`schema.json` has exactly the committed census's column set. A census that
registers a metric is red on it by construction. Its own message says what to
do: "Re-author with scripts/gnomon-injection.sh on the canonical box in the
same commit as the census refresh."

**Leg 2 — the authoring script refuses the delivery's own dirt.**
`scripts/gnomon-injection.sh` refuses to run unless
`git status --porcelain -- ":!windows/lab/tests/fixtures/injection"` is
empty, because it rewrites tracked source in place and restores it with
`git checkout --`. The delivery's staged goldens and timings row are exactly
that non-empty status. So the arms cannot be re-authored until the refresh is
committed, and the refresh cannot be committed until the arms are re-authored.

**Leg 3 — a second gate test carries the column count in its own name, and no
machine may re-pin it.** `windows/lab/src/domesday/anomaly.rs::tests::
evaluable_columns_measured_surface_on_the_<N>_column_census` asserts the
evaluable and excluded column counts of the committed census, and its
doc-comment is a per-campaign narrative of every epoch's counts ("re-measure
and update this"). It is what failed FIRST in the log above. It is in the
sub-floor roster (`docs/timings/subfloor-roster.tsv:1279`) by exact name —
the roster's own comment says the name is updated in the same commit — so a
growing census reds it on every delivery commit that takes the gate-commit
path. And the delivery takes that path whenever the census's artifact sweep
moves `docs/audits/type-audit-report.md`, which `pre-commit`'s
`rust_relevant` regex names: the log's first line above is that file moving.

The Warp broke all three by hand: an ungated intermediate commit object
(`git write-tree`/`commit-tree`, so the goldens existed somewhere clean), a
fresh worktree on lefford that checked it out and ran the eight-minute
re-authoring under queue contention, a hand re-pin of the count witness, and
the delivery commit on top through the gate (ledger #12, #13). Nothing that
landed bypassed a hook. Everything about how it landed was labour that the
queue exists to remove.

**The scope is wider than The Warp.** Any campaign whose close registers a
metric hits this at its census refresh, and windows/lab/CLAUDE.md already
records that the Gnomon arms are "still owed a re-authoring on the canonical
box … covered by no drift check" after every such registration. The Weft's
delivery (`b162273b4`, +22 columns) carried no arms at all; The Warp's
manifest is stamped with the intermediate object's SHA rather than the
census's ref. The evidence has been drifting from the census it is scored
against for as long as the two have been authored by different hands.

## 2. Scope

In:

1. `scripts/sluice-census.sh` re-authors the Gnomon injection arms in the
   census worktree, at the censused ref, before its delivery commit, whenever
   the run moved a census golden or the arms' column set differs from the
   census's; ledgers the cost; takes the shared flock for the duration; stages
   the arms into the same commit.
2. `scripts/gnomon-injection.sh`'s clean-tree guard is narrowed from "the
   whole tree except my own fixtures" to "everything the built binary and the
   mutated source can see" — dirt under `book/` and `docs/` no longer refuses.
   A `check` subcommand runs the guards and stops, so a caller can ask before
   paying for the lock and so the guard is testable without a build.
3. The count witness is stood down for the delivery commit only, through a
   term-removing exclusion in `scripts/subfloor-roster.sh` that `pre-commit`
   sets inside its existing `HV_CENSUS_DELIVERY` block. Deferred, never
   discharged: the merge of the delivery branch runs the full suite and
   demands the re-pin, exactly as it demands the calibration pins.
4. Tests for all three, in the shell test files the `outboard` set already
   runs; prose that describes the delivery, the arms and the witness brought
   current; one decision record; the registry row flipped; chronicle,
   retrospective, freshness sweep.

Out (§7): re-pinning any witness by machine; changing what the witnesses
assert; the recall witness's bar or battery; any other registry-frozen
fixture (there is none — `find windows/lab/tests/fixtures -maxdepth 1` lists
`injection/`, `sentinel-waivers.txt`, `affect-trace-seed-42.txt`); the census
itself; `census-run.sh`.

## 3. Design

### 3.1 The rule that places every census-shaped check

A delivery commit is a commit made by a machine at the one moment the
canonical box holds the freshest possible world. The `HV_CENSUS_DELIVERY`
opt-out already stands down two checks for it, and each was justified
separately: the golden-pins guard because "re-pinning is the owning
campaign's judgment call", the yellow alarm because its acknowledgement "is
keyed to this run's own timestamp and cannot be satisfied before the row
exists". This campaign states the rule those two are instances of, and
places the third and fourth checks by it:

> **A delivery satisfies every check whose remedy is a regeneration, and
> defers only a check whose remedy is a human re-statement.**

| check the delivery commit meets | remedy | placement |
| --- | --- | --- |
| golden-pins guard (`census-check`) | a human re-pins after reading the diff | deferred (existing) |
| yellow-census alarm | a human records a profiling finding | deferred (existing) |
| `the_fixture_columns_match_the_census` | re-run `gnomon-injection.sh` on the box at the ref | **satisfied** (this campaign) |
| `evaluable_columns_measured_surface_on_the_<N>_column_census` | a human re-measures, renames the test, re-states the narrative, updates the roster line | **deferred** (this campaign) |
| everything else in the sub-floor roster | — | runs, unchanged |

Deferral is safe for one reason only, and it is not this campaign's: the
delivery never pushes `main` (its header, decision 0139), so every deferred
check is re-demanded by the chamber's full suite when the branch is submitted
as a merge, and the owning campaign re-states the witness on the branch it
merges — as The Warp's Task 8 did, and as every campaign already does for the
calibration pins. Nothing stood down here can reach `main` un-gated. This is
decision 0836's content (§8).

### 3.2 The delivery re-authors the arms

In `scripts/sluice-census.sh`, after `census-run.sh` returns 0 and before
anything is staged:

1. **Stage, then decide.** The existing staging runs first, unchanged
   (`add -A -- book/src/laboratory/`, then `add -u`), so `n_goldens` is
   `census_golden_count` over the index exactly as today and the function
   and its tests are untouched. The column-set comparison reads every
   `windows/lab/tests/fixtures/injection/*/schema.json` against
   `book/src/laboratory/generated/the-census/schema.json`, extracting the
   `"name"` values nested under `columns` (indented ≥ 4 spaces in the
   serde-pretty output). Verified at drafting time against the committed
   files:

   ```
   $ grep -c '"name":' the-census/schema.json baseline-a/schema.json   → 285, 285
   $ grep -c '"kind":' …                                                → 284, 284
   $ diff <(sort names-census) <(sort names-arm)
   102a103  > "name": "gnomon-injection"
   216d216  < "name": "the-census"
   ```

   The one surplus `"name"` on each side is the study's own, at indent 2;
   the column names are at indent 6. Both studies declare `"metrics": "all"`
   (`studies/the-census.study.json`, `studies/gnomon-injection.study.json`),
   so at one ref the sets are identical by construction and a difference
   means the arms were authored at another.

   **CORRECTED AT CLOSE.** The study's name sits at indent 4, nested under
   `"study"`, not at indent 2 — `≥ 4` therefore kept it, and every arm read
   above was stale on that account: it counted the study's own `"name"` line
   as a column. The shipped extractor anchors on exactly six spaces
   (`^ {6}"name": "`), which is the column names' depth and only theirs.
   Verified at close against all nine real files: `"kind"` count 290, names
   at indent 6 count 290, at indent 4 count 1 (the study's own line), at
   indent 2 count 0. Found by re-deriving these figures at close; fixed in
   `81968faee` (`census_schema_columns`, `scripts/sluice-census.sh`).

   Re-author iff `n_goldens > 0` OR any arm's column set differs. A null
   census with matching arms re-authors nothing and says so.

2. **Pre-flight.** `bash "$wt/scripts/gnomon-injection.sh" check` — the
   worktree's OWN copy, because the `ARMS` table's literals must match the
   source at the ref (an old ref with a renamed constant would fail the
   "TARGET NOT FOUND" assertion under the queue's copy). `check` runs the
   canonical-host guard and the clean-tree guard and exits without building.
   A refusal here is reported and the delivery exits 4 with the goldens in
   place, exactly the shape of today's COMMIT REFUSED (§5 says when this
   happens).

3. **Lock.** `exec 9>"${HV_CENSUS_LOCK:-/tmp/hv-census.lock}"; flock -w
   "${HV_CENSUS_WAIT_TIMEOUT:-2700}" 9`. `census-run.sh` released this flock
   when it exited; decision 0133 says every expensive job on the box takes
   it, and an eight-minute `lab run` over 20 seeds × 8 arms is one. No claim
   file is written: the eight-field claim format is owned by
   `windows/lab/src/census_claim.rs` and written by `census-run.sh` and
   `sluice-run.sh`; a third writer is the drift `cli/tests/lane_sets.rs`
   exists to refuse. **Accepted gap:** for the re-authoring's duration
   `census-run.sh status` and `lab claim-status` report no run while any
   flock-taker waits. The queue row stays `running` throughout (the drain
   runs the census script synchronously — `sluice-drain.sh`: `(cd … && bash
   "$runner" "$ID")`), so no queued job is dispatched under it.

   `flock(1)` exists on lefford (`/usr/bin/flock`) and not on this Mac
   (`which flock` → nothing), measured at drafting. The step FAILS CLOSED
   without it (exit 4, goldens left staged, nothing pushed): in production
   it is unreachable without `flock`, because `census-run.sh` took the same
   flock on the same host minutes earlier, and the only harness that drives
   this path, `scripts/test-sluice.sh`, already exits SKIP on a host without
   `flock` (its first guard). So no host that reaches this step lacks the
   primitive, and refusing costs nothing while degrading would invent a
   second locking story for a host that never delivers. The test's positive
   control asserts the lock was HELD while the stub authored (§4.2a).

4. **Author.** `bash "$repo_root/scripts/timed.sh" gnomon-injection -- bash
   "$wt/scripts/gnomon-injection.sh"` run with cwd `$wt`. `timed.sh`
   resolves its ledger with `git rev-parse --show-toplevel`, so the row lands
   in `$wt/docs/timings.md` beside the census's own row and is swept by the
   same `add -u`. The label is new; `docs/timings.md`'s readers key on label
   only where they count a specific one (`| census |`), so an added label is
   additive — confirmed at execution by running the timings tests, not
   assumed. The release binary is warm: `census-run.sh` built it
   (`cargo run --release`) minutes earlier in the same worktree.

5. **Release the flock, stage the arms, commit.** `exec 9>&-`, then
   `add -A -- windows/lab/tests/fixtures/injection` and a second `add -u`
   (which sweeps the re-authoring's own timings row). The commit message
   gains one line: `Gnomon arms re-authored at <ref> (8 arms)` or `Gnomon
   arms unchanged (census moved nothing; columns match)`. The report line
   counts arms separately from goldens.

A failed re-authoring (non-zero from the script) is a refusal: the delivery
exits 4, prints the script's tail, leaves the goldens and any partial
fixtures in the worktree, pushes nothing. It is NOT a census failure and the
message says so, as today's COMMIT REFUSED does. A void arm is a real
finding (`every_injection_moved_the_world_and_the_baselines_did_not` would
red on it) and must not be papered over by delivering the goldens without
the arms.

### 3.3 The guard, narrowed to what it protects

`gnomon-injection.sh`'s guard exists for two stated reasons: it mutates
tracked source and restores it with `git checkout --` (uncommitted edits to
those files would be destroyed), and the manifest stamps `sha=$(git
rev-parse HEAD)` as a provenance claim about what was built. Both are claims
about SOURCE. The new predicate:

```
git status --porcelain -- . ":!$FIXTURES" ":!book" ":!docs" ":!clients"
```

must be empty. `book/` holds the census's own output and the project book;
`docs/` holds prose and ledgers; `clients/` is outside the cargo workspace
entirely (root `Cargo.toml` excludes it) and no `lab run` reads it. Neither
`book/` nor `docs/` is compiled into the `hornvale` binary and neither is
read by a `lab run` of the injection study — verified at drafting time:
every `laboratory/generated` load in `windows/lab/src` is under
`#[cfg(test)]` or in the `domesday` report path (`comparators.rs:204`,
`stats.rs:103` open `mod tests` before their loads; `grep -rln
'laboratory/generated' windows/lab/src/metrics/` → 0 files). **Corrected at
close:** the dirt enumeration below was incomplete — a census delivery's
dirt is `book/src/laboratory/generated/**`, `book/src/domesday/**`,
`docs/timings.md`, `docs/audits/*.md`, `docs/generated-path-writes.tsv`, AND
`clients/game/core/tests/fixtures/**` (declared `artifacts` in
`docs/generated-paths.txt`, regenerated by the census's own artifact sweep)
— all inside the (now four-way) exclusion — while an uncommitted edit to
`domains/`, `kernel/`, `windows/`, `cli/`, `studies/`, `Cargo.*` or
`scripts/` still refuses.

`check` is a subcommand, not a flag: it runs both guards, prints which
would refuse and why, and exits 0/1. The delivery calls it before waiting
for the lock. The test file drives it in a scratch repo without a build.

### 3.4 The count witness stands down for the delivery commit

`scripts/subfloor-roster.sh` gains `HV_SUBFLOOR_EXCLUDE`, an ERE matched
against the roster's test-path column; matching lines are omitted from the
flat `test(=a) | test(=b)` expression. Removal keeps the expression flat,
which `subfloor-run-chunked.sh` requires ("the split is lossless because the
roster is flat … a future roster shape with real nesting would need a
smarter splitter"). `pre-commit` sets it, inside the existing
`HV_CENSUS_DELIVERY` block on the gate-commit path, to
`evaluable_columns_measured_surface_on_the_[0-9]+_column_census`, and prints
the same three-line stand-down notice the yellow alarm prints ("keyed to a
count that does not exist until the census does; DEFERRED, not discharged —
the merge will demand it").

A pattern matching no roster line is a no-op, and that is the safe
direction: the witness then runs, reds, and the delivery is refused loudly.
The test asserts both the stand-down and the control (the witness IS
selected without the escape), copying `test-census-guard.sh`'s existing pair.

The witness's own comment says it "is not in the sub-floor tier a local gate
runs". The roster and The Warp's log both say otherwise. The comment is
corrected, and its re-statement note gains a line: the census delivery
defers this witness to the merge; re-state it on the branch that merges.

### 3.5 Prose brought current

`scripts/sluice-census.sh` header; `scripts/gnomon-injection.sh` header
(the `ssh lefford … git checkout <sha> && scripts/gnomon-injection.sh` line
is still the by-hand path, now the exception); the fixtures' `README.md`
("Regenerate with the script, and only with the script" — the delivery is
the script's ordinary caller now); `windows/lab/CLAUDE.md` (the "still owed
a re-authoring … by hand" paragraph); `scripts/CLAUDE.md`; root `CLAUDE.md`'s
census block (one paragraph: what a delivery now carries and what it
defers); `anomaly_injection.rs`'s branch table on the fixture-columns test
("Task 7 carries this" → the delivery carries it).

## 4. Testing

Every test below is a shell test in a file `scripts/lane-outboard.sh`
already runs (or is added to it), driving a scratch git repo — the pattern
`test-sluice-census.sh` and `test-sluice.sh`'s census block set. No test
builds Rust or runs a census.

1. **`scripts/test-gnomon-injection.sh` (new).** In a scratch repo with a
   fake `domains/x.rs`, `book/y.csv`, `docs/timings.md` and the fixture dir:
   `check` passes on a clean tree; passes with `book/` and `docs/` dirty
   (modified AND untracked-new); refuses with `domains/x.rs` modified;
   refuses with a staged new file under `scripts/`; ignores dirt inside the
   fixture dir. The host guard is exercised by running on this Mac without
   `HV_GNOMON_PILOT` (refuses, names the host) and with it (proceeds to the
   tree guard). Registered in `lane-outboard.sh`.
2. **`scripts/test-sluice.sh`, census block.** The stub census tree gains a
   stub `scripts/gnomon-injection.sh` that answers `check` per a mode file,
   records its invocation, writes a marker fixture, and — where `flock`
   exists — records whether `flock -n` on `HV_CENSUS_LOCK` FAILED while it
   ran (the positive control that the delivery held the lock; SKIP printed
   where `flock` is absent). Arms: (a) goldens moved → the stub ran, the
   delivered branch carries the marker fixture, the commit message names
   the arms, the lock was held; (b) null census with matching columns → the
   stub did NOT run, the log says arms unchanged; (c) null census whose arm
   schema lacks a census column → the stub ran (the column trigger alone);
   (d) stub exits 1 from authoring → rc=4, no branch pushed, the log names
   the re-authoring, the goldens remain staged in the worktree; (e) `check`
   refuses → rc=4, no branch, the log carries the refusal text, and the stub
   records that authoring was never entered.
3. **`scripts/test-sluice-census.sh`.** The column-set extractor over two
   synthetic schema files: identical → no difference; a column added to the
   census → reported by name; a column only in the arm → reported; the
   study-name line is ignored on both sides (the anti-vacuity half — a
   fixture whose study name differs but whose columns match reads
   "identical").
4. **`scripts/test-census-guard.sh`.** The gate-commit path cannot be
   executed cheaply (it builds and runs the roster), so this is a two-way
   agreement test rather than an execution: it extracts the
   `HV_SUBFLOOR_EXCLUDE` pattern from `pre-commit`'s `HV_CENSUS_DELIVERY`
   block by text (so the test cannot drift from the hook), runs
   `scripts/subfloor-roster.sh` against the committed roster with that
   pattern set and unset, and asserts the difference is exactly one
   `test(=…)` term whose name matches the pattern. It also pins the number
   of `HV_CENSUS_DELIVERY` stand-down branches in the hook at three, so a
   fourth is added by editing the test's expectation and its reason.
5. **`scripts/subfloor-roster.sh`.** An exclusion that matches nothing
   leaves the output byte-identical (asserted by `cmp`).

**The one command that would embarrass this campaign** is the real thing: a
`make sluice-census` of a ref that grows the registry. No such ref exists on
`main` today and manufacturing one is out of scope; what the campaign CAN
run is a census of its own tip (columns unchanged, goldens possibly moved by
nothing) to prove the null path and the timings row in production, and it
does, at the pre-merge close.

**CORRECTED AT CLOSE.** It does not, and the sentence above overstated what
running the campaign's own tip through the queue can prove. The census that
ran (queue row `req-81968faee96c-20260906T144636Z`, `census-run.sh` rc=0 in
1272 s, NO GOLDENS MOVED, delivering `census/81968faee96c-20260906T155128Z`
with the timings row only) proved **main's** null path — dead until this
campaign, live now that the delivery ships — and the `docs/timings.md` row.
It proved nothing about the arms step, because `scripts/sluice-drain.sh`
dispatches `scripts/sluice-census.sh` relative to its own repo root (the
queue operator's main checkout on lefford, which read `b71296a8a` at the
time and had zero occurrences of "Gnomon" in that file), and only
`scripts/gnomon-injection.sh` is read from the censused worktree (§3.2 step
2). The delivery log carries no arms-verdict line at all. See §6 criterion 2
for the corrected success criterion and the two-step production proof that
replaces this paragraph's claim.

## 5. Migration and the refs it cannot help

Nothing migrates. The first ref to benefit is any ref that carries this
campaign's `gnomon-injection.sh`; every campaign branch absorbs `main` at
each stage boundary, so after this merges every live branch carries it
within one absorption.

A census of a ref that PREDATES the merge runs that ref's old
`gnomon-injection.sh`, whose whole-tree guard refuses the delivery's dirt.
The delivery detects this at the `check` pre-flight and exits 4 with the
goldens in place and a message naming the cause; recovery is The Warp's
by-hand path. This is bounded and self-expiring, and it is the reason the
queue's copy of the script is NOT used against an older tree (§3.2 step 2).

**Caveat.** On such a predating ref, the old script has no `check`
subcommand at all — `check` is parsed as an ARM NAME, and the old script
clears the fixture directory (`rm -rf`s the arm subdirectories, removes
`manifest.json`) before it fails to match "check" against `ARMS` and prints
"unknown arm". This is unreachable in production: that ref's whole-tree
guard runs first and already refuses on the delivery's own staged goldens,
so the arm-name branch is never entered. And even if it were, the cleared
fixture directory self-heals on the next `reset --hard` of the census
worktree, since nothing was committed.

## 6. Success criteria

1. `scripts/test-gnomon-injection.sh`, the census block of
   `scripts/test-sluice.sh`, `scripts/test-sluice-census.sh` and
   `scripts/test-census-guard.sh` pass with every arm in §4 present, and
   `make shellcheck` is clean.
2. A `make sluice-census` of this campaign's own tip at pre-merge close
   delivers a branch whose log contains the arms verdict line and whose
   `docs/timings.md` gained a `gnomon-injection` row if the arms were
   re-authored, or the "arms unchanged" line if not.

   **CORRECTED AT CLOSE: this criterion was unsatisfiable by construction,
   and the census that ran (`81968faee`) did not satisfy it.**
   `scripts/sluice-drain.sh` dispatches `scripts/sluice-census.sh` — and
   through it `census-run.sh` and the whole delivery — relative to **its
   own repo root**, the queue operator's main checkout on lefford; only
   `scripts/gnomon-injection.sh` is taken from the censused ref (§3.2 step
   2). A campaign that changes the delivery script can therefore never
   exercise its own change through the queue before that change is on
   `main` — the queue always runs main's copy of `sluice-census.sh`, never
   the branch tip's. Queue row `req-81968faee96c-20260906T144636Z`,
   `census-run.sh` rc=0 in 1272 s, NO GOLDENS MOVED, delivered
   `census/81968faee96c-20260906T155128Z` (timings row only, merged into
   this branch at `c4595ba81`); its log carries no Gnomon-arms line at
   all, because the queue's checkout of `scripts/sluice-census.sh` at that
   moment (`b71296a8a`) had zero occurrences of "Gnomon". That run proved
   main's null path (dead until this campaign, live now that the delivery
   ships) and the `docs/timings.md` row; it proved nothing about the arms
   step. The real production proof is two-step, both necessarily after
   this campaign merges: (a) the first census the queue runs once
   lefford's main checkout has advanced past the merge, expected to report
   `Gnomon arms unchanged` on a null; and (b) the first census by a
   campaign that registers a metric, expected to report `re-authoring the
   Gnomon injection arms` and a `gnomon-injection` timings row. Pre-merge,
   the only evidence for the arms step is the harness on lefford
   (`scripts/test-sluice.sh`, 263/263), which stubs the census.
3. `HV_CENSUS_DELIVERY=1` stands down exactly three checks in `pre-commit`
   — the two it stood down before and the count witness — and
   `test-census-guard.sh` pins that count.
4. The registry row reads `shipped`; decision 0836 is in force; the prose
   in §3.5 no longer describes a by-hand re-authoring as the ordinary path.

## 7. Non-goals

- Re-pinning the count witness or the recall witness by machine. Both
  demand a human's re-statement and the merge demands it of the owning
  campaign.
- Writing a claim file from the delivery (§3.2 step 3).
- Any change to `census-run.sh`, the census study, the injection study, the
  arms table, or the recall bar.
- A registry of fixtures frozen against the metric registry. One family
  exists; the rule in §3.1 says where a second would go.

## 8. Decision to mint

**0836 — a census delivery regenerates the evidence its gate reads and
defers only what needs a human re-statement.** Amends the `HV_CENSUS_DELIVERY`
opt-out from a list of two checks to a rule with four instances; records
that the Gnomon arms are authored by the delivery at the census's ref
(manifest `sha` = census ref, host = canonical); records that the count
witness is deferred to the merge. Relates: 0079 (one authoring host), 0133
(one serial claim), 0139 (main advances only through the chamber), 0514 (a
census is ordinary queued work).

## 9. Risks

| risk | why it is bounded |
| --- | --- |
| the re-authoring adds ~8 min to a moving census | only on a moving census; ledgered, so the number is read from `docs/timings.md` not from this file |
| the flock is held with no claim file | status readers blind for those minutes; every flock-taker still waits; stated in §3.2 |
| an old ref cannot self-deliver | detected at pre-flight, refused loudly, expires as branches absorb main (§5) |
| the exclusion pattern rots when the witness is renamed | rot fails SAFE: the witness runs and reds the delivery; `test-census-guard.sh` pins the omitted count at 1 against the committed roster, so a rename reds the outboard set first |
| the column extractor's indentation rule | both files are written by the same serde pretty-printer at the same ref; a mis-parse can only cause a needless re-author, never a missed one, because a real column difference is always a `"name"` line difference. **CORRECTED AT CLOSE:** the rule as drafted here (`indent >= 4`) did not discriminate at all — it caught the study's own `"name"` line at indent 4 as well as every column's at indent 6, so every arm compared stale by construction. The fail-safe argument in this row held (a needless re-author, never a missed one, exactly as stated) even though its premise did not; fixed to indent-exactly-6 at close (`81968faee`) |
