# The Attestation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make an absent action visible, by declaring what should be present and
diffing the ledger that already records what happened.

**Architecture:** No new record. `docs/timings.md` already carries one row per
phase per chamber job. This plan adds the two **declarations** that make an
absence a diff — the roster's rungs agreeing with the chamber's phase lists, and
an author column on every declared generated path — plus a reader that diffs
them **in both directions**.

**Tech Stack:** Rust 2024 (workspace tests in `cli/tests/suite/`), POSIX `sh`
(dash on the canonical box), `cargo nextest`. No new dependency.

**Spec:** `docs/superpowers/specs/2026-08-29-the-attestation-design.md`

## Global Constraints

- **No new workspace dependency.** `serde`, `serde_json`, `libm` only
  (decision 0004). The canonical box's `/bin/sh` is **dash** — no bashisms in
  anything `sluice-run.sh` sources, and no TSV parser beyond `awk`/`cut`.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, enforced by
  `clippy.toml`.
- **Every crate sets `#![warn(missing_docs)]`**; every public item gets a
  one-line doc comment.
- **Every check states the direction it enforces, in its own doc comment**, and
  what it is blind to. This is house style in both files being edited
  (`lane_sets.rs` and `generated_paths.rs` each do it) and it is also this
  campaign's subject matter. A check added here without a stated direction is a
  defect, not a style miss.
- **`cargo fmt --all` last, then `make gate-commit`,** before every commit.
- **Run a test set ONCE and inspect many.** Capture to a file, then grep it;
  never re-run a suite to read a second line.
- **No committed byte may move** except `docs/generated-paths.txt` (gains a
  column) and whatever Task 5 deliberately deletes.
- **Push at every task boundary.** An unpushed branch is invisible to the mouth,
  the chamber, and every peer.

## What this plan already got wrong three times — read before Task 1

The spec was corrected **three times while this plan was being written**, each
time by running a command against a claim the spec asserted:

1. §4's "add a check that every declared path is written by its author" — that
   check **already exists** (`generated_paths.rs:243`). It matches the path as a
   literal substring of the script, which is why a directory passes while 814
   files beneath it are never written.
2. The positive control cannot live in a unit test (128.720 s measured), so it
   belongs in the `artifacts` phase, which already runs the author.
3. **§3's headline approach was wrong.** Deriving the phase lists from the
   roster would have silently reordered every merge, because roster order
   (`gate artifacts outboard`) and phase order (`artifacts outboard gate`) are
   transposed and phase order is load-bearing.

None of the three was found by re-reading. Each died to one command. So: **if a
step tells you something about this repository, check it before building on it,
and report in your task summary what you checked and what it returned.**
Overriding this plan and saying so is expected behaviour, not an escalation.

## File structure

| file | responsibility |
|---|---|
| `cli/tests/suite/lane_sets.rs` | MODIFY — gains the two-way set-agreement check and a shared phase-list scraper |
| `docs/generated-paths.txt` | MODIFY — gains an `author` column |
| `cli/tests/suite/generated_paths.rs` | MODIFY — per-author proxy; every path names a known author |
| `scripts/regenerate-artifacts.sh` | MODIFY — record which declared files a run wrote |
| `cli/src/attest.rs` | CREATE — the reader: declarations vs. ledger, both directions |
| `cli/tests/suite/attest.rs` | CREATE — the reader's tests |
| `cli/tests/suite/decision_blocks.rs` | CREATE — a record sits inside its author's reserved block |

---

### Task 1: Classify the unaccounted files (settles H1) — COMPLETE: 132, not 585

No production code. This task produces the fact every later task depends on, and
the spec explicitly refuses to assume it.

**Files:**
- Create: `docs/generated-path-authors.md` (the classification, committed)

  **Not `docs/audits/`** — that directory is itself a declared generated path
  (`docs/generated-paths.txt:50`). A classification *of* generated paths must
  not live *inside* one, or this task manufactures a fresh instance of the
  defect Task 5 deletes, and Task 5's list has to include a file this campaign
  just wrote. `docs/` root already holds `timings.md`, `generated-paths.txt`
  and `README.md`.

- [ ] **Step 1: Reproduce the measurement.** From the repo root, on a clean tree:

```bash
marker=$(mktemp); sleep 1
make rebaseline 2>&1 | tail -5
for p in $(grep -v '^#' docs/generated-paths.txt | grep -v '^$'); do
  tot=$(git ls-files "$p" | wc -l | tr -d ' ')
  wrote=$(git ls-files "$p" | while read -r f; do [ "$f" -nt "$marker" ] && echo x; done | wc -l | tr -d ' ')
  printf '%-46s %s/%s\n' "$p" "$wrote" "$tot"
done
```

Expected shape (measured 2026-08-29 at `26db4df99`): **98 of 941 written**, with
`book/src/laboratory/` at 11/825. Report the numbers you actually get. **If they
differ materially from 98/941, STOP and say so** — the spec's §1.3 is
load-bearing for Tasks 3-5, and a moved number needs re-deriving rather than
patching.

- [ ] **Step 2: Classify every unwritten file.** For each tracked file under a
  declared path that the run did NOT write, determine which of these it is:

  - authored by the census (`rebaseline` with the census flag set),
  - authored by the heavy tier,
  - authored by some other command,
  - **not generated at all** — hand-written prose.

  `book/src/laboratory/census-of-coasts-ii.md` is a confirmed example of the
  last kind: narrative voice, no generated-file banner, not rewritten by the
  run. Do not classify by filename — open the file, or find the writer.

- [ ] **Step 3: Write `docs/generated-path-authors.md`** — one row per
  declared path, giving total tracked, written-by-rebaseline, and the author of
  the remainder. Where a single declared path holds files with **different**
  authors, say so explicitly: that is the finding, and it decides whether an
  author column can be per-path at all.

- [ ] **Step 4: Report H1.** The spec predicts the unaccounted files fall into
  a small number of authors. **Task 1 measured 132, not the spec's 585** — the
  spec's arithmetic missed a second census-gated directory of 455 files. Tasks
  3 and 5 use 132. **If they fall into many, or into "no author" in bulk, STOP and
  re-present** — that is the preregistered null, and it turns Task 5 from a
  tidy-up into a scope question for Nathan.

- [ ] **Step 5: `cargo fmt --all`, `make gate-commit`, commit, push.**

---

### Task 2: The roster's rungs and the chamber's lists agree, both ways

**Files:**
- Modify: `cli/tests/suite/lane_sets.rs`

**Interfaces:**
- Produces: `fn chamber_phase_lists() -> Vec<(String, Vec<String>)>` — the
  scraper, extracted from the existing test so both tests read one
  implementation. Returns `[("merge_phases", [...]), ("stage_phases", [...])]`.
- Consumes: the existing `fn roster() -> Vec<(String, String, String, String, String)>`,
  whose five fields are **`(name, gate, where, authors, command)`** — the
  header's own words at `scripts/lane-sets.tsv:51`. The file's prose calls the
  *values* of field 2 "rungs", so both words are live for that column; field 4
  is `authors` (yes/no), not `writes`.

- [ ] **Step 1: Extract the scraper.** The existing
  `every_phase_the_chamber_runs_is_rostered` scrapes the two lists inline. Move
  that scraping into `chamber_phase_lists()` verbatim — including its
  `assert_eq!(lists.len(), 2, …)` guard, which exists so a rename cannot leave
  the check asserting nothing — and have the existing test call it. **This is a
  pure refactor and must move no behaviour**; the existing test must still pass
  unchanged.

- [ ] **Step 2: Write the failing test**

```rust
/// The chamber's two phase lists and the roster's `rung` column agree, as
/// SETS, in both directions.
///
/// # Direction this check enforces
///
/// Both. Every `stage`-rung set appears in `stage_phases`; every `stage`- or
/// `merge`-rung set appears in `merge_phases`; and neither list carries a set
/// the rungs do not imply. The sibling check beside this one asserts only that
/// each listed token HAS a roster row, which is satisfied by a set sitting in
/// the WRONG list — and that is exactly how decisions 0148 and 0426 moved
/// `heavy` between the lists, in both directions, with nothing objecting.
///
/// # What it deliberately does NOT assert
///
/// **Order.** The roster is ordered by rung and the chamber by sequence, and
/// the two are transposed (`gate artifacts outboard` against `artifacts
/// outboard gate`). Phase order is load-bearing — `artifacts` regenerates and
/// commits, so `gate` must not precede it — and the roster does not encode it.
/// The script owns sequence; the roster owns membership; this asserts only the
/// membership both files actually claim.
///
/// `integration` is excluded: it is the `merge`-rung row describing
/// `sluice-run.sh` itself, so it can never be one of its own phases.
#[test]
fn the_phase_lists_and_the_roster_rungs_agree_both_ways() {
    use std::collections::BTreeSet;

    let roster = roster();
    let implied = |rungs: &[&str]| -> BTreeSet<String> {
        roster
            .iter()
            .filter(|r| r.0 != "integration")
            .filter(|r| rungs.contains(&r.1.as_str()))
            .map(|r| r.0.clone())
            .collect()
    };

    let expected: Vec<(&str, BTreeSet<String>)> = vec![
        ("stage_phases", implied(&["stage"])),
        ("merge_phases", implied(&["stage", "merge"])),
    ];

    for (key, want) in expected {
        let got: BTreeSet<String> = chamber_phase_lists()
            .into_iter()
            .find(|(k, _)| k == key)
            .unwrap_or_else(|| panic!("{key} not found in scripts/sluice-run.sh"))
            .1
            .into_iter()
            .collect();

        let missing: Vec<&String> = want.difference(&got).collect();
        let extra: Vec<&String> = got.difference(&want).collect();
        assert!(
            missing.is_empty() && extra.is_empty(),
            "{key} disagrees with scripts/lane-sets.tsv's `rung` column.\n  \
             in the roster's rungs but not in the list: {missing:?}\n  \
             in the list but not implied by any rung: {extra:?}\n\
             One of the two files is wrong. Decide which — moving a set between \
             rungs is a decision (0148 and 0426 each did it), and editing the \
             list without the rung is how that decision goes unrecorded."
        );
    }
}
```

- [ ] **Step 3: Prove the test discriminates.** A test that passes on today's
  green tree tells you nothing. Do BOTH mutations, one at a time, restoring in
  between:

  1. In `scripts/lane-sets.tsv`, change `heavy`'s rung from `merge` to
     `campaign`. **Expected: FAIL**, naming `heavy` as "in the list but not
     implied by any rung".
  2. In `scripts/sluice-run.sh`, delete `heavy` from `merge_phases`.
     **Expected: FAIL**, naming `heavy` as "in the roster's rungs but not in the
     list".

  **Report both failure messages verbatim in your task summary.** Mutation 2
  reproduces the real historical defect; if it does not fail, the test asserts
  nothing and the task is not done. Restore each file with
  `git checkout -- <file>` and confirm `git status` is clean before continuing.

- [ ] **Step 3b: Prove the silent breakage, then fix it.** Before repairing
  `scripts/scheduled/nightly-drift.sh`, demonstrate the failure it would have
  had: with the two-column file in place and the script unrepaired, show that
  its `git diff --exit-code` construction exits 0 while a real drift exists.
  **Paste that output.** Then add `cut -f1` and show the same probe now
  detects the drift. This is the campaign's thesis executed on the campaign's
  own migration, and it is the one step of this task that cannot be replaced
  by reading.

- [ ] **Step 4: Run the set once and inspect it**

```bash
cargo test -p hornvale --test suite -- lane_sets > /tmp/hv-lane.log 2>&1; echo "exit=$?"
grep -E '^test result|FAILED|panicked' /tmp/hv-lane.log
```

Expected: every check in the file passes, including the pre-existing ones.

- [ ] **Step 5: `cargo fmt --all`, `make gate-commit`, commit, push.**

---

### Task 3: Every declared generated path names its author

**Files — EIGHT, not two. The plan said two and was wrong; I named every
reader before dispatching and this is the corrected list.**
- Modify: `docs/generated-paths.txt` — gains the column
- Modify: `cli/tests/suite/generated_paths.rs` — the parser
- Modify: `scripts/scheduled/nightly-drift.sh:82` — **a real parser that
  word-splits the file**
- Modify the drift command in each of the five guides that show it:
  `CLAUDE.md:636`, `cli/CLAUDE.md:174`, `scripts/CLAUDE.md:54`,
  `windows/CLAUDE.md:129`, `domains/terrain/CLAUDE.md:50`
- Modify: `docs/generated-path-authors.md:17` — Task 1's own file shows the
  one-column loop too

**WHY THIS LIST EXISTS — and the controller's first answer here was WRONG.**
I claimed the breakage would be silent: that `nightly-drift.sh`'s
`git diff --exit-code -- $paths` would exit 0 forever. **Task 3's Step 3b
probe refuted it, and I then reproduced the refutation myself.** Unquoted
`$(...)` word-splits on the embedded tab (it is in the default `IFS`), so
`path<TAB>author` becomes *two* tokens, the bogus one is a pathspec matching
nothing, and `git diff` unions its pathspecs — exit 1, correctly:

```
  word-split of "real/<TAB>artifacts"  ->  [real/] [artifacts]
  git diff --exit-code -- real/ artifacts   ->  exit=1   (correct)
  git diff --exit-code -- artifacts         ->  exit=0   (matches nothing)
```

Only the last line was true, and it never bites, because word-splitting never
LOSES the real path.

**The eight-file list stands on weaker but sufficient grounds:** the Rust
reader would have broken loudly (every row red on the tracked-ness check), the
bash behaviour is *accidentally* safe rather than guaranteed — a quoted
`"$paths"` or a changed `IFS` breaks it — and a reader of a two-column file
should read the column it means. Step 3b stays, because a probe that refutes
the plan is worth more than one that confirms it.

**Interfaces:**
- Produces: `fn declared() -> Vec<(String, String)>` — `(path, author)`,
  replacing the existing `declared_paths() -> Vec<String>`. Update every
  existing test in that file which calls it.
- Every shell reader takes the path from field 1: `cut -f1`.

- [ ] **Step 0 (BLOCKING): find each author's writing site.** The static check
  needs, per author, the source file that writes its paths. `rebaseline` is
  `scripts/regenerate-artifacts.sh`. **`heavy` and `census` you must discover** —
  do not take a path from this plan, which is written from outside the code.
  Record what you found and how you confirmed it. If an author's writing site
  cannot be identified, say so: that is a finding about that author, and Task 5
  needs it.

- [ ] **Step 1: Add the column.** `docs/generated-paths.txt` becomes
  `path<TAB>author`. Extend the file's header to say the column names an
  **invocation** — the command a person would run to make that file current —
  not a program: `regenerate-artifacts.sh` authors both the ordinary artifacts
  and the census goldens, distinguished only by a flag, so a column naming the
  script would collapse exactly the distinction this campaign draws. Fill all ten
  rows from Task 1's classification.

  **If Task 1 found an author beyond the three below, extend `KNOWN` in Step 2
  to match and say so in your summary.** The list in this plan is what the
  measurement of 2026-08-29 implies; Task 1 is what actually decides it, and a
  plan that hardcodes a set its own predecessor task computes is exactly the
  two-sources-of-truth defect this campaign is about.

- [ ] **Step 2: Write the failing test**

```rust
/// Every declared path names an author, and the author is one we know.
///
/// # Direction this check enforces
///
/// Declared-implies-attributed. Blind to a generated path nobody declared —
/// the same blindness `every_declared_generated_path_is_tracked` documents,
/// and for the same reason: nothing enumerates this repository's generated
/// output independently of this file.
#[test]
fn every_declared_path_names_a_known_author() {
    // The roster's own set names, NOT invented labels: an author name IS a
    // roster set name IS the suffix of a `sluice:<set>` ledger label.
    const KNOWN: &[&str] = &["artifacts", "census", "heavy"];
    let bad: Vec<String> = declared()
        .into_iter()
        .filter(|(_, author)| !KNOWN.contains(&author.as_str()))
        .map(|(p, a)| format!("{p} -> {a:?}"))
        .collect();
    assert!(
        bad.is_empty(),
        "docs/generated-paths.txt rows whose author is missing or unknown \
         (known: {KNOWN:?}). A declared generated path with no author is a \
         claim about this repository that nothing can check:\n  {}",
        bad.join("\n  ")
    );
}
```

- [ ] **Step 3: Re-point the existing writer check per author.** The check at
  `generated_paths.rs:243` matches every declared path against
  `scripts/regenerate-artifacts.sh`. Change it to match against **the source of
  the author that path names** (from Step 0). Keep its existing statement of
  what substring matching cannot see, and **add the direction this campaign
  found**: it is satisfied at directory granularity while files beneath are
  unwritten — a false negative, which fails *unsafe*, unlike the false positive
  the header already anticipates.

- [ ] **Step 4: Run the set once and inspect it**

```bash
cargo test -p hornvale --test suite -- generated_paths > /tmp/hv-gp.log 2>&1; echo "exit=$?"
grep -E '^test result|FAILED|panicked' /tmp/hv-gp.log
```

If a path's author cannot satisfy even the substring proxy, that is a Task 5
input — record it, and **do not weaken the test to accommodate it**.

- [ ] **Step 5: `cargo fmt --all`, `make gate-commit`, commit, push.**

---

### Task 4: The artifacts phase records what it wrote

**Files:**
- Modify: `scripts/regenerate-artifacts.sh`

- [ ] **Step 1: Capture the write set.** At the start of a run, stamp a marker;
  at the end, emit one line per declared path giving how many of its tracked
  files that run wrote. This is Task 1 Step 1's measurement made a byproduct of
  a run that already happens. It must be **dash-safe** and must not change any
  artifact's bytes.

  **`docs/generated-paths.txt` is TWO COLUMNS by the time this task runs** —
  Task 3 lands the author column first. Take the path from field 1 only
  (`cut -f1`). A one-column reader would treat `path<TAB>author` as a single
  pathspec, match nothing, and report every declared path as zero-written: a
  plausible all-zeros result, which is exactly the silent-wrong-number failure
  this campaign exists to catch. Nothing reads this file from the script today
  (verified: `grep -n generated-paths scripts/regenerate-artifacts.sh` returns
  only a comment at line 207), so you are adding the reader, not amending one.

- [ ] **Step 2: Prove it changes no committed byte**

```bash
make rebaseline && git status --porcelain
```

Expected: only `docs/timings.md` (its own ledger row). **Anything else moving is
a defect in this task**, not an artifact update — this task adds an observation,
not a regeneration.

- [ ] **Step 3: Prove the capture is honest.** In a scratch copy of the script,
  delete one declared path's generation, re-run, and confirm the emitted count
  for that path drops to zero. Restore. **A capture that reports a plausible
  number without measuring anything is the exact failure this campaign exists to
  find** — report the before and after counts.

- [ ] **Step 4: `make gate-commit`, commit, push.**

---

### Task 5: A path with no author is a declaration error (G3-FLAGGED)

**Files:**
- Modify: `docs/generated-paths.txt`
- Modify: `cli/tests/suite/generated_paths.rs`

**This task's outcome is deletions, and it is the item flagged for Nathan at
G3.** Removing a declaration is the correct response to a false claim AND it
mechanically narrows what the drift check covers.

- [ ] **Step 1: Produce the list, do not act on it.** From Task 1's
  classification, list every declared path containing files that no author
  writes — with, for each, the count and a named example. `book/src/laboratory/`
  is known to be one (`census-of-coasts-ii.md`).

- [ ] **Step 2: STOP. Present the list and wait.** Post it for Nathan with, for
  each path, three options: narrow the declaration to the generated subtree,
  remove the declaration entirely, or keep it and accept the check is partial.
  **Do not choose.** The plan does not get to assume this answer.

- [ ] **Step 3: Implement the decision**, whatever it is, and make the check
  enforce it so the same drift cannot re-accrete.

- [ ] **Step 4: `make gate-commit`, commit, push.**

---

### Task 6: The reader — declarations against the ledger, both directions

**Files:**
- Create: `cli/src/attest.rs`
- Create: `cli/tests/suite/attest.rs`
- Modify: `cli/src/main.rs` (subcommand wiring)

**Interfaces:**
- Consumes: `docs/timings.md` rows, whose columns are
  `when (UTC) | label | wall_s | user_s | sys_s | cpu_ratio | waited_s | commit | branch | host | cores`.
- Produces: `pub fn attest_report(timings: &str, roster: &str, declared: &str) -> Report`
  — pure over its three inputs, so the tests need no filesystem.

- [ ] **Step 1: Write the failing tests.** Cover, at minimum:

  - a job whose rows are missing an owed phase is reported (**owed but absent**);
  - a job carrying a row for a phase no rung implies is reported (**present but
    unowed**) — the mirror direction, which the spec's first draft omitted;
  - an author with no row in the window is reported, with the date of its last;
  - a well-formed tree reports nothing.

  Each test builds its inputs as string literals. **The both-directions pair is
  the point of this task**: a reader that only reported absences would be a
  one-sided check, which is the defect Task 2 exists to fix.

- [ ] **Step 2: Run to verify they fail.** Expected: FAIL, `attest_report` does
  not exist.

- [ ] **Step 3: Implement.** Two properties of the ledger must appear in
  `attest.rs`'s module doc, because both bound what the reader may claim:

  - **A row has no exit code.** The columns carry no `rc`. A row witnesses that
    a phase RAN, never that it passed; the outcome lives in the queue's own
    state. The reader must not imply otherwise.
  - **A job is identified by its rows' adjacency, not by a job id** — the ledger
    carries no job column. State how you group rows into jobs and what that
    grouping cannot distinguish.

- [ ] **Step 4: Run the tests.** Expected: PASS.

- [ ] **Step 5: Run it against the real committed ledger** and record the output
  in your task summary. **This settles H3.** A null — it finds only the two
  absences already known — is a result: report it as one. **Do not tune the
  reader until it finds something.**

- [ ] **Step 6: `cargo fmt --all`, `make gate-commit`, commit, push.**

---

### Task 7: A decision record sits inside its author's reserved block

**Files:**
- Create: `cli/tests/suite/decision_blocks.rs`

- [ ] **Step 0 (BLOCKING): can a checkout read the block ledger?** The
  reservations live on the canonical box (`scripts/decision-block.sh list`, via
  `make decision-blocks`, over ssh). A workspace test **must not ssh**. Find out
  whether that ledger is, or can be, committed. **If it cannot be, STOP and
  report** — the check is then not writable as specced, and saying so is the
  correct outcome. Spec §5a names this as the one design question it carries
  into the plan rather than settling.

- [ ] **Step 1: Write the failing test**, given a readable ledger: every record
  in `docs/decisions/` whose number falls inside a reserved block belongs to the
  campaign that reserved it. State the direction: it is blind to a record in no
  block at all, which is a different defect — and is the one The Overture had.

- [ ] **Step 2: Prove it discriminates** against a fixture where a record sits
  in another campaign's block — the `the-stride`/`the-burr` shape, `0160` inside
  `0156-0165`. Report the failure message.

- [ ] **Step 3: Implement, run, verify.**

- [ ] **Step 4: `cargo fmt --all`, `make gate-commit`, commit, push.**

---

### Task 8: Artifacts, book, chronicle, retrospective, decisions, registry

**Definition of Done for every merged plan (CLAUDE.md Process, decisions 0013,
0020) — not optional.**

- [ ] **Step 1: `make rebaseline`, then `git status` IMMEDIATELY**, then the
  drift check:

```bash
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)
```

Note the `cut -f1`: the list has two columns now, and the old one-column form of
this command would hand `author` to `git diff` as a pathspec. **Branch table:**
`docs/audits/` and `docs/digest/` moving is EXPECTED. Anything under
`book/src/domesday/`, `book/src/gallery/` or an almanac moving → **STOP**, that
is a determinism finding.

- [ ] **Step 2: Decision records** from block **0456–0465**, per spec §10.
Derive the real list from what shipped — §10 was written before Tasks 1-7 and
some of it will no longer match. Read two recent records for house format.

- [ ] **Step 3: Chronicle** `book/src/chronicle/the-attestation.md`, wired into
`book/src/SUMMARY.md`. **Lead with what was measured.** H1's classification and
H3's result are the headline; if H3 came back null, say so first.

- [ ] **Step 4: Retrospective** `docs/retrospectives/the-attestation.md` plus its
index row. The three spec corrections found while planning belong here.

- [ ] **Step 5: Registry** — a row for seam-guard having a declaration and no
verdict record (spec §6), carrying its measurement.

- [ ] **Step 6: Freshness sweep + Confidence Gradient.** **Grep
`book/src/open-questions.md` before concluding no bet moved.**

- [ ] **Step 7: `make gate-commit`, commit, push.** Do NOT submit to the merge
queue — that is the controller's.
