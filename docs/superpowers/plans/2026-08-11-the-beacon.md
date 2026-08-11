# The Beacon Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the Cairn's board operate across `ambrose` and `lefford` through
`origin`, without letting the cross-host post volume silence the session-start
render it exists to feed.

**Architecture:** Each host keeps writing only its own `refs/hornvale/board`
(one writer per ref, so compare-and-swap stays correct); publishes it to
`refs/hornvale/hosts/<host>` on `origin`; and fetches peers into
`refs/hornvale/peers/<host>`. Reads union those refs and deduplicate by post id,
which is free because an id *is* the content hash. No merge commit, no schema
change, no migration. Judgments never cross hosts: a post read from a peer ref is
judged by TTL alone.

**Tech Stack:** Rust 2024, `tools/board/` (its own workspace root, outside the
cargo workspace), `serde`/`serde_json`, shelling out to `git` plumbing. Tests are
`cargo test --manifest-path tools/board/Cargo.toml` (24.4 s). Shell seams are
POSIX `sh` under `shellcheck`.

## Global Constraints

- **Spec:** `docs/superpowers/specs/2026-08-11-the-beacon-design.md`. Decisions
  cited as **B*n*** are that spec's; **D*n*** are The Cairn's
  (`2026-08-09-the-cairn-design.md`). Where this plan and the spec disagree, the
  spec governs — say so in the task report rather than silently following either.
- **Never reroot `refs/hornvale/board`** (decision 0118 part 3). No operation may
  force-update it to a fresh root, and **no push may use `--force`** (B3).
- **No new dependencies.** `tools/board/Cargo.toml` gets nothing added. The
  workspace allowlist does not reach here, but the project's temperament does.
- **No schema change.** No new *required* post field. New post *kinds* are
  conventions, shipped as a post, not as an enum (D12).
- **The board never blocks and is never authority** (D7/D7c). Every read seam
  stays non-fatal; a corrupt post warns and is skipped; the render exits zero.
- **`board digest` is the exception and must stay loud** — it exits non-zero on
  error where the ambient render exits zero (B12). Preserve the asymmetry.
- **`#![warn(missing_docs)]`** is set; every new public item, field and variant
  gets a one-line doc comment.
- **No wall-clock time** except at the two sanctioned call sites already in this
  crate (`main.rs`'s digest arm and `live.rs`'s probe). This crate lives outside
  the workspace's ban; do not add a third site without saying why.
- **`cargo fmt` as the final step before every commit.** Run it inside
  `tools/board/` (`cargo fmt --manifest-path tools/board/Cargo.toml`); the
  workspace `cargo fmt` does not reach this package — it is not a workspace
  member (`members = ["kernel", "domains/*", "windows/*", "cli"]`).
- **Commit granularity:** one commit per task minimum. Commit messages say
  *why*, and cite the B-number.
- **After any task that changes a registry row or a doc under `docs/`**, run
  `cargo test -p hornvale --test docs_consistency` from the worktree root. Four
  guards bite: five columns counting `\|` as an escape, Idea cell ≤ 600 chars,
  the closed status vocabulary (`raw`/`elaborated`/`spec'd`/`shipped`/
  `ratified`/`rejected`), and a non-empty **Where** cell.

---

## File Structure

**`tools/board/src/git.rs`** — git plumbing. Gains a bytes-returning stdin
helper, because `cat-file --batch` output is size-framed and the existing
`git_stdin` returns a lossily-decoded, trailing-whitespace-trimmed `String`.

**`tools/board/src/store.rs`** — the log. Gains batched object reads, the
multi-ref union read, peer-ref discovery, and `redact`. Keeps `snapshot()` and
`reap()` single-ref, deliberately.

**`tools/board/src/live.rs`** — liveness judgments. Gains the foreign-post branch
(B4) and the corrected notice predicate (B11).

**`tools/board/src/render.rs`** — rendering. Gains the unverifiable-here
vocabulary (B5), peer staleness reporting (B6), and excludes `suggest` from the
ambient view (B12).

**`tools/board/src/digest.rs`** — the human seam. Gains batched reads, redaction
suppression (B8), corroboration tallies and the `suggest` section (B10/B12).

**`tools/board/src/sync.rs`** *(new)* — push own ref, fetch peers, record sync
times. Its own file because it is the only module that touches the network and
the only one whose failures are always non-fatal; that is one responsibility and
it does not belong inside the store.

**`tools/board/src/post.rs`** — post construction. Gains the credential-shape
scan (B9).

**`tools/board/src/main.rs`** — the CLI. Gains `sync` and `redact` subcommands.

**`scripts/board-sync.sh`** *(new)*, **`Makefile`**, **`scripts/hooks/pre-commit`**,
**`scripts/CLAUDE.md`**, root **`CLAUDE.md`** — the seams and the lane rule.

---

## Task 1: Absorb main, and take the before-arm measurement

Main landed decision 0125 while this spec was being written. Absorb it at this
plan-stage boundary per CLAUDE.md, and measure the render *on this host* before
changing anything — The Whetstone's lesson is that a baseline from another host
names the wrong hot spot.

**Files:**
- Modify: `docs/superpowers/specs/2026-08-11-the-beacon-design.md` (link 0125)

**Interfaces:**
- Consumes: nothing.
- Produces: a recorded before-arm timing that Task 3 and Task 12 compare against.

- [ ] **Step 1: Confirm the ancestry gate**

```bash
cd /Users/nathan/Projects/hornvale/.claude/worktrees/the-beacon
make preflight
```

Expected: an ancestry **NO-GO**, because main has moved to `e4109d81` (0125)
since this branch was cut. That is the expected result, not a problem.

- [ ] **Step 2: Merge main INTO the branch and re-run the gate here**

```bash
git merge --no-edit origin/main
make preflight
```

Expected: GO. If the merge conflicts, stop and report — do not resolve a
conflict in `book/src/frontier/idea-registry.md` by hand without checking whether
`merge=union` should have handled it.

- [ ] **Step 3: Restore the 0125 link now that the record exists**

The spec deliberately cites 0125 without a link because the record was
uncommitted when the spec was written. Verify it now resolves, then link it.

```bash
ls docs/decisions/0125-*.md
```

Then in the spec, replace the sentence `Decision **0125** (GitHub Actions is
retired) records the same visibility change` so that `**0125**` becomes
`[0125](../../decisions/0125-github-actions-is-retired.md)`, and delete the
clause `and is *in flight rather than landed*: at the time this spec was written
its record existed only as staged, uncommitted work in `main`'s checkout, so it
is deliberately cited without a link and gets one at the first absorption.`
— replacing it with `.` after `visibility change from the CI side`.

- [ ] **Step 4: Measure the before-arm, five runs, and record it**

```bash
cargo build --release --manifest-path tools/board/Cargo.toml
for i in 1 2 3 4 5; do /usr/bin/time -p bash scripts/board-render.sh >/dev/null; done 2>&1 | grep real
git ls-tree --name-only refs/hornvale/board posts/ | wc -l
hostname -s
```

Record all five numbers, the post count, and the hostname in the task report.
The spec's §2 fact 5 recorded 1.43/1.86/1.52 s at 26 posts on `ambrose`; the
count has since grown, so expect worse. **Do not proceed to Task 3 without this
number** — it is the only evidence Task 3 improved anything.

- [ ] **Step 5: Verify the docs gate and commit**

```bash
cargo test -p hornvale --test docs_consistency
git add docs/superpowers/specs/2026-08-11-the-beacon-design.md
git commit -m "docs(the-beacon): absorb main, link 0125 now that the record has landed"
```

---

## Task 2: Notice liveness — main is unconditionally live (B11)

**This task has a deadline.** Main-authored notices currently test as *merged*,
and `reap` drops merged notices regardless of age, so a `board reap` before this
lands permanently deletes them — including one that says main is red on a
heavy-tier calibration. Do this task first among the code tasks, and do not run
`board reap` until it is green.

**Files:**
- Modify: `tools/board/src/live.rs`
- Test: `tools/board/src/live.rs` (in-module `#[cfg(test)]`, the crate's pattern)

**Interfaces:**
- Consumes: nothing.
- Produces: no signature change. The behaviour change is internal to the notice
  branch of liveness judgment.

- [ ] **Step 1: Read the predicate before touching it**

Read `tools/board/src/live.rs` in full, and in particular find where a notice's
author branch is classified — the code that populates `live_branches` and
`merged_branches`, and the `merge-base --is-ancestor` call behind it. **Name the
exact function and line range in your task report before editing.** The three
discriminators available to you are: the branch is `main`; the branch has a live
worktree; and the branch's ahead/behind counts. Pick after reading; the spec
states the property, not the mutation.

- [ ] **Step 2: Write three failing tests, one per author class**

Follow the existing test style in `live.rs` (the `a_claim_from_another_host_is_not_judged_by_our_process_table`
test is the closest model). The three arms:

```rust
#[test]
fn a_notice_authored_by_main_is_live_because_main_is_never_superseded() {
    // main is trivially its own ancestor, so an ancestry-derived predicate
    // classifies it as merged and filters every notice main ever posts --
    // including, when this was found, one reporting main red on a heavy-tier
    // calibration. B11.
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let post = Post::new("notice", "main").with("note", json!("main is red"));
    let stored = stored_notice(&post, /* committed_at */ 0);
    let ctx = LiveContext::probe(&repo, std::slice::from_ref(&stored)).expect("probe");
    assert!(
        matches!(liveness(&stored, &ctx), Liveness::Live),
        "a notice from main must render; it is the default author"
    );
}

#[test]
fn a_notice_from_a_branch_with_no_commits_of_its_own_is_live() {
    // A fresh campaign branch's tip EQUALS main, so it tests as merged and the
    // post announcing a campaign's start is swallowed. Self-heals on the first
    // commit -- measured going 0 -> 1 mid-session. B11.
    let (_dir, repo) = crate::git::test_support::temp_repo();
    repo.git(&["checkout", "-b", "campaign/newborn"]).expect("branch");
    let post = Post::new("notice", "campaign/newborn").with("note", json!("starting"));
    let stored = stored_notice(&post, 0);
    let ctx = LiveContext::probe(&repo, std::slice::from_ref(&stored)).expect("probe");
    assert!(matches!(liveness(&stored, &ctx), Liveness::Live));
}

#[test]
fn a_notice_from_a_genuinely_merged_branch_still_stops_rendering() {
    // The arm that keeps the fix honest: D9's decay must still work, or this
    // is not a fix, it is a removal.
    let (_dir, repo) = crate::git::test_support::temp_repo();
    repo.git(&["checkout", "-b", "campaign/done"]).expect("branch");
    // one real commit, so the branch is ahead...
    commit_empty(&repo, "work");
    repo.git(&["checkout", "main"]).expect("back");
    merge_branch_into_main(&repo, "campaign/done");
    commit_empty(&repo, "main moves on");   // main now ahead of the branch
    let post = Post::new("notice", "campaign/done").with("note", json!("stale"));
    let stored = stored_notice(&post, 0);
    let ctx = LiveContext::probe(&repo, std::slice::from_ref(&stored)).expect("probe");
    assert!(
        !matches!(liveness(&stored, &ctx), Liveness::Live),
        "a merged branch's notice must still decay, or D9 is gone"
    );
}
```

`merge_branch_into_main` already exists in `store.rs`'s tests — move it to a
shared test helper or duplicate it locally, whichever the crate's existing
layout prefers; say which you did and why. `stored_notice` and `commit_empty`
are helpers you write; keep them next to the tests.

- [ ] **Step 3: Run the three tests and confirm they fail for the right reason**

```bash
cargo test --manifest-path tools/board/Cargo.toml notice 2>&1 | tail -20
```

Expected: the `main` and `newborn` tests FAIL asserting on `Liveness::Live`; the
merged test PASSES already. **If the merged test fails too, stop** — that means
your helpers are wrong, not the predicate, and fixing the predicate against a
broken harness produces a fix that proves nothing.

- [ ] **Step 4: Change the predicate**

Implement the discriminator you chose in Step 1. Whatever you choose, the failure
direction must be safe: when the classification is uncertain, render. A
just-merged notice lingering an extra day is a cost; a live `hold-off` vanishing
is the defect this board exists to prevent. Document the direction in the
function's doc comment, naming which way it errs — a check whose direction is
unstated reads as total to the next reader.

- [ ] **Step 5: Confirm all three pass, and that nothing else broke**

```bash
cargo test --manifest-path tools/board/Cargo.toml 2>&1 | tail -5
```

Expected: all three new tests pass; the whole suite green.

- [ ] **Step 6: Confirm the real board recovers**

```bash
cargo build --release --manifest-path tools/board/Cargo.toml
./tools/board/target/release/board read | grep -c '^  \[notice\] main'
```

Expected: **3**, not 0. This is the live confirmation; a green unit test over a
temp repo is not the same evidence.

- [ ] **Step 7: fmt and commit**

```bash
cargo fmt --manifest-path tools/board/Cargo.toml
git add tools/board/src/live.rs
git commit -m "fix(board): main can post a live notice (B11)

Notice liveness was derived from ancestry, and main is trivially its own
ancestor, so every notice main ever posted was filtered from the render --
three were sitting unread, one reporting main red on a heavy-tier
calibration. A newborn campaign branch hit the same predicate temporarily,
which is how it was found: this campaign's own opening notice rendered to
nobody, then started rendering once the branch gained a commit.

Urgent because reap drops merged notices regardless of age, so these posts
were not merely invisible, they were on death row."
```

---

## Task 3: Batched object reads in the store (B7)

**Files:**
- Modify: `tools/board/src/git.rs` (add `git_stdin_bytes`)
- Modify: `tools/board/src/store.rs` (`posts_in`, add `cat_file_batch`)
- Test: `tools/board/src/store.rs` in-module tests

**Interfaces:**
- Consumes: nothing.
- Produces: `Repo::git_stdin_bytes(&self, args: &[&str], input: &[u8]) -> Result<Vec<u8>, BoardError>`
  and `Board::cat_file_batch(&self, ids: &[String]) -> Result<BTreeMap<String, Vec<u8>>, BoardError>`.
  Task 4 reuses both.

**Why a bytes helper rather than the existing `git_stdin`:** `cat-file --batch`
output is **size-framed** — `<oid> <type> <size>\n<contents>\n` per record,
where `size` is a *byte* count. `git_stdin` returns
`String::from_utf8_lossy(...).trim_end()`. Lossy decoding replaces invalid UTF-8
with U+FFFD, which **changes byte lengths** and desynchronises size-framed
parsing — and this crate's test suite deliberately splices raw bytes into posts
(`splice_raw_post` in `store.rs`'s tests), so a corrupt post is not hypothetical,
it is covered. `trim_end` additionally eats the final record's trailing newline.

- [ ] **Step 1: Write the failing test for the batch reader**

```rust
#[test]
fn cat_file_batch_returns_every_requested_object_and_skips_a_missing_one() {
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let board = Board::new(repo.clone());
    let a = board.append(&Post::new("technique", "main").with("note", json!("alpha"))).expect("a");
    let b = board.append(&Post::new("technique", "main").with("note", json!("beta"))).expect("b");
    let absent = "0".repeat(40);

    let got = board
        .cat_file_batch(&[a.clone(), b.clone(), absent.clone()])
        .expect("batch");

    assert_eq!(got.len(), 2, "the missing object must be absent, not an error");
    assert!(!got.contains_key(&absent));
    let text = String::from_utf8(got[&a].clone()).expect("utf8");
    assert!(text.contains("alpha"), "got {text:?}");
    assert!(String::from_utf8_lossy(&got[&b]).contains("beta"));
}

#[test]
fn cat_file_batch_frames_by_byte_length_not_by_newlines() {
    // The regression guard for the reason this is a bytes API: a post whose
    // note contains a newline must not truncate the record, and the record
    // after it must still parse.
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let board = Board::new(repo.clone());
    let first = board
        .append(&Post::new("technique", "main").with("note", json!("line one\nline two")))
        .expect("first");
    let second = board
        .append(&Post::new("technique", "main").with("note", json!("after")))
        .expect("second");

    let got = board.cat_file_batch(&[first.clone(), second.clone()]).expect("batch");

    assert_eq!(got.len(), 2);
    assert!(String::from_utf8_lossy(&got[&first]).contains("line two"));
    assert!(String::from_utf8_lossy(&got[&second]).contains("after"));
}
```

- [ ] **Step 2: Run and confirm it fails to compile**

```bash
cargo test --manifest-path tools/board/Cargo.toml cat_file_batch 2>&1 | tail -10
```

Expected: FAIL — `no method named cat_file_batch`. A compile failure is the
expected red here; there is no behavioural red available for a function that does
not exist.

- [ ] **Step 3: Add the bytes helper to `git.rs`**

```rust
    /// Run `git` with `input` on stdin, returning stdout as **bytes**.
    ///
    /// [`git_stdin`](Self::git_stdin) decodes lossily and trims trailing
    /// whitespace, both of which corrupt a size-framed stream: lossy decoding
    /// changes byte lengths (U+FFFD is three bytes) and trimming eats the last
    /// record's terminator. `cat-file --batch` is size-framed, so it needs
    /// this.
    pub fn git_stdin_bytes(&self, args: &[&str], input: &[u8]) -> Result<Vec<u8>, BoardError> {
        use std::io::Write;
        use std::process::Stdio;
        let mut child = Command::new("git")
            .arg("-C")
            .arg(&self.root)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| BoardError::Io(format!("spawning git: {e}")))?;
        child
            .stdin
            .as_mut()
            .ok_or_else(|| BoardError::Io("git stdin".into()))?
            .write_all(input)
            .map_err(|e| BoardError::Io(format!("writing git stdin: {e}")))?;
        let out = child
            .wait_with_output()
            .map_err(|e| BoardError::Io(format!("waiting on git: {e}")))?;
        if !out.status.success() {
            return Err(BoardError::Git {
                cmd: args.join(" "),
                code: out.status.code(),
                stderr: String::from_utf8_lossy(&out.stderr).trim().to_string(),
            });
        }
        Ok(out.stdout)
    }
```

- [ ] **Step 4: Add `cat_file_batch` to `store.rs`**

```rust
    /// Read many objects in ONE `git cat-file --batch`, by object id.
    ///
    /// Replaces one subprocess per post with one per read. Measured on `main`
    /// at 26 posts: 0.850 s for 26 individual `cat-file -p` calls against
    /// 0.041 s batched, and the batched cost does not grow per post.
    ///
    /// Ids are bare object ids, which works because a post's id IS its object
    /// id (D11) — so this is tip-independent, and the same call serves a union
    /// over several refs (B1).
    ///
    /// A missing or unreadable object is **omitted with a warning** rather than
    /// failing the read: one corrupt post must never break a session's render
    /// (D7). The caller therefore treats absence as "already warned about".
    fn cat_file_batch(
        &self,
        ids: &[String],
    ) -> Result<std::collections::BTreeMap<String, Vec<u8>>, BoardError> {
        use std::collections::BTreeMap;
        let mut found: BTreeMap<String, Vec<u8>> = BTreeMap::new();
        if ids.is_empty() {
            return Ok(found);
        }
        let mut input = Vec::new();
        for id in ids {
            input.extend_from_slice(id.as_bytes());
            input.push(b'\n');
        }
        let out = self.repo.git_stdin_bytes(&["cat-file", "--batch"], &input)?;

        let mut pos = 0usize;
        while pos < out.len() {
            // Header line: "<oid> <type> <size>" or "<name> missing".
            let Some(rel) = out[pos..].iter().position(|b| *b == b'\n') else {
                break;
            };
            let header = String::from_utf8_lossy(&out[pos..pos + rel]).to_string();
            pos += rel + 1;

            let mut parts = header.split(' ');
            let name = parts.next().unwrap_or_default().to_string();
            match parts.next() {
                Some("missing") | None => {
                    eprintln!("board: skipping unreadable post {name}: object missing");
                    continue;
                }
                Some(_) => {}
            }
            let Some(size) = parts.next().and_then(|s| s.parse::<usize>().ok()) else {
                eprintln!("board: unparseable cat-file header {header:?}; stopping this batch");
                break;
            };
            if pos + size > out.len() {
                eprintln!("board: truncated cat-file output for {name}; stopping this batch");
                break;
            }
            found.insert(name, out[pos..pos + size].to_vec());
            pos += size + 1; // contents, plus git's trailing newline
        }
        Ok(found)
    }
```

- [ ] **Step 5: Rewrite `posts_in`'s read loop to use it**

In `posts_in`, leave `post_ids_in` and the single `git log` time-map exactly as
they are. Replace only the per-id `cat-file -p` loop. The `when` lookup, the
unreachable-fallback warning, the malformed-post skip, and the final
`sort_by_key` all stay byte-for-byte as they are:

```rust
        let blobs = self.cat_file_batch(&ids)?;
        let mut out = Vec::new();
        for id in ids {
            // Absent means `cat_file_batch` already warned; do not warn twice.
            let Some(bytes) = blobs.get(&id) else {
                continue;
            };
            let text = String::from_utf8_lossy(bytes);
            match Post::from_json(&text) {
                Ok(post) => {
                    // ... existing `when` lookup and warning, unchanged ...
                }
                Err(e) => eprintln!("board: skipping malformed post {id}: {e}"),
            }
        }
```

- [ ] **Step 6: Run the whole suite — behavioural identity is the requirement**

```bash
cargo test --manifest-path tools/board/Cargo.toml 2>&1 | tail -6
```

Expected: green, including the existing malformed-post and resilience tests. If
any resilience test fails, the framing parse is wrong — **do not relax the
test**; those tests are the reason this is a bytes API.

- [ ] **Step 7: Measure the after-arm against Task 1's number**

```bash
cargo build --release --manifest-path tools/board/Cargo.toml
for i in 1 2 3 4 5; do /usr/bin/time -p bash scripts/board-render.sh >/dev/null; done 2>&1 | grep real
```

Record all five. Report them **beside Task 1's five**, same host, same post
count. Expected: a large drop, and the spec's assumption 1 budget is ≤ 1 s. If it
did not drop, say so plainly — that means the subprocess model in §2 fact 6 is
wrong about where the time goes, which is a finding worth more than the task.

- [ ] **Step 8: fmt and commit**

```bash
cargo fmt --manifest-path tools/board/Cargo.toml
git add tools/board/src/git.rs tools/board/src/store.rs
git commit -m "perf(board): one cat-file --batch per read, not one per post (B7)

The ambient render was at 1.43-1.86 s against a 2 s budget at 26 posts on
one host, and a render over budget is SKIPPED rather than slow. ~26 of ~36
subprocesses were the per-post object read; batched, 0.850 s becomes
0.041 s and stops scaling per post, which is what makes cross-host volume
affordable at all.

Needs a bytes-returning stdin helper: the batch stream is size-framed, and
git_stdin decodes lossily (U+FFFD is three bytes, so lengths shift) and
trims the last record's terminator. The suite splices raw bytes into posts
deliberately, so that is a covered case, not a hypothetical."
```

---

## Task 4: Batched reads in the digest (B7, second loop)

`digest::history` has its **own** per-post `cat-file -p` loop at `digest.rs:59`
and does not go through `posts_in`. Its N is sized by the history window rather
than by the tip, so it is potentially far larger — and it is the human read seam
D14 exists for, so leaving it slow defeats the point of fixing the other one.

**Files:**
- Modify: `tools/board/src/digest.rs`
- Modify: `tools/board/src/store.rs` (widen `cat_file_batch` visibility to
  `pub(crate)`)
- Test: `tools/board/src/digest.rs` in-module tests

**Interfaces:**
- Consumes: `Board::cat_file_batch` from Task 3, as `pub(crate)`.
- Produces: no signature change to `digest::history`.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn history_reads_every_post_in_the_window_in_one_batch() {
    // Behavioural, not a spawn count: the guarantee is that batching changed
    // nothing observable. Ten posts, all inside the window, all present, in
    // committed order.
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let board = Board::new(repo.clone());
    for i in 0..10 {
        board
            .append(&Post::new("technique", "main").with("note", json!(format!("note {i}"))))
            .expect("append");
    }
    let posts = history(&board, 14, 2_000_000_000).expect("history");
    assert_eq!(posts.len(), 10);
    for i in 0..10 {
        assert!(
            posts.iter().any(|p| p.post.str_field("note") == Some(&format!("note {i}")[..])),
            "post {i} missing from the digest window"
        );
    }
}
```

Match the existing helper names in `digest.rs`'s test module rather than assuming
these; read them first.

- [ ] **Step 2: Run it — it should PASS against the current code**

```bash
cargo test --manifest-path tools/board/Cargo.toml history_reads_every_post 2>&1 | tail -5
```

Expected: **PASS**. This is a characterisation test, not a red-first test: the
behaviour must not change, so the test's job is to hold it still while the
implementation changes underneath. Say in your report that you know this test
started green and why that is correct here.

- [ ] **Step 3: Replace the loop**

Change `cat_file_batch` to `pub(crate) fn` in `store.rs`. In `digest::history`,
collect the in-window ids in the existing `git log` walk first, then read them in
one batch, then build `StoredPost`s. Keep the existing `seen_ids` oldest-first
dedupe and the `committed_at` attribution exactly as they are — the dedupe is
load-bearing (there is a test named for reap-then-repost of identical content).

- [ ] **Step 4: Confirm the suite is green**

```bash
cargo test --manifest-path tools/board/Cargo.toml 2>&1 | tail -6
```

- [ ] **Step 5: Measure the digest, before and after**

You cannot measure "before" after changing it, so use git:

```bash
git stash && cargo build --release --manifest-path tools/board/Cargo.toml
for i in 1 2 3; do /usr/bin/time -p ./tools/board/target/release/board digest 90 >/dev/null; done 2>&1 | grep real
git stash pop && cargo build --release --manifest-path tools/board/Cargo.toml
for i in 1 2 3; do /usr/bin/time -p ./tools/board/target/release/board digest 90 >/dev/null; done 2>&1 | grep real
```

Report both triples. A 90-day window is used deliberately: it is the case where
history-sized N exceeds tip-sized N.

- [ ] **Step 6: fmt and commit**

```bash
cargo fmt --manifest-path tools/board/Cargo.toml
git add tools/board/src/digest.rs tools/board/src/store.rs
git commit -m "perf(board): batch the digest's reads too — the second loop (B7)

digest::history had its own per-post cat-file loop and never went through
posts_in, so fixing only the store would have left the HUMAN seam slow.
Its N is sized by the history window, not the tip, so it is the larger of
the two."
```

---

## Task 5: The union read over per-host refs (B1)

**Files:**
- Modify: `tools/board/src/store.rs` (`read_refs`, `posts_at_tip`,
  `post_ids_at_tip`, `StoredPost`)
- Modify: `tools/board/src/{render,relevance,digest,live}.rs` (the 20
  `StoredPost` literal sites)
- Test: `tools/board/src/store.rs`, `tools/board/tests/merge_properties.rs`

**Interfaces:**
- Consumes: `cat_file_batch` (Task 3).
- Produces:
  - `pub enum Origin { Local, Peer(String) }` — `Peer` carries the host name.
  - `StoredPost { pub id: String, pub post: Post, pub committed_at: u64, pub origin: Origin }`
  - `Board::read_refs(&self) -> Result<Vec<String>, BoardError>`
  - `Board::peers_ref_prefix()` → `"refs/hornvale/peers/"` (associated const or fn)
  Tasks 6, 7, 8 and 10 all rely on `Origin`.

**Two things that must NOT change**, and a test asserts each: `snapshot()` and
`reap()` stay on `self.refname` only. Compaction is per-log and terminal; a reap
that could see a peer ref would let one host's judgment govern another's view.

- [ ] **Step 1: Add `Origin` and the field, and fix the 20 literal sites**

Adding a struct field breaks every full-literal construction. There are **20**
`StoredPost {` sites across `store.rs`, `render.rs`, `relevance.rs`,
`digest.rs` and `live.rs`. They are compile errors, so the compiler will find
them all — but expect to touch all five files, and do not "fix" a test by
changing its assertions while you are in there.

```rust
/// Which ref a post was read from — the board's provenance, and the only
/// trustworthy source of it.
///
/// NOT derived from the post's `host` field: zero of the 32 posts on the board
/// at the time of writing carried `host` at all (it is a `claim` convention and
/// optional even there), so a field-reading predicate would classify every peer
/// post as local and B4 would be silently inert. A ref name cannot be omitted
/// or mistyped by a posting session.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Origin {
    /// Read from this host's own log, `refs/hornvale/board`.
    Local,
    /// Read from a peer mirror, `refs/hornvale/peers/<host>`; carries the host.
    Peer(String),
}
```

- [ ] **Step 2: Write the failing union tests**

```rust
#[test]
fn a_read_unions_the_local_log_with_every_peer_ref() {
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let board = Board::new(repo.clone());
    let mine = board.append(&Post::new("technique", "main").with("note", json!("local"))).expect("mine");

    // A peer ref built the same way the fetch would build it.
    let peer = Board::with_ref(repo.clone(), "refs/hornvale/peers/lefford");
    let theirs = peer.append(&Post::new("technique", "main").with("note", json!("remote"))).expect("theirs");

    let ids: Vec<String> = board.posts_at_tip().expect("read").into_iter().map(|s| s.id).collect();
    assert!(ids.contains(&mine), "local post missing from the union");
    assert!(ids.contains(&theirs), "peer post missing from the union");
}

#[test]
fn the_union_deduplicates_a_post_present_in_two_refs() {
    // The CRDT property, at read time: an id IS a content hash, so the same
    // post in two refs is the same string twice.
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let board = Board::new(repo.clone());
    let post = Post::new("technique", "main").with("note", json!("same"));
    let a = board.append(&post).expect("a");
    let peer = Board::with_ref(repo.clone(), "refs/hornvale/peers/lefford");
    let b = peer.append(&post).expect("b");
    assert_eq!(a, b, "content addressing should make these one id");

    let ids: Vec<String> = board.posts_at_tip().expect("read").into_iter().map(|s| s.id).collect();
    assert_eq!(ids.iter().filter(|i| **i == a).count(), 1, "duplicated in the union");
}

#[test]
fn the_union_skips_this_hosts_own_mirror_so_a_reaped_post_cannot_return() {
    // peers/<self> is behind the local log whenever a reap has not been
    // pushed, so including it would resurrect this host's own reaped posts.
    // Skipping the ref makes push-before-fetch an optimisation rather than a
    // correctness requirement.
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let board = Board::new(repo.clone());
    let host = crate::live::current_host();          // whatever Task 6 exposes; see note
    let own_mirror = Board::with_ref(repo.clone(), &format!("refs/hornvale/peers/{host}"));
    let ghost = own_mirror
        .append(&Post::new("technique", "main").with("note", json!("reaped here")))
        .expect("ghost");

    let ids: Vec<String> = board.posts_at_tip().expect("read").into_iter().map(|s| s.id).collect();
    assert!(!ids.contains(&ghost), "own mirror must be skipped");
}

#[test]
fn reap_and_snapshot_never_see_a_peer_ref() {
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let board = Board::new(repo.clone());
    let peer = Board::with_ref(repo.clone(), "refs/hornvale/peers/lefford");
    let theirs = peer.append(&Post::new("technique", "main").with("note", json!("theirs"))).expect("t");

    let snap = board.snapshot().expect("snapshot");
    match snap {
        None => {}
        Some(s) => assert!(
            !s.posts().iter().any(|p| p.id == theirs),
            "a snapshot must be single-ref: reap must never judge a peer's log"
        ),
    }
}
```

For `current_host()`: `live.rs` currently shells `hostname` inline in two places
(`live.rs` ~103 and `store.rs` ~1204). Extract it to one `pub fn current_host()
-> String` in `live.rs` as part of this task and use it in all three places —
three copies of a value that decides ref names is how the hostname-collision
hazard in B3 gets subtle.

- [ ] **Step 3: Run and confirm failures**

```bash
cargo test --manifest-path tools/board/Cargo.toml union 2>&1 | tail -15
```

- [ ] **Step 4: Implement `read_refs` and the union**

```rust
    /// The refs a read draws from: this host's own log plus every peer mirror,
    /// **excluding this host's own mirror** (see the skip test for why).
    pub fn read_refs(&self) -> Result<Vec<String>, BoardError> {
        let mut refs = vec![self.refname.to_string()];
        let listed = self.repo.git(&[
            "for-each-ref",
            "--format=%(refname)",
            Self::PEERS_PREFIX,
        ])?;
        let own = format!("{}{}", Self::PEERS_PREFIX, crate::live::current_host());
        for line in listed.lines().filter(|l| !l.is_empty()) {
            if line != own && line != self.refname {
                refs.push(line.to_string());
            }
        }
        Ok(refs)
    }
```

Then `posts_at_tip` resolves each ref to a tip, calls the existing `posts_in`
per tip, tags each result's `origin` from the ref name, concatenates, dedupes by
id **keeping the earliest `committed_at`** (the same rule `digest::history`
already uses for reap-then-repost), and sorts by `(committed_at, id)` as before.
`post_ids_at_tip` unions too — `Cursor::record` prunes against it, and if it
stayed single-ref every foreign post would be pruned from `seen` and re-render
forever.

A ref that fails to resolve is skipped with a warning, never fatal.

- [ ] **Step 5: Add the two-clone arm to `merge_properties.rs`**

The existing harness builds two divergent clones. Add an arm asserting that the
*union read* sees both clones' posts after each is fetched into a distinct
`peers/<host>` ref — the read-side counterpart to the existing merge-tree arms.

- [ ] **Step 6: Full suite, then fmt and commit**

```bash
cargo test --manifest-path tools/board/Cargo.toml 2>&1 | tail -6
cargo fmt --manifest-path tools/board/Cargo.toml
git add tools/board/src/ tools/board/tests/
git commit -m "feat(board): reads union the local log with peer refs (B1)

One writer per ref, so CAS stays correct cross-host; a reap stays terminal
for the log that made it. Dedupe is free because an id IS a content hash.

Provenance comes from the ref, not from a host field: zero of the 32 posts
on the board carry host at all, so a field-reading predicate would have
been inert. StoredPost gains an Origin, which is why 20 literal sites move.

The union skips peers/<self>: that mirror is behind the local log whenever
a reap has not been pushed, so including it would resurrect this host's own
reaped posts. Skipping it makes push-before-fetch an optimisation rather
than a correctness requirement."
```

---

## Task 6: Foreign posts are judged by time alone (B4, B5)

**Files:**
- Modify: `tools/board/src/live.rs`, `tools/board/src/render.rs`
- Test: both, in-module

**Interfaces:**
- Consumes: `Origin` (Task 5), `current_host()` (Task 5).
- Produces: a `Liveness` variant or wrapper expressing *unverifiable here*.
  Task 10's render golden depends on the rendered wording.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn a_foreign_notice_renders_even_though_its_branch_does_not_resolve_here() {
    // The silent-suppression guard. lefford's campaign branches do not exist
    // in this clone, so an ancestry-derived predicate judges every one of its
    // notices dead -- and a reap would then drop a LIVE hold-off out of the
    // local view. B4.
    let stored = peer_notice("campaign/only-on-lefford", "lefford", "do not pin heights");
    let ctx = ctx_for(&[stored.clone()]);
    assert!(matches!(liveness(&stored, &ctx), Liveness::Live));
}

#[test]
fn a_foreign_claim_inside_its_ttl_is_live_but_never_locally_verified() {
    // B5: this host cannot check another host's process table, and must not
    // present a claim it cannot check as one it can.
    let stored = peer_claim("lefford", /* pid */ 999_999, /* ttl_s */ 900, /* age_s */ 10);
    let ctx = ctx_for(&[stored.clone()]);
    assert!(matches!(liveness(&stored, &ctx), Liveness::Live));
    let text = render(&[stored], 0, &RenderOptions::full());
    assert!(
        text.contains("unverifiable"),
        "a foreign claim must say this host cannot check it; got {text}"
    );
}

#[test]
fn a_foreign_claim_past_its_ttl_expires() {
    let stored = peer_claim("lefford", 999_999, 900, /* age_s */ 1_000);
    let ctx = ctx_for(&[stored.clone()]);
    assert!(!matches!(liveness(&stored, &ctx), Liveness::Live));
}

#[test]
fn a_local_claim_is_still_judged_against_this_hosts_process_table() {
    // The arm that keeps the fix from being a removal: local pid checking must
    // survive. A dead pid inside its TTL still expires.
    let stored = local_claim(/* pid */ 999_999, 900, 10);
    let ctx = ctx_for(&[stored.clone()]);
    assert!(!matches!(liveness(&stored, &ctx), Liveness::Live));
}
```

- [ ] **Step 2: Run, confirm the first three fail and the fourth passes**

```bash
cargo test --manifest-path tools/board/Cargo.toml foreign 2>&1 | tail -15
```

- [ ] **Step 3: Implement the branch**

In `liveness`, short-circuit on `Origin::Peer(_)`: judge TTL only, and never
consult `ps` or `merge-base`. Two consequences to note in the doc comment: this
is the same rule `live.rs` already applies to a claim whose `host` differs (the
existing `a_claim_from_another_host_is_not_judged_by_our_process_table` test),
now generalised from `pid` to branches; and it makes foreign posts *cheaper* than
local ones, which is what stops F14's unresolved-author term growing with the
peer population.

Also skip foreign authors in `LiveContext::probe`'s branch-resolution pass
entirely — otherwise the probe still pays two `rev-parse` calls per foreign post
even though nothing reads the answer.

- [ ] **Step 4: Add the render vocabulary**

A foreign post renders with its origin host and, for claims, an explicit
unverifiable marker. Keep it inside the existing line budget — this is a few
characters per post, and B7 bought latency headroom, not line headroom.

- [ ] **Step 5: Suite, fmt, commit**

```bash
cargo test --manifest-path tools/board/Cargo.toml 2>&1 | tail -6
cargo fmt --manifest-path tools/board/Cargo.toml
git add tools/board/src/live.rs tools/board/src/render.rs
git commit -m "feat(board): a foreign post is judged by time alone (B4, B5)

Generalises a rule live.rs already had for pid -- only this host's process
table is authoritative for this host's claims -- to branch liveness, which
had no such guard. Without it, importing lefford's notices would judge
every one unresolved, eventually reap a LIVE hold-off out of the local
view, and pay two uncached rev-parse calls per foreign post forever.

A foreign claim renders as unverifiable-here rather than identically to a
local one: presenting a claim this host cannot check as one it can is how
D7c's non-authority stops being true."
```

---

## Task 7: `board sync` (B2, B3, B6)

**Files:**
- Create: `tools/board/src/sync.rs`
- Modify: `tools/board/src/lib.rs`, `tools/board/src/main.rs`,
  `tools/board/src/render.rs`
- Create: `scripts/board-sync.sh`
- Modify: `Makefile`, `scripts/preflight-merge.sh`
- Test: `tools/board/src/sync.rs` in-module

**Interfaces:**
- Consumes: `current_host()`, `Board::read_refs`, `Origin`.
- Produces:
  - `pub fn sync(repo: &Repo, remote: &str) -> SyncReport`
  - `pub struct SyncReport { pub pushed: Result<(), String>, pub fetched: Result<Vec<String>, String> }`
  - `pub fn peer_ages(repo: &Repo, now_unix: u64) -> Vec<(String, Option<u64>)>`
  Task 10's render golden depends on the staleness wording.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn a_push_is_never_forced() {
    // The one operation in this design that could violate 0118 part 3 across
    // hosts. Asserted on the argv, because the consequence of getting it wrong
    // is unobservable until it has already destroyed history. B3.
    let args = push_argv("origin", "ambrose");
    assert!(
        !args.iter().any(|a| a == "--force" || a == "-f" || a.starts_with("+")),
        "no force, and no leading-plus refspec either: {args:?}"
    );
    assert!(args.contains(&"refs/hornvale/board:refs/hornvale/hosts/ambrose".to_string()));
}

#[test]
fn a_rejected_push_names_the_hostname_collision_hypothesis() {
    // Every legitimate move of a host's own log is a forward commit, so a
    // rejection means two machines both answer to this name.
    let msg = explain_push_failure("ambrose", "! [rejected] (non-fast-forward)");
    assert!(msg.contains("ambrose"));
    assert!(msg.to_lowercase().contains("hostname"), "got {msg}");
}

#[test]
fn sync_failure_is_never_fatal_and_the_local_post_stands() {
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let board = Board::new(repo.clone());
    let id = board.append(&Post::new("technique", "main").with("note", json!("kept"))).expect("post");
    let report = sync(&repo, "no-such-remote");
    assert!(report.pushed.is_err(), "an absent remote must fail the push");
    assert!(
        board.posts_at_tip().expect("read").iter().any(|s| s.id == id),
        "the local post must survive a failed sync"
    );
}

#[test]
fn a_never_synced_peer_is_distinguishable_from_a_just_synced_one() {
    // 0119: an instrument's silence means the claim held, so a peer that has
    // never been synced must not read as "nothing is happening over there".
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let ages = peer_ages(&repo, 1_000_000);
    assert!(ages.iter().all(|(_, age)| age.is_none()), "no sync recorded yet");
    record_sync(&repo, "lefford", 999_000).expect("record");
    let ages = peer_ages(&repo, 1_000_000);
    assert_eq!(ages.iter().find(|(h, _)| h == "lefford").and_then(|(_, a)| *a), Some(1_000));
}
```

- [ ] **Step 2: Run and confirm failures**

```bash
cargo test --manifest-path tools/board/Cargo.toml sync 2>&1 | tail -15
```

- [ ] **Step 3: Implement `sync.rs`**

Push, then fetch, in that order:

```
push:  git push <remote> refs/hornvale/board:refs/hornvale/hosts/<host>
fetch: git fetch <remote> +refs/hornvale/hosts/*:refs/hornvale/peers/*
```

The `+` on the *fetch* refspec is correct and required — a peer's log can be
compacted by its owner, so its mirror is not always a fast-forward here, and this
is a read-only local mirror we do not own. The `+` on a **push** refspec is
forbidden, which is why the test above rejects both `--force` and a leading `+`.

Sync times go in the repository-**common** dir, not the per-worktree one: a fetch
serves every worktree at once, so per-worktree state would report nine different
ages for one fetch. Resolve it with `git rev-parse --git-common-dir` — verified
to return `/…/.git` from inside a worktree where `--git-dir` returns
`/…/.git/worktrees/<name>`. Note that `Repo::git_path` uses `--git-path`, which
gives the per-worktree answer, so this needs its own accessor.

- [ ] **Step 4: Wire the CLI, the script, and the Makefile**

`board sync [remote]`, defaulting to `origin`. Exit code: **zero even on
failure**, printing what failed — it is a best-effort operation and a non-zero
exit would make `make board-sync` look broken when the network is merely down.
Report the distinction in the output, not in the exit code.

`scripts/board-sync.sh` mirrors `scripts/board-render.sh`'s binary-discovery
block (release then debug, silent if absent). Run `shellcheck` on it.

Makefile target beside the existing `board` / `board-digest`:

```make
board-sync: ## The Beacon: publish this host's board and fetch the peers' (B2)
	@cargo run --quiet --manifest-path tools/board/Cargo.toml -- sync
```

Then add a `sync` call to `scripts/preflight-merge.sh` **before** its existing
`hold-off` read, so the pre-merge check sees peers' notices. Keep it non-fatal
and keep the existing rule that the board never changes the verdict (D7).

- [ ] **Step 5: Add peer staleness to the render (B6)**

The render header reports each peer's age, and a never-synced peer says so
rather than being omitted.

- [ ] **Step 6: Live end-to-end against the real remote**

```bash
cargo build --release --manifest-path tools/board/Cargo.toml
./tools/board/target/release/board sync
git ls-remote origin 'refs/hornvale/hosts/*'
git for-each-ref 'refs/hornvale/peers/*' --format='%(refname) %(objectname:short)'
```

Expected: a `hosts/ambrose` ref on origin, and the local peers namespace
populated (empty is a legitimate result until lefford has synced once). Record
the output.

- [ ] **Step 7: shellcheck, suite, fmt, commit**

```bash
shellcheck scripts/board-sync.sh
cargo test --manifest-path tools/board/Cargo.toml 2>&1 | tail -6
cargo fmt --manifest-path tools/board/Cargo.toml
git add tools/board/src/ scripts/board-sync.sh scripts/preflight-merge.sh Makefile
git commit -m "feat(board): board sync — publish this host's log, fetch the peers' (B2, B3, B6)

origin is the hub, so both hosts reach it independently and the sync is
symmetric: lefford being unable to resolve ambrose stops mattering.

Never force-pushes. Every legitimate move of a host's own log is a forward
commit (appends CAS forward, reap is a forward commit), so a rejection can
only mean two machines answer to one hostname -- which is the timings
baseline's documented hazard one layer up, and the one place 0118 part 3
could be violated across hosts. The + on the FETCH refspec is required and
the + on a push refspec is forbidden; a test asserts the argv, because the
consequence is unobservable until history is already gone.

Sync state lives in the git COMMON dir: a fetch serves every worktree, so
per-worktree state would report nine ages for one fetch."
```

---

## Task 8: `board redact` and digest suppression (B8)

**Files:**
- Modify: `tools/board/src/store.rs`, `tools/board/src/digest.rs`,
  `tools/board/src/main.rs`
- Test: `tools/board/src/store.rs`, `tools/board/src/digest.rs`

**Interfaces:**
- Consumes: `Board::reap`'s tree-rewriting internals.
- Produces: `Board::redact(&self, by: &str, id: &str) -> Result<String, BoardError>`.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn redact_evicts_from_the_tip_but_history_still_holds_the_post() {
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let board = Board::new(repo.clone());
    let id = board.append(&Post::new("technique", "main").with("note", json!("oops"))).expect("id");
    let root_before = board.root().expect("root");

    board.redact("main", &id).expect("redact");

    assert!(!board.post_ids_at_tip().expect("ids").contains(&id), "still at the tip");
    assert_eq!(board.root().expect("root"), root_before, "the ref was rerooted (0118 part 3)");
    // Reachable by oid, because the id IS the object id (D11).
    assert!(repo.git(&["cat-file", "-p", &id]).is_ok(), "history lost the post");
}

#[test]
fn the_digest_reports_that_a_redaction_happened_without_the_body() {
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let board = Board::new(repo.clone());
    let id = board
        .append(&Post::new("technique", "main").with("note", json!("SENSITIVE-BODY-TEXT")))
        .expect("id");
    board.redact("main", &id).expect("redact");

    let text = digest(&history(&board, 14, 2_000_000_000).expect("history"));
    assert!(!text.contains("SENSITIVE-BODY-TEXT"), "the body survived the redaction");
    assert!(text.contains("redacted"), "the act must still be recorded; got {text}");
    assert!(text.contains("main"), "who redacted it must be recorded");
}
```

- [ ] **Step 2: Run, confirm failures**

- [ ] **Step 3: Implement**

`redact` appends a `redact` post naming the target, then rewrites the tip tree
without the target — a forward commit parented on the tip it read, exactly as
`reap` does, reusing `reap`'s tree-building and CAS path rather than a second
copy of it. The digest suppresses the body of any post named by a `redact` post
in its window while still reporting the redaction and its author.

Add to `main.rs`: `board redact <by> <post-id>`, usage-exit 2 on missing args,
matching the existing `retract` arm's shape.

- [ ] **Step 4: Document the prohibition in the code**

Put a comment on `redact` recording that byte removal from history is prohibited
*and does not work*: a canary commit force-pushed out of a probe ref's history
was still served by oid — commit and blob plaintext both — so a rewrite breaks
0118 part 3 and buys nothing. Future readers will otherwise propose it.

- [ ] **Step 5: Suite, fmt, commit**

```bash
cargo test --manifest-path tools/board/Cargo.toml 2>&1 | tail -6
cargo fmt --manifest-path tools/board/Cargo.toml
git add tools/board/src/
git commit -m "feat(board): board redact — tip eviction plus digest suppression (B8)

Redaction is a read-time judgment (D10), so 0118 part 3 is untouched:
history keeps the post and the ref is never rerooted.

Byte removal stays prohibited AND was measured not to work -- a canary
commit force-pushed out of a probe ref was still served by oid, blob
plaintext included. Multi-machine makes it worse: a rewrite breaks
fast-forward, so redaction would mean coordinated force-pushes plus a gc
on every clone, and an offline clone keeps the bytes regardless."
```

---

## Task 9: The pre-post secret scan (B9)

Given Task 8's finding, prevention is the only effective control: it acts before
the bytes exist.

**Files:**
- Modify: `tools/board/src/post.rs`, `tools/board/src/main.rs`
- Test: `tools/board/src/post.rs`

**Interfaces:**
- Produces: `pub fn credential_shapes(post: &Post) -> Vec<&'static str>` —
  returns the names of patterns that matched, empty when clean.

- [ ] **Step 1: Write the failing tests, false-positive arm first**

The false-positive arm is the one that matters: a guard habitually overridden
trains the override, and `PROC-claim-shape-s-heuristic` is this crate's standing
example of a predicate too broad to trust.

```rust
#[test]
fn an_ordinary_technique_post_is_not_refused() {
    // Deliberately adversarial: real board prose full of the words a naive
    // scanner would trip on.
    for note in [
        "git mktree rejects any path containing a slash; use a temp index",
        "the census claim key is HV_CENSUS_CLAIM_PATH and it defaults to /tmp/hv-census.claim",
        "set REBASELINE=1 to accept drifted goldens",
        "ssh lefford and check the token count in the run log",
    ] {
        let post = Post::new("technique", "main").with("note", json!(note));
        assert!(
            credential_shapes(&post).is_empty(),
            "false positive on ordinary prose: {note:?}"
        );
    }
}

#[test]
fn a_post_carrying_a_credential_shape_is_refused() {
    for (name, note) in [
        ("github-pat", "use ghp_0123456789abcdefghijklmnopqrstuvwxyzAB to auth"),
        ("aws-akid", "AKIAIOSFODNN7EXAMPLE is the key"),
        ("private-key", "-----BEGIN OPENSSH PRIVATE KEY-----"),
    ] {
        let post = Post::new("technique", "main").with("note", json!(note));
        assert!(
            !credential_shapes(&post).is_empty(),
            "missed {name} in {note:?}"
        );
    }
}
```

- [ ] **Step 2: Run, confirm the second test fails and the first passes**

- [ ] **Step 3: Implement, high-confidence patterns only**

Prefixed, structured shapes only — `ghp_`/`github_pat_`, `AKIA`+16, PEM private
key headers, `xox[baprs]-`. **No entropy heuristics and no generic
`password=`/`token=` matching**: board prose is full of environment-variable
names, and a scanner that fires on them will be routed around within a week.
Scan every string field, not only `note`.

- [ ] **Step 4: Wire the refusal with an override**

`board post` refuses with exit 1, naming the matched pattern and the field, and
saying how to override (an explicit flag or env var — match this crate's existing
conventions). Refusal message must state *what* matched, so a false positive is
diagnosable rather than mysterious.

- [ ] **Step 5: Suite, fmt, commit**

```bash
cargo test --manifest-path tools/board/Cargo.toml 2>&1 | tail -6
cargo fmt --manifest-path tools/board/Cargo.toml
git add tools/board/src/post.rs tools/board/src/main.rs
git commit -m "feat(board): refuse a post carrying a credential shape (B9)

The control that works, because it acts before the bytes exist -- Task 8
established that removing them afterwards is both prohibited and
ineffective.

High-confidence prefixed shapes only, no entropy heuristics and no
password=/token= matching: board prose is full of env var names, and a
guard that is habitually overridden trains the override.
PROC-claim-shape-s-heuristic is the standing local example."
```

---

## Task 10: Conventions v2 — `confirm`, `stale`, `suggest` (B10, B12)

**Files:**
- Modify: `tools/board/src/digest.rs`, `tools/board/src/render.rs`
- Test: both, in-module

**Interfaces:**
- Consumes: `Origin`, the digest's history walk.
- Produces: no new types. `suggest` is excluded from the ambient render; the
  digest gains a corroboration tally and a suggestions section.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn a_suggest_post_is_digest_only() {
    // Both arms. Absent from the ambient view because it is not actionable by
    // the reading session and is the likeliest flood source; present in the
    // digest because that is where the reader who can act on it looks. B12.
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let board = Board::new(repo.clone());
    board
        .append(&Post::new("suggest", "main").with("note", json!("render should show sync age")))
        .expect("post");

    let posts = board.posts_at_tip().expect("read");
    let ctx = ctx_for(&posts);
    let ambient = render(&live_posts(&posts, &ctx), 0, &RenderOptions::session_start());
    assert!(!ambient.contains("sync age"), "a suggestion must not render ambiently");

    let text = digest(&history(&board, 14, 2_000_000_000).expect("history"));
    assert!(text.contains("sync age"), "a suggestion must reach the digest");
}

#[test]
fn a_suggest_post_does_not_consume_the_ambient_post_budget() {
    // The cost half of B12: if suggestions merely rendered as nothing but
    // still counted, they would elide real posts.
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let board = Board::new(repo.clone());
    for i in 0..20 {
        board.append(&Post::new("suggest", "main").with("note", json!(format!("idea {i}")))).expect("s");
    }
    board.append(&Post::new("notice", "main").with("note", json!("REAL NOTICE"))).expect("n");

    let posts = board.posts_at_tip().expect("read");
    let ctx = ctx_for(&posts);
    let ambient = render(&live_posts(&posts, &ctx), 0, &RenderOptions::session_start());
    assert!(ambient.contains("REAL NOTICE"), "suggestions elided a real post");
}

#[test]
fn the_digest_tallies_corroboration_per_technique() {
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let board = Board::new(repo.clone());
    let t = board
        .append(&Post::new("technique", "main").with("note", json!("mktree rejects slashes")))
        .expect("t");
    board.append(&Post::new("confirm", "campaign/x").with("post", json!(t.clone()))).expect("c1");
    board.append(&Post::new("confirm", "campaign/y").with("post", json!(t.clone()))).expect("c2");

    let text = digest(&history(&board, 14, 2_000_000_000).expect("history"));
    assert!(text.contains('2'), "the corroboration count is the measurement; got {text}");
}

#[test]
fn the_digest_fails_loud_where_the_ambient_render_stays_quiet() {
    // The asymmetry IS the property (B12): the digest is the instrument that
    // would report the board being broken, so a quiet digest makes board
    // defects invisible by construction. Asserted in one test so the next
    // change to error handling cannot flatten them separately.
    let (_dir, repo) = crate::git::test_support::temp_repo();
    let board = Board::new(repo.clone());
    corrupt_the_ref(&repo, board.refname());       // point the ref at a non-commit
    assert!(history(&board, 14, 0).is_err(), "the digest must fail loud");
    // and the ambient path must not:
    assert!(Board::new(repo.clone()).posts_at_tip().is_err(), "the read reports the error");
    // main.rs's render arm swallows it and returns; assert that contract holds
    // wherever it is expressed in this crate.
}
```

- [ ] **Step 2: Run, confirm failures**

- [ ] **Step 3: Implement**

Exclude `suggest` from the ambient render **before** the post budget is applied,
so suggestions cannot elide real posts. Add a digest section for suggestions and
a corroboration tally that counts `confirm` and `stale` posts naming each
technique.

- [ ] **Step 4: Post the version-2 convention set to the board**

Per D12 conventions are data, superseded by posting a later set with a higher
version — not by editing code. The v1 set is on the board already; post v2
carrying `confirm`, `stale` and `suggest` alongside the existing kinds, including
`suggest`'s promotion path (`suggest` → `board digest` → a `PROC-*` registry
row).

**Heed the quoting hazard:** `make board-post` re-evaluates `NOTE` in a second
shell, so backticks, dollar signs and quote characters are unsafe even when
single-quoted at your own shell level — a prior post lost two words to command
substitution and still returned a hash. Post via the binary directly, keep the
note free of shell metacharacters, and **read it back** with `board read` or
`git cat-file` before trusting it landed.

- [ ] **Step 5: Suite, fmt, commit**

---

## Task 11: The lane and the hook rule (B13)

**Files:**
- Modify: `scripts/hooks/pre-commit`, `scripts/CLAUDE.md`, root `CLAUDE.md`
- Create: `docs/decisions/<next>-<slug>.md`

**Interfaces:** none in code.

- [ ] **Step 1: Read the hook's existing path filter**

Read `scripts/hooks/pre-commit` and find where it decides Rust-relevance —
documented as `.rs`, `Cargo.*`, `clippy.toml`, `rust-toolchain.toml`, `.cargo/`,
`tools/type-audit/`. Quote the exact lines in your report before editing.

- [ ] **Step 2: Add the rule**

When the staged set is **entirely** under `tools/board/`, run
`cargo test --manifest-path tools/board/Cargo.toml` instead of `make quick`.
Any other staged path keeps current behaviour. The measured justification, which
belongs in a comment: `tools/board` is not a workspace member
(`members = ["kernel", "domains/*", "windows/*", "cli"]`), so `make quick` cannot
see it — while the `.rs` filter runs `make quick` anyway. The 24.4 s suite that
does test it runs in no gate.

- [ ] **Step 3: Verify both arms, by running them**

```bash
cd /Users/nathan/Projects/hornvale/.claude/worktrees/the-beacon
# arm 1: a board-only change runs the board suite
printf '\n' >> tools/board/src/lib.rs && git add tools/board/src/lib.rs
git commit -m "chore: hook arm 1 probe" 2>&1 | tail -5
git reset --hard HEAD~1
# arm 2: a MIXED change still runs make quick -- the arm that matters, since a
# filter that is too broad silently drops the workspace gate
printf '\n' >> tools/board/src/lib.rs && printf '\n' >> cli/src/main.rs
git add tools/board/src/lib.rs cli/src/main.rs
git commit -m "chore: hook arm 2 probe" 2>&1 | tail -5
git reset --hard HEAD~1
```

Record both outputs verbatim. Arm 2 failing to run `make quick` is a defect in
the filter, not an inconvenience.

- [ ] **Step 4: Document the lane**

Root `CLAUDE.md`'s board section gains the lane, its exclusion list (`reap`
semantics, the CAS/append path, the sync/push path), the risk-not-schedule naming
rule, and the prohibition on wiring suggestions to auto-implementation.
`scripts/CLAUDE.md`'s hook paragraph gains the new branch of the filter.

- [ ] **Step 5: Write the decision record**

`docs/decisions/` — find the next free number with `ls docs/decisions/ | tail -3`
(0125 is taken; check for a collision with any branch that landed while this
campaign ran, which has happened before). Follow the house form: title as a
claim, **Status/Decider/Relates** header, Context, The decision, Consequences,
See also. Record the *process* choice, since a process choice that lives only in
a spec gets relitigated.

- [ ] **Step 6: Docs gate and commit**

```bash
cargo test -p hornvale --test docs_consistency
shellcheck scripts/hooks/pre-commit
git add scripts/hooks/pre-commit scripts/CLAUDE.md CLAUDE.md docs/decisions/
git commit -m "chore(board): a risk-scoped lane, and a hook rule that gates what it changes (B13)"
```

---

## Task 12: Close — lefford, budget, artifacts, capture

**Files:**
- Modify: `book/src/frontier/idea-registry.md`, `book/src/chronicle/the-beacon.md`
  (create), `docs/retrospectives/the-beacon.md` (create)

- [ ] **Step 1: Build the board on lefford and verify the round trip**

```bash
git push origin campaign/the-beacon
ssh lefford 'cd ~/Projects/hornvale && git fetch origin && git checkout <full-sha> -- . 2>/dev/null; cargo build --release --manifest-path tools/board/Cargo.toml && ./tools/board/target/release/board sync && ./tools/board/target/release/board read | head -20'
```

Use a **full SHA**, never a branch name — the census dispatch rule exists because
`reset --hard` on a branch name can land on a stale local branch over there.
Then sync back here and confirm lefford's posts arrive:

```bash
./tools/board/target/release/board sync
git for-each-ref 'refs/hornvale/peers/*' --format='%(refname) %(objectname:short)'
```

Record the output. **This is the campaign's actual acceptance test**; everything
before it is a unit test over a temp repo.

- [ ] **Step 2: Measure the render at cross-host volume**

```bash
for i in 1 2 3 4 5; do /usr/bin/time -p bash scripts/board-render.sh >/dev/null; done 2>&1 | grep real
./tools/board/target/release/board read | wc -l
```

Compare against Task 1 and Task 3. The spec's assumption 1 budget is ≤ 1 s at
100 posts across two peers. If it is over, report the number rather than
adjusting the budget.

- [ ] **Step 3: Regenerate artifacts and check for drift**

```bash
make rebaseline
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
```

`docs/digest/` is expected to drift: Task 11 adds a decision record, and the
in-force decision index is generated. Commit the regenerated artifacts in the
same commit that caused them.

- [ ] **Step 4: Registry amendments**

- `PROC-board-cross-host`: `spec'd` → `shipped`.
- `PROC-board-crypto-shred`, `PROC-board-post-signing`: unchanged, still `raw`.
- `PROC-board-vote-tally`: unchanged, `rejected`.
- **F14 and F16**: amend to record which term actually dominated — they aim at
  the unresolved-author term, and the per-post read was an order of magnitude
  larger. Their home is `docs/retrospectives/the-cairn.md`.
- Any `suggest` posts that arrived during the campaign: promote the survivors to
  `PROC-*` rows. Check with `./tools/board/target/release/board digest 30`.

Run `cargo test -p hornvale --test docs_consistency` after.

- [ ] **Step 5: Chronicle and retrospective**

`book/src/chronicle/the-beacon.md` at the book's altitude — technical, no code
required to follow it. `docs/retrospectives/the-beacon.md` for process lessons,
with the campaign's followups promoted out of
`.superpowers/sdd/decision-ledger.md` **before** the worktree is torn down; that
scratch dies with the worktree. The 16-entry ledger is the source.

Re-score `book/src/open-questions.md` if any Confidence Gradient bet moved.

- [ ] **Step 6: The full gate, then preflight**

```bash
make gate 2>&1 | tail -20
make preflight
```

`make gate` is ~8 min and does not build `tools/board`; run the board suite
alongside it. Stagger against other sessions — two concurrent gates cost about
thirty minutes and both look hung.

- [ ] **Step 7: Post the closing notice, and stop**

Post a `notice` recording that the board is now cross-host and what changed for
readers. Then **stop**: G6 is a hard stop. Present the post-G3 ledger digest and
wait for Nathan before merging.

---

## Self-Review

**Spec coverage.** B1 → Task 5. B2/B3/B6 → Task 7. B4/B5 → Task 6. B7 → Tasks 3
and 4 (both loops). B8 → Task 8. B9 → Task 9. B10/B12 → Task 10. B11 → Task 2,
first among code tasks because it has a deadline. B13 → Task 11. Assumption 1's
budget → Tasks 1, 3, 12 as a before/after/at-volume triple. Assumption 4
(lefford has no board) → Task 12 Step 1. Tests 1–17 and the two recorded hook
arms all appear. §10's three open questions stay open and are not smuggled into
tasks.

**Two deliberate deviations from the spec, both flagged rather than silent.**
Task 5 extracts `current_host()` into one function; the spec does not mention it,
but three copies of the value that decides ref names is how B3's collision hazard
becomes subtle. Task 7 puts sync state in the git *common* dir and notes that
`Repo::git_path` gives the per-worktree answer, so a new accessor is needed —
the spec says "common dir" without naming that obstacle.

**Type consistency.** `Origin::{Local, Peer(String)}` is introduced in Task 5 and
consumed by name in Tasks 6, 8 and 10. `cat_file_batch` is private in Task 3 and
widened to `pub(crate)` in Task 4 — stated in Task 4's Interfaces rather than
left to be discovered. `current_host()` is defined in Task 5 and used in Tasks 5
and 7. `SyncReport`, `peer_ages`, `record_sync`, `push_argv` and
`explain_push_failure` are all named in Task 7's Interfaces and tests.

**Known plan-level risk.** Several tasks' test helpers (`ctx_for`,
`peer_notice`, `peer_claim`, `local_claim`, `stored_notice`, `commit_empty`,
`corrupt_the_ref`) are named but not written here, because the crate's existing
in-module test modules already have close analogues and the implementer should
match them rather than import a second style. Each task's first step therefore
says to read the existing tests first. If an implementer cannot find an analogue,
writing the helper is in scope for that task — say so in the report rather than
reshaping the test to avoid it.
