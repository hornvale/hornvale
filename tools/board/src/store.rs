//! The board's write and read-tip paths.
//!
//! D11 — one immutable, content-addressed file per post; every operation is an
//! append. D13 — no code path here can reroot the ref.

use crate::BoardError;
use crate::git::Repo;
use crate::post::Post;
use std::sync::atomic::{AtomicU64, Ordering};

/// The board's ref. Deliberately outside `refs/heads/`, so it is invisible to
/// branch listings and never participates in a merge with `main`.
pub const BOARD_REF: &str = "refs/hornvale/board";

/// How many times a contended write retries before failing loudly.
const MAX_ATTEMPTS: u32 = 24;

/// Ids per `git cat-file --batch` invocation, in [`Board::cat_file_batch`].
///
/// `Repo::git_stdin_bytes` writes the whole request before it reads any of
/// the response. `cat-file --batch`'s OUTPUT grows with both id count and
/// each object's size, so a big enough batch can fill git's stdout pipe
/// before git has drained our stdin; git then blocks writing output nobody
/// is draining yet, which stops it reading further input, which leaves our
/// own write blocked on a pipe git will never come back to drain — a
/// deadlock, not a slow path, with no timeout anywhere in the chain (fix
/// round 1 on B7, 2026-08-11: reproduced at n=4000 against ~500-byte
/// objects, and independently on real board posts; cost 42 minutes of wall
/// clock before it was diagnosed).
///
/// The fix bounds only the INPUT side, which is what actually matters here:
/// if one chunk's id-line list fits inside the smallest plausible pipe
/// buffer, `write_all` for that chunk can never block, so the read that
/// drains stdout is always reached — and once THAT read has started, the
/// OUTPUT size stops mattering, because `wait_with_output` just keeps
/// draining until git exits. Each id line is a 40-character hex oid plus a
/// newline: 41 bytes. `256 * 41 = 10,496` bytes (~10.25 KiB).
///
/// **The argument requires this number to stay UNDER pipe capacity, not
/// above any floor** — get the direction backwards and the comment reads as
/// reassuring while licensing an unsafe increase (fix round 2 on B7 wrote a
/// version of this comment that did exactly that, reasoning from a 4 KiB
/// page as a "floor" the chunk size was "comfortably above" — which argues
/// FOR raising this constant, the opposite of what keeps it safe). Measured
/// directly on this host in fix round 3's review: an undrained pipe accepts
/// 65,536 bytes before a writer blocks, matching Linux's own default pipe
/// capacity. 10,496 B against a measured 65,536 B is roughly a 6x margin —
/// state that margin, and check any future change against it, rather than
/// reasoning from an unrelated lower bound.
const CAT_FILE_BATCH_CHUNK: usize = 256;

/// True if `id` is a full, unabbreviated git object id: exactly 40 lowercase
/// hex characters (this project's objects are SHA-1).
///
/// [`Board::cat_file_batch`] requires this of every id it is given: git's
/// `cat-file --batch` echoes the *resolved* id in its header line, not
/// whatever was asked for, so an abbreviation or a ref name would key the
/// result map under a different string than the original id — a silent
/// miss, not an error. The requirement is enforced by filtering (warn and
/// skip) **inside `cat_file_batch` itself** — the one point every consumer
/// converges on, including `digest.rs`'s `history` walk, which harvests ids
/// straight from its own `git log` and never passes through
/// [`Board::post_ids_in`] — because an id that fails this check is untrusted
/// repository data, not a caller bug. `post_ids_in` also filters (it can
/// name the tree path in its own warning, which `cat_file_batch` cannot),
/// but `cat_file_batch` does not rely on it: see that function's doc
/// comment for why fix round 3 moved the filter here after a debug-only
/// assertion at this exact spot turned out to be reachable from
/// `digest.rs`, and why blanking or panicking the whole board on one bad
/// filename is a worse outcome than skipping it.
///
/// **Deliberately SHA-1-only (40 chars), not 40-or-64.** A SHA-256
/// repository (`git init --object-format=sha256`) would fail every check
/// here, including on ids this tool wrote itself, and fall back to warn-and-
/// skip for every single post — not a crash, but a silently empty board on
/// a format this project does not use anywhere (fix round 3's review
/// verified the failure mode directly and confirmed it degrades gracefully
/// rather than panicking). Left as a known, named gap rather than widened to
/// accept 64 hex characters too: hornvale has no SHA-256 repository today,
/// and every other id-shaped constant in this crate (`"0".repeat(40)` in
/// tests, the `41` in `CAT_FILE_BATCH_CHUNK`'s arithmetic) already assumes
/// 40, so widening only this one check would not have made the crate
/// SHA-256-capable, only inconsistent about it.
fn is_full_object_id(id: &str) -> bool {
    id.len() == 40
        && id
            .bytes()
            .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
}

thread_local! {
    /// Malformed post filenames [`Board::post_ids_in`] has already warned
    /// about, in THIS thread.
    ///
    /// `post_ids_in` runs three times in one ordinary `board render`/`read`
    /// invocation (`posts_at_tip`, `relevance::unseen`, `Cursor::record`
    /// each call it independently), so without this, one bad filename would
    /// print the identical warning three times. Thread-local rather than a
    /// `Board` field or a process-global `static`: the CLI this dedup
    /// exists for is single-threaded per invocation, so thread-local already
    /// gives exactly the scope wanted (once per `board` process, reset on
    /// the next one) without widening `Board`'s shape — and it keeps
    /// `cargo test`'s parallel test threads from sharing (and so masking)
    /// each other's warnings, which a process-global would not.
    static WARNED_MALFORMED_POST_FILENAMES: std::cell::RefCell<std::collections::BTreeSet<String>> =
        const { std::cell::RefCell::new(std::collections::BTreeSet::new()) };
}

/// Text that appears in exactly one place a stderr can come from: CAS-loss
/// exhaustion (the final `Err` in `append_with_attempts`). Shared by the
/// message itself and by the test that checks a *permanent* failure is never
/// misreported as exhaustion — hoisting it here means a future reword of the
/// message cannot silently decouple that assertion from what it is meant to
/// be checking (it would otherwise still compile and still pass, just
/// against nothing).
const EXHAUSTION_MARKER: &str = "compare-and-swap races";

/// Discriminates concurrent throwaway-index paths (and, mixed into the retry
/// jitter, concurrent backoffs) within one process.
///
/// The process id alone is not enough for either job: `Board::append` is safe
/// to call from multiple threads of the same process (the concurrency
/// property this crate exists to hold), and a pid is shared by every thread
/// in that process. Two threads naming the same `GIT_INDEX_FILE` race on
/// git's own `.lock` file (a hard failure, not a graceful compare-and-swap
/// loss); two threads computing the same jitter retry in lockstep instead of
/// spreading out. Mixing a per-call counter into both keeps them unique
/// across threads within one process, on top of the pid keeping them unique
/// across processes.
static CALL_DISCRIMINANT: AtomicU64 = AtomicU64::new(0);

/// A post as stored: its id, its content, and when it was appended. The commit
/// is the clock (D5) — posts carry durations, never instants.
#[derive(Debug, Clone)]
pub struct StoredPost {
    /// Object id of the post's bytes; also its filename.
    pub id: String,
    /// The post itself.
    pub post: Post,
    /// Unix seconds of the commit that appended it.
    pub committed_at: u64,
}

/// One atomic read of the board: the tip commit, and exactly the posts
/// present in that commit's tree.
///
/// This type exists because `reap` is the one operation with *permanent*
/// consequences, and it needs three things to agree: the post set the
/// liveness probe was taken over, the post set the reap predicate is
/// evaluated against, and the tip the compare-and-swap is baselined on.
/// Reading the tip twice — once in a caller, once inside `reap` — silently
/// broke that agreement: a `claim` appended between the two reads was never
/// `ps`-probed, so it read as pid-dead and was dropped from the tip tree
/// with the CAS succeeding. That is a permanent loss of a live claim, the
/// exact double-start the board exists to prevent.
///
/// A `TipSnapshot` bundles the tip and its posts into one value read once,
/// so the reap set and the CAS baseline cannot be two different readings of
/// the board. [`ReapPlan`] closes the third: it is the only way to reach
/// [`Board::reap`], and its only constructor probes the very snapshot it is
/// handed.
#[derive(Debug, Clone)]
pub struct TipSnapshot {
    tip: String,
    posts: Vec<StoredPost>,
}

impl TipSnapshot {
    /// The tip commit this snapshot was read from — the only legitimate CAS
    /// baseline for a write derived from it.
    pub fn tip(&self) -> &str {
        &self.tip
    }

    /// The posts present in that tip's tree, oldest first.
    pub fn posts(&self) -> &[StoredPost] {
        &self.posts
    }
}

/// A reap's authorization: a tip snapshot plus the liveness probe taken over
/// **exactly that snapshot's posts**.
///
/// [`Board::reap`] takes one of these and nothing else, and the only way to
/// build one is [`ReapPlan::probe`], which probes the snapshot it is given.
/// So the probe set, the reap set, and the CAS baseline are the same three
/// things by construction — there is no signature through which a caller can
/// hand `reap` a context probed over a different, earlier reading of the
/// board. That is the same move [`crate::relevance::Displayed`] made for
/// `Cursor::record`: make the desynchronised state unrepresentable rather
/// than merely absent today.
#[derive(Debug)]
pub struct ReapPlan<'a> {
    snapshot: &'a TipSnapshot,
    ctx: crate::live::LiveContext,
}

impl<'a> ReapPlan<'a> {
    /// Probe the world over `snapshot`'s posts, and only those.
    pub fn probe(repo: &Repo, snapshot: &'a TipSnapshot) -> Result<Self, BoardError> {
        let ctx = crate::live::LiveContext::probe(repo, snapshot.posts())?;
        Ok(Self { snapshot, ctx })
    }

    /// The snapshot this plan was probed over.
    pub fn snapshot(&self) -> &TipSnapshot {
        self.snapshot
    }

    /// The liveness context, for inspection.
    pub fn context(&self) -> &crate::live::LiveContext {
        &self.ctx
    }

    /// Move the probed clock forward by `secs`.
    ///
    /// The one deliberate seam for exercising the TTL and grace-period
    /// boundaries without waiting real days. Deliberately *only* the clock:
    /// a general `&mut LiveContext` accessor would let a caller substitute a
    /// context probed over some other post set, which is precisely what this
    /// type exists to make impossible.
    pub fn advance_clock(&mut self, secs: u64) {
        self.ctx.now_unix = self.ctx.now_unix.saturating_add(secs);
    }
}

/// One board, on one ref, in one repository.
#[derive(Debug, Clone)]
pub struct Board {
    repo: Repo,
    refname: String,
}

impl Board {
    /// The board on the canonical ref.
    pub fn new(repo: Repo) -> Self {
        Self::with_ref(repo, BOARD_REF)
    }

    /// The board on an arbitrary ref — used by tests to simulate clones.
    pub fn with_ref(repo: Repo, refname: &str) -> Self {
        Self {
            repo,
            refname: refname.to_string(),
        }
    }

    /// The repository this board lives in.
    pub fn repo(&self) -> &Repo {
        &self.repo
    }

    /// This board's ref name.
    pub fn refname(&self) -> &str {
        &self.refname
    }

    /// The current tip commit, or `None` if the board does not exist yet.
    pub fn tip(&self) -> Result<Option<String>, BoardError> {
        self.repo.rev_parse_verify(&self.refname)
    }

    /// Append a post. Returns its id — the object id of its own bytes, which is
    /// also its filename, so appending identical content twice is idempotent.
    pub fn append(&self, post: &Post) -> Result<String, BoardError> {
        self.append_with_attempts(post, MAX_ATTEMPTS)
    }

    /// `append`'s retry loop, with the attempt budget as a parameter so a
    /// test can drive the exhaustion arm deterministically without waiting on
    /// real contention. Private: this is not a wider public surface, just a
    /// seam `append` and its tests both go through.
    fn append_with_attempts(&self, post: &Post, max_attempts: u32) -> Result<String, BoardError> {
        let bytes = post.canonical_bytes()?;
        let id = self.repo.hash_object(&bytes)?;
        let path = format!("posts/{id}.json");
        let mut last_reason: Option<String> = None;

        for attempt in 0..max_attempts {
            let call_id = CALL_DISCRIMINANT.fetch_add(1, Ordering::SeqCst);
            let old = self.tip()?;
            let tree = self.tree_with(call_id, old.as_deref(), &id, &path)?;

            if let Some(parent) = &old {
                let base_tree =
                    self.repo
                        .git(&["rev-parse", "--verify", &format!("{parent}^{{tree}}")])?;
                if tree == base_tree {
                    // Content addressing means the only way this tree can come
                    // back byte-identical to the parent's is that `path`
                    // already held exactly `blob` — this exact post is already
                    // recorded. D11's idempotence should hold at the commit
                    // level too, not only the file level: nothing to commit.
                    return Ok(id);
                }
            }

            let mut args: Vec<String> = vec![
                "commit-tree".into(),
                tree.clone(),
                "-m".into(),
                format!("board: {} by {}", post.kind, post.by),
            ];
            if let Some(parent) = &old {
                args.push("-p".into());
                args.push(parent.clone());
            }
            let refs: Vec<&str> = args.iter().map(String::as_str).collect();
            let new = self.repo.git(&refs)?;

            match self.cas(&new, old.as_deref())? {
                None => return Ok(id),
                Some(stderr) => last_reason = Some(stderr),
            }
            // Contended: another writer moved the ref. Back off with jitter
            // that mixes the pid (decorrelates processes) with `call_id`
            // (decorrelates threads within one process — a pid alone is
            // identical for every thread, so it cannot spread them out). No
            // `rand` dependency by design.
            let jitter = 3 + ((u64::from(std::process::id()) ^ call_id) % 11);
            std::thread::sleep(std::time::Duration::from_millis(
                jitter * u64::from(attempt + 1),
            ));
        }
        Err(BoardError::Git {
            cmd: format!("append to {}", self.refname),
            code: None,
            stderr: format!(
                "lost {max_attempts} {EXHAUSTION_MARKER}; the ref is under sustained \
                 contention and this post was NOT recorded. Last reason: {}",
                last_reason.as_deref().unwrap_or("unknown")
            ),
        })
    }

    /// The tip and its posts, read once — see [`TipSnapshot`] for why the
    /// pair has to be one value. `None` means the board does not exist yet.
    pub fn snapshot(&self) -> Result<Option<TipSnapshot>, BoardError> {
        let Some(tip) = self.tip()? else {
            return Ok(None);
        };
        let posts = self.posts_in(&tip)?;
        Ok(Some(TipSnapshot { tip, posts }))
    }

    /// Post ids present in the tip tree, sorted.
    pub fn post_ids_at_tip(&self) -> Result<Vec<String>, BoardError> {
        let Some(tip) = self.tip()? else {
            return Ok(Vec::new());
        };
        self.post_ids_in(&tip)
    }

    /// Post ids present in `tip`'s tree, sorted. Takes the tip rather than
    /// re-reading it, so a caller holding a [`TipSnapshot`] can ask about
    /// exactly the commit it read.
    ///
    /// A tree entry under `posts/` whose filename is not a full 40-hex
    /// object id is **skipped, with a warning** — not surfaced as an error.
    /// This is a second validation point for [`is_full_object_id`], not the
    /// only one: [`cat_file_batch`](Self::cat_file_batch) filters again,
    /// independently, right before it invokes `git` — see that function's
    /// doc comment for why neither point can rely on the other having run
    /// first. This one exists anyway because it is strictly more
    /// informative (it can name the full tree path in its warning; a bare
    /// id reaching `cat_file_batch` cannot), and every caller of THIS
    /// function already treats an id's absence from the returned list as
    /// "already warned about". Fix round 2 on B7 found that a hard `Err`
    /// here (or downstream) makes ONE bad filename blank the ENTIRE board —
    /// every claim and hold-off, not just the one bad post — which is worse
    /// than D7's ordinary corrupt-post case and the opposite of what D7
    /// requires.
    ///
    /// The warning is deduplicated per process (see
    /// `WARNED_MALFORMED_POST_FILENAMES`) because this function runs three
    /// times in one ordinary `board render`/`read` invocation
    /// (`posts_at_tip`, `relevance::unseen`, `Cursor::record` each call it
    /// independently) — without the dedup, one bad filename would print the
    /// same warning three times per invocation, which is noise `make board`
    /// would see even though the corrupt-*content* warning in `posts_in`
    /// below prints only once for the equivalent case.
    fn post_ids_in(&self, tip: &str) -> Result<Vec<String>, BoardError> {
        let listed = self.repo.git(&["ls-tree", "-r", "--name-only", tip])?;
        let mut ids: Vec<String> = Vec::new();
        for line in listed.lines() {
            let Some(rest) = line.strip_prefix("posts/") else {
                continue;
            };
            let Some(id) = rest.strip_suffix(".json") else {
                continue;
            };
            if is_full_object_id(id) {
                ids.push(id.to_string());
            } else {
                let already_warned = WARNED_MALFORMED_POST_FILENAMES
                    .with(|warned| !warned.borrow_mut().insert(line.to_string()));
                if !already_warned {
                    eprintln!(
                        "board: skipping malformed post filename {line:?}: not a full 40-hex \
                         object id -- one corrupt filename must never blank the whole board (D7)"
                    );
                }
            }
        }
        ids.sort();
        Ok(ids)
    }

    /// Every post in the tip tree, with its append time, oldest first.
    ///
    /// A post that fails to parse is skipped with a warning: one corrupt post
    /// must never break a session's render (D7).
    pub fn posts_at_tip(&self) -> Result<Vec<StoredPost>, BoardError> {
        let Some(tip) = self.tip()? else {
            return Ok(Vec::new());
        };
        self.posts_in(&tip)
    }

    /// [`posts_at_tip`](Self::posts_at_tip)'s body, against a tip the caller
    /// already read. Private, and the single implementation both
    /// `posts_at_tip` and [`snapshot`](Self::snapshot) go through — so there
    /// is no second copy of this walk that could drift from it.
    fn posts_in(&self, tip: &str) -> Result<Vec<StoredPost>, BoardError> {
        let ids = self.post_ids_in(tip)?;
        let mut when: std::collections::BTreeMap<String, u64> = std::collections::BTreeMap::new();
        // One `git log` over the ref, mapping each added post file to the
        // commit time that added it — cheaper than a call per post.
        let log = self.repo.git(&[
            "log",
            "--format=@%ct",
            "--diff-filter=A",
            "--name-only",
            "--reverse",
            tip,
        ])?;
        let mut current = 0u64;
        for line in log.lines() {
            if let Some(ts) = line.strip_prefix('@') {
                current = ts.parse().unwrap_or(0);
            } else if let Some(id) = line
                .strip_prefix("posts/")
                .and_then(|l| l.strip_suffix(".json"))
            {
                when.entry(id.to_string()).or_insert(current);
            }
        }
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
                    // Every id from `post_ids_at_tip()` should have a matching
                    // entry in `when`: each append is a single-parent commit
                    // that adds exactly one file, so the `git log
                    // --diff-filter=A` walk above should attribute every post
                    // to the commit that added it. This fallback is therefore
                    // currently unreachable in practice (a corpus of committed
                    // reasoning: no `--root` needed, and a deletion-only
                    // commit contributes no line to skew the map either) --
                    // but if it is ever reached, epoch 0 biases the post
                    // toward `Expired` (maximally old), the OPPOSITE of this
                    // crate's fail-open convention elsewhere (`live.rs`'s
                    // `hostname`/`ps`/`merge-base` fallbacks all fail toward
                    // `Live`). Warn loudly rather than let that bias through
                    // silently.
                    let committed_at = match when.get(&id) {
                        Some(ts) => *ts,
                        None => {
                            eprintln!(
                                "board: post {id} has no recorded append time; defaulting to \
                                 epoch 0, which biases it toward Expired rather than Live -- \
                                 this should be unreachable under the current one-file-per-\
                                 commit append invariant"
                            );
                            0
                        }
                    };
                    out.push(StoredPost {
                        committed_at,
                        id,
                        post,
                    });
                }
                Err(e) => eprintln!("board: skipping malformed post {id}: {e}"),
            }
        }
        out.sort_by_key(|s| (s.committed_at, s.id.clone()));
        Ok(out)
    }

    /// Read many objects in one or more `git cat-file --batch` calls, by
    /// object id.
    ///
    /// Replaces one subprocess per post with one per [`CAT_FILE_BATCH_CHUNK`]
    /// ids. Measured on `main` at 26 posts: 0.850 s for 26 individual
    /// `cat-file -p` calls against 0.041 s batched, and the batched cost does
    /// not grow per post.
    ///
    /// Ids MUST be full, unabbreviated object ids, which works because a
    /// post's id IS its object id (D11) — so this is tip-independent, and the
    /// same call serves a union over several refs (B1). `cat-file --batch`
    /// echoes the *resolved* id in each header line, not whatever was asked
    /// for, so anything else (an abbreviation, a ref name) would key the
    /// result map under a different string than the original id and
    /// silently miss on lookup — see [`is_full_object_id`].
    ///
    /// **This is filtered right here, warn-and-skip, before any `git`
    /// invocation** — not merely documented, and not by a caller upstream.
    /// Fix round 2 on B7 put the filter in [`post_ids_in`](Self::post_ids_in)
    /// instead and asserted the requirement here with a debug-only
    /// `debug_assert!`, reasoning that `post_ids_in` was the one place every
    /// id converges. That reasoning was wrong on a fact fix round 3's review
    /// caught by simulating Task 4: `digest.rs` harvests its own ids
    /// straight from `git log --diff-filter=A --name-only` and calls this
    /// function directly, **never through `post_ids_in`** — so the assert
    /// was reachable with an ordinary malformed filename, and it fired as a
    /// release-mode-only debug assertion: `board digest` (which
    /// `make board-digest` always runs *without* `--release`) would panic —
    /// exit 101, empty stdout — on exactly the input fix round 2 spent an
    /// entire round teaching `read`/`render` to survive. A panic on
    /// untrusted repository data is not better than fix round 1's hard
    /// `Err`; it is the same failure with a louder exit code. `post_ids_in`
    /// still filters too (kept — it can name the tree path in its warning,
    /// which this function cannot), but this function no longer trusts any
    /// caller, including its own siblings in this crate, to have done so
    /// first.
    ///
    /// A missing or unreadable object is **omitted with a warning** rather than
    /// failing the read: one corrupt post must never break a session's render
    /// (D7). The caller therefore treats absence as "already warned about" —
    /// which now includes an id this function itself rejected, not only one
    /// git reported missing.
    fn cat_file_batch(
        &self,
        ids: &[String],
    ) -> Result<std::collections::BTreeMap<String, Vec<u8>>, BoardError> {
        let mut found: std::collections::BTreeMap<String, Vec<u8>> =
            std::collections::BTreeMap::new();
        let mut valid: Vec<String> = Vec::with_capacity(ids.len());
        for id in ids {
            if is_full_object_id(id) {
                valid.push(id.clone());
            } else {
                eprintln!(
                    "board: skipping {id:?}: not a full 40-hex object id -- cat-file --batch \
                     echoes the resolved id, so anything else would key the result map \
                     differently from what was asked for"
                );
            }
        }
        // Chunked, not one shot: see CAT_FILE_BATCH_CHUNK's doc comment for
        // why an unbounded batch here is a deadlock hazard, not just a perf
        // one.
        for chunk in valid.chunks(CAT_FILE_BATCH_CHUNK) {
            self.cat_file_batch_chunk(chunk, &mut found)?;
        }
        Ok(found)
    }

    /// One `git cat-file --batch` invocation over (at most
    /// [`CAT_FILE_BATCH_CHUNK`]) `ids`, merging results into `found`.
    ///
    /// Split out of [`cat_file_batch`](Self::cat_file_batch) purely so that
    /// function can call this once per chunk without duplicating the
    /// header-framing parse; `ids` here is already validated by the caller.
    fn cat_file_batch_chunk(
        &self,
        ids: &[String],
        found: &mut std::collections::BTreeMap<String, Vec<u8>>,
    ) -> Result<(), BoardError> {
        if ids.is_empty() {
            return Ok(());
        }
        let mut input = Vec::new();
        for id in ids {
            input.extend_from_slice(id.as_bytes());
            input.push(b'\n');
        }
        let out = self
            .repo
            .git_stdin_bytes(&["cat-file", "--batch"], &input)?;

        let mut pos = 0usize;
        while pos < out.len() {
            // Header line: "<oid> <type> <size>" or "<name> <status>", where
            // <status> is "missing" or one of cat-file's other status-only
            // words (see the match below).
            let Some(rel) = out[pos..].iter().position(|b| *b == b'\n') else {
                eprintln!("board: unterminated cat-file header at byte {pos}; stopping this batch");
                break;
            };
            let header = String::from_utf8_lossy(&out[pos..pos + rel]).to_string();
            pos += rel + 1;

            let mut parts = header.split(' ');
            let name = parts.next().unwrap_or_default().to_string();
            match parts.next() {
                // KNOWN STATUS-ONLY tokens: git's header line ends right
                // here, with no third field and no body to skip past.
                // `continue` — skip just this one id and keep parsing the
                // rest of the chunk. "missing" is the ordinary case (object
                // absent); "ambiguous"/"dangling"/"notdir" are `cat-file
                // --batch`'s other documented statuses, demonstrated by fix
                // round 3's review with an actually-ambiguous short input
                // (`printf '5093\n<oid>\n' | git cat-file --batch` answers
                // `5093 ambiguous` with no body). Every id `cat_file_batch`
                // hands this function has already passed `is_full_object_id`
                // upstream, which makes an ambiguity genuinely unreachable
                // for a full 40-hex oid in practice — but the earlier
                // version of this match fell through to the size parse on
                // ANY unrecognised second token, `break`-ing the whole
                // chunk (losing every id still queued behind the odd one,
                // up to `CAT_FILE_BATCH_CHUNK - 1` of them) rather than
                // losing just the one id that produced it. Handling these
                // tokens explicitly is cheap insurance against that, not a
                // response to a live hazard.
                Some("missing" | "ambiguous" | "dangling" | "notdir") => {
                    eprintln!("board: skipping {name:?}: cat-file reported {header:?}");
                    continue;
                }
                // A real object type (blob/tree/commit/tag): a third field
                // (the byte size) and a body still follow, parsed below.
                Some(_) => {}
                // GENUINE FRAMING DESYNC, not a recognised status: a header
                // with no second field at all. Unlike the tokens above, this
                // means the byte stream itself cannot be trusted — there is
                // no way to know whether a body follows or how long it is —
                // so `break` (stop the whole chunk) is the only safe
                // response here. `continue`-ing instead would risk reading
                // some body's bytes as the next record's header.
                None => {
                    eprintln!("board: unparseable cat-file header {header:?}; stopping this batch");
                    break;
                }
            }
            let Some(size) = parts.next().and_then(|s| s.parse::<usize>().ok()) else {
                eprintln!("board: unparseable cat-file header {header:?}; stopping this batch");
                break;
            };
            // `pos + size` would wrap silently in release on a desynced
            // stream carrying a bogus, huge size, defeating the bounds check
            // below and panicking on the slice instead — a panic on this
            // path is exactly what D7 says a corrupt post must never cause.
            let Some(end) = pos.checked_add(size) else {
                eprintln!("board: cat-file size overflow for {name}; stopping this batch");
                break;
            };
            if end > out.len() {
                eprintln!("board: truncated cat-file output for {name}; stopping this batch");
                break;
            }
            found.insert(name, out[pos..end].to_vec());
            pos = end + 1; // contents, plus git's trailing newline
        }
        Ok(())
    }

    /// Build a tree equal to `base`'s tree plus one post file.
    ///
    /// `git mktree` cannot do this: it rejects any path containing a slash. The
    /// working recipe is a throwaway index, named with `call_id` so concurrent
    /// callers in the same process (which share a pid) never share a path.
    fn tree_with(
        &self,
        call_id: u64,
        base: Option<&str>,
        blob: &str,
        path: &str,
    ) -> Result<String, BoardError> {
        let raw = self
            .repo
            .git_path(&format!("hv-board-index-{}-{call_id}", std::process::id()))?;
        // `Repo::git_path` returns an absolute path. Guard defensively
        // anyway: a relative path here would resolve against the *process*
        // cwd on the `std::fs` calls below, not the repo root, which is
        // exactly the leak this function used to have.
        let index = if raw.is_absolute() {
            raw
        } else {
            self.repo.root().join(raw)
        };

        // Pre-clean: a leaked index from an earlier crashed or interrupted
        // run must not contaminate this write (stale entries would survive
        // into `write-tree` on the `base == None` path, which has no
        // `read-tree` to overwrite them). Anything other than "already
        // absent" is a real problem worth surfacing, not swallowing — that
        // silence is what let the leak go unnoticed.
        match std::fs::remove_file(&index) {
            Ok(()) => {}
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => {
                return Err(BoardError::Io(format!(
                    "removing stale throwaway index {}: {e}",
                    index.display()
                )));
            }
        }

        let result = (|| -> Result<String, BoardError> {
            if let Some(base) = base {
                self.repo.git_with_index(&index, &["read-tree", base])?;
            }
            self.repo.git_with_index(
                &index,
                &[
                    "update-index",
                    "--add",
                    "--cacheinfo",
                    &format!("100644,{blob},{path}"),
                ],
            )?;
            self.repo.git_with_index(&index, &["write-tree"])
        })();

        // Post-clean on every exit path, success or failure — not only after
        // success, which is what let this leak unboundedly. Best-effort: by
        // this point the write has already happened or definitely failed, so
        // a cleanup problem here is clutter, not corruption.
        let _ = std::fs::remove_file(&index);
        result
    }

    /// The ref's root commit — the invariant D13 protects.
    pub fn root(&self) -> Result<Option<String>, BoardError> {
        let Some(tip) = self.tip()? else {
            return Ok(None);
        };
        let roots = self.repo.git(&["rev-list", "--max-parents=0", &tip])?;
        Ok(roots.lines().next().map(str::to_string))
    }

    /// Drop posts `is_reapable` judges permanently dead from the tip tree,
    /// as a FORWARD commit whose parent is the current tip.
    ///
    /// Deliberately judged by `is_reapable`, not `liveness`: liveness
    /// governs what a render shows this time (wrong once, self-correcting);
    /// this governs what disappears from the tip tree forever. The renders
    /// already filter dead posts out at read time, so a dead post sitting
    /// in the tip tree is already invisible -- reap buys tidiness and tree
    /// size, not correctness, which is why it is held to the stricter,
    /// separate predicate (see `is_reapable`'s doc comment).
    ///
    /// History is untouched, so every post ever written stays reachable
    /// (D13): there is no code path here that rewrites or force-updates the
    /// ref to anything other than a descendant of the tip it read. Returns
    /// how many posts were dropped; a reap with nothing dead is a no-op (no
    /// empty commit).
    ///
    /// **Reads nothing.** Every input — the post set to judge, the liveness
    /// context to judge it with, and the tip to compare-and-swap against —
    /// comes out of the single [`ReapPlan`] handed in, which is itself built
    /// from one [`TipSnapshot`]. This method deliberately does not call
    /// `tip()` or `posts_at_tip()`: doing so was the fourth instance of this
    /// campaign's silent-permanent-loss class (see [`TipSnapshot`]), and the
    /// signature is what keeps it closed rather than a comment asking
    /// future edits not to.
    pub fn reap(&self, plan: &ReapPlan<'_>) -> Result<usize, BoardError> {
        use crate::live::is_reapable;

        let ctx = plan.context();
        let posts = plan.snapshot().posts();
        let old = plan.snapshot().tip().to_string();
        let keep: Vec<&StoredPost> = posts.iter().filter(|s| !is_reapable(s, ctx)).collect();
        let dropped = posts.len() - keep.len();
        if dropped == 0 {
            return Ok(0);
        }

        // A fresh throwaway index, never the repo's real one (`Repo::git_path`
        // is a private, per-worktree path) -- a reap must not dirty the
        // working tree. Named with a per-CALL discriminant, not just the
        // pid: two reaps (or a reap racing an append) in the same process
        // would otherwise collide on git's index lock. This is the exact
        // atomic `append_with_attempts` uses for the same reason, reused
        // here rather than reinvented -- see its doc comment above.
        let call_id = CALL_DISCRIMINANT.fetch_add(1, Ordering::SeqCst);
        let raw = self
            .repo
            .git_path(&format!("hv-board-reap-{}-{call_id}", std::process::id()))?;
        // `Repo::git_path` documents itself as always absolute; guard
        // defensively anyway, exactly as `tree_with` does, since a relative
        // path here would resolve the `std::fs` cleanup below against the
        // *process* cwd rather than the repo root.
        let index = if raw.is_absolute() {
            raw
        } else {
            self.repo.root().join(raw)
        };

        // Pre-clean: a leaked index from an earlier crashed run must not
        // contaminate this write.
        match std::fs::remove_file(&index) {
            Ok(()) => {}
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => {
                return Err(BoardError::Io(format!(
                    "removing stale throwaway index {}: {e}",
                    index.display()
                )));
            }
        }

        let result = (|| -> Result<String, BoardError> {
            // Built from the survivors only -- no `read-tree` of the old
            // tree first, so a dead post is never present to begin with,
            // not merely removed after the fact.
            for s in &keep {
                self.repo.git_with_index(
                    &index,
                    &[
                        "update-index",
                        "--add",
                        "--cacheinfo",
                        &format!("100644,{},posts/{}.json", s.id, s.id),
                    ],
                )?;
            }
            self.repo.git_with_index(&index, &["write-tree"])
        })();

        // Post-clean on every exit path, success or failure.
        let _ = std::fs::remove_file(&index);
        let tree = result?;

        let new = self.repo.git(&[
            "commit-tree",
            &tree,
            "-p",
            &old,
            "-m",
            &format!("board: reap {dropped}"),
        ])?;

        // `old` is the tip the SNAPSHOT was read from, not a tip re-read
        // after the probe -- so a post appended at any point after that read
        // makes this swap lose, and the reap becomes a no-op rather than
        // silently dropping a post the probe never saw. That is the half of
        // the fix the CAS carries; `ReapPlan` carries the other half.
        //
        // A lost race is control flow, not an error: reaping is idempotent
        // and cheap, so rather than retry against a moving target, let the
        // next run catch it and report nothing dropped THIS call, since the
        // tree we built no longer reflects the current tip.
        if self.cas(&new, Some(&old))?.is_some() {
            return Ok(0);
        }
        Ok(dropped)
    }

    /// Compare-and-swap the ref.
    ///
    /// `Ok(None)` means we won. `Ok(Some(stderr))` means we lost a *genuine*
    /// race — classified semantically, not by matching git's stderr text:
    /// after `update-ref` fails, re-read the tip. If it no longer equals
    /// `old`, something really did move the ref out from under us, so this
    /// is expected control flow and the caller should retry; git's stderr is
    /// returned so a caller that exhausts its attempts can report the last
    /// real reason. If the tip is still `old` (or we cannot even read it),
    /// nothing moved — the failure is permanent (a name collision, a
    /// permissions problem, …) and must not be retried away, so it comes back
    /// as `Err` with git's own diagnosis intact.
    ///
    /// D13 carve-out: a present `old` reading back as `None` is never treated
    /// as "the ref just doesn't exist yet" — see the guard below.
    fn cas(&self, new: &str, old: Option<&str>) -> Result<Option<String>, BoardError> {
        let result = match old {
            Some(old) => self.repo.git(&["update-ref", &self.refname, new, old]),
            None => self.repo.git_stdin(
                &["update-ref", "--stdin"],
                format!("create {} {new}\n", self.refname).as_bytes(),
            ),
        };
        let err = match result {
            Ok(_) => return Ok(None),
            Err(e) => e,
        };
        let current = self.tip();

        // D13: never treat a vanished ref as an as-yet-uncreated one. If we
        // had a tip (`old` was `Some`) and the ref now reads back as `None`,
        // it was deleted out from under us — the `moved` check below would
        // otherwise read this as an ordinary lost race (`None != old`), and
        // the retry would then take the `old == None` branch, build a tree
        // with no parent, and mint a fresh root: every earlier post silently
        // orphaned. That is unrecoverable in a way nothing else in this
        // function is, so it is always permanent, regardless of how
        // `update-ref`'s stderr happens to read.
        if old.is_some() && matches!(&current, Ok(None)) {
            return Err(BoardError::Git {
                cmd: format!("append to {}", self.refname),
                code: None,
                stderr: format!(
                    "the ref {} was deleted while we were writing to it; this is not a \
                     retryable race — retrying would build a tree with no parent and \
                     reroot the board, discarding its entire history (D13). git's \
                     original diagnosis: {}",
                    self.refname,
                    match &err {
                        BoardError::Git { stderr, .. } => stderr.as_str(),
                        _ => "unknown",
                    }
                ),
            });
        }

        let moved = matches!(&current, Ok(c) if c.as_deref() != old);
        if moved {
            let stderr = match &err {
                BoardError::Git { stderr, .. } => stderr.clone(),
                other => other.to_string(),
            };
            Ok(Some(stderr))
        } else {
            Err(err)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::git::test_support::temp_repo;

    #[test]
    fn append_creates_the_ref_and_touches_no_working_tree() {
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        assert_eq!(
            board.tip().expect("tip"),
            None,
            "no ref before the first post"
        );
        let id = board
            .append(&Post::new("claim", "campaign/x"))
            .expect("append");
        assert!(board.tip().expect("tip").is_some(), "ref exists after");
        assert_eq!(board.post_ids_at_tip().expect("ids"), vec![id]);
        let dirty = repo.git(&["status", "--porcelain"]).expect("status");
        assert!(
            dirty.is_empty(),
            "a board write must not dirty the checkout: {dirty:?}"
        );
    }

    #[test]
    fn the_same_post_appended_twice_is_one_file() {
        // D11: content addressing makes a double-record idempotent.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo);
        let p = Post::new("claim", "campaign/x");
        let a = board.append(&p).expect("first");
        let b = board.append(&p).expect("second");
        assert_eq!(a, b, "same content, same id");
        assert_eq!(board.post_ids_at_tip().expect("ids").len(), 1);
    }

    #[test]
    fn a_duplicate_append_does_not_grow_the_commit_chain() {
        // Q7: idempotence should hold at the commit level too, not just the
        // file level — a re-record of an already-present post must not add
        // an empty commit on top.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let p = Post::new("claim", "campaign/x");
        board.append(&p).expect("first");
        let tip_after_first = board.tip().expect("tip").expect("some");
        board.append(&p).expect("second");
        let tip_after_second = board.tip().expect("tip").expect("some");
        assert_eq!(
            tip_after_first, tip_after_second,
            "a duplicate append must not move the tip at all"
        );
        let chain_len = repo
            .git(&["rev-list", "--count", &tip_after_second])
            .expect("rev-list");
        assert_eq!(chain_len, "1", "still exactly one commit, not two");
    }

    #[test]
    fn appends_accumulate_and_never_reroot() {
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let first_tip = {
            board.append(&Post::new("claim", "a")).expect("a");
            board.tip().expect("tip").expect("some")
        };
        board.append(&Post::new("claim", "b")).expect("b");
        let tip = board.tip().expect("tip").expect("some");
        assert_eq!(board.post_ids_at_tip().expect("ids").len(), 2);
        let ancestor = repo.git(&["merge-base", "--is-ancestor", &first_tip, &tip]);
        assert!(
            ancestor.is_ok(),
            "D13: the tip must descend from every earlier tip"
        );
    }

    #[test]
    fn cas_treats_a_deleted_ref_as_permanent_not_a_lost_race() {
        // D13/A: a ref that vanishes between our tip-read and our
        // `update-ref` must never be treated as "no one has created it yet"
        // -- doing so would let a retry rebuild a tree with no parent and
        // mint a fresh root, orphaning every earlier post. Reproduces the
        // exact interleaving the re-review measured, directly against `cas`
        // (no second thread needed): append once, capture the tip and the
        // board's root commit, delete the ref out from under it, then
        // attempt a `cas` that still believes `old` is the captured tip --
        // exactly the state a caller's in-flight attempt would be in.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        board.append(&Post::new("claim", "a")).expect("a");
        let old_tip = board.tip().expect("tip").expect("some");
        let root = repo
            .git(&["rev-list", "--max-parents=0", &old_tip])
            .expect("root commit");

        repo.git(&["update-ref", "-d", BOARD_REF])
            .expect("delete the ref out from under the board");

        // The commit a caller would already have built before discovering
        // the deletion: parented on `old_tip`, exactly as `append_with_attempts`
        // constructs it.
        let tree = repo
            .git(&["rev-parse", "--verify", &format!("{old_tip}^{{tree}}")])
            .expect("tree");
        let new = repo
            .git(&["commit-tree", &tree, "-p", &old_tip, "-m", "would-be-next"])
            .expect("candidate commit");

        let err = board
            .cas(&new, Some(&old_tip))
            .expect_err("a deleted ref must be a permanent failure, not a lost race");
        let BoardError::Git { stderr, .. } = err else {
            panic!("expected BoardError::Git, got a different variant");
        };
        assert!(
            stderr.contains("deleted"),
            "the error should say the ref was deleted, not just that it moved: {stderr}"
        );
        assert!(
            !stderr.contains(EXHAUSTION_MARKER),
            "this is a single-call classification, not exhaustion: {stderr}"
        );
        assert!(
            board.tip().expect("tip").is_none(),
            "the rejected cas must not have created a fresh root: the ref must stay absent"
        );

        // Positive half of the same invariant: once the ref is legitimately
        // restored (as an operator recovering from the deletion would do)
        // and a normal append follows, the original root commit is still
        // there and still an ancestor -- D13 holds on the path that is
        // supposed to succeed, not only rejected on the path that must not.
        repo.git(&["update-ref", BOARD_REF, &old_tip])
            .expect("restore the ref");
        board
            .append(&Post::new("claim", "b"))
            .expect("recovered append");
        let new_tip = board.tip().expect("tip").expect("some");
        let ancestor = repo.git(&["merge-base", "--is-ancestor", &root, &new_tip]);
        assert!(
            ancestor.is_ok(),
            "the original root must still be an ancestor of a legitimately continued history"
        );
    }

    #[test]
    fn cas_reports_a_lost_race_when_the_ref_moved_away_from_the_expected_old_value() {
        // Q1/I1: a deterministic stand-in for a real race. `update-ref`'s
        // expected-value check fails exactly the same way whether the ref
        // moved because of a concurrent writer or, as here, because we hand
        // it a stale `old` on purpose — and `cas` must classify both as a
        // lost race, not a permanent failure.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        board.append(&Post::new("claim", "a")).expect("a");
        let a_tip = board.tip().expect("tip").expect("some");
        board.append(&Post::new("claim", "b")).expect("b");
        let b_tip = board.tip().expect("tip").expect("some");

        let lost = board
            .cas(&b_tip, Some(&a_tip))
            .expect("cas must not error on a genuine race");
        assert!(
            lost.is_some(),
            "the ref moved out from under `old`, so this must be reported as a lost race"
        );
        assert_eq!(
            board.tip().expect("tip").expect("some"),
            b_tip,
            "a lost race must not have moved the ref again"
        );
    }

    #[test]
    fn cas_reports_a_real_error_on_a_permanent_df_conflict() {
        // Q1/I1/M2: the reviewer's D/F-conflict setup, isolated. `new` here
        // is a real, valid commit (not a bogus oid), so a D/F conflict is
        // the *only* anomaly in play — this test pins that cause
        // specifically, unlike the original version which also passed a
        // bogus oid and could not tell the two apart. A ref already exists
        // *under* the board's own ref path, so creating a leaf ref there
        // fails every single time — permanently, not by chance — and the
        // board's own ref never budges. `cas` must not classify this as a
        // lost race.
        let (_d, repo) = temp_repo();
        let scratch = Board::with_ref(repo.clone(), "refs/test/scratch");
        scratch
            .append(&Post::new("claim", "x"))
            .expect("scratch append");
        let commit = scratch.tip().expect("tip").expect("some");
        repo.git(&["update-ref", &format!("{BOARD_REF}/child"), &commit])
            .expect("child ref");

        let board = Board::new(repo.clone());
        let err = board
            .cas(&commit, None)
            .expect_err("a D/F conflict is permanent even with a perfectly valid object");
        let BoardError::Git { stderr, .. } = err else {
            panic!("expected BoardError::Git, got a different variant");
        };
        assert!(
            stderr.contains(BOARD_REF),
            "should carry git's own diagnosis: {stderr}"
        );
        assert!(
            board.tip().expect("tip").is_none(),
            "the failed create must not have made the ref exist"
        );
    }

    #[test]
    fn cas_reports_a_real_error_on_a_nonexistent_object() {
        // M2's other half: isolates the "bogus object" cause on its own,
        // with no D/F conflict set up at all, so a nonexistent `new` is the
        // only possible reason this can fail. Confirms the classification
        // is not accidentally tied to the D/F conflict's specific stderr
        // shape — any permanent, non-retryable git failure must come back
        // as `Err`, not just this one.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let err = board
            .cas("deadbeefdeadbeefdeadbeefdeadbeefdeadbeef", None)
            .expect_err("a nonexistent object must not be classified as a lost race");
        let BoardError::Git { stderr, .. } = err else {
            panic!("expected BoardError::Git, got a different variant");
        };
        assert!(
            stderr.contains("nonexistent object"),
            "should carry git's own diagnosis: {stderr}"
        );
        assert!(
            board.tip().expect("tip").is_none(),
            "the failed create must not have made the ref exist"
        );
    }

    #[test]
    fn append_propagates_a_permanent_failure_without_exhausting_attempts() {
        // The user-facing half of the previous test: `append` itself must
        // surface a permanent D/F conflict immediately, not spin through its
        // attempt budget treating it as contention. Checked by content, not
        // by timing (a wall-clock threshold here is at the mercy of however
        // busy the machine running the suite happens to be): give it a
        // budget of exactly 1. If the conflict were misclassified as a lost
        // race, that single attempt would be consumed and the failure would
        // come back as the generic "lost N compare-and-swap races"
        // exhaustion message instead of git's own diagnosis -- so the two
        // failure modes are textually distinguishable regardless of speed.
        let (_d, repo) = temp_repo();
        let scratch = Board::with_ref(repo.clone(), "refs/test/scratch");
        scratch
            .append(&Post::new("claim", "x"))
            .expect("scratch append");
        let commit = scratch.tip().expect("tip").expect("some");
        repo.git(&["update-ref", &format!("{BOARD_REF}/child"), &commit])
            .expect("child ref");

        let board = Board::new(repo.clone());
        let err = board
            .append_with_attempts(&Post::new("claim", "y"), 1)
            .expect_err("a permanent D/F conflict must not be retried away as a race");
        let BoardError::Git { stderr, .. } = err else {
            panic!("expected BoardError::Git, got a different variant");
        };
        assert!(
            !stderr.contains(EXHAUSTION_MARKER),
            "a permanent failure must propagate git's own diagnosis, not the generic exhaustion message: {stderr}"
        );
        assert!(
            stderr.contains(BOARD_REF),
            "the propagated error should carry git's own diagnosis: {stderr}"
        );
    }

    #[test]
    fn exhaustion_is_loud_and_names_the_ref_and_the_post() {
        // I3: make the exhaustion arm reachable without waiting on chance.
        // Attackers use raw plumbing (reuse one tree, `commit-tree` +
        // `update-ref`; no `tree_with`) so each of their iterations is
        // several times cheaper than the victim's real `append_with_attempts`
        // attempt (which builds a throwaway index, diffs the tree, and
        // commits). That speed asymmetry, not luck, is what makes a 2-attempt
        // budget against a dozen of them exhaust reliably -- verified with
        // repeated reruns (see the report).
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        board
            .append(&Post::new("claim", "genesis"))
            .expect("genesis");
        let genesis_tip = board.tip().expect("tip").expect("some");
        let tree = repo
            .git(&["rev-parse", "--verify", &format!("{genesis_tip}^{{tree}}")])
            .expect("tree");

        let stop = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let attackers: Vec<_> = (0..12)
            .map(|_| {
                let attacker_repo = repo.clone();
                let attacker_stop = stop.clone();
                let attacker_tree = tree.clone();
                std::thread::spawn(move || {
                    while !attacker_stop.load(Ordering::Relaxed) {
                        let Ok(Some(old)) = attacker_repo.rev_parse_verify(BOARD_REF) else {
                            continue;
                        };
                        let Ok(new) = attacker_repo.git(&[
                            "commit-tree",
                            &attacker_tree,
                            "-p",
                            &old,
                            "-m",
                            "attack",
                        ]) else {
                            continue;
                        };
                        let _ = attacker_repo.git(&["update-ref", BOARD_REF, &new, &old]);
                    }
                })
            })
            .collect();

        let result = board.append_with_attempts(&Post::new("claim", "victim"), 2);
        stop.store(true, Ordering::Relaxed);
        for attacker in attackers {
            attacker.join().expect("attacker thread");
        }

        let err = result
            .expect_err("a tiny attempt budget against sustained contention must fail loudly");
        let BoardError::Git { stderr, .. } = err else {
            panic!("expected BoardError::Git, got a different variant");
        };
        assert!(
            stderr.contains(&board.refname),
            "the error should name the ref: {stderr}"
        );
        assert!(
            stderr.contains("NOT recorded"),
            "the error should say the post was not recorded: {stderr}"
        );
    }

    /// Create `branch` off `main`, commit once on it, merge it back into
    /// `main` (`--no-ff`, so the merge is its own commit), and leave the
    /// checkout on `main`. `main` needs a root commit of its own first --
    /// the board's commits all live on `refs/hornvale/board`, entirely
    /// separate from this checkout, so a fresh `temp_repo()` has no commit
    /// on `main` to branch from until this helper adds one.
    fn merge_branch_into_main(repo: &crate::git::Repo, branch: &str) {
        std::fs::write(repo.root().join("root.txt"), "root").expect("write root");
        repo.git(&["add", "root.txt"]).expect("add root");
        repo.git(&["commit", "-m", "root"]).expect("commit root");
        repo.git(&["checkout", "-q", "-b", branch]).expect("branch");
        std::fs::write(repo.root().join("work.txt"), branch).expect("write work");
        repo.git(&["add", "work.txt"]).expect("add work");
        repo.git(&["commit", "-m", "work"]).expect("commit work");
        repo.git(&["checkout", "-q", "main"]).expect("back to main");
        repo.git(&["merge", "--no-ff", "-m", "merge", branch])
            .expect("merge");
    }

    #[test]
    fn reap_drops_a_notice_whose_branch_resolves_and_is_merged_regardless_of_age() {
        // The unambiguous half of the split predicate: a resolved-and-merged
        // branch is a positive statement no transient race can produce, so
        // this notice is reaped on the very first reap -- no grace period.
        let (_d, repo) = temp_repo();
        merge_branch_into_main(&repo, "campaign/merged");
        let board = Board::new(repo.clone());
        let dead = board
            .append(
                &Post::new("notice", "campaign/merged").with("note", serde_json::json!("stale")),
            )
            .expect("dead post");
        let durable = board
            .append(&Post::new("technique", "campaign/x"))
            .expect("technique");
        let root_before = board.root().expect("root");

        let snapshot = board.snapshot().expect("snapshot").expect("some");
        let plan = ReapPlan::probe(&repo, &snapshot).expect("probe");
        let dropped = board.reap(&plan).expect("reap");

        assert_eq!(
            dropped, 1,
            "a merged branch is unambiguous, so this must not wait on age"
        );
        let ids = board.post_ids_at_tip().expect("ids");
        assert!(!ids.contains(&dead), "gone from the tip tree");
        assert!(ids.contains(&durable), "technique survives");
        assert_eq!(
            board.root().expect("root"),
            root_before,
            "D13: the ref was not rerooted"
        );
        let still_there = repo
            .git(&["cat-file", "-t", &dead])
            .expect("object still exists");
        assert_eq!(
            still_there, "blob",
            "the post is still reachable through history"
        );
    }

    #[test]
    fn reap_does_not_drop_an_unresolved_notice_within_the_grace_period_the_hazard_regression() {
        // THE HAZARD REGRESSION: a branch that does not resolve at all is
        // ambiguous -- it could be a transient race (a fetch in flight, a
        // permissions blip), not genuine, permanent absence. A young notice
        // on such a branch must survive a reap, or a wrongly-observed race
        // would permanently erase it from the tip tree.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let notice = board
            .append(
                &Post::new("notice", "campaign/never-existed")
                    .with("note", serde_json::json!("stale")),
            )
            .expect("notice");
        board
            .append(&Post::new("technique", "campaign/x"))
            .expect("technique");
        let before = board.tip().expect("tip");

        let snapshot = board.snapshot().expect("snapshot").expect("some");
        // Fresh probe: the notice's age relative to `now_unix` is ~0,
        // nowhere near `NOTICE_GRACE_PERIOD_S`.
        let plan = ReapPlan::probe(&repo, &snapshot).expect("probe");
        let dropped = board.reap(&plan).expect("reap");

        assert_eq!(
            dropped, 0,
            "an unresolved branch is ambiguous while young -- must not be reaped yet"
        );
        assert_eq!(
            board.tip().expect("tip"),
            before,
            "no commit for a no-op reap"
        );
        let ids = board.post_ids_at_tip().expect("ids");
        assert!(ids.contains(&notice), "the notice must still be present");
    }

    #[test]
    fn reap_drops_the_same_unresolved_notice_once_past_the_grace_period() {
        // Same setup as the hazard regression above, but with the clock
        // advanced past `NOTICE_GRACE_PERIOD_S`: a branch that has stayed
        // unresolved that long is durably gone, not a blip.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let dead = board
            .append(
                &Post::new("notice", "campaign/never-existed")
                    .with("note", serde_json::json!("stale")),
            )
            .expect("notice");
        let durable = board
            .append(&Post::new("technique", "campaign/x"))
            .expect("technique");
        let root_before = board.root().expect("root");

        let snapshot = board.snapshot().expect("snapshot").expect("some");
        let mut plan = ReapPlan::probe(&repo, &snapshot).expect("probe");
        plan.advance_clock(crate::live::NOTICE_GRACE_PERIOD_S + 1);
        let dropped = board.reap(&plan).expect("reap");

        assert_eq!(dropped, 1, "past the grace period, the notice is reapable");
        let ids = board.post_ids_at_tip().expect("ids");
        assert!(!ids.contains(&dead), "gone from the tip tree");
        assert!(ids.contains(&durable), "technique survives");
        assert_eq!(
            board.root().expect("root"),
            root_before,
            "D13: the ref was not rerooted"
        );
        let still_there = repo
            .git(&["cat-file", "-t", &dead])
            .expect("object still exists");
        assert_eq!(
            still_there, "blob",
            "the post is still reachable through history"
        );
    }

    #[test]
    fn reap_still_drops_a_claim_past_its_ttl() {
        // No regression: claim expiry is unambiguous (time only moves
        // forward), so it is reaped exactly as before this task's change.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let dead = board
            .append(&Post::new("claim", "campaign/x").with("ttl_s", serde_json::json!(1)))
            .expect("claim");
        let durable = board
            .append(&Post::new("technique", "campaign/x"))
            .expect("technique");

        let snapshot = board.snapshot().expect("snapshot").expect("some");
        let mut plan = ReapPlan::probe(&repo, &snapshot).expect("probe");
        plan.advance_clock(60); // well past the 1s ttl

        let dropped = board.reap(&plan).expect("reap");
        assert_eq!(dropped, 1, "a claim past its ttl is unambiguous");
        let ids = board.post_ids_at_tip().expect("ids");
        assert!(!ids.contains(&dead));
        assert!(ids.contains(&durable));
    }

    #[test]
    fn reap_with_nothing_dead_is_a_no_op_and_does_not_commit() {
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        board
            .append(&Post::new("technique", "campaign/x"))
            .expect("post");
        let before = board.tip().expect("tip");
        let snapshot = board.snapshot().expect("snapshot").expect("some");
        let plan = ReapPlan::probe(&repo, &snapshot).expect("probe");
        assert_eq!(board.reap(&plan).expect("reap"), 0);
        assert_eq!(board.tip().expect("tip"), before, "no empty commit");
    }

    #[test]
    fn reap_does_not_dirty_the_working_tree() {
        let (_d, repo) = temp_repo();
        merge_branch_into_main(&repo, "campaign/merged");
        let board = Board::new(repo.clone());
        board
            .append(&Post::new("notice", "campaign/merged"))
            .expect("dead post");
        board
            .append(&Post::new("technique", "campaign/x"))
            .expect("technique");
        let snapshot = board.snapshot().expect("snapshot").expect("some");
        let plan = ReapPlan::probe(&repo, &snapshot).expect("probe");
        let dropped = board.reap(&plan).expect("reap");
        assert_eq!(dropped, 1, "sanity: this reap must actually do something");
        let dirty = repo.git(&["status", "--porcelain"]).expect("status");
        assert!(
            dirty.is_empty(),
            "a reap must not dirty the checkout: {dirty:?}"
        );
    }

    #[test]
    fn a_post_appended_after_the_snapshot_loses_the_reap_cas_and_is_not_dropped() {
        // C2, THE FOURTH SILENT-LOSS REGRESSION. `reap` used to read the
        // board itself, so a post appended between the caller's read (which
        // the `ps` probe ran over) and `reap`'s own read was present in the
        // reap set but absent from `ctx.live_pids` -- it read as pid-dead,
        // was dropped from the tip tree, and the CAS still succeeded because
        // `reap` captured `old` AFTER the probe. A live claim announcing "I
        // am using this box" vanished permanently: the exact double-start
        // this board exists to prevent.
        //
        // Reproduced here by appending inside that window, deterministically:
        // snapshot, probe, THEN append. The claim names this host and this
        // very process's pid, so it is genuinely, verifiably live -- there is
        // no reading of the world under which dropping it is correct.
        let (_d, repo) = temp_repo();
        merge_branch_into_main(&repo, "campaign/merged");
        let board = Board::new(repo.clone());
        let dead = board
            .append(&Post::new("notice", "campaign/merged").with("note", serde_json::json!("old")))
            .expect("a genuinely reapable post, so the reap is not a no-op for another reason");

        let snapshot = board.snapshot().expect("snapshot").expect("some");
        let plan = ReapPlan::probe(&repo, &snapshot).expect("probe");

        // The window: another session posts after the probe.
        let host = std::process::Command::new("hostname")
            .arg("-s")
            .output()
            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
            .expect("hostname");
        let latecomer = board
            .append(
                &Post::new("claim", "campaign/other")
                    .with("host", serde_json::json!(host))
                    .with("pid", serde_json::json!(std::process::id()))
                    .with("ttl_s", serde_json::json!(3_600)),
            )
            .expect("the latecomer claim");

        let dropped = board.reap(&plan).expect("reap");
        assert_eq!(
            dropped, 0,
            "the tip moved after the snapshot, so the CAS must lose and the reap \
             must be a no-op -- not a partial compaction against a stale picture"
        );
        let ids = board.post_ids_at_tip().expect("ids");
        assert!(
            ids.contains(&latecomer),
            "a live claim appended inside the probe window must NOT be dropped: {ids:?}"
        );
        assert!(
            ids.contains(&dead),
            "and nothing else may be dropped either, since the whole reap lost the race: {ids:?}"
        );
    }

    #[test]
    fn a_snapshot_is_a_frozen_reading_the_reap_cannot_widen() {
        // The structural half of C2's fix, stated as a property rather than
        // trusted from the signature: a `TipSnapshot` taken before an append
        // reports the tip and the posts as they were AT THAT MOMENT, and
        // stays that way. `reap` takes only this value, so it cannot see the
        // later post at all -- there is no post set it could reap that the
        // probe did not see.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let first = board
            .append(&Post::new("technique", "campaign/x"))
            .expect("a");
        let snapshot = board.snapshot().expect("snapshot").expect("some");
        let second = board
            .append(&Post::new("technique", "campaign/y"))
            .expect("b");

        let ids: Vec<&str> = snapshot.posts().iter().map(|sp| sp.id.as_str()).collect();
        assert_eq!(
            ids,
            vec![first.as_str()],
            "the snapshot must not have grown to include a later append"
        );
        assert_ne!(
            snapshot.tip(),
            board.tip().expect("tip").expect("some"),
            "and its tip must be the one it read, not the current one"
        );
        assert!(!second.is_empty());
    }

    /// Splice raw `bytes` into the tip tree as a post file, forward-only,
    /// bypassing `append`.
    ///
    /// Deliberately not expressible through the crate's own write path:
    /// `canonical_bytes` gates every post this tool writes, so a corrupt post
    /// can only arrive from outside — a clone, an older version of the tool,
    /// or a hand write — which is exactly why the skip-and-warn arms exist
    /// and exactly why testing them needs this.
    fn splice_raw_post(board: &Board, repo: &Repo, bytes: &[u8]) -> String {
        let blob = repo.hash_object(bytes).expect("hash-object");
        splice_raw_post_at(board, repo, &format!("posts/{blob}.json"), bytes);
        blob
    }

    /// [`splice_raw_post`]'s general form: splice `bytes` under an arbitrary
    /// `path`, not necessarily one named by the bytes' own object id.
    ///
    /// Exists for the malformed-*filename* case, distinct from the
    /// malformed-*content* case `splice_raw_post` covers: a tree entry whose
    /// name is not any real object's id is exactly as reachable from outside
    /// this tool (a clone, an older version, a hand write) as corrupt bytes
    /// are, and nothing at write time stops it — `append` only ever names a
    /// file by its own blob's id, which is why the tool's own writes can
    /// never produce one.
    fn splice_raw_post_at(board: &Board, repo: &Repo, path: &str, bytes: &[u8]) {
        let blob = repo.hash_object(bytes).expect("hash-object");
        let old = board.tip().expect("tip").expect("some");
        let call_id = CALL_DISCRIMINANT.fetch_add(1, Ordering::SeqCst);
        let index = repo
            .git_path(&format!("hv-board-splice-{}-{call_id}", std::process::id()))
            .expect("index path");
        let _ = std::fs::remove_file(&index);
        repo.git_with_index(&index, &["read-tree", &old])
            .expect("read-tree");
        repo.git_with_index(
            &index,
            &[
                "update-index",
                "--add",
                "--cacheinfo",
                &format!("100644,{blob},{path}"),
            ],
        )
        .expect("update-index");
        let tree = repo
            .git_with_index(&index, &["write-tree"])
            .expect("write-tree");
        let _ = std::fs::remove_file(&index);
        let new = repo
            .git(&["commit-tree", &tree, "-p", &old, "-m", "a spliced post"])
            .expect("commit-tree");
        repo.git(&["update-ref", board.refname(), &new, &old])
            .expect("update-ref");
    }

    #[test]
    fn one_unparseable_post_does_not_hide_the_rest_of_the_board() {
        // Spec test-plan item 9, and D7's promise that "one corrupt post must
        // never break a session's render". The skip-and-warn arm in
        // `posts_in` was entirely unexercised: replacing it with `?` kept the
        // whole suite green, which would have made D7 false with nothing to
        // say so (see the mutation check in the fix-wave report).
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let good = board
            .append(
                &Post::new("technique", "campaign/x").with("note", serde_json::json!("keep me")),
            )
            .expect("good post");
        let bad = splice_raw_post(&board, &repo, b"this is not json at all\n");

        let posts = board
            .posts_at_tip()
            .expect("a corrupt post must be SKIPPED, never turned into an Err");
        let ids: Vec<&str> = posts.iter().map(|sp| sp.id.as_str()).collect();
        assert_eq!(
            ids,
            vec![good.as_str()],
            "the good post must survive and the corrupt one must be skipped: {ids:?}"
        );
        assert!(
            board.post_ids_at_tip().expect("ids").contains(&bad),
            "sanity: the corrupt file really is at the tip, so this test is \
             exercising the skip arm rather than an empty tree"
        );
    }

    #[test]
    fn a_malformed_post_filename_is_skipped_not_a_reason_to_blank_the_whole_board() {
        // The companion regression the review's fix round 2 asked for:
        // `one_unparseable_post_does_not_hide_the_rest_of_the_board` (above)
        // covers corrupt CONTENT under a valid filename (a real object id).
        // This covers the distinct failure mode -- a malformed FILENAME
        // (not any real object's id) holding otherwise well-formed content.
        // `resilience.rs` only ever exercises corrupt bytes, never a bad
        // path, which is exactly the gap that let an earlier version of
        // `cat_file_batch` respond to this case with a hard `Err` that
        // blanked the entire board (every post lost, not just this one) --
        // worse than D7's ordinary corrupt-post case, and never caught by a
        // test until now.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let good = board
            .append(
                &Post::new("technique", "campaign/x").with("note", serde_json::json!("keep me")),
            )
            .expect("good post");
        // Well-formed content, but filed under a filename that is not any
        // real object's id -- git happily stores this; nothing at write
        // time validates a tree entry's path against its own blob.
        let post_bytes = Post::new("technique", "campaign/x")
            .with("note", serde_json::json!("malformed filename"))
            .canonical_bytes()
            .expect("canonical bytes");
        splice_raw_post_at(&board, &repo, "posts/deadbeef.json", &post_bytes);

        let posts = board
            .posts_at_tip()
            .expect("a malformed filename must be SKIPPED, never turned into an Err");
        let ids: Vec<&str> = posts.iter().map(|sp| sp.id.as_str()).collect();
        assert_eq!(
            ids,
            vec![good.as_str()],
            "the good post must survive and the malformed filename must be skipped: {ids:?}"
        );

        let tip = board.tip().expect("tip").expect("some");
        let listed = repo
            .git(&["ls-tree", "-r", "--name-only", &tip])
            .expect("ls-tree");
        assert!(
            listed.contains("posts/deadbeef.json"),
            "sanity: the malformed filename really is at the tip, so this test is \
             exercising the skip arm rather than an empty tree: {listed:?}"
        );
    }

    #[test]
    fn a_post_that_parses_but_fails_the_attribution_guard_is_also_skipped_not_fatal() {
        // The other half of the same arm: well-formed JSON that `from_json`
        // rejects on its own guards (D7c -- a blank `by` is not attribution).
        // `canonical_bytes` cannot emit this, so it can only arrive from
        // outside, which is the case the arm is for.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let good = board
            .append(&Post::new("notice", "campaign/x"))
            .expect("good post");
        splice_raw_post(&board, &repo, br#"{"kind":"notice","by":"   "}"#);

        let posts = board.posts_at_tip().expect("must skip, not error");
        let ids: Vec<&str> = posts.iter().map(|sp| sp.id.as_str()).collect();
        assert_eq!(ids, vec![good.as_str()], "only the good post: {ids:?}");
    }

    #[test]
    fn a_git_failure_is_an_error_not_an_empty_board() {
        // The distinction the whole read path rests on: "could not read the
        // board" must never be indistinguishable from "the board is empty".
        // Injected rather than reasoned about -- a repo root that is not a
        // repository at all.
        let repo = Repo::new("/nonexistent-hornvale-board-path");
        let board = Board::new(repo);
        assert!(
            board.posts_at_tip().is_err(),
            "a git failure must surface as Err, never as Ok(vec![])"
        );
        assert!(
            board.snapshot().is_err(),
            "and the snapshot a reap is built from must fail loudly too"
        );
    }

    #[test]
    fn cat_file_batch_returns_every_requested_object_and_skips_a_missing_one() {
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let board = Board::new(repo.clone());
        let a = board
            .append(&Post::new("technique", "main").with("note", serde_json::json!("alpha")))
            .expect("a");
        let b = board
            .append(&Post::new("technique", "main").with("note", serde_json::json!("beta")))
            .expect("b");
        let absent = "0".repeat(40);

        let got = board
            .cat_file_batch(&[a.clone(), b.clone(), absent.clone()])
            .expect("batch");

        assert_eq!(
            got.len(),
            2,
            "the missing object must be absent, not an error"
        );
        assert!(!got.contains_key(&absent));
        let text = String::from_utf8(got[&a].clone()).expect("utf8");
        assert!(text.contains("alpha"), "got {text:?}");
        assert!(String::from_utf8_lossy(&got[&b]).contains("beta"));
    }

    #[test]
    fn cat_file_batch_never_panics_on_a_non_full_oid_even_when_called_directly() {
        // Regression test for fix round 3's item 1. `digest.rs`'s `history`
        // walk harvests its own ids from `git log --diff-filter=A
        // --name-only` and calls `cat_file_batch` directly -- Task 4 routes
        // exactly that through this function, never through
        // `post_ids_in`. Fix round 2 put the non-full-oid guard behind a
        // `debug_assert!` here, reasoning `post_ids_in` was the one
        // upstream choke point; fix round 3's review simulated Task 4 and
        // found the assert IS reachable this way, and it fires as a
        // debug-only panic -- exit 101, empty stdout -- on a build profile
        // `make board-digest` always uses (`cargo run` with no `--release`).
        // A panic on untrusted repository data is not an improvement on the
        // hard `Err` fix round 2 replaced; it is the identical failure
        // shape with a louder exit code and a narrower reproduction window
        // (debug only). This calls `cat_file_batch` the same way a
        // Task-4-shaped caller would -- directly, with an id that never
        // passed through `post_ids_in` -- and must return `Ok` with the
        // bad id simply absent, under a debug build, not panic.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let board = Board::new(repo.clone());
        let good = board
            .append(&Post::new("technique", "main").with("note", serde_json::json!("keep me")))
            .expect("good post");

        let got = board
            .cat_file_batch(&[good.clone(), "deadbeef".to_string()])
            .expect("a non-full-oid id must be skipped, never panic or Err, even in debug");

        assert_eq!(got.len(), 1, "only the good post should come back: {got:?}");
        assert!(got.contains_key(&good));
    }

    #[test]
    fn cat_file_batch_frames_by_byte_length_not_by_newlines() {
        // The regression guard for the reason this is a bytes API: a post whose
        // note contains a newline must not truncate the record, and the record
        // after it must still parse.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let board = Board::new(repo.clone());
        let first = board
            .append(
                &Post::new("technique", "main")
                    .with("note", serde_json::json!("line one\nline two")),
            )
            .expect("first");
        let second = board
            .append(&Post::new("technique", "main").with("note", serde_json::json!("after")))
            .expect("second");

        let got = board
            .cat_file_batch(&[first.clone(), second.clone()])
            .expect("batch");

        assert_eq!(got.len(), 2);
        assert!(String::from_utf8_lossy(&got[&first]).contains("line two"));
        assert!(String::from_utf8_lossy(&got[&second]).contains("after"));
    }

    #[test]
    fn an_ambiguous_status_token_costs_one_id_not_the_rest_of_the_chunk() {
        // Regression test for fix round 3's item 2: a header whose status is
        // a recognised token OTHER than "missing" (git's "ambiguous",
        // "dangling", "notdir") must cost exactly the one id that produced
        // it, not every id still queued behind it in the same chunk. Before
        // this fix, any second-token value other than "missing" fell through
        // to the size parse, found no third field, and `break`, silently
        // dropping every subsequent id in the chunk -- demonstrated
        // end-to-end in the fix-round report with walk order
        // good1, deadbeef, 5093, good2: the unbatched read kept both good
        // posts, the batched one silently lost the second.
        //
        // "x1006" and "x3205" are not meaningful content -- they are two
        // magic strings, found once by an offline birthday search over
        // ~4000 candidates, whose git blob object ids (a content-only hash,
        // independent of which repository holds them) both start with the
        // same 4-hex-char prefix:
        //   cf6e7b5aa2d6cc5cb24323c224d9481d5afe3a5b  ("x1006")
        //   cf6e33624013139ce10daeeb07ef4a512212cfcb  ("x3205")
        // so asking `git cat-file --batch` for the bare prefix "cf6e"
        // reproducibly answers `cf6e ambiguous`, with no body -- on real
        // git, no mocking, the same status the review's own repro
        // (`printf '5093\n<oid>\n' | git cat-file --batch` -> `5093
        // ambiguous`) demonstrated.
        //
        // `is_full_object_id` makes this unreachable through the public
        // `cat_file_batch`: a full 40-hex oid is never itself ambiguous, so
        // every id that function accepts already can't trigger this. This
        // test calls the lower-level `cat_file_batch_chunk` directly,
        // bypassing that filter, specifically to exercise the header-parsing
        // match arm on its own -- the defensive code fix round 3's review
        // asked to keep even though the filter makes it unreachable in
        // practice today.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let board = Board::new(repo.clone());
        let good1 = board
            .append(
                &Post::new("technique", "main").with("note", serde_json::json!("FIRSTGOODPOST")),
            )
            .expect("good1");
        let good2 = board
            .append(
                &Post::new("technique", "main").with("note", serde_json::json!("LATERGOODPOST")),
            )
            .expect("good2");
        repo.hash_object(b"x1006").expect("hash x1006");
        repo.hash_object(b"x3205").expect("hash x3205");

        let mut found = std::collections::BTreeMap::new();
        board
            .cat_file_batch_chunk(
                &[good1.clone(), "cf6e".to_string(), good2.clone()],
                &mut found,
            )
            .expect("chunk must not error on an ambiguous status token");

        assert!(
            found.contains_key(&good1),
            "the good post BEFORE the ambiguous id must still be read: {found:?}"
        );
        assert!(
            found.contains_key(&good2),
            "the good post AFTER the ambiguous id must still be read -- this is exactly what \
             an old `break` on any unrecognised token would have silently lost: {found:?}"
        );
    }

    #[test]
    fn cat_file_batch_does_not_deadlock_on_a_batch_large_enough_to_fill_both_pipes() {
        // Regression test for fix round 1's Critical: `Repo::git_stdin_bytes`
        // writes the whole request before it reads any of the response. Once
        // the accumulated OUTPUT `cat-file --batch` would produce fills
        // git's stdout pipe before git has drained our stdin, git blocks
        // writing output nobody is reading yet, which stops it reading
        // further input, which leaves OUR write blocked on a pipe git will
        // never come back to drain -- a silent, timeout-free hang, not a
        // slow path. It cost 42 minutes of wall clock before it was
        // diagnosed; `cat_file_batch` now chunks requests (see
        // CAT_FILE_BATCH_CHUNK) specifically so this cannot happen.
        //
        // Reproduced independently against the review's own numbers at
        // n=4000 with ~500-byte objects (confirmed directly against the
        // unchunked helper before this test was written -- see the fix-round
        // report). This constructs an equivalent payload: the SAME 500-byte
        // object requested 4000 times, since git still emits one full
        // record per requested line regardless of whether the underlying
        // object is the same each time -- 4000 distinct objects are not
        // needed to fill both pipes.
        //
        // This crate runs under plain `cargo test`, which has NO per-test
        // timeout, so a naive version of this test would itself be a hang
        // generator on a regression, not a guard. Bounded instead: the call
        // runs on its own thread, and the test waits on a channel with a
        // timeout rather than joining that thread. If chunking ever
        // regresses, this test FAILS after the timeout (and leaks the stuck
        // thread, harmlessly, until the process exits) instead of hanging
        // the run.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let board = Board::new(repo.clone());
        let id = repo
            .hash_object(&vec![b'x'; 500])
            .expect("hash-object a 500-byte blob");
        let ids: Vec<String> = std::iter::repeat_n(id.clone(), 4_000).collect();

        let (tx, rx) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            let result = board.cat_file_batch(&ids);
            // The receiver may already be gone if this races past the
            // timeout below; nothing left to report to, which is fine.
            let _ = tx.send(result);
        });

        let result = rx.recv_timeout(std::time::Duration::from_secs(10)).expect(
            "cat_file_batch did not return within 10s -- this is the write-before-read \
             pipe deadlock regressing, not a slow call",
        );
        let got = result.expect("batch");
        assert_eq!(
            got.len(),
            1,
            "4000 requests for the same id collapse to one entry"
        );
        assert_eq!(got[&id].len(), 500);
    }
}
