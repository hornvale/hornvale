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
/// `Repo::git_stdin_bytes` writes the whole request before reading any
/// response, so a chunk's INPUT must stay under the pipe buffer or both
/// sides deadlock: git blocks on its own full stdout, stops draining our
/// stdin, and our write never completes (no timeout anywhere in that
/// chain). Each id line is 41 bytes (40-hex oid + newline):
/// `256 * 41 = 10,496` bytes, against a measured 65,536-byte pipe capacity
/// on this host — roughly a 6x margin. Check any change to this constant
/// against that margin (the argument needs staying UNDER it), not against
/// an unrelated floor.
const CAT_FILE_BATCH_CHUNK: usize = 256;

/// True if `id` is a full, unabbreviated git object id: exactly 40 lowercase
/// hex characters (this project's objects are SHA-1).
///
/// [`Board::cat_file_batch`] requires this: `cat-file --batch` echoes the
/// *resolved* id in its header, not whatever was asked for, so anything
/// else (an abbreviation, a ref name) would key the result map under a
/// different string and silently miss.
///
/// Deliberately SHA-1-only: a SHA-256 repository (`git init
/// --object-format=sha256`) degrades gracefully on this check (warns and
/// skips every post, exit 0) rather than crashing, but is not supported —
/// hornvale has no SHA-256 repository today.
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

/// Collapse a union read to one record per post id, oldest first.
///
/// Deduplication is free rather than clever: an id IS the object id of the
/// post's own bytes (D11), so the same post fetched from two hosts is the
/// same string twice, with nothing to reconcile. Two fields still have to
/// choose:
///
/// - `committed_at` keeps the **earliest** occurrence, the same rule
///   [`crate::digest::history`] already applies to a reap-then-repost of
///   byte-identical content. One rule for one situation, not two.
/// - `origin` keeps the **first ref** the id was seen in.
///   [`Board::read_refs`] lists this host's own log first, so a post this
///   host also holds is never reported as foreign — which matters, because
///   B4 judges a foreign post by time alone.
///
/// The final sort is `(committed_at, id)`, exactly as the single-ref read
/// always sorted: `Displayed::cap` drops the OLDEST, so a union that merely
/// concatenated per-ref runs would elide by ref rather than by age.
///
/// **Known, deliberately unfixed: a merged record can have MIXED PROVENANCE.**
/// Its `origin` comes from one ref and its `committed_at` may come from
/// another, so a peer whose clock runs behind, holding a byte-identical copy
/// of a post this host also has, drags that post's timestamp earlier. `reap`
/// is unaffected — it judges the un-merged single-ref [`TipSnapshot`], never
/// this value — but read-time liveness (`ttl_s`, `NOTICE_GRACE_PERIOD_S`)
/// does use the merged time, so such a post can render expired early. Narrow
/// (it needs byte-identical content on two hosts AND clock skew), and carried
/// into Task 6's brief rather than papered over here: the alternative rules
/// (latest-wins, or per-ref times) each break something else, and the choice
/// belongs with the code that judges foreign posts by time.
pub(crate) fn merge_by_id(collected: Vec<StoredPost>) -> Vec<StoredPost> {
    let mut by_id: std::collections::BTreeMap<String, StoredPost> =
        std::collections::BTreeMap::new();
    for s in collected {
        match by_id.entry(s.id.clone()) {
            std::collections::btree_map::Entry::Vacant(slot) => {
                slot.insert(s);
            }
            std::collections::btree_map::Entry::Occupied(mut slot) => {
                if s.committed_at < slot.get().committed_at {
                    slot.get_mut().committed_at = s.committed_at;
                }
            }
        }
    }
    let mut out: Vec<StoredPost> = by_id.into_values().collect();
    out.sort_by_key(|s| (s.committed_at, s.id.clone()));
    out
}

/// Classify a failure to read ONE ref of a union read.
///
/// This board's own log is fatal: the distinction the whole read path rests
/// on is that "could not read the board" must never be indistinguishable from
/// "the board is empty". A peer mirror is not — one unreadable mirror must
/// never blank the whole board (D7), and the local half is exactly the half
/// this session needs to see its own claims.
pub(crate) fn tolerate_unreadable_peer(
    origin: &Origin,
    refname: &str,
    e: BoardError,
) -> Result<(), BoardError> {
    match origin {
        Origin::Local => Err(e),
        Origin::Peer(_) => {
            eprintln!(
                "board: skipping peer ref {refname}: {e} -- one unreadable mirror must never \
                 blank the whole board (D7)"
            );
            Ok(())
        }
    }
}

/// A post as stored: its id, its content, when it was appended, and which log
/// it came from. The commit is the clock (D5) — posts carry durations, never
/// instants.
#[derive(Debug, Clone)]
pub struct StoredPost {
    /// Object id of the post's bytes; also its filename.
    pub id: String,
    /// The post itself.
    pub post: Post,
    /// Unix seconds of the commit that appended it.
    pub committed_at: u64,
    /// Which ref this reading of the post came from (B1).
    pub origin: Origin,
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

/// What [`Board::redact`]'s tip eviction actually did **on this host**.
///
/// Redaction's two halves have different scopes, and this type exists
/// because only one of them is per-host. The `redact` control post is a
/// post like any other: it propagates through sync, and every reader
/// computes suppression from it board-wide
/// ([`crate::live::redacted_ids`]). Eviction, by contrast, rewrites the tip
/// tree of the one ref this `Board` holds — so it can miss entirely, and an
/// operator is owed the difference between "the bytes are off this host's
/// tip" and "they are still on it".
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RedactOutcome {
    /// The target was in this host's tip tree and the swap won: the bytes
    /// are off the tip here (history still holds them — D13).
    Evicted,
    /// The target was in this host's tip tree, but the single-shot
    /// compare-and-swap lost a race, so the tip still carries it. Running
    /// `redact` again retries against whatever landed, and is safe: the
    /// identical control post is not re-appended (D11).
    LostRace,
    /// The target is not in this host's tip tree at all — already evicted,
    /// or it lives only in a peer's log, or only as an identical-bytes copy
    /// another host authored. Nothing was evicted here, and nothing could
    /// be; the authoring host is where its copy comes off the tip.
    NotHere,
}

impl RedactOutcome {
    /// One operator-facing line: what happened to `id` here, and what — if
    /// anything — to do next.
    ///
    /// A method rather than a `match` at the call site so all three
    /// sentences are pinned by one test, and so a fourth outcome could not
    /// be added with no words attached.
    ///
    /// Every variant says suppression still applies board-wide, because the
    /// dangerous misreading of "nothing was evicted" is "nothing happened,
    /// the secret is still on display" — which invites re-posting it. Each
    /// sentence also carries the qualifier the CLI previously dropped:
    /// "board-wide" means every read that has *fetched* the control post,
    /// not every read, full stop — a peer that has not synced yet keeps
    /// rendering the body regardless of what happened here, which is exactly
    /// the quiet-sync-failure this campaign is about.
    pub fn diagnostic(&self, id: &str) -> String {
        match self {
            Self::Evicted => format!(
                "board: redacted {id} -- evicted from this host's log; the body is suppressed \
                 board-wide on every read that has fetched this control post, and history \
                 still holds it (D13)"
            ),
            Self::LostRace => format!(
                "board: redacted {id} -- the control post landed, but the eviction lost a \
                 compare-and-swap race and this host's tip STILL carries the post; run the \
                 same command again to retry it. The body is suppressed board-wide on every \
                 read that has fetched this control post, regardless"
            ),
            Self::NotHere => format!(
                "board: redacted {id} -- NOT in this host's log, so nothing was evicted here \
                 (it may live only in a peer's log, or only as an identical-bytes copy \
                 another host authored). The body is suppressed board-wide on every read that \
                 has fetched this control post; run this on the authoring host to take its \
                 copy off that host's tip too"
            ),
        }
    }
}

/// One board, on one ref, in one repository.
#[derive(Debug, Clone)]
pub struct Board {
    repo: Repo,
    refname: String,
}

impl Board {
    /// Where a peer host's log is mirrored locally: `refs/hornvale/peers/`,
    /// plus the peer's host name. One writer per ref — this host never writes
    /// under this prefix; a fetch does (B1).
    pub const PEERS_PREFIX: &'static str = "refs/hornvale/peers/";

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

    /// The [`Origin`] of a post read from `refname`, from this board's point
    /// of view: its own ref is `Local`, anything under
    /// [`PEERS_PREFIX`](Self::PEERS_PREFIX) is that host's `Peer`.
    ///
    /// A ref that is neither cannot come out of [`read_refs`](Self::read_refs),
    /// so the remaining arm exists only for a board pointed at some other ref
    /// directly (the tests' `refs/test/*` boards): that ref is the only log
    /// such a board has, which is what `Local` means here — "this board's own
    /// log", not "the canonical board".
    fn origin_of(&self, refname: &str) -> Origin {
        if refname == self.refname {
            return Origin::Local;
        }
        match refname.strip_prefix(Self::PEERS_PREFIX) {
            Some(host) => Origin::Peer(host.to_string()),
            None => Origin::Local,
        }
    }

    /// The origin of a post read from this board's own ref.
    ///
    /// Private: `snapshot` is its only caller. It was `pub` while
    /// `history_in`'s ancestor needed a way to tag its own reads, but that
    /// justification died once `history_in` (and every other reader) took
    /// `origin` as a parameter instead of deriving it locally.
    fn own_origin(&self) -> Origin {
        self.origin_of(&self.refname)
    }

    /// The refs a read draws from: this host's own log plus every peer
    /// mirror, **excluding this host's own mirror**.
    ///
    /// Own-mirror exclusion is not tidiness. `peers/<self>` is whatever this
    /// host last *pushed*, so it sits behind the local log whenever a reap
    /// has not been pushed yet — unioning it would resurrect this host's own
    /// reaped posts, permanently, on every read. Pushing before fetching
    /// would also avoid that, but then a compaction invariant would depend on
    /// the order of two calls in a shell script; skipping the ref makes
    /// push-before-fetch an optimisation instead.
    ///
    /// A failure to LIST the peer refs is propagated, not swallowed: this
    /// crate's one hard rule about reads is that a git failure must never be
    /// indistinguishable from an empty board. Failures to *resolve* an
    /// individual peer ref are a different matter and are skipped with a
    /// warning by the caller.
    pub fn read_refs(&self) -> Result<Vec<String>, BoardError> {
        let mut refs = vec![self.refname.clone()];
        // `%(refname)` is load-bearing, not just the shortest format that
        // works: it is answered from the ref store alone and never looks the
        // object up. A format that dereferences (`%(objecttype)`,
        // `%(committerdate)`, …) makes `for-each-ref` exit 128 on ONE
        // dangling mirror (measured: `fatal: missing object <oid> for
        // refs/hornvale/peers/dangling`, against exit 0 here) — and because
        // a listing failure is propagated (see
        // above), that would blank the ambient render for this host until
        // someone deleted the bad ref. The constraint is invisible in the
        // code, so it is written down here rather than rediscovered.
        let listed = self
            .repo
            .git(&["for-each-ref", "--format=%(refname)", Self::PEERS_PREFIX])?;
        // An empty host (`hostname` unavailable) matches no real ref, so
        // nothing is skipped and every mirror is read. That is the fail-open
        // direction: a resurrected post is visible and self-correcting on the
        // next push, while a swallowed peer hold-off is silent.
        let own = format!("{}{}", Self::PEERS_PREFIX, crate::live::current_host());
        for line in listed.lines().filter(|l| !l.is_empty()) {
            if line != own && line != self.refname {
                refs.push(line.to_string());
            }
        }
        Ok(refs)
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
    ///
    /// **Single-ref, deliberately, and it must stay that way** (B1). This is
    /// the value [`ReapPlan`] and [`reap`](Self::reap) are built from, and
    /// compaction is a judgment: which posts are permanently dead is decided
    /// by the host that owns the log, against a process table and a set of
    /// branches only that host can see. A snapshot that unioned peer refs
    /// would let this host's predicate delete another host's posts — and,
    /// because one writer per ref is what keeps the compare-and-swap correct
    /// across machines, it could not even do so safely.
    pub fn snapshot(&self) -> Result<Option<TipSnapshot>, BoardError> {
        let Some(tip) = self.tip()? else {
            return Ok(None);
        };
        let posts = self.posts_in(&tip, &self.own_origin())?;
        Ok(Some(TipSnapshot { tip, posts }))
    }

    /// Post ids present at every ref a read draws from
    /// ([`read_refs`](Self::read_refs)), deduplicated and sorted.
    ///
    /// Unions for the same reason [`posts_at_tip`](Self::posts_at_tip) does,
    /// and it is not merely for symmetry: [`crate::relevance::Cursor::record`]
    /// prunes its "already shown" set against this. Left single-ref, every
    /// foreign post would be pruned out of `seen` on the very read that
    /// displayed it, and would then re-render at every session start forever.
    pub fn post_ids_at_tip(&self) -> Result<Vec<String>, BoardError> {
        // A set, not a `Vec`: the same post at two refs is one id (D11), and
        // this is the value the cursor's prune is a membership test against.
        let mut ids: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
        for (refname, origin, tip) in self.resolved_read_refs()? {
            match self.post_ids_in(&tip) {
                Ok(found) => ids.extend(found),
                Err(e) => tolerate_unreadable_peer(&origin, &refname, e)?,
            }
        }
        Ok(ids.into_iter().collect())
    }

    /// Post ids present in `tip`'s tree, sorted. Takes the tip rather than
    /// re-reading it, so a caller holding a [`TipSnapshot`] can ask about
    /// exactly the commit it read.
    ///
    /// A `posts/` filename that is not a full 40-hex object id is
    /// **skipped, with a warning** — never an error (D7: one corrupt post
    /// must never blank the whole board). [`cat_file_batch`](Self::cat_file_batch)
    /// filters the same way, independently; this filter stays too because
    /// it can name the full tree path in its warning, which `cat_file_batch`
    /// cannot. Deduplicated per THREAD (see `WARNED_MALFORMED_POST_FILENAMES`)
    /// because this runs three times in one ordinary render/read
    /// (`posts_at_tip`, `relevance::unseen`, `Cursor::record`).
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

    /// Every post at every ref a read draws from
    /// ([`read_refs`](Self::read_refs)) — this host's log unioned with each
    /// peer mirror — deduplicated by id, oldest first, each tagged with the
    /// [`Origin`] of the ref it came from (B1).
    ///
    /// The union is a read-time operation over refs each of which has exactly
    /// one writer; nothing here merges histories. That is what keeps the
    /// compare-and-swap in [`append`](Self::append) correct across machines,
    /// and what keeps a reap terminal for the log that made it: under a merge
    /// design, host A reaps a post and the next fetch from B resurrects it,
    /// forever.
    ///
    /// A post that fails to parse is skipped with a warning, and so is a peer
    /// ref that cannot be read at all: one corrupt post — or one unreadable
    /// mirror — must never break a session's render (D7). A failure on this
    /// host's OWN log is still an error, never an empty board.
    pub fn posts_at_tip(&self) -> Result<Vec<StoredPost>, BoardError> {
        let mut collected: Vec<StoredPost> = Vec::new();
        for (refname, origin, tip) in self.resolved_read_refs()? {
            match self.posts_in(&tip, &origin) {
                Ok(posts) => collected.extend(posts),
                Err(e) => tolerate_unreadable_peer(&origin, &refname, e)?,
            }
        }
        Ok(merge_by_id(collected))
    }

    /// Every ref a read draws from, resolved to a commit, with the origin to
    /// tag its posts with.
    ///
    /// One resolution policy for both public reads, rather than each
    /// resolving for itself: [`posts_at_tip`](Self::posts_at_tip) and
    /// [`post_ids_at_tip`](Self::post_ids_at_tip) must draw from the same
    /// refs, because [`crate::relevance::Cursor::record`] prunes one against
    /// the other.
    ///
    /// **Why `^{commit}` rather than the bare ref.** Not because `git log`
    /// rejects a tree — measured on git 2.50.1, `git log <tree-oid>` exits 0
    /// with *empty output*, and `ls-tree` accepts it too. That is exactly
    /// what makes the bare form bad: a mirror pointing at a tree would be
    /// read successfully, contribute every post in it, and attribute all of
    /// them to **epoch 0** (`posts_in`'s `when` map comes from that empty
    /// `git log`), which biases every one of them toward `Expired` — the
    /// opposite of this crate's fail-open convention, arriving as a pile of
    /// stderr warnings with no statement of the actual cause. The peel turns
    /// that into one clear D7 skip naming the ref.
    ///
    /// A ref that does not resolve to a commit is skipped: absent is how a
    /// board that does not exist yet reads, and a listed-but-unresolvable
    /// peer mirror says so on stderr rather than failing the read.
    pub(crate) fn resolved_read_refs(&self) -> Result<Vec<(String, Origin, String)>, BoardError> {
        let mut out = Vec::new();
        for refname in self.read_refs()? {
            let origin = self.origin_of(&refname);
            let peeled = format!("{refname}^{{commit}}");
            match self.repo.rev_parse_verify(&peeled) {
                Ok(Some(tip)) => out.push((refname, origin, tip)),
                Ok(None) => {
                    if let Origin::Peer(_) = origin {
                        eprintln!(
                            "board: skipping peer ref {refname}: it does not resolve to a commit \
                             -- one unreadable mirror must never blank the whole board (D7)"
                        );
                    }
                }
                Err(e) => tolerate_unreadable_peer(&origin, &refname, e)?,
            }
        }
        Ok(out)
    }

    /// One ref's posts, against a tip the caller already resolved, tagged
    /// with that ref's `origin`. Private, and the single implementation
    /// [`posts_at_tip`](Self::posts_at_tip) and [`snapshot`](Self::snapshot)
    /// both go through — so there is no second copy of this walk that could
    /// drift from it.
    fn posts_in(&self, tip: &str, origin: &Origin) -> Result<Vec<StoredPost>, BoardError> {
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
                        origin: origin.clone(),
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
    /// Filtered (warn-and-skip) right here, before any `git` invocation, not
    /// delegated to a caller: this is the one point all three consumers
    /// converge on (`posts_in`, once per ref of the cross-ref union, and
    /// `digest.rs`'s `history`), and an id reaching this function is
    /// repository data —
    /// untrusted, not a caller bug — so a bad one must never blank or crash
    /// the whole read (D7). [`post_ids_in`](Self::post_ids_in) also filters,
    /// independently (it can name the tree path in its own warning, which
    /// this function cannot), but this function does not rely on that
    /// having run first.
    ///
    /// A missing or unreadable object is **omitted with a warning** rather than
    /// failing the read (D7). The caller treats absence as "already warned
    /// about" — including an id this function itself rejected.
    pub(crate) fn cat_file_batch(
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
    /// header-framing parse. `cat_file_batch` only ever passes ids that have
    /// already passed [`is_full_object_id`]; a test in this module calls
    /// this function directly with one that has not, to exercise the parse
    /// on its own.
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
                // Body-less statuses: the header ends right here, so skip
                // just this id and keep parsing the chunk. None of these is
                // ever a valid object type, so this can't misfire on a real
                // content line. (`symlink`/`dangling`/`loop`/`notdir` are
                // first-field `--follow-symlinks` shapes, not reachable via
                // this second-token match; not used here.)
                Some("missing" | "ambiguous" | "submodule" | "excluded") => {
                    eprintln!("board: skipping {name:?}: cat-file reported {header:?}");
                    continue;
                }
                // A real object type: size and body still follow, below.
                Some(_) => {}
                // No second field at all: the stream itself can't be
                // trusted, so stop the batch rather than guess at a body.
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
        if self.evict_and_swap(&old, &keep, &format!("board: reap {dropped}"))? {
            Ok(dropped)
        } else {
            Ok(0)
        }
    }

    /// Evict one post named `id` from the tip tree, appending a `redact`
    /// control post first so the act itself is durably recorded — B8.
    ///
    /// **Byte removal from history is prohibited, and it does not work.**
    /// This was measured during the spec, not assumed: a commit containing a
    /// canary string was force-pushed out of a probe ref's history, and
    /// GitHub still served both the commit and the blob's full plaintext, by
    /// oid, afterwards. So a history rewrite buys nothing while breaking D13
    /// (the ref's root must never move) — and multi-machine makes it worse:
    /// a rewrite breaks fast-forward, so "real" redaction would mean
    /// coordinated force-pushes plus a `gc` on every clone, and an offline
    /// clone keeps the bytes regardless. `redact` is therefore a *read-time*
    /// judgment instead (D10): the target post stays reachable by its own
    /// object id forever, exactly like every other appended post (D11), and
    /// what changes is only what the TIP tree carries forward and what
    /// [`crate::digest::digest`] is willing to print. If a future reader is
    /// tempted to propose a rewrite here — this paragraph is why not.
    ///
    /// The `redact` post is appended FIRST, through the ordinary retrying
    /// [`append`](Self::append) path, and only once it has landed does this
    /// read a fresh snapshot to evict the target from — so the control post
    /// itself never depends on the eviction's own single-shot CAS
    /// succeeding. Eviction reuses [`reap`](Self::reap)'s own
    /// tree-build-and-CAS path ([`evict_and_swap`](Self::evict_and_swap))
    /// rather than a second copy of it.
    ///
    /// Returns the id of the `redact` control post **and** which of
    /// [`RedactOutcome`]'s three cases this call landed in.
    ///
    /// The outcome is not decoration. A bare `Ok(id)` reported success
    /// identically whether the eviction happened, lost its race, or never had
    /// anything to evict on this host at all — so `board redact <an-id-this-
    /// host-does-not-hold>` exited 0 saying nothing, which is the silent
    /// success the brief ruled out. None of the three is an `Err`: a lost
    /// race is control flow (see [`evict_and_swap`](Self::evict_and_swap)),
    /// and a target absent from this host's log is not a failure of this call
    /// — board-wide suppression applies regardless, because it is driven by
    /// the control post landing in the union read, not by whose tip the
    /// target happens to occupy. What the caller owes the operator is a
    /// *diagnostic*, and [`RedactOutcome::diagnostic`] is it.
    pub fn redact(&self, by: &str, id: &str) -> Result<(String, RedactOutcome), BoardError> {
        let redact_id = self.append(
            &Post::new("redact", by).with("post", serde_json::Value::String(id.to_string())),
        )?;

        // Re-snapshot AFTER the append: it just moved the tip (or, on an
        // idempotent replay of an identical redact, left it exactly where it
        // already was), so eviction must be baselined on THAT tip, never an
        // earlier one this call might have read before appending.
        let Some(snapshot) = self.snapshot()? else {
            // Unreachable in practice: the append above just succeeded, so
            // the ref exists. Guarded anyway rather than unwrapped, matching
            // this module's D7 stance that an absent read is reported, not
            // panicked on.
            return Ok((redact_id, RedactOutcome::NotHere));
        };
        let outcome = self.evict_target(&snapshot, id)?;
        Ok((redact_id, outcome))
    }

    /// The eviction half of [`redact`](Self::redact): drop `id` from
    /// `snapshot`'s tip tree, single-shot.
    ///
    /// Extracted so the `LostRace` arm can be held directly rather than only
    /// through `redact`, which always re-snapshots immediately after its own
    /// append (see `redact`'s doc comment) and so never hands itself a
    /// snapshot that is already stale by the time this runs. A test can:
    /// hold a snapshot, append past it (moving the tip out from under that
    /// snapshot), then call this with the now-stale snapshot directly.
    fn evict_target(&self, snapshot: &TipSnapshot, id: &str) -> Result<RedactOutcome, BoardError> {
        let posts = snapshot.posts();
        let keep: Vec<&StoredPost> = posts.iter().filter(|s| s.id != id).collect();
        if keep.len() == posts.len() {
            // Nothing to evict at this tip -- already redacted, or `id` never
            // named a post in THIS log (it may live only in a peer's, or only
            // as an identical-bytes copy authored there, D11). The control
            // post above still recorded the act either way, and suppression
            // is board-wide because of it.
            return Ok(RedactOutcome::NotHere);
        }
        // Single-shot, exactly as `reap`'s is: reporting the loss is the fix
        // here, not retrying against a moving target.
        if self.evict_and_swap(snapshot.tip(), &keep, &format!("board: redact {id}"))? {
            Ok(RedactOutcome::Evicted)
        } else {
            Ok(RedactOutcome::LostRace)
        }
    }

    /// Build a tree containing only `keep`, commit it as a forward child of
    /// `old`, and try to compare-and-swap it into the ref.
    ///
    /// The single implementation of "shrink the tip tree without touching
    /// history" — [`reap`](Self::reap) and [`redact`](Self::redact) are the
    /// only two operations that ever do this, and both go through this one
    /// path rather than each carrying its own copy of the index-build,
    /// commit, CAS dance (the same reasoning that put `cat_file_batch` in
    /// one place for every reader instead of one per caller).
    ///
    /// `Ok(true)` means the swap won and `keep` is now the tip tree.
    /// `Ok(false)` means the CAS lost a genuine race (something else moved
    /// the ref between `old` being read and this call) — expected control
    /// flow, not an error: nothing was written, so the caller's snapshot is
    /// simply stale.
    fn evict_and_swap(
        &self,
        old: &str,
        keep: &[&StoredPost],
        message: &str,
    ) -> Result<bool, BoardError> {
        // A fresh throwaway index, never the repo's real one (`Repo::git_path`
        // is a private, per-worktree path) -- this must not dirty the
        // working tree. Named with a per-CALL discriminant, not just the
        // pid: two evictions (or an eviction racing an append) in the same
        // process would otherwise collide on git's index lock. This is the
        // exact atomic `append_with_attempts` uses for the same reason,
        // reused here rather than reinvented -- see its doc comment above.
        let call_id = CALL_DISCRIMINANT.fetch_add(1, Ordering::SeqCst);
        let raw = self
            .repo
            .git_path(&format!("hv-board-evict-{}-{call_id}", std::process::id()))?;
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
            // tree first, so a dropped post is never present to begin with,
            // not merely removed after the fact.
            for s in keep {
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

        let new = self
            .repo
            .git(&["commit-tree", &tree, "-p", old, "-m", message])?;

        Ok(self.cas(&new, Some(old))?.is_none())
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
    fn redact_evicts_from_the_tip_but_history_still_holds_the_post() {
        // B8, D10: redaction is tip eviction plus digest suppression, never
        // a history rewrite -- see `Board::redact`'s doc comment for why a
        // rewrite was measured to buy nothing. The three properties this
        // asserts: gone from the tip, the ref's root untouched (D13), and
        // still reachable by oid (D11) because the id IS the object id.
        let (_dir, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let id = board
            .append(&Post::new("technique", "main").with("note", serde_json::json!("oops")))
            .expect("id");
        let root_before = board.root().expect("root");

        board.redact("main", &id).expect("redact");

        assert!(
            !board.post_ids_at_tip().expect("ids").contains(&id),
            "still at the tip"
        );
        assert_eq!(
            board.root().expect("root"),
            root_before,
            "the ref was rerooted (0118 part 3)"
        );
        // REACHABILITY, not mere existence. `cat-file -p <id>` succeeds for a
        // loose object that nothing points at any more -- one `git gc` from
        // being gone -- so it cannot distinguish "history holds the post"
        // from "the object database has not been swept yet", which is the
        // entire property D13 is about. `rev-list --objects <ref>` asks the
        // question the comment above claims to be asking: is this blob
        // reachable from the board's ref?
        let reachable = repo
            .git(&["rev-list", "--objects", board.refname()])
            .expect("rev-list");
        let listed = |oid: &str| {
            reachable
                .lines()
                .any(|l| l.split_whitespace().next() == Some(oid))
        };
        assert!(
            listed(&id),
            "history lost the post: {id} is not reachable from {}:\n{reachable}",
            board.refname()
        );

        // And prove it the hard way once, since the whole design rests on it:
        // an aggressive prune that drops every unreachable object leaves this
        // one exactly where it was.
        repo.git(&["gc", "--aggressive", "--prune=now"])
            .expect("gc");
        assert!(
            repo.git(&["cat-file", "-p", &id]).is_ok(),
            "an aggressive gc took the post: reachability was not real"
        );
    }

    #[test]
    fn redact_reports_which_of_the_three_outcomes_it_landed_in() {
        // The silent-success fix. `redact` used to return a bare `Ok(id)`
        // whether it evicted, lost the CAS, or never had anything here to
        // evict -- so `board redact <id-not-in-this-log>` exited 0 with no
        // diagnostic, which is exactly the "must not silently claim success"
        // the brief ruled out. `reap` already returns `Ok(0)` so its caller
        // learns it lost; this is the same courtesy.
        let (_dir, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let id = board
            .append(&Post::new("technique", "main").with("note", serde_json::json!("oops")))
            .expect("id");

        let (_, outcome) = board.redact("main", &id).expect("redact");
        assert_eq!(
            outcome,
            RedactOutcome::Evicted,
            "the target was in this host's tip and the swap won"
        );

        // A second call has nothing left to evict HERE. Not an error, and
        // not "evicted" either -- the operator asked for something that did
        // not happen on this host.
        let (_, again) = board.redact("main", &id).expect("second redact");
        assert_eq!(again, RedactOutcome::NotHere, "already gone from this tip");

        // A post only a peer holds is the case that actually bites: the
        // union read shows it, so an operator reasonably runs `redact` here.
        let peer = peer_board(&repo, &foreign_host());
        let theirs = peer
            .append(&Post::new("technique", "main").with("note", serde_json::json!("theirs")))
            .expect("peer post");
        let (_, foreign) = board.redact("main", &theirs).expect("redact a peer's post");
        assert_eq!(
            foreign,
            RedactOutcome::NotHere,
            "eviction is per-log and cannot reach a peer's tip"
        );
    }

    #[test]
    fn each_redact_outcome_says_something_different_and_says_suppression_still_applies() {
        // The diagnostic is the whole point of the outcome, so it is pinned
        // here rather than left to a caller: three outcomes that print the
        // same sentence would be exactly as silent as returning `Ok(id)`.
        let id = "abc123";
        let evicted = RedactOutcome::Evicted.diagnostic(id);
        let lost = RedactOutcome::LostRace.diagnostic(id);
        let absent = RedactOutcome::NotHere.diagnostic(id);
        assert_ne!(evicted, lost);
        assert_ne!(lost, absent);
        assert_ne!(evicted, absent);
        for d in [&evicted, &lost, &absent] {
            assert!(d.contains(id), "names the target: {d}");
            assert!(
                d.contains("board-wide"),
                "every outcome must say suppression still applies board-wide -- an \
                 operator who reads `nothing evicted` as `nothing happened` would post \
                 the secret again: {d}"
            );
            // Task 10's carry: the qualifier is what makes "board-wide" true
            // rather than misleading -- it means every read that has
            // FETCHED this control post, not every read, full stop. All
            // three sentences say "board-wide" on their own; only this
            // checks the qualifier survives on EVERY variant, not just the
            // two the old assertions happened to cover (`id` and, for
            // `LostRace` alone, "again").
            assert!(
                d.contains("on every read that has fetched this control post"),
                "the board-wide qualifier must survive on every variant, not just \
                 the ones an existing assertion happens to cover: {d}"
            );
        }
        assert!(
            lost.contains("again"),
            "a lost race is the one outcome with an action attached: {lost}"
        );
    }

    #[test]
    fn a_stale_baseline_loses_the_swap_rather_than_overwriting_the_tip() {
        // The mechanism behind `RedactOutcome::LostRace`, pinned directly
        // because `redact` reads its own snapshot and so offers no seam to
        // inject a concurrent writer into deterministically. `Ok(false)`
        // here, plus `redact`'s one-line mapping of it, is the whole path.
        let (_dir, repo) = temp_repo();
        let board = Board::new(repo.clone());
        board
            .append(&Post::new("technique", "main").with("note", serde_json::json!("first")))
            .expect("first");
        let stale = board.tip().expect("tip").expect("some");
        // The ref moves out from under `stale`.
        board
            .append(&Post::new("technique", "main").with("note", serde_json::json!("second")))
            .expect("second");
        let current = board.tip().expect("tip").expect("some");

        let won = board
            .evict_and_swap(&stale, &[], "board: a swap that must lose")
            .expect("a lost CAS is control flow, never an error");
        assert!(!won, "a stale baseline must not win the swap");
        assert_eq!(
            board.tip().expect("tip").expect("some"),
            current,
            "and must leave the tip exactly where it found it"
        );
    }

    #[test]
    fn evict_target_loses_the_swap_on_a_stale_snapshot_and_leaves_the_tip_untouched() {
        // The `LostRace` arm of `evict_target`, held directly rather than
        // only through `redact` -- `redact` always re-snapshots immediately
        // after its own append (see its doc comment), so nothing in
        // `redact`'s own tests can hand it a snapshot that is already stale.
        // `evict_target` is the seam Task 9's review extracted precisely so
        // this could be held: hold a snapshot, append PAST it (moving the
        // tip out from under that snapshot), then call `evict_target` with
        // the now-stale snapshot directly. Mirrors
        // `a_stale_baseline_loses_the_swap_rather_than_overwriting_the_tip`
        // one level down -- same race, at the level above `evict_and_swap`.
        let (_dir, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let id = board
            .append(&Post::new("technique", "main").with("note", serde_json::json!("first")))
            .expect("first");
        let stale = board.snapshot().expect("snapshot").expect("some");
        // The ref moves out from under `stale`.
        board
            .append(&Post::new("technique", "main").with("note", serde_json::json!("second")))
            .expect("second");
        let current = board.tip().expect("tip").expect("some");

        let outcome = board
            .evict_target(&stale, &id)
            .expect("a lost CAS is control flow, never an error");
        assert_eq!(
            outcome,
            RedactOutcome::LostRace,
            "the snapshot's baseline is no longer the ref's value, so the CAS must lose \
             deterministically"
        );
        assert_eq!(
            board.tip().expect("tip").expect("some"),
            current,
            "a lost race must leave the tip exactly where it found it"
        );
    }

    #[test]
    fn redact_is_idempotent_and_retries_the_eviction_on_a_second_call() {
        // Calling `redact` twice for the same target must not double-append
        // the control post (D11: identical content is the same post) and
        // must still leave the target evicted -- a caller retrying after an
        // uncertain first attempt must not be punished for it.
        let (_dir, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let id = board
            .append(&Post::new("technique", "main").with("note", serde_json::json!("oops")))
            .expect("id");

        let (first, first_outcome) = board.redact("main", &id).expect("first redact");
        let (second, second_outcome) = board.redact("main", &id).expect("second redact");
        assert_eq!(first, second, "the same redact content is the same post");
        assert!(!board.post_ids_at_tip().expect("ids").contains(&id));
        // The IDs match, but the outcomes must not: the second call had
        // nothing left to evict, and reporting that as another eviction is
        // the silent success this outcome type exists to end.
        assert_eq!(first_outcome, RedactOutcome::Evicted);
        assert_eq!(second_outcome, RedactOutcome::NotHere);
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
        let host = crate::live::current_host();
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
        // The companion to `one_unparseable_post_does_not_hide_the_rest_of_
        // the_board` (above), which covers corrupt CONTENT under a valid
        // filename. This covers a malformed FILENAME (not any real
        // object's id) holding otherwise well-formed content.
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

    // --- B1: the union read over per-host refs ---

    /// A host name that is guaranteed not to be this host's own.
    ///
    /// Not a hardcoded `"lefford"`: this suite runs on lefford too, where
    /// that literal names THIS host's mirror, is correctly skipped by the
    /// union, and fails these tests for a reason that has nothing to do with
    /// what they check. Derived from the real host so the two can never
    /// collide however either machine is renamed.
    fn foreign_host() -> String {
        format!("{}-peer", crate::live::current_host())
    }

    /// A peer's log, mirrored under `refs/hornvale/peers/<host>` exactly as
    /// the fetch in Task 7 will build it. Deliberately built by appending
    /// through a `Board` on that ref rather than by copying the local one:
    /// that is what makes the resulting posts genuinely foreign content
    /// rather than the same commits under a second name.
    fn peer_board(repo: &Repo, host: &str) -> Board {
        Board::with_ref(repo.clone(), &format!("{}{host}", Board::PEERS_PREFIX))
    }

    #[test]
    fn a_read_unions_the_local_log_with_every_peer_ref() {
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let board = Board::new(repo.clone());
        let mine = board
            .append(&Post::new("technique", "main").with("note", serde_json::json!("local")))
            .expect("mine");

        let peer = peer_board(&repo, &foreign_host());
        let theirs = peer
            .append(&Post::new("technique", "main").with("note", serde_json::json!("remote")))
            .expect("theirs");

        let posts = board.posts_at_tip().expect("read");
        let ids: Vec<String> = posts.iter().map(|s| s.id.clone()).collect();
        assert!(ids.contains(&mine), "local post missing from the union");
        assert!(ids.contains(&theirs), "peer post missing from the union");

        // Provenance comes from the REF, which is the whole point of reading
        // it here rather than from a `host` field the posts do not carry.
        let origin_of = |id: &String| {
            posts
                .iter()
                .find(|s| &s.id == id)
                .map(|s| s.origin.clone())
                .expect("post present")
        };
        assert_eq!(origin_of(&mine), Origin::Local);
        assert_eq!(origin_of(&theirs), Origin::Peer(foreign_host()));
    }

    #[test]
    fn the_union_deduplicates_a_post_present_in_two_refs() {
        // The CRDT property, at read time: an id IS a content hash, so the
        // same post in two refs is the same string twice.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let board = Board::new(repo.clone());
        let post = Post::new("technique", "main").with("note", serde_json::json!("same"));
        let a = board.append(&post).expect("a");
        let peer = peer_board(&repo, &foreign_host());
        let b = peer.append(&post).expect("b");
        assert_eq!(a, b, "content addressing should make these one id");

        // Local-first is structural (`read_refs` pushes this board's own ref
        // before the loop, `resolved_read_refs` preserves that order, and
        // `merge_by_id` only ever sets `origin` on the FIRST occurrence) --
        // but nothing else asserts it, and listing the local log last would
        // leave the rest of the suite green while relabelling a shared post
        // `Peer(..)`. That mislabel is not cosmetic: B4 judges a foreign post
        // by time alone, so it would silently change how this post is judged.
        assert_eq!(
            board
                .posts_at_tip()
                .expect("read")
                .iter()
                .find(|s| s.id == a)
                .map(|s| s.origin.clone()),
            Some(Origin::Local),
            "a post this host ALSO holds must not be reported foreign"
        );

        let ids: Vec<String> = board
            .posts_at_tip()
            .expect("read")
            .into_iter()
            .map(|s| s.id)
            .collect();
        assert_eq!(
            ids.iter().filter(|i| **i == a).count(),
            1,
            "duplicated in the union"
        );
        assert_eq!(
            board
                .post_ids_at_tip()
                .expect("ids")
                .iter()
                .filter(|i| **i == a)
                .count(),
            1,
            "and the id read must dedupe identically -- `Cursor::record` prunes against it"
        );
    }

    #[test]
    fn the_union_skips_this_hosts_own_mirror_so_a_reaped_post_cannot_return() {
        // peers/<self> is behind the local log whenever a reap has not been
        // pushed, so including it would resurrect this host's own reaped
        // posts. Skipping the ref makes push-before-fetch an optimisation
        // rather than a correctness requirement.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let board = Board::new(repo.clone());
        let host = crate::live::current_host();
        assert!(
            !host.is_empty(),
            "this test is vacuous without a hostname: every peer ref would be read"
        );
        let own_mirror = peer_board(&repo, &host);
        let ghost = own_mirror
            .append(&Post::new("technique", "main").with("note", serde_json::json!("reaped here")))
            .expect("ghost");

        let ids: Vec<String> = board
            .posts_at_tip()
            .expect("read")
            .into_iter()
            .map(|s| s.id)
            .collect();
        assert!(!ids.contains(&ghost), "own mirror must be skipped");
        assert!(
            !board.post_ids_at_tip().expect("ids").contains(&ghost),
            "and the id read must skip it too, or `unseen` would re-offer it forever"
        );
        assert!(
            !board
                .read_refs()
                .expect("refs")
                .contains(&own_mirror.refname),
            "the skip must be at the ref list, not at each individual read"
        );
    }

    #[test]
    fn reap_and_snapshot_never_see_a_peer_ref() {
        // Compaction is a judgment, and judgments do not union: a reap that
        // could see a peer ref would let this host's predicate govern another
        // host's log.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        merge_branch_into_main(&repo, "campaign/merged");
        let board = Board::new(repo.clone());
        board
            .append(&Post::new("technique", "main").with("note", serde_json::json!("mine")))
            .expect("mine");
        let peer = peer_board(&repo, &foreign_host());
        // Reapable BY THIS HOST'S predicate -- a notice on a branch that
        // resolves here and is merged here -- so if the reap could see it, it
        // would drop it.
        let theirs = peer
            .append(&Post::new("notice", "campaign/merged").with("note", serde_json::json!("t")))
            .expect("t");
        let peer_tip_before = peer.tip().expect("peer tip").expect("some");

        let snap = board.snapshot().expect("snapshot").expect("some");
        assert!(
            !snap.posts().iter().any(|p| p.id == theirs),
            "a snapshot must be single-ref: reap must never judge a peer's log"
        );

        let mut plan = ReapPlan::probe(&repo, &snap).expect("probe");
        plan.advance_clock(crate::live::NOTICE_GRACE_PERIOD_S + 1);
        board.reap(&plan).expect("reap");
        assert_eq!(
            peer.tip().expect("peer tip").expect("some"),
            peer_tip_before,
            "a reap must not move a peer ref at all"
        );
        assert!(
            peer.post_ids_at_tip().expect("peer ids").contains(&theirs),
            "and the peer's post must survive this host's compaction judgment"
        );
    }

    #[test]
    fn a_peer_ref_that_points_at_a_tree_is_skipped_with_a_warning_not_fatal() {
        // D7 at the ref level: one unreadable mirror must never blank the
        // board. Two things this test has to get right, both learned the hard
        // way:
        //
        //  1. The tree must hold a post the LOCAL log does not, or the
        //     dedupe hides whatever the broken ref contributes and the test
        //     passes with or without the `^{commit}` peel. Built on a
        //     throwaway ref, so its post is genuinely absent locally.
        //  2. A tree-pointing ref is not rejected by the reads -- `ls-tree`
        //     accepts a tree and `git log <tree>` exits 0 with EMPTY output
        //     (measured, git 2.50.1). Unpeeled, this mirror would be read
        //     "successfully" and every post in it attributed to epoch 0,
        //     biasing it toward Expired. The peel is what turns that into
        //     one clear skip.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let board = Board::new(repo.clone());
        let mine = board
            .append(&Post::new("technique", "main").with("note", serde_json::json!("keep me")))
            .expect("mine");

        let elsewhere = Board::with_ref(repo.clone(), "refs/test/only-in-the-broken-mirror");
        let hidden = elsewhere
            .append(&Post::new("notice", "main").with("note", serde_json::json!("unreachable")))
            .expect("a post the local log does not hold");
        let foreign_tip = elsewhere.tip().expect("tip").expect("some");
        let tree = repo
            .git(&["rev-parse", "--verify", &format!("{foreign_tip}^{{tree}}")])
            .expect("tree");
        repo.git(&[
            "update-ref",
            &format!("{}broken", Board::PEERS_PREFIX),
            &tree,
        ])
        .expect("a peer ref pointing at a tree");
        assert_ne!(hidden, mine, "the two logs must hold different posts");

        let ids: Vec<String> = board
            .posts_at_tip()
            .expect("one broken mirror must never fail the whole read")
            .into_iter()
            .map(|s| s.id)
            .collect();
        assert_eq!(
            ids,
            vec![mine.clone()],
            "the local log must still be read: {ids:?}"
        );
        assert_eq!(
            board.post_ids_at_tip().expect("ids"),
            vec![mine],
            "and the id read must degrade the same way"
        );
    }

    #[test]
    fn a_peer_ref_naming_an_absent_object_is_skipped_with_a_warning_not_fatal() {
        // The OTHER arm of `resolved_read_refs`: `Ok(None)`, a ref that is
        // listed but resolves to nothing.
        //
        // Reachable with no race and no injection seam, which is why this is
        // a committed test rather than a deferred one: `for-each-ref
        // --format=%(refname)` never looks the object up, so a loose ref file
        // naming an object that does not exist is listed happily, and
        // `rev-parse --verify --quiet` then exits 1 with EMPTY stderr --
        // which `Repo::rev_parse_verify` reports as absence, not failure.
        // Written as a file because `update-ref` refuses a missing object,
        // which is exactly how such a ref arrives in real life: a fetch or a
        // copy that brought the ref without its objects.
        //
        // The oid must be PLAUSIBLE, not the all-zeros null oid: git treats
        // the null oid as a broken ref and drops it from `for-each-ref`
        // output entirely (`warning: ignoring broken ref ...`), so that
        // version of this test never reaches the arm at all -- measured,
        // git 2.50.1.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let board = Board::new(repo.clone());
        let mine = board
            .append(&Post::new("technique", "main").with("note", serde_json::json!("keep me")))
            .expect("mine");

        let refname = format!("{}dangling", Board::PEERS_PREFIX);
        let path = repo.git_path(&refname).expect("loose ref path");
        std::fs::create_dir_all(path.parent().expect("parent")).expect("ref dir");
        std::fs::write(&path, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef\n")
            .expect("write the loose ref");
        let listed = repo
            .git(&["for-each-ref", "--format=%(refname)", Board::PEERS_PREFIX])
            .expect("for-each-ref");
        assert!(
            listed.contains(&refname),
            "sanity: the dangling ref must be LISTED, or this test exercises nothing: {listed:?}"
        );
        assert_eq!(
            repo.rev_parse_verify(&format!("{refname}^{{commit}}"))
                .expect("absence, not failure"),
            None,
            "sanity: it must resolve to absence rather than an error"
        );

        let ids: Vec<String> = board
            .posts_at_tip()
            .expect("a dangling mirror must never fail the whole read")
            .into_iter()
            .map(|s| s.id)
            .collect();
        assert_eq!(
            ids,
            vec![mine.clone()],
            "the local log must still be read: {ids:?}"
        );
        assert_eq!(
            board.post_ids_at_tip().expect("ids"),
            vec![mine],
            "and the id read must degrade the same way"
        );
    }

    #[test]
    fn the_union_keeps_the_earliest_time_and_the_first_refs_origin_for_one_id() {
        // The dedupe rule, stated on its own because two real appends land in
        // the same wall-clock second and so cannot express it. Earliest wins,
        // matching `digest::history`'s rule for a reap-then-repost of
        // byte-identical content -- one rule for the same situation, not two.
        // Origin comes from the FIRST ref the id was seen in, and `read_refs`
        // puts the local log first, so a post this host also holds is never
        // reported as foreign.
        let same = Post::new("technique", "main");
        let local = StoredPost {
            id: "a".repeat(40),
            post: same.clone(),
            committed_at: 200,
            origin: Origin::Local,
        };
        let peer = StoredPost {
            id: "a".repeat(40),
            post: same,
            committed_at: 100,
            origin: Origin::Peer("lefford".into()),
        };
        let merged = merge_by_id(vec![local, peer]);
        assert_eq!(merged.len(), 1, "one id, one post: {merged:?}");
        assert_eq!(merged[0].committed_at, 100, "the earliest time wins");
        assert_eq!(
            merged[0].origin,
            Origin::Local,
            "a post this host also holds is not foreign"
        );
    }

    #[test]
    fn the_union_is_ordered_oldest_first_across_refs() {
        // `Displayed::cap` drops the OLDEST, so a union that concatenated
        // per-ref runs instead of re-sorting would silently elide by ref
        // rather than by age.
        //
        // The ids COUNTER-SORT against the timestamps deliberately. `merge_by_id`
        // collects into a `BTreeMap` keyed by id, so ids that co-sort with time
        // (`format!("{n:040}")`, the first version of this test) come back in the
        // right order whether or not anything sorts them -- the test passed with
        // the sort deleted entirely. Real ids are content hashes, so their order
        // is random with respect to time; counter-sorting is the cheapest way to
        // make this test see that. `100 - n` assumes every call site passes
        // n <= 100 -- true of the three calls below (10, 20, 30), but not
        // enforced, so a future `post(150)` would underflow-panic in debug
        // rather than fail the assertion it was meant to check.
        let post = |n: u64| StoredPost {
            id: format!("{:040}", 100 - n),
            post: Post::new("technique", "main"),
            committed_at: n,
            origin: Origin::Peer("lefford".into()),
        };
        let merged = merge_by_id(vec![post(30), post(10), post(20)]);
        let times: Vec<u64> = merged.iter().map(|s| s.committed_at).collect();
        assert_eq!(times, vec![10, 20, 30]);
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
        // `digest.rs`'s `history` walk harvests its own ids from `git log`
        // and calls `cat_file_batch` directly, never through `post_ids_in`
        // -- this calls it the same way, with an id that never passed
        // through that upstream filter, and must return `Ok` with the bad
        // id simply absent, not panic.
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
        // A header whose status is a recognised token other than "missing"
        // must cost exactly the one id that produced it, not every id still
        // queued behind it in the chunk.
        //
        // "x1006" and "x3205" are two magic strings (found once, offline,
        // by a birthday search) whose blob ids -- a content-only hash,
        // independent of which repository holds them -- both start with
        // "cf6e":
        //   cf6e7b5aa2d6cc5cb24323c224d9481d5afe3a5b  ("x1006")
        //   cf6e33624013139ce10daeeb07ef4a512212cfcb  ("x3205")
        // so `git cat-file --batch` for the bare prefix "cf6e" reproducibly
        // answers `cf6e ambiguous`, on real git, no mocking.
        //
        // Calls `cat_file_batch_chunk` directly, bypassing
        // `is_full_object_id` (which makes a full-oid ambiguity unreachable
        // through the public `cat_file_batch`), to exercise the
        // header-parsing match arm on its own.
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

        // Sanity: confirm "cf6e" is genuinely ambiguous in THIS repo before
        // trusting the assertions below -- otherwise a change in git's
        // behavior (or a hash algorithm switch) could make both posts
        // survive for a reason unrelated to the fix, and this test would
        // pass while no longer guarding anything.
        let rev_parse_err = repo
            .git(&["rev-parse", "--verify", "cf6e"])
            .expect_err("\"cf6e\" must be genuinely ambiguous, or this test exercises nothing");
        let BoardError::Git { stderr, .. } = rev_parse_err else {
            panic!("expected BoardError::Git, got a different variant");
        };
        assert!(
            stderr.contains("ambiguous"),
            "git must report \"cf6e\" as ambiguous, not some other reason: {stderr}"
        );

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
            "the good post AFTER the ambiguous id must still be read: {found:?}"
        );
    }

    #[test]
    fn cat_file_batch_does_not_deadlock_on_a_batch_large_enough_to_fill_both_pipes() {
        // `Repo::git_stdin_bytes` writes the whole request before reading
        // any response, so a batch whose output fills git's stdout pipe
        // before our stdin drains is a silent, timeout-free hang, not a
        // slow path. Uses the SAME 500-byte object 4000 times, not 4000
        // distinct objects: git emits one full record per requested line
        // regardless, so a repeated id fills both pipes just as well.
        //
        // This crate runs under plain `cargo test`, with NO per-test
        // timeout, so a naive version of this test would itself be a hang
        // generator on a regression. Bounded instead: the call runs on its
        // own thread, and the test waits on a channel with a timeout rather
        // than joining it, so a regression FAILS after the timeout (leaking
        // the stuck thread harmlessly until the process exits) instead of
        // hanging the run.
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
