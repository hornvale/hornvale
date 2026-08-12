//! The Beacon (B2/B3/B6): the transport. `sync` publishes this host's own
//! log to a remote and fetches every peer's; `peer_ages` reports how stale
//! each known peer's mirror is.
//!
//! **Never fatal.** The local append this session cares about has already
//! succeeded before `sync` is ever called (it is the write seam Task 1-6
//! built), so an unreachable remote degrades to exactly the single-box
//! behaviour that shipped before this campaign. Nothing here panics or
//! returns an `Err` a caller has to propagate; every failure becomes a
//! `String` inside [`SyncReport`], for the caller to print on an exit code
//! that still says success.

use crate::BoardError;
use crate::git::Repo;
use crate::live::current_host;
use crate::store::Board;
use std::collections::{BTreeMap, BTreeSet};

/// Where this host publishes its own log. One writer per ref: this host
/// only ever pushes to `hosts/<its own name>`; every other host's slot under
/// this prefix is read-only from here (a peer mirror, fetched into
/// [`Board::PEERS_PREFIX`], never written to directly).
const HOSTS_PREFIX: &str = "refs/hornvale/hosts/";

/// The directory, under the repository's **common** dir (see
/// [`Repo::git_common_path`]), where each peer's last sync time is recorded
/// — one small file per host, holding a unix-seconds decimal string.
///
/// Common, not per-worktree: a fetch serves every worktree of this
/// repository at once, so per-worktree state would report as many different
/// ages as there are worktrees for one fetch.
const SYNC_STATE_DIR: &str = "hv-board-sync";

/// What `sync` accomplished, or failed to.
///
/// Two independent [`Result`]s, not one combined outcome: a push can fail
/// while a fetch succeeds, or the reverse, and collapsing them would leave a
/// caller unable to say which half of the sync a network blip actually hit.
#[derive(Debug)]
pub struct SyncReport {
    /// `Ok(())` if this host's log reached the remote; `Err` names why not.
    pub pushed: Result<(), String>,
    /// `Ok(peers)` — the peer hostnames whose mirrors were fetched — if the
    /// fetch itself ran; `Err` names why not. An empty `Ok` is a legitimate
    /// result: no peer has published anything yet.
    pub fetched: Result<Vec<String>, String>,
}

/// The exact `git push` argv for publishing this host's board to `remote`
/// under its own name.
///
/// Isolated from [`sync`] itself so B3's no-force invariant can be asserted
/// on the argv directly, because the consequence of a force-push here is
/// unobservable until it has already destroyed another host's history (a
/// violation of decision 0118 part 3) — by the time a symptom shows up,
/// there is nothing left to compare against. This must never carry `--force`,
/// `-f`, or a leading `+` on the refspec: every legitimate move of a host's
/// own log is a forward commit (appends CAS forward, `reap` is a forward
/// commit parented on the tip it read), so a rejection here can only mean
/// two machines answer to the same `hostname -s` — see
/// [`explain_push_failure`].
pub fn push_argv(remote: &str, host: &str) -> Vec<String> {
    vec![
        "push".to_string(),
        remote.to_string(),
        format!("refs/hornvale/board:{HOSTS_PREFIX}{host}"),
    ]
}

/// The exact `git fetch` argv for mirroring every peer's log locally.
///
/// The leading `+` here is required, not an oversight to match against
/// [`push_argv`]'s ban: a peer can `reap` (compact) its own log at any time,
/// so its mirror here is not always a fast-forward of what this host last
/// saw — and this ref is a read-only local mirror this host does not own, so
/// a non-fast-forward update to it costs nothing. That asymmetry is the
/// whole point: forbidden on push (this host's own history must never be
/// force-moved by anyone), required on fetch (a peer's compaction must not
/// be refused here).
pub fn fetch_argv(remote: &str) -> Vec<String> {
    vec![
        "fetch".to_string(),
        remote.to_string(),
        format!("+{HOSTS_PREFIX}*:{}*", Board::PEERS_PREFIX),
    ]
}

/// Turn a rejected push's stderr into the one hypothesis worth acting on —
/// but only when the failure genuinely IS a rejection.
///
/// Every legitimate move of a host's own log is a forward commit, so a
/// *rejection* can only mean something illegitimate happened — and the
/// overwhelmingly likely cause is two machines both answering to
/// `hostname -s` == `host`, both publishing under the same
/// `refs/hornvale/hosts/<host>` slot. But "the push failed" and "the push
/// was rejected" are not the same event: the commonest failure by far is an
/// unreachable remote, expired credentials, or no network at all, and NONE
/// of those are a rejection — git's own stderr for them never says
/// "rejected" or "non-fast-forward" (measured: `fatal: 'no-such-remote'
/// does not appear to be a git repository` names none of that). Applying
/// the hostname-collision hypothesis to an offline box announces the
/// campaign's most alarming hazard for its single most ordinary failure,
/// and calls a push "rejected" that the remote never even saw — so this
/// checks for those two tokens first and passes anything else through
/// verbatim, unembellished.
///
/// The collision wording itself never suggests `--force`: forcing here is
/// the one operation in this whole design capable of violating decision
/// 0118 part 3 across hosts, discarding history the local ref no longer
/// holds.
pub fn explain_push_failure(host: &str, stderr: &str) -> String {
    let trimmed = stderr.trim();
    let lower = trimmed.to_lowercase();
    if lower.contains("rejected") || lower.contains("non-fast-forward") {
        format!(
            "push to refs/hornvale/hosts/{host} was rejected ({trimmed}) -- every legitimate \
             move of a host's own log is a forward commit, so this almost certainly means \
             another machine also answers to the hostname \"{host}\" and is publishing under \
             the same ref; never force this push (0118 part 3) -- rename one of the two hosts \
             instead"
        )
    } else {
        format!("push to refs/hornvale/hosts/{host} failed: {trimmed}")
    }
}

/// Publish this host's own board to `remote`, then fetch every peer's.
///
/// Push before fetch (B2): `origin` is the hub, so each host reaches it
/// independently and the two directions are symmetric — one host being
/// unable to resolve another over the network stops mattering, because
/// neither ever talks to the other directly. This function attempts both
/// regardless of whether the other succeeded, and reports each
/// independently in [`SyncReport`] rather than short-circuiting on the
/// first failure.
///
/// A successful fetch records "synced now" for every peer the fetch touched
/// ([`record_sync`]), which is what makes [`peer_ages`] meaningful — sync
/// age is the only local signal a `notice` (no `ttl_s`) leaves behind once
/// its authoring host goes quiet.
pub fn sync(repo: &Repo, remote: &str) -> SyncReport {
    let host = current_host();
    // Wall clock: sync bookkeeping, not sim state. This crate's one
    // sanctioned use is `live.rs`'s probe and `main.rs`'s `digest`; this is
    // the same class of call, kept local to where it is used rather than
    // threaded in as a parameter, matching this crate's existing pattern.
    #[allow(clippy::disallowed_types)]
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);

    let push_args = push_argv(remote, &host);
    let push_refs: Vec<&str> = push_args.iter().map(String::as_str).collect();
    let pushed = match repo.git(&push_refs) {
        Ok(_) => Ok(()),
        Err(BoardError::Git { stderr, .. }) => Err(explain_push_failure(&host, &stderr)),
        Err(other) => Err(other.to_string()),
    };

    let fetch_args = fetch_argv(remote);
    let fetch_refs: Vec<&str> = fetch_args.iter().map(String::as_str).collect();
    let fetched = match repo.git(&fetch_refs) {
        Ok(_) => {
            let peers = list_peer_hosts(repo).unwrap_or_default();
            for peer in &peers {
                if let Err(e) = record_sync(repo, peer, now) {
                    eprintln!("board: could not record a sync time for {peer}: {e}");
                }
            }
            Ok(peers)
        }
        Err(e) => Err(e.to_string()),
    };

    SyncReport { pushed, fetched }
}

/// Peer host names currently mirrored under [`Board::PEERS_PREFIX`], read
/// from the ref names themselves — **excluding this host's own name**.
///
/// A wildcard fetch (`+refs/hornvale/hosts/*:refs/hornvale/peers/*`) cannot
/// tell "self" from "peer" at the ref-name level: once this host has ever
/// pushed, its own log mirrors right back into `refs/hornvale/peers/<this
/// host>` on the very same fetch (measured live: pushing from `ambrose`
/// and fetching immediately after populates `refs/hornvale/peers/ambrose`).
/// [`Board::read_refs`] already excludes this case for the same reason —
/// unioning a stale self-mirror could resurrect a post this host itself
/// reaped — and the exclusion matters here for a different reason: without
/// it, this host would show up as a "peer" of itself in [`peer_ages`]'s
/// report and in [`record_sync`]'s bookkeeping, which is noise, not
/// staleness.
fn list_peer_hosts(repo: &Repo) -> Result<Vec<String>, BoardError> {
    let out = repo.git(&["for-each-ref", "--format=%(refname)", Board::PEERS_PREFIX])?;
    let own = current_host();
    Ok(out
        .lines()
        .filter_map(|line| line.strip_prefix(Board::PEERS_PREFIX))
        .filter(|host| !host.is_empty() && *host != own)
        .map(str::to_string)
        .collect())
}

/// Record that `host`'s mirror was synced at `now_unix`.
///
/// Lives under the repository's **common** dir ([`Repo::git_common_path`]),
/// not the per-worktree one — a fetch serves every worktree at once, so
/// per-worktree state would report nine different ages for one fetch.
pub fn record_sync(repo: &Repo, host: &str, now_unix: u64) -> Result<(), BoardError> {
    let dir = repo.git_common_path(SYNC_STATE_DIR)?;
    std::fs::create_dir_all(&dir)
        .map_err(|e| BoardError::Io(format!("creating {}: {e}", dir.display())))?;
    let path = dir.join(host);
    std::fs::write(&path, now_unix.to_string())
        .map_err(|e| BoardError::Io(format!("writing {}: {e}", path.display())))
}

/// Every recorded sync time, host -> unix seconds.
///
/// Absent or unreadable degrades to "nothing recorded yet" rather than an
/// error (D7's fail-open convention, extended to sync bookkeeping): a
/// missing directory just means no one has synced from here yet, not a
/// reason to blank the whole report.
fn read_sync_times(repo: &Repo) -> BTreeMap<String, u64> {
    let mut out = BTreeMap::new();
    let Ok(dir) = repo.git_common_path(SYNC_STATE_DIR) else {
        return out;
    };
    let Ok(entries) = std::fs::read_dir(&dir) else {
        return out;
    };
    for entry in entries.flatten() {
        let Ok(host) = entry.file_name().into_string() else {
            continue;
        };
        let Ok(text) = std::fs::read_to_string(entry.path()) else {
            continue;
        };
        if let Ok(t) = text.trim().parse::<u64>() {
            out.insert(host, t);
        }
    }
    out
}

/// Every peer this host knows about, and how long ago (in seconds) it was
/// last synced — `None` for a peer that has never been synced at all.
///
/// **0119: an instrument's silence must never read as "nothing is happening
/// over there."** Task 6 established that a foreign post cannot decay
/// locally — the authoring host alone decides, and `reap` is single-ref — and
/// a `notice` (the kind carrying `polarity=hold-off`) carries no `ttl_s` at
/// all, so nothing local bounds its lifetime. For a frozen or retired peer,
/// sync age is the *only* remaining signal that its content might be stale.
/// A peer omitted from this list reads as though it does not exist, which is
/// strictly worse than reading as "never synced" — so the known-peer set
/// here is the union of every ref currently mirrored under
/// [`Board::PEERS_PREFIX`] and every host this repository has *ever*
/// recorded a sync time for: a peer can appear in one without the other (a
/// compacted-away mirror still has a recorded time; a mirror this very
/// fetch just created has no recorded time until [`sync`] calls
/// [`record_sync`] on it), and dropping either half would silently narrow
/// the set this predicate exists to keep complete.
///
/// This host's own name is excluded from both halves of that union, for the
/// same reason [`list_peer_hosts`] excludes it from the ref half: a
/// self-mirror is not a peer, and a legacy or foreign recorded-time file
/// bearing this host's own name (e.g. from a version of this tool that
/// predates that exclusion) must not resurrect it as one either.
pub fn peer_ages(repo: &Repo, now_unix: u64) -> Vec<(String, Option<u64>)> {
    let own = current_host();
    let mut hosts: BTreeSet<String> = BTreeSet::new();
    if let Ok(peers) = list_peer_hosts(repo) {
        hosts.extend(peers);
    }
    let times = read_sync_times(repo);
    hosts.extend(times.keys().filter(|h| **h != own).cloned());

    hosts
        .into_iter()
        .map(|host| {
            let age = times
                .get(&host)
                .map(|synced_at| now_unix.saturating_sub(*synced_at));
            (host, age)
        })
        .collect()
}

/// How long ago each known peer's log **itself** last actually moved — the
/// mirror ref's most recent commit time, not when this host last fetched it.
///
/// This is the signal the spec amendment behind B6 (commit `6fb5301a`)
/// actually needs, and [`peer_ages`] alone cannot give it: a host that syncs
/// on a regular cadence reports every peer's MIRROR as freshly synced
/// forever, even while a given peer has posted nothing in a month —
/// [`peer_ages`]'s age answers "is our view of this peer current," not "is
/// this peer's content current." Reported separately rather than folded
/// into `peer_ages` itself, because the two questions have different
/// failure surfaces (a network problem bounds the first; the peer's own
/// silence bounds the second) and `peer_ages`'s existing signature and
/// tests are pinned by the brief this module was built against.
///
/// A dangling or unresolvable mirror degrades to `None` for that one host,
/// not a failed read: `git log -1` runs per host, so one bad ref costs one
/// entry, never the whole listing — deliberately not a dereferencing
/// `for-each-ref` format, which `Board::resolved_read_refs`'s own doc
/// comment documents as exiting 128 on a single bad mirror.
pub fn peer_content_ages(repo: &Repo, now_unix: u64) -> Vec<(String, Option<u64>)> {
    let hosts = list_peer_hosts(repo).unwrap_or_default();
    hosts
        .into_iter()
        .map(|host| {
            let refname = format!("{}{host}", Board::PEERS_PREFIX);
            let age = repo
                .git(&["log", "-1", "--format=%ct", &refname])
                .ok()
                .and_then(|out| out.trim().parse::<u64>().ok())
                .map(|posted_at| now_unix.saturating_sub(posted_at));
            (host, age)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::post::Post;
    use serde_json::json;

    #[test]
    fn a_push_is_never_forced() {
        // The one operation in this design that could violate 0118 part 3
        // across hosts. Asserted on the argv, because the consequence of
        // getting it wrong is unobservable until it has already destroyed
        // history. B3.
        //
        // An exact `assert_eq!` on the whole vector, not merely a
        // does-not-contain check on `--force`/`-f`/a leading `+`: those
        // three tokens are what could sneak in TODAY, but the guard has to
        // survive a future edit, not just today's code -- `--mirror`
        // (which deletes remote refs), `--delete`, `--prune`, or
        // `--force-with-lease` would each slip past a contains-check
        // unnoticed. Locking the exact shape is what makes any of those
        // additions a visible, deliberate diff against this test instead of
        // a silent pass.
        let args = push_argv("origin", "ambrose");
        assert_eq!(
            args,
            vec![
                "push".to_string(),
                "origin".to_string(),
                "refs/hornvale/board:refs/hornvale/hosts/ambrose".to_string(),
            ],
            "the push argv must be exactly this shape, nothing added: {args:?}"
        );
    }

    #[test]
    fn a_fetch_refspec_carries_the_required_leading_plus() {
        // The asymmetry's other half: forbidden on push, required on fetch.
        // A peer's log can be compacted, so its mirror here is not always a
        // fast-forward, and this is a read-only local mirror we do not own.
        let args = fetch_argv("origin");
        assert!(
            args.iter()
                .any(|a| a.starts_with('+') && a.contains("refs/hornvale/hosts/*")),
            "the fetch refspec must force-update the local peer mirror: {args:?}"
        );
    }

    #[test]
    fn a_rejected_push_names_the_hostname_collision_hypothesis() {
        // Every legitimate move of a host's own log is a forward commit, so
        // a rejection means two machines both answer to this name.
        let msg = explain_push_failure("ambrose", "! [rejected] (non-fast-forward)");
        assert!(msg.contains("ambrose"));
        assert!(msg.to_lowercase().contains("hostname"), "got {msg}");
        assert!(
            !msg.contains("--force") && !msg.contains("-f "),
            "must never suggest forcing the push: {msg}"
        );
    }

    #[test]
    fn a_non_rejection_failure_is_passed_through_without_the_collision_hypothesis() {
        // Important 1 (Task 7 review): the commonest failure by far --
        // offline, VPN, expired credentials, an unreachable remote -- is
        // NOT a rejection, and must not be announced as the campaign's most
        // alarming hazard. This is git's real stderr for a nonexistent
        // remote (measured live against `no-such-remote`).
        let msg = explain_push_failure(
            "ambrose",
            "fatal: 'no-such-remote' does not appear to be a git repository\n\
             fatal: Could not read from remote repository.",
        );
        assert!(
            !msg.to_lowercase().contains("hostname"),
            "an unreachable remote is not a hostname collision: {msg}"
        );
        assert!(
            !msg.contains("rejected"),
            "it was never rejected -- the remote never saw it: {msg}"
        );
        assert!(
            msg.contains("no-such-remote"),
            "the real diagnosis must still be visible: {msg}"
        );
    }

    #[test]
    fn sync_failure_is_never_fatal_and_the_local_post_stands() {
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let board = Board::new(repo.clone());
        let id = board
            .append(&Post::new("technique", "main").with("note", json!("kept")))
            .expect("post");
        let report = sync(&repo, "no-such-remote");
        assert!(
            report.pushed.is_err(),
            "an absent remote must fail the push"
        );
        assert!(
            board
                .posts_at_tip()
                .expect("read")
                .iter()
                .any(|s| s.id == id),
            "the local post must survive a failed sync"
        );
    }

    #[test]
    fn a_never_synced_peer_is_distinguishable_from_a_just_synced_one() {
        // 0119: an instrument's silence means the claim held, so a peer that
        // has never been synced must not read as "nothing is happening over
        // there".
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let ages = peer_ages(&repo, 1_000_000);
        assert!(
            ages.iter().all(|(_, age)| age.is_none()),
            "no sync recorded yet"
        );
        record_sync(&repo, "lefford", 999_000).expect("record");
        let ages = peer_ages(&repo, 1_000_000);
        assert_eq!(
            ages.iter()
                .find(|(h, _)| h == "lefford")
                .and_then(|(_, a)| *a),
            Some(1_000)
        );
    }

    #[test]
    fn a_self_recorded_sync_time_is_never_reported_as_a_peer() {
        // Measured live (2026-08-11): a wildcard fetch cannot tell "self"
        // from "peer" at the ref-name level, so once a host has pushed, its
        // own log mirrors right back into `refs/hornvale/peers/<itself>` on
        // the very next fetch -- and, before `list_peer_hosts` excluded
        // this case, `sync` recorded a stray self-sync-time file that then
        // made THIS HOST show up as its own "peer" in every render's peer
        // header. Even with a legacy or foreign recorded-time file bearing
        // this host's own name still on disk, `peer_ages` must not surface
        // it.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let own = current_host();
        record_sync(&repo, &own, 500).expect("record a self entry directly, bypassing sync()");
        let ages = peer_ages(&repo, 1_000);
        assert!(
            ages.iter().all(|(h, _)| *h != own),
            "this host's own name must never appear in its own peer list: {ages:?}"
        );
    }

    #[test]
    fn record_sync_lives_in_the_common_dir_not_the_per_worktree_one() {
        // The requirement this whole module rests on: a fetch serves every
        // worktree at once, so this state must be visible from a SECOND
        // `Repo` handle on the same repository root, not squirreled away in
        // a per-worktree private path that a different worktree could never
        // see. This test does not need an actual linked worktree to pin
        // that -- it pins the weaker, sufficient property that two
        // independent `Repo` values pointed at the same root agree, which is
        // exactly what `--git-common-dir` guarantees and `--git-path` does
        // not (see `Repo::git_common_path`'s doc comment).
        let (dir, repo) = crate::git::test_support::temp_repo();
        record_sync(&repo, "lefford", 500).expect("record");
        let second = crate::git::Repo::new(&dir);
        let ages = peer_ages(&second, 1_500);
        assert_eq!(
            ages.iter()
                .find(|(h, _)| h == "lefford")
                .and_then(|(_, a)| *a),
            Some(1_000),
            "a second Repo handle on the same root must see the same recorded sync"
        );
    }

    #[test]
    fn a_peer_known_only_from_a_mirror_ref_reads_as_never_synced() {
        // The other half of the union `peer_ages` draws its known-peer set
        // from: a mirror ref can exist with no recorded sync time at all
        // (this run's very first fetch, before `sync` calls `record_sync`
        // on it) -- that peer must still be reported, as never-synced, not
        // silently dropped for lacking a timestamp.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let board = Board::with_ref(repo.clone(), &format!("{}lefford", Board::PEERS_PREFIX));
        board
            .append(&Post::new("technique", "campaign/x"))
            .expect("seed a peer mirror ref");
        let ages = peer_ages(&repo, 1_000);
        assert_eq!(
            ages.iter()
                .find(|(h, _)| h == "lefford")
                .and_then(|(_, a)| *a),
            None,
            "a mirror ref with no recorded sync time must read as never-synced, not be dropped"
        );
    }

    #[test]
    fn peer_content_ages_reports_how_long_since_the_peer_actually_posted() {
        // The spec-gap fix: read the peer mirror's own real commit time back
        // out of git rather than mint a third wall-clock call site, exactly
        // as `digest.rs`'s `history_reads_every_post_in_the_window_in_one_batch`
        // already does -- the test is then exact (age == 500), not a
        // "should be small" approximation.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let board = Board::with_ref(repo.clone(), &format!("{}lefford", Board::PEERS_PREFIX));
        board
            .append(&Post::new("technique", "campaign/x"))
            .expect("seed a peer mirror");
        let tip = board.tip().expect("tip").expect("some");
        let posted_at: u64 = repo
            .git(&["log", "-1", "--format=%ct", &tip])
            .expect("commit time")
            .parse()
            .expect("timestamp");
        let ages = peer_content_ages(&repo, posted_at + 500);
        assert_eq!(
            ages.iter()
                .find(|(h, _)| h == "lefford")
                .and_then(|(_, a)| *a),
            Some(500)
        );
    }

    #[test]
    fn peer_content_ages_is_none_for_a_dangling_mirror_ref_not_an_error() {
        // A per-ref failure, not a whole-listing one: `git log -1` on ONE bad
        // ref must not blank the report for every other peer. Written as a
        // loose ref file naming a plausible-but-absent object, exactly as
        // `store.rs`'s `a_peer_ref_naming_an_absent_object_is_skipped_with_a_warning_not_fatal`
        // does, for the same reason: `update-ref` refuses a missing object
        // outright, and the null oid gets dropped from `for-each-ref`
        // entirely -- neither reaches the arm this test exercises.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let refname = format!("{}dangling", Board::PEERS_PREFIX);
        let path = repo.git_path(&refname).expect("loose ref path");
        std::fs::create_dir_all(path.parent().expect("parent")).expect("ref dir");
        std::fs::write(&path, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef\n")
            .expect("write the loose ref");
        let ages = peer_content_ages(&repo, 1_000);
        assert_eq!(
            ages.iter()
                .find(|(h, _)| h == "dangling")
                .and_then(|(_, a)| *a),
            None,
            "a dangling mirror must read as unknown, not crash or blank the report"
        );
    }
}
