//! Read-time liveness. D10 — the store is dumb; nothing here is ever written
//! back. A claim decays, a notice dies with its branch, technique is durable.

use crate::BoardError;
use crate::git::Repo;
use crate::store::StoredPost;
use std::collections::BTreeSet;

/// Why a post is or is not currently worth rendering.
#[derive(Debug, Clone, PartialEq)]
pub enum Liveness {
    /// Render it.
    Live,
    /// Do not render it as current; the string is the physical reason.
    Expired(String),
    /// Explicitly withdrawn by a later post.
    Retracted(String),
}

/// Everything liveness needs to know about the world right now.
#[derive(Debug, Clone)]
pub struct LiveContext {
    /// Wall clock, unix seconds. Permitted here: this is a tool, not the sim.
    pub now_unix: u64,
    /// `hostname -s` — claims from other hosts are not judged locally (D8).
    pub host: String,
    /// Ids named by `retract` posts.
    pub retracted: BTreeSet<String>,
    /// Live pids on this host.
    pub live_pids: BTreeSet<u32>,
    /// Branches that exist and are not merged into `main`.
    pub live_branches: BTreeSet<String>,
    /// Branches that RESOLVE and ARE merged into `main` — an unambiguous,
    /// positive "definitely dead" signal that `is_reapable` treats
    /// differently from a branch that simply does not resolve at all (see
    /// `is_reapable`'s doc comment for why the distinction matters). "Merged"
    /// here means genuinely absorbed and superseded (no commits of its own
    /// left outstanding, AND `main` has since moved past it) — not merely
    /// identical to `main`'s tip. `main` compared with itself, and a
    /// campaign branch that has not yet made its first commit, are both
    /// identical-to-`main` rather than merged, and belong in
    /// `live_branches` (B11).
    pub merged_branches: BTreeSet<String>,
}

/// Judge one already-run `ps -p <pid>` invocation. `Ok` means `ps` ran, so its
/// exit status is authoritative: success is alive, failure is dead. `Err`
/// means `ps` itself could not be spawned (missing binary, a sandboxed PATH,
/// …), so liveness is *unknown*, not dead — this board's claims exist so a
/// session can see another session is using the box, and a claim that wrongly
/// vanishes causes a silent double-start (the exact harm the board prevents),
/// while a claim that wrongly persists is merely visible and self-correcting
/// on the next TTL. So this fails OPEN on a spawn error: treat the pid as
/// live, and say so loudly on stderr rather than swallowing the unknown.
fn pid_probe_alive(pid: u32, spawned: std::io::Result<std::process::Output>) -> bool {
    match spawned {
        Ok(out) => out.status.success(),
        Err(e) => {
            eprintln!(
                "board: could not run `ps -p {pid}` ({e}); process liveness for pid {pid} is \
                 unknown, not dead -- treating it as live so its claim keeps rendering"
            );
            true
        }
    }
}

/// Resolve `by` to the single, unambiguous ref that answers "does this
/// branch exist" — preferring the disambiguated `refs/heads/<by>` form, and
/// falling back to the bare name only if that disambiguated form does not
/// exist (so a `by` that already names a fully-qualified ref still
/// resolves). Returns that resolved ref, never the bare name, so a caller
/// that then asks git a second question about the same branch (e.g.
/// `merge-base`) asks it about the exact ref this function found — not a
/// bare name git could re-disambiguate to something else entirely.
///
/// This is I1's fix: git's ref-disambiguation order for a bare name is
/// `refs/<name>`, then `refs/tags/<name>`, then `refs/heads/<name>`
/// (gitrevisions(7)), so a tag sharing a branch's name can silently steal a
/// second, independent bare-name lookup even though the first one (this
/// function) correctly found the branch. Resolving once and reusing the
/// resolved ref closes that gap structurally: there is no second bare-name
/// lookup left to go astray.
fn resolve_branch_ref(repo: &Repo, by: &str) -> Result<Option<String>, BoardError> {
    let qualified = format!("refs/heads/{by}");
    if repo.rev_parse_verify(&qualified)?.is_some() {
        return Ok(Some(qualified));
    }
    if repo.rev_parse_verify(by)?.is_some() {
        return Ok(Some(by.to_string()));
    }
    Ok(None)
}

impl LiveContext {
    /// Probe the world once for a whole render.
    pub fn probe(repo: &Repo, posts: &[StoredPost]) -> Result<Self, BoardError> {
        // Wall clock: TTL is meaningless without it. Tool-only (see the crate's
        // Global Constraints); the sim's no-wall-clock rule is untouched.
        // The workspace-wide `disallowed-types` lint (decision 0001) exists to
        // keep wall-clock time out of the *simulation*; this crate is a tool
        // outside that workspace and this is the one sanctioned call site.
        #[allow(clippy::disallowed_types)]
        let now_unix = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_err(|e| BoardError::Io(format!("clock: {e}")))?
            .as_secs();

        let host = std::process::Command::new("hostname")
            .arg("-s")
            .output()
            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
            .unwrap_or_default();

        let retracted = posts
            .iter()
            .filter(|s| s.post.kind == "retract")
            .filter_map(|s| s.post.str_field("post").map(str::to_string))
            .collect();

        let mut live_pids = BTreeSet::new();
        for s in posts.iter().filter(|s| s.post.kind == "claim") {
            if s.post.str_field("host") != Some(host.as_str()) {
                continue;
            }
            if let Some(pid) = s.post.u64_field("pid") {
                let pid = pid as u32;
                // `ps -p` works on Darwin and Linux alike; `kill -0` would need
                // libc, which this crate deliberately does not depend on.
                let spawned = std::process::Command::new("ps")
                    .args(["-p", &pid.to_string()])
                    .output();
                if pid_probe_alive(pid, spawned) {
                    live_pids.insert(pid);
                }
            }
        }

        let mut live_branches = BTreeSet::new();
        let mut merged_branches = BTreeSet::new();
        for s in posts.iter() {
            let by = s.post.by.clone();
            if live_branches.contains(&by) || merged_branches.contains(&by) {
                continue;
            }
            // Resolve once; reuse the resolved ref for the ancestry check
            // below (I1) rather than asking git a second, bare-name question
            // it could answer about a different ref entirely.
            let Some(resolved) = resolve_branch_ref(repo, &by)? else {
                continue;
            };
            // `main` qualified too, for the same reason as `resolved` above:
            // a tag named "main" would let the right-hand side of this same
            // ambiguity steal the answer just as easily as the left-hand
            // side did before I1's fix.
            //
            // B11: a plain ancestor check (`merge-base --is-ancestor resolved
            // main`) is trivially true whenever `resolved` and `main` are the
            // SAME commit, not just when `resolved` is a strict ancestor --
            // and that is exactly `by == "main"` comparing itself, or a
            // freshly-branched campaign that has not yet made a commit of its
            // own. Both were being classified `merged` and filtered from
            // every render (and were one `reap` away from permanent
            // deletion). Ahead/behind counts distinguish the two: "merged"
            // means the branch's own commits are fully absorbed (ahead == 0)
            // AND main has since moved past it (behind > 0); a branch that is
            // merely IDENTICAL to main (ahead == 0, behind == 0) has not
            // diverged at all and is not "merged" in any meaningful sense.
            // On any failure to compute the counts (a `refs/heads/main` that
            // itself does not resolve, say), this errs toward LIVE, not
            // merged -- an uncertain classification must render, never
            // silently vanish (the exact failure mode this predicate exists
            // to close).
            let merged = repo
                .git(&[
                    "rev-list",
                    "--left-right",
                    "--count",
                    &format!("{resolved}...refs/heads/main"),
                ])
                .ok()
                .and_then(|counts| {
                    let mut parts = counts.split_whitespace();
                    let ahead: u64 = parts.next()?.parse().ok()?;
                    let behind: u64 = parts.next()?.parse().ok()?;
                    Some(ahead == 0 && behind > 0)
                })
                .unwrap_or(false);
            if merged {
                merged_branches.insert(by);
            } else {
                live_branches.insert(by);
            }
        }

        Ok(Self {
            now_unix,
            host,
            retracted,
            live_pids,
            live_branches,
            merged_branches,
        })
    }
}

/// Judge one post. Retraction wins over everything; `technique` never decays.
pub fn liveness(stored: &StoredPost, ctx: &LiveContext) -> Liveness {
    if ctx.retracted.contains(&stored.id) {
        return Liveness::Retracted("retracted by a later post".to_string());
    }
    match stored.post.kind.as_str() {
        "claim" => {
            // A convention-numeric field that is not a number silently
            // SKIPS its decay check -- an unparseable `ttl_s` is an immortal
            // claim, decision 0080's stuck alarm by typo. The tool does not
            // reject the post (D12: it validates nothing beyond `kind` and
            // `by`), but it must not draw the wrong conclusion quietly: say
            // so every time the conclusion is drawn.
            for field in stored.post.non_numeric_convention_fields() {
                eprintln!(
                    "board: claim {} has a non-numeric `{field}` ({}); its {} check is being \
                     SKIPPED, so this claim may never decay -- repost it with a numeric value",
                    stored.id,
                    stored
                        .post
                        .extra
                        .get(field)
                        .map(std::string::ToString::to_string)
                        .unwrap_or_default(),
                    if field == "ttl_s" { "ttl" } else { "process" },
                );
            }
            if let Some(ttl) = stored.post.u64_field("ttl_s") {
                let age = ctx.now_unix.saturating_sub(stored.committed_at);
                if age > ttl {
                    return Liveness::Expired(format!("ttl_s {ttl} elapsed ({age}s old)"));
                }
            }
            // Only this host's process table is authoritative for this host's
            // claims (D8).
            if stored.post.str_field("host") == Some(ctx.host.as_str())
                && let Some(pid) = stored.post.u64_field("pid")
                && !ctx.live_pids.contains(&(pid as u32))
            {
                return Liveness::Expired(format!("pid {pid} is not running on {}", ctx.host));
            }
            Liveness::Live
        }
        "notice" => {
            if ctx.live_branches.contains(&stored.post.by) {
                Liveness::Live
            } else {
                Liveness::Expired(format!("branch {} is gone or merged", stored.post.by))
            }
        }
        // Technique and threads are durable; unknown kinds are never silently
        // suppressed (D12).
        _ => Liveness::Live,
    }
}

/// How long a notice's authoring branch may sit unresolved before `reap` is
/// permitted to treat its absence as durable.
///
/// Seven days: far beyond any plausible transient race (a fetch in flight,
/// a permissions blip, a delete-and-recreate window), bounded so the tip
/// tree does not carry a dead notice forever, and short compared to the
/// interval over which anyone would notice tree size — so waiting it out
/// costs little. See `is_reapable`'s doc comment for why this gate exists
/// only for the unresolved-branch case and nowhere else.
pub const NOTICE_GRACE_PERIOD_S: u64 = 7 * 24 * 60 * 60;

/// Whether `reap` may permanently drop this post from the tip tree.
///
/// Deliberately a SEPARATE predicate from `liveness`, not the same
/// threshold behind a flag: `liveness` governs what a render shows THIS
/// TIME — wrong once, self-correcting on the next run — while this governs
/// what disappears from the tip tree FOREVER (history still holds the
/// bytes, per D13, but nothing will render it again once dropped). The two
/// questions read the same underlying signals very differently:
///
/// - A `claim` past its TTL, or bound to a pid that a live process table
///   genuinely does not contain, is unambiguous: time only moves forward,
///   and `pid_probe_alive` already fails open on a spawn error rather than
///   reporting a false negative. Reaped exactly when `liveness` would mark
///   it `Expired`.
/// - A `notice` whose branch RESOLVES and IS merged into `main` is
///   unambiguous: a resolved ref plus a successful ancestry check is a
///   positive statement that no transient race can produce. Reaped
///   immediately, regardless of age.
/// - A `notice` whose branch DOES NOT RESOLVE at all is ambiguous — gone
///   for good, or a transient hiccup that will resolve normally moments
///   later. Reaped only once it has sat unresolved for longer than
///   `NOTICE_GRACE_PERIOD_S`; a single unresolved reading is never enough
///   on its own.
/// - Every other kind (durable posts, and a notice or claim that is
///   genuinely still live) is never reaped.
///
/// Retraction is unambiguous regardless of kind — an explicit later post
/// naming this one — and is reaped immediately, exactly as `liveness`
/// treats it.
pub fn is_reapable(stored: &StoredPost, ctx: &LiveContext) -> bool {
    if ctx.retracted.contains(&stored.id) {
        return true;
    }
    match stored.post.kind.as_str() {
        "claim" => matches!(liveness(stored, ctx), Liveness::Expired(_)),
        "notice" => {
            if ctx.merged_branches.contains(&stored.post.by) {
                return true;
            }
            if ctx.live_branches.contains(&stored.post.by) {
                return false;
            }
            // Unresolved: ambiguous. Wait out the grace period rather than
            // trust a single reading -- see `NOTICE_GRACE_PERIOD_S`.
            let age = ctx.now_unix.saturating_sub(stored.committed_at);
            age > NOTICE_GRACE_PERIOD_S
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::post::Post;
    use serde_json::json;
    use std::collections::BTreeSet;

    fn stored(post: Post, id: &str, at: u64) -> StoredPost {
        StoredPost {
            id: id.to_string(),
            post,
            committed_at: at,
        }
    }

    fn ctx() -> LiveContext {
        LiveContext {
            now_unix: 1_000,
            host: "ambrose".into(),
            retracted: BTreeSet::new(),
            live_pids: BTreeSet::from([42]),
            live_branches: BTreeSet::from(["campaign/live".to_string()]),
            merged_branches: BTreeSet::new(),
        }
    }

    #[test]
    fn a_claim_inside_its_ttl_with_a_live_pid_is_live() {
        let p = Post::new("claim", "campaign/live")
            .with("host", json!("ambrose"))
            .with("pid", json!(42))
            .with("ttl_s", json!(900));
        assert!(matches!(
            liveness(&stored(p, "a", 900), &ctx()),
            Liveness::Live
        ));
    }

    #[test]
    fn a_claim_past_its_ttl_is_expired_and_says_why() {
        let p = Post::new("claim", "campaign/live")
            .with("host", json!("ambrose"))
            .with("pid", json!(42))
            .with("ttl_s", json!(60));
        match liveness(&stored(p, "a", 100), &ctx()) {
            Liveness::Expired(why) => assert!(why.contains("ttl"), "reason names the cause: {why}"),
            other => panic!("expected expired, got {other:?}"),
        }
    }

    #[test]
    fn a_claim_whose_pid_is_dead_is_expired_inside_its_ttl() {
        let p = Post::new("claim", "campaign/live")
            .with("host", json!("ambrose"))
            .with("pid", json!(9999))
            .with("ttl_s", json!(900));
        match liveness(&stored(p, "a", 900), &ctx()) {
            Liveness::Expired(why) => assert!(why.contains("pid"), "reason names the cause: {why}"),
            other => panic!("expected expired, got {other:?}"),
        }
    }

    #[test]
    fn a_claim_with_a_non_numeric_ttl_stays_live_and_is_reported_as_the_hazard_it_is() {
        // I8. This pins the HAZARD, not a fix: a non-numeric `ttl_s` skips the
        // TTL check, so this claim is immortal by that route. Changing the
        // behaviour to expire it would be a silent loss of a possibly-live
        // claim -- the class this campaign fought four times -- so the answer
        // is to keep the conservative reading and make the typo loud
        // (`liveness` warns on stderr, and `board post` warns at write time).
        // If this assertion ever flips, the change needs its own decision.
        let p = Post::new("claim", "campaign/live")
            .with("host", json!("ambrose"))
            .with("pid", json!(42))
            .with("ttl_s", json!("60s")); // the typo
        assert!(
            matches!(liveness(&stored(p.clone(), "a", 0), &ctx()), Liveness::Live),
            "a claim whose ttl_s cannot be read has no TTL to be past"
        );
        assert_eq!(
            p.non_numeric_convention_fields(),
            vec!["ttl_s"],
            "and the reader must be told why it will never decay"
        );
        assert!(
            !is_reapable(&stored(p, "a", 0), &ctx()),
            "so a reap must not drop it either -- it never reads as Expired"
        );
    }

    #[test]
    fn a_claim_from_another_host_is_not_judged_by_our_process_table() {
        // D8: claims are physical. Another box's pid means nothing here.
        let p = Post::new("claim", "campaign/live")
            .with("host", json!("lefford"))
            .with("pid", json!(9999))
            .with("ttl_s", json!(900));
        assert!(matches!(
            liveness(&stored(p, "a", 900), &ctx()),
            Liveness::Live
        ));
    }

    #[test]
    fn a_notice_from_a_vanished_branch_is_expired() {
        // D9: an abandoned campaign's warning must stop costing other sessions.
        let p = Post::new("notice", "campaign/gone").with("subject", json!("elevation"));
        match liveness(&stored(p, "a", 900), &ctx()) {
            Liveness::Expired(why) => assert!(why.contains("branch"), "reason: {why}"),
            other => panic!("expected expired, got {other:?}"),
        }
    }

    #[test]
    fn a_retracted_post_is_retracted_whatever_else_is_true() {
        let mut c = ctx();
        c.retracted.insert("a".to_string());
        let p = Post::new("notice", "campaign/live");
        assert!(matches!(
            liveness(&stored(p, "a", 900), &c),
            Liveness::Retracted(_)
        ));
    }

    #[test]
    fn a_technique_post_never_expires() {
        // Sec 1b: technique is the durable half; only claims and notices decay.
        let p = Post::new("technique", "campaign/gone").with("note", json!("mktree hates slashes"));
        assert!(matches!(
            liveness(&stored(p, "a", 1), &ctx()),
            Liveness::Live
        ));
    }

    #[test]
    fn is_reapable_drops_a_notice_whose_branch_resolves_and_is_merged_regardless_of_age() {
        let mut c = ctx();
        c.merged_branches.insert("campaign/merged".to_string());
        let p = Post::new("notice", "campaign/merged");
        // Fresh (age 0 relative to `now_unix`) and long-committed both reap:
        // the merged-branch signal is unambiguous and unaffected by age.
        assert!(is_reapable(&stored(p.clone(), "a", c.now_unix), &c));
        assert!(is_reapable(&stored(p, "a", 0), &c));
    }

    #[test]
    fn is_reapable_does_not_drop_an_unresolved_notice_within_the_grace_period() {
        // THE HAZARD REGRESSION: "branch does not resolve" is ambiguous --
        // it could be a transient race, not genuine absence. A notice that
        // has only just gone unresolved must survive.
        let c = ctx(); // "campaign/gone" is in neither live_branches nor merged_branches
        let p = Post::new("notice", "campaign/gone");
        assert!(!is_reapable(&stored(p, "a", c.now_unix), &c));
    }

    #[test]
    fn is_reapable_drops_the_same_unresolved_notice_once_past_the_grace_period() {
        let mut c = ctx();
        c.now_unix = 1_000 + NOTICE_GRACE_PERIOD_S + 1;
        let p = Post::new("notice", "campaign/gone");
        assert!(is_reapable(&stored(p, "a", 1_000), &c));
    }

    #[test]
    fn is_reapable_still_drops_a_claim_past_its_ttl() {
        // No regression: an unambiguous claim expiry is reaped exactly as
        // `liveness` marks it `Expired`.
        let c = ctx();
        let p = Post::new("claim", "campaign/live")
            .with("host", json!("ambrose"))
            .with("pid", json!(42))
            .with("ttl_s", json!(60));
        assert!(is_reapable(&stored(p, "a", 100), &c), "age 900s > ttl 60s");
    }

    #[test]
    fn is_reapable_never_drops_a_durable_technique_post() {
        let c = ctx();
        let p = Post::new("technique", "campaign/gone");
        assert!(!is_reapable(&stored(p, "a", 0), &c));
    }

    #[test]
    fn is_reapable_drops_a_retracted_post_regardless_of_kind() {
        let mut c = ctx();
        c.retracted.insert("a".to_string());
        let p = Post::new("technique", "campaign/gone");
        assert!(is_reapable(&stored(p, "a", c.now_unix), &c));
    }

    #[test]
    fn pid_probe_alive_trusts_a_successful_ps_exit_status() {
        // Deterministic stand-in for "ps ran and found the process": any
        // command that exits 0 exercises the exact same `Ok(out) =>
        // out.status.success()` branch `ps` would, without depending on any
        // real pid's liveness.
        let out = std::process::Command::new("true")
            .output()
            .expect("spawn true");
        assert!(pid_probe_alive(1, Ok(out)));
    }

    #[test]
    fn pid_probe_alive_trusts_a_failing_ps_exit_status() {
        // Same idea for "ps ran and found nothing": any command that exits
        // nonzero, not a pid anyone has to believe is dead.
        let out = std::process::Command::new("false")
            .output()
            .expect("spawn false");
        assert!(!pid_probe_alive(1, Ok(out)));
    }

    #[test]
    fn pid_probe_alive_fails_open_when_ps_cannot_be_spawned_at_all() {
        // The bug this task exists to close: a spawn failure (missing
        // binary, sandboxed PATH, ...) must read as "unknown", not "dead".
        // Collapsing the two is how a live local claim silently vanishes
        // from a render.
        let err = std::io::Error::new(std::io::ErrorKind::NotFound, "no such file or directory");
        assert!(pid_probe_alive(1, Err(err)));
    }

    // --- B11: notice liveness for main and newborn branches ---

    /// `stored()` with a fixed id -- these tests never check the id itself,
    /// only `committed_at`, so a constant stand-in keeps them readable.
    fn stored_notice(post: &Post, committed_at: u64) -> StoredPost {
        stored(post.clone(), "a", committed_at)
    }

    /// One commit with no file changes -- these tests care about ancestry,
    /// not content, so an empty commit is the boring choice.
    fn commit_empty(repo: &crate::git::Repo, message: &str) {
        repo.git(&["commit", "--allow-empty", "-m", message])
            .expect("commit");
    }

    /// Duplicated locally rather than shared from `store.rs`: that helper
    /// creates its own root commit and its own branch (it owns the whole
    /// setup), while these tests need to merge a branch that ALREADY has
    /// commits of its own -- a different calling convention, not the same
    /// helper. Sharing would mean reshaping one caller to fit the other.
    fn merge_branch_into_main(repo: &crate::git::Repo, branch: &str) {
        repo.git(&["checkout", "-q", "main"])
            .expect("checkout main");
        repo.git(&["merge", "--no-ff", "-m", "merge", branch])
            .expect("merge");
    }

    #[test]
    fn a_notice_authored_by_main_is_live_because_main_is_never_superseded() {
        // main is trivially its own ancestor, so an ancestry-derived predicate
        // classifies it as merged and filters every notice main ever posts --
        // including, when this was found, one reporting main red on a heavy-tier
        // calibration. B11.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        // A real `main` always has a commit; the docstring's mechanism
        // (self-ancestry) only exists once `main` resolves to something.
        commit_empty(&repo, "root");
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
        // `main` needs its own commit first so the branch's tip can be
        // IDENTICAL to it (the docstring's scenario) rather than merely two
        // unborn refs with nothing behind either of them.
        commit_empty(&repo, "root");
        repo.git(&["checkout", "-b", "campaign/newborn"])
            .expect("branch");
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
        // `main` needs a commit of its own before it can be checked back out
        // to below -- a fresh `temp_repo()` has no commit on ANY branch yet,
        // and switching straight to a second unborn branch loses the first
        // branch's name entirely (there is no ref to come back to).
        commit_empty(&repo, "root");
        repo.git(&["checkout", "-b", "campaign/done"])
            .expect("branch");
        // one real commit, so the branch is ahead...
        commit_empty(&repo, "work");
        repo.git(&["checkout", "main"]).expect("back");
        merge_branch_into_main(&repo, "campaign/done");
        commit_empty(&repo, "main moves on"); // main now ahead of the branch
        let post = Post::new("notice", "campaign/done").with("note", json!("stale"));
        let stored = stored_notice(&post, 0);
        let ctx = LiveContext::probe(&repo, std::slice::from_ref(&stored)).expect("probe");
        assert!(
            !matches!(liveness(&stored, &ctx), Liveness::Live),
            "a merged branch's notice must still decay, or D9 is gone"
        );
    }

    // --- probe() against a real repo: I1's regression coverage ---
    //
    // Every test above drives `liveness()` against a hand-built
    // `LiveContext`; none of them touch `LiveContext::probe()` itself, which
    // is exactly how the I1 ambiguous-ref bug (a same-named tag stealing a
    // bare-name `merge-base` lookup from the branch it should have answered
    // about) shipped unnoticed. These tests exercise `probe()` against a
    // real git repository instead.

    use crate::git::test_support::temp_repo;

    /// Write and commit one file on the current branch of `repo`.
    fn commit_file(repo: &crate::git::Repo, name: &str, contents: &str) {
        std::fs::write(repo.root().join(name), contents).expect("write file");
        repo.git(&["add", name]).expect("add");
        repo.git(&["commit", "-m", &format!("commit {name}")])
            .expect("commit");
    }

    fn notice_posts(by: &str) -> Vec<StoredPost> {
        vec![stored(Post::new("notice", by), "a", 0)]
    }

    #[test]
    fn probe_treats_an_unmerged_branch_as_live() {
        let (_d, repo) = temp_repo();
        commit_file(&repo, "root.txt", "root");
        repo.git(&["checkout", "-q", "-b", "campaign/still-live"])
            .expect("branch");
        commit_file(&repo, "work.txt", "work");
        repo.git(&["checkout", "-q", "main"]).expect("back to main");

        let ctx = LiveContext::probe(&repo, &notice_posts("campaign/still-live")).expect("probe");
        assert!(
            ctx.live_branches.contains("campaign/still-live"),
            "an existing, unmerged branch must be live: {:?}",
            ctx.live_branches
        );
    }

    #[test]
    fn probe_treats_a_merged_branch_as_not_live() {
        let (_d, repo) = temp_repo();
        commit_file(&repo, "root.txt", "root");
        repo.git(&["checkout", "-q", "-b", "campaign/merged-away"])
            .expect("branch");
        commit_file(&repo, "work.txt", "work");
        repo.git(&["checkout", "-q", "main"]).expect("back to main");
        repo.git(&["merge", "--no-ff", "-m", "merge", "campaign/merged-away"])
            .expect("merge");

        let ctx = LiveContext::probe(&repo, &notice_posts("campaign/merged-away")).expect("probe");
        assert!(
            !ctx.live_branches.contains("campaign/merged-away"),
            "a merged branch must not be live: {:?}",
            ctx.live_branches
        );
    }

    #[test]
    fn probe_treats_a_nonexistent_branch_as_not_live() {
        let (_d, repo) = temp_repo();
        commit_file(&repo, "root.txt", "root");

        let ctx =
            LiveContext::probe(&repo, &notice_posts("campaign/never-existed")).expect("probe");
        assert!(
            !ctx.live_branches.contains("campaign/never-existed"),
            "a branch that was never created must not be live: {:?}",
            ctx.live_branches
        );
    }

    #[test]
    fn probe_is_not_fooled_by_a_same_named_tag_that_is_merged() {
        // I1's regression test. Sets up the exact collision the reviewer
        // reproduced: a branch that is genuinely unmerged, plus a tag of the
        // same name pointing at a commit that IS an ancestor of main. Git's
        // bare-name disambiguation order (refs/<name>, refs/tags/<name>,
        // refs/heads/<name>) means a second, independent bare-name lookup
        // would answer about the TAG, not the branch -- silently expiring a
        // live notice. `probe()` must resolve the branch ref once and reuse
        // that resolved ref, never asking a second ambiguous question.
        let (_d, repo) = temp_repo();
        commit_file(&repo, "root.txt", "root");
        let root_commit = repo.git(&["rev-parse", "HEAD"]).expect("root sha");

        repo.git(&["checkout", "-q", "-b", "campaign/collide"])
            .expect("branch");
        commit_file(&repo, "work.txt", "work"); // diverges from main; NOT merged
        repo.git(&["checkout", "-q", "main"]).expect("back to main");

        // A tag with the SAME name as the branch, pointing at a commit that
        // trivially IS an ancestor of main (main's own root commit).
        repo.git(&["tag", "campaign/collide", &root_commit])
            .expect("tag");

        let ctx = LiveContext::probe(&repo, &notice_posts("campaign/collide")).expect("probe");
        assert!(
            ctx.live_branches.contains("campaign/collide"),
            "the genuinely unmerged branch must render as live despite the \
             same-named merged tag: {:?}",
            ctx.live_branches
        );
    }
}
