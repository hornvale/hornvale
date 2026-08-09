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
        for s in posts.iter() {
            let by = s.post.by.clone();
            if live_branches.contains(&by) {
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
            let merged = repo
                .git(&["merge-base", "--is-ancestor", &resolved, "refs/heads/main"])
                .is_ok();
            if !merged {
                live_branches.insert(by);
            }
        }

        Ok(Self {
            now_unix,
            host,
            retracted,
            live_pids,
            live_branches,
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
