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
                let alive = std::process::Command::new("ps")
                    .args(["-p", &pid.to_string()])
                    .output()
                    .map(|o| o.status.success())
                    .unwrap_or(false);
                if alive {
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
            let exists = repo.rev_parse_verify(&by)?.is_some()
                || repo
                    .rev_parse_verify(&format!("refs/heads/{by}"))?
                    .is_some();
            if !exists {
                continue;
            }
            let merged = repo
                .git(&["merge-base", "--is-ancestor", &by, "main"])
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
}
