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
    /// Branches that exist and are judged NOT merged into `main` — either
    /// because a live worktree says so directly, or because they are
    /// unresolved-into-merged by ahead/behind counts (which also covers a
    /// branch that is merely IDENTICAL to `main`'s tip: `main` compared
    /// with itself, or a campaign branch with no commits of its own yet —
    /// neither has diverged, so neither is "merged" in any meaningful
    /// sense; see `merged_branches`' doc and `probe`'s B11 comment). Not
    /// literally "not merged" in the git sense — a genuinely merged branch
    /// with a live worktree still checked out on it lands here too, by
    /// design (D9's stated intent is a torn-down worktree, not a merge).
    pub live_branches: BTreeSet<String>,
    /// Branches that RESOLVE and ARE merged into `main` — an unambiguous,
    /// positive "definitely dead" signal that `is_reapable` treats
    /// differently from a branch that simply does not resolve at all (see
    /// `is_reapable`'s doc comment for why the distinction matters). "Merged"
    /// here means genuinely absorbed and superseded (no commits of its own
    /// left outstanding, AND `main` has since moved past it) — not merely
    /// identical to `main`'s tip, and not a branch with a live worktree
    /// still checked out on it (that check runs first; see `probe`). `main`
    /// compared with itself, and a campaign branch that has not yet made
    /// its first commit, are both identical-to-`main` rather than merged,
    /// and belong in `live_branches` (B11).
    pub merged_branches: BTreeSet<String>,
}

/// This host's short name, as `hostname -s` reports it, or an empty string if
/// `hostname` cannot be run at all.
///
/// One function rather than a shellout per call site, because this value
/// decides two different things that must agree: which claims are judged
/// against this machine's process table (D8), and which peer mirror is *this*
/// host's own and so must be skipped by a union read
/// ([`Board::read_refs`](crate::store::Board::read_refs)). Two copies of it
/// could disagree only in the confusing direction — a hostname collision is
/// already the subtlest failure this design has, and it does not need a
/// second, local source of disagreement on top.
///
/// Fails to the empty string rather than erroring, matching the rest of this
/// crate's fail-open convention: an unknown host makes every claim foreign
/// (rendered, not silently dropped) and makes no peer ref look like our own.
pub fn current_host() -> String {
    std::process::Command::new("hostname")
        .arg("-s")
        .output()
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
        .unwrap_or_default()
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

/// Pull the checked-out branch names out of `git worktree list --porcelain`
/// output. Pure and total: never panics or errors on any input, including
/// empty, truncated, or entirely unrecognised text — a line that is not a
/// `branch refs/heads/<name>` line (a detached-HEAD worktree, a bare repo
/// entry, garbage) is simply not a match, not a parse failure. This is what
/// makes it safe to feed it whatever a failed `git` call leaves behind.
fn parse_worktree_branches(out: &str) -> BTreeSet<String> {
    let mut branches = BTreeSet::new();
    for line in out.lines() {
        if let Some(rest) = line.strip_prefix("branch ")
            && let Some(name) = rest.strip_prefix("refs/heads/")
        {
            branches.insert(name.to_string());
        }
    }
    branches
}

/// Branches currently checked out in ANY worktree of this repository,
/// including the primary checkout — a direct, unambiguous "this campaign is
/// still active" signal that does not depend on ancestry at all.
///
/// This is B11's fix for the half ahead/behind counts cannot cover: once
/// `main` advances past a branch that has made no commits of its own, that
/// branch reads as `ahead == 0, behind > 0` — exactly the signature of a
/// genuinely merged branch (see `probe`'s B11 comment). Ahead/behind alone
/// therefore only protects a newborn branch for as long as `main` stands
/// still; a worktree existing protects it unconditionally, for as long as
/// the campaign is actually checked out somewhere. This also matches D9's
/// STATED intent better than ancestry ever did: D9 names the notice that
/// should decay as one whose authoring worktree was later torn down, not
/// one that merely looks absorbed by a merge-base check — so a genuinely
/// merged branch whose worktree is still checked out (an active campaign
/// revisiting old work) is correctly still live, not a cost.
///
/// Callers must NOT propagate this `Result` with `?`: unlike
/// `resolve_branch_ref`, which only runs when a specific post needs it, this
/// runs once per `probe()` call, unconditionally. A `git worktree list`
/// failure here must fall through to the ahead/behind check, not abort the
/// whole render for every post on the board -- `probe`'s caller does exactly
/// that on any propagated error (see `main.rs`), which is the one failure
/// mode this board exists to avoid (D7/D14: a board nobody can read is
/// worthless).
fn live_worktree_branches(repo: &Repo) -> Result<BTreeSet<String>, BoardError> {
    let out = repo.git(&["worktree", "list", "--porcelain"])?;
    Ok(parse_worktree_branches(&out))
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

        let host = current_host();

        let retracted = posts
            .iter()
            .filter(|s| s.post.kind == "retract")
            .filter_map(|s| s.post.str_field("post").map(str::to_string))
            .collect();

        let mut live_pids = BTreeSet::new();
        for s in posts.iter().filter(|s| s.post.kind == "claim") {
            // B4, symmetric with the branch-resolution skip below: a foreign
            // claim's `host` FIELD is self-reported and could collide with
            // this host's OWN short name (duplicate short hostnames are not
            // hypothetical in this repo -- see CLAUDE.md's `MacBookPro` vs
            // `ambrose` timing-baseline fork). Without this, such a
            // collision would spawn `ps` for a pid this host never claimed,
            // and could admit that pid into `live_pids`, where it might then
            // coincidentally match a genuinely dead LOCAL claim's pid and
            // read it as live. `Origin` cannot collide the way a self-
            // reported field can, so it is checked first.
            if matches!(s.origin, crate::store::Origin::Peer(_)) {
                continue;
            }
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
        // One `git worktree list` for the whole probe, not per-post: see
        // `live_worktree_branches`'s doc for why this check runs FIRST,
        // ahead of the ahead/behind fallback below.
        //
        // Fails OPEN, not propagated: this runs unconditionally on every
        // probe, unlike `resolve_branch_ref` below (which only runs per
        // post). A `git worktree list` failure must not abort the whole
        // render for every post on the board -- an empty set here just means
        // every branch falls through to the ahead/behind fallback, exactly
        // as it did before this discriminator existed.
        let worktree_branches = live_worktree_branches(repo).unwrap_or_else(|e| {
            eprintln!(
                "board: could not enumerate worktrees ({e}); no branch will be treated as \
                 having a live worktree for this probe -- falling through to ahead/behind counts"
            );
            BTreeSet::new()
        });
        for s in posts.iter() {
            // B4: `liveness` never consults `live_branches`/`merged_branches`
            // for a foreign post (see its doc comment), so resolving one
            // here would be two `rev_parse`/`rev-list` calls per foreign
            // post that nothing ever reads -- a cost that grows with the
            // peer population for no benefit. Skipped entirely, not merely
            // deprioritised.
            if matches!(s.origin, crate::store::Origin::Peer(_)) {
                continue;
            }
            let by = s.post.by.clone();
            if live_branches.contains(&by) || merged_branches.contains(&by) {
                continue;
            }
            // B11 (the discriminator that actually closes the hazard): a
            // branch with a live worktree is live no matter what ahead/behind
            // says below. See `live_worktree_branches`'s doc for why this
            // must run first rather than as a tiebreak.
            if worktree_branches.contains(&by) {
                live_branches.insert(by);
                continue;
            }
            // Resolve once; reuse the resolved ref for the ahead/behind check
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
            // B11 (fallback, for a branch with no worktree currently checked
            // out): a plain ancestor check (`merge-base --is-ancestor
            // resolved main`) is trivially true whenever `resolved` and
            // `main` are the SAME commit, not just when `resolved` is a
            // strict ancestor -- and that is exactly `by == "main"` comparing
            // itself, or a freshly-branched campaign that has not yet made a
            // commit of its own. Both were being classified `merged` and
            // filtered from every render (and were one `reap` away from
            // permanent deletion). Ahead/behind counts distinguish the two:
            // "merged" means the branch's own commits are fully absorbed
            // (ahead == 0) AND main has since moved past it (behind > 0); a
            // branch that is merely IDENTICAL to main (ahead == 0, behind ==
            // 0) has not diverged at all and is not "merged" in any
            // meaningful sense. Note this fallback is INCOMPLETE on its own:
            // once main advances past an as-yet-uncommitted branch, it reads
            // ahead == 0, behind > 0 -- indistinguishable from a genuinely
            // merged branch by counts alone. The worktree check above is what
            // actually closes that gap; this is the second opinion for a
            // branch with no worktree live right now. On any failure to
            // compute the counts (a `refs/heads/main` that itself does not
            // resolve, say), this errs toward LIVE, not merged -- an
            // uncertain classification must render, never silently vanish
            // (the exact failure mode this predicate exists to close).
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
///
/// **B4/B5 — a foreign post is judged by TTL alone.** [`Origin::Peer`] short-
/// circuits both decaying-kind arms below, *after* the TTL check (which is
/// physical and needs no local authority) but *before* either liveness
/// predicate that only the authoring host can decide: process liveness
/// (`pid` against this host's OWN `ps` table) and branch liveness (`by`
/// resolving in THIS clone at all, plus the worktree check `probe` adds).
/// This generalises a rule this function already had for one of those two —
/// `a_claim_from_another_host_is_not_judged_by_our_process_table` skips the
/// pid check when the claim's `host` FIELD differs from `ctx.host` — from
/// `pid` to branches, and from a self-reported, optional convention to
/// [`Origin`], which cannot be omitted, forged, or mistyped by a posting
/// session (see `Origin`'s own doc comment).
///
/// Without this, importing a peer's log would judge every one of its
/// notices by ancestry in a clone that never had the branch to begin with —
/// unresolved forever, not merely today — and this function's own
/// `"notice"` arm would then render every one of them `Expired`: a live
/// `hold-off` from a peer would simply stop appearing in every render on
/// this host, silently, for as long as the branch stays unresolved here
/// (which, for a peer's branch, is forever). **This is a render-suppression
/// bug, not a reap one**: `is_reapable`'s `"notice"` arm does not call this
/// function at all (it reads `live_branches`/`merged_branches`/the grace
/// period directly), and `reap` never sees a foreign post regardless,
/// because `Board::snapshot` is single-ref by construction (B1) — so a
/// peer's post can never be reaped from here no matter what this function
/// returns. The fix below is still correct and needed; it just closes a
/// different door than reap's. It also makes a foreign post CHEAPER to
/// judge than a local one: see `LiveContext::probe`'s matching skip, which
/// is what stops the unresolved-author cost from growing with the peer
/// population.
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
            // B4: past this point only this host's own process table could
            // say more, and a foreign claim's pid means nothing to it --
            // TTL alone is the whole verdict (B5: rendered as unverifiable,
            // never as equivalent to a local claim; see render.rs).
            if matches!(stored.origin, crate::store::Origin::Peer(_)) {
                return Liveness::Live;
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
            // B4: this clone may never have held `by` at all (a peer's
            // campaign branch), so "does not resolve" cannot mean "dead" for
            // a notice this host did not author -- see the function doc.
            if matches!(stored.origin, crate::store::Origin::Peer(_)) {
                return Liveness::Live;
            }
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
///   unambiguous: no live worktree is checked out on it (see
///   `live_worktree_branches`), AND its ahead/behind counts against `main`
///   read as fully absorbed (`ahead == 0, behind > 0`) — a positive
///   statement that no transient race can produce. Reaped immediately,
///   regardless of age.
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
            origin: crate::store::Origin::Local,
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

    // --- B4/B5: a foreign post is judged by TTL alone ---

    /// A [`Liveness::Live`] `notice` authored on a branch that has NEVER
    /// existed in this clone -- `ctx()` on its own would resolve this the
    /// same way a real union read resolves one of lefford's campaign
    /// branches: never in `live_branches`, never in `merged_branches`.
    /// Read as [`Origin::Peer`], the way a union read actually tags it.
    fn peer_notice(by: &str, host: &str, note: &str) -> StoredPost {
        StoredPost {
            id: "peer-notice".to_string(),
            post: Post::new("notice", by).with("note", json!(note)),
            committed_at: 1_000,
            origin: crate::store::Origin::Peer(host.to_string()),
        }
    }

    /// A `claim` read as [`Origin::Peer`], `age_s` seconds after
    /// `committed_at` relative to `ctx()`'s fixed `now_unix` (1,000).
    fn peer_claim(host: &str, pid: u32, ttl_s: u64, age_s: u64) -> StoredPost {
        StoredPost {
            id: "peer-claim".to_string(),
            post: Post::new("claim", "campaign/live")
                .with("host", json!(host))
                .with("pid", json!(pid))
                .with("ttl_s", json!(ttl_s)),
            committed_at: 1_000 - age_s,
            origin: crate::store::Origin::Peer(host.to_string()),
        }
    }

    /// The [`Origin::Local`] control: a claim this host DID author, on ITS
    /// own host name (`ctx()`'s `"ambrose"`), so the pre-existing pid check
    /// still applies in full.
    fn local_claim(pid: u32, ttl_s: u64, age_s: u64) -> StoredPost {
        StoredPost {
            id: "local-claim".to_string(),
            post: Post::new("claim", "campaign/live")
                .with("host", json!("ambrose"))
                .with("pid", json!(pid))
                .with("ttl_s", json!(ttl_s)),
            committed_at: 1_000 - age_s,
            origin: crate::store::Origin::Local,
        }
    }

    /// `ctx()` plus whatever `posts` themselves imply about retraction --
    /// the same derivation `LiveContext::probe` does, kept a hand-built
    /// value here (as `ctx()` already is) so these tests need no git.
    fn ctx_for(posts: &[StoredPost]) -> LiveContext {
        let mut c = ctx();
        c.retracted = posts
            .iter()
            .filter(|s| s.post.kind == "retract")
            .filter_map(|s| s.post.str_field("post").map(str::to_string))
            .collect();
        c
    }

    #[test]
    fn a_foreign_notice_renders_even_though_its_branch_does_not_resolve_here() {
        // The silent-suppression guard. lefford's campaign branches do not
        // exist in this clone, so an ancestry-derived predicate judges every
        // one of its notices dead -- and a reap would then drop a LIVE
        // hold-off out of the local view. B4.
        let stored = peer_notice("campaign/only-on-lefford", "lefford", "do not pin heights");
        let ctx = ctx_for(std::slice::from_ref(&stored));
        assert!(matches!(liveness(&stored, &ctx), Liveness::Live));
    }

    #[test]
    fn a_foreign_claim_inside_its_ttl_is_live_but_never_locally_verified() {
        // B5: this host cannot check another host's process table, and must
        // not present a claim it cannot check as one it can.
        let stored = peer_claim(
            "lefford", /* pid */ 999_999, /* ttl_s */ 900, /* age_s */ 10,
        );
        let ctx = ctx_for(std::slice::from_ref(&stored));
        assert!(matches!(liveness(&stored, &ctx), Liveness::Live));
        let text = crate::render::render(&[stored], 0, &crate::render::RenderOptions::full());
        assert!(
            text.contains("unverifiable"),
            "a foreign claim must say this host cannot check it; got {text}"
        );
    }

    #[test]
    fn a_foreign_claim_past_its_ttl_expires() {
        let stored = peer_claim("lefford", 999_999, 900, /* age_s */ 1_000);
        let ctx = ctx_for(std::slice::from_ref(&stored));
        assert!(!matches!(liveness(&stored, &ctx), Liveness::Live));
    }

    #[test]
    fn a_local_claim_is_still_judged_against_this_hosts_process_table() {
        // The arm that keeps the fix from being a removal: local pid
        // checking must survive. A dead pid inside its TTL still expires.
        let stored = local_claim(/* pid */ 999_999, 900, 10);
        let ctx = ctx_for(std::slice::from_ref(&stored));
        assert!(!matches!(liveness(&stored, &ctx), Liveness::Live));
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

    #[test]
    fn a_notice_from_a_diverged_unmerged_branch_is_live() {
        // MUTATION COVERAGE (Important #1): `ahead == 0 && behind > 0` --
        // dropping the `ahead == 0` half (mutating to `Some(behind > 0)`)
        // still classifies THIS branch as merged, because a diverged-but-
        // unmerged branch and a genuinely merged one both have
        // `behind > 0`. Diverged-and-unmerged (ahead >= 1, behind >= 1) is
        // the ORDINARY resting state of any active campaign that has not
        // absorbed main recently -- most branches, most of the time -- so a
        // mutant that drops `ahead == 0` reinstates B11 for the majority
        // case. This is the arm that kills it (verified in the fix report).
        let (_dir, repo) = crate::git::test_support::temp_repo();
        commit_empty(&repo, "root");
        repo.git(&["checkout", "-b", "campaign/diverged"])
            .expect("branch");
        commit_empty(&repo, "branch work"); // campaign/diverged is ahead of main
        repo.git(&["checkout", "main"]).expect("back to main");
        commit_empty(&repo, "main work"); // main is ALSO ahead of campaign/diverged
        // Current checkout is `main`, not `campaign/diverged` -- deliberately,
        // so the live-worktree check cannot be what rescues this notice; only
        // ahead/behind can, which is what this arm exists to pin.
        let post = Post::new("notice", "campaign/diverged").with("note", json!("still going"));
        let stored = stored_notice(&post, 0);
        let ctx = LiveContext::probe(&repo, std::slice::from_ref(&stored)).expect("probe");
        assert!(
            matches!(liveness(&stored, &ctx), Liveness::Live),
            "ahead >= 1 must stay live regardless of behind"
        );
    }

    #[test]
    fn a_notice_from_a_squash_merged_branch_stays_live_by_design() {
        // A safe error, not an accident (Important #1's second, cheap arm):
        // a squash-merged branch's own commit never becomes an ancestor of
        // `main` (the squash lands as a NEW commit with unrelated history),
        // so it reads `ahead >= 1` forever and this predicate cannot tell it
        // apart from an ordinary active branch. Pinned as documented
        // behaviour: the notice lingers past the point the campaign is truly
        // done, which is a cost (one extra render), never the silent-
        // deletion hazard B11 exists to close.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        commit_empty(&repo, "root");
        repo.git(&["checkout", "-b", "campaign/squashed"])
            .expect("branch");
        commit_empty(&repo, "branch work");
        repo.git(&["checkout", "main"]).expect("back to main");
        // `--squash` stages the branch's diff without recording it as a
        // merge parent, so the resulting commit shares no ancestry with
        // `campaign/squashed`'s tip -- confirmed empirically: `rev-list
        // --left-right --count campaign/squashed...main` reads `1  1` after
        // this, not `0  1`.
        repo.git(&["merge", "--squash", "campaign/squashed"])
            .expect("squash stage");
        commit_empty(&repo, "squashed in"); // the squash commit itself
        let post = Post::new("notice", "campaign/squashed").with("note", json!("done, allegedly"));
        let stored = stored_notice(&post, 0);
        let ctx = LiveContext::probe(&repo, std::slice::from_ref(&stored)).expect("probe");
        assert!(
            matches!(liveness(&stored, &ctx), Liveness::Live),
            "a squash-merged branch's own commit is never an ancestor of main, \
             so it cannot be told apart from an active branch by this predicate"
        );
    }

    #[test]
    fn a_notice_from_a_newborn_branch_with_a_live_worktree_stays_live_after_main_advances() {
        // Important #2 regression: ahead/behind ALONE cannot tell a newborn
        // branch from a merged one once `main` moves -- reproduces the exact
        // signature the reviewer measured (`live_branches={}
        // merged_branches={"campaign/newborn"}`, `liveness=Expired(...)`,
        // `is_reapable=true`) and confirms the live-worktree check protects
        // it where ahead/behind cannot.
        let (dir, repo) = crate::git::test_support::temp_repo();
        commit_empty(&repo, "root");
        let worktree_path = dir.with_file_name(format!(
            "{}-newborn-wt",
            dir.file_name().expect("dir name").to_string_lossy()
        ));
        let _ = std::fs::remove_dir_all(&worktree_path);
        repo.git(&[
            "worktree",
            "add",
            "-q",
            "-b",
            "campaign/newborn",
            worktree_path.to_str().expect("utf8 path"),
        ])
        .expect("worktree add");
        // main advances while campaign/newborn STILL has no commit of its
        // own -- this is the window the reviewer found: ahead == 0,
        // behind > 0, the same signature a genuinely merged branch has.
        commit_empty(&repo, "main advances");
        let post = Post::new("notice", "campaign/newborn").with("note", json!("starting"));
        let stored = stored_notice(&post, 0);
        let ctx = LiveContext::probe(&repo, std::slice::from_ref(&stored)).expect("probe");
        assert!(
            ctx.live_branches.contains("campaign/newborn"),
            "a live worktree must protect a newborn branch regardless of main's \
             progress: {:?}",
            ctx.live_branches
        );
        assert!(
            matches!(liveness(&stored, &ctx), Liveness::Live),
            "and the notice must render"
        );
    }

    // --- B11 (round 2): `live_worktree_branches` must fail open ---
    //
    // Injecting a genuinely failing `git worktree list --porcelain` into
    // this crate's test harness would need either mutating process-global
    // `PATH` (racy against every other test's own `git` calls, which run
    // concurrently in the same test binary) or a git-runner injection seam
    // `Repo` does not have and this fix should not add just for one test.
    // So the smallest unit actually testable here is the parse step itself:
    // it must never panic and must degrade to an empty set on anything that
    // is not real `branch refs/heads/<name>` output, including the empty
    // string a failed call's absent stdout would leave behind. This does
    // NOT cover the `git` invocation failing outright, or the
    // `unwrap_or_else` fallback wired to it in `probe`; both were instead
    // checked manually (predicate-mutation style, matching Important #1's
    // verification) and are NOT re-checked by any test that remains in the
    // tree -- see the fix report for that transcript.

    #[test]
    fn parse_worktree_branches_of_empty_output_is_an_empty_set() {
        // The shape a failed call's absent stdout would leave behind, if it
        // were ever (wrongly) fed to the parser instead of short-circuited.
        assert!(parse_worktree_branches("").is_empty());
    }

    #[test]
    fn parse_worktree_branches_ignores_error_shaped_text_without_panicking() {
        // Not real porcelain output at all -- stderr text, a truncated
        // line, garbage. None of it matches `branch refs/heads/<name>`, so
        // none of it should produce a branch, and none of it should panic.
        let garbage = "fatal: not a git repository (or any of the parent directories): .git\n\
                        branch\n\
                        branch refs/tags/not-a-branch\n\
                        brnach refs/heads/typo";
        assert!(parse_worktree_branches(garbage).is_empty());
    }

    #[test]
    fn parse_worktree_branches_reads_real_porcelain_output() {
        // The shape `git worktree list --porcelain` actually produces:
        // multiple worktree blocks, one of them detached (no `branch` line
        // at all), separated by blank lines.
        let out = "worktree /repo\n\
                    HEAD abc123\n\
                    branch refs/heads/main\n\
                    \n\
                    worktree /repo-wt/campaign-x\n\
                    HEAD def456\n\
                    branch refs/heads/campaign/x\n\
                    \n\
                    worktree /repo-wt/detached\n\
                    HEAD 789abc\n\
                    detached\n";
        let branches = parse_worktree_branches(out);
        assert_eq!(
            branches,
            BTreeSet::from(["main".to_string(), "campaign/x".to_string()]),
            "must collect both real branches and skip the detached worktree: {branches:?}"
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
    fn probe_never_resolves_a_foreign_authors_branch_at_all() {
        // Minor 1 from Task 6's review: removing the `Origin::Peer` skip in
        // `probe`'s branch-resolution loop restores two `rev-parse` calls
        // per foreign post and NOTHING FAILS for a branch that never
        // existed here (`resolve_branch_ref` returns `None` either way, so
        // the resulting sets are identical) -- that measurement lived only
        // in a throwaway example, not a test. `main` is different: it
        // ALWAYS resolves, and B11 makes it unconditionally live (self-
        // ancestry), so a post naming it is a case where "resolved" and
        // "skipped" produce OBSERVABLY different `live_branches`. A local
        // post naming `main` lands there (see
        // `a_notice_authored_by_main_is_live_because_main_is_never_superseded`);
        // this pins that a FOREIGN one does not, because it is never asked.
        let (_d, repo) = temp_repo();
        commit_file(&repo, "root.txt", "root");
        let foreign = StoredPost {
            id: "a".repeat(40),
            post: Post::new("notice", "main"),
            committed_at: 0,
            origin: crate::store::Origin::Peer("lefford".to_string()),
        };
        let ctx = LiveContext::probe(&repo, &[foreign]).expect("probe");
        assert!(
            !ctx.live_branches.contains("main"),
            "a foreign post's `by` must never be resolved, not even into a \
             TRUE answer: {:?}",
            ctx.live_branches
        );
        assert!(
            !ctx.merged_branches.contains("main"),
            "nor into a false one: {:?}",
            ctx.merged_branches
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
