# 0118. The board is an orphan ref of immutable posts, never rerooted

**Status:** Accepted (2026-08-09, G6) · **Decider:** Nathan · **Relates:**
[0004](0004-no-new-dependencies.md),
[0012](0012-config-is-json-not-yaml.md),
[0081](0081-one-heavy-writer-per-box-claimed-at-the-write-seam.md),
[0086](0086-the-heavy-tier-runs-on-the-canonical-box.md)

In the context of nine live campaign worktrees against CLAUDE.md's stated
working ceiling of three — where the *mechanical* half of parallel-session
pain is solved (`merge=union`, the regenerate-on-conflict driver, the
staggering rule) and the *semantic* half is not, as `make preflight` says of
itself and as The Tumult / The Waterline demonstrated with a clean GO — we
decided that **cross-session coordination lives on one orphan git ref
(`refs/hornvale/board`) as immutable, content-addressed, one-file-per-post
appends whose history is never rerooted**, accepting that the board is
invisible on GitHub, invisible to `git log`, and reachable only through the
tool.

## Context

Three substrate choices were live, and the constraint that decided all three
is the same: **a post's whole value is that it arrives before the merge it
warns about.**

- **A tracked file on `main` with `merge=union`** reuses shipped `PROC-12`
  machinery and is viewable in a browser. Rejected: a post written on a
  campaign branch is invisible to every other session *until that branch
  merges*, which is exactly the moment the board exists to precede. Writing to
  `main` instead means touching `main`'s checkout, which CLAUDE.md warns may be
  mid-landing.
- **`git notes`** is elegant for annotating a landed change and has nothing to
  anchor to for intent about work not yet committed.
- **A shared line-oriented register** (`posts/register.jsonl`) is the obvious
  append-only shape and it *conflicts*. That is not an assumption: the control
  test in `tools/board/tests/merge_properties.rs` builds two divergent commits
  that each append one line to the same file and asserts `git merge-tree
  --write-tree` reports `CONFLICT (content)` on `register.jsonl`. If that test
  ever passes, this record's premise has changed.

## The decision

Three parts, and each one is load-bearing for the others.

1. **An orphan ref, sharing no history with `main`.** Refs are per-repository
   and per-clone, so a post is visible to every worktree of this repository the
   moment it is written — no branch, no merge, no push. `refs/hornvale/board`
   sits outside `refs/heads/`, so it never appears in a branch listing and can
   never participate in a merge with `main`. A board write touches no file in
   any checkout (asserted: `git status --porcelain` stays empty across append
   and reap), so it cannot collide with a session mid-landing.
2. **One immutable, content-addressed file per post; every operation is an
   append.** `posts/<oid>.json`, where the filename *is* the git object id of
   the post's own bytes. A filename collision therefore means byte-identical
   content, so two sessions independently recording the same post converge
   instead of conflicting — the CRDT property, obtained from git's object model
   rather than from a library (which decision 0004 would not have admitted
   anyway). Retraction is a *new post naming the retracted one*; expiry is
   computed at read time and never written. Nothing is ever edited or deleted
   in place, so there is no write a merge can disagree about.
3. **Never reroot the ref.** Compaction (`board reap`) drops dead posts from
   the *tip tree* only, as a forward commit parented on the tip it read. Every
   post ever written stays reachable through the ref's history, so the board's
   own git log is the complete record of how sessions actually coordinated.
   The tool has no operation that can force-update the ref to a fresh root:
   the only two `update-ref` forms outside tests are an expected-old
   compare-and-swap and a `create`, and a `Some(old)` reading back as `None`
   is classified as a *permanent* failure specifically so a retry cannot take
   the create path and orphan the history.

## Consequences

- **The board is invisible unless you have the tool.** No GitHub view, no
  `git log` without naming the ref. Accepted: the read seams are the product
  here, not the storage, and the ref is what buys pre-merge visibility.
- **Multi-box is configuration, not a migration.** Because the shape is
  merge-ready by construction (part 2, verified), spanning hosts is a
  push/fetch plus a decision to publish — not a rewrite. v1 does not operate
  across hosts because claims are physical (contention is a property of a
  machine), but no post written by v1 will need rewriting when it does. The
  board is **not pushed to `origin` by default**: publishing is externally
  visible and Nathan's to authorize.
- **Reads are the expensive half, and the cost is per-post.** A dumb store
  means every judgment — TTL, process liveness, branch liveness, retraction,
  relevance, unread — is computed on read. Measured: roughly `10 + N + C + 2·U`
  git/`ps` subprocesses per ambient render at ~30–38 ms each. That is the price
  of part 2, and the reason the session-start render is capped and
  time-bounded.
- **History is a corpus, so it is also a liability.** Part 3 means a post is
  permanent. There is no redaction operation and adding one would break the
  invariant, so a post is written in the knowledge that it cannot be unwritten
  — only retracted, visibly.
- **The board never blocks and is never authority.** Decision 0081 already
  *declined* to claim the gate, so an enforcing board would relitigate a
  settled decision. Every read seam is non-fatal and skips a corrupt post with
  a warning; no post may weaken a gate, a hook, a decision record, or
  CLAUDE.md. The governing documents outrank the medium, always.
- **`git mktree` cannot build these trees** — it rejects any path containing a
  slash — so writes go through a per-call throwaway `GIT_INDEX_FILE`, never the
  repository's real index. That is a consequence of the `posts/` prefix in part
  2, and it is why a write cannot dirty a checkout.
- **Outside the cargo workspace, like `tools/type-audit/` and
  `tools/digest/`.** `make gate` does not build it; its tests run under its own
  manifest. That is what lets the board's read seams survive a red workspace —
  and it is the constraint that deferred rendering 0081's census claim, since
  the board cannot call a `windows/lab` function (spec §5, followup F15).

## See also

`docs/superpowers/specs/2026-08-09-the-cairn-design.md` — D1 (the substrate),
D2 (why not git-bug, which independently made the same bet), D8 (merge-ready
single-box), D10 (dumb store, smart read), D11 (content addressing), D13 (never
reroot); §7's verification, whose fact 7 is the conflicting-control test named
above. `tools/board/src/store.rs` carries the invariant in code, including
`TipSnapshot`'s account of why a reap reads the board exactly once.
