# The Beacon — Design

**Campaign:** The Beacon
**Branch:** `campaign/the-beacon`
**Date:** 2026-08-11
**Status:** spec, awaiting G3

A cairn marks a place. A beacon relays between summits — the same stones,
stacked on a second hill, so that a signal lit on one is seen from the other.
This campaign takes The Cairn (decision
[0118](../../decisions/0118-the-board-is-an-orphan-ref-of-immutable-posts-never-rerooted.md))
cross-machine.

Decision references of the form **D*n*** are The Cairn's, from
[`2026-08-09-the-cairn-design.md`](2026-08-09-the-cairn-design.md). This spec's
own decisions are numbered **B*n*** to keep the two apart.

## 1. The problem

0118 shipped a board that is *merge-ready by construction* and *not
operationally cross-host*:

> **Multi-box is configuration, not a migration.** Because the shape is
> merge-ready by construction (part 2, verified), spanning hosts is a
> push/fetch plus a decision to publish — not a rewrite. v1 does not operate
> across hosts because claims are physical (contention is a property of a
> machine), but no post written by v1 will need rewriting when it does.

Two things have since changed, and both are the campaign's trigger.

**The topology got a second inhabited box.** Decision
[0086](../../decisions/0086-the-heavy-tier-runs-on-the-canonical-box.md) (The
Siding) put the heavy tier and censuses on `lefford` and left campaign work and
the commit gate on the Mac. Sessions run on both, and the goldens are authored
and *committed* on lefford. So there are now two populations of session that
cannot see each other's intent at all: the board on `ambrose` is invisible to
lefford, and lefford has no board (§2 fact 1).

**The publication gate opened.** 0118 withheld cross-host operation behind
Nathan's authorization because publishing to a public repository is
irreversible. The repository became **private** on 2026-08-11, and Nathan
authorized GitHub as the transport in this campaign's G1.

Decision **0125** (GitHub Actions is retired) records the same visibility change
from the CI side and is *in flight rather than landed*: at the time this spec was
written its record existed only as staged, uncommitted work in `main`'s checkout,
so it is deliberately cited without a link and gets one at the first absorption.
Two of its consequences bear on this campaign. It **removes CI from the argument
for `origin`** — D12c's "reach across accounts and to CI" loses its second half,
leaving cross-account reach as the whole case. And it makes the local gate the
only gate, which raises rather than lowers the value of a medium that carries
intent between sessions, since nothing else will notice a collision on their
behalf.

There is a third trigger that is not about topology at all, and it decides the
campaign's shape more than either of the above: **the primary read seam is
already at its latency ceiling on one host** (§2 fact 5). Cross-host reads
roughly double the post count. Shipping the feature onto an unfixed read cost
would silence the seam the feature exists to serve, which is the failure D14
was written to prevent, reached by arithmetic rather than by neglect.

## 2. What is already true, measured

Established by direct probe on 2026-08-11 from `ambrose`, `main` at `8c61cbc5`.
Commands and full output in §7.

1. **lefford has no board and cannot be reached from itself.**
   `git for-each-ref refs/hornvale/` on lefford is **empty**, and there is no
   `tools/board/target/*/board` binary there. Both boxes have exactly one
   remote, `origin`, over HTTPS.
2. **GitHub accepts a custom ref namespace, from both boxes, in both
   directions.** A push to `refs/hv-board-probe/ambrose` succeeded; lefford
   fetched it and pushed its own probe back, with no configuration change and
   with credentials already present on both boxes. This is the single fact the
   whole transport rests on, and it is exactly the kind of thing a host may
   reject.
3. **lefford cannot reach `ambrose` directly.**
   `ssh lefford 'ssh ambrose hostname -s'` →
   `Could not resolve hostname ambrose`. Under a direct-ssh transport this
   would have forced an asymmetric, one-initiator design. Via `origin` it is
   irrelevant — which is the main design simplification the transport choice
   bought (B2).
4. **The board is opt-in and cannot be published by accident.**
   `remote.origin.fetch` is `+refs/heads/*:refs/remotes/origin/*`, and
   `git push --all --dry-run` proposes branches only. Nothing under
   `refs/hornvale/` travels unless a command names it.
5. **The ambient render is at its ceiling.** `scripts/board-render.sh`, three
   runs, 26 posts at the tip, one host: **1.43 s / 1.86 s / 1.52 s** against a
   **2 s** budget. This is the 15–40-post cliff D-assumption 1 predicted,
   arriving on schedule.
6. **The dominant cost is the per-post object read, not the unresolved-author
   probe.** Of ~36 subprocesses, ~26 are one `git cat-file -p` per post. Author
   tally at the tip: `campaign/the-grain` and `main` resolve,
   `campaign/the-cairn` does not — so **U = 2** while **N = 26**. Twenty-six
   individual `cat-file` calls take **0.850 s**; the same ids through one
   `git cat-file --batch` take **0.041 s**.
7. **Force-pushing does not redact on GitHub.** A commit containing a canary
   string, force-pushed out of a probe ref's history, remained retrievable by
   oid through the API — commit *and* blob, full plaintext (§7).
8. **A notice from a branch that has not committed yet is invisible.** Posting
   from a freshly created campaign branch returned a hash and stored correct
   bytes, and rendered **not at all**: notice liveness requires the `by` branch
   to be *unmerged*, tested by ancestry, and a fresh branch's tip **equals**
   main. Found by dogfooding this campaign's own opening notice.

## 3. Decisions

**B1 — Per-host refs with a read-time union; no merge commit.** Each host keeps
writing only its own `refs/hornvale/board`. Peers are fetched into
`refs/hornvale/peers/<host>`, and every read seam unions the tips, deduplicated
by post id — which is free, because an id *is* the content hash, so the same
post mirrored twice is the same string twice.

This is **D10 (dumb store, smart read) applied at the replication level.** A
monotone set of immutable posts converges trivially. Every *non-monotone*
judgment the board makes — liveness, compaction, relevance, unread — is already
reader-local, so none of it needs to cross. Replicate the monotone half; never
replicate a judgment.

Three properties follow, and each one is a defect avoided rather than a
preference:

- **CAS stays correct by construction**, because there is exactly one writer
  host per ref. Followup F2 identifies precisely this as what is lost
  cross-host ("compare-and-swap serializes writers within one ref store only").
  Partitioning the write space restores it instead of replacing it.
- **The lifecycle is acyclic.** Under a merge-commit union, host A reaps a post
  and the next fetch from B resurrects it, indefinitely — because *compaction is
  a judgment, and judgments do not union*. Under per-host refs a reap is
  terminal for the log that made it.
- **A host's compaction cannot govern another host's view.** A merge-and-push
  design would push A's reap judgments onto B. Here, the only source of A's
  posts is A's log.

**Rejected: the merge-commit union**, which is what F2 and registry row
`PROC-board-cross-host` both prescribe ("git-bug solved exactly this with an
operation-log/CRDT model: steal the design, not the dependency"). Its one real
advantage is that it touches less code — every read seam keeps reading one ref.
It does not survive the resurrection cycle. **This spec therefore contradicts a
standing registry row**, which is flagged at G3 and, if approved, amended at
close; F6 records that only git-bug's README was ever read, so the row is a
pointer to prior art rather than a ratified choice.

**Rejected as primary: query the peer on demand** (`ssh lefford board read`).
A network call cannot sit inside a 2 s render budget, it fails offline, and it
stores nothing, so D13's corpus is unreachable through it. Retained as a small
deliberate extra, because an explicit peer read is exactly what is wanted in the
ten seconds before dispatching a census.

**B2 — `origin` is the hub, and sync is symmetric.** Each host publishes its own
log to `refs/hornvale/hosts/<host>` on `origin` and fetches the others back.
Nathan authorized this at G1, having made the repository private rather than
accepting the public-permanence tradeoff; 0118 reserves the call to him
explicitly.

Because both hosts reach `origin` independently (§2 fact 2), nothing is
special-cased for §2 fact 3. Every host runs the same command.

**The standing consequence, stated plainly because a future session will not
infer it: the board's privacy is now the repository's privacy.** If the
repository is ever made public again, every post ever pushed becomes public
retroactively, and 0118 part 3 means there is no redaction operation to reach
for — a point §2 fact 7 makes stronger than 0118 did. Changing repository
visibility is now also a decision about the board.

**B3 — Sync never force-pushes, and a rejected push is a hostname collision.**
Every legitimate movement of a host's own log is a forward commit: appends are
CAS'd forward, and `reap` is documented as "a forward commit parented on the tip
it read" (0118 part 3). So a push can only be rejected if something illegitimate
happened, and the overwhelmingly likely cause is two different machines both
answering to `<host>` and therefore both claiming
`refs/hornvale/hosts/<host>`. A `--force` here is the one operation in this
design capable of violating 0118 part 3 *across* hosts, destroying history the
local ref no longer holds.

The precedent is one layer down and has already fired for real:
`docs/timings/test-baseline-<host>.tsv` is keyed on `hostname -s`, and The
Whetstone forked the baseline by running on a box whose name differed from the
one in the ledger. Same keying, same failure, worse consequence.

*Deferred:* keying the published ref on a generated per-clone id would be more
robust, at the cost of a human no longer being able to read
`refs/hornvale/hosts/lefford` and know what it is. The loud-rejection path
catches the collision anyway.

**B4 — A foreign post is judged by time alone.** Liveness predicates decidable
only at the authoring host — process liveness (`pid`) and branch liveness (`by`
resolving to a live, unmerged branch) — are **not applied to a post from another
host**. A foreign post is live while its TTL holds.

This *generalizes an existing rule rather than inventing one*: `live.rs` already
refuses to judge another host's `pid` against the local process table, with a
test named `a_claim_from_another_host_is_not_judged_by_our_process_table` and the
comment "Only this host's process table is authoritative for this host's"
claims. Branch liveness is the same category and has no such guard. Importing
peers' notices without this rule produces two defects at once:

- a peer's **live** `hold-off` is judged unresolved and eventually reaped out of
  the local view — silent suppression of exactly the warning the board exists to
  carry; and
- every foreign post pays two uncached `rev-parse` calls on every render
  forever, because `LiveContext::probe` memoises an author only by inserting it
  into `live_branches` or `merged_branches` (the measured F14 U-term).

One predicate closes both, and it makes foreign posts *cheaper* to judge than
local ones.

*Rejected:* carrying the authoring host's judgment inside the post — it inverts
D10, and a judgment written at post time is stale by construction. *Rejected:*
renewal/heartbeat posts — they grow the corpus without bound to express what a
TTL already expresses.

**B5 — A foreign claim renders as unverifiable, not as equivalent.** The render
distinguishes a locally-verified liveness ("pid alive here") from a time-only
one ("from lefford, within TTL, unverifiable here"). This is honest about what
the reading host can know, and it is what keeps D7c's non-authority true rather
than merely stated: a claim this host cannot check must not present as one it
can.

**B6 — Sync is out-of-band, and it announces its age.** No network call sits in
any render path. Sync is its own operation, invoked where the wire is already
being crossed, and each peer's staleness is reported at read time.

The budget is the reason: the render's fixed cost is ~0.4 s of a 2 s ceiling and
is dominated by subprocess spawns, so an ssh or HTTPS round trip does not fit —
and putting it there would make the read seam fail exactly when the network is
bad. *Rejected:* an async `SessionStart` hook, because F19 records that whether
an async hook's stdout reaches context is **unsettled**, and that is not a
foundation.

Announcing staleness is not decoration. Decision
[0119](../../decisions/0119-an-instruments-silence-means-the-claim-held.md) rules
that an instrument's silence means the claim held, so a peer unsynced for three
days must not read as "nothing is happening over there" — that is decision 0080's
stuck alarm in another organ.

**B7 — Fixing the read cost is a prerequisite task, not a followup, and F14
optimizes the wrong term.** Batch the per-post object reads into a single
`git cat-file --batch`.

The arithmetic is in §2 facts 5 and 6: the render is at 1.43–1.86 s of a 2 s
budget *today, on one host*, and ~26 of ~36 subprocesses are the per-post reads.
Batching them is a measured 20× on that term and removes per-post scaling
entirely — one spawn regardless of N. Cross-host roughly doubles N, and a render
over budget is **skipped**, not slow.

F14 and D-assumption 1 both aim their mitigations at the **U** term (negative-
cache unresolved authors; resolve branches only for `notice` authors) because
that is what the fix wave measured. For the actual post population U = 2 and
N = 26. F14's mitigations stay correct and become secondary — and B4 removes the
U term for foreign posts anyway.

This also largely **dissolves** F16's durable/ambient tension ("durable posts are
exactly what drives the render past its latency budget, and nothing currently
arbitrates it") rather than arbitrating it: once N is not paid per-spawn, durable
posts stop being what breaches the budget. *Rejected:* raising the 2 s budget,
which hides the cliff without moving it; and reaping aggressively before each
render, which makes the corpus a cost to minimise, against D13's whole premise.

**B8 — Redaction is tip eviction plus digest suppression. Byte removal is
prohibited, and it does not work.** `board redact <id>` evicts a post from the
tip tree immediately rather than waiting for it to die, and the digest — which
reads history (D14) — renders `[redacted by <branch>]` in place of the body.
Both are read-time judgments, i.e. D10, so 0118 part 3 is untouched.

0118 says "there is no redaction operation and adding one would break the
invariant." §2 fact 7 makes that stronger than a rule: a force-push **does not
redact**. The canary commit and its blob were both still served by oid after the
rewrite. So byte removal breaks the invariant *and* buys nothing, and
multi-machine makes it worse — a rewrite breaks fast-forward, so redaction
becomes coordinated force-pushes plus a garbage-collect on every clone, and an
offline clone keeps the bytes indefinitely. **Distributed redaction is not
achievable in the general case.**

*Deferred (registry row):* crypto-shredding — store a sensitive body as
ciphertext and destroy the key to delete it. It is the only construction that
achieves irrecoverability *without* touching 0118 part 3: history intact, pushes
still fast-forward, peers hold ciphertext they can no longer read, no
coordination needed. It costs a key-distribution subsystem and makes the corpus
opaque to the archaeology D13 exists for, so it would be selective rather than
universal.

**B9 — The control that works acts before the bytes exist.** `board post`
scans for obvious credential shapes and refuses. Given B8, prevention is the
only effective control, and multi-machine shortens the window between a post
landing and its propagation.

This is a **refusal at the write seam, not a validation of content** — D12's open
schema is about *shape*, and this is about a small set of high-confidence
patterns with an explicit override for the rare false positive. The posture is
`scripts/hv-guard-bash.sh`'s, which already occupies this role for commands.

**B10 — Corroboration, not consensus: `confirm` and `stale`.** Two conventions,
posted as data (D12), carrying the evidence convention D12b already establishes:

- `confirm{post, evidence{cmd,out}}` — "I hit this too; it held; here is my
  command and output."
- `stale{post, evidence{cmd,out}}` — "this no longer reproduces; here is the
  failure."

They are additive, incapable of suppressing anything, and evidence-bearing
rather than opinion-bearing. A count of corroborations is a **measurement**, and
it is the instrument D-assumption 6 currently lacks — whether technique posts
are worth their cost, where "the honest failure mode is not silence but noise."

**Rejected: upvote/downvote tallies for consensus moderation.** The mechanism is
nearly free — a vote is a post, D12's open schema admits it, D10 tallies it at
read time — and the semantics are the problem:

1. **There is no principal to attach a vote to.** `by` is a branch name and the
   tool validates only `kind` and `by`, so votes are unauthenticated and
   sybil-trivial. Signing would buy *authenticity*, never distinct personhood:
   one key can sign a thousand votes from a thousand invented branch names.
2. **The errors are correlated.** Every voter is a Claude session spawned from
   the same skills reading the same CLAUDE.md. Majority voting buys accuracy
   only under error independence; this is one estimator sampled N times.
3. **It inverts D7c, and this is decisive.** A tally manufactures authority: it
   converts N claims into a number that reads as a verdict. The Cairn's own
   evidence is the quoted reasoning it was built against — *"task impossible,
   **peers doing it.** We should continue"* — where peer count was load-bearing
   in a decision to cross a stated boundary. A vote system does not merely
   permit that reasoning; it institutionalizes it and gives it a quorum.
4. **Moderation could only act on rendering**, since posts are immutable — and
   the post most likely to attract downvotes is an inconvenient `hold-off`, i.e.
   precisely the warning the board exists to carry.

**B11 — Notice liveness must not swallow a newborn branch.** §2 fact 8 is a
defect in shipped behaviour: a notice posted before the branch's first commit
renders as merged, so the board silently swallows the post announcing a
campaign's start. D9's decay rule is right; its implementation has a false
negative at **birth** rather than only at death. What makes it worse than it
looks is that the post succeeds, returns a hash, and reads back byte-correct —
nothing looks wrong unless you grep the render.

The fix is stated as a **property, not a mutation**: a branch with a live
worktree is live whatever the ancestry says, and failing that, a nascent branch
is `ahead 0, behind 0` where a merged one is `ahead 0, behind >0`. The
implementer picks the discriminator after reading `live.rs`. The failure
direction must be the safe one — a just-merged notice lingering slightly beats a
new one vanishing.

## 4. The design

### 4a. Refs

| ref | written by | meaning |
|---|---|---|
| `refs/hornvale/board` | this host only | this host's log; **unchanged**, still the sole CAS target |
| `refs/hornvale/hosts/<host>` (on `origin`) | its owner only, fast-forward only | this host's published mirror |
| `refs/hornvale/peers/<host>` | `board sync` | local mirrors of other hosts' logs |

A read is the union of `refs/hornvale/board` and every `refs/hornvale/peers/*`
**except this host's own mirror**, deduplicated by post id.

Skipping `peers/<self>` is not tidiness. The fetch refspec brings a host's own
published mirror back, and that mirror is by definition *behind* the local log
whenever a `reap` has not yet been pushed — so including it would resurrect this
host's own reaped posts into the read view. Pushing before fetching (§4b) also
avoids it, but relying on call order to preserve a compaction invariant is the
kind of coupling that survives until someone reorders two lines. Skip the ref
instead, and let the push order be an optimisation rather than a correctness
requirement.

No schema change, no new required field, no migration: the 26 posts on the board
today work unaltered, and `host` is already carried by claims.

### 4b. Sync

`board sync` does two things and neither may be fatal:

1. **push** `refs/hornvale/board` → `refs/hornvale/hosts/<host>`, without
   `--force` (B3). A non-fast-forward rejection is reported as a probable
   hostname collision, naming the ref.
2. **fetch** `+refs/hornvale/hosts/*:refs/hornvale/peers/*`.

Then it records a per-peer sync time in repository-**common** private state.
Unlike the read cursor, sync state is per-repository rather than per-worktree: a
fetch serves every worktree at once, so recording it per-worktree would report
nine different sync ages for one fetch. Verified: from inside a worktree,
`git rev-parse --git-dir` gives
`/…/.git/worktrees/the-beacon` while `git rev-parse --git-common-dir` gives
`/…/.git` — so the common dir is reachable and distinct, which is what the
cursor's `git_path` helper does *not* give.

Failure is always non-fatal: the local append already succeeded, so an
unreachable `origin` degrades to exactly the single-box behaviour that ships
today. Invocation points, none of them a render path:

- after `board post`, best-effort — a warning travels at the moment its value is
  highest, which is 0118's stated reason for the whole design ("a post's whole
  value is that it arrives before the merge it warns about");
- `make board-sync`, explicitly;
- inside `make preflight`, the pre-merge moment a peer's `hold-off` matters most;
- inside the remote-dispatch paths that already cross the wire.

### 4c. Reads

`live_posts` gains one branch: a post whose `host` differs from this host's is
judged by TTL alone (B4) and rendered as unverifiable-here (B5). Nothing else
about relevance, capping, or the cursor changes — the cursor is already a set of
post ids, which are globally unique content hashes, so it works over a union
unmodified. Its one required change is that `record`'s prune must retain ids
present at *any* tip in the union, or foreign posts are pruned from `seen` and
re-render forever.

The render header reports each peer's sync age (B6). A peer never synced, or
synced beyond a visible threshold, says so.

### 4d. The read-cost fix

One `git cat-file --batch` replaces the per-post `git cat-file -p` loop (B7).

**There are two such loops, not one, and the second is the worse offender.**
`Board::posts_in` (`store.rs:293`) is the one behind `posts_at_tip` and
`snapshot`, so batching there covers every ambient and on-demand read seam
without changing their shape. But `digest::history` (`digest.rs:59`) walks
`git log --diff-filter=A --name-only` and issues **its own** `cat-file -p` per
post — it does not go through `posts_in`. Its N is sized by the *history window*
rather than by the tip, so it is potentially far larger than 26, and the human
read seam D14 exists for is the one that would stay slow if only `posts_in` were
fixed. Both loops are in scope.

`TipSnapshot`'s single-read discipline is preserved: it reads the board exactly
once, and that stays exactly once.

### 4e. Redaction, the secret scan, corroboration

- `board redact <id>` — appends a redaction post and evicts the target from the
  tip tree (B8). The digest suppresses the body of a redacted post while still
  reporting that it existed and who redacted it; the corpus records the act.
- `board post` refuses obvious credential shapes with an explicit override
  (B9).
- `confirm` and `stale` ship as a **version-2 convention post** superseding the
  version-1 set already on the board, per D12's supersede-by-posting rule. The
  digest tallies corroborations per technique. Whether either needs code beyond
  the digest tally is an implementation finding, not a spec claim.

### 4f. Degradation ladder

| condition | behaviour |
|---|---|
| `origin` unreachable | local post stands; peers go stale; staleness announced |
| a peer ref absent | union of what exists |
| a foreign post corrupt | skipped with a warning, render still exits zero (existing) |
| push rejected non-fast-forward | loud, names the ref and the collision hypothesis; never forced |
| a peer's `host` unknown locally | judged by TTL, rendered unverifiable |

## 5. Non-goals

- **Not automatic sync.** Nothing syncs on a timer or in a hook; F17's
  automate-compaction question is adjacent and stays open.
- **Not a lock, and still never authority.** Unchanged from D7/D7c. A peer's
  claim is information about another machine, and B5 makes that visible rather
  than assumed.
- **Not post signing.** Deferred to a registry row, with the argument that this
  campaign is what creates the case for it: a post arriving in `peers/<host>`
  asserts its `by` with nothing behind it.
- **Not byte-level deletion** (B8), and not a claim that redaction is achievable
  across clones.
- **Not a vote or reputation system** (B10).
- **Not a change to the post schema.** No new required field; `host` already
  exists.
- **Not part of determinism.** The board writes nothing any world, artifact, or
  seed derivation reads.

## 6. Assumptions requiring measurement

1. **The batched read holds the render under budget at cross-host volume.**
   Budget: ≤ 1 s at 100 posts across two peers. Measured for the batch primitive
   in isolation (§2 fact 6); unmeasured end-to-end.
2. **Two hosts is the real population, and N grows roughly linearly with
   hosts.** If a third box appears, the union's fixed cost grows by 2 per peer,
   which is negligible; the post count is the term that matters.
3. **`origin` round-trip latency is acceptable at the invocation points
   chosen.** Unmeasured. If a post-then-push is slow enough to be noticed, the
   push moves to the explicit seams only.
4. **Sessions on lefford will actually read the board.** The write side is
   judgment (D-assumption 4) and the read side is mechanized — but lefford has
   no board binary today (§2 fact 1), so the seam must be built there, and
   `make prewarm`/`doctor`'s existing "the board binary is not built" message is
   what covers the gap.
5. **The secret scan's false-positive rate is low enough not to be routed
   around.** A guard that is habitually overridden is worse than none, because
   it trains the override. If it fires spuriously, narrow it or drop it —
   `PROC-claim-shape-s-heuristic` is the standing example of a too-broad
   predicate in this exact tool.
6. **Corroboration gets used.** `confirm`/`stale` is the instrument for
   D-assumption 6, and it is itself an assumption: if no technique is ever
   corroborated or marked stale, that is a finding about the channel, reportable
   as such.

## 7. Verification

All from `ambrose`, `main` at `8c61cbc5`, 2026-08-11.

```
# fact 1: lefford has no board, and both boxes have only origin
  $ ssh lefford 'cd ~/Projects/hornvale && git for-each-ref refs/hornvale/'
  -> (empty)
  $ ssh lefford '… ls tools/board/target/*/board'
  -> (no output)
  $ git remote -v            # and the same on lefford
  -> origin  https://github.com/hornvale/hornvale.git (fetch/push)

# fact 2: GitHub accepts a custom ref namespace, both boxes, both directions
  $ git push origin a87a88c6…:refs/hv-board-probe/ambrose
  -> * [new reference]   a87a88c6… -> refs/hv-board-probe/ambrose      (rc=0)
  $ git ls-remote origin 'refs/hv-board-probe/*'
  -> a87a88c6…  refs/hv-board-probe/ambrose
  $ ssh lefford '… git fetch origin refs/hv-board-probe/ambrose:refs/hv-board-probe/from-ambrose'
  -> * [new ref]         refs/hv-board-probe/ambrose -> …/from-ambrose
  $ ssh lefford '… git push origin …/from-ambrose:refs/hv-board-probe/lefford'
  -> * [new reference]
  cleanup: git push origin --delete (both), git update-ref -d on lefford,
           final ls-remote -> (empty)

# fact 3: lefford cannot reach ambrose
  $ ssh lefford 'ssh -o BatchMode=yes ambrose hostname -s'
  -> ssh: Could not resolve hostname ambrose: Name or service not known

# fact 4: the board cannot be published by accident
  $ git config --get-all remote.origin.fetch
  -> +refs/heads/*:refs/remotes/origin/*
  $ git push --all --dry-run origin
  -> * [new branch]  campaign/the-grain -> campaign/the-grain     (branches only)

# fact 5: the render is at its ceiling (26 posts, one host)
  $ for i in 1 2 3; do /usr/bin/time -p bash scripts/board-render.sh; done
  -> real 1.43 / real 1.86 / real 1.52          against a 2 s budget

# fact 6: the per-post read dominates, and batching is 20x
  $ git ls-tree --name-only refs/hornvale/board posts/ | wc -l
  -> 26
  author tally -> campaign/the-grain RESOLVES, main RESOLVES,
                  campaign/the-cairn UNRESOLVED        => U = 2, N = 26
  $ time (for i in $ids; do git cat-file -p "$i" >/dev/null; done)
  -> real 0m0.850s
  $ time (printf '%s\n' $ids | git cat-file --batch >/dev/null)
  -> real 0m0.041s

# fact 7: force-pushing does not redact on GitHub
  commit 1aead4f8… contains {"kind":"technique",…,"note":"SECRET-CANARY-9f3a"}
  commit 9212cfce… is the rewrite without it
  $ git push origin 1aead4f8…:refs/hv-redact-probe/x       -> ok
  $ git push --force origin 9212cfce…:refs/hv-redact-probe/x -> ok
  $ gh api repos/hornvale/hornvale/git/commits/1aead4f8… --jq .sha
  -> 1aead4f8a96f07b5ce2bf3e6584ed799ba5d2dec        # STILL THERE
  $ gh api repos/hornvale/hornvale/git/blobs/92ade666… | base64 -d
  -> {"kind":"technique","by":"probe","note":"SECRET-CANARY-9f3a"}   # PLAINTEXT
  cleanup: probe ref deleted; objects now unreachable and gc'd at
           GitHub's discretion, which is not a guarantee

# fact 8: a notice from a newborn branch is invisible
  $ git worktree add .claude/worktrees/the-beacon -b campaign/the-beacon main
  $ board post notice campaign/the-beacon polarity=fyi paths=[…] …
  -> 0d0a04c501b62acdb7554c4811dda606d2ed3b28
  $ git cat-file -p refs/hornvale/board:posts/0d0a04c5….json
  -> correct bytes, all fields intact
  $ board read | grep -c the-beacon
  -> 0                                             # SWALLOWED
  $ git merge-base --is-ancestor campaign/the-beacon main; echo $?
  -> 0                                             # tests as merged
```

Test plan (`cargo test --manifest-path tools/board/Cargo.toml`):

1. **Union read** — a post present only in a peer ref renders; ids are
   deduplicated across refs; a post in both refs renders once.
2. **Reap never touches a peer ref**, and a locally-reaped post does not return
   to the local log after a fetch.
3. **A foreign notice whose `by` does not resolve locally still renders** (B4) —
   the regression guard for the silent-suppression defect.
4. **A foreign claim past its TTL expires; inside its TTL it renders as
   unverifiable, never as locally-verified** (B4/B5).
5. **The push path never forces**, and a synthesized non-fast-forward exits
   non-zero naming the ref and the collision hypothesis (B3).
6. **Sync is idempotent** — a second sync with no new posts changes no ref.
7. **Sync failure is non-fatal** — with an unreachable remote, `post` still
   lands and the render still exits zero.
8. **Staleness is reported** — a peer with an old or absent sync marker renders
   its age; the never-synced case is distinguishable from the just-synced one.
9. **Cursor over a union** — `record`'s prune retains ids present at any tip; a
   foreign post shown once does not re-render.
10. **The batched read is behaviourally identical** to the per-post loop —
    same posts, same order, same malformed-post skipping (a golden over a fixture
    tip), and a corrupt object still warns and skips rather than failing the read.
11. **Render budget** — 100 posts across two peers renders inside the line and
    latency budgets.
12. **Redaction** — a redacted post leaves the tip tree, remains reachable in
    history, and the digest reports the redaction without the body (B8).
13. **Secret scan** — a post carrying a credential shape is refused; the
    override works; a post carrying an ordinary note is not refused (the
    false-positive arm, which is the one that matters per assumption 5).
14. **Newborn-branch notice** — a notice from a branch with no commits of its
    own renders (B11), and a genuinely merged branch's notice still stops
    rendering. Both arms, or the fix is untested in the direction that matters.

## 8. Definition of Done

- `board sync`, the union read, B4/B5's liveness split, B7's batched read,
  `board redact`, the secret scan, and the `confirm`/`stale` convention post
  landed, with §7's fourteen tests green.
- The read seams updated: `scripts/board-render.sh`, `doctor.sh`'s board
  section, `preflight-merge.sh`'s `hold-off` surface, `board digest`.
- `make board-sync` added; `make help` lists it. The board built on lefford so
  the seam exists there (assumption 4).
- B11's defect fixed with both arms tested.
- A `technique` post for each measured fact worth propagating — the custom-ref-
  namespace command set, the force-push-does-not-redact finding, the
  newborn-branch trap (already posted, `d02aaf2c…`).
- Registry rows: crypto-shredding; post signing (deferred, with the
  multi-machine argument); vote tallies as **rejected**, with B10's reasoning so
  it is not re-proposed from scratch. `PROC-board-cross-host` amended, since B1
  contradicts its prescription. F14/F16's entries amended to record which term
  actually dominated.
- Decision record for B1/B2/B4 (the substrate, the transport and its standing
  privacy consequence, and the foreign-post judgment rule).
- Chronicle entry, `docs/retrospectives/the-beacon.md`, book freshness sweep,
  Confidence Gradient re-score if any bet moved.

## 9. Open questions

1. **Does a third host ever appear?** The design is N-host by construction, but
   only two are real. Nothing here assumes two except the measurement budgets.
2. **Does `origin` want a `hosts/` cleanup policy?** A retired box leaves its
   ref behind forever. It reads fine (a union just includes it) and it is
   history, so B1 says leave it; a future session may disagree.
3. **Does the census/heavy claim move onto the board now?** F3 deferred it
   pending the board proving reliable; cross-host claims are the capability that
   made it interesting, and B5's unverifiable-here rendering is the honest form
   it would take. Still out of scope: it would put a new tool in front of the
   guard protecting census writes.
