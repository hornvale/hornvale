# The Beacon — Design

**Campaign:** The Beacon
**Branch:** `campaign/the-beacon`
**Date:** 2026-08-11
**Status:** spec, G3 approved 2026-08-11 with two extensions (§9)

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

Decision [0125](../../decisions/0125-github-actions-is-retired.md) (GitHub Actions is retired) records the same visibility change
from the CI side. Two of its consequences bear on this campaign. It **removes CI from the argument
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
8. **Notice liveness is tested by ancestry, so any author that is an ancestor of
   `main` is filtered out — which has two instances, and the severe one is
   permanent.** Notice liveness requires the `by` branch to exist *and be
   unmerged*.
   - **Temporary: a newborn campaign branch.** A fresh branch's tip *equals*
     main, so it tests as merged. Found by dogfooding this campaign's own
     opening notice, which returned a hash, stored byte-correct, and rendered
     not at all. **Observed self-healing mid-session:** the same post went from
     rendering 0 to rendering 1 purely because the branch gained its first
     commit. That transition is the measurement that identifies "ahead > 0" as
     the operative variable.
   - **Permanent: `main` itself.** `git merge-base --is-ancestor main main`
     exits 0 — main is trivially its own ancestor — so **main can never post a
     live notice.** Three main-authored notices sit at the tip and render to
     nobody, one of which announces that main is red on a heavy-tier
     calibration. Independently found and documented by a session working on
     main (board technique `725655f4`), whose framing is the right one: this
     hits the *default* author, and chores landing directly on main are exactly
     the changes other branches most need warning about.
   - **And they are not merely invisible — a reap deletes them.**
     `store.rs`'s `reap_drops_a_notice_whose_branch_resolves_and_is_merged_regardless_of_age`
     is the shipped behaviour, and main's notices both resolve *and* test as
     merged, so the unresolved-notice grace period does not apply to them. The
     next `board reap` silently drops them from the tip tree.
   - **Not the same defect: `technique` posts are unaffected.** Verified against
     the suspicion — techniques render regardless of author, and the three of
     five main-authored techniques that do not render are exactly the three
     named by `retract` posts, which is intended. So `725655f4`'s stated
     workaround (post it as a technique instead) is sound.

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

**B4 — A foreign post is judged by time alone, and foreignness comes from the
ref it was read from.** Liveness predicates decidable only at the authoring host
— process liveness (`pid`) and branch liveness (`by` resolving to a live,
unmerged branch) — are **not applied to a post from another host**. A foreign
post is live while its TTL holds.

A post is foreign iff it was gathered from a peer ref. **Not** iff its `host`
field differs: zero of the 32 posts at the tip carry `host` at all (§4c), so a
field-reading predicate would be inert, and a field can be omitted or mistyped
where a ref name cannot. Structural provenance is the property B1's layout buys,
and this is where it gets spent.

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

**Task 6 promoted this from a rendering detail to a hard requirement, and it is
worth knowing why the argument changed.** B4 means a foreign post cannot decay
locally — the authoring host decides, and `reap` is single-ref. For a `notice`,
which carries no `ttl_s`, that means *nothing local bounds its lifetime at all*
(§10 question 2). So a reader needs some signal that a peer's content is stale,
and it must ship with the sync rather than after it.

**Two different ages, and the first draft of this paragraph confused them.** As
originally amended, this section claimed "sync-age is the only remaining signal
that its content is stale." That is wrong, and Task 7's review caught it:

- **Mirror age** — how long since *this host* last fetched. Reports that our copy
  may be behind. On a host that syncs regularly it reads a few seconds forever,
  **including when the peer has been frozen for a month**, so in exactly the
  retired-peer scenario this section argues from, it is flat.
- **Peer content age** — how long since the peer itself last posted, read from
  its mirror ref's own last commit. *This* is the signal the argument needs.

Both are reported. The distinction matters because they fail in opposite
directions: a stale mirror with a busy peer means *we* are out of touch, while a
fresh mirror with a silent peer means *they* are gone. Only the second is the
retired-box hazard.

One implementation constraint worth recording, because it looks like an
arbitrary choice: peer content age is read per-ref (`git log -1 --format=%ct`),
not through a dereferencing `for-each-ref` format. `read_refs` deliberately uses
`%(refname)`, because a dereferencing format makes the *whole listing* exit 128
on a single dangling mirror — which would blank the ambient render for that host.
A per-ref call fails only the ref it names, which the read path already tolerates.

Even with both ages, this is **not a bound** — a stale-but-rendering hold-off
still competes for the ambient budget forever. These are signals; the bound is
§10 question 2's obligation, which is dated rather than open.

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

**B11 — Notice liveness must not be derived from ancestry alone; `main` is
unconditionally live.** §2 fact 8 is a defect in shipped behaviour with two
instances. D9's decay rule is right — a notice from a torn-down worktree *is* a
false warning that costs other sessions work — but deriving "superseded" from
"is an ancestor of main" has a false negative at **birth** and a permanent one
at **`main`**, rather than only the intended one at death.

The severe instance is main, for three compounding reasons: it is permanent
rather than self-healing; it hits the **default author**, so it is the case a
session falls into without choosing it; and changes that land directly on main
are precisely the ones other branches need warning about. There is a red-main
warning unread on the board right now because of it.

**And a reap deletes these posts, so this is not only a rendering bug.** Merged
notices are dropped regardless of age, with no grace period, so the fix must
land before anyone runs `board reap` or the evidence goes with it. That makes
B11 the one item in this campaign with a *deadline* rather than merely a
priority.

The fix is stated as a **property, not a mutation**: `main` is live
unconditionally; a branch with a live worktree is live whatever the ancestry
says; and failing both, a nascent branch is `ahead 0, behind 0` where a merged
one is `ahead 0, behind >0`. The implementer picks the discriminator after
reading `live.rs` — the newborn-branch transition measured in §2 fact 8 is the
evidence that `ahead` is the operative variable. The failure direction must be
the safe one: a just-merged notice lingering slightly beats a live one
vanishing.

Test both arms, and all three authors — main, a newborn branch, and a genuinely
merged branch — or the fix is untested in the direction that matters.

**B12 — Suggestions are a post kind, they carry the friction that provoked
them, and they are digest-only.** `suggest{note, evidence{cmd,out}?, paths[]}`
— a proposal for improving the board, posted on the board.

The gap it fills is friction, not absence of a home. A board improvement already
*has* a durable home: CLAUDE.md is explicit that "the tooling/process backlog is
the idea registry's `TOOL-*` and `PROC-*` rows." But a registry row costs a
commit, a gate, five-column conformance and a 600-character budget — this
campaign spent three rounds trimming two rows to fit. That cost is right for an
idea meant to outlive a campaign and wrong for "the render should report sync
age." So `suggest` is the zero-friction inbox and the registry stays the durable
record; the promotion path is `suggest` → digest → `PROC-*` row for whatever
survives, which is the same promote-before-teardown discipline the campaign
followup register already uses.

Two properties, both load-bearing:

- **It carries its provoking incident**, exactly as D12b makes a `technique`
  carry the command and output that established it. "The render should show sync
  age" is a wish; "I ran this, wanted that, got the other" is a bug report. The
  convention makes the difference visible to a reader without anyone policing
  it — the same reason D12b can afford not to be a validation rule.
- **It never renders ambiently; it renders only in `board digest`.** Three
  reasons that compound: a suggestion is not actionable by the session that
  reads it, so ambient delivery is pure cost; it is the lowest-effort post kind
  and therefore the likeliest flood source, against which F13 records that
  nothing bounds distinct posts; and B7 is spending real effort on the render
  budget, so a new *free* post kind is worth having. Digest-only costs the
  render nothing and reaches the one reader who can act on it (D14).

**A property to preserve rather than change:** `board digest` exits non-zero on
error, unlike the deliberately quiet ambient render (D7). That asymmetry must
survive, because the digest is the instrument that would report the board itself
being broken — a digest that failed quietly would make board defects invisible
by construction, which is D14's failure mode reached through the back door.

**B13 — A risk-scoped fast lane for the board's non-destructive surface, and a
path-scoped hook rule.** The board is dev tooling outside the cargo workspace
with no determinism surface, so campaign cadence is the wrong price for a render
fix. Two separate mechanisms, which solve two different problems and do not
substitute for each other.

**The hook rule fixes a mis-targeted check.** Measured: `tools/board` is *not* a
workspace member (`members = ["kernel", "domains/*", "windows/*", "cli"]`), so
`cargo clippy --workspace` and `make quick` never examine it — while the
pre-commit hook's Rust-relevant filter *does* include `.rs`, so a board change
triggers `make quick` anyway. The suite that actually tests the board takes
**24.4 s** and is run by no gate at all. So a board change today pays for a gate
that cannot see it and skips the one that can. The rule: a `tools/board/`-only
change runs `cargo test --manifest-path tools/board/Cargo.toml` in place of
`make quick`. This is a strict improvement and is independent of any branch.

**The lane fixes write contention on `main`.** `scripts/CLAUDE.md` records that
a linked worktree may not commit to `main` and the primary checkout may — so
main has a single writer, and that writer is frequently mid-landing (it was
during this campaign's own spec work, with 0125 staged). A shared lane that any
worktree may commit to and that merges promptly sidesteps the bottleneck.

**Named by risk, not by schedule.** Nothing here runs nightly, so a name
promising a cadence invites the lane becoming long-lived — which would make it a
second `main` and reintroduce every divergence problem B1 exists to avoid. One
lane, short-lived per cycle, merged promptly.

**The lane is not uniform across the board's surface, and cross-host shortens
the eligible list:**

| change class | lane? | why |
|---|---|---|
| render, relevance, digest | yes | read-only, per-host, non-destructive |
| a new post kind or convention | yes | D12: the schema is open, unknown fields round-trip |
| liveness predicates (the B11 family) | yes | B1 keeps `reap` per-host, so the blast radius is one log |
| **`reap` semantics** | **no** | the only destructive operation; a wrong rule deletes posts |
| **the CAS / append path** | **no** | silent write loss |
| **the sync / push path** | **no** | B3 — this is where 0118 part 3 can be violated across hosts |

"The board cannot break a world" does not cover the exclusions: `reap` can
delete the evidence, which is precisely the hazard B11 surfaced. Those three
keep campaign discipline.

**B12 and B13 must not be wired into a loop.** They arrived in one sentence,
which invites fusing them: a suggestion posted, then auto-implemented on the
lane. That would make the board self-modifying with no human in it, and the
board is what every session reads at `SessionStart`. The lane lowers *ceremony*,
never *review* — the human-visible commit and the suite stay. Stated as a
decision because the fusion is the attractive mistake, not an unlikely one.

**And a non-obvious reason the suite is mandatory on the lane.** The read seams
are deliberately non-fatal (D7): the hook is `|| true` and a failed render warns
and stays quiet. So a broken board does not announce itself — it goes *silent*,
which is the one failure mode D14 says kills a board. Non-fatal is not the same
as safe, and the 24 s suite is the only thing that would notice.

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

`live_posts` gains one branch: a **foreign** post is judged by TTL alone (B4)
and rendered as unverifiable-here (B5).

**Foreignness is the ref a post was read from, not a field in the post.**
Measured 2026-08-11: of the 32 posts at the tip, **zero** carry a `host` field —
8 notices, 18 techniques, 4 retracts, a reply and a convention post, all
`host=None`. `host` is a *claim* convention, and it is optional even there, so a
predicate reading it would classify every peer post as local and B4 would be
silently inert. Provenance instead comes from the read: a post gathered from
`refs/hornvale/peers/<host>` is foreign, and one from `refs/hornvale/board` is
local. This is strictly better than a field — the ref name cannot be forged,
omitted, or mistyped by a posting session, which is the structural-provenance
property B1's per-host layout was supposed to buy in the first place. `host`
keeps its existing narrower job: naming which machine's process table can judge
a claim's `pid`.

Concretely, `StoredPost` gains an origin discriminant alongside `id`,
`post` and `committed_at`, set by the reader from the ref it walked. **This is a
struct-field addition, so it breaks every full-literal construction site**: 20 of
them, across `store.rs`, `render.rs`, `relevance.rs`, `digest.rs` and `live.rs`.
They are compile errors rather than silent breakage, but the count and the file
list belong in the plan rather than being rediscovered. Nothing else
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
14. **Notice liveness, three authors** (B11) — a notice authored by `main`
    renders; a notice from a branch with no commits of its own renders; a
    genuinely merged branch's notice still stops rendering. All three, or the
    fix is untested in the direction that matters.
15. **A reap does not drop a live-by-B11 notice** — the regression guard for the
    deadline: main-authored and newborn-branch notices survive compaction, while
    a genuinely merged branch's notice is still dropped.
16. **A `suggest` post is digest-only** (B12) — present in `board digest`,
    absent from both the ambient render and `board read`, and it does not
    consume the ambient post budget. Both arms: absent where it should be,
    present where it should be.
17. **The digest still fails loud** (B12) — an unreadable board makes
    `board digest` exit non-zero, where the ambient render exits zero. The
    asymmetry is the property, so assert both halves in one test or the next
    change to error handling will quietly flatten them.

Verification steps that are not unit tests, and must be run and recorded rather
than asserted:

- **The hook rule** (B13) — stage a `tools/board/`-only change and confirm the
  hook runs the board suite and not `make quick`; then stage a mixed change and
  confirm it runs `make quick`. The second arm is the one that matters, since a
  path filter that is too broad silently drops the workspace gate.

## 8. Definition of Done

- `board sync`, the union read, B4/B5's liveness split, B7's batched read,
  `board redact`, the secret scan, and the `confirm`/`stale` convention post
  landed, with §7's seventeen tests green, plus B13's two recorded hook-rule arms.
- The read seams updated: `scripts/board-render.sh`, `doctor.sh`'s board
  section, `preflight-merge.sh`'s `hold-off` surface, `board digest`.
- `make board-sync` added; `make help` lists it. The board built on lefford so
  the seam exists there (assumption 4).
- B11's defect fixed with all three authors tested, and a reap regression guard. This item has a DEADLINE: a reap drops these notices permanently.
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
- `suggest` shipped as part of the version-2 convention post (B12), digest-only,
  with a `PROC-*` promotion path stated in the convention itself.
- B13's hook rule landed in `scripts/hooks/pre-commit` and documented in
  `scripts/CLAUDE.md`; the lane, its exclusion list, and the
  no-loop-with-B12 rule documented in the root `CLAUDE.md` board section.
- Decision record for B13 — it is a process change, and process choices that
  live only in a spec get relitigated.
- An invitation to suggest posted **to the board**, since B12's channel is
  worth nothing unannounced.
- Chronicle entry, `docs/retrospectives/the-beacon.md`, book freshness sweep,
  Confidence Gradient re-score if any bet moved.

## 9. Post-G3 amendments (2026-08-11)

Approved at G3 with no changes, then extended by two directives from Nathan,
both folded in as B12 and B13.

**N1 — encourage suggestions for improving the board.** Became B12. The design
question it forced was not *where* suggestions live — the idea registry already
claims that role — but why nobody uses it for small things, which is friction.
The answer is a two-tier path (free post, durable row) rather than a new
backlog, and the non-obvious call is that suggestions must be **digest-only**:
the lowest-effort post kind is the likeliest flood source, and B7 is spending
real effort on exactly the budget it would consume.

**N2 — a fast lane for board changes, off `main`.** Became B13, and it changed
under measurement. The proposal was one mechanism; the ideonomy pass established
that two different problems were in play — a mis-targeted check and single-writer
contention on `main` — which are orthogonal, so neither substitutes for the
other. Both are in scope. Two adjustments to what was asked for: the lane is
named by **risk rather than schedule**, because nothing runs nightly and a
cadence-shaped name invites a long-lived second `main`; and B12 and B13 are
explicitly **not** wired together, because auto-implementing suggestions would
make the medium every session reads self-modifying with no human in it.

The measurement that shaped it: `tools/board` is not a workspace member, so
`make quick` cannot see it, while the pre-commit hook's `.rs` filter runs
`make quick` anyway — and the 24.4 s suite that does test it runs in no gate.
The current cost is not merely high, it is spent in the wrong place.

## 10. Open questions

1. **Does a third host ever appear?** The design is N-host by construction, but
   only two are real. Nothing here assumes two except the measurement budgets.
2. **A retired box's `hosts/` ref needs a cleanup policy, and this is no longer
   an open question — it is a dated obligation.** Originally filed as "it reads
   fine and it is history, so B1 says leave it; a future session may disagree."
   Measured during Task 6, that framing is wrong, and the correction is the
   sharpest consequence the campaign found:

   **A foreign `notice` is unbounded in local time.** A local notice decays when
   its branch dies (D9). B4 correctly removes that predicate for foreign posts,
   because only the authoring host can judge it — and puts nothing in its place,
   because the `notice` convention has no `ttl_s` to fall back on. Verified: of
   the 32 posts on the board, **zero carry `ttl_s`, `host`, or `pid`**.

   The other kinds are unaffected for reasons that make this narrower and worse
   rather than broader and milder. `technique`, `convention`, `reply` and
   `retract` already render unconditionally regardless of origin — they were
   unbounded before B4. A real `claim` does carry `ttl_s` by convention, so
   foreign claims genuinely expire. **`notice` is the only kind whose decay B4
   removes — and it is the kind that carries `polarity=hold-off`, the one post
   that asks other sessions to wait.** So the exposure is a permanent hold-off
   from a machine nobody is running any more.

   Two things bound a peer and both die with it: its own `reap` (the correct
   authority, but it requires a live peer), and retraction, which does propagate
   cross-origin — `retracted` is built over the whole union and checked *before*
   the origin short-circuit, so a live peer can withdraw its own post and this
   host honours it. A retired host posts no more retracts.

   **B6's sync-age reporting is therefore necessary but NOT sufficient**, and
   this is the part worth stating precisely: sync-age tells a reader the mirror
   is stale, while the posts still render as current. A frozen peer's hold-off
   keeps competing for the ambient line budget, and because an elided post is
   never recorded as seen, it re-qualifies whenever the board is quiet. It never
   dies. Sync-age is a *signal*, not a bound.

   The obligation: answer this **before the peer population grows beyond two**.
   Candidate shapes — a max-staleness after which a peer's posts stop rendering;
   an explicit retire-a-host operation; or a TTL convention for `notice` — but
   the answer is not in scope here, and leaving it unanswered while adding hosts
   is what would make it expensive.

   **Two adjacent problems belong to the same obligation**, both surfaced by Task
   7's review and both keyed on the same weakness: the self-mirror exclusion, and
   every other per-host identity here, is keyed on `hostname -s`.

   - **A host's name changing** — by rename, or by a *second machine* appearing —
     leaves `peers/<oldname>` behind as a phantom peer, and, the worse half,
     re-admits it to the union read, which is precisely the reaped-post
     resurrection B1's own-mirror exclusion exists to prevent.

     Not hypothetical, though the precedent is not the one first cited here. An
     earlier draft claimed a rename `MacBookPro` → `ambrose`; the Whetstone
     retrospective §3 says the opposite — *"the baseline is keyed `MacBookPro` at
     10. **Same ledger, different box.**"* It was a second machine, not a rename.

     That correction strengthens the point rather than weakening it. A **host
     fork** is the sharper precedent, because it is exactly this failure: a new
     host key appears, the old one persists in a shared per-host ledger, and
     nothing reconciles them. It has already happened once to
     `docs/timings/test-baseline-<host>.tsv`, which is keyed on `hostname -s` for
     the same reason `refs/hornvale/hosts/<host>` is — and there the consequence
     was a silently-recorded baseline that *could not alarm*.
   - **Retiring a host** is the same operation seen from the other end.

   So "retire a host" and "rename a host" are one feature, not two, and B3's
   hostname-collision rejection is the third face of it. Whatever answers this
   should answer all three.
3. **Does the census/heavy claim move onto the board now?** F3 deferred it
   pending the board proving reliable; cross-host claims are the capability that
   made it interesting, and B5's unverifiable-here rendering is the honest form
   it would take. Still out of scope: it would put a new tool in front of the
   guard protecting census writes.
