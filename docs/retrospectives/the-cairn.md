# The Cairn — retrospective

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-cairn.md) and the substrate choice
is [decision 0118](../decisions/0118-the-board-is-an-orphan-ref-of-immutable-posts-never-rerooted.md).

Nine tasks, roughly eleven fix rounds, one whole-branch review, one fix wave.
The tool works; most of what this campaign has to teach is about the loop that
built it.

## 1. The plan carried the code, so review mostly corrected the plan's author

Almost every fix round in this campaign corrected a defect in **the plan text**,
not in an implementer's work. Task 1's file list omitted a module the `lib.rs` it
also specified declared, so the crate could not compile. Task 3's throwaway index
was named from the pid alone, which eight threads of one process share. Task 6's
revision reintroduced a bug three earlier rounds had removed. The plan's Global
Constraints asserted a fact about clippy's config resolution that was simply
wrong. In each case the implementer transcribed faithfully and the transcription
was the problem.

That is the *expected* distribution when a plan contains complete code, and it is
worth stating because it reframes what per-task review is for. It is not
primarily checking whether the implementer did what the plan said — they did —
but whether what the plan said was right. Two practical consequences, both of
which paid here: aim each review at a **failure class** rather than at a
checklist (Task 4's review was pointed at "a post that silently vanishes when it
should have rendered", and it found exactly that), and tell the reviewer
explicitly when a brief's own code blocks are stale, because a reviewer measuring
code against a stale draft files false findings on precisely the parts the
revision fixed.

## 2. One defect class, four instances, each through a different door

The defect: a post silently and permanently lost — marked seen without being
shown, or dropped from the tip tree without being dead.

1. **The relevance filter.** The cursor held a *tip*, so a notice that was unseen
   but filtered out as irrelevant was marked seen anyway. When the reading
   session later started editing those paths, the notice was relevant and already
   seen, so it never rendered. By construction, not on an error path — no failure
   had to occur.
2. **The render cap.** Posts past the line budget were elided into a "… N more"
   line while `record` was handed the entire *untruncated* set. With more than a
   dozen simultaneously unseen-and-relevant posts — ordinary data — the elided
   ones were permanently seen without ever being shown.
3. **Reap's threshold.** Liveness and reapability were one predicate. A branch
   that fails to *resolve* is indistinguishable from one merged and pruned long
   ago, and that reading is reachable by a transient race, so reap could
   permanently drop a live notice.
4. **Reap's two separate reads.** The caller probed liveness over one post set
   and `reap` re-read its own, so the predicate was evaluated over a post set the
   context had never seen. A claim appended inside that window had never been
   passed to `ps`, read as pid-dead, and was dropped.

**Closing one door on a defect class does not close the class.** Instances 1 and
2 were fixed, carefully, with tests — and instances 3 and 4 arrived afterwards
through different doors. What finally worked was making the bad state
*unrepresentable* rather than merely absent:

- `Displayed`, whose only public constructor performs the relevance filter
  itself, so handing `record` a raw unfiltered set is a compile error. A bare
  newtype would have bought nothing here: `Shown(unseen)` compiles as easily as
  the raw set. The constructor has to be the filter.
- `cap(self, …)` taking **ownership**, so the pre-cap value is moved by the call
  and cannot survive to be recorded instead of the capped one. There is no live
  binding left for a future edit to desynchronise.
- `TipSnapshot` and `ReapPlan`, which leave `reap` no second walk to reintroduce:
  it takes a plan and nothing else, and calls neither `tip()` nor
  `posts_at_tip()`.

The general rule: when the same failure recurs at a third site, stop fixing sites.
The invariant has to be unforgeable, not maintained by agreement between call
sites — because "sites agreeing until one changed" is the entire failure history
of this invariant.

## 3. `make gate` was red for about five tasks and nothing noticed

Every task verified with the board's own per-manifest test run, which is
*correct*: `tools/board/` is outside the cargo workspace, so `cargo nextest run
--workspace` does not build it. None of them ran `make gate`, and Task 9
discovered a pre-existing red — proving it predated its own edits with `git
stash` before fixing it.

**Being outside the workspace exempts a crate from the gate, not from the gate's
reach.** The workspace-wide enforcement tests in `cli/tests/` *scan repo-wide
source*, `tools/` included. So a task touching files those scans read must run
them, even when its own crate is not in the workspace. The controller's per-task
verification instructions were wrong about this for eight consecutive tasks, and
nothing in the loop could have surfaced it earlier, because every individual
verification step passed.

## 4. The same lint false-positived three times in a week, across two campaigns

`cli/tests/claim_shape.rs` decides a test iterates seeds by `lower == "s" ||
lower.contains("seed")`, so **any** closure binding named `s` reads as a seed
loop. Five new board tests wrote `.map(|s| …)` over `StoredPost`s and reddened
the gate. Independently, in the same week, `main` hit it from the same cause in
`clients/game/core/src/spread.rs` and fixed it the same way, by renaming the
binding.

Three instances in one week is a predicate that is too broad, not three careless
authors. The structural part is worse: **both offending crates live outside the
cargo workspace, so neither can run the lint that judges them without paying for
a full gate.** That is now plausibly decision-worthy — either narrow the
predicate or give out-of-workspace crates a cheap way to run the repo-wide
enforcement scans. Captured as `PROC-claim-shape-s-heuristic`. Worth noting what
the right instinct looked like when it happened: the fix renamed the bindings
rather than tagging non-seed-looping tests with a fake claim shape. Declining to
game a linter is the cheap half of keeping it meaningful.

## 5. A fallback's direction cannot be judged at the function boundary

Two textually identical `unwrap_or_default()` calls, in the same task, written by
the same hand:

- on `changed_paths`, genuinely safe — fewer path filters means *more* posts reach
  the reader;
- on `unseen`, catastrophic — an empty set means nothing is rendered, which means
  the cursor advances over posts nobody saw, which means every post pending at
  that moment is permanently seen.

**Fail-open versus fail-closed is a property of the caller, not of the
function.** In isolation the two calls are indistinguishable, which is why the
implementer's worst-failure pass — which was conscientious and explicitly asked
for — cleared the second one. It cannot be caught by looking at the fallback; it
is caught by tracing the value to what consumes it and asking what that consumer
does with an empty answer.

The habit that follows: for every defaulted error, name the consumer and state
the consequence in a comment or a doc line. `unseen`'s doc comment now says that
an empty return means "genuinely nothing new" and never "could not tell", which
is the distinction the bug erased.

## 6. Verify-don't-assume paid three times, all three against the controller

Not against the implementers — against the plan's own stated facts:

1. **"The tool must be `std`-only."** It need not be; `tools/digest/` already
   depends on `serde` and `serde_json`, the workspace allowlist does not reach
   outside the workspace, and round-tripping unknown fields untouched is one line
   of `serde_json::Value` against an error-prone hand-rolled parser.
2. **"The repo's `clippy.toml` cannot reach an out-of-workspace crate."** It
   does. **Clippy's config search and cargo's manifest search are independent**:
   the empty `[workspace]` table stops cargo walking up to an outer workspace and
   does nothing to clippy, whose lookup walks up from the crate directory and
   finds the repo-root `clippy.toml`. So the workspace-wide `disallowed-types`
   bans bind every tool under `tools/`, and an exception needs the same scoped,
   commented `#[allow]` it would need inside.
3. **"The ambient render will be fast."** 0.57 s with two posts on the board, and
   the cost is subprocess spawns rather than board size — then the scaling claim
   was wrong twice more, in opposite directions, before the ground truth
   (`10 + N + C + 2·U` spawns, growing on *both* axes) was written down.

All three were presented as facts in a plan's constraints section. The rule the
project already has covers this exactly, and the point worth keeping is that
**the controller's own assertions are the least reviewed text in the loop** —
implementers are reviewed, reviewers are re-reviewed, and the plan's prose is
read as ground truth by everyone downstream.

## 7. Two of this campaign's own verification harnesses were broken

Both in the direction of the answer they gave, which is the only direction that
matters:

- The merge-property probe printed `CONFLICT` for its first two arms from a
  harness that had never successfully built a tree — `git mktree` rejects any
  path containing a slash, and `posts/<id>.json` contains one. The output looked
  like a measurement of git's merge behaviour and was a measurement of the
  harness. Re-run through a real index, the arms reversed.
- A duplicate-id check used `uniq -d`, which **exits 0 when it prints nothing** —
  a check that passes whether or not the property holds.

This is the campaign's own recurring theme aimed at itself: a check that cannot
fail. It is also why every fix round in this campaign carried a **mutation
check** — remove the guard, confirm the *new* test goes red and the old ones stay
green, restore, confirm green. That protocol caught real vacuity twice (Task 2's
attribution guards had no test that exercised them; a "retry evidence" comment
claimed something the chain-length assertion could not show) and cost a few
minutes each time. A probe's first duty is to fail when it should.

## 8. The decision-number re-check fired for real

The campaign predicted 0114 and wrote it into CLAUDE.md, the spec, and the plan.
`origin/main` already held 0114 through 0117, from two campaigns that closed
while this one ran. The record landed as **0118** and every citation was
corrected at the merge.

It was caught only by checking `origin/main` rather than the branch's view of
`docs/decisions/`. This is the known failure of comparing against a local ref
(`PROC-preflight-resolves-local-main`), and the cost of missing it is not
cosmetic: a decision record is append-only, so a citation inside one cannot be
corrected later, only superseded. The check is one `git ls-remote`-cheap
`git log origin/main -- docs/decisions/` away.

## 9. Stage-boundary absorption was missed, and the clean merge was luck of territory

CLAUDE.md requires absorbing `main` at every plan-stage boundary. This branch's
first meeting with `main` was at close, **71 commits later**. It merged cleanly —
three conflicts, all mechanical: a `.PHONY` union, the hand-maintained decision
index, and a generated file resolved by regenerating it rather than reconciling
the hunk.

That outcome was **territory, not diligence.** The two campaigns that closed
underneath ran in `clients/game` and `windows/worldgen`; this one is
`tools/board` plus scripts and docs. Had any of the three shared a file, a
71-commit merge is where the semantic collision would have surfaced — which is
the exact failure this campaign built a tool to prevent, occurring in the
campaign that built it.

Note the compounding detail: local `main` was itself 71 commits stale, and
`make preflight` compares against the **local** ref. Running it before the pull
would have returned a confident GO derived from a two-campaign-old picture.

## 10. A red run entered the cost ledger as a cheap green one

`scripts/timed.sh` appends a row to `docs/timings.md` whether the command passed
or failed. So the gate that fail-fasted at test 44 of 3283 landed as
`| gate | 24.238 | … |`, in the file CLAUDE.md calls the archaeology of how the
suite's cost moved and which `make ci`'s baseline reasoning depends on. A red run
entering as a cheap one makes the gate look **faster** than it is, which is the
dangerous direction.

> **CORRECTION (The Radiation, 2026-08-10).** The correction below is itself
> half wrong, and the half that is right was right for the wrong reason. Checked
> against `scripts/timed.sh` on the merged tree:
>
> - **`timed.sh` does NOT record `rc`.** Its append is a single `printf` with
>   eleven `%s`, and the eleven values are date, label, real, user, sys, ratio,
>   `HV_CENSUS_WAITED_S`, commit, branch, hostname, cores. `rc` is computed
>   (`rc=$?`), returned, and echoed to **stderr** in the `(recorded) rc=$rc`
>   line — never to the ledger. The header's eleven columns match the writer
>   field for field. The original lesson was right.
> - **The stale column map is real, and this correction found it** — but it is
>   not `rc` occupying `$8`. It is **`waited_s`**, inserted after `ratio`, which
>   shifted commit from `$8` to `$9` and was never reflected in the map. The
>   summary printed `$10` (branch) where it meant host and `$8` (waited_s) where
>   it meant commit: a row whose true `host@commit` is `ambrose@1931a904`
>   displayed as `campaign/the-cairn@0`. Verified by running it, both before and
>   after; fixed in the same commit as this note.
>
> So there were two independent defects in one file and each campaign found one.
> The Cairn found the stale map and misattributed it; The Range found the
> missing `rc` and missed the map. Both were reading an artifact rather than the
> writer — the header in one case, the summary's own comment in the other. The
> lesson this correction states is exactly right; it simply applies to the
> correction too.

**This lesson was itself written wrong the first time, which is the sharper half.**
The original text asserted the ledger "has no `rc` column". Checking at close:
`timed.sh` does capture `rc`, and every row carries it — so the obvious remedy was
already in place and the finding as filed would have sent a future campaign to
implement something that exists. What is actually broken is subtler and survived
the miscount: the summary's column map is **stale relative to the row format**. Its
comment and its awk both read `$8` as the commit and `$10` as the host, but `rc`
now occupies `$8` — so `make timings` prints rc where it means commit and branch
where it means host. And the median does not filter on `rc`, so a failed run's wall
time is folded into the baseline a later run is judged against.

Two things worth carrying. A row was relabelled by hand rather than deleted, since
deleting rewrites a real measurement — but a hand fix is not a mechanism, hence
followup F18. And a finding written from a plausible inference rather than from the
script was wrong in the direction that matters: it named a remedy already shipped
while missing the live defect underneath. That is this campaign's own
verify-don't-assume rule, failing on the campaign's own retrospective, at the last
possible moment to catch it.

The asymmetry that makes the underlying defect worth fixing: `ci-record` already
refuses to write its baseline on a red run; `timed.sh` has no such discipline.

## 11. An external implementation shipped mid-campaign, and the answer was narrowing

Claude Code's cross-session messaging landed after Task 6. The honest response
was neither to abandon the campaign nor to wave the feature away: it subsumes
exactly one capability (directed question and answer between two *live*
sessions, which it does better), it does one thing this design does not do at all
(live push into a running session — a real gap, since a hold-off posted
mid-session reaches nobody until their next start), and it cannot reach
durability, unaddressed posts, path routing, the technique corpus, or — the
decisive one — **across accounts.** So the overlapping convention was kept and
sharpened rather than dropped, and an integration was specified: when a question
is answered over the wire, post the answer back as a durable reply.

The process lesson is about the first draft of that assessment, which proposed
dropping the overlapping feature outright and was pushed back on. **A capability
that a better tool subsumes is not necessarily a capability to delete** — ask
first what the two tools do *not* share, because that is where the surviving
design lives.

Worth recording alongside it: three independent convergences on this design in
one campaign. An existing git-native issue tracker had made the same substrate
bet (objects in the repository, not files in the tree). The accidental board
reported in the wild had validated the medium, including that it was rebuilt out
of directory names after being shut down. And this feature's safety rules — a
message cannot approve, cannot change configuration, commands in it do not run —
are two of this spec's own decisions arrived at independently. Convergence is not
proof, but three of them on the same three points is a reason to stop
relitigating those points.

## 12. A subagent's self-assessment can be too charitable and still be conscientious

The implementer in lesson 5 was explicitly asked for its worst failure mode,
answered thoughtfully, and cleared a genuinely catastrophic fallback — because at
the function boundary it looked identical to a safe one three lines away. The
same pattern appears elsewhere in the ledger in the good direction: one
implementer reported a structural hazard instead of quietly redesigning around
it, another generalised a lesson and flagged the *next* instance unprompted.

So the fix is not distrust; it is a better question. **Ask for the failure mode's
direction, not merely whether one was considered**: if this returns the default,
who consumes it, and does the consumer then do less or more than it should?
"Would a failure here lose data or duplicate it?" is answerable at the function
boundary. "Did you consider failure?" is not.

## Follow-ups (promoted from the campaign's scratch register)

`.superpowers/sdd/` dies with the checkout, so these are the durable copy.

**F1–F8 were briefly believed lost, and the reason is a lesson in its own right.**
The close agent found the register beginning mid-file and no trace of F1–F8 in the
scratch, the plan, or the spec. They were not lost — they were **split across two
files**. This campaign wrote its early scratch to the flat `.superpowers/sdd/`
path, then the workspace resolver moved it to a per-plan
`.superpowers/sdd/<plan>/` directory, and every later append went to the new
location while the old file sat there unread. Nothing was deleted and nothing
warned; a mid-campaign path change silently forked the record, and the second half
looked like the whole of it.

That is worth more than "scratch dies with the checkout", which the project already
knows. The sharper rule: **when a tool relocates a scratch file mid-campaign, the
old path keeps its contents and stops being read.** Check the ancestor path before
concluding anything is gone — and note that the campaign whose *second job* is
preventing expensive rediscovery nearly lost eight of its own findings this way.

- **F1 — The ~75% rung: route-by-path at the touch point.** *Trigger:* a
  `PreToolUse` hook warning when a session edits a path another session posted a
  `hold-off` on. Deferred from v1: it adds an interrupt surface and a second
  failure mode, and `scripts/hv-guard-bash.sh` already occupies that seam. The v1
  relevance filter delivers most of the value at render time instead.
- **F2 — Cross-host operation, and the merge model it needs.** *Trigger:* the first
  time a claim or notice genuinely needs to cross machines. Compare-and-swap
  serializes writers within one ref store only, so two clones that both write and
  push diverge and CAS gives nothing there; the problem is divergent-history merge,
  and git-bug's operation-log model is the prior art. Steal the design, not the
  dependency. (Superseded in part by **F15**, which names the dependency question
  that gates the census-claim read.)
- **F3 — Subsume decision 0081's `/tmp` census claim.** *Trigger:* the board
  proving itself reliable across several campaigns. One claim mechanism would be
  tidier, but it would put a new tool in front of the guard protecting census
  writes.
- **F4 — `git notes` annotation of landed changes.** *Trigger:* additive interest.
  The substrate rejected as primary is genuinely good at the one thing it does —
  anchoring a note to the commit that caused it — so a notice about a *landed*
  change could migrate to `refs/notes/board` and gain union-merge for free.
- **F5 — Acknowledgement semantics for `hold-off`.** *Trigger:* the first time a
  `hold-off` is ignored and costs a merge. Only the negative polarity is
  meaningless without an ack; v1 ships no `ack` verb at all, and acknowledgement is
  expressible today as a `reply`. See **F11**: the delivered/held/refused
  vocabulary supersedes this entry's original binary framing.
- **F6 — Evaluate git-bug properly if that decision is ever reopened.** *Trigger:*
  reopening it. Only its README was read; the reversal cost was zero when nothing
  was built and rises with every task shipped.
- **F7 — Measure the render's real steady-state volume** (spec assumption 1).
  *Trigger:* the render cap being hit routinely. The ≤ 15-line / ≤ 1 KB budget is a
  budget, not a measurement, and if it is hit the diagnosis differs by cause —
  either the content rule is being violated or the relevance filter is too loose.
- **F8 — Replay the campaign's motivating collisions against the finished tool**
  (spec assumption 3). *Trigger:* it can be run today, against history, rather than
  waiting for a new collision. Would a `hold-off` naming the colliding paths have
  reached the other session in time during The Tumult/The Waterline, or The
  Actants? This is the only real test of whether relevance-by-changed-paths matches
  the collisions the board was built for, and it is the assumption the whole design
  rests on that remains unmeasured.

- **F9 — Push urgent `hold-off` notices over the wire.** *Trigger:* the board
  renders only at session start, the self-map, and preflight, so a notice posted
  mid-session never reaches a session already running. Deliver a path-matching
  hold-off immediately via the messaging wire; the board stays the ledger and the
  wire becomes its delivery arm. Mechanism note: the session's own inbox socket is
  exported to hooks as `CLAUDE_CODE_MESSAGING_SOCKET`, `SessionStart` included.
- **F10 — Post the durable `reply` after a wire answer.** *Trigger:* D12c makes
  this a convention and nothing enforces it. The digest already lists asks with no
  reply; consider distinguishing "no reply" from "answered elsewhere, never
  written down", which is the failure the convention exists to prevent and is
  currently invisible.
- **F11 — Adopt delivered / held / refused for acknowledgement.** *Trigger:* F5
  framed acknowledgement as binary. Three outcomes, with the sender told which,
  is the better vocabulary — worth taking if F5 is ever built.
- **F12 — A per-session inbound control for the ambient render.** *Trigger:*
  the messaging feature's `accept|hold|refuse` analogue. Let a session opt out of
  the ambient render, or take a count-only summary, without editing the hook.
  This is the pressure valve if the context budget ever proves too tight.
- **F13 — A flood ceiling.** *Trigger:* nothing bounds how many *distinct* posts
  one runaway author can append. Content addressing dedupes identical repeats
  forever and the render cap protects readers, but the board's size is unbounded.
  Probably a digest-visible warning rather than a hard limit, since a hard limit
  could silently drop a real warning.
- **F14 — Batch `LiveContext::probe`'s per-author git calls.** *Trigger:*
  measured render cost of ~0.57 s with two posts, dominated by subprocess spawns,
  scaling on both the post axis (a `cat-file` each, a `ps` per local claim) and
  the author axis (a resolve plus a `merge-base` per distinct author, and authors
  accumulate permanently while posts get reaped). One `for-each-ref` for the
  branch set, one cached `hostname`, and a negative cache for unresolved authors
  should collapse most of it. The 2 s budget makes this bounded, not fixed.
- **F15 — Render the census claim, once the dependency question is settled.**
  *Trigger:* spec §5 promised v1 reads `hornvale_lab::census_claim::current_holder()`
  and nothing does. The implementation is not the hard part — the **dependency
  shape** is, and it is a decision: depend on `hornvale-lab` (and make the board's
  read seams die with a red workspace, defeating its whole placement), read the
  claim file directly (duplicating a format `windows/lab` owns), or decide the
  render is redundant and amend §5 to a non-goal. Choose first, then implement.
- **F16 — Arbitrate the durable/ambient tension.** *Trigger:* the render-cost
  measurement. Durable technique posts are never reaped and are exactly what
  drives the ambient render past its latency ceiling; F14 buys headroom, not a
  rule. Candidates: cap the ambient render's *sources* rather than its size
  (technique in the digest only), or age-weight relevance so a durable post leaves
  the ambient render while staying in `board read`. The campaign's most
  interesting unresolved design question.
- **F17 — Automate compaction, after re-reading why it was not automated.**
  *Trigger:* nothing runs `board reap`, so the tip tree grows without bound and
  F14's costs worsen over time. The reap path is where the fourth silent-loss
  instance lived; the fix wave closed it structurally, so the ground is now safe,
  but whoever automates an operation with permanent consequences should read
  `store.rs`'s `TipSnapshot` doc comment first. A preflight-time reap is tempting
  and wrong — preflight runs at integration, when other sessions are most likely
  to be posting.
- **F18 — the timing ledger's summary reads the wrong columns, and its median
  counts failed runs.** *Trigger:* lesson 10, found the hard way — then **corrected
  at close, because the finding as first written was wrong.** `timed.sh` *does*
  capture `rc` and the rows *do* carry it, so "add an `rc` column" was already
  done. Two real defects survive that correction. First, the summary's column map
  is stale: its comment and its awk both read `$8` as the commit and `$10` as the
  host, but `rc` now occupies `$8`, so `make timings` prints **rc where it means
  commit and branch where it means host**. Second, the median does not filter on
  `rc`, so a failed run's wall time is folded into the baseline a later run is
  judged against — which is how a red 24 s gate made the gate look faster. The
  asymmetry that makes it worth fixing: `ci-record` already refuses to write its
  baseline on a red run; `timed.sh` has no such discipline. Not fixed here — it is
  pre-existing shared tooling, and the merge point is the wrong moment to touch it.
- **F19 — Settle whether an async `SessionStart` hook's stdout reaches context.**
  *Trigger:* the hooks documentation says session-start stdout becomes context and
  separately that `async: true` runs in the background, and is silent on the
  interaction. The board's hook is synchronous *because* of that gap and pays
  ~0.57 s per session start for it. Cheap to settle with a throwaway hook that
  writes a marker — and worth doing **before** F14, since a positive answer makes
  F14 unnecessary. Cannot be settled from inside the session that asks: verifying
  it requires inspecting a *new* session's context.
  *(Renumbered from F15 at close. Two entries were independently given F15 — the
  duplicate-`TOOL-24` hazard exactly, where one ambiguous id travelled through a
  spec, a plan, a study JSON and a decision. This one moved rather than the other,
  because the other is cited from **append-only decision 0118** and from spec §5,
  and a citation inside an append-only record cannot be corrected later, only
  superseded. **Renumber the uncited one; never the cited one.**)*

Two smaller findings that are not fixes, recorded because a future feature would
have to know them: `posts_at_tip` orders by `(committed_at, id)`, so **within one
second the order is by content hash** — arbitrary, and both "oldest" and "newest"
are second-granular concepts on this board. And **a notice posted from a branch
sitting at `main`'s tip is dead on arrival**, because that branch is an ancestor
of `main` and so reads as merged: a session's very first act on a fresh branch
cannot be a hold-off anyone sees.

**One loss to record honestly.** The scratch register's first eight entries
(F1–F8, captured during spec and plan — the plan's own Definition of Done names
them) did not survive in the file; only F5's gist is recoverable, from the plan's
note that it deferred `ack` semantics and spec §9 A5's note that
delivered/held/refused supersedes its binary framing. That is precisely the hazard CLAUDE.md names about
`.superpowers/sdd/` — promote findings *before* teardown or they are gone — and
this campaign lost eight of its own to it while writing the tool whose second job
is to stop exactly that.
