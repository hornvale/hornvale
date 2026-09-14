# The Watchman — decision ledger

Campaign slug: `the-watchman`. Subject: a running sluice *drainer* (the loop
that pops queue rows and dispatches them) is invisible to every queue
instrument, so two operators can unknowingly run competing loops.

Provenance: on 2026-09-13 the session operating the queue chained its own
drain and collided with a standing drainer a previous session had left
running. Its script's own header read, in capitals, `ONE LOOP ONLY. A second
waiter races this one for the same queue.` It could not be seen.

---

#1 [Q] — **Was the collision actually harmful, or only untidy?**
· **Decision:** harmful in one specific way, and NOT in the way it first
appeared. · **Why:** measured rather than assumed. The `flock` on
`/tmp/hv-census.lock` serialized the work correctly — one holder
(`sluice-run.sh req-1a21e863d825`), one waiter (`req-799ad4e2a7f4`, whose only
child was `flock -w 7200 9`) — and `sluice-queue.sh claim` is atomic, so two
simultaneous claims took two *different* rows rather than one row twice. Worktree
integrity held too: `sluice-run.sh` takes the flock at line 707 and does not
touch the shared chamber worktree until line 788. So the *mechanism* was sound.
What the mutex cannot absorb is **policy**: the standing loop deliberately
refuses to auto-drain merges, and the new one drained three. Decision 0133 is
vindicated for work and silent about loops. · **Alternatives discarded:** "it
was harmless, do nothing" — rejected, because two correct loops with different
policies produce an incoherent queue with a green light the whole way.
· **ideonomy passes / overturns:** 1 / 1 (the pass overturned "the problem is
two loops" into "the problem is two *policies*"). · **Capture:** board post
`747a8e28d296`; memory `check-for-an-orphaned-drainer-before-starting-one`.

#2 [G1] — **What mechanism makes a drainer discoverable?**
· **Decision:** a non-blocking `flock` **per queue KIND**, held for the life of
the loop, with a human-readable registration file beside it and a DRAINERS
block in `make sluice-status`. · **Why (precedent):** `windows/lab/src/census_claim.rs`
already solves the neighbouring problem this way, and its `contending_holder()`
work is the repo's own record that liveness-by-ancestry beats
liveness-by-assertion. CLAUDE.md's standing rule — *"if the answer to 'what
stops this recurring?' is a person, it isn't in code"* — rules out anything
advisory. · **Alternatives discarded:**
  - *pidfile + status* (the shape approved in conversation): **lies in both
    directions.** It outlives a SIGKILLed drainer and a drainer killed before
    cleanup leaves a file asserting life. An `flock` cannot go stale — the
    kernel releases it on death. Adopting it would be a second, worse
    derivation of a problem `census_claim.rs` already solved.
  - *single exclusive lock, one drainer per box*: rejected on evidence from
    this very session — a merge-only dispatcher running beside a stage/census
    loop was **correct** and useful. "One drainer per box" is the wrong
    invariant; "no two loops claiming the same kinds" is the right one.
  - *derive from the process table / from the queue row's claim reason*:
    cannot go stale, but carries no policy and is invisible where operators
    actually look (`make sluice-status`).
  - *systemd unit*: one per box by construction, but heavyweight, carries no
    policy, and `scripts/scheduled/README.md` records that this repo's last
    systemd-timer install step was never performed on lefford.
· **ideonomy passes / overturns:** 1 / 2. Tuple: negation + organon-construction,
matrix organon, dimension prompts naturalness / purpose / discovery-vs-invention.
Overturn 1: per-POLICY rather than per-drainer locking (from negating "one record
per drainer"). Overturn 2: **per-KIND locks rather than comparing policy
strings** — one `flock` per kind a loop claims makes overlap detection exact and
free, with no csv canonicalisation and no string comparison to get wrong.
· **Capture:** this entry; matrix retained in the campaign spec.

#3 [G1] — **Where does the loop itself live?**
· **Decision:** **in the repository**, as a `watch` mode of
`scripts/sluice-drain.sh` — not in an operator's scratchpad. · **Why:** this is
the decisive finding and it is not a new argument; it is `sluice-drain.sh`'s own
header applied one level up. That header records that the *dispatch*
orchestration "lived in an operator's session scratchpad for weeks, ungated and
untested, while doing real gating work. It was the direct cause of two defects
in one night (2026-08-27), so it lives here now." The *watching* stayed outside
and reproduced the identical failure exactly. · **Mechanically forced, too:**
today's standing drainer is a shell `while true` that calls `sluice-drain.sh 1`
repeatedly, so a lock taken *inside* `sluice-drain.sh` would be acquired and
released per job — two such loops would take turns and never see each other. A
registration that does not span the loop's sleeps is not a registration.
· **Alternatives discarded:** a wrapper script operators are asked to use
(same scratchpad problem, one directory over); documenting the hazard in
CLAUDE.md only (the rule-enforced-by-hand test fails it).
· **ideonomy passes / overturns:** covered by #2's pass, which produced this
as an implication of "consulted continuously" rather than "consulted before
starting". · **Capture:** this entry.

---

## Follow-ups

- The standing drainer left by the previous session is still running and still
  unregistered. Consolidating onto `watch` mode means stopping it; that is an
  externally-visible action on another session's process and is deliberately
  NOT auto-resolved here. Carve-out: bring to Nathan.
- `make sluice-status` reads `queue.tsv` over ssh with `cat`. Testing lock
  liveness remotely needs `flock -n` in that ssh command; confirm lefford's ssh
  environment has it on `PATH` non-interactively before relying on it.
- `$HV_SLUICE_DIR` already holds `queue-watch.state`, from an earlier watcher
  (`scripts/queue-watch.sh`). Check whether that watcher and this one should
  share a registration surface rather than accreting a second one.

---

## Execution — complete

**One thing was added to the design by running it, not by thinking about it.**
The spec had locks, registration and a status block; it had no orphan guard.
Then a test invocation — `timeout 6 make sluice-drain` — left a watch loop
alive at **20 minutes**, orphaned, holding locks. My own tooling produced the
exact hazard this work exists to remove, in front of me, ten minutes after I
wrote the comment describing it.

So `is_orphaned()` was added: the loop checks whether it has been reparented to
init and exits, saying why and releasing its locks. `--allow-orphan` opts out
for deliberate daemonising, because a nohup/systemd drainer has ppid 1
legitimately and must not be killed by this. Visibility and stopping are both
wanted and they are different properties: the locks make such a loop *visible*,
which is what a reader needs; this makes it *stop*, which is what the box needs.
No ideonomy pass was run for this addition — it was forced by an observed
failure, not selected from alternatives.

**Three defects in my own work, each caught by something executable.**

1. `policy_is_valid` refused **every** policy, including the real ones. `local
   IFS=,` was still in force for the inner loop over the space-separated
   `DRAIN_KNOWN_KINDS`, which therefore iterated once over the single word
   "merge stage census" and matched nothing. All four *"refuses a bad policy"*
   assertions passed while it did so. Only the positive control — "real
   policies validate" — could see it. A drainer that can never start would
   have shipped behind four green assertions.

2. The orphan **control** orphaned its own drainer. It was written as
   `( cmd & echo $! > file )`, and that subshell exits the instant it has
   echoed. The control then failed against a guard that was working correctly.
   A control has to be built as carefully as the thing it controls.

3. I verified the orphan guard with `pgrep -f` and got a false negative,
   because the pattern matched the *test harness's own command line*. Re-checked
   by PID. This is already written down in memory as a rule and was walked into
   anyway; the general form is that `pgrep -f` cannot distinguish a process from
   a process that merely mentions one.

**And one claim I nearly asserted without checking**, while attributing an
unrelated red in the same session: I read a token diff showing only
`configuration_hash` differences and was about to write "source_revision
matches, therefore a config parameter drifted." It did not match — the diff had
elided everything past the eighth difference. Checking cost one command and
inverted the diagnosis from *stale build* to *the world moved underneath them*.

## Follow-ups

- The standing drainer left by the previous session is still running and still
  unregistered. Nathan authorised the consolidation on 2026-09-13 in response to
  a direct question; the sequence is land this, then stop that loop, then start
  `make sluice-drain KINDS=stage,census` in its place. It must be in that order:
  `watch` mode does not exist on the canonical box's checkout until this lands,
  and killing the standing loop first would stop stage/census draining meanwhile.
- `make sluice-status` degrades with a named message on a checkout that predates
  this change, rather than a raw `No such file or directory`. That message goes
  stale the moment this lands everywhere; it is cheap and honest until then.
- `$HV_SLUICE_DIR` is now exported by `sluice-drain.sh`. It was previously a
  plain assignment, so it agreed with its children only by coincidence — every
  one of them recomputes the same default independently. Worth checking whether
  the other sluice scripts have the same latent half-redirection.
