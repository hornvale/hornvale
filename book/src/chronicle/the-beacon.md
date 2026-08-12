# The Beacon

A cairn is legible to whoever stands beside it. This campaign made the
project's own cairn — the append-only board built by [The Cairn](the-cairn.md)
— legible from a second machine, and it cost more than the design expected to
find that out.

## What crosses the wire now

The board's storage did not change shape. What changed is where a session
looks for it. `board sync` publishes this host's log to a per-host mirror on
`origin` (`refs/hornvale/hosts/<host>`, fast-forward only, never forced) and
fetches every other host's mirror into a local shadow
(`refs/hornvale/peers/<host>`). A read is the union of this host's own log and
every peer mirror **except its own** — skipping the self-mirror is not
tidiness, it is what stops a host's own compacted-away posts from being
fetched back and resurrected the moment a `reap` outruns a push.

A post arriving from a peer ref is judged differently than a local one, and
deliberately so. **Foreignness is a property of which ref a post was read
from, not a field stamped on the post** — measured across the 32 posts on the
board at the time, zero of them carried a `host` field, so a predicate reading
one would have classified every peer post as local and done nothing. A
foreign post is instead judged by its time-to-live alone, never against this
host's own process table or branch state, and rendered as unverifiable here.
That asymmetry is the whole reason the render's peer header carries two ages,
not one: how long since *this host* last fetched a peer's mirror (sync age),
and how long since *that peer* last posted anything at all (content age). The
two fail in opposite directions — a stale mirror behind a busy peer means we
are out of touch; a fresh mirror behind a silent peer means they are gone —
and only the second is the signal that a retired host has gone quiet forever
while still rendering as current.

Two write-side features round out the surface. `board redact <id>` evicts a
post from a host's own tip and propagates a suppression that every read seam
honors from then on; the digest still reports that a post existed and who
redacted it, because the corpus records the act rather than pretending it
never happened. Distributed byte-level deletion was measured and rejected — a
canary secret force-pushed out of a probe ref was still served, blob and
commit both, by its object id, so a rewrite breaks the one thing the board's
push discipline buys (fast-forward) and still fails to remove the bytes from
any clone that already has them. `board post` also now refuses obvious
credential shapes on the way in, on the theory that once a secret is committed
prevention was the only control that ever worked; the false-positive rate
measured against fourteen credential-adjacent sentences was three in
fourteen, all in the expensive direction (prose *about* a token, not a live
one), which is recorded as designed behavior rather than a defect to chase.
Corroboration — `confirm` and `stale` — shipped as a version-2 convention
post, tallied in the digest, so a technique someone else verified or found
wrong leaves a trace without needing a vote.

None of this changed the cadence at which a session may touch it. **The lane**
(decision 0129) lets any checkout commit board changes — a new post kind, a
render tweak, a relevance rule — without campaign ceremony, so the surface
stays cheap to extend. Three operations stay off the lane deliberately:
`reap` (the one destructive operation), the CAS/append path (where a bug means
silent write loss), and the sync/push path (where a bug would violate the
board's never-rerooted guarantee across hosts). The lane lowers ceremony,
never review.

## `retract` and `redact` are distinct in two places and identical in two more

The spec was explicit that these two controls must not collapse into one
meaning: `retract` says *I withdraw this claim, the post was wrong*; `redact`
says *this content must stop being displayed, and the post may be perfectly
true*. Task 8's review went and checked, by constructing three posts in a peer
log — one retracted only, one redacted only, one both — where eviction cannot
paper over the difference because a peer log is read-only from here.

The finding was not reassuring in the way a retrospective glance might expect.
**The digest and the store keep the distinction; the render and the reap path
do not.** A retracted technique is still published in full to the human
digest, with the retraction noted; a redacted one is suppressed and the act of
redaction reported instead. In the store, `redact` evicts a post from the tip
tree and `retract` is pure data, never touching storage. But at the
`live_posts` render seam and at the reap-eligibility check, the two are
identical — both are simply non-`Live`, both are equally eligible for
compaction, and nothing distinguishes them. Whether that gap is a defect
depends on a fact that turns out to be unobservable today: the only caller of
the liveness computation collapses it to a boolean (`matches!(_, Liveness::Live)`),
so no seam ever prints *which* reason a post stopped being live. Collapsing
the digest's or the store's separation would show up immediately in behavior;
collapsing the render's or the reap path's would not, because nothing today
reads far enough into the value to notice. So: half of this separation is
load-bearing, and half is currently decorative insurance against a seam that
does not exist yet. It was kept rather than simplified away, on the argument
that the insurance is cheap and the day something does print the reason, the
half that looks decorative today stops being so.

## The measurements, including the one that failed

Every number below is real, and one of them is a falsified prediction rather
than a success — recorded as such rather than softened, per this project's
standing rule that a preregistered result which misses its target is a
finding, not a defect to quietly fix before anyone reads the chronicle.

**The per-post object read, batched, is a 20–33× win, and it held at
cross-host scale.** The render's dominant cost was never subtle: roughly
twenty-six of the thirty-six subprocesses a render spawned at the time were a
`git cat-file -p` invocation per post, one spawn each. Replacing the loop with
a single `git cat-file --batch` took the same single-host, 32-post render from
**1.69–2.05 s to 0.60 s**, comfortably inside the spec's original ≤ 1 s
single-host budget. The board's separate human-facing digest, which walks
history rather than the tip and pays its own unbatched `cat-file` loop, was
the worse offender by construction — its N is sized by the history window, not
the tip, so it can run far larger than the render ever does. Fixed the same
way, a 450-post fixture (300 tip, 150 retracted-and-reaped) went from
**13.4 s to 0.4 s, roughly 33×** — independently reproduced at 600 posts by a
reviewer at a consistent ~32×.

**The cross-host render missed its own budget, by a structural margin rather
than a regression.** The spec set ≤ 1 s at 100 posts across two peers, before
the union or peer-status reporting existed to measure against. At close, on a
real two-host board (38 posts, 30 rendering, one live peer),
`scripts/board-render.sh` measured **1.29 / 1.44 / 1.39 / 1.36 / 1.51 s** —
every run over budget, at under a third of the post count the budget was set
against. The batching held; what the original number did not anticipate is
that going cross-host roughly *doubles* the fixed cost rather than adding to
it linearly: the union reads two refs instead of one, each paying its own tip
resolve, tree listing, time-map and batch, and peer-status reporting adds
roughly four more calls on top. It is explicitly **not** the term the
predecessor campaign's F14 finding aimed at — measured directly with a
throwaway invocation-logging shim, the unresolved-author term came in at
**U = 0** at close, every notice author resolving cleanly. And it is not an
operational failure either: the seam's real ceiling is the pre-commit hook's
2 s timeout, which 1.29–1.51 s clears with room. What missed is the spec's own
self-imposed number, written before the shape of the cost that would end up
dominating was known. The honest read, stated in the spec itself rather than
adjusted after the fact: ≤ 1 s was the wrong target, set before the union and
peer status existed to be measured; a cross-host render costs about what a
single-host render costs, twice, plus a small per-peer tax, and closing that
gap is a design question — most plausibly caching the peer-status block
between renders — that this campaign did not open.

## The incident

Task 11 landed a hook rule (decision 0129's B13): a commit touching only
`tools/board/` now runs that crate's own test suite at commit time, so a
board-only change cannot land untested the way nothing else in the
knowledge-architecture stack currently is. Task 11b is the story of what that
rule actually did the first time it ran from a linked checkout, which is how
every campaign in this project does its work.

`git -C <dir>` sets the working directory a git invocation runs from. It does
not decide which repository that invocation acts on — `GIT_DIR` outranks it
unconditionally, and a linked checkout (unlike a primary checkout) exports an
*absolute path into the real repository* as `GIT_DIR` to every process it
spawns, hooks included. The board's test suite builds its fixtures in a fresh
temporary directory and calls `git -C <tempdir> ...` against it, which is a
correct and ordinary way to isolate a test — except that the hook rule handed
every one of those invocations an inherited `GIT_DIR` that pointed at the
developer's actual repository, and `-C` could not override it. A second,
independent leak sat beside the first: one code path resolved a ref path with
`git rev-parse --git-path` and then wrote to it directly with `std::fs::write`,
which needed no git invocation at all to reach the real repository once
`git-path` had already answered from the wrong one.

The damage was not simulated. The developer's real repository had its
`core.bare` flag flipped to `true` (a linked checkout's `.git` file does not end in
`/.git`, so `git init`'s repository-type guess landed on bare), its
`user.name`/`user.email` rewritten to a test fixture identity, three stray
branches and several stray commits created under that identity, and a
dangling ref written into the real ref store that broke `git fetch` across the
entire repository. The real board log itself was contaminated: 46 of 81
reachable commits at one point carried the test identity, and roughly a third
of the posts at the tip were test fixtures rather than real board traffic.

It was found by forensics, not by the test suite going red — the suite passed
throughout, because every assertion it made was still true of *some*
repository; it was simply the wrong one. The fix scrubbed every
location-and-identity-bearing `GIT_*`/`GIT_AUTHOR_*`/`GIT_COMMITTER_*`
environment variable before any git invocation adds its own `-C`, collapsed
two raw spawn call sites into that one scrubbing constructor so the fix could
not be half-applied, and added a belt-and-suspenders `env -u` prefix to the
hook's own invocation line. Repair of the already-contaminated board used the
board's own cross-host machinery as an oracle: `origin`'s pre-contamination
mirror of this host's log, fetched before the incident and therefore
provably clean, was diffed against the corrupted tip to confirm that every
genuine post — thirty-three of them — survived, and the fixture noise was
dropped in one forward commit rather than by rewriting history, which
decision 0118 forbids regardless of the reason. A concurrent session in its
own linked checkout, sharing exactly the vulnerable configuration, was warned
by a `technique` post before it could be bitten by the same defect — the
board doing, for the first time this campaign observed it, the second job it
was built for: carrying an operational fact to someone who was not part of
the conversation that discovered it.

The transferable lesson is stated plainly because it generalizes past this
one bug: **a temporary directory is not isolation when the environment names
the repository.** No amount of care inside a test can substitute for
controlling every channel through which a repository's identity reaches a
child process — and a linked checkout, the normal way this project's own
campaign work is checked out, is exactly the configuration that turns an
ordinary-looking `-C` call into a live wire.
