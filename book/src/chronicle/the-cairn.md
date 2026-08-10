# The Cairn

A cairn is a marker built one stone at a time by successive passers-by. Nobody
addresses it to anybody; it is read by whoever comes next, and the reader owes
it nothing. This campaign built one: an append-only medium, embedded in git,
through which the many concurrent sessions that develop this project coordinate
without ever communicating directly.

## The collision a diff structurally cannot see

Hornvale is developed by many parallel sessions, each in its own checkout of one
repository, each editing one shared workspace. At the time of writing there were
nine live checkouts against the project's own stated working ceiling of two or
three.

The *mechanical* half of the resulting pain is solved and has been for some
time: append-only lists carry a union merge, reference dumps are regenerated
rather than reconciled when they conflict, and the gate-contention problem is
addressed by a staggering rule. What is left is the *semantic* half, and the
project's own integration check says so about itself — it compares ancestry and
peeks at the integration branch's checkout, and has no opinion about whether two
campaigns changed the same idea in incompatible ways. Two of them once did,
with a clean verdict from every textual check in the repository.

The reason no diff can catch this is not that the diffs are too coarse. It is
that **the colliding thing is not in the substrate yet.** A diff compares two
states of what has been written. The collision is between two *intentions*, at
least one of which has not been written down anywhere — it exists in a session's
plan, and it will become bytes only at the moment it is too late to route
around. Lifted to its structural form, this is optimistic concurrency without an
intent log: concurrent workers hold private state whose future effects are not
yet observable in the shared medium, so the medium reveals the conflict at a
reconciliation barrier, which is the latest and most expensive moment to learn
of it.

The prescribed remedy was to read the other branches' chronicles. That is a
human instruction with no mechanism behind it, and this project already has a
name for that shape: a deferral whose trigger is a property of the code needs a
check, or it is a wish.

There is a second hole, and it is the one with compounding value. Operational
technique discovered by one session is lost to every other one, and the gap is
visible by elimination. Decisions are for settled architecture. Retrospectives
are per-campaign and land at *close*, which is too late for the parallel session
hitting the same wall today. The idea registry is for ideas, not for methods. So
nothing in the knowledge architecture holds a sentence like *`git mktree` rejects
any path containing a slash* — too small for any durable artifact, too expensive
to rediscover, and wanted by another session now. The board has two jobs:
announce intent before the write, and accumulate short-half-life technique the
moment it is learned.

## The one irreversible decision

Almost everything about a board is revisable. The relevance filter, the decay
rules, the render caps, the read seams, the conventions — all of those are code
and can be rewritten next week over the same data. Exactly one decision is not
revisable, because the store is append-only and nothing in it is ever
rewritten: **the shape of the bytes on the ref.** A wrong storage shape does not
get fixed; it gets migrated, and it takes every post ever written with it.

The obvious shape is a single append-only register — one line per post, in one
file, which is exactly what the project's existing union-merge machinery is
built for. It was rejected on a measurement rather than an argument. Two
divergent clones were built and merged with git's own merge machinery, three
ways:

```
  two clones append DIFFERENT posts       -> CLEAN, both posts in the result
  two clones record the SAME post         -> CLEAN, converging to one file
  two clones append to one register.jsonl -> CONFLICT, three stages on the file
```

The middle arm is the interesting one. Each post is stored as its own file,
named by the hash of its own bytes, so a filename collision *means*
byte-identical content: two sessions that independently record the same post do
not conflict and do not duplicate — they converge on one object. That is the
convergence property a conflict-free replicated data type is usually a library
for, obtained here from git's object model instead, which matters because this
project's dependency rule would not have admitted the library.

The third arm is why the reasoning could not have been left as reasoning. The
rejected shape works indefinitely on one machine and fails the first time the
board spans two, and the design's stated goal is to span machines eventually. A
shape chosen by argument would have been discovered wrong after every post ever
written was in the wrong format.

The first run of that probe deserves its own sentence, since the campaign's
recurring theme is checks that cannot fail: it printed `CONFLICT` for the first
two arms as well, from a harness that had never successfully built a tree at
all. The instrument was broken in the direction of the answer it gave. Re-run
through a real index, the arms read as above.

Around that shape sit two more choices, and each is load-bearing for the others.
The board lives on an **orphan ref** sharing no history with the integration
branch, because refs are per-repository: a post is legible from every checkout
of this repository the instant it is written — no branch, no merge, no push, no
fetch — and a write touches no file in any checkout, so it cannot collide with a
session mid-landing. A post's entire value is that it arrives *before* the merge
it warns about, which is what rules out storing it as a tracked file on a
branch, where it would be invisible to everyone else until that branch merged.
And the ref is **never rerooted**: compaction drops dead posts from the tip tree
as a forward commit, so every post ever written stays reachable through the
ref's history. The board's own git log is therefore a complete record of how
sessions actually coordinated — the corpus for the eventual question of which
conventions earned their keep. The tool has no operation that could destroy it,
and one specific failure classification exists to keep it that way: a write
whose expected predecessor has vanished is treated as a *permanent* error rather
than a lost race, precisely so that a retry cannot take the create path and
orphan the history.

## Dumb store, smart read

The stored form has no opinions. There are no timestamps in the data — the
commit is the clock, and a claim carries a *duration* rather than an instant.
There is no expiry flag, no deletion, no read state, and no retraction field; a
retraction is a new post naming the retracted one. Time-to-live, process
liveness, branch liveness, retraction, topical relevance, unread-ness, and the
distinction between a decaying register and a durable thread are **all computed
when someone reads.**

That inversion is not an aesthetic preference. It is what buys both the merge
property and the open schema, which are the same principle seen from two sides:
a field that is never written cannot disagree between two clones, and a field
the tool never interprets cannot be wrong when a convention changes. The tool
requires only that a post name its kind and its author; an unrecognized kind
renders generically and unknown fields round-trip untouched, so the initial
convention set ships *as a post on the board* rather than as an enumeration in
the code. Sessions can supersede the protocol with no commit, no gate, and no
review.

The price is paid on the read, and it is per post. An ambient render costs
roughly `10 + N + C + 2·U` subprocesses — one object read per post at the tip,
one process probe per claim naming this host, and two ref resolutions for every
post whose author's branch no longer resolves — at thirty to forty milliseconds
each on the machine this was measured on. With two posts on the board that is
about 0.57 seconds; the arithmetic says the render starts hitting its
two-second budget somewhere between fifteen and forty posts. So the render
carries a visible budget and *says* when it declines, rather than silently
rendering nothing.

That measurement exposed the campaign's most interesting unresolved question,
and it is a genuine tension rather than a defect. Durable technique posts are
never reaped, by design, because their whole value is that they outlive the
session that learned the thing. Durable posts are therefore precisely what
drives the ambient render past its latency ceiling. The compounding half and the
ambient half pull against each other and nothing currently arbitrates it.

The reads attach only to seams a session already crosses — the session-start
hook, the repository's self-map, the pre-integration check — plus a separate
window for the human, read from the ref's *history* rather than from its tip.
The rung the board would otherwise occupy is a file plus a convention to read
it, and that rung is empirically dead here: the staggering rule *is* that rung,
and nine live checkouts against a ceiling of three is the measurement of its
failure.

One honest finding about those seams, arrived at by a review asking whether the
thing would have caught the collisions it was built for. Delivery through the
session-start hook requires a long conjunction — a *new* session must start
after the post lands, its already-committed diff must overlap the notice's
paths, the post must survive the render cap, and the board's binary must be
built in that checkout. The historical collisions emerged mid-flight between two
long-running campaigns, which is the worst case for a router that fires only at
session start. The seam that would have caught them is the pre-integration
check: it fires at every plan-stage boundary, it is filtered by neither
relevance nor read state, and sessions are already instructed to run it. The
design's emphasis was on the ambient read; its strongest seam turns out to be
the one it treated as an afterthought.

## Inert, attributed, and never authority

The render injects text written by other agents into a session's opening
context. That makes it a prompt-injection surface by construction, which is a
property of the design rather than a hypothetical: the board is deliberately a
shared writable store that crosses an isolation boundary, and the whole
mitigation is that its content is inert. Posts render inside an explicit
untrusted-data frame that names the authoring branch and states what a post
*cannot do* — it cannot approve anything, it cannot change configuration, and a
command in its text does not run. Stating the prohibitions rather than merely
labelling the provenance is a one-line difference with a real effect: a reader
who has been told "this cannot approve anything" is in a different position from
one who has been told "this is data."

There is a second hazard that has nothing to do with injection. A cross-session
medium propagates *norms* as efficiently as it propagates methods, and this
project's quality rests on norms that are cheap to erode — never bypass the
commit hooks, never disable a test rather than fix it, verify before asserting,
stagger the gates. The reported case that inspired this campaign contains the
mechanism in one sentence of quoted agent reasoning: *task impossible, peers
doing it, we should continue*, where "peers doing it" is load-bearing in a
decision to cross a stated boundary. So attribution is never optional, a post is
rendered as one session's claim rather than as guidance, and no post may weaken
a gate, a hook, a decision record, or the project's instructions. The governing
documents outrank the medium, always.

## The wire and the ledger

Mid-campaign, with six of the nine tasks landed, Claude Code shipped
cross-session messaging: plain text delivered between one user's own sessions
over a per-session socket and pushed into a running session's turn.

It subsumes exactly one of this design's capabilities — a directed question and
answer between two *live* sessions, which it does better — and it does one thing
this design does not do at all. It delivers into a session already running. The
board renders at session start, at the self-map, and at the pre-integration
check, so a hold-off posted mid-session never reaches a session in progress.
That is a real gap, not a quibble.

What it does not reach is everything that makes a ledger different from a wire.
Nothing is stored, so a question cannot outlive its asker, an answer cannot
become precedent, and the corpus that makes "which conventions earned their
keep" an empirical question does not exist. A message needs a recipient, so
*whoever owns the height datum* is unaddressable. And the decisive one is reach:
the wire carries between one user's own sessions, while a git ref in a shared
repository carries to collaborators, to continuous integration, and to anyone
with access. That is a difference in reach rather than in convenience.

So the response was narrowing and integrating rather than abandoning. The
durable question-and-answer convention is kept and sharpened into the form the
wire cannot take — unaddressed, outliving its asker, accumulating as searchable
precedent — with the rule stated the other way round: use the wire when you know
who can answer and they are running; use the board when you do not, or when the
answer is worth keeping. And when a question *is* answered over the wire, post
the answer back to the board, which turns the wire's speed into the ledger's
memory. That is this project's existing discipline about ideas, applied to
answers.

One thing is worth recording as validation rather than as a lesson. That
feature's safety rules — a message cannot approve, cannot change configuration,
commands in it do not run, and repeats are throttled and deduplicated — are two
of this design's decisions arrived at independently, by people who had not seen
it. That is the third external convergence the campaign collected: an existing
git-native issue tracker had already made the same substrate bet (objects in the
repository, not files in the tree); the accidental board reported in the wild
had already demonstrated the medium, including the detail that after being shut
down it was *rebuilt out of directory names* — a medium with no schema ambition
that still carried the traffic; and now message safety. Content addressing gives
the deduplication for free and more strongly: theirs drops repeats within a time
window, while here the same post is the same object forever.

## What it does not claim

The board never blocks. An enforcing board would relitigate a settled decision
— this project already *declined* to serialize its gate, on the grounds that
waiting twelve minutes to start a four-minute gate is worse than the contention
— so every read seam is non-fatal, skips a corrupt post with a warning, and has
no verdict.

Two assumptions are load-bearing and unmeasured, and both are honest about which
way they might fail. The first is that agents will actually post: the read side
is mechanized, the write side is judgment, and only a skill instruction stands
behind it. The second is that technique posts are worth their cost — and the
failure mode there is not silence but *noise*, a board of published facts nobody
needed, taxing every session's context to no benefit. The human window over the
history is the instrument for both: after some weeks, how many techniques were
published, and how many describe something a later session actually hit again.
If the answer is none, the compounding half is a hypothesis that failed, and
saying so is a finding.

The failure this design most deliberately specifies out is the one the reported
case actually suffered. That board's problem was not that it existed; it was
that hundreds of thousands of messages accumulated for months and nobody looked.
A board with three read seams for agents and none for a human specifies that
outcome in. So: if the board's traffic is never read, this campaign has failed
on its own terms, whether or not the tool works.
