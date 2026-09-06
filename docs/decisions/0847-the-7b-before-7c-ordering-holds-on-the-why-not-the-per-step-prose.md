# 0847. The 7b-before-7c ordering holds on the why, not on the per-step prose

**Status:** Accepted (2026-09-06) · **Decider:** Nathan (autopilot) ·
**Campaign:** The Warrant (Penstock stage 7b) · **Amends:**
[0238](0238-stage-7-is-three-stages-and-their-order-is-forced.md)'s fidelity
argument — **not** its stage order, which stands · **Relates:**
[0846](0846-the-errand-is-the-committed-unit-of-intention.md),
[0016](0016-studies-preregister-hypotheses.md);
[The Warrant spec](../superpowers/specs/2026-09-05-the-warrant-design.md) §1

In the context of [0238](0238-stage-7-is-three-stages-and-their-order-is-forced.md)
resting the whole 7b-before-7c ordering on a claim that was read off the code
and never rendered — *"The trail is content, not bookkeeping. Each step's
`provenance` is authored prose … Replacing per-step commits with per-errand ones
before the intention carries its own compositional `why?` would delete readable
content"* — we decided, having rendered it, that **the ordering stands and its
stated reason does not**. The prose is content; *per-step commits* are not what
carry it. **0238 is not reversed and its stage table is untouched.**

## The measurement

The instrument is the real `windows/historiography::recount`, driven through
`hornvale possess --seed N --script <look; wait 12; !why 1>` — the same call the
repl's `why` and the session's `!why` make. Forty residents per seed, twelve sim
days, counting maximal runs of constant `provenance` in each resident's own
trail:

| seed | `agent-at` facts | errands (runs) | distinct prose strings | steps per errand (mean) |
|---|---|---|---|---|
| 7 | 2600 | 40 | **1** | 65.00 |
| 14 | 3080 | 40 | **1** | 77.00 |
| 23 | 1364 | 579 | **3** | 2.36 |
| 42 | **0** | 0 | 0 | n/a |

A run of identical strings carries the information of **one** string. Between
2.36 and 77 steps restate the same sentence, and the distinct-string count per
resident over twelve days is one, one and three. On seeds 7 and 14 the
repetition is **98.5% of the rendered lines**. A per-errand fact is therefore
lossless with respect to today's rendered prose *by construction*, because a run
boundary is defined as the point where the string changes.

The fourth row is its own finding and it bounds the blast radius: **seed 42 —
the flagship, on which every committed artifact is built — commits no `agent-at`
fact at all** (67 residents, swept individually, over 90 sim days, zero). Its
roster condenses on water and never has to walk.

## What replaces the argument

The ordering is right for a **stronger** reason than 0238 recorded: not "per-step
prose is content that abstention would delete", but **"the why is content, and
it currently has no home except a string repeated at every step."** 7b gives it
a home. Once it has one, 7c may drop steps without deleting anything a reader
can distinguish — which is a better precondition than 0238 claimed to be
establishing, and it is what stage 8's compaction will need.

## Consequence

7c inherits a *stated* guarantee rather than an assumed one: every committed step
is covered by an errand fact that names its reason and its origin, so dropping
steps deletes position, never why. The two asymmetries that come with it are in
[0846](0846-the-errand-is-the-committed-unit-of-intention.md) and in the spec's
§8.

What this costs is a correction that must not be read as a rescue. 0238's
fidelity instinct was sound and its conclusion was correct; only its evidence
was assumed. That is why this record amends rather than supersedes: reversing a
decision whose *order* measurement confirms would be the wrong repair, and
leaving the false half in force would let the next reader cite "per-step prose
is content" as a reason to keep per-step commits.

**See also.** [The Warrant chronicle](../../book/src/chronicle/the-warrant.md);
[retrospective](../retrospectives/the-warrant.md);
[ledger](../superpowers/ledgers/2026-09-05-the-warrant.md) #4;
[the spec's reproduction appendix](../superpowers/specs/2026-09-05-the-warrant-design.md).
