# The Portolan (part I) — retrospective

**Merged:** 2026-08-20, as part of [The Stylus](the-stylus.md)'s Task 0
absorption. Process lessons only. The result is in
[the chronicle](../../book/src/chronicle/the-portolan.md): a free-roaming map
cursor, a load-time cell index, declared per-class salience, and a
cursor-tracked status strip. Its own look mode was superseded by the campaign
that landed alongside it, and part II (fog of discovery) is spec'd, planned,
and not started.

This retrospective is written after the fact, by The Stylus's close, for the
same reason its chronicle entry is: without it, fifteen reviewed commits land
on `main` documented nowhere.

## A paused branch accrues rule-drift, not just code-drift

The branch sat 50 commits behind `main` while paused between campaigns, and
nothing compiled it against `main`'s moving state during that time. Once The
Stylus absorbed it, it was green on the client's own gate (`make
game-check`) — but it tripped a test-binary consolidation ratchet
(`cli/tests/suite/test_binary_ratchet.rs`) that had landed on `main` while
the branch slept, and that this branch's own tests had never been measured
against. The code itself was fine; the surrounding rule about how tests are
organized had moved underneath it. Being green against the `main` a branch
branched *from* proves less and less the longer a pause runs — a resumption
owes itself an explicit check against whatever `main` grew in the meantime,
not just a rebase.

## A measurement inherits the question's framing

Recorded at build time and worth keeping here as a process lesson rather
than only a chronicle anecdote: the first design spike (painting every
feature label directly onto the terrain) produced a catastrophic number —
43.5% of labels colliding even at 288 columns — that read as proof the whole
approach was unworkable. It was proof the *question* was unworkable. Asking
"what is at this cell" instead of "what labels fit on this map" made the
same information trivial (85% of cells resolve to exactly one feature). The
hour the first spike cost was not wasted; it is what let the second spike's
reframing be recognized as a reframing rather than a coincidence.
