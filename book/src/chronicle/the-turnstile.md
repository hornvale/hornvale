# The Turnstile

There is one Lefford. Forty cores, and a census that uses thirty-eight of
them for six minutes at a stretch. Everything else about the project's
determinism discipline — the byte-identical worlds, the golden fixtures, the
drift checks — assumes those six minutes happen once at a time.

Nothing enforced it.

## Three doors, one lock

Census goldens could be written three ways. `scripts/census-run.sh` took a
lock. `HV_CENSUS=1 bash scripts/regenerate-artifacts.sh` did not. `cargo run
-p hornvale -- lab run studies/the-census.study.json` did not.

The project had already solved this exact problem once, for a sibling
invariant. The rule that only one machine may author census goldens is
enforced twice over: in shell, and again in Rust at the point where goldens
are written. The Rust half's own documentation explains why it exists — a
shell wrapper cannot guard a bare `cargo run`, so the guard has to live where
the writing happens, "so every caller — present or future — inherits it."

The lock never got that treatment. And the documentation pointed at the
unguarded door: the project's own instructions named `HV_CENSUS=1 bash
scripts/regenerate-artifacts.sh` as *the sanctioned refresh*. A careful
reader, following the docs exactly, took the one path with no lock on it.

## What that cost, and what it didn't

Two censuses ran together on Lefford for seven minutes — one triggered from
the other machine over SSH, one launched during a campaign's close by the
documented command.

Nothing broke. Determinism is indifferent to how busy the machine is: the
worlds a contended run produces are the worlds an uncontended run produces,
byte for byte, and the regen that came out of it proved a zero diff. What the
collision destroyed was not correctness but *knowledge*. The census phase
came in at 6m57s against a clean 5m53s, and read as an 18% regression from a
campaign that had just measured a 24% speedup. The contention was found only
because an unrelated process listing happened to show a second `hornvale`
burning 2668% of a CPU that nobody in the session had started.

That is the failure this campaign is named for: not the collision, but that a
contended run and an ordinary one were indistinguishable.

## Drawing the line by cost

The obvious fix — serialize everything that writes census goldens — draws the
line by *name*, and names rot. Every future census-scale study would have to
remember to join the list, and the one that forgot would be exactly the one
that collided.

The line is drawn by cost instead. A study declares its seeds and its pin
sets, so its size is known before it runs; at or above two hundred projected
world-builds — roughly forty seconds of the whole machine — it takes the
claim. Below that, it does not.

The threshold falls out of arithmetic rather than taste. Two long jobs that
each need the whole box for time *T* will both finish around *2T* if they
share it, and at *T* and *2T* if they queue: the first finishes twice as
fast, the second no later, and both timings come out clean. Serializing long
jobs is free. A ten-second job is the opposite case — contended it takes
twenty seconds and steals a rounding error from the census, but queued it
waits twelve minutes to do ten seconds of work. So: serialize long against
long, and never make a short job wait for a long one. For the same reason the
commit gate only *advises*; a developer waiting twelve minutes to start a
four-minute gate is worse off than one who tolerates the contention, and a
gate is not a measurement.

## Making the wait legible

A blocking lock makes the original problem worse unless the waiting explains
itself. An unexplained pause on a forty-core machine looks exactly like a
hang, and this campaign exists precisely because two indistinguishable states
were confused for one another.

So the claim is also the instrument. It records who holds the machine, since
when, which tree they are writing, and which branch and commit they are
running — that last one because *the census blocking me is running someone
else's branch* is a different situation from *it is running main*. A blocked
run says so immediately rather than pausing in silence, reports progress
against both clocks while it waits, says how long it queued when it starts,
announces loudly when it takes over a claim whose owner has died, and on
timeout names the holder and the command to inspect it.

The timing ledger gained a column for the same reason. It had always
described itself as recording "full-fixture regens, censuses, full gates",
and it had never once recorded a census — the only caller that reached it
skips them. Now census runs land there, carrying the seconds they spent
queued as distinct from the seconds they spent working. A row that reads
`wall=1240 waited_s=620` explains itself. The same row without that column is
a mystery, and mysteries about wall time are what started this.

## The bug it could have shipped

Serializing completely introduced a hazard that refusing would not have. The
sanctioned path nests: the wrapper takes the lock, then calls the regenerate
script, which calls the study runner. Each layer wants the same lock. File
locks are held per open file description, so the inner layer, opening the
same path afresh, would have blocked against its own parent — and under a
bounded wait, hung the machine for the full timeout.

The guard is that an outer layer announces the process id holding the lock,
and an inner layer treats a live *ancestor* as "already serialized". Ancestry,
not merely liveness: a stale variable inherited from an unrelated shell would
otherwise disable serialization everywhere while reporting success, which is
worse than having no guard at all.

It is tested in both directions, because a lock that never blocks and a lock
that always blocks fail identically from the outside.
