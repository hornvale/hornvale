# The Whetstone

**August 2026 · outcome: merged — the test suite runs in a little over half
the time, because the largest cost in it was never in Hornvale's own code**

## What was attempted

The commit gate had drifted. It was budgeted at four minutes when the project
last tuned it deliberately; by August it was closer to fifteen, and fifteen
minutes is long enough to change how a person works — long enough to be worth
avoiding, and a gate people avoid is not a gate. So: profile the suite
honestly, find where the time goes, and take it back.

The previous campaigns of this kind had all found the same *shape* of defect —
some quantity being recomputed when it could have been remembered — and this
one began by looking for a third instance of it. That is not what the
flamegraph said.

## What the flamegraph said

A sampling profile of the single most expensive test in the suite attributed
**seventy per cent of its time to the standard library**: to advancing an
iterator, to looking up a key in a tree, to checking that a slice index was in
bounds. Not to sculpting terrain, not to fitting the coexistence stack, not to
any line anyone in this project has written. Those are one-instruction
operations. They cannot cost seventy per cent of anything.

They cost it here because they were being compiled without optimization — and
the reason *that* had gone unnoticed for so long is the interesting part.

Rust compiles a generic function not once, but separately for each type it is
used with, and it compiles each of those copies **into the crate that uses
it**, at that crate's optimization setting. Hornvale had been tuning
optimization crate by crate: five of its twenty-five crates had been marked
"optimize this one" by five earlier campaigns, each of which had profiled its
own hot spot and fixed it locally. But the crate that *defines* a piece of
shared machinery is not the crate that *pays* for it. Marking the kernel as
optimized optimized the kernel's own code, and did precisely nothing for the
copy of `look this key up in a tree` that a different, unmarked crate had
stamped out for itself. Every earlier campaign's fix had been real, and every
one of them had been leaking through the same hole, invisibly, because from
inside a single crate the hole cannot be seen at all.

There was a second instance of the same blind spot, one layer out.
Hornvale routes every transcendental function — every exponential, sine, and
arc-cosine in the simulation — through a single external library, deliberately,
so that the arithmetic is identical on every machine. Rust leaves external
libraries unoptimized in a test build even when your own crates are optimized.
So the innermost arithmetic of the entire simulation, the function called more
often than any other, was the one thing in the build compiled with no
optimization whatsoever. In one representative test, the exponential function
alone accounted for **a quarter of the total running time** — nearly all of it
inside the bit-shuffling helper routines that vanish the moment an optimizer
is allowed to look at them.

## What shipped

The per-crate tuning is gone, replaced by one line that optimizes the whole
workspace, one that optimizes its dependencies, and one that gives the
mathematics library the same setting the shipping build already gives it.

The result that made this an easy decision is the cost side, which turned out
not to exist. Optimizing everything was expected to buy speed at the price of
slower compiles; measured, a full rebuild of every test binary took *the same
time as before* — within noise. An optimizer that is allowed to delete code
hands the code generator and the linker much less to do, and the two effects
cancel. The project had been paying a large, invisible tax to avoid a cost that
was not there.

Two genuine recomputations turned up underneath, once the codegen noise was
out of the way, and both were fixed:

- The fact ledger answers "what is this thing's name?" by consulting an index
  built for exactly that question — and then wasn't using it. It gathered
  *every* fact about the subject, sorted them, and scanned the sorted list for
  the one predicate it wanted. The index is already keyed by subject *and*
  predicate; asking it directly, and taking the first or last position rather
  than sorting, is the same answer without the gathering or the sorting.
- The ecology's capacity calculation asks, for every cell of the globe and
  every species, how heavily that species draws on each of six resource axes.
  A species' answer is the same for every cell — it is a property of the
  creature, not of the ground — yet all six were being looked up out of a tree,
  per cell, per species. They are now looked up once per species, above the
  loop.

## Why it is safe

Hornvale's central promise is that the same seed yields the same world, byte
for byte. A performance change that moves an output is not an optimization; it
is a corruption. So the standard here is not "the tests pass" but "no
committed byte moved", and it was met rather than assumed.

Optimization level is not a semantic knob in Rust — the language never enables
the fast-and-loose floating-point transformations that would make it one — and
the project already leans on this without having said so out loud: the shipping
build has always optimized every crate, while the test suite pins hundreds of
exact strings against artifacts produced by that shipping build. Had optimization
moved a digit, that arrangement would have been failing for years.

The two recomputation fixes were made identical by construction rather than by
tolerance. The ledger's index returns the same positions the sorted list did,
so the first and last of them are the same facts. The hoisted resource weights
multiply the same six products and add them in the same order — floating-point
addition is not associative, so the order is the guarantee, and a test asserts
the two forms agree bit for bit rather than approximately. A second test reads
the source of both loops back out of the file and checks their axis orders still
match the order the weights were resolved in, because that pairing is the one
thing a future edit could silently break.

Then the whole thing was regenerated: every almanac, every map, every derived
page, the thousand-world survey. Zero bytes different.

## What it measures

The whole suite, one machine, one morning, the before and after arms run three
hours apart under the same background load:

| | suite wall clock | summed per-test durations |
|---|---|---|
| before | **17m 07s** | 12,140 s over 3,280 tests |
| after | **8m 26s** | 6,034 s over 3,283 tests |

Twice as fast, by both readings. The second column deserves a caveat the first
does not: those are each test's *elapsed* time, and ten tests share ten cores,
so a test that spends its time waiting is counted as though it were working.
It is a fair way to rank tests against each other within one run and a poor way
to account for processor time. The wall clock is the number that is simply
true.

Read per crate — same caveat, same ranking use — the possession layer fell
2.7×, the composition root 2.2×, the measurement lab 2.1×. One group moved the
other way: the dozen tests that launch the command-line tool as a *separate
process* got slower on paper, because packing the same work into half the wall
clock leaves those subprocesses contending with twice as many busy neighbours.
Run on its own, the largest of them takes 12.7 s — against the 30 s the
*before* run attributed to it and the 43 s the after run did. Nothing about it
regressed; it was never being measured in the first place.

## What it leaves reserved

The profile after the change looks the way a numerical workload is supposed to
look: a quarter in the mathematics library doing real transcendental
arithmetic, a sixth in the mesh and the noise field. There is no longer a
single dominant waste to point at, which is the honest signal that this
particular vein is worked out.

Two threads are left deliberately. The resource vector is still a tree keyed by
a dense, complete run of small integers — the shape the kernel's own house
rules say should be an array — and converting it is a change of *meaning* as
well as representation, because the type currently distinguishes an axis
recorded as zero from an axis not recorded at all. That is its own campaign,
not a rider on this one. And the thousand-world census, which costs six hours
of processor time and runs on another machine entirely, has not been
re-measured under any of this; the same reasoning says it should have gained
substantially, but the same discipline says a number nobody has run is not a
result.
