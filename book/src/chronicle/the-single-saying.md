# The Single Saying

*A name that cannot lie is not yet a name that cannot be forgotten.*

[The Given Word](./the-given-word.md) made a stream label a type: a
`StreamLabel` can only be built from a declared constant or a genuinely
runtime leg, never a bare literal, so a typo can no longer hide inside a
working build. But every crate still had to say each label *twice* — once
to declare it, once more, by hand, in a separate list, to publish what it
meant for the project's own generated record of itself, the stream
manifest a book reader can open and read cover to cover. Nothing bound the
two sayings together. A constant could be added, correctly, permanently,
and the manifest simply never learn of it — not a compile error, not a
test failure, just a silence where a sentence should have been.

This happened. Twice, in fact, on `main`, discovered only by looking:
`domains/terrain`'s own manifest was missing five constants that had been
drawn from in production code for a full campaign already — `LOBING`,
three orthogonal crust noise slices, a rift crenulation leg — undocumented
and unremarked, with every gate green the whole time. `domains/climate`'s
manifest still said, in its own doc comment, "none yet," while
`WEATHER_PHASE` had been live since The Firmament. And `domains/astronomy`,
which had tried hardest to guard against exactly this — a hand-maintained
list of every constant, cross-checked by its own test, added after an
earlier omission during The Reckoning — still needed three separate
authored copies of the same twenty-one facts to keep that guard standing.

The fix is not a better list. It is one list that says both things at
once. A new `stream_labels!` macro takes a single authored entry —
a name, a value, a doc comment, a description — and generates, from that
one saying, both the permanent constant and its row in the manifest.
There is no longer a second place to remember, so there is nothing left
to forget: a constant that exists but goes unpublished is no longer a
possible shape for the code to take. Every crate that draws a
deterministic stream now speaks its labels this way. Astronomy's
hand-maintained cross-check list, built to catch a drift the type system
couldn't, is retired — the drift it watched for can no longer occur, so
there is nothing left for it to watch.

Nothing about what any label *derives* changed. A root-and-leg crate's
real derivation still chains two separate draws exactly as before; only
the manifest's documentation string, composed at compile time from the
same two literals, is new. The project's own pin-isolation suites — the
strongest proof this codebase has that a stream's order and values held —
passed unchanged, crate by crate, before the macro existed and after. What
changed is smaller than a formula and larger than it looks: a name, once
spoken, is now spoken exactly once, and every reader of the record — the
compiler, the manifest, the next person migrating a crate — hears the same
word.
