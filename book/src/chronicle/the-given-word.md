# The Given Word

*Every world this project has ever generated stands on a single promise: the
same seed always says the same thing. That promise was kept by a string.*

`Seed::derive` has always taken a plain `&str` — a label naming which branch
of the world's one deterministic tree a value comes from. Every crate that
drew anything already knew, in principle, that these labels are permanent
contracts; changing one silently corrupts every saved world, and the project's
own conventions said as much in a comment above each one. But a `&str`
accepts anything. A hand-typed `"astronomy"` and a hand-typed `"astronmy"`
compile identically, differ by one letter, and derive two completely
unrelated streams — and nothing in the type system would ever tell you which
one you'd written. Most call sites already did the right thing, reaching for
a declared constant instead of a fresh literal. Enough didn't that the
promise rested, in the end, on care rather than on anything the compiler
could enforce.

This campaign makes that promise a type. A `StreamLabel` can only be built
two ways: `from_static`, a `const fn` meant to live nowhere but a crate's own
`streams.rs`, or `dynamic`, for the genuinely runtime-computed legs — a
species name, a settlement's cell id, a formatted salt — that were never
candidates for centralizing in the first place. `Seed::derive` now takes a
`StreamLabel`, not a string, so passing a bare literal anywhere is a compile
error, not a hopeful convention. A new check in the project's own type-audit
tool closes the one gap the type alone couldn't: nothing stops a `StreamLabel`
built from an inline literal *outside* a `streams.rs`, so the audit flags
that shape directly, wherever it appears.

Every crate that draws a stream now declares its labels once, by name, in one
place. Nothing about what any label *says* changed — every migration moved
where a string was referenced from, never what it read, and the project's own
determinism suites prove it: the same seed, given the same pins, still
produces the same world, byte for byte, everywhere. What changed is that a
typo can no longer hide inside a working build. If a label's spelling is ever
wrong now, the crate that would have silently drawn the wrong stream simply
does not compile.
