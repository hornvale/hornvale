# The Forebay

A forebay is the pond immediately upstream of a penstock: water already
drawn and waiting, so the turbine below never idles on the river above. This
campaign built the sim's own forebay — one general store for a derived
value, `Derived<K, V>`, and moved `RoomMeshMemo` onto it — and along the way
it corrected two beliefs about the code it was migrating, both wrong in the
same direction: each trusted a description of the code over the code.

## The design the constructor overturned

The campaign's own spec opened with three validity classes for a derived
value: pure geometry that never goes stale, a ledger-derived fold that goes
stale when a watched fact commits, and a third, "world-scoped" class for
`RoomMeshMemo`'s `corner_weights` table — its doc comment says the cache is
valid only "for the ONE `(geo, index)` pair this memo is used with," which
reads as a dependency on which world is loaded.

Reading the constructor said otherwise. `Geosphere::new` takes a `level: u32`
and nothing else — `grep -n Seed kernel/src/geosphere.rs` returns no hits at
all — so a geosphere, and everything derived from it, is determined
entirely by its subdivision level. Two geospheres at the same level are
byte-identical. `corner_weights` is not world-scoped; it is a pure function
of `(RoomAddr, level)`, and the level was simply missing from the key.

That collapsed the design to two classes rather than three, split at a
different line than the spec first drew: not *provenance* (does this value
come from the world or the ledger?) but *key-completeness* (does the key
this value is stored under carry every parameter its computation reads?).
"World-derived" turns out not to be a class at all — it is `Pure` with the
world's identity folded into the key, exactly as a seed-keyed derivation
like `domains/terrain/`'s rill reading already is. Decision 0206 records the
refinement; decision 0207 records the store's shape, one generic
`Derived<K, V>` per value, never a single heterogeneous store keyed by
`TypeId` — whose ordering is not stable across builds, and would put an
unstable iteration order under a byte-identity guarantee the moment anyone
iterated it.

## A guard that was there because the key was wrong

The migration then met a test that predicted its own obsolescence.
`corner_weights_memo_asserts_against_geo_level_aliasing` fed one memo a
level-2 geosphere and then a level-3 one and required a panic — a
`debug_assert_eq!` catching a caller who reused a memo across globes it was
never valid for. The natural first reading is that completing the key made
this guard *total* rather than partial: its own comment worried about two
different geospheres at the *same* level slipping past it, and since same-
level geospheres are identical by construction, that gap can never open.
True, and beside the point.

The guard did not exist to catch a bug that could not happen. It existed
because the key — a bare `RoomAddr` — did not name which geosphere resolved
it, so mixing levels in one memo was a real caller error with nothing to
catch it but a runtime check. Put the level in the key, and two levels
become two different keys: the memo holds both and answers each correctly.
There is nothing left for the guard to catch, not because it grew stronger,
but because the bug class it watched for stopped being expressible.
Completing an incomplete key does not harden a guard against a wrong
answer — it removes the condition the guard was insurance against.

That is a capability gain, not merely a removal: one memo may now
legitimately serve several globe levels in the same run, which is precisely
what the census — thousands of worlds sharing one process — wants. Decision
0208 records it, and the `should_panic` test was replaced rather than
deleted: dropping an assertion silently is a loss of coverage even when the
thing it guarded against is gone, so its replacement asserts the stronger
property directly — one memo, two levels, both answers correct — rather
than leaving the guard's absence untested.

## What the store actually buys today

The honest headline is not that this campaign made anything faster. Task 1
built the four counters `RoomMeshMemo` needed to answer its own founding
question — is the existing cache's reuse being thrown away, or is the
residual cost simply new lookups a cache cannot help? — and the answer,
read off `agent_scaling.rs`'s own bench, is the second one: that harness
already hoists the memo above its tick loop, so the profile's 13.4%
`scan_at` figure was measured against an *already-warm* cache. A warm cache
still costing 13.4% is miss-bound by direct observation, not by inference.

A synthetic kernel-side probe, built when the natural readout site
(`windows/vessel`, held off by a concurrent campaign) could not be reached,
measured a 96.2% hit rate under a locality-shaped walk — and turned out to
be a restatement of its own walk width rather than independent evidence: with
nothing evicted, every miss is a first touch, so the hit rate is exactly
`1 - distinct_keys/total_lookups`, a number the walk's own parameters set.
Its real job is narrower and still useful: it rules out the alternative
explanation that the memo is broken and never hits at all. The profile,
not the probe, carries the verdict.

So `Derived<K, V>` ships for the reason the metaplan already named —
generality for a workload that has not arrived, not a measured win against
today's — and it ships honestly, having verified rather than assumed which
of those two stories is true. Its `Ledger` validity class ships with no
current tenant at all: ledger reads are 0.04% of a tick today, so the class
exists because belief and the social graph are expected to make it hot,
not because anything hot needs it now.

## The common thread

Both corrections came from the same discipline: opening the file the
documentation was describing, rather than trusting the description. A doc
comment said "world-scoped"; the constructor said "level only." A test's
name said "asserts against aliasing"; its precondition — an incomplete
key — was the thing that made the assertion necessary, and reading the
migrated type showed the precondition was gone. Neither correction changed
what the campaign built. Both changed what the campaign is honestly allowed
to claim about it.

## Postscript: the ledger class has a tenant it can never serve (2026-08-26)

The store's `Ledger` validity class shipped with no tenant, on the expectation
that belief and the social graph would make it hot. The first candidate to
arrive was neither, and it exposed a boundary the class had not had to state.

[The Tailrace](./the-tailrace.md) went looking for a home for six folds over an
agent's committed movement history. A memo is the obvious fit and it is the
wrong one, structurally rather than marginally: the watched dependency for such
a fold is *that agent's own position*, which the tick commits every time the
agent moves. The entry is stale every tick, every read is a miss, and every
recomputation is the whole-history walk the cache existed to avoid — a cache
with a guaranteed hundred per cent miss rate.

What those folds want is not a memo at all. Invalidation asks *is this still
true?*; accumulation asks *what does this become?* There is nowhere in a memo to
put an update function, and adding one would change what the type is. So the
answer was a sibling module in the kernel rather than a third validity class,
and this store was left exactly as it shipped (decision 0236). The class is
still expected to earn its tenant from belief and the social graph; what is now
settled is that a dependency touched on every tick disqualifies a memo by
construction, and that this is a property of the *read cadence against the
write cadence*, not of how the value is computed.
