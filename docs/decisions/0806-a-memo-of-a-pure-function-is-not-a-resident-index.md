# 0806. A memo of a pure function is not a resident index

**Status:** Accepted (2026-09-06) · **Decider:** Nathan

In the context of caching work inside a fold that reads the committed ledger,
facing the fact that decision 0756 governs resident indexes and is the nearest
rule a reviewer will reach for, we decided that **a cache whose keys and values
are derived from no fact — a memo of a pure function over fixed geometry — is
not a resident index and 0756 does not bind it; the test is whether the thing
ABSORBS FACTS, not whether it lives beside code that does** — accepting that
two adjacent caches in one file may be governed by different rules, and that
each therefore owes a one-line statement of which.

**Context.** Decision 0756 says a resident index earns its state only when a
read is asymptotically cheaper than on its parent. That rule exists because a
resident index is a *fold*: it absorbs committed facts, it has a parent scan it
must beat, and its state is a derived view of the ledger that can go stale
against it. `RouteMemo` (The Culvert) has none of those properties.
`plan_to_room(from, dest, budget, ∅)` is pure over mesh geometry alone —
`NavSpace` holds exactly `dest` and `avoid`, `edges_from` computes `move_cost`
over `Facet::neighbors`, and the chain down to `Facet::neighbors` reads only
integers and a `OnceLock` derived from a const. No terrain, no ledger, no tick.
There is no parent scan to be cheaper than, because there is no fact being
folded.

**Three instances, not one, which is why this is a decision rather than a
comment.** `RoomMeshMemo` caches a fixed lattice; `PrimaryAfraidMemo` caches a
verdict over a fixed ledger snapshot; `RouteMemo` caches a route over a fixed
mesh. All three are caller-owned, all three are session- or tick-scoped, and
none is a tenant of the resident store. The Kerf declined to mint its own
proposed 0757 because no second site had re-derived its boundary; here a
reviewer independently checked 0756 against `RouteMemo` and reached this same
conclusion, which is the second derivation that rule was waiting for.

**The failure this prevents is a review failure, not a runtime one.** 0756 is
correct and load-bearing for the things it governs, and a reviewer applying it
to a pure-function memo would ask for an asymptotic argument the memo cannot
make and does not owe — or, worse, would approve moving the memo INTO the
resident store to satisfy it. The Culvert measured what that costs: putting
`RouteMemo` in the `RefCell` fold store would re-borrow across the ranking whose
store guard `believed_water` deliberately drops, converting a compile-time
refusal into a runtime panic risk.

**Consequence.** A cache introduced beside a resident index states which rule
governs it, in its own doc comment, in one line. `RouteMemo`'s says so
explicitly. This is cheap and it is the whole mechanism: the categories are not
distinguishable by where the code sits, only by what the state is derived from.

**What this does not say.** It does not relax 0756 for anything that absorbs a
fact, and it does not license a fact-free cache to skip the question of whether
it earns its memory. `RouteMemo` answers that separately and by measurement —
and the measurement is **not a demonstrated ceiling**. The distinct-pair
population is small and *decelerating*: 83 entries at wait 12, 190 at wait 60,
with no ceiling shown in sixty waits. So the memo is held for a session because
190 entries of two `Facet`s and a `usize` is trivially cheap, not because the
population was shown to stop growing. **The Culvert's spec §1.3(d) is the
canonical statement of that and this record defers to it rather than restating
it** — because the draft of this paragraph that shipped at `a6c6cfa46` did
restate it, and restated it wrongly: it read "83 entries, saturating, bounded by
the distinct-pair population rather than by history length", which this record's
own cited ledger #14 refutes.

**And it states no bar about unbounded growth.** That same draft continued: "a
cache that could grow without bound in session length is excluded on that ground
alone, which is why The Culvert's own `shared_believed_water` site is not
memoized." Read as a general rule that is wrong twice over. The campaign did not
apply it evenly — it excluded `shared_believed_water` for failing to demonstrate
a ceiling while including two sites whose own curve had not demonstrated one
either — and the exclusion did not rest on a measured separation at all: ledger
#14's ruling R12 decides it on spec Rule 3's **conservative default under
genuine uncertainty**, explicitly saying so. That asymmetry is the campaign's
own inconsistency, recorded in §1.3(d) as such, and it is not a bar this
decision hands forward. **Whether a fact-free cache earns its memory stays a
live question at each site**, answered by measurement rather than by this rule —
which is what the still-open `TOOL-route-memo-has-no-held-bytes-accessor` exists
to make possible.

*(Pre-merge correction, 2026-09-06, before ratification: this record was
branch-only when the two paragraphs above were rewritten. What was false is
named inline rather than summarised, per `docs/CLAUDE.md`.)*

See The Culvert's ledger #14 and #16, and spec §1.3(d) and §2.3.
