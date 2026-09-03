# 0636. A world-scoped derivation lives on `WorldContext`

**Status:** Accepted (2026-09-03) · **Decider:** Nathan · **Campaign:** The Terrier

In the context of `brief::brief_of` rebuilding the world's entire occupation
register from the ledger on every call it served, facing the choice between
threading a session-lived memo through the derivation (the shape its own cost
note explicitly forbids) and hoisting the derivation to the object that
already owns every other world-scoped read, we decided that **a derivation
that is a pure function of the immutable `World` is built once on
`WorldContext` and read from there; no session path ever rebuilds one**,
accepting that the guard for this is a structural scan of what the source
names, not a counter, because a counter for a whole-world read that no longer
happens on a turn path has no writer.

## Context

`hornvale_worldgen::occupations_by_vertex(world)` reconstructs every
occupation committed to the ledger, grouped by vertex, from
`find(IS_OCCUPATION)` plus a `value_of` read and parse per record. Before
this decision, `brief::brief_of` called it fresh on every invocation.
Measured (`windows/vessel/examples/move_cost.rs`, release, seed 42,
contended — `uptime` load averages 28–51): **8.7–26 ms per call**, and a
chamber turn calls `brief_here` two to five times (`enter` four in `handle`
and one more in `snapshot`). The function's own doc had carried a `NOTE ON
COST` since `4569d883d` (2026-07-27) prescribing exactly this hoist — "the
session can hold it for the possession's life" — and explicitly forbidding a
memo inside the function ("a hidden cache in a derivation path is how
derived state stops being derived").

`WorldContext` (The Quire) already exists for precisely this shape: terrain,
climate, the locale context and the demography report are each derived once
per world and shared by every session `WorldContext` starts, because each is
a pure function of the immutable `World` it is built from.

## The decision

A derivation that reads only `world.ledger` or the world's sculpted fields —
draws nothing from a seed stream, and produces the same value for the life of
the `World` it is derived from — is built exactly once, on `WorldContext`,
and every session reads it from there. No session path is permitted to
reconstruct one per call.

Concretely: `WorldContext` gains a `pub(crate) occupations:
BTreeMap<Vertex, Vec<OccupationRecord>>` field, built at the end of
`WorldContext::build` (after the five seeded derivations, so a reader cannot
mistake a stream-consuming sixth derivation for this read). `brief_of` takes
`&BTreeMap<Vertex, Vec<OccupationRecord>>` instead of `&World`, and
`Session::brief_here` passes `&self.wctx.occupations`.

The guard is a **structural source scan**
(`windows/vessel/tests/suite/the_terrier.rs`), not a counted budget in the
shape of decision 0598's `TurnWork`: after the hoist, no turn path performs a
whole-world occupation read at all, so a counter for it would have no bump
site — the permanently-green zero 0598 itself argues against. The scan
asserts that production code under `windows/vessel/src` names
`occupations_by_vertex`, `occupations_at` or `occupation_records` only inside
the body of `WorldContext::build`, witnessed red against the pre-hoist tree
and against a mutated post-hoist tree before being trusted green. Memoising
inside `brief_of` itself remains forbidden, for the reason its own comment
already gave.

## Consequences

- `WorldContext::build` (and so `Session::start`) pays the map's cost exactly
  once per world instead of paying it repeatedly per turn; `repossess`, which
  reuses an already-built `WorldContext`, pays nothing.
- A future world-scoped read reappearing on a session or turn path is a
  ratchet failure at the commit that introduces it, on any machine — not a
  finding some later profiling campaign has to rediscover.
- **The scan's direction is presence, not completeness**, and is stated as
  such in its own doc: it proves nothing whole-world is read outside
  `WorldContext::build`; it does not prove the hoisted map is itself correct
  or current. That half is carried by a separate VIEW ≡ SCAN assertion —
  `wctx.occupations[v] == occupations_by_vertex(world)[v]` for every vertex,
  and `brief_of` over the hoisted map equal to `brief_of` over a fresh map at
  every locale a script visits — which holds by construction because `World`
  is immutable for the life of a `WorldContext`, and is asserted anyway
  because "by construction" is the sentence a later writer to `world.ledger`
  would not read.
- The registry row this decision corrects
  (`TOOL-chamber-snapshot-prices-a-shadowcast`) had priced the wrong step of
  this same derivation at ~8 ms; the shadowcast itself is 0.011–0.013 ms.

## See also

Decision 0092 (derivation happens at named construction sites); decision
0598 (per-turn work is a counted budget — and the argument this decision
borrows for why a scan, not a counter, guards a thing that no longer
happens); [The Quire's chronicle](../../book/src/chronicle/the-quire.md)
(`WorldContext` as the shared owner of every world-scoped derivation);
[The Terrier spec](../superpowers/specs/2026-09-03-the-terrier-design.md)
§1–§3; [The Terrier chronicle](../../book/src/chronicle/the-terrier.md);
`docs/superpowers/ledgers/2026-09-03-the-terrier.md` entries #1–#3;
`windows/vessel/src/session.rs` (`WorldContext::occupations`,
`brief::brief_of`); `windows/vessel/tests/suite/the_terrier.rs`.
