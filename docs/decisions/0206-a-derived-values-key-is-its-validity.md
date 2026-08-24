# 0206. A derived value's key is its validity

**Status:** Accepted (2026-08-23) · **Decider:** Nathan

In the context of the metaplan's `Derived` store design (§6.6), which named
three validity classes and drew the line between them at "world-derived" vs.
"ledger-derived," facing the campaign's own migration refuting that line —
`RoomMeshMemo`'s `corner_weights` looked world-scoped by doc comment but is
in fact a pure function of `(RoomAddr, Geosphere::level())`, since
`Geosphere::new` takes only a level (`grep -n Seed kernel/src/geosphere.rs`
returns nothing) — we decided to **ship `Derived<K, V>` with exactly two
validity classes, `Pure` and `Ledger`, split at key-completeness rather than
at provenance**, accepting that the `Ledger` class ships with no tenant in
this campaign.

**The two classes.** `Pure`: a pure function of its key, never invalidated,
provided the key is complete. `Ledger`: a fold over a ledger prefix ending at
a recorded position, stale once a fact committed after that position touches
a watched dependency (`kernel/src/derived.rs`'s `Validity` enum and
`Validity::is_stale`). "World-derived" is not a third class — it is `Pure`
with the world's identity (a seed, a level, whatever the derivation actually
reads) folded into the key. A seed-dependent derivation such as
`domains/terrain/`'s `rills_of`/`rill_reading` (which resolves from a `Seed`
via `CatchmentCut::Drawn`) is `Pure` with a key that happens to contain a
`Seed` — not evidence for a third class.

**What refines, and where.** This refines metaplan §6.6's own thesis — "the
storage is identical; only the invalidation policy differs" — by locating the
one policy question at key-completeness instead of at provenance. The
original draft trusted a doc comment's prose (`corner_weights` "valid only
for the ONE `(geo, index)` pair this memo is used with") over the type it
documented; reading the constructor collapsed the three-class table to two.

**The mechanism is real, not aspirational.** `Derived`'s entries are
`BTreeMap<K, (V, Validity)>`, one `Validity` per entry, judged by two
accessors: `Derived::get` (the `Pure`-only fast path, no ledger cursor) and
`Derived::get_at` (validity-aware, judges both classes and evicts a stale
`Ledger` entry on read rather than returning it). The kernel test
`a_stale_ledger_entry_is_never_returned_as_a_hit`
(`kernel/tests/suite/derived.rs`) pins exactly that behaviour.

**Accepted cost: `Ledger` ships untenanted.** Ledger reads are 0.04% of a
tick (spec §6.5's measurement) — there is no hot ledger-derived derivation to
migrate today. Building the class anyway is generality on §6.6's own
direction, not a measured win for this campaign; §10 of the spec names the
falsifier ("if the campaign after next still finds no tenant, that class was
speculative generality and should be deleted rather than maintained").

**Consequence.** A future `Ledger` tenant (belief, the social graph — the
workload §6.5's N×M argument projects) has a class and an accessor waiting
for it, with no redesign. A shape that turns out to read something its key
cannot carry is a distinct failure mode this decision does not cover — spec
§10's third falsifier names it, and the chaos-eviction battery (§5) is what
would surface it.

**See also.** Spec §2, §2.2, §2.3, §6, §8 item 1 (`docs/superpowers/specs/
2026-08-23-the-forebay-design.md`); metaplan §6.6
(`docs/superpowers/specs/2026-08-22-the-penstock-metaplan.md`); decision 0207
(the store is generic per shape); decision 0208 (the level guard this
campaign's own migration retired); decision 0005 (deterministic collections,
which `BTreeMap<K, (V, Validity)>` depends on).
