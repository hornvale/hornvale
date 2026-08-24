# 0207. The derived store is generic per value shape, never heterogeneous

**Status:** Accepted (2026-08-23) · **Decider:** Nathan

In the context of designing one store for the growing family of derived
values `RoomMeshMemo` and its successors compute, facing a choice between a
single heterogeneous store keyed by type and a generic type instantiated once
per value shape, we decided to **ship `Derived<K, V>` as a plain generic
struct, aggregated per shape rather than erased into one heterogeneous
container**, accepting that a caller wanting several shapes holds several
`Derived` instances rather than indexing into one.

**Why: determinism before ergonomics.** A heterogeneous store needs
`Box<dyn Any>` keyed by `TypeId` to hold different `V`s side by side, and
`TypeId`'s ordering is not stable across builds — it is an opaque hash that
can differ between compilations of the same source. A `BTreeMap<TypeId, _>`
would put an unstable iteration order under a byte-identity guarantee, which
is exactly the defect class the workspace's `HashMap`/`HashSet` ban already
exists to close (decision 0005, `clippy.toml` `disallowed-types`). A
generic-per-shape store keeps every map a plain `BTreeMap<K, _>` with
`K: Ord`, whose iteration order is `K`'s own order — deterministic and
build-stable across hosts and compilations, with nothing currently iterating
over it but nothing foreclosed either.

**What this buys beyond determinism.** It satisfies metaplan §3.6's "closed
view set before open" commitment: the author of a `Derived<K, V>` instance
declares the shape at the type, so the property battery can enumerate every
shape that exists rather than discovering them through a runtime registry. A
heterogeneous store would have made that enumeration itself a runtime
question.

**Consequence.** `RoomMeshMemo` (`kernel/src/room.rs`) is a thin facade over
two `Derived` instances — `Derived<RoomAddr, [RoomAddr; 3]>` for `neighbors`
and `Derived<(RoomAddr, u32), Option<[(CellId, u64); 3]>>` for
`corner_weights` — with its public API unchanged (decision 0206's Pure/Ledger
split lives inside each instance, not across them). Adding a new derived
shape means declaring a new `Derived<K, V>` field, never touching a shared
registry or an `Any`-erased map. The cost is exactly the one accepted above:
no single store to iterate "every derived value" through; a caller that needs
that view assembles it from the shapes it knows about.

**See also.** Spec §3 (`docs/superpowers/specs/2026-08-23-the-forebay-
design.md`); metaplan §3.6 (`docs/superpowers/specs/2026-08-22-the-penstock-
metaplan.md`); decision 0005 (deterministic collections and sorts); decision
0206 (the two validity classes each `Derived<K, V>` instance carries per
entry).
