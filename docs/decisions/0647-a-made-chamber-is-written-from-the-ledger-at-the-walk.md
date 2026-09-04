# 0647. A made chamber is written from the ledger at the walk, never committed as a fact

**Status:** Accepted (2026-09-03) · **Decider:** Nathan (G3) / autopilot ·
**Relates:** [0568](0568-cycle-density-is-derived-not-authored.md) (cycle
density is derived from rock and workmanship — this is the `worked` clause its
text reserved),
[0618](0618-a-descent-key-makes-the-plan-a-save-format-contract.md) (the plan
grammar is a save-format contract — §Context argues why an input is not a
grammar change),
[0646](0646-the-inhabited-reading-is-a-function-of-the-plan.md) (the reading
this makes narratable),
[0617](0617-a-locks-substance-is-derived-from-rock-and-work.md) (a lock's
substance is derived from rock and work),
[0398](0398-a-capability-nothing-can-reach-is-not-a-capability.md) (a
capability nothing can reach is not a capability) ·
[The Plat](../../book/src/chronicle/the-plat.md)

In the context of `ChamberOrigin::Made` having a writer that nothing called
and every reader passing an empty override map — so that no walked descent was
ever worked and no player could reach a door — facing the choice between
committing dig facts to the ledger and deriving the origins when the
possession enters, we decided that **a column's chamber origins are derived
from the committed ledger at `delve_at`: `column_origins` reads
`occupations_at` and seats each occupying people's environment niche with
`seat_at`, marking the seated rung `Made` and every other rung `(Found,
Wild)`; workmanship then joins the plan per level as `worked(character) ||
origin == Made`** — accepting a per-descent ledger read whose cost is paid at
the walk, and accepting that a column's plan now differs from the plan the
walk would have derived for it before this campaign.

## Context

The bake-side writer's own doc header read "NOTHING IN THE SHIPPED PATH CALLS
THIS, AND THAT IS THE WHOLE DISCLOSURE", and priced its per-column twin at
"~30 lines, cheap enough for one verb". That is what shipped.

The alternative was **committing dig facts** — a `cut` predicate on a
`(vertex, rung)` subject, written at bake time and read by the walk. It was
refused because it makes the world ledger the home of a quantity that is
already derivable from the ledger, which is a second copy that can drift, and
because it is a world-file change (an epoch) for a fact nothing needs to
persist. Deriving costs one linear scan per delve and no format.

Threading the bake-side `History` through the session was also refused: the
session context would grow a field for a value the ledger already implies.

**Branch 0 is inherited, not minted.** The walk has no branch. The barrier
check already fixes the walk's address at branch 0, level 0, and the bake-side
writer marks every branch of a seated rung identically — so the choice of
branch changes nothing about which rung is made. The walk-versus-lattice debt
is narrowed, not closed: the walk still reads no run, junction or per-branch
character.

## Decision

`windows/worldgen/src/delve_seating.rs`:

```
Tenancy = Wild | Inhabited | Abandoned
column_origins(world, terrain, vertex, rungs) -> Vec<(ChamberOrigin, Tenancy)>
```

For each occupation `occupations_at(world, vertex)` returns whose people has a
row in the environment-niche registry, `seat_at(niche, cave, gradient,
water_table)` — the same call with the same three inputs the bake-side writer
and the capacity probe already make — names the rung, and that rung is marked
`Made`. Every other rung is `(Found, Wild)`, which is the pre-campaign answer,
so a column with no cave or no occupation is entered byte for byte as it was.

`Session::delve_at` calls it once per delve and hands the result to
`Underground::enter`, which stops hardcoding `ChamberOrigin::Found`.

In `circuit.rs`, `cycle_budget(kind, character, origin)` computes its
workmanship term as

```rust
let worked: u8 = u8::from(by_character || origin == ChamberOrigin::Made);
```

evaluated **per level with that level's origin**, and `brattice::admissible`
reads the same term: a `ReqKind::Key` row is admissible for a realm only if
the level its gate would sit on is worked. `worked()` stays an exhaustive
match over `Character` and takes the origin as a second argument, so a fourth
character or a third origin fails to compile rather than inheriting
"unworked".

**One writer, two grains, pinned to agree.** The bake-side aggregate over a
`History` and the ledger-side per-column read are compared on three seeds: the
set of `(vertex, rung)` pairs one marks equals the set the other marks.

## Consequence

- **This is an input, not a grammar change, and 0618 is not engaged.** No draw
  is added; no selection rule changes. The one input that moved had exactly
  one reachable value before this campaign — `Found` everywhere — and for that
  value the plan is pinned byte-identical by a digest (`0x9684f7669a211894`
  over 900 plans) taken before any grammar file was touched. A plan at a
  column with a made rung is a plan **no saved world has ever held**, because
  the walk could not produce one: a new population, not a moved one. The
  distinction is arguable, which is why it is recorded here rather than
  assumed.
- **A door is reachable in production for the first time.** On the panel's
  three seeds, 16 of 26, 2 of 3 and 3 of 5 made rungs hang at least one door;
  the same rungs under all-`Found` origins hang **zero**, by construction.
- **Made rungs are shallow, so one descent is enough.** All 34 occupied
  columns across the panel seeds seat at the top or second habitation rung.
- **The cost is a ledger scan per delve.** `occupations_at` is an un-indexed
  linear re-derivation; one call per delve is the accepted price. It is also
  paid per cave-bearing vertex by the two audit verbs, which is why both now
  build to `BuildDepth::Full` (32.9 s and 11.2 s for seed 42, measured).
- **A second writer that can drift now has a test that says so.** The
  agreement test is the reason this pair is one writer at two grains rather
  than two writers.

## See also

- [The Plat design](../superpowers/specs/2026-09-03-the-plat-design.md) §3.2,
  §3.3, §5.
- [The Plat ledger](../superpowers/ledgers/2026-09-03-the-plat.md) #2, #5,
  Task 2.
- [The Plat chronicle](../../book/src/chronicle/the-plat.md).
