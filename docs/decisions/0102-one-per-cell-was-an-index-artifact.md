# 0102. The one-community-per-cell rule is an index artifact, not a design position

**Status:** Accepted (2026-08-04) · **Decider:** Nathan · **Relates:**
[0048](0048-flow-condensation-replaces-the-suitability-scatter.md),
[0082](0082-locale-chamber-place.md),
[0101](0101-geometry-and-society-are-separate-vocabularies.md)

In the context of asking why every generated world's settlements sit in a
handful of tight carpets, having established by git archaeology that the
~112 km minimum separation nobody designed is a consequence of a lookup
optimisation's type, we decided to **record that the one-community-per-cell
constraint carries no design rationale, so relaxing it requires an epoch and a
census rebaseline but no architectural relitigation** — accepting that a
ratified-looking registry row was wrong and that we say so in the log rather
than quietly.

## What was found

`Bake::vacant_habitable` is the enforcement point:

```rust
fn vacant_habitable(&self, era: &EraClimate, cell: CellId) -> bool {
    Self::factor(era, cell) > 0.0 && !self.node_index.contains_key(&cell)
}

/// The single alive community per occupied cell (the scan≡index invariant).
node_index: BTreeMap<CellId, usize>,
```

A `BTreeMap<CellId, usize>` **structurally cannot hold two communities per
cell.** The constraint is a property of the index type.

The archaeology:

- `node_index` and `vacant_habitable` entered in `a3afef9c`, **2026-07-20**, in
  *The Living Community*'s deep-history bake.
- Frontier `SOC-dense-settlement` — which states *"no two living settlements are
  closer than ~110 km"* as though it were a design claim, and attaches Nathan's
  wish to relax it — cites a brainstorm of **2026-07-27**, a week later.
- *The Blocking*'s spec, chronicle and retrospective say nothing about 110 km or
  one-per-cell.
- *The Living Community*'s spec mentions `node_index` **once**, as a performance
  note: `scan ≈ Z²·¹, index ≈ Z¹·²`.

So the sequence was: an index was chosen for lookup speed; a spacing floor
emerged from that index meeting `GLOBE_LEVEL = 6`; and a week later the emergent
behaviour was written down as a design rule with a rationale it never had.

## Why this is worth a record

This is [0082](0082-locale-chamber-place.md)'s pathology a second time — *"the
product of two constants in different crates that never meet"*, which *"no test
would have caught."* 0082 caught it in the anchor vocabulary. Here it produced a
phantom design position, and the phantom was durable: it read as settled, it
carried a decision-shaped sentence, and it would have been defended.

Measured consequences, for the record. Across seeds 42, 7, 999999 and
16244526067196353746 the minimum nearest-neighbour distance is **identical to
the decimal** (0.99°, ~110.2 km at Earth radius) — the signature of a floor being
the binding constraint rather than a distribution's tail. Every world shows 6–12
contiguous carpets; every measurable `occ-founded-from` hop is one cell.

## What follows

- **Relaxing one-per-cell needs no design argument.** It moves world identity, so
  it needs an epoch and a census rebaseline (and under
  [0099](0099-worlds-are-version-locked.md) the epoch is cheaper than it was).
  What it does *not* need is a case against a prior position, because there is
  no prior position.
- **`SOC-dense-settlement` must be corrected, not merely re-statused.** Its
  claim about what the rule *forbids* — rival polities in sight of one another —
  is a real and useful observation; its implication that this was chosen is not.
- **The remaining carpet causes are separate and still open**:
  `GENESIS_TOP_CELLS = 64` of 40,962, `GENESIS_SITES_MIN/MAX = 2..4`, one-hop
  daughter budding at `DAUGHTER_PROB = 0.06`, and — the keystone — `factor()`
  being species-blind. Relaxing one-per-cell alone would not scatter
  settlements; it would only let carpets overlap.
- **A registry row is not a decision.** Frontier rows are elaboration, not
  ratification (0031), and this episode is the concrete cost of reading one as
  settled. When a row states a rule, check the enforcing symbol's history before
  defending or relitigating it.

## See also

Frontier `SOC-dense-settlement`, `SOC-settlement-tiers`;
[0101](0101-geometry-and-society-are-separate-vocabularies.md) (the conflation
that made a settlement a cell in the first place).
