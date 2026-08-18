# 0144. One community per place, where a place is a cell and a rung

**Status:** Accepted (2026-08-18) · **Decider:** Nathan · **Relates:**
[0102](0102-one-per-cell-was-an-index-artifact.md),
[0142](0142-the-underworld-carries-two-ladders.md)

In the context of decision 0102 having established twelve days earlier that the
one-community-per-cell rule is a lookup optimisation's artifact carrying no
design rationale, and of The Underworld needing an underworld community that
does not evict whoever lives overhead, we decided to **key the bake's node index
on `(CellId, DelveRung)` rather than `CellId`**, because the exclusion rule the
world actually wants is one community per *place*, and a cell is not a place
once the world has a vertical coordinate.

## Why the rung is total, and why surface density is unchanged

`DelveRung` carries an explicit `Surface` variant so the key type is total: no
reader can mistake "no rung recorded" for "the overworld", and no `Option`
appears in the key.

**Surface density is unchanged structurally rather than by measurement.**
`Surface` is one rung, so `(cell, Surface)` is one key, so a cell still holds
exactly one surface community — the property is a consequence of the key's shape,
not an empirical result that could drift. It was nonetheless pinned: twelve
single-surface-people worlds (four peoples × three seeds) are byte-identical
before and after, including an order-sensitive record-stream digest, and all
pre-existing history-bake tests are unchanged.

What the wider key buys is the two things a `BTreeMap<CellId, _>` could not
express: an underworld community no longer competes with the surface one for a
cell's single slot, and two underworld communities at different rungs can share
a column.

## The consequence that must be checked by anyone re-keying a node index

**Any draw keyed on a settlement's identity must carry the rung, and one did
not.** The disposition draw key is `(site, founded-year)`. Its uniqueness rested
on the premise that one alive community occupies a cell — a premise this
re-key removed — and nobody revisited it. Measured rather than reasoned about:
seed 1, cell 16317, founding year 91313, `bugbear` and `drow` both standing,
sharing one draw key and therefore one drawn mind vector.

The general rule this settles: **widening an index key widens every key derived
from what that index guaranteed.** Enumerating those derived keys is part of the
re-key, not a follow-up.

The specific defect is deterministic and corrupts no save, and it is **not**
repaired here. The repair needs a `settlement/disposition/v2` epoch *and* a way
for the ledger-side wrapper to resolve a rung, which it cannot do today: an
`Occupation` commits its site and its people and nothing about depth, and the
wrapper holds no terrain to re-derive one from. That needs either a committed
rung predicate or a signature change, both beyond the epoch budgeted here. The
assertion that found it was **replaced rather than relaxed** — every colliding
alive group must hold at most one *surface* people, so the collision shape this
decision licenses is recorded while a genuinely new one still fails.
