# 0160. The chamber address gains a floor, and `Sunless` becomes `Nadir`, on one `chamber/v3` epoch

**Status:** Accepted (2026-08-22) · **Decider:** Nathan · **Relates:**
[0039](0039-epochs-replace-tiers-refine.md),
[0099](0099-worlds-are-version-locked.md),
[0094](0094-a-deliberate-duplicate-shares-its-roster-never-its-derivation.md),
[0102](0102-one-per-cell-was-an-index-artifact.md);
[The Stope](../../book/src/chronicle/the-stope.md)

In the context of an underworld that addressed one interior-less point per
depth band per column — a five-storey building with four stairwells where each
storey is exactly one room — facing the fact that `chamber_key` formats the
whole address into the seed-derivation label, so *any* change to the address
relocates every chamber in every world, we decided to **take one epoch,
`chamber/v3`, and spend it on three re-keyings at once: `ChamberAddr` gains a
`floor`, `slot` is renamed `branch`, and `DelveRung::Sunless` is renamed
`Nadir`** — accepting that every chamber in every world moves, that
`chamber/v1` and `chamber/v2` are retired and must never be reused, and that
every committed witness reading a chamber had to be re-derived rather than
silenced.

## Why an epoch suffix, and never a rename in place

The label is the contract. A world is a seed plus a ledger and everything else
is re-derived, so the derivation labels *are* the save format: renaming
`chamber/v2` in place would leave two incompatible worlds answering to one
name, with nothing in the tree able to tell them apart. An epoch suffix makes
the incompatibility legible — `chamber/v3` is a different label, so a world
generated under it is a different world, and 0099 already governs what that
means for anything holding the old one. The discipline is
`settlement/name/v2`'s, applied unchanged.

The epoch was also *cheaper as one*. `chamber_key` spells the address's fields
and the rung's **name** — through an explicit `rung_name` match table in
`windows/worldgen/src/chamber.rs`, deliberately not a `Debug` impl — so the
`Sunless` → `Nadir` rename is itself a full re-keying of every rank-4 chamber.
Taken with the address change it costs nothing; taken later it costs a second
epoch at full price. Riding it was the point of doing it now.

## Why the rung was renamed

"Sunless" promised the eldritch deep. The rung means *past the depth at which
the delve ladder stopped modelling habitability* — it is the open-ended bottom
bin `[50 K, ∞)` above an authored `HABITABLE_CEILING_K`, and leftover buckets
are large by construction: 38.65% of cave systems terminate there, pooled over
the seed panel. `Nadir` is astronomical vocabulary, which is this project's
native idiom, and the delve ladder is measured as ΔT *above the surface datum*,
so "the lowest point relative to the datum" is coherent with the ladder's own
coordinate rather than decorative.

## Depth is physics; access is content

The campaign's first measurement falsified its own spec, and the response was a
change of *mechanism*, not of threshold. The scarcity was on the wrong axis: the
design wanted the deep to be **harder**, and had tried to make it **rarer**.

> **The ladder says how far the rock lets you go. The branch says what is in
> the way. Neither is a source of truth for the other.**

This is not two sources of truth for one quantity — it is one source each for
two quantities, and conflating them is what made a 38.65% terminating share
read as a calibration defect. It also prices correctly, which is why it is
recorded *here*, in the epoch's own record: moving a ΔT threshold relocates
chamber existence in every world and re-pins every witness, while changing a
gating draw edits a constant. **No ΔT threshold was moved to buy a rarity
number.** A sixth rung was considered and measured rather than argued, and
refused on the evidence — 99% of the entire super-50 K population lives in an
11 K lump, and the only candidate edge inside the target band swings 9.5× across
the seed panel.

## What follows

- Every new count the epoch admits — floors per run, branches per system,
  entrances per system, a branch's root floor — is a **draw keyed on a place
  in the fixed lattice**, never on a generation ordinal (0102). The lattice
  ceilings (`BRANCHES_PER_SYSTEM`, `FLOORS_PER_RUN_CEILING`) size the address
  space; the draws decide realization within it. A drawn count sizing the
  address space would let a generation quantity define the lattice.
- `hornvale_climate::DelveZone` was renamed with `DelveRung` under 0094's
  roster rule: the two share the roster of rungs that must be answered for and
  keep their derivations independent, so a rung that exists in one and not the
  other is a compile error rather than a silent omission.
- **0039's keystone-retirement clause does not bite here, and that is a
  measurement rather than an assumption.** 0039 rules that an identity keystone
  frozen on pre-epoch reality *retires* at an epoch, because mechanically
  updating it either makes its `assert_ne!` vacuous or forces its exclusion
  list to swallow everything the epoch moved. No keystone's premise was broken
  by `chamber/v3`: the committed keystone fixture
  `cli/tests/fixtures/world-seed-42.json` and all three of its consumers —
  `cli/tests/suite/lens_purity.rs`, `cli/tests/suite/repose_byte_identity.rs`
  and `windows/worldgen/tests/suite/deep_realm_rehome.rs` — pass **unchanged**
  across the epoch. The reason is substantive: **a chamber is never stored.**
  Existence and content are pure functions of an address, so relocating every
  chamber in every world moves no byte of the committed world JSON. The
  derived-not-stored discipline is what makes an underworld epoch this cheap,
  and this is the clearest demonstration of it the project has taken.
- The witnesses that *did* have to move were re-derived, never silenced: the
  versioned-label roster in `cli/src/streams.rs`, the deep-realm chamber
  sweeps, and the reach and separation readouts. One spec prediction was
  falsified in the doing and is recorded as a null — "The Underworld's drow
  seating moves" is **false**, because `seat_at` reads the rung and the column
  and never `chamber_exists`, so every seating figure is byte-identical across
  the epoch.

**See also.** The Stope's spec (`docs/superpowers/specs/2026-08-20-the-stope-design.md`),
amendments A.1 (the label is v3, not v2), B.3 (the rename) and B.4 (the
keystone amendment quoted above); the chronicle entry
[The Stope](../../book/src/chronicle/the-stope.md).
