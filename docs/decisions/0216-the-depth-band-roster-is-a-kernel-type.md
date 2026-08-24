# 0216. The depth-band roster is a kernel type, and moving it is not an epoch

**Status:** Accepted (2026-08-23) · **Decider:** Nathan · **Relates:**
[0044](0044-shared-units-live-in-the-kernel.md) (clause (a)),
[0094](0094-a-deliberate-duplicate-shares-its-roster-never-its-derivation.md),
[0002](0002-domains-depend-only-on-kernel.md),
[0143](0143-the-underworld-carries-two-ladders.md),
[0176](0176-the-chamber-address-gains-a-floor-and-sunless-becomes-nadir.md);
[The Drift](../../book/src/chronicle/the-drift.md)

In the context of the underworld's habitation ladder having been spelled three
different ways — `DelveRung` in `domains/terrain`, `DelveZone` in
`domains/climate`, and a bare `u8` rank on `ChamberAddr.band` — and of the
climate copy citing decision 0094 for a duplication that 0094 was not written
to cover, we decided that **the five-rung roster becomes one kernel enum,
`hornvale_kernel::Band`, carrying the members, their order and the
ladder-stepping operations, while every derivation that maps a physical
quantity onto a rung stays in the domain that owns that quantity** — and that
this move, on its own, is **not** a save-format epoch.

## Why the kernel, under 0044 clause (a)

0044 gives a quantity spoken by more than one domain exactly one legal home.
The habitation ladder is spoken by three places: terrain derives a rung from a
temperature offset above the cell's surface datum (`rung_at_depth`), climate
names an underworld community's zone, and worldgen's chamber address indexes
into it. Under decision 0002 a domain may not import a sibling, so before this
decision the only way for climate to speak the ladder at all was to restate it.
Clause (a) applies literally and names the destination.

**What did not move.** The kernel holds the roster and the ordering and
nothing else. `rung_at_depth`, the ΔT spacing, the 50 K habitable ceiling and
every question about *which* rung a depth lands on stay in
`hornvale_terrain::delve`; the underworld naming rules stay in climate. This
is 0044's own split — the kernel holds the type, the domain holds the meaning —
and it is why the move costs no behaviour.

## Why 0094 did not cover the climate mirror

`DelveZone`'s doc cited 0094, and the citation was wrong in a way worth
recording, because a decision cited for a case it does not cover reads as
settled and stops the question being asked.

0094 is about a duplicate that exists **to buy independence**: a Laboratory
metric re-implements a production rule on purpose, so that the check is not an
echo of the thing it checks, and the price is a shared roster. Both sides
compute. `DelveZone` computed nothing. It restated five names because the
layering rule left it no other way to speak them — a *forced* duplicate for
structural reasons, filed under a decision written for *deliberate* ones. The
distinguishing test is whether deleting one side would remove an independent
answer: deleting `DelveZone` removed a spelling.

The mirror had a guard, `cli/tests/suite/delve_roster_mirror.rs`, which
compared the two spellings and reddened when they diverged. That guard retires
with the mirror: one type cannot disagree with itself. The narrower thing the
guard also happened to cover — that `rung_name()`'s hand-written lowercase
save-format strings track a renamed variant — was **never** covered by it and
is not covered now; `rung_name()`'s own doc declares that a hand-maintained
contract, and the compiler's exhaustive match forces a reader through every
site on a rename, so a rename cannot land silently even though a *wrong
spelling* still can.

## Why this is not a chamber epoch

The chamber address's seed-derivation key spells the band's **name**, through
an explicit match table rather than a `Debug` impl. `Band`'s five members keep
their spellings — `Undercroft`, `Shallows`, `Deeps`, `Underdeep`, `Nadir` — so
consolidating the type moves no key, relocates no chamber, and regenerates no
world. Renaming a *type* is free; renaming a *rung* costs an epoch, which is
exactly what `Sunless -> Nadir` cost under 0176.

The same reasoning settles the rock ladder. `BandKind` became `Horizon` to free
the word `Band` for the ladder that was already using it in three places; that
is, with the `DelveRung` alias deleted in the same sweep, 507 call sites across
thirty-eight files and **zero moved bytes**, verified with a
positive control rather than by an empty diff — swapping which `Horizon` variant
labels two committed rows moved the underworld witness page, and reverting
reproduced it byte for byte, so the empty diff on the real rename means
"regenerated and agreed" rather than "this path reaches nothing".

**The campaign that carried this move IS an epoch, for an unrelated reason.**
The Drift dropped `entrance` from the chamber lattice and deleted the existence
draw, bumping `chamber/run-floors`, `chamber/branch-count`,
`chamber/branch-character`, `chamber/branch-barrier`, `chamber/entrance-count`
and `chamber/entrance-mouth` to v2, retiring `chamber/branch-root`, and adding
`chamber/band-descent/v1`. None of that
is attributable to this decision, and separating the two is the point of
recording it: a future campaign moving a roster into the kernel should expect
to pay nothing, and should not read The Drift's epoch as the price of doing so.

## What this costs

- **A type-level guarantee became a runtime test.** `DelveZone` had no
  `Surface` variant, so an underworld community at the surface was
  *unrepresentable*. `Band` has one, because terrain's ladder needs it, so the
  same property is now merely asserted by
  `no_underworld_community_occupies_the_surface`. The repair, if it is ever
  wanted, is a private-field newtype in climate — `Habitation(Band)` with a
  validating constructor refusing `Band::Surface` — which narrows the value set
  without duplicating the roster. Deferred: the test is real and non-vacuous
  meanwhile.
- **One more type in the kernel.** `Band` joins the substrate a domain may not
  avoid depending on, which is the standing cost of 0044 and is accepted as
  such.
- **The rock ladder is parked on an imprecise word.** `Horizon` is a
  soil-science term doing a crustal job; the rigorous term is *stratum*, and
  climate currently holds `Stratum` for the ocean column, whose members are
  precisely the pelagic zones. The correct end state is
  `Band` / `Stratum` / `PelagicZone` across ~750 sites. Deliberately **not**
  built here and filed as `PROC-three-ladder-vocabulary`: this campaign takes
  the one rename its own work requires and refuses the tail that would have
  tripled its diff for no measured gain.
