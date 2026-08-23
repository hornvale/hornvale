# The Drift — design

*A drift is the horizontal passage that follows the vein. It is the part of a
mine that connects.*

**Status:** spec, awaiting G3.
**Autopilot:** engaged.
**Successor to:** [The Stope](../../../book/src/chronicle/the-stope.md), which
built the underworld's extent. This campaign makes it connected, and corrects
the vocabulary that made its own author misread it.
**Blocks:** The Winze, whose energy design is denominated in a count this
campaign changes.

---

## 1. What occasioned it

The Stope's premise check for The Winze measured how much of the underworld a
player can actually reach. The answer, on seed 42:

```
open entrances                        511
levels reachable from them          1,496
levels that exist                  21,328
                                    ---
reachable per open entrance          2.93
```

**You open a door into a cave system and reach 2.93 of its ~24 levels.**

The first reading of that number was wrong, and the way it was wrong is the
reason this spec exists. It was read as *"the underworld is full of sealed
rooms"* — a connectivity problem to be tuned. Nathan's correction: a chamber
is not a room. **It is a level** — a screen-filling map with its own interior,
the thing `windows/vessel/src/underworld_level/` already generates. And
`branch` is not a parallel corridor; it is a NetHack-style branch, an
alternative pathway. The measurement was right and every word describing it
was wrong.

With the units corrected the diagnosis changes completely. This is not
percolation and it is not tuning. **It is that levels are not contiguous.**

## 2. Keystone

> **A branch is levels 1..n. There is no level 7 that does not exist.**

Depth varies; the run does not have gaps. Every roguelike in this lineage
works this way, and the shipped world does not.

## 3. The findings this rests on

All verified in the tree at `49bf2427c`, not inferred.

### 3.1 `chamber_exists` already specifies a contiguous shape, then destroys it

The function gates in six steps. The first five are structural and each has a
reason:

```
1. branch < BRANCHES_PER_SYSTEM            the lattice's width
2. floor  < FLOORS_PER_RUN_CEILING         the lattice's height
3. branch < branch_count_of(...)           C.1's drawn branch count
4. band  <= rung_rank(rung_at_depth(...))  how deep the rock allows
5. floor  < floors_in_run(...)             this run's drawn length
```

Those five define a fully determined, contiguous dungeon: branches `0..drawn`,
bands `0..deepest`, levels `0..drawn length`. Step six is:

```rust
chamber_stream(seed, addr).next_f64() < EXISTENCE_DENSITY   // 0.5
```

**A 50% coin, punching random holes through a shape that was already correct.**
A branch of 24 drawn levels realizes about 12 of them, scattered, and a descent
requires an unbroken chain of survivors down a column. That is why 2.93.

### 3.2 The passage graph is not the problem

`passages_from` offers up to four neighbours — branch ±1 laterally, and one
step of the C.4 descent sequence vertically — and **every pair of adjacent
existing levels is connected automatically**. No passage is ever drawn or
withheld. The only randomness in the entire connectivity story is whether a
level exists.

So there is nothing to tune in the graph. There is a spurious draw to delete.

### 3.3 One axis, three names, and one of them is already taken

| name | where | what it is |
|---|---|---|
| `DelveRung` | `domains/terrain/src/delve.rs` | the ladder, spaced by ΔT above the surface datum |
| `DelveZone` | `domains/climate/src/underworld.rs` | a mirrored roster of the same five names |
| `ChamberAddr.band` | `windows/worldgen/src/chamber.rs` | a rank **into that ladder** |

`ChamberAddr.band` holds a delve rung rank — the code reads
`rung_of_rank(addr.band)`. Meanwhile **`BandKind`** (`domains/terrain/src/strata.rs`)
is a different ladder entirely: Regolith, Cover, Basement, Roots, Underneath —
the rock. So the depth axis carries three names and the third one already
belongs to the stratigraphic column.

### 3.4 The climate mirror is a *forced* duplicate, not a deliberate one

`DelveZone` cites decision 0094 — *a deliberate duplicate shares its roster,
never its derivation*. But 0094 is about duplicates that exist **to buy
independence**: a lab metric re-implementing production rules so the check is
not an echo. Climate does not compute rungs at all; it cannot **import**
terrain (decision 0002). That is a forced duplicate for layering, filed under a
decision written for deliberate ones.

Decision 0044 clause (a) — *a quantity belongs in the kernel when more than one
domain speaks it* — applies squarely and says where it goes.

### 3.5 Two draws are keyed as though a branch persisted downward

```
floors_in_run(seed, RunAddr { cell, entrance, branch, band })   correct
branch_count_of(seed, cell, entrance)                           no band
character_of(seed, cell, entrance, branch)                      no band
```

`floors_in_run` is already keyed per (band, branch) — it already draws "how
many levels in this area of this band." The other two are keyed once per
system, which encodes The Stope's explicit "a branch persists downward"
semantics into the derivation itself.

### 3.6 The address key spells names, not fields

```rust
format!("{}/{}/{}/{band}/{}", addr.cell.0, addr.entrance, addr.branch, addr.floor)
```

Positional, and the only *word* in it is the band's name. Renaming a field
costs nothing; renaming a **rung** costs an epoch, which is exactly what
`Sunless -> Nadir` cost The Stope (decision 0176).

## 4. The design

### 4.1 Delete the existence draw

Remove step six. `chamber_exists` returns true for every address inside the
shape steps 1-5 describe. `EXISTENCE_DENSITY` is deleted, not lowered.

The draw is a per-address derived stream, not a shared cursor, so removing the
call shifts no other draw's position. **No stream consumption order changes.**

### 4.2 The depth axis becomes a kernel `Band`

`DelveRung` and `DelveZone` collapse into one kernel enum **`Band`**, with the
same five members and the same order. The derivation that maps a ΔT to a band
stays in `hornvale_terrain::delve` — the kernel holds the roster and the
ordering; the domains keep their own meanings and behaviour.
`cli/tests/suite/delve_roster_mirror.rs` retires with the mirror it guarded.
(The path is worth stating correctly: `domains/climate/src/underworld.rs`'s own
doc comment still cites the pre-consolidation `cli/tests/` path, and this spec
copied it before checking. Fixing that comment is in scope.)

The rock ladder `BandKind` becomes **`Horizon`**, freeing the word. `Horizon`
is imprecise for the deepest members and is accepted as such: the rigorous term
is *stratum*, and climate holds it for the ocean (see §7).

### 4.3 The address reads the way the world is described

```
(cell, entrance, band, branch, level)
```

- **band** — a `Band`. Undercroft, Shallows, Deeps, Underdeep, Nadir.
- **branch** — an alternative pathway *within one band*. The Kuo-Toa City.
- **level** — one screen-filling map. `chamber` retires as a name for it.

A branch owns a contiguous run of levels inside a single band. **Identity spans
the levels of a branch and stops at the band boundary**: you are inside the
Kuo-Toa City for five levels, and it is not "continued" in the Underdeep.

### 4.4 Branches are per-band, and so is character

`branch_count_of` and `character_of` both gain a band. A system may have two
branches in the Undercroft, one in the Shallows and two in the Deeps; each
branch carries its own character. Both labels take an epoch suffix.

### 4.5 Connections between bands are drawn, and connectivity holds by construction

Descent occurs **only at a branch's bottom level**, into the top level of a
branch in the next band. The edge set for each adjacent band pair is drawn so
that two guarantees are true without a repair pass:

- every branch in the upper band draws **at least one child** below, so nothing
  descends into a dead end;
- every branch in the lower band draws **at least one parent** above, so
  nothing is unreachable.

Take the union of those two sets, then draw any additional edges on top. Both
properties hold **by construction** — the same discipline that has kept
`junctions_at` symmetric, where canonical endpoints made agreement structural
rather than something two derivations had to maintain.

A repair loop is explicitly rejected: draw-then-patch is order-dependent, and
an order-dependent repair is a determinism hazard as well as a correctness one.

### 4.6 What follows

- `passages_from`'s vertical rule is rewritten against §4.5's edges.
- `root_floor_of` retires, and the pathology it produced retires with it — The
  Stope measured that 91% of Nadir arrivals never descended from the surface,
  because a side entrance's root band was picked uniform over all realized
  bands. Entrances land in top-band branches.
- `junctions_at` re-scopes to (band, branch).

## 5. Save-format consequences — NOT a chamber epoch

The five band **names** are unchanged, so `chamber_key`'s only word is
unchanged, so **`chamber/v3` survives**. Renaming `floor` to `level` and
`slot`-descended `branch` to a band-scoped `branch` moves no address.

What does change:

| label | why |
|---|---|
| branch count | re-keyed to include the band (§4.4) |
| branch character | re-keyed to include the band (§4.4) |
| *(new)* band-transition edges | §4.5 draws something that did not exist |

Every world's underworld changes — it becomes contiguous, and roughly doubles
in realized levels — but the address space does not move. This is a
**regeneration, not a relocation**, and it is materially cheaper than The
Stope.

## 6. Preregistration

Frozen here, before the code.

```
SHARE OF A SYSTEM'S LEVELS REACHABLE FROM ITS OWN ENTRANCES
    Per system, over systems with at least one open entrance, reported as a
    distribution. NOT a sum of per-entrance walks: two entrances into one
    system reach overlapping sets, and summing them counts levels twice. The
    Stope's witness carried exactly that hazard -- its `reachable` was
    double-count-safe only because `passages_from` never varied `entrance`,
    and this campaign makes entrances vary.
    >= 95% median        -> the intent: a system is walkable end to end.
    50 - 95%             -> report, and find what is still cutting runs.
    < 50%                -> the deletion did not do what this spec claims.
                            STOP and report.

SHARE OF ALL EXISTING LEVELS REACHABLE   today 7.0% (1,496 of 21,328, seed 42)
    The whole-world figure, so it is comparable to the number that occasioned
    this campaign. Levels in systems with NO open entrance stay inside the
    denominator on purpose: an unreachable system is a real outcome and must
    stay visible rather than be defined away.
    >= 90%               -> the intent.
    < 90%                -> either a connectivity hole survives §4.5's
                            guarantees, or entrance-less systems are commoner
                            than expected. Report WHICH, with the split.

LEVELS PER SYSTEM                    today 24.4 existing, ~12 reachable
    REPORTED, NEVER GATED. The count roughly doubles by construction; the
    interesting quantity is the distribution, not the mean.

BRANCHES PER BAND
    mode 1               -> proceed, matching C.1's intent one level down
    mode > 1             -> report as a finding; do NOT re-weight to rescue it
```

**A guarantee is asserted, never measured into existence.** §4.5's two
properties get tests that fail on a constructed counterexample, not a panel
scan that finds none — a scan over three seeds is evidence about three seeds.

## 7. Non-goals

- **Restricted passage.** Boss battles, locked doors, collapses that magic can
  clear, and the rare chamber that stays lost with something interesting in it.
  This campaign makes a system structurally connected; *restricting* passage
  across an edge is a later layer, and it is a better layer for having
  something to restrict.
- **Signage.** "DOWN to the Mines, Sector 34," in the Dwarf tongue. The
  per-branch character axis is the hook and it already exists; naming is not
  this campaign.
- **Content for the branches.** No Duergar Kingdom, no Drow Empire. This ships
  the structure that makes them addressable.
- **The three-way terminology rotation.** `Stratum` belongs to the rock and
  climate holds it for the ocean, whose members are precisely *the pelagic
  zones*. The correct end state is `Band` / `Stratum` / `PelagicZone`, about
  750 sites across three ladders. Filed as a registry row; not built here.

## 8. Open questions

- **How many extra edges** beyond §4.5's guaranteed union? Zero extra is a
  tree; many is a mesh. The draw's shape is an implementation choice the plan
  should measure rather than assume, and it is the one dial in this design that
  a reader might expect to be tuned.
- **Does `character_of` gaining a band change what a character means** to
  `junctions_at`, which currently reads it per-branch across all bands? The
  junction eligibility rule consumes it; re-keying may change which systems
  join.
