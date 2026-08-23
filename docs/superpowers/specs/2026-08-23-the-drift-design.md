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

`branch_count_of`, `character_of` **and `barrier_of`** all gain a band. A
system may have two branches in the Undercroft, one in the Shallows and two in
the Deeps; each branch carries its own character and its own barrier. All three
labels take an epoch suffix.

**The barrier was missed in this spec's first draft** and found while checking
signatures for the plan: `BRANCH_BARRIER` is keyed `(cell, entrance, branch)`
exactly as the other two are, and B.5 of The Stope's spec makes character and
barrier *one object per branch*. Re-keying one without the other would split
that object across two granularities.

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

| label | today | why |
|---|---|---|
| `chamber/branch-count/v1` | keyed `(cell, entrance)` | re-keyed to include the band (§4.4) |
| `chamber/branch-character/v1` | keyed `(cell, entrance, branch)` | re-keyed to include the band (§4.4) |
| `chamber/branch-barrier/v1` | keyed `(cell, entrance, branch)` | re-keyed to include the band (§4.4) |
| `chamber/branch-root/v1` | keyed `(cell, entrance, branch)` | **retires** with `root_floor_of` (§4.6) |
| *(new)* band-transition edges | — | §4.5 draws something that did not exist |

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

---

# AMENDMENT A, 2026-08-23: entrances are apertures into ONE system, and this IS an epoch

Nathan's ruling, after Task 0's baseline exposed a model conflict this spec
was written without seeing.

## A.1 What Task 0 found

`chamber_exists` gates on `branch_count_of(seed, cell, addr.entrance)` and
every lattice draw carries the entrance, so **each entrance realizes its own
private sublattice**. The tree states this and calls it deliberate
(`chamber.rs:662-670`): *"a mouth can name a branch its own entrance never
realized and be refused downstream... C.3 sanctions per-entrance
realization."* Measured: seed 42 has 21,328 levels summed across entrances,
against 14,976 at entrance 0 alone.

## A.2 Why it cannot stand

The model this campaign was designed from has a Blacksmith's Cellar and a
Cave under the Well — two Undercroft entrances — **both descending into the
same Spider Cave**. That requires one lattice per system with several
apertures into it.

Under per-entrance realization, §4.5's guarantees are true *within an
entrance's private world* and say nothing about the system. The campaign's
central promise would be structurally weaker than it reads, and a later
campaign putting content in branches would discover it the hard way.

## A.3 The ruling

**`entrance` leaves the lattice entirely.** It survives only as *which
aperture you came in by* — how many a system has (`ENTRANCE_COUNT`, keyed on
cell) and where each one lands (`ENTRANCE_MOUTH`, keyed on `(cell, entrance)`).
It is no longer part of any address, any lattice gate, or any per-branch draw.

```
ChamberAddr   (cell, band, branch, level)          -- entrance GONE
RunAddr       (cell, band, branch)                 -- entrance GONE
branch_count_of(seed, cell, band)                  -- entrance out, band in
character_of   (seed, cell, band, branch)          -- entrance out, band in
barrier_of     (seed, cell, band, branch, pins)    -- entrance out, band in
levels_in_branch(seed, RunAddr)                    -- entrance out
```

This also dissolves the asymmetry `entrance_mouth`'s doc records as accepted:
with one lattice there is no such thing as a branch "this entrance never
realized", so a mouth either lands on a level that exists or it does not.

## A.4 §5 is REVERSED: The Drift is a chamber epoch

The spec's §5 claimed this campaign was not one, on the grounds that
`chamber_key` spells only the band's name and no address moves. **Dropping
`entrance` changes the key**, so every chamber in every world relocates.

The label goes to **`chamber/v4`**. An epoch suffix, never a rename;
`chamber/v1` through `v3` are retired and never reused.

Nathan approved this cost explicitly. The campaign was already regenerating
every world (§4.1 deletes the existence draw), so the epoch buys the correct
model on a regeneration that was happening anyway — the same argument decision
0176 made for riding the `Sunless -> Nadir` rename on The Stope's epoch.

## A.5 What this does to the baseline, and to §6

**§6's primary gate is unaffected and this is why it was chosen.** The
per-system *share* of levels reachable is a ratio; it stays comparable across
the change.

**The absolute counts are not comparable, for a structural reason, and Task 8
must say so rather than reporting a movement.** Today's 21,328 levels on seed
42 is a sum over per-entrance sublattices. After A.3 there is one lattice per
system, so the count falls for a reason that has nothing to do with §4.1's
deleted coin. Reporting "levels went down" without that sentence would invert
the campaign's own story.

## A.6 CORRECTION to A.4 — the epoch is real, but `chamber_key` does not carry it

A.4 said dropping `entrance` from `chamber_key` relocates every chamber and
therefore forces `chamber/v4`. **The conclusion stands and the mechanism was
wrong**, found while reviewing Task 1.

`#[cfg(test)] mod tests` begins at `chamber.rs:1233`. **Every** `chamber_key`
call inside `chamber.rs`, and both `derive(CHAMBER)` sites, sit below it. With
§4.1's coin deleted, `chamber_exists` draws nothing, so:

- **nothing in production derives from `crate::streams::CHAMBER`**, and
- **`chamber_key` has exactly one production caller** —
  `underworld_readout.rs:646`, which *prints* it as the witness's `key`
  column.

So changing `chamber_key` relocates **nothing**. What actually carries the
epoch is the four labels that really do key on `entrance` and really are
consumed:

```
chamber/run-floors/v1       (cell, entrance, branch, band)   -> v2
chamber/branch-count/v1     (cell, entrance)                 -> v2
chamber/branch-character/v1 (cell, entrance, branch)         -> v2
chamber/branch-barrier/v1   (cell, entrance, branch)         -> v2
chamber/branch-root/v1      (cell, entrance, branch)         -> RETIRED (§4.6)
```

**`CHAMBER` therefore stays at `chamber/v3` and is NOT bumped.** Bumping a
label nothing derives from would mint an **empty epoch** — a label recording a
discontinuity that never happened through it, which is precisely the case a
monotonic label check cannot catch (`scripts/sluice-headline.sh` names it).

**And Task 1 falsified a committed artifact's prose without touching it.**
`docs/audits/underworld-lattice-seed-panel.md` says *"The `key` column is the
real derivation key of that run's floor 0"*, and
`underworld_readout.rs`'s module doc says the same. As of §4.1 that is false:
it is a formatted address that derives nothing. Task 4 owns the correction —
both the artifact prose and `chamber_key`'s own doc, which still claims the
address is the seed-derivation key.

This is the campaign's own standard turned on itself: a string presented as a
derivation key, in a committed artifact, that no longer derives anything.

---

# AMENDMENT B, 2026-08-23: the descent verb loses an outcome, deliberately

Nathan's ruling, after Task 1's measurement exposed a behaviour change the
spec had not anticipated.

## B.1 What happened

`hornvale-vessel`'s `delve_has_three_distinguishable_outcomes` passed at
`69d1f5469` and fails after §4.1. Its panic names the loss exactly:

> *no **sealed** cave found in seed 42's terrain — the fixture no longer has
> one of the three outcomes this campaign's descent verb needs to
> distinguish*

A cave was **sealed** when its chambers lost their existence coin flips. With
the coin deleted, every cave in shape realizes chambers. Task 1's own probe
had already proved this without anyone reading it that way:
`systems_with_open_mouth == systems` on all three seeds — **874/874,
1681/1681, 1266/1266**. Sealed is not rare now. It is **impossible**.

## B.2 The ruling

**Accept two outcomes. Sealed passage comes later.**

The descent verb ships with two distinguishable outcomes until a later
campaign builds restricted passage — locked doors, boss encounters, collapses
that magic can clear, and the rare chamber that stays lost with something
worth finding in it. §7 already listed that as a non-goal; B.1 promotes it
from *nice to have* to **owed**, because this campaign removed something the
game already had rather than merely declining to add it.

## B.3 What that obliges this campaign to do

1. **Amend the test rather than delete it.** It becomes
   `delve_has_two_distinguishable_outcomes`, asserting what is true, with a
   doc comment recording that the third outcome existed, what removed it, and
   what would restore it. A deleted test is a removed guard; an amended one
   carries the history.
2. **Make the loss loud, not silent.** The amended test must fail if a sealed
   cave ever becomes possible again while the test still claims two — the
   same STALE-DECL discipline `seam-guard` uses, where a one-directional
   acknowledgement is a claim that rots.
3. **Say it in the chronicle.** A campaign that removes a player-facing
   outcome and does not write that down has misreported itself.
4. **File the restricted-passage work** as a registry row, carrying B.1's
   measurement as its motivation.

## B.4 Why this is not a quiet re-baseline

The project's standing rule is that a falsified prediction is a finding and a
retuned constant needs saying so. This is the same shape one level up: a test
that stops passing because the world changed is evidence about the world. The
danger was never the red — it was that the report which surfaced it called it
*"pre-existing and unrelated"*, which would have made a real behaviour change
look like background noise. **The measurement was real and the attribution
was invented**, which is this project's most frequently recurring defect.

---

# AMENDMENT C, 2026-08-23: §6's per-system gate is blind, and is replaced

Found by Task 4's review, by mutation rather than by argument.

## C.1 The gated statistic cannot fail

§6 gates the **per-system median** share of levels reachable, at `>= 95%`.
Task 4's reviewer severed lateral movement between branches — a real
connectivity break — and measured:

```
                     whole-world      per-system p10    per-system MEDIAN
unmutated               100.00%            100.00%            100.00%
lateral moves refused    67.82%             35.14%            100.00%
```

**The median held at exactly 100.00% on all three seeds while connectivity was
broken.** The reason is structural and §6 could have predicted it: C.1 of The
Stope makes the branch-count mode **1**, so most systems have a single branch,
so most systems are unaffected by anything that severs *between* branches. A
median over a population dominated by one-branch systems cannot see a
branch-severing defect at all.

## C.2 The replacement

The gated per-system statistic becomes the **p10** share, at the same `>= 95%`
intent, reported alongside the **share of systems below 100%**. Both moved
sharply under the same mutation (p10 100% → 35.14%), so both can fail.

The **whole-world** arm is unchanged: it moved 32 points under the same
mutation and discriminates as written.

## C.3 Why this is not metric-chasing, and the distinction is the point

This project's standing rule is that you do not retune a criterion after
unblinding to rescue a result. **This amendment does the opposite of that**,
and the difference is worth stating precisely so a later reader can check it:

- Metric-chasing moves a threshold because the measured value fell on the
  wrong side of it. Here the measured value is at **ceiling** — 100.00%, the
  most comfortable possible reading. Nothing is being rescued.
- What is being replaced is a statistic **proved unable to fail**, by a
  mutation that broke the property the gate exists to protect and left the
  gate reading perfect.

A gate that cannot fail is not a lenient gate; it is not a gate. Leaving it in
place while reporting "both arms cleared" would have been the campaign
misreporting itself — and §6's own closing line already binds us: *a guarantee
is asserted, never measured into existence.*

## C.4 What Task 8 must do with this

Apply the p10 arm and the below-100% share, report the median as context
rather than as a gate, and **state in the readout that the median was replaced
and why** — including the mutation numbers above. A future reader who sees a
median in the output must not mistake it for the thing that was checked.
