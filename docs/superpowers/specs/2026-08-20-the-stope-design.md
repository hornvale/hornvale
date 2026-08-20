# The Stope — design

*A stope is the void a working opens as it takes ore out. It is the shape a
mine leaves behind, and it grows.*

**Status:** spec, awaiting G3.
**Autopilot:** engaged. Ledger at `.superpowers/sdd/decision-ledger.md`.
**Relation to The Winze:** The Winze wanted a delving that finds something.
This is the substrate under it — the underworld has no floors, no branches
and one entrance, so there is nowhere for a delving to *go*. The Winze waits.

---

## 1. What occasioned it

The underworld ships as **a five-storey building with four stairwells, where
each storey is exactly one room**, and that room has no interior.

```
ChamberAddr { cell, entrance, band, slot }
  band  = one of five depth classes      (Undercroft .. Sunless)
  slot  = 0..SLOTS_PER_BAND (=4)
  entrance = u8, and EVERY CALLER PASSES 0
```

Adjacency already exists in both directions — sideways within a band,
vertically between bands at the same slot — so `slot` is *already* a branch
that persists downward. Nothing has ever used it as one.

Measured consequences:

- **Realized chambers per land cell: 0.50 / 0.61 / 0.80** across seeds 42 / 7
  / 1234. The underworld is *smaller* than the surface, not larger. (An
  earlier claim of 50-80x was a lattice ceiling — caves times maximum slots —
  and not a count. Corrected in `BIO-underworld-has-no-energy`.)
- `Chamber` is **a point with properties** — rung, stratum, conditions — and
  carries no interior. `RoomAddr` is a *surface* address (icosahedron face
  plus refinement path) and never reaches underground.
- **`CaveKind` (Karst / LavaTube / Fracture) is derived, varies, and nothing
  downstream reads it.** A dissolution maze, a drained lava tube and a
  joint-controlled fracture system generate identically today.

## 2. Keystone

> **A branch owns a character; a character owns a run of floors; an engine
> owns a run.** Interleaving is impossible because the engine cannot reach
> across a branch boundary.

## 3. The design

### 3.1 The address gains a floor; `entrance` stops being a lie

```
TODAY      { cell, entrance, band, slot }
PROPOSED   { cell, entrance, branch, band, floor }

  entrance  0..E   ways IN. Plural, and a mapping to branches.
  branch    0..S   what `slot` already is: a column persisting downward.
                   RENAMED, because "slot" reads as a position and it is an
                   identity.
  band      0..4   depth class. UNCHANGED --- five is right.
  floor     0..F(band)   the missing rung.
```

Floors per band, as ranges rather than constants (the counts are drawn per
branch, and the distribution is Task 2's to measure, not this spec's to
freeze):

```
  Undercroft  1-5      \
  Shallows    3-10      |   15-50 floors in a system that runs the full
  Deeps       5-20      |   ladder --- and most systems terminate earlier,
  Underdeep   5-10      |   because depth_reach_m already says where they stop
  Sunless     1-5      /
```

**A system reaching Sunless should be uncommon.** That is a prediction, not a
setting: `depth_reach_m` already determines termination, so the rate is
measurable before anything is built (§4.1).

### 3.2 A floor is a point with properties, and that is sufficient

A floor carries what a chamber carries now — rung, stratum, rock, temperature,
moisture, water table — plus its engine. **An engine generates the floor's map
from that, in the ordinary roguelike way, at the moment something looks.**

Explicitly NOT required, by Nathan's ruling: intra-floor consistency across
visits beyond what the seed gives, and vertical alignment between floors.
Nobody expects a staircase to land in a corresponding spot on the level below,
and pretending otherwise would buy nothing and cost a great deal.

This keeps the world-scale lattice coarse and pushes every detail into the
engine, which is where detail belongs and where `windows/locale` already
lives.

### 3.3 A branch owns a character; an engine set is `f(character, band)`

```
a branch draws a CHARACTER      generic cave, dwarven kingdom, drow city,
                                duergar hold, the eldritch, ...

engine set = f(character, band)
    Dwarven Kingdom:  Undercroft -> gatehouse   (heavy gate, guard post,
                                     bureaucrat's office, canteen, stair down)
                      Shallows   -> kingdom proper
                      Deeps      -> deep works
                      Underdeep  -> ABSENT; the kingdom ends

a RUN = the floors of one branch within one band, and one engine owns it
```

**Coherence is structural, not a rule to enforce.** No generic-dungeon floor
can appear between two dwarven-kingdom floors, because the engine owns the
whole run; and no dwarven kingdom appears under a blacksmith's cellar, because
the cellar is a different branch.

**A character declares which bands it may occupy.** Drow and Duergar are
Underdeep characters; the eldritch is Sunless; a cellar is Undercroft-only. A
branch that outlives its character's range terminates or hands off. This is
the modularity rule: **a new character is a new table entry and edits nothing
else** — the same discipline `windows/worldgen`'s domain roster already has.

### 3.4 Entrances are plural, and they are what make two doors interesting

`entrance` becomes a real count, and **entrance → branch is a mapping.** Two
entrances that open on different branches are two genuinely different
experiences of one cave system: the well in the town square drops into the
natural cave; the blacksmith's cellar drops into the dwarven works. Same cell,
same system, different character.

Terrain reports one cave per cell with no aperture count today, so the
aperture count is derived here at the composition root, from what the cave
already carries. **`ChamberAddr.entrance`'s own doc anticipates exactly this**
("the field exists so a future terrain change does not require relayering this
type").

### 3.5 Junctions: the underworld becomes a network

Two systems that both reach a shared band, in adjacent cells, may join there.
That realizes `MAP-underworld-shortcut` — *"hard to enter, easy to traverse
once inside; two points far apart on the surface can be close below"* — which
is false by construction today.

**A junction must be DERIVED, never drawn.** The temptation is a random link;
the honest version keys on facts (shared band, cell adjacency, compatible
characters) so a shortcut is a consequence of the geology rather than a die
roll. A drawn link would also be a new draw, and therefore an epoch on top of
an epoch.

### 3.6 Where the variety comes from

```
character per branch    x  which bands it occupies
                        x  where the system terminates      (SHIPS)
                        x  branch count and their characters
                        x  entrance count and branch mapping
                        x  per-chamber conditions            (SHIPS)
                        x  CaveKind                          (SHIPS, UNREAD)
```

Every factor but `character`, `floor` and entrance-count already ships. The
cheapest single win in the campaign is **making `CaveKind` select an engine**:
karst, lava tube and fracture are three shipped, derived, three-valued
characters that nothing reads.

## 4. Preregistration

Frozen before the code (decision 0016). Branch tables, not predictions.

### 4.1 Where do systems terminate, and how rare is Sunless?

Over the seed panel, from `depth_reach_m` alone — **measurable before any
code**: the share of systems whose deepest band is each rung.

```
Sunless is common (>25% of systems)   -> the ladder's ΔT boundaries, not this
                                         spec, decide rarity. Report and stop:
                                         the eldritch band being ordinary is a
                                         finding about The Underworld's
                                         calibration, not this campaign's.
Sunless is 1-15%                      -> proceed. Report the exact rate.
Sunless is ~0%                        -> the deepest characters have nowhere
                                         to live. STOP and report; this is
                                         the same shape as GateScar's 0/0/36.
```

### 4.2 Do the floor counts produce the intended range?

After §3.1 lands: the distribution of total floors per system. The intent is
15-50 for a full-ladder system and fewer for the common case.

```
median total floors < 5      -> the ranges are not producing depth; report
median in 5-25               -> proceed
>50 routinely                -> too many; a system nobody can exhaust is the
                                same defect as one with nothing in it
```

### 4.3 Does character actually vary?

The share of branches by character, per world. **A world that is 90% generic
cave has not solved the oatmeal problem**, and that is the campaign's own
falsification criterion.

```
one character >80% of branches  -> FAILED. Report as the headline. Do NOT
                                   re-weight to rescue it without saying so
                                   in the chronicle.
```

### 4.4 Determinism

Every new count (floors per run, branches, entrances) is a **draw**, and each
one must be keyed on **a place in a fixed lattice, never a generation
ordinal** (decision 0102).

## 5. Save-format consequences — THIS IS AN EPOCH

**The address is the seed-derivation key.** This is the largest consequence
in the campaign and it leads the G3 flagged section:

```rust
fn chamber_key(addr: ChamberAddr) -> String {
    format!("{}/{}/{band}/{}", addr.cell.0, addr.entrance, addr.slot)
}
fn chamber_stream(seed, addr) -> Stream {
    seed.derive(CHAMBER).derive(StreamLabel::dynamic(&chamber_key(addr)))
}
```

Adding `floor` and renaming `slot` changes `chamber_key`, which changes every
chamber's derived stream, which **relocates every chamber in every world**.
That is an epoch, not a drift.

Consequences that follow, and none may be skipped:

- The label takes an **epoch suffix** (`chamber/v2`), never a rename — the
  discipline `settlement/name/v2` set.
- Every committed artifact touching chambers regenerates: the vessel session
  fixtures, the census, the gallery.
- **The Underworld's drow seating moves**, because seating reads chamber
  addresses. Its committed readouts are witnesses and will fire; they must be
  re-derived and re-pinned, not silenced.
- New draws (floors, branches, entrances, junction derivation) each perturb
  stream consumption order.

**Nothing in §3 may be built before the epoch is deliberately accepted.**

## 6. Non-goals

- **No intra-floor consistency guarantee and no vertical alignment.** Nathan's
  ruling; §3.2.
- **No room-grain address underground.** A floor is a point plus an engine;
  `RoomAddr` stays a surface construct until something needs otherwise.
- **No new energy model.** Feeding the underworld is
  `BIO-underworld-has-no-energy` and its own campaign; this one gives it
  somewhere to be fed.
- **No metaphysics.** The Sunless band gets a character slot and no content:
  `thaumic` stays 0.0, UNI-2 unratified, nothing named.
- **No mines, no hazard, no wound.** The Winze resumes after this.

## 7. Task shape

```
0  MEASURE termination rates from depth_reach_m alone (4.1) --- no code
1  the address: floor added, slot renamed to branch, chamber/v2 epoch
2  floors per run: counts drawn, keyed on the lattice
3  character per branch; the band-eligibility table
4  engine sets: f(character, band); CaveKind selects for the generic case
5  entrances: plural, mapped to branches
6  junctions: derived, never drawn
7  MEASURE 4.2 and 4.3; the oatmeal criterion is falsifiable here
8  book, chronicle, retrospective, decision record, epoch note
```

## 8. Provenance

Nathan's design, 2026-08-20, in conversation. Three `ideonomy-plain` passes
across the surrounding work; this spec's shape came from the last —
tree-finding + abstraction-lift + substitution over scale and lattice
organons, on the axes *cardinality*, *autonomy*, *age*, *modularity*,
*longevity*.

The substitution that produced §3.3 is worth recording: I had written "a
**system** draws a character." Moving the owner one rung down to the
**branch** is what makes multiple entrances meaningful, and it is NetHack's
Gnomish Mines relation — a branch with its own generator hanging off a main
dungeon. The scale organon is what exposed the missing `floor` rung and the
vestigial `entrance` one, sitting either side of the grain the lattice
actually addresses.

---

# AMENDMENT, 2026-08-20: three corrections found while writing the plan

Each was found by checking a claim in §3 against the tree before a task
inherited it. The design is unchanged; three of its premises were wrong, and
all three make the campaign SMALLER.

## A.1 The epoch label is `chamber/v3`, not `chamber/v2`

`CHAMBER` is **already** `chamber/v2` — bumped by The Underworld when
`ChamberAddr.band` switched from indexing the stratigraphic ladder to the
delve ladder. Its own doc: *"`chamber/v1` is retired and must never be
reused."* §5 said v2; it is **v3**.

## A.2 A level generator already ships, and it is already descent-shaped

The Adit landed `generate_descent`
(`windows/vessel/src/underworld_level/mod.rs:404`):

```rust
pub fn generate_descent(
    rungs: &[DelveRung],          // ALREADY a sequence
    cave_kind: CaveKind,
    origins: &[ChamberOrigin],
    depths_m: &[f64],
    water_table_m: f64,
    seed: Seed,
) -> Vec<Level>
```

It is tested against real `Chamber`/`Cave` values and flood-fills each level
to assert one connected walkable component
(`windows/vessel/tests/suite/underworld_level_generation.rs`).

**So §3.2's "an engine generates the floor's map" is not new work.** It
exists, it takes a *sequence* of rungs, and it already returns a `Vec<Level>`.
The campaign's contribution is therefore narrower and better-seated than §3
implies:

```
  today   rungs: &[DelveRung] with ONE entry per band
  Stope   MANY entries per band --- a run --- and a CHARACTER selecting
          WHICH engine builds it. generate_descent becomes one
          implementation behind a selector rather than the only path.
```

## A.3 `CaveKind` is read after all — one rung below where it matters

§3.6 called it "shipped, derived, and unread", and named it the campaign's
cheapest win. `generate_descent` takes `cave_kind`, so karst / lava-tube /
fracture **do** already differentiate the generated map.

The true statement is one rung up: **nothing in `windows/worldgen` uses cave
kind to select a character, pick an engine, or vary anything at world
scale.** It remains a free three-valued variety axis; it is just not free in
the place §3.6 claimed. Registry row renamed `MAP-cavekind-selects-nothing`.

## A.4 What this changes about the plan

Three seams, not a rewrite:

```
1. ChamberAddr gains `floor`; `slot` -> `branch`; chamber/v3 epoch
2. the rungs slice carries a RUN --- many entries per band, not one
3. a character selects the engine for a run; generate_descent is the
   first implementation behind that selector
```

Everything else in §3 stands, and §4's preregistration is unaffected.

---

# AMENDMENT B, 2026-08-20: Task 0's null, a refused sixth rung, and the barrier

Task 0 ran before any production code, as §4.1 required, and it falsified
§3.1's prediction. What follows is Nathan's ruling on the consequences, taken
in conversation after two measurements. **The design is not rescued by
retuning** — the falsified prediction stands as the campaign's first
publishable null, and the response is a change of *mechanism*, not of
threshold.

## B.1 The null: Sunless is the modal terminating rung, not a rare one

§3.1 said *"A system reaching Sunless should be uncommon. That is a
prediction, not a setting."* It is now measured, and it is wrong.

| seed | systems | terminate Sunless | share |
|---|---|---|---|
| 42 | 874 | 214 | 24.49% |
| 7 | 1681 | 727 | 43.25% |
| 1234 | 1266 | 536 | 42.34% |
| **pooled** | **3821** | **1477** | **38.65%** |

Denominator: cave-bearing **land** cells (terrain reports at most one `Cave`
per cell; ocean cells carrying a cave = 0 on all three seeds).

**Two things make this a design finding rather than a calibration bug.**

1. **The number was already in the tree.** `domains/terrain/src/delve.rs`'s
   module doc has published 24.5 / 43.2 / 42.3% since The Underworld, to the
   unit. §3.1 called the rate "measurable before anything is built" while it
   was already *measured*, and readable. The prediction was falsifiable by
   grepping, and the campaign wrote it anyway.
2. **`Sunless` is the ladder's open-ended bottom bin `[50 K, inf)`, and
   `HABITABLE_CEILING_K = 50` is an authored fidelity choice, not a fitted
   one.** `delve.rs:79-85` says so in terms: *"50 K is the least well-placed
   edge in the table and cannot be moved … an accepted cost of the ceiling
   being a fidelity choice rather than a measured one."* So the rung does not
   mean "extraordinarily deep". It means "past the depth at which the ladder
   stopped modelling habitability" — the leftover bucket, and leftover buckets
   are large by construction.

## B.2 A sixth rung was considered, measured, and REFUSED

The obvious repair — split `[50, inf)` at some X and put the eldritch below it
— was measured rather than argued. It does not work, and the measurement is
committed (`085f3239`).

**There is nowhere to cut. 99% of the entire super-50 K population lives in
`[50, 61)`** — an 11 K-wide lump (99.1 / 99.7 / 98.9% by seed). Observed max
ΔT is **68.086 K** against a possible 87.7 K.

| X (K) | P(reach X \| reached Underdeep) | share within ±0.5 K | verdict |
|---|---|---|---|
| 58 | 3.4 / 7.4 / 28.0% | 2.2-3.5% | outside the target band on seed 1234 |
| **60** | 0.8 / 0.6 / 5.4% | **4.11%** worst | in band, but the rate swings **9.5x** across the panel and the edge is **2.7x less stable than the worst edge already in the ladder** — worse than the 10 K edge that was condemned and moved |
| 61 | 0.8 / 0.2 / 0.9% | 0.1-1.0% | holds **2 / 2 / 6 systems**. The good stability and the empty rung are the same fact: an edge in a desert always looks stable |
| >= 62 | 0 on seed 42 | — | §4.1's own stop condition, one rung down |

A sixth rung would be within budget — the ladder's own test allows `4..=6`
habitation rungs — so this is refused on evidence, not on permission.

**A wrong mechanism was proposed and then refuted by the same measurement, and
that is recorded because the campaign would otherwise carry it.** The
controller predicted the super-50 K population was pinned against
`CAVE_REACH_CEILING_M = 3000.0`. **Only 3 of 3821 caves (0.08%) sit at that
clamp, and zero on two of three seeds; no clamp binds on this population at
all** — not the reach ceiling, not `LAVATUBE_CEILING_M`, not the gradient
band `[15, 30]` (realized span 20.7-29.2, 1.41x). The concentration is instead
that ΔT = gradient x reach and the two do not attain their maxima together, so
the product concentrates without either factor clipping. Had the prior been
confirmed rather than tested, this spec would now assert that raising
`CAVE_REACH_CEILING_M` spreads the deep class — which the data does not
support. Right answer, wrong *because*.

## B.3 `Sunless` is renamed to `Nadir`, inside the `chamber/v3` epoch

Two reasons, and the second one has a deadline.

**The word was carrying a promise the ladder does not make.** "Sunless" reads
as the eldritch deep; the rung means "past the modelled habitable ceiling".
`Nadir` is an *astronomical* term — this project's native idiom — and the
delve ladder is measured as ΔT **above the surface datum**, so "the lowest
point relative to the datum" is coherent with the ladder's own coordinate
rather than decoration. It also owes nothing to another world's vocabulary.

**It must happen now or cost a second epoch.** `chamber_key` spells the rung's
**name** into the seed-derivation key, through the explicit `rung_name` match
table at `windows/worldgen/src/chamber.rs:274` (deliberately not a `Debug`
impl). So renaming a rung relocates every chamber in every world — a label
epoch. Task 1 already mints `chamber/v3`; the rename rides it for free, and
lands at any later date at full price.

## B.4 The keystone amendment: depth is physics, access is content

The campaign's scarcity was on the wrong axis. §3.1 tried to make the deep
**rarer**; what it wanted was for the deep to be **harder**.

> **The ladder says how far the rock lets you go. The branch says what is in
> the way. Neither is a source of truth for the other.**

This is not two sources of truth for one quantity — it is one source each for
two different quantities, and conflating them is what made the 38.65% read as
a defect. It also prices correctly: moving a ΔT threshold relocates chamber
existence in every world and re-pins every witness, while changing a gating
draw edits a constant.

Measured, the ladder already agrees with the intended reading at three of five
rungs (reach probabilities, seeds 42 / 7 / 1234):

| rung | intended | reaches it today | |
|---|---|---|---|
| Undercroft | cellars, cave mouths, dungeon entrances; too many to count | 100% | agrees |
| Shallows | familiar, less hostile, commerce with the surface, freely scattered | 91 / 95 / 93% | agrees |
| Deeps | the frontier: towns, ecosystems, a Wild West between Shallows and Underdeep; common, and where oatmeal must be fought | 76 / 59 / 81% | agrees |
| Underdeep | organized, powerful, self-sufficient; Drow / Duergar / Svirfneblin are typical but **not exclusive**; rare | 31 / 52 / 53% | too common **as a band** |
| Nadir | endgame; reachable only past everything above it | 80 / 83 / 81% given Underdeep | far too common |

Both mismatches are the same error and take the same fix: **reaching a band is
not the same as meeting what lives there.** A drow-tier civilization is a
*character drawn on a branch*, at whatever rate the design wants; the band
merely says the rock is cold enough. Note the step-conditional
`P(Nadir | Underdeep)` is 80-83% on all three seeds while every step above it
swings (40-88% for one step) — that stability is structural, and it means
essentially no rock separates the Underdeep from the Nadir. **The endgame gate
is content, and it could never have been geology.**

## B.5 The barrier: one dial, per branch, whose meaning varies with depth

Nathan's design, and it replaces §3.3's "the eldritch is Sunless" outright.

**A branch draws a barrier thinness.** The `Nadir` is not a deeper cave; it is
a borderland where the barrier between the underworld and whatever lies beyond
it is thin. What is common is the *region*. What is rare is the *thinness*.

```
sealed   the barrier holds            moderate effects
warded   a rupture, sealed by someone <- implies a warder, therefore a
                                        society that survived doing it
thin     the barrier is failing
open     a portal into Chaos          civilization-ending
```

**One scalar, read differently by band** — which is what makes it one source of
truth rather than two:

- In the **Deeps**, a thin barrier is the semi-eldritch frontier: a cursed but
  coherent society, commerce with the surface, portals. Accessible on purpose;
  59-81% of systems reach the Deeps, and a weird trading city has to be
  findable.
- At the **Underdeep**, it is part of why anything down there is powerful and
  self-sufficient enough to hold territory.
- At the **Nadir**, a thin barrier is **what lets you through at all**. So
  reaching the Nadir *is* the metaphysical event, not merely a deeper cave, and
  "you got past the Drow and found a way further down" is not a die roll — it
  is *the barrier was thin here*, a fact about that branch other systems can
  also read.

**The scalar is pinnable**, in the idiom `--sky` / `--plates` /
`--ocean-fraction` already establish, so the whole curve is tuneable without
regenerating or retuning the ladder.

## B.6 §6's "nothing named" non-goal is LIFTED; the metaphysics ban is not

§6 said: *"No metaphysics. The Sunless band gets a character slot and no
content: `thaumic` stays 0.0, UNI-2 unratified, nothing named."* Nathan lifted
the **"nothing named"** clause: the barrier's four states are named here.

**The rest of that non-goal stands, and it costs less than it appears to.**
`thaumic` is a reserved axis on `MaterialBuffer`
(`domains/terrain/src/lithology.rs:105`), pinned to identically zero by
`buffer_axes_are_bounded_and_thaumic_is_zero`. But `thaumic` is a property of
**rock** — a lithology axis — and barrier thinness is a property of a **place
in the underworld**, a branch. Different objects. So the barrier ships as its
own quantity: **`thaumic` stays 0.0, that test stays green, and UNI-2 stays
unratified.**

**What a thin barrier DOES remains out of scope.** The cursed society, the
incursion, the civilization-ending open portal: a later campaign. This one
ships the dial, not the noise it makes.

## B.7 Preregistration, amended

§4.3's oatmeal criterion stands unchanged. Two additions, frozen here, before
the code:

```
P(Nadir access | branch reached Underdeep)
    0-10%    -> the intent. Report the exact rate.
    >10%     -> the gate is not gating. Report; do NOT retune to rescue it
                without saying so in the chronicle.
    ~0%      -> the endgame is unreachable, which is the same defect as an
                ungated one. Report and stop.

share of branches by barrier state
    reported as a distribution, not gated. A world that is >90% `sealed` has
    a dial nobody can feel; a world with no `sealed` branches has no baseline
    to contrast against. Both are findings for the controller.
```

## B.8 A preregistration defect, recorded rather than repaired

§4.1's branch table has a **hole between 15% and 25%**, and seed 42 (24.49%)
fell in it. The verdict was unaffected — pooled and two of three seeds cleared
25% outright — but the table must be repaired before reuse, and repairing it
*now*, knowing the answer, is precisely what preregistration exists to
prevent. Left as written; carried to the retrospective.

## B.9 What this changes about the task shape

```
0  DONE. Terminates measured; §3.1 falsified; the sixth rung refused
1  the address: floor added, slot -> branch, Sunless -> NADIR, chamber/v3
2  floors per run (unchanged)
3  character per branch AND barrier thinness per branch --- one object, one
   lattice key, one test shape. Drow-tier civilizations are a rare CHARACTER
   draw, never a band property
4  engine sets: f(character, band); the barrier read here (unchanged otherwise)
5  entrances (unchanged)
6  junctions (unchanged)
7  MEASURE 4.2, 4.3, and B.7's two additions
8  DoD, plus a decision record for chamber/v3 AND the rename
```
