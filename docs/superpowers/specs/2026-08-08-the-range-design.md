# The Range — biome-keyed habitat, and the path that decides a world

**Campaign:** The Range. Inserted before C2d (The Radiation) at the owner's
direction, 2026-08-08, on the C2w precedent: *every elf authored before it
would be authored in a frame it changes.*

**Status:** spec, awaiting G3.

---

## 1. The finding this campaign exists to fix

**The Warren's realm gate cannot move a world.** This was measured, not
inferred.

`habitat_realm_registry` was mutated to declare `gnoll` — a peopled, settling
kind — as `Subterranean`, confining it to the ~12 % of land that holds a cave.
Seed 42 was rebuilt before and after:

```
                        readout                      committed world
                (per_species_suitability)            (seed 42, sha256/16)
  gnoll Surface   11010 / 11066 land = 99.49%        78256db156fb1c87
  gnoll Subterr.    622 / 11066 land =  5.62%        78256db156fb1c87
```

One line of source moves the readout by **93.87 points** and the committed
world by **zero bytes** — identical hash, identical 1 738 950 bytes, identical
7 764 facts, same flagship village.

**Why this is a real null and not a no-op mutation.** An empty diff is worth
nothing without a positive control, and this project has shipped guards
asserted to work that were never seen to fire. Three controls were run:

1. the revert asserted its target text was present before substituting, so the
   mutation could not have silently failed to apply;
2. the binary's mtime is 15 s *after* the source edit, ruling out a stale
   build;
3. **the readout column is itself the positive control** — it proves the
   mutation reached compiled code and changed behaviour there.

The probe is preserved at `.superpowers/sdd/range_probe.rs.txt`.

### 1.1 Why it is dead

`per_species_suitability` (`windows/worldgen/src/lib.rs:1203`), which carries
the gate at :1309, has exactly **one** production caller —
`demography_report_from` at :1550. That function's own comment at :1561 says
its index is "never serialized, never identity", and :6380 calls it "the Lab's
coexistence-stack readout". Every other caller is a test.

The path that actually places settlements is `bake_history_from` →
`emit_history`, whose capacity input is `per_species_capacity_at` — which
**takes no realm parameter and applies no realm gate.**

The chronology:

```
  2026-07-20  a47e10d0  "retire the draft placer — history places the world"
                        per_species_suitability stops being the identity path
  2026-08-06  d59e92f2  "the placement layer asks which realm a kind lives in"
                        the gate is wired into per_species_suitability
```

Seventeen days separate them. It was not the placement layer.

### 1.2 The class, and why nothing caught it

This is the same defect on its third pass:

1. C2a shipped `subterranean_substrate` whose only consumer was a test;
2. The Warren found that and wired the gate into a **readout**;
3. The Range would have been the third had it added a biome factor at the same
   site — the most elaborate no-op the project has shipped, green in every test
   written against it.

**No lint could fire.** `per_species_suitability` is `pub` in a *library*
crate, so `dead_code` never applies — public items are API surface. And the
function genuinely *is* called. "Is this item live?" and "does this item run
when the product runs?" are different questions, and no compiler lint answers
the second.

**The project's own rule was followed and still missed it.** The peoples-program
metaplan §3 defines a probe-validity ladder whose rung 2 ("expressible, unread")
it calls "the trap a campaign walks into by accident", and mandates that *"every
campaign in this program owes a mutation step … a demonstration that the readout
would report differently if the axis moved."* The Warren performed exactly that
step; its 100 % → 12 % **is** the mandated demonstration.

The rule failed on one word: **readout**. Rung 4 is defined as "the readout
differentiates the axis", so a readout-only mechanism scores a perfect 4. The
ladder tops out one rung below the thing that matters, and there is no rung for
*reaches world identity* — so nothing in the program's vocabulary could say what
was wrong.

**Proposed amendment** (doc-only, to `2026-08-03-the-peoples-program-design.md`
§3; flagged at G3 because it edits a governing document):

```
   5    reaches world identity    perturbing the axis changes the committed world
```

## 2. Why biome, and why now

`ConditionNiche` (`domains/species/src/lib.rs:350`) has exactly four axes:
temperature, moisture, insolation, elevation. **There is no biome axis**, and
`domains/species/` appears nowhere among the crates referencing `Biome::` —
climate, render, travel, locale, scene, vessel and the lab all consume biomes;
species habitat never has.

Meanwhile `Biome` has 22 variants including `TemperateForest`, `Desert`,
`Tundra`, `Ice`, `Taiga`, `CoralReef`, `Epipelagic` — **the exact vocabulary
the elf roster is specified in**. Wood = temperate forest. Sea = ocean.
Desert = desert. Snow = tundra.

Authoring those six elves in the four-scalar vocabulary means hand-fitting
Gaussian curves so their Liebig minimum approximately lights up where a biome
classification already sits. That is authoring a **proxy for a thing the model
already computes** — the same error class as duergar's 300 m, and worse, because
there the model genuinely could not say "deep" whereas here it can say
"temperate forest" and is simply not asked.

Three facts make the omission conspicuous rather than merely historical:

- **Biome already gates.** Travel gates on biome.
- **Biome already grades.** `marine_forage_supply_field` (:950) sets
  productivity straight off the biome class — `Upwelling => 1.0`,
  `CoralReef | KelpForest => 0.85`, `Epipelagic => 0.45`, `Mesopelagic => 0.15`,
  `Bathypelagic => 0.05`, `Abyssal | HadalTrench => 0.02`.
- **A mask sits outside the Liebig minimum.** This is the load-bearing one; see
  §3.2.

### 2.1 The demonstrator already exists, measured and named

The Delvers established a theorem with no terrain in it: *elevation is the
binding axis on every land cell iff the kind's authored elevation devotion is
below its sovereignty floor*, because a floored axis can never read below its
floor and an unfloored axis can never read above its devotion.

Gnoll was **re-derived here rather than inherited**, from
`sovereignty_floor(mass, potency)` at `kernel/src/ecology.rs:287` with gnoll's
authored `mass: 136.1`, `potency: 0.0`:

```
  floor    = 0.95 · (1 − exp(−(0.15 · ln 136.1 + 1.0 · 0.0)))  =  0.4954
  devotion = 0.40   (elevation, domains/species/src/lib.rs:1564)
  0.40 < 0.4954  ⇒  elevation binds on 100 % of land
```

0.4954 reproduces the Delvers chronicle's figure to four decimals. So gnoll's
authored desert curves — temperature devotion **0.80**, moisture **0.75**, the
strongest desert authoring among the peoples — are computed and then discarded
by the minimum on every cell in the world. A people with a documented desert
stronghold selects no arid cells, for a reason that has nothing to do with
deserts.

The Delvers declined to fix this: *"moving an existing people's capacity in the
same change that adds three new ones would destroy the attribution of both."*
**The Range is where it is attributable**, because The Range adds no peoples.

## 3. Design

### 3.1 The store

A **sparse** `ComponentStore<KindId, BiomeAffinity>` in `domains/species`, on
the `habitat_realm_registry` pattern: **absence means unrestricted**, so every
undeclared kind is untouched and the change is additive.

`BiomeAffinity` carries a graded per-biome factor with a default for unlisted
biomes. `0.0` is a legal value, so a hard mask is the special case rather than a
separate mechanism.

**Its RANGE is deliberately left open until the plan's task 0.** An earlier
draft specified `[0, 1]`; an ideonomy pass established that bound was inherited
by analogy from `availability` (genuinely `{0,1}` — a cave exists or does not)
and `tolerance` (genuinely `[0,1]` — a fraction of an optimum), and that neither
analogy argues anything about a biome affinity. The choice is **downstream of an
unmeasured fact**: whether the bake's contest takes an argmax over *cells per
kind* or over *kinds per cell*.

```
  mechanism                              (a) kind    (b) cell    (c) stack
                                         picks cells picks kind  weighted
  -------------------------------------  ---------   ---------   ---------
  1  mask, [0,1], downward only          YES         NO          partial
  2  boost, above 1.0 permitted          YES         YES         YES
  3  conserving — redistribute, total    YES         YES         YES
     capacity held fixed
  4  rival exclusion at the destination   n/a        YES         YES
  5  two-sided, ± around the kind's mean  YES        YES         YES
```

The mask is the only row with a NO in it. **Task 0 determines the contest shape
and thereby collapses this matrix to one row**, so the mechanism is chosen
mechanically rather than argued after a falsification. Recorded here in advance
so that the choice cannot later be presented as a discovery.

Two constraints on the choice, both from the same pass:

- **A boost above 1.0 is ecologically incoherent** in a model where capacity is
  supply × tolerance: it asserts a biome makes a creature better than its own
  resource supply supports. Row 3 achieves the same relocation without that
  claim — at the cost of a **nonlocal** coupling (a cell's value depending on
  the whole map), which collides with the per-cell purity `EraInvariantSupply`
  relies on. Neither cost is disqualifying; both must be stated in whichever is
  chosen.
- **A downward-only factor is a ratchet.** It composes multiplicatively with
  every future preference layer, so each campaign adding a preference makes the
  world monotonically emptier. No single campaign observes this; the sum is a
  slow drain. Rows 2, 3 and 5 do not have the property.

### 3.1a The double-counting hazard

Gnoll's temperature (0.80) and moisture (0.75) curves **already are** a proxy for
desert — that is §2's whole argument for why biome should exist. Adding a desert
affinity on top applies the same preference **twice**, and the campaign would
measure the sum while attributing it to the new mechanism. This is a direct
threat to attribution, which is this campaign's entire product.

The plan must resolve it one of two ways, chosen before measurement: the
affinity **displaces** the climate curves' role for a declared kind, or the
measurement **separates** the two contributions. Doing neither invalidates P1″
and P2 regardless of what they report.

Precedent for sparse-vs-field is the consumer-count rule: `HabitatRealm` is
sparse because it has one consumer holding a slice; `LifeSchedule` is a
`BiosphereTraits` field because six consumers already held the row. Biome
affinity will have two consumers holding slices (§3.3), so sparse.

### 3.2 Where it applies — outside the minimum

The factor multiplies the final result, beside The Warren's `availability`:

```rust
saturated * tolerance_liebig(cn, s, floor_buf) * availability * affinity
```

**This placement is the campaign's central claim, not a style choice.** The
Warren measured that its substrate swap is *inert*: going underground improves
moisture (.585 → .787) and insolation (.467 → .840) and the Liebig minimum never
sees it, because elevation is unfloored and therefore scarcer. Generalised:
**a non-lethal preference cannot matter while an unfloored axis is scarcer.**

A biome affinity folded *into* `tolerance_liebig` as a fifth axis would be
dominated exactly the same way, and would ship inert. Outside the minimum it is
not subject to that domination at all.

The existing doc comment at :1299 already draws the distinction the campaign
needs — `availability` "is not a tolerance — it is a presence mask … folding it
into `tolerance_liebig` would type it as a fifth environmental axis, which it is
not." It then notes that for a mask in `{0,1}`, `min` and `*` agree
arithmetically. **For a graded factor they do not**, which is precisely why the
placement is load-bearing here where it was semantic there.

### 3.3 Both sites, or it is dead on arrival

The factor is applied in **`per_species_suitability` and
`per_species_capacity_at`**. §1 is the whole reason: one of those is the readout
and the other is the world.

### 3.4 Commit shape

Approved by the owner, 2026-08-08. Preceded by a measurement, then three
commits, so that a behaviour change and a mechanism addition never share one
diff:

0. **Task 0 — determine the bake's contest shape.** A read of
   `bake_history_from`, before any mechanism is written. It collapses §3.1's
   matrix to one row and so *selects* the affinity's range rather than leaving
   it to be discovered by a falsification. Cheap, and everything downstream
   depends on it: if the contest is (b) or (c), a downward-only mask is dead on
   arrival and P1″ would falsify for a reason having nothing to do with biomes.
1. **Carry the realm gate to the identity path.** `per_species_capacity_at`
   gains the realm slice and applies `availability`. No new mechanism. Its own
   before/after world diff. This is where xorn and rust-monster start actually
   being confined, which moves worlds.
2. **Add `BiomeAffinity`**, sparse and empty. Byte-neutral by construction;
   proved, not asserted (§5, P3).
3. **Declare occupants** (§4) and measure.

## 4. Occupants

A mechanism must ship with occupants — The Long Age shipped `LifeSchedule` with
nobody in it and C2c had to fill it. And it needs **more than one**, because as
The Delvers put it, *a mechanism with one instance cannot be told apart from a
mechanism hard-coded to that instance.*

- **`gnoll` → Desert / Savanna.** The repair case of §2.1: a named, measured,
  pre-existing defect, now attributable.
- **A second declarant**, chosen at plan time from kinds whose authored optima
  are already a biome proxy, preferring a **fauna** kind so the peopled
  settlement contest moves for exactly one reason in this campaign.

**P1‴ strengthens this from a second direction.** The one-instance argument is
about *attribution*; the zoning reading is about *capability*. If a downward-only
mask cannot relocate a kind into a contested cell, then a **single occupant may
be structurally incapable of demonstrating P1″** — there is nobody to vacate the
desert. Complementary affinities across two occupants is not a nicety here; it
may be the only configuration in which relocation is observable at all. The plan
must pick the second occupant with that in mind, not merely for contrast.

Kobold is a candidate on the merits — authored as a "cool HIGHLANDER … staked to
high elevation as its exclusive, hard-excluding stronghold axis", which is an
elevation proxy for Alpine/Tundra — but it is a *peopled* kind, and moving two
peopled kinds at once is the attribution risk this campaign is otherwise built
to avoid. Deferred unless the plan finds no fauna candidate.

## 5. Preregistered predictions

Frozen before the code that would move them. A falsified prediction is a
finding; nothing here is retuned after unblinding without saying so.

**P1 was rewritten after an ideonomy pass found it non-falsifiable.** Its
original form — "the capacity field changes on a material share of land" — is
guaranteed by arithmetic: a multiplier below 1.0 on non-desert cells changes the
field on every non-desert cell, so its "> 5 % of land" threshold cleared at
~80–90 % before any simulation ran. It was a tautology in a prediction's
clothes, and the design and the prediction had the same author: the factor was
placed *outside* the minimum precisely so nothing could discard it, and P1 then
predicted that nothing discards it. The chain it should have been measured along:

```
  link                                    can it absorb the signal?
  --------------------------------------  --------------------------------
  1  capacity field changes               NO — arithmetic guarantees it
  2  gnoll's ranking of cells reorders    unlikely (affinity varies by biome)
  3  the settlement contest resolves      YES  <- was unguarded
     differently
  4  committed facts change               YES  <- was unguarded
  5  census metrics move                  YES
```

- **P1 — wiring sanity check, NOT a finding.** The capacity field of a declared
  kind differs from its undeclared value. **This cannot fail by construction**
  and carries no threshold; it asserts the multiplication happens, not that it
  matters. Recorded so that a later reader does not mistake it for evidence.
- **P1′ — the rung-5 test.** Declaring gnoll's affinity changes seed 42's
  **committed ledger**. **Falsifier:** byte-identical. This is precisely the
  test The Warren's gate fails (§1), and it belongs in the spec of the campaign
  that discovered that.
- **P1″ — suppression is not relocation.** The **arid share** of gnoll's
  settlements rises. **Falsifier:** settlement count falls while arid share is
  flat or falling. The share is preregistered rather than the count deliberately:
  a falling count with a rising share is *success*, and both falling is the
  failure mode the original P1 would have reported as a win.

  The mechanism makes this a real risk rather than a formality. An affinity in
  `[0, 1]` **can only reduce** — gnoll's absolute desert capacity does not rise,
  it merely fails to fall — so the mask can shrink a kind's range without ever
  moving it toward its niche. (The zoning analogue: taxing bakeries outside the
  market district produces fewer bakeries, not bakeries in the market, because
  it never improved the bakery's standing against the bank where it mattered.
  What works there is exclusive use — a restriction on the *rivals at the
  destination*.) Whether gnoll's *relative* standing improves depends on the
  bake's contest mechanics, which this spec does not assert and the plan must
  check.
- **P1‴ — the repair, stated before unblinding so it is a finding and not a
  retune.** If P1″ falsifies, the diagnosis is that a downward-only mask cannot
  relocate into a contested cell, and the repair is either an affinity permitted
  **above 1.0** or **complementary occupants** vacating the destination —
  explicitly *not* a retuned constant.
- **P2 — the factor differentiates.** Declaring gnoll's desert affinity
  separates its capacity field from that of kinds without one. Preregistered
  separately because The Delvers established that **binding and differentiating
  are not the same property** — a kind can win the first everywhere and still
  lose the second.
- **P3 — undeclared kinds are byte-identical.** With the store present but a
  kind undeclared, its capacity field is bit-identical to before, and seed 42's
  committed world is byte-identical across commit 2. Asserted at bit level, as
  The Warren did for its `Surface` path.
- **P4 — the identity fix moves worlds.** Commit 1 changes seed 42's committed
  world. **Falsifier:** it does not, which would mean the diagnosis in §1 is
  incomplete and the campaign stops until it is understood.

P4 is deliberately the cheap one to run first: it is the mutation proof of §1
run in reverse, and if it fails, nothing else in this spec is trustworthy.

## 6. Non-goals

- **The six elves.** C2d. This campaign exists so they are authored in the right
  frame; it authors none of them.
- **The world-identity mutation guard.** Deferred at the owner's direction
  ("a good idea for later"), captured as `PROC-readout-is-not-identity` with its
  measurement, the missing rung, and its born-red property, so it can be built
  later without re-deriving any of it.
- **Cave biomes / the underworld as a place.** The Delvers named it; it needs
  its own campaign and would make this one unattributable.
- **Re-authoring the four-scalar niches.** Biome affinity is *additive*. No
  existing `ConditionNiche` is touched — that would confound P1 and P2 beyond
  recovery.
- **Removing the elevation axis.** A save-format event with a blast radius far
  past this campaign.

## 7. Costs, flagged

- **Census regen on lefford** — required, and **authorization-gated**. Both
  fixtures rewrite wholesale (1000 + 1000 rows): commit 1 re-decides the
  settlement contest, and any new metric column rewrites every row textually.
- **~40 fixture tests redden** until that regen lands, if a lab metric is added.
  The gate cannot be green before it.
- **Epoch:** *provisionally none.* Worlds move, but no new draws are consumed
  and no stream label changes — a multiplier on an existing field alters no draw
  order. Decision 0084's rule is to declare an epoch only if a derivation
  actually moved a *drawn* quantity. **This is the flagged save-format call and
  it leads the G3 package**; it must be re-checked against the pin-isolation
  tests before commit 1 lands, not assumed.
- **Heavy tier:** two tests are red at main for written, measured, non-Range
  reasons (`scene_cost`, `session_cost`). A *third* heavy failure during this
  campaign is this campaign's.

## 8. Definition of Done

Standard, plus:

- Chronicle entry (`book/src/chronicle/the-range.md`) and a book freshness
  sweep; the Confidence Gradient re-scored if this moves one of its bets.
- Retrospective (`docs/retrospectives/the-range.md`).
- The metaplan ladder amendment of §1.2, if approved at G3.
- The three registry rows already captured — `PROC-readout-is-not-identity`,
  `BIO-provenance-mark`, `LANG-split-time-from-history` — reviewed for status
  flips at close.
