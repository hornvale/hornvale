# The Winze — design

*A winze is a shaft driven downward from inside a working. It is the part of a
mine that goes looking.*

**Status:** spec, awaiting G3.
**Autopilot:** engaged. Ledger at `.superpowers/sdd/decision-ledger.md`.
**Sibling:** supersedes the mechanism of
[The Planes](2026-08-18-the-planes-design.md), whose transit-realm scope
returns to the Chorography's campaign-3 slot unbuilt.

---

## 1. What occasioned it

The Planes proposed three mechanisms and measured two of them dead. The
sequence is the reason this spec exists and is worth stating plainly:

1. **A wound is a cell property gated on ancient crust.** Measured: 0 / 0 / 36
   scar cells and **zero** occupations over one, across seeds 42 / 7 / 1234.
   Falsified.
2. **A wound is the deepest terminus of a fraction of caves, selected by
   rank.** Better — it removed an absolute threshold on a quantity whose
   distribution nobody had measured. But still geometry: it placed wounds *by
   depth*.
3. **Nathan, 2026-08-19:** depth neither guarantees nor precludes one. *"It's
   just that if the cave reaches one, it's not likely to go further."* The
   causality is inverted. A wound does not appear because a place is deep; a
   place is deep **because a wound stopped the digging there.**

**"A fraction of the extreme minima" is therefore a survivorship effect, not a
placement rule** — and the thing that *finds* a wound is a culture delving, not
a cave existing. A natural void discovers nothing.

That reframing is cheaper than either design it replaces: no order statistic,
no fraction constant, no clamped-distribution problem. But it needs a
substrate that does not exist, and finding that out is what makes this its own
campaign rather than a third attempt at the last one.

## 2. Keystone

> **A delving stops where it finds something. What it found is legible only
> to the people it killed, and only for as long as anyone remembers.**

## 3. The findings this rests on

All verified in the tree, not inferred.

### 3.1 Every occupation in every world is `Agrarian`

`windows/worldgen/src/history_bake.rs:2260` contains the **only**
`Function::` assignment in production code:

```rust
function: Function::Agrarian,
```

`Function::{Mine, Trade, Cult, Fort}` are constructed nowhere outside test
fixtures. There are no mines in any world. There never have been.

### 3.2 Four of five vestige kinds cannot occur

`vestige_from_occupation` maps function to kind:

```
Function::Mine        -> AbandonedDelving   UNREACHABLE (no mine exists)
Function::Fort|Cult   -> SealedVault        UNREACHABLE (neither exists)
everything else       -> BuriedRuin         the only live kind
pre-human scar        -> GateScar           measured 0/0/36 cells
the deep sealing itself -> NaturalSeal      same gate as GateScar
```

The committed seed-42 gallery page carries a **five-entry legend** for this
taxonomy. One entry can occur.

### 3.3 An occupation has no depth

`Community.rung: DelveRung` exists build-local in `history_bake.rs:942`.
`OccupationRecord` carries no depth at all, so *"how deep did this delving
get"* is unsaved and unaskable from the ledger.

### 3.4 `dread` has no consumer

Read only by `render.rs` (a colour) and one census metric. Nothing in
placement, seating, or history consults it. There is no avoidance mechanism in
the world, in any form.

### 3.5 The knowledge machinery is half-built, and the built half is the right half

```rust
warning_legibility = exp(-(now - end) / WARNING_HALF_LIFE_DAYS)
dread              = base + 0.4 * (1.0 - warning_legibility)
```

**Dread rises as legibility decays.** The world already encodes "the older it
is, the less you know what it is, and the more you fear it." Nothing reads
either number. `KnownChannel` is a *player-session* overlay, not a
culture-level one, so knowledge *between* cultures is not modelled.

### 3.6 The derivations the design needs all ship

- `prospectivity(...)` — exported from `hornvale-terrain`.
- `seat_at(...)` — which rung of a cell's column a people would settle at
  (The Underworld).
- `Cave { kind, deepest_band, depth_reach_m }` — a terminus in metres, with
  `deepest_band == band_at_depth(column, depth_reach_m)` enforced by
  construction.

## 4. The design

### 4.1 A function is derived, not drawn

A settlement founded on rich ore is a `Mine`. `prospectivity(site)` is shipped
and exported; `history_bake.rs:2260` already has `site`, `people`, `year` and
`population` in scope. This is one line becoming a derivation, and it unblocks
`AbandonedDelving` on its own.

**The other functions are NOT in scope.** `Trade`, `Cult` and `Fort` each want
their own derivation (a route, a shrine-worthy feature, a defensible seat) and
each is its own argument. This campaign derives exactly one and says so.

### 4.2 A delving has a depth, and the depth is committed

A mine's working depth derives from `seat_at`, deepened over its tenure. It
goes on `OccupationRecord`, because "how deep did they get" is exactly the
question the rest of the design asks and it cannot be re-derived from the seed
once the occupation has ended.

### 4.3 The hazard is per-increment, and it is the whole mechanism

Each increment of delving carries a small probability of breaching. **A
delving that breaches stops** — and ends.

Nothing selects on depth. The survivorship shape falls out: a breached delving
sits at its own maximum by construction, and delvings that end for ordinary
reasons are shallower. "A fraction of the extreme minima" is an **output**.

### 4.4 The consequence is on the finders, not on the future

Nathan's ruling, 2026-08-19. A breach **ends the delving culture** — flees or
is destroyed. It does **not** place a permanent penalty on the ground.

**Future people may move back in.** This is explicit and it is the reason
avoidance-as-field-penalty is refused (§7): a cell that is forever unsettleable
is a scar on the map, not a memory in a culture.

### 4.5 What a later culture knows is transmitted, decays, and can be wrong

Three states, and the model already has the numbers for all three:

```
RECENT      the warning is legible. A later people can read what happened.
DECAYED     warning_legibility -> 0, dread -> high. They know SOMETHING is
            wrong and not what. (MEM-2's floating gap: living memory reaches
            ~3 generations.)
WARDED      SealState::Maintained reads SAFE --- and may be wrong. A ward
            that is being kept is indistinguishable from a place that was
            never dangerous.
```

That last line is the design's best property and it is free: **the model can
be mistaken in the direction that kills people**, without anyone authoring a
deception. It is also decision 0003's source-blindness arriving structurally —
a later culture receives an appearance (dread), never a source (what is
behind the wall).

**Whether a later founding acts on that knowledge is NOT in this campaign**
(§7). The campaign produces the knowledge; consuming it is the next one.

### 4.6 Nothing is named

A breach records that a delving ended by breaching. It does not record what
came through, because nothing knows. `thaumic` stays 0.0; UNI-2 stays
unratified.

## 5. Preregistration

Frozen before the code (decision 0016). Branch tables, not predictions.

### 5.1 Do mines exist, and how many? (before the hazard is written)

```
mines per world, across seeds [42, 7, 1234], once function derives:

  0 on a majority of seeds -> the prospectivity derivation is wrong. STOP.
                              Do NOT widen the ore threshold to manufacture
                              mines; report and let the controller choose.
  1-5 per world            -> viable but thin. Pool across the panel;
                              single-seed claims banned for the campaign.
  6-40 per world           -> proceed.
  >40% of all occupations  -> TOO MANY. A world that is mostly mines has the
                              same defect as one with none. Report first.
```

### 5.2 Does the hazard produce the survivorship shape?

The claim is that breached delvings sit at their own maximum depth **without
being selected for depth**. Measure the depth distribution of breached versus
ordinarily-ended delvings.

```
breached and ordinary distributions are indistinguishable
    -> the survivorship claim is FALSE and the mechanism is decoration.
       Report it as the headline. Do NOT tune the hazard to separate them.
breached are deeper, with overlap
    -> the claim holds. Report the separation and the overlap honestly.
```

### 5.3 Determinism

- The hazard **draws**, so it perturbs `history_bake`'s stream and every
  world's history changes. This is deliberate, is an epoch-scale artifact
  move, and must be re-pinned in the commit that causes it.
- `OccupationRecord` gains a field — a save-format change, additive, never a
  rename.

## 6. Save-format and epoch consequences

**Additive; not a rename; every world's history moves.**

- A new field on `OccupationRecord`, a new `CauseOfEnd` variant, a new
  predicate, and a new draw in the bake. The draw is the big one: stream
  consumption order is a contract, and a new draw inside the epoch loop moves
  every seed's history.
- Keystone fixtures refreeze at merge, from main's tip.
- For an enum widening, **the compiler is the enumeration** — no plan will
  carry a match-site list.

## 7. Non-goals

- **No avoidance-as-placement-penalty.** Nathan's ruling. Future people may
  resettle; the ground is not cursed, the culture is dead.
- **No cursed biome.** `HazardKind::Cursed` is a vestige classification; there
  is no biome, and adding one is a climate-layer argument this campaign does
  not make.
- **No culture-level knowledge channel.** The campaign produces legibility and
  dread with honest semantics; building the transmission layer (MEM-1..5) is
  its own campaign.
- **No `Trade`/`Cult`/`Fort` derivation.** One function, derived, argued for.
- **No metaphysics.** Nothing named; `thaumic` stays 0.0.
- **No transit realms.** The Planes' original scope returns to the
  Chorography's c3 slot, unbuilt.

## 8. Task shape

```
1  MEASURE: does prospectivity separate? would mines exist, and how many?
2  Function derives from ore --- mines exist; AbandonedDelving becomes reachable
3  A delving has a committed depth
4  The hazard, and the CauseOfEnd it produces
5  MEASURE: the survivorship shape (5.2) --- the null is publishable
6  Legibility and dread get honest semantics at a breached delving
7  Narration: almanac + census
8  Book, chronicle, retrospective, decision record
```

## 9. Provenance

Nathan's reframing of 2026-08-19, in conversation, after The Planes' second
mechanism was scoped. Three `ideonomy-plain` passes across the two campaigns;
the one that produced this shape was dimension-identification +
organon-construction over a matrix, on the axes *visibility*, *source*, and
*hierarchicalness* — which surfaced that `source` walks rock → history →
survivors → observer exactly once per state, and that `visibility` splits into
"can you read what it is" (`warning_legibility`, shipped) and "can you tell
it is something" (the physical seal, unmodelled).

Every mechanism in §4 is Nathan's. The contribution of this document is
checking each against the tree and finding that three of the four things it
needs are declared and unproduced.
