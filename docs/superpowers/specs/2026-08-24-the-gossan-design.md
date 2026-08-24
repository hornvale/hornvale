# The Gossan — chemotrophy becomes expressible

Rung 1 of the Underworld Larder
(`2026-08-24-the-underworld-larder-metaplan.md`).

A *gossan* is the weathered cap over a sulphide ore body — the rock that says
what is underneath by oxidizing. Sulphide oxidation is one of the six sources
rung 2 will key on.

## 1. What occasioned it

`marine_forage_supply_field` refuses to make hydrothermal vents productive,
and says why in its own doc: a vent community is chemotrophic, "a metabolic
class the enum does not have". Nothing in the world can eat rock, because the
type cannot say it. This campaign makes the type say it and changes nothing
else.

## 2. Keystone

**`MetabolicClass` splits into the two axes it conflates, the mapping is
bijective, and no number moves.**

## 3. The findings this rests on

### 3.1 The blast radius is ~100 sites and every one is compiler-found

Measured by stubbing the change and running `cargo check --workspace
--all-targets` on `5ccde4bb1`:

**CORRECTED 2026-08-24, and the first figure was low.** What this section
originally reported:

```
81  construction sites   9 files
14  read sites           5 files
 3  exhaustive matches   2 files
 1  projection struct
```

The read-site count was a floor read as a total. The probe applied the field
split, saw "14 errors in 5 files", and stopped — but a compile error in an
early crate means every downstream crate is never checked, so the list was the
first wave. **A `cargo check` that fails early has enumerated nothing.**
Re-measured by grep, which cannot fail early:

```
221  occurrences of `metabolic_class` / `MetabolicClass`
 34  files
  7  crates   domains/species + windows/{almanac,hearsay,lab,sentiment,vessel,worldgen}
  3  struct carriers, not 2 — BiosphereTraits, PeopleTraits, and
       `windows/vessel/src/body.rs:55`, which the first pass never found
 80  construction sites spelled `metabolic_class: MetabolicClass::X,`
  3  exhaustive match arms
```

`windows/hearsay` is an entire crate — eleven files — that this spec did not
previously mention at all. The 3-match-arm figure stands: it came from the
fifth-variant probe, which DID run to a clean workspace.

The figure in circulation was 94, from `BIO-chemotrophy`'s "widening that enum
makes every reader a blast radius". That 94 was 94 *uses of the `Endotherm`
value*, nearly all in a Rust-authored kind table and in test fixtures. Those
do not break — but the real total is larger than either number, as above.

**"Every one compiler-found" survives the correction, and was re-verified
rather than carried over:** every site is a named use of a named type or
field, no `serde` derive exists on any of the three carriers, and no macro
constructs them. Renaming the field and the type therefore makes the compiler
enumerate every use. Only the count was wrong, never the method.

### 3.2 No save-format consequence

`MetabolicClass` derives `Clone, Copy, Debug, PartialEq, Eq` — **no
`Serialize`** — and appears in no authored JSON. The kind table is Rust. No
new field, no new draw, no epoch.

### 3.3 The two readers group `Autotroph` DIFFERENTLY, and this is the design

```
basal_metabolic_rate_w:   Endotherm | Autotroph  => B0_ENDOTHERM
rise_at (liveness):       Autotroph | Ametabolic => base
```

**There is no single thermal value for `Autotroph` that preserves both
behaviours.** An earlier sketch of this campaign proposed mapping `Autotroph`
to `Endothermic` "so no numbers move"; that would have silently given every
treant an endotherm's heat-excess thirst model. The conflation is not
cosmetic — the codebase already disagrees with itself about which axis
`Autotroph` lives on.

### 3.4 Three autotrophs ship, not two

`MetabolicClass::Autotroph`'s doc says "the two shipped autotrophs (treant,
twig-blight)". The table carries **three**: `treant`, `twig-blight`,
`shrieker`. `shrieker` was added later and the doc was not updated. Verified
by count and by name against `biosphere_registry()`.

## 4. The design

### 4.1 Two types, and a fourth thermal value that already existed unnamed

```rust
ThermalStrategy          TrophicMode
  Endothermic              Heterotrophic
  Ectothermic              Phototrophic
  Unmodelled               Chemotrophic
  Absent                   Absent
```

`Unmodelled` means *has a metabolism; its thermal behaviour is not modelled*.
It is distinct from `Absent` (*has no metabolism*), and the distinction is
already load-bearing in shipped code — `Ametabolic => return 0.0` versus
`Autotroph => B0_ENDOTHERM` — it simply had no name. Naming it is what makes
§3.3's disagreement expressible instead of forced.

**`Absent`, not `None`.** The plan found that
`rise_at_couples_heat_to_thirst_per_metabolic_class` glob-imports the enum's
variants (`use MetabolicClass::*;`), where a `None` variant collides with
`Option::None`. The name changed before any code was written.

### 4.2 The mapping is bijective

```
Endotherm   ->  (Endothermic, Heterotrophic)
Ectotherm   ->  (Ectothermic, Heterotrophic)
Autotroph   ->  (Unmodelled,  Phototrophic)
Ametabolic  ->  (Absent,      Absent)
```

**Byte-identity is therefore structural, not argued.** Every existing `match`
becomes the same match with one variant name renamed; `TrophicMode` has no
reader at all. The campaign is one axis renamed 1:1 plus one axis nothing
reads.

### 4.3 Four spellings of one question become one predicate

```
allometry.rs:141   if class == Ametabolic       -> nulls the biological traits
liveness.rs:326    matches!(class, Ametabolic)  -> "uncanny"
liveness.rs:3830   matches!(.., Ametabolic)
liveness.rs:4385   matches!(.., Ametabolic)
```

Four spellings, two crates, no shared name — the same drift that produced the
mixed enum. They become one function in `domains/species`:

```rust
pub fn is_ametabolic(thermal: ThermalStrategy) -> bool
```

**CORRECTED after Task 4. This section originally specified a two-axis
predicate, and that predicate has ZERO POSSIBLE CALLERS.** Measured against
the post-Task-4 tree, every one of the four sites holds only a thermal value:

```
allometry.rs   life_history(mass, class: ThermalStrategy, schedule)   thermal only
liveness.rs:326    a `class: ThermalStrategy` parameter               thermal only
liveness.rs:3829   `npc.thermal_strategy` — `Body` has no trophic axis
liveness.rs:4384   `npc.thermal_strategy` — same
```

`Body` (`windows/vessel/src/body.rs`) deliberately carries only the axis the
vessel layer reads, on §4.5's own reasoning: an unread axis there, with no
sanctioned-pair guard at that layer, would be exactly the rot this campaign
exists to fix. So the two-axis signature was specified for a world in which
some caller holds both, and no such caller exists.

**The single-axis predicate DOES rely on the §4.4 invariant, and must say so
rather than hide it.** Its doc names the guard that enforces it — the
sanctioned-pair table of §4.4 — so the reliance is a stated direction rather
than a silent assumption. That is the same discipline the project applies to
any check whose blind side is real: name what it enforces, and name what
enforces the rest. The original text was right that a predicate trusting the
invariant is where a bad pair breaks first; it was wrong that a two-axis
signature was available to avoid it.

### 4.4 The cost of splitting, named: 16 representable pairs, 4 meaningful

Four variants become 4 x 4. The type now admits `(None, Heterotrophic)` — a
thing with no metabolism that eats other life. Pretending otherwise would
repeat the original enum's mistake.

The guard is a **declared table of sanctioned pairs**, failing on any kind
carrying an unsanctioned one. Rung 1 declares the four of §4.2; adding a fifth
is a deliberate edit — the same ratchet shape as `tropes check` and the
type-audit waivers.

`TrophicMode::Chemotrophic` ships as **`Declared`** in `coverage.rs`'s existing
vocabulary ("the variant or branch exists; no kind carries it"), asserted as
such. Rung 2's success condition is that it becomes `Witnessed` — written
down, in a test, on this campaign's branch.

### 4.5 An unread axis is how the original enum rotted, and the guard is the answer

Rung 1 ships `TrophicMode` with **no consumer**. That is deliberate (§7) and
it is also the exact condition that produced the defect this campaign exists
to fix. `MetabolicClass::Autotroph`'s own doc says so:

> The class was witnessed by The Menagerie without the modelling decision ever
> being made, and this doc claimed "unused seam" for three campaigns after it
> stopped being one.

A vocabulary nobody reads drifts, and the drift is invisible because nothing
consults it. `Ametabolic`'s doc still says "Unused seam" while four sites read
it; the autotroph count in the doc is stale by one kind. Both are the same
failure.

**§4.4's pair table is what stops the recurrence, and that is its real job.**
It is not merely a guard against the 16-vs-4 widening — it is a READER. Every
kind's `TrophicMode` is consulted by an assertion on every commit-gate run, so
a value that stops being true reddens instead of rotting quietly. The
`Declared`/`Witnessed` assertion on `Chemotrophic` does the same for the
variant that has no kind at all. Stating this converts a property the design
had by luck into one it has on purpose.

## 5. Preregistration

### 5.1 The instrument does not exist, and the obvious one is vacuous

Committed artifacts DO carry life-history numbers — `lifespan`,
`generation-length`, `pace-of-life`, `basal-metabolic-rate-w` — under
`book/src/laboratory/generated/`. **For two kinds only:**

```
goblin  Endotherm  -> Endothermic   covered
kobold  Ectotherm  -> Ectothermic   covered
treant / shrieker / twig-blight -> Unmodelled   NOT COVERED
xorn                            -> None         NOT COVERED
```

Every other appearance of `treant` in a generated artifact is a lexeme —
dictionary entries, concept-registry gaps, trope tables. Names, not numbers.

**And the two covered paths are not covered locally.**
`scripts/regenerate-artifacts.sh` writes the census only under the census
environment flag (its host guard, line 150), so `make rebaseline` never
touches it. A local drift check comes back clean whether the mapping is right
or wrong. Shipping on "no artifacts moved" would be a green light from an
instrument pointed at nothing.

### 5.2 Two instruments, because one has a blind spot

```
1. THE SPECIES GOLDEN   every kind x every life-history quantity
   covers  pace_multiplier, basal_metabolic_rate_w, life_history
2. THE DRIVE PIN        rise_at across the four thermal values
                        ALREADY EXISTS as `rise_at_couples_heat_to_thirst_
                        per_metabolic_class`, and is already in the subfloor
                        roster, so it already runs every commit. It covered
                        Endotherm/Ectotherm/Autotroph and NOT Ametabolic;
                        the campaign extends it rather than building it.
   covers  the liveness reader and the four matches! sites
```

Instrument 1 alone would miss §3.3 entirely: `rise_at` produces no
species-level life-history quantity, so a mis-mapped `Autotroph` is invisible
to it.

**Ordering is the mechanism.** Both goldens are captured on `main`'s behaviour
and committed GREEN before the type is touched. The split then lands as a
second commit changing ~100 sites and moving zero golden bytes.

### 5.3 Positive controls — run and recorded, not predicted

```
C1  Autotroph -> Endothermic (instead of Unmodelled)
    species golden GREEN, drive pin RED on 3 kinds
    proves instrument 2 exists AND that instrument 1's blind spot is real
C2  Ectotherm -> Endothermic
    species golden RED on kobold, drive pin RED
    proves instrument 1 can fail at all
C3  Ametabolic -> (None, Heterotrophic), an unsanctioned pair
    the §4.4 pair table RED
    proves the 16-vs-4 guard is not decorative
```

**If C1 comes back green on both instruments, instrument 2 is broken and the
campaign stops until it is not.** A control is a measurement; each result is
recorded with its command, and a surprise is a finding rather than a thing to
re-run until it agrees.

### 5.4 Determinism

No new draw, no changed stream consumption order, no serialized type. Worlds
are byte-identical across this campaign by construction, and §5.2's goldens
are what demonstrate it.

## 6. Save-format and epoch consequences

**None.** See §3.2.

## 7. Non-goals

- **No fix to `BIO-autotroph-physics`.** The split states the lie out loud — a
  phototroph will visibly carry `thermal: Unmodelled` — and fixes nothing.
  Fixing it moves three kinds' life-history and every golden they touch; the
  enum's own doc says that fix is "deliberately NOT bundled" with changes that
  would destroy its attribution.
- **`shrieker` stays `Phototrophic`.** A fungus is not photosynthetic and this
  is a real corpus error (§3.4). It is a DATA claim; mixing it into a
  structural rename hides it. Recorded as a followup with the finding intact.
- **No consumer for `TrophicMode`.** Giving it a reader is rung 2 by
  definition.
- **No unification with `windows/locale`'s `EnergySource`.** Deferred; see the
  metaplan §5. One reason belongs here rather than there, because it is about
  THIS type: locale's enum is `{ Sunlit, Chemosynthetic, Geothermal }`, and
  `Sunlit`/`Chemosynthetic` name *how life captures energy* while `Geothermal`
  names *where heat comes from* — a different kind of answer. That is the same
  shape of conflation this campaign is removing from `MetabolicClass`.
  Unifying now would import a second conflation into the type we just
  decarbonized. Rung 2 may well fix locale's axis too; rung 1 must not inherit
  it.
- **No census refresh, no authored chemotroph kind.**

## 8. Task shape

```
T1  the two goldens, on main's behaviour        green before anything changes
T2  the types, the bijection, the pair table
T3  the rename, ~100 compiler-found sites
T4  is_ametabolic consolidates four spellings
T5  docs: the stale "two autotrophs" -> three; the registry rows
T6  the three positive controls, run and recorded
```

T1 lands and is green before T2 begins. T6 runs against the finished tree.

## 9. Provenance

Nathan, 2026-08-24, choosing approach (b) — "split the enum into the axes it's
actually conflating" — over adding a fifth variant to a mixed axis, and
approving §§1-4 section by section. §3.3's discovery (that the two readers
group `Autotroph` differently, so the naive split silently changes behaviour)
came from reading the reader inventory the blast-radius probe produced, and it
is the reason `Unmodelled` exists. §5.1's instrument-coverage measurement is
why this campaign builds its own goldens rather than trusting the drift check.
