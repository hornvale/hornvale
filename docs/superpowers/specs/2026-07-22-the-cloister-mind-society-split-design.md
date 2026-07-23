# The Cloister — cleaving the individual mind from the society mind

**Campaign 4 of the Dragons program** (after 0064 The Assay, 0065 The Eremite,
0066 The Solitary Tongue). An architectural split, endorsed by Nathan as the
enabler for the two heavier campaigns that follow it (Perception, then
Placement): separate `PsychVector`'s six dimensions into the ones a lone
creature genuinely has (`MindVector`) and the ones only a society has
(`SocietyVector`), and let a solitary creature carry only the first.

Status: **spec, awaiting G3 review.**

## 1. Problem

The Eremite (0065) made sociality a first-class axis and let a solitary
creature — a dragon — carry a *mind* without being a settling people. But the
mind it carries is still `PsychVector`, a six-dimension bundle authored for the
peoples, and half of those dimensions are **society concepts that are
meaningless for a community of one**:

- `in_group_radius` — "how wide is *us*" — is undefined when *us* = the self.
- `sociality` (authority shape: Hierarchic/Communal) — a lone dragon has no
  authority structure to shape.
- `status_basis` (what earns standing: Rank/Knowledge/Generosity) — standing
  *among whom*?

The struct's own doc comments betray this: `threat_response` is documented as
"how a **society** answers threat," yet the vessel reads it as an individual
animal's flee-or-stand boldness (The Mettle). The type conflates two things.
For a dragon we paper over it by authoring plausible-looking society values
(`Hierarchic`/`Rank`/`in_group_radius` 0.05) that assert a social shape the
creature does not have. That is the last inheritance-flavored weld the Dragons
program set out to dissolve, one level deeper than The Eremite reached: not
"has a mind ⇒ is a people," but "has a mind ⇒ has a *society's* mind."

Cleaving the vector removes the papering-over, and — the reason to do it *now*,
before Perception and Placement — gives both of those campaigns a clean
solitary-mind boundary to build on, and establishes the baseline-fallback
pattern Perception will reuse for the goblin-baseline exposure stopgap.

## 2. The cleave is real — the consumer map already respects it

Sorting all six dimensions by which code reads them shows the split is not
arbitrary; it falls along an existing seam in the codebase:

```
INDIVIDUAL-MIND dims → MindVector    Read by (a lone creature's behavior)
  threat_response                    vessel liveness (The Mettle); settlement suitability
  deliberation_latency               vessel liveness (arbitration); voice formality
  time_horizon                       vessel liveness; settlement freshwater-pull

SOCIETY-MIND dims → SocietyVector    Read by (only-a-society machinery)
  sociality (authority)              chorus belief/doctrine; voice repetition
  status_basis (standing)            chorus belief; voice epithet; morph honorifics
  in_group_radius ("us")             chorus stance toward other peoples
```

The load-bearing observation: **the vessel — the entire game-liveness /
individual-NPC layer — reads exclusively the three individual-mind
dimensions.** Not one society dimension crosses into it. The society dimensions
are read only by the chorus (collective belief / authority) and the language
register. So a wild solitary dragon, driven by the vessel, needs *only* the
mind vector. The split makes the type state what the code already does.

## 3. Design

### 3.1 The two vectors

`PsychVector` is renamed `MindVector` and **shrinks** to the three
individual-mind dimensions; it stays the innermost mind carried by *every*
minded kind (dragons and peoples alike):

```rust
pub struct MindVector {
    pub threat_response: f64,     // flee 0 ↔ stand 1
    pub deliberation_latency: f64,// decision speed
    pub time_horizon: f64,        // immediate 0 ↔ generational 1
}
```

A new `SocietyVector` carries the three society-mind dimensions and is carried
**only by `Settled` kinds**:

```rust
pub struct SocietyVector {
    pub sociality: Sociality,       // authority shape
    pub status_basis: StatusBasis,  // what earns standing
    pub in_group_radius: f64,       // insular 0 ↔ expansive 1
}
```

`Sociality` and `StatusBasis` (the enums) move with the dimensions that use
them; `MindVector` becomes a pure `bare-ok(ratio)` struct.

### 3.2 Membership and the lattice

`WorldComponents` keeps the `psyche` store, now holding `MindVector`, and gains
a `society: ComponentStore<KindId, SocietyVector>` store (mirroring how
`perception` is already its own store). `psyche_registry()` keeps its name and
returns the `MindVector` store for all minded kinds; a new `society_registry()`
returns the `SocietyVector` store for the four settling peoples.

The Eremite's nested-capacity lattice (`check_integrity`) gains one clause:

> **`society` key-set == `Settled` key-set.** Every `Settled` kind carries a
> society vector; no non-`Settled` kind does. (Society is orthogonal to the
> speech ⊆ perception ⊆ mind chain — it is a capacity that only a
> community-forming kind possesses.)

Gregarious and Colonial kinds are not minded in v1, so the question of a
reduced pack-society vector is deferred (§7).

### 3.3 The baseline fallback (the mixed consumers)

Three consumers read *both* a mind dimension and a society dimension —
`voice_params`, `chorus` stance, and the settlement species-facts projection.
For a `Settled` people they read `Some(&SocietyVector)`. For a `Solitary`
creature the society vector is absent, and they fall back to a canonical
baseline:

```rust
impl SocietyVector {
    /// The goblin-baseline society reading, resolved for a Solitary kind that
    /// carries no society vector. Matches the goblin's authored values.
    pub const fn baseline() -> Self {
        Self { sociality: Sociality::Hierarchic,
               status_basis: StatusBasis::Rank,
               in_group_radius: 0.5 }
    }
}
```

This is deliberately the *same* baseline-fallback shape the Perception campaign
will use to replace the goblin-baseline exposure stopgap in `exposure_of_impl`
— The Cloister lays the pattern down once, on the simpler surface.

### 3.4 The dragons drop their society dims

The three chromatic dragons keep their `MindVector` (`threat_response` 0.95,
`deliberation_latency` 0.5, `time_horizon` 0.90 — an apex that stands, a
centuries-long hoarder) and carry **no** `SocietyVector`. Their previously
authored `Hierarchic`/`Rank`/`in_group_radius` 0.05 is deleted, not migrated:
a dragon relates to no society, so it asserts no society shape. Any consumer
that needs a society reading for a dragon resolves `SocietyVector::baseline()`.

## 4. Determinism & blast radius

**Save-format:** no new seeded draws; no stream label touched; **not an epoch.**
The world psyche-fact emission (`lib.rs:4770`) is already gated on `Settled`, so
dragons never emit psyche facts — dropping their society dims changes **zero
world bytes**. The peoples keep both vectors with identical values, so their
facts are unchanged. **No census regen.**

**Reference catalogue (the Draconic derivation):** predicted **byte-identical**,
by this mechanism — the only catalogue consumers of a dragon's psyche are
`voice_params` and `morph_options`, which among the society dims read only
`status_basis` and `sociality`; the dragons' authored values there
(`Rank`/`Hierarchic`) already equal `SocietyVector::baseline()`. The one dragon
society dim that differs from baseline (`in_group_radius` 0.05) is read by *no*
unplaced-path consumer — only `chorus` stance, which runs over *placed* peoples.
**This is a prediction, to be VERIFIED at execution** by regenerating and
diffing the dictionary / phonology / audio reference pages and the three
seed-42 almanacs — not asserted. If any byte moves, it is a consumer this map
missed, and it is investigated before the campaign proceeds.

**Consumer enumeration** (the recurring Eremite/Given-Word lesson — widening or
reshaping a shared type makes every reader the blast radius; enumerate at spec
time). Every reader of `PsychVector`, classified by what the split does to it:

| Class | Sites | Change |
|---|---|---|
| Type-rename only | `species` def + `impl Component` + tests; `components.rs` store/assemble/from_parts; `cli/phonology.rs` param; `cli/audio.rs` iteration | `PsychVector` → `MindVector` |
| Pure-individual (MindVector only) | `vessel/liveness.rs` (threat/latency/horizon); `worldgen/lib.rs` settlement suitability (`~658`) | read from `MindVector`; no society access |
| Pure-society (SocietyVector, baseline-fallback for Solitary) | `chorus.rs` `beta_of` / `doctrine_beta_of`; `worldgen/lib.rs` `morph_options` (`~3695`) | take a resolved `SocietyVector` |
| Mixed (both) | `worldgen/lib.rs` `voice_params` (`~3683`); `chorus.rs` stance (`~1239`); species-facts projections (`~4476`, `~7732`) | take `MindVector` + resolved `SocietyVector` |
| Settled-gated society emission | `worldgen/lib.rs` sfacts (`~4788`) | reads `society_registry()`; always present, no fallback |

`check_integrity` gains the `society == Settled` clause. The exact old→new text
for every site above is the plan's job (transcription-grade, as The Assay).

## 5. Model-card delta

The species chapter's psychology model card splits into a *mind* card (the
three individual dims, carried by every minded kind) and a *society* card (the
three community dims, carried only by settling peoples), and the dragon entry
gains a one-line note that a solitary carries a mind but no society vector, so
its society reading resolves to the baseline.

## 6. Test plan

- **Type/lattice:** `check_integrity` accepts a valid world; rejects a
  non-`Settled` kind that carries a society vector and a `Settled` kind missing
  one. `society_registry()` key-set == the four peoples.
- **Baseline fallback:** `SocietyVector::baseline()` equals the goblin's
  authored society dims; a mixed consumer given `None` produces the same output
  it produced for the pre-split dragon (Rank/Hierarchic reads).
- **Byte-identity (the load-bearing verification, §4):** a regen+diff harness
  over the dictionary/phonology/audio pages and the three seed-42 almanacs shows
  no drift; `make gate` green; census untouched (no regen run).
- **Vessel isolation:** an assertion that the individual-mind path compiles and
  runs reading only `MindVector` — the game layer never names `SocietyVector`.

## 7. Non-goals (deferred)

- **Dragon perception** — the next campaign; replaces the goblin-baseline
  exposure stopgap with a real dragon perception, reusing §3.3's fallback
  pattern.
- **Placement** (BIO-35 Stage 2 / the ANIMAL_PREY prey field) — the capstone;
  materializes dragons and the carnivore menagerie in worlds. Scope it around
  the *landscape-of-fear cascade* (regions peoples avoid; settlement pattern
  bending around apex predators), not a mere occupant token; census-regenerating
  (carve-out). Fix the ~7 `.expect("peopled pass over a fauna kind")` panics
  with baseline fallbacks on the way in.
- **Roster & Ages** — filling the sociality × lifespan periodic grid; best done
  with/after Placement so new kinds actually appear.
- **A reduced society vector for Gregarious (pack) minds** — no minded
  Gregarious kind exists in v1.

## 8. Proposed decision (ratify at close)

> **Decision 00XX — the mind/society vector split.** `PsychVector` is cleaved
> into `MindVector` (threat_response, deliberation_latency, time_horizon;
> carried by every minded kind) and `SocietyVector` (sociality, status_basis,
> in_group_radius; carried only by `Settled` kinds). A `Solitary` kind carries
> no society vector; consumers needing a society reading for one resolve
> `SocietyVector::baseline()` (the goblin-equal values). Not an epoch; worlds
> and the reference catalogue are byte-identical.
