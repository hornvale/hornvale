# The Assay — potency as assayed might (from 5E Challenge Rating)

**Campaign:** The Assay — campaign 1 of the Dragons program (the warm-up)
**Registry:** re-derives `BiosphereTraits.potency` from a canonical 5E source ·
touches the sovereignty/coexistence buffering path only · unblocks nothing,
but establishes the `CR/30` authoring convention the program's later
age-ladder and roster campaigns reuse
**Status:** G3 draft (awaiting Nathan)
**Date:** 2026-07-21

---

## 1. Problem

`BiosphereTraits.potency` — the dimensionless "magical might" a creature
carries, `0.0` for a purely material one — is the `b·potency` term in
`kernel::sovereignty_floor(mass, potency)` (`kernel/src/ecology.rs`). A high
floor buys a creature freedom from environmental constraint: the mighty are
viable in few cells, so they place rare (MAP-22 / the coexistence stack —
"might buys cost and rarity, not ubiquity").

Every potency value in the roster is **eyeballed**. The three chromatic
dragons read red `0.95`, white `0.85`, black `0.85`; treant `0.6`; xorn
`0.5`. Two problems:

1. **The ranking is wrong even by the fiction.** By 5E adult Challenge
   Rating the chromatics rank red (17) > black (14) > white (13); the roster
   has white = black and treats all three as a near-tie at the top of the
   scale.
2. **There is no source.** `mass` cites "D&D 5E canon (kg)" in a comment and
   is defensible; `potency` cites nothing. It is the one "might" number in
   the model with no principled origin.

This campaign gives `potency` a canonical source and a documented rule,
fixing the ranking as a side effect. It is deliberately the smallest slice
of the Dragons program (§8): it changes numbers and a convention, not
architecture.

## 2. Background — why the blast radius is small

The shipped placement roster (the census, the seed-42 almanacs) is the **four
goblinoid peoples only**. Dragons, treant, and xorn exist in
`biosphere_registry` (`domains/species/src/lib.rs`) but are the **Stage-B
menagerie** — not placed in any committed world. `sovereignty_floor` only
bites at placement (`windows/worldgen/src/lib.rs:764`), so re-deriving the
mighty kinds' potency moves **no generated artifact today**; it changes the
buffering those kinds *will* receive once the menagerie is placed. The four
peoples carry `potency = 0.0` and stay there, so no census/almanac golden is
touched (§6 enumerates the true fallout — five literals and a few comments).

The immediate behavioral change is negligible even for the mighty: a red
dragon's floor moves `0.84 → 0.79` under the chosen mapping, because `mass`
(2700 kg) already supplies most of the buffering. This is therefore a
**semantic** change — giving might a source and headroom — not a
placement-tuning one.

## 3. Design

### 3.1 The source: Challenge Rating

`potency` is authored from the creature's **5E Monster Manual Challenge
Rating** — the single scalar 5E publishes as "how formidable," monotone and
defined for every monster. Among three considered sources (Campaign 1
decisions):

- **CR** (chosen) — canonical formidability, one number, correct ranking.
  Lightly double-counts `mass` (CR bundles physical + magical menace), an
  accepted cost for the warm-up.
- **Hit dice / HP** (rejected) — tracks size/toughness, which correlates
  hard with `mass`; heaviest double-count, least faithful to "might *beyond*
  body."
- **Magical residual** (rejected — deferred as a possible refinement) — CR
  with the mass-expected part regressed out; purest fit to potency's
  "beyond-mass" definition but needs an expected-might-from-mass model,
  larger than a warm-up.

### 3.2 The mapping: `potency = CR / 30`

Linear, anchored at CR's published ceiling (**CR 30**, the tarrasque), so the
whole 5E range maps onto `[0, 1]`:

```
potency = CR / 30      (CR from the 5E MM adult stat block)
```

Adult chromatics land as "mighty mortals," not near-gods — leaving headroom
above them for ancient wyrms (CR ~24 → 0.80) and deities (→ 1.0). The mapping
is deliberately **age-ladder-ready**: when a later campaign (§8, #4) adds
wyrmling→ancient categories, each age's CR slots straight into the same rule
with no re-derivation (e.g. red wyrmling CR 4 → 0.13, ancient red CR 24 →
0.80).

Rejected alternatives (Campaign 1 decision): *anchored-high* (pin adult
≈0.90, preserving today's band but leaving no room above an adult) and
*compressed sqrt* (adults ~0.7–0.75). Linear was chosen as the honest reading
of CR's own 0–30 range.

### 3.3 The supernatural set — who gets a nonzero potency

`potency > 0` **iff** the kind carries innate supernatural *might* in 5E:
dragon-type, or a fey/elemental whose magical nature confers resilience.
Mundane beasts, monstrosities, and minions — and the four peoples — stay
`0.0` regardless of their CR, because their formidability is physical (already
carried by `mass`) and potency is specifically the *magical* term. Every
roster kind's verdict:

```
kind             5E type          CR      potency = CR/30    note
--------------   --------------   -----   ----------------   ---------------------------
red-dragon       dragon           17      0.56667            adult chromatic
black-dragon     dragon           14      0.46667            adult chromatic
white-dragon     dragon           13      0.43333            adult chromatic
treant           plant (fey-ish)   9      0.30000            magical nature
xorn             elemental         5      0.16667            elemental
--------------   --------------   -----   ----------------   ---------------------------
owlbear          monstrosity       3      0.0                mundane; might is physical
woolly-mammoth   beast             6      0.0                mundane
giant-elk        beast             2      0.0                mundane
giant-goat       beast           1/2      0.0                mundane
otyugh           aberration      1/2      0.0                telepathy is a sense, not might
rust-monster     monstrosity     1/2      0.0                mundane
twig-blight      plant           1/8      0.0                minion, no might
goblin           humanoid        1/4      0.0                a people; not supernatural
kobold           humanoid        1/8      0.0                a people; draconic blood != innate magic
hobgoblin        humanoid        1/2      0.0                a people
bugbear          humanoid          1      0.0                a people
```

The one borderline is **otyugh** (aberration with limited telepathy). It is
kept at `0.0`: telepathy is a perceptual/communication trait, not the
magical might that raises the sovereignty floor. The rule keys on *might*,
not on non-material typing.

### 3.4 Authoring form — the literal cites its source

The stored field stays `potency: f64` alone. No `challenge_rating` field is
added to the physical model, and no CR→potency function runs at runtime — CR
is an offline authoring input, per "models author, dice roll" (decision
0009). To make the provenance self-documenting, the literal is written as the
division itself with a naming comment:

```rust
// adult red dragon — CR 17 (5E MM); potency = CR / 30
potency: 17.0 / 30.0,
```

`17.0 / 30.0` is an IEEE-exact division (not a transcendental), byte-identical
across platforms; quantize-at-emit is unaffected. This mirrors `mass`'s
"5E canon" comment and turns the CR into a reviewable, visible source rather
than a bare magic number.

## 4. Determinism

No stream draws, no consumption-order change, no new save-format labels. The
only mutated values are compile-time `f64` constants inside
`biosphere_registry`. `potency` is already a serialized field; its *values*
change but its *contract* does not. Because the mighty kinds are unplaced,
**no committed artifact's bytes change** — the CI drift check stays green
without a regenerate step. (When the menagerie is later placed, that campaign
owns its own goldens.)

## 5. Model card delta

`potency` moves from **drawn/authored-freehand** to **authored-derived**: a
committed constant, derived offline by a fixed rule (`CR/30`) from a cited 5E
quantity. It remains an authored input to `sovereignty_floor`, not a computed
runtime quantity. `mass` (already "5E canon") and `potency` (now "5E CR/30")
are thus both canon-sourced; the model card's species-traits row is updated to
say so.

## 6. Blast radius — the exact fallout

- **Values:** five literals in `biosphere_registry` (red/black/white dragon,
  treant, xorn) rewritten as `CR/30` expressions with comments. Eleven kinds
  unchanged at `0.0`.
- **Range-assertion tests (pass unchanged):** `domains/species/src/lib.rs`
  asserts `red-dragon/treant/xorn potency > 0` and `owlbear/rust-monster/
  goblin == 0` — all still hold (0.567 / 0.30 / 0.167 > 0).
- **`domains/species/tests/instance_lens.rs`:** reads treant's potency
  dynamically (assertion is value-agnostic); only the stale comment "(0.6)"
  needs updating to `(0.30)`.
- **Sovereignty/insolation tests:** `windows/worldgen/tests/insolation_probe.rs`
  computes `sovereignty_floor(def.mass, def.potency)` — verify it asserts a
  band, not an exact dragon float; refresh any exact expectation.
- **Lab synthetic roster:** `windows/lab/src/roster.rs:129`
  (`awakened_traits.potency = 0.6`) is an *awakened-tree* synthetic, not the
  shipped roster. Decision (self-resolved): retrack it to treant's new value
  (`9.0/30.0`) so the Lab fixture stays consistent with its canonical
  counterpart; it is not a save-format constant.
- **Stale value-citing comments:** sweep for `0.85`/`0.95`/`0.6`/`0.5`
  potency mentions in prose/comments and correct them.

## 7. Test plan

- **Ranking invariant (new):** a unit test asserting the derived potencies
  satisfy `red > black > white > treant > xorn > 0`, and that every non-set
  kind is exactly `0.0`. This pins the *rule's outcome*, not the raw floats,
  so it documents intent (measure-don't-narrate: the assertion must fail if
  the ranking regresses).
- **Rule-consistency (new):** assert each supernatural kind's potency equals
  its documented `CR/30` (the CRs live in the test as the source-of-truth
  table), so a future edit that breaks the convention reddens.
- **Existing suites:** the species range-assertion tests and the worldgen
  build/insolation tests pass with only the comment/band refreshes in §6.
- **Gate:** `cargo test -p hornvale-species` + `-p hornvale-worldgen`, then
  the full `make gate`. No census, no artifact regen (§4).

## 8. Non-goals (the rest of the Dragons program)

The reframe that opened this program — *"avoid drawing artificial distinctions
like saying dragons aren't people; the ECS exists because these don't fit an
inheritance hierarchy; rethink what it means to be intelligent and have
language for solitary/eremitic creatures"* — is **explicitly out of scope
here**. This campaign touches only the `potency` number. The program's later
campaigns (each its own spec):

2. **The Dissolution of Personhood** *(keystone, architectural)* — replace the
   "peopled cluster" coherence invariant (`windows/worldgen/src/components.rs`)
   with à-la-carte capacity components + a first-class **sociality axis**;
   dragons gain mind/perception/language as `Solitary`, with the
   demography/settlement flow gated on sociality rather than on tier
   membership.
3. **The Solitary Tongue** *(conceptual, rides on #2)* — what language *is*
   off-community: drift-rate as a function of sociality (a settled people's
   tongue splits into dialects; a solitary ancient's is near-frozen and
   idiolectic — the octopus↔dragon contrast).
4. **Roster & Ages** *(additive, rides on #2)* — age categories (wyrmling→
   ancient) as a mass+`CR/30`-potency ladder, and new kinds chosen by filling
   productive cells of the sociality×lifespan grid (captured in the idea
   registry), not by copying the Monster Manual.

Behavioral canon (lairs, hoards, legendary presence) is game-layer (the Walk /
possession seam) and is parked in the idea registry, outside this program.

### 8.1 Program conceptual backdrop (captured here; registry rows at close)

Two ideas surfaced brainstorming this program are recorded here so they are
not lost, to be promoted to `book/src/frontier/idea-registry.md` rows with
IDs computed at close (per the compute-IDs-at-close discipline):

- **Personhood is a region, not a node.** The "peopled cluster" invariant is
  an inheritance tier inside an ECS built to abolish tiers. The repair is to
  stop treating *person* as a type and treat it as a **region of
  component-space** (has-mind ∧ has-language ∧ settles). Dragons occupy a
  neighbouring region (has-mind ∧ has-language ∧ solitary); the capacities
  compose freely. This is the thesis of campaign #2.
- **The sociality × lifespan grid** (a roster *generator*). Two axes —
  community topology (solitary / pair / nomadic band / settled / colonial) ×
  lifespan (short / long / ancient) — enumerate "kinds of minded, lingual
  being," each cell fixing a language regime (a settled people drifts into
  dialects; a solitary ancient is near-frozen and idiolectic — the octopus↔
  dragon contrast). Empty cells are creature-design predictions. New roster
  kinds (campaign #4) are chosen by filling productive cells, not by copying
  the Monster Manual.

## 9. Proposed decision (ratify at close)

**00NN — potency is CR/30 from 5E canon, applied to the supernatural set.**
`BiosphereTraits.potency` is authored as `challenge_rating / 30` (5E MM adult
CR), nonzero only for kinds with innate supernatural might (dragon/fey/
elemental); mundane beasts and the peoples are `0.0`. CR is an offline
authoring input, not a stored field or a runtime computation. Supersedes the
freehand potency values shipped through the coexistence stack.
