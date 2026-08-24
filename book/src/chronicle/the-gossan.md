# The Gossan

*A gossan is the rusted cap over an ore body — the rock that says what is
underneath by oxidizing. It is a signal, not a resource.*

Nothing underground has anything to eat. Carrying capacity is computed from
insolation, so a people seated eight hundred metres down is, in the model, fed
by sunlight. The same gap keeps the sea's hydrothermal vents barren, and the
code says why in its own voice — `marine_forage_supply_field` gives a vent
near-zero productivity on purpose, with the reason written beside it:

> a real vent community is CHEMOTROPHIC, which is a metabolic class the enum
> does not have, so making it productive here would feed vent biomass to
> photosynthesis-based consumers.

That is a refusal, and it is correct. It is also the whole problem in one
sentence: **the type system could not express eating rock, so nothing anywhere
could do it.** This campaign made the type say the word, and changed nothing
else.

## One enum asking two questions

`MetabolicClass` had four variants — `Endotherm`, `Ectotherm`, `Autotroph`,
`Ametabolic` — and its own doc said its job was to select the allometric
normalization coefficient and the pace multiplier. Two numbers, both about how
fast a body burns. Here is what the function that selects the first of them
actually did:

```rust
Endotherm | Autotroph => B0_ENDOTHERM,
Ectotherm             => B0_ENDOTHERM * ECTOTHERM_METABOLIC_FRACTION,
Ametabolic            => return 0.0,
```

A phototroph grouped with a warm-blooded animal. The doc admitted the outcome —
the shipped autotrophs "are computed exactly as endotherms of the same mass" —
without naming the cause, which is that `Autotroph` is not an answer to the
question the enum asks. Endothermy and ectothermy are **thermal strategies**:
how a body regulates its temperature, which is a claim about *demand*.
Autotrophy is a **trophic mode**: where the energy comes from, which is a claim
about *supply*. The enum conflated a demand axis with a supply axis, and
allometry, having no supply question to ask, gave the supply value an
endotherm's numbers because it had nothing else to do with it.

The axis this program needs is the one the enum handled worst.

## The disagreement that named a fourth value

The obvious split maps each old variant onto a pair. Three of the four are
immediate. `Autotroph` is not, and the reason is that two shipped functions
already disagreed about which axis it lived on:

```text
basal_metabolic_rate_w:   Endotherm | Autotroph  => B0_ENDOTHERM
rise_at (thirst):         Autotroph | Ametabolic => base
```

One groups it with the endotherm. The other groups it with the thing that has
no metabolism at all. **No single thermal value preserves both**, and the first
draft of this campaign's design — mapping `Autotroph` to `Endothermic` on the
strength of the first grouping alone — would have silently given every treant
an endotherm's heat-driven thirst.

The honest answer is a value that says the modelling call was never made:

```text
ThermalStrategy          TrophicMode
  Endothermic              Heterotrophic
  Ectothermic              Phototrophic
  Unmodelled               Chemotrophic
  Absent                   Absent
```

`Unmodelled` means *has a metabolism whose thermal behaviour nobody has
modelled*. It is distinct from `Absent`, which means *has none* — and the
distinction was already load-bearing in shipped code, in the gap between
`Ametabolic => return 0.0` and `Autotroph => B0_ENDOTHERM`. It simply had no
name. Naming it makes the disagreement expressible rather than forced, and the
mapping bijective:

```text
Endotherm   ->  (Endothermic, Heterotrophic)
Ectotherm   ->  (Ectothermic, Heterotrophic)
Autotroph   ->  (Unmodelled,  Phototrophic)
Ametabolic  ->  (Absent,      Absent)
```

Which reduces the campaign to a sentence: **one axis renamed one-for-one, plus
a second axis nothing reads.** Byte-identity stops being an argument and
becomes a property of the structure.

## The instrument that did not exist

The claim "no number moved" needs something that could have said otherwise, and
the project's own drift check could not. Committed artifacts do carry
life-history numbers — lifespan, generation length, pace of life, basal
metabolic rate — but for **two kinds only**, goblin and kobold. The
`Unmodelled` and `Absent` paths have no artifact coverage at all; every other
appearance of `treant` in a generated page is a lexeme, in a dictionary or a
concept-gap table. Names, not numbers. And the census that carries even those
two regenerates only under its own flag, so a local drift check comes back
clean whether the mapping is right or wrong.

So the campaign built its own, in two pieces, because one piece has a blind
spot:

```text
1. THE SPECIES GOLDEN   every kind x every life-history quantity
2. THE DRIVE PIN        rise_at across all four thermal values
```

`rise_at` produces no species-level life-history quantity, so a thermal
mis-mapping that only moves thirst is invisible to the first. Both were
captured on the pre-split behaviour and landed green *before* the type was
touched; the swap then landed as a change to two hundred and twenty-one sites
that moved zero golden bytes.

Three controls asked whether any of it could fail. The one that matters gave
`Unmodelled` the endotherm's thirst model — precisely the defect the naive
mapping would have introduced — and the two instruments split exactly as the
design predicted:

```text
species golden  GREEN     <- blind, as claimed
drive pin       RED       <- catches it
```

A second control moved an ectotherm's coefficient and reddened the golden,
which is how a whole-table fixture proves it can fail at all. A third set a
kind to an unsanctioned pair and reddened the guard by name.

## What the split costs, and the guard that pays it

Four meaningful values became sixteen representable pairs. The type can now say
*no metabolism, eats other life*. Pretending otherwise would repeat the
original enum's mistake, so a declared table of sanctioned pairs fails on
anything outside the four — the same ratchet shape the trope check and the
type-audit waivers use.

That table has a second job, and it is the one worth stating. `TrophicMode` has
almost no readers: the campaign's own thesis is that an unread axis rots, and
the evidence is the enum it just replaced, whose doc recorded a variant
"witnessed by The Menagerie without the modelling decision ever being made" and
went on calling itself an unused seam for three campaigns afterwards. The pair
table is a **reader** — every kind's trophic value is consulted by an assertion
on every run of the suite, so a value that stops being true reddens instead of
drifting quietly.

`Chemotrophic` ships **declared and unwitnessed**: the variant exists, no kind
carries it, and a test asserts precisely that. The next rung's success condition
is that this assertion has to change.

## The one place the swap reproduced what it was removing

The mechanical rule — a read site of the old field becomes a read of the thermal
axis — held at every site but one. `prey_pressure_from` excludes autotrophs from
the prey base, because a plant is not a carnivore's prey, and that is a claim
about *supply*. Rewritten by the rule, it asked the thermal axis a trophic
question:

```rust
!matches!(bio.thermal_strategy, ThermalStrategy::Unmodelled)   // wrong axis
bio.trophic_mode != TrophicMode::Phototrophic                  // the question
```

Behaviour is identical today only because the three phototrophic kinds are
exactly the three unmodelled ones. It is a latent change armed by the very fix
that would give autotrophs a real thermal model: on that day the treants stop
being `Unmodelled`, silently re-enter the prey base, and prey pressure moves in
every world.

Fixing it gave the supply axis its first genuine production reader, which
falsified a doc comment in the campaign's favour.

## What shipped

A world can now say that a thing eats rock. No world does, and no number in any
world moved: the frozen life-history table has one commit in its entire history,
and it survived contact with thirty-two commits of `main` unchanged. The vents
are still barren, because a variant is not a food supply — what feeds them is
the next campaign, and this one exists so that campaign has a word to use.
