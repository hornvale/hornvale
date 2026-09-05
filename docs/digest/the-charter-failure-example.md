# Digest context

Revision: `e3355f441db045f0960576ead12b0f8acc56ca7d`

Working tree: **dirty**

## Thing domain registry (`hornvale.thing`)

Scopes: `domains/thing`

### Authored requirements

#### `hornvale.thing:registry-contract`

The Thing source roster, component registry, and composed concept registry agree in both directions, with each kind owned by Thing unless BORROWED cedes it to its declared owner.

Authorities/sources: `domains/thing/src/lib.rs (THING\_KINDS, BORROWED, thing\_registry, register\_concepts)`, `domains/settlement/src/lib.rs (register\_concepts)`, `domains/CLAUDE.md`

Evidence: Checked. Expected observations: `hornvale.thing:registration`, `hornvale.thing:component-roster`, `hornvale.thing:concept-ownership`. Result: **not satisfied**.

### Authored instructions

#### `hornvale.thing:maintainer-guide`

Consult `domains/CLAUDE.md` before editing the domain. `hornvale_thing::THING_KINDS` is the authored ordered roster and `hornvale_thing::thing_registry` supplies its component rows. A concept is owned by Thing unless `hornvale_thing::BORROWED` names another owner; the actual composition calls `hornvale_settlement::register_concepts` before `hornvale_thing::register_concepts` and inspects owners through `ConceptRegistry::concept`. These finite checks establish roster and concept-owner agreement only. They do not construct a world, inspect world-generation wiring, validate save compatibility, or establish portable, openable, lockable, placement, or other item behavior.

Requirements: `hornvale.thing:registry-contract`

Observations: `hornvale.thing:registration`, `hornvale.thing:component-roster`, `hornvale.thing:concept-ownership`

### Observations

#### `hornvale.thing:component-roster` — contradicted

Method: Compare hornvale_thing::THING_KINDS with hornvale_thing::thing_registry().ids() as sets in both directions

Subject: The authored Thing kind roster and canonical Thing component registry

Details: source roster: alcove, altar, anvil, bed, bench, brazier, cave-mouth, door, ground, hearth, high-seat, key, log, pool, screen, strongbox, threshold, vessel; component registry: alcove, altar, anvil, bed, bench, brazier, cave-mouth, door, ground, hearth, high-seat, key, log, loom, pool, screen, strongbox, threshold, vessel; missing from component registry: (none); extra in component registry: loom; duplicate source labels: (none); duplicate component labels: (none). Source roster order is retained as authored.

Requirements: `hornvale.thing:registry-contract`

#### `hornvale.thing:concept-ownership` — satisfied

Method: Use ConceptRegistry::concept for forward owner checks and ConceptRegistry::concepts for reverse inclusion of Thing-owned concepts

Subject: THING_KINDS, BORROWED, and the composed concept registry owners

Details: source roster: alcove, altar, anvil, bed, bench, brazier, cave-mouth, door, ground, hearth, high-seat, key, log, pool, screen, strongbox, threshold, vessel; declared borrowing: hearth->settlement; missing concepts: (none); wrong owners: (none); extra Thing-owned concepts: (none); borrowed labels outside source roster: (none); duplicate borrowing declarations: (none). Concepts owned by other domains and absent from THING_KINDS are outside the reverse Thing-owned comparison.

Requirements: `hornvale.thing:registry-contract`

#### `hornvale.thing:registration` — satisfied

Method: Call hornvale_settlement::register_concepts, then hornvale_thing::register_concepts, while catching a registration panic as refusal

Subject: The composed ConceptRegistry used to observe Thing concept ownership

Details: The supplied Settlement-then-Thing registration completed without error or panic.

Requirements: `hornvale.thing:registry-contract`

### Limits

- Observations cover only the named method and finite subject.
- Contributor checks are reviewed code and can themselves be wrong.
- Revision and dirty state do not establish reproducibility or an atomic source snapshot.
- This context does not authorize gate omission or approve changes to its governing rules.
