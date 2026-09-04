# The Housemark — a dwelling belongs to its people

**Campaign:** The Housemark, The Staple's R1 reading rung.  
**Status:** design for G3 review.  
**Epoch:** none intended; the artifact response is measured, not predicted (§7).

## 1. The claim

A player who has seen several dwellings made by one people should begin to
recognize that people's work in an unfamiliar dwelling. The signal is spatial:
where authority sits, whether the threshold turns inward or welcomes a guest.
It is not a species-colour palette, a room template, or a label in prose.

The system already carries the needed coordinate. `Brief::people` is populated
from the living occupation and is read nowhere in `windows/vessel`; The Staple's
five-seed probe measured 12–15 living peoples per world. The system also already
contains the architecture: The Hearth specified one authored pattern inventory,
per-culture derived selection, and one validator; The Blocking shipped that
inventory and composer. This campaign joins those two existing halves.

The smallest success is not fifteen unique houses. It is a compact, derived
grammar whose combinations are all reachable, whose output differs in the
chamber renderer, and whose cultural source can be recovered better than chance
from pattern relations alone.

## 2. The housemark

`Housemark` is a vessel-owned reading derived from a `SocietyVector`:

```rust
pub struct Housemark {
    pub authority: AuthorityMark,
    pub threshold: ThresholdPosture,
}

pub enum AuthorityMark { Command, Common }
pub enum ThresholdPosture { Inward, Plain, Outward }
```

The fields remain independent. There is no six-variant culture enum and no
species-name match. `Housemark::try_from_society` succeeds over admitted
society vectors and returns a contextual error elsewhere:

```text
sociality                    authority
--------------------------   ---------
Hierarchic                   Command
Communal                     Common

in_group_radius              threshold
--------------------------   ---------
0.00 ..= 0.35                Inward
0.50 ..= 0.60                Plain
0.65 ..= 1.00                Outward
0.35 < x < 0.50              REFUSE
0.60 < x < 0.65              REFUSE
outside 0..=1                REFUSE
```

The gaps are deliberate. `in_group_radius` is authored data documented as
insular 0 ↔ expansive 1; the three architectural bands are a new interpretation,
not a fact hiding in the scalar. Every one of today's fifteen rows falls inside
a band, and every `AuthorityMark × ThresholdPosture` cell has a witness. A new
row in a gap forces this policy back into view rather than inheriting an
accidental nearest band.

`status_basis` is not read. Rank → high seat looks plausible, but Knowledge →
loom and Generosity → water jar are occupational stereotypes rather than spatial
consequences. A consumer owes a reason for each field it reads; symmetry is not
one.

### The current six cells

```text
              INWARD                 PLAIN                     OUTWARD
Command       hobgoblin, drow        goblin, hill-dwarf,       gnoll, human
                                      high-elf
Common        kobold, bugbear        gully-dwarf, sea-elf,     desert-dwarf,
                                      snow-elf                  desert-elf,
                                                                wood-elf
```

This table is a measured roster claim, not a permanent cast list. Tests derive
it from `society_registry()` and assert cell coverage; they do not freeze which
proper names occupy each cell.

Verified on the design baseline by parsing the live registry and applying the
table above (the implementation test replaces this source probe):

```text
$ ruby -e '<parse society_registry; classify by the §2 bands; tally cells>'
Communal/inward: kobold,bugbear
Communal/outward: desert-dwarf,desert-elf,wood-elf
Communal/plain: gully-dwarf,sea-elf,snow-elf
Hierarchic/inward: hobgoblin,drow
Hierarchic/outward: gnoll,human
Hierarchic/plain: goblin,hill-dwarf,high-elf
rows=15 cells=6 missing=0 gaps=0
```

## 3. One inventory, culturally diagnostic relations

Universal necessities stay universal: ground, threshold, warmth where climate
requires it, and the room's functional role. The housemark only governs four
chamber-band readings:

```text
field       value      admitted relational pattern
----------  ---------  ---------------------------------------------
authority   Command    a command seat beside the entry threshold
authority   Common     a common bench beside the entry room's ground
threshold   Inward     the existing screen beside the entry threshold
threshold   Plain      no diagnostic threshold pattern
threshold   Outward    guest water beside the entry threshold
```

The two fields compose. An inward common dwelling has a screened threshold and
a common bench; an outward command dwelling offers water at the threshold and
places a command seat there. `Plain` is an intentional zero, not missing data:
the authority mark still makes the dwelling legible.

Three entries reuse existing kinds and relations:

- The screen already exists beside the threshold. Its locale-band admission
  remains unchanged; only its chamber-band cultural admission narrows.
- `HIGH_SEAT` already means “the seat that commands the entrance.” A new
  chamber-band pattern makes that relationship reachable in a threshold room;
  the existing hall pattern remains the hall's vocabulary.
- `VESSEL` already represents a water vessel. A distinct pattern name places
  guest water beside the threshold. Pattern names, not kind indices, are the
  stable selection keys.

One new object kind, `BENCH`, is added for common seating. It carries no new
affordance in this campaign: sitting is not yet a verb, and inventing an
interaction to justify a noun would reverse the dependency. Its pattern belongs
to the hearthroom and attaches beside ground. The distinction is relational —
common seating at the entrance versus a seat commanding it — not decorative
prose.

Both authority patterns belong to `Role::Threshold`. This is load-bearing:
`Structure.chambers` has length `1..=MAX_CHAMBERS`, so the threshold is the only
chamber every structure possesses. The housemark must survive substitution of a
one-chamber dwelling for a four-chamber one; size may change what lies beyond
the mark, never whether the mark exists.

Every cultural pattern is `at_locale: false`. The walk-band `selection()` path
does not receive a housemark and stays byte-for-byte governed by its existing
predicate. This is what keeps thermal history outside R1.

## 4. Admission, not a parallel composer

`Pattern` gains a chamber-only admission predicate:

```rust
pub enum HousemarkGate {
    Universal,
    Authority(AuthorityMark),
    Threshold(ThresholdPosture),
}
```

`selection_for` receives `Option<Housemark>` and intersects the existing role,
built, cold, populous, and dependency gates with the housemark gate. `None`
admits `Universal` only. The locale-band `selection` ignores `HousemarkGate`
entirely, so an existing `at_locale: true` pattern can remain universal at the
locale band while becoming diagnostic at the chamber band.

There is still one inventory, one order-sensitive `draw_from` walk, one
`compose`, and one `permits`. A second cultural inventory or a post-composition
mutation pass would create two sources of truth for admissibility and is
forbidden by the design.

`Housemark` is computed once with the `Brief`, not by rebuilding
`society_registry()` inside every chamber fold. The production `brief_of` path
resolves the living occupation's `people` to its society row and fails with
context if a placed people lacks one. Synthetic and unoccupied briefs carry
`None` explicitly. `Brief::people` remains present because it is the durable
identity and future consumers need not reverse a lossy two-field housemark.

## 5. Ownership and data flow

```text
domains/species
  society_registry(): KindId -> SocietyVector
                         |
                         v
windows/vessel::brief_of
  living occupation.people + society row -> Brief { people, housemark, ... }
                         |
                         v
windows/vessel::chamber_interior_of
  role + place gates + housemark -> selection_for -> compose -> Interior
                         |
                         v
                    chamber prose
```

The species domain owns what a people is; the vessel owns how that social
reading becomes architecture. No domain learns about rooms, patterns, benches,
or the vessel. No sibling-domain dependency is introduced.

`Housemark::from_society` is a pure function with no seed, stream, world time,
or place. Two dwellings occupied by the same people therefore share a vocabulary
even though their structure and local climate may differ. That repeatability is
the feature: a housemark that re-rolls per room cannot be learned.

## 6. Failure behavior

- A `SocietyVector` outside `[0,1]` or in an unassigned radius gap returns a
  descriptive derivation error naming the value and the two admitted neighboring
  bands. It is not rounded.
- A production occupation whose `people` has no society row fails at brief
  construction with the people id. The enforced settled-people registry should
  make this unreachable; the error preserves context if that invariant regresses.
- An unoccupied or synthetic brief has no housemark and draws only universal
  patterns. It does not silently receive `SocietyVector::MANIKIN`: the manikin
  is explicitly nobody, and assigning its house to ruins would invent occupants.
- Ordinary pattern dependency failure retains today's behavior: a pattern whose
  required kind is absent is skipped. The cultural layer receives no exception.

## 7. Determinism, epoch, and artifact branch table

The intended implementation changes only chamber derivation. `Interior` remains
unserialized under decision 0069, no stream is added or consumed, no worldgen
path changes, and the locale-band selection keeps its current arguments and
admission behavior. Under decisions 0084 and The Staple's standing rule, that is
a READING rung and not an epoch.

That paragraph describes the boundary; it does not predict generated output.
After the first integrated implementation, run the actual artifact commands and
classify the observed diff:

```text
observed result                                      response
---------------------------------------------------  --------------------------
only chamber-rendering transcripts move             RE-PIN; no epoch
world JSON, ledger facts, census or lab goldens move STOP; misclassified rung,
                                                     bring to Nathan as epoch
no committed artifact moves                          keep only if the tests and
                                                     direct readout prove the
                                                     feature is reachable
```

Appending `BENCH` and the cultural pattern rows is permitted only at the end of
the inventory and only with `at_locale: false`. Insertion or reordering is an
epoch under the inventory's own rule and is outside this design.

## 8. Preregistered acceptance claims

The implementation plan begins with a probe/test surface before production
code. These claims are frozen here.

### H1 — the derivation has real support

Across the live `society_registry()` roster:

1. every society row derives successfully;
2. all six `AuthorityMark × ThresholdPosture` cells have at least one witness;
3. no cell contains all peoples and at least three cells contain more than one,
   guarding against both collapse and name-keyed singleton masquerade.

If any condition fails after absorbing current `main`, stop and revise the
bands before implementation. Do not weaken the assertions to fit the roster.

### H2 — culture changes relations, not necessities

For one otherwise-identical built, warm threshold room in each of the six cells:

1. ground and threshold remain present in every threshold room;
2. the authority pattern matches the row field;
3. the threshold pattern matches the column field;
4. every composition passes `permits`;
5. no composition contains two anchors of one kind.

The test is a cross-product generated from the enums, not six copied fixtures.

### H3 — the living surface is distinguishable

Over real living occupations from at least the five Staple seeds, render the
threshold chamber and reduce it to the ordered multiset of
`(kind, relation-to-required-kind)`. The housemark cell must be recoverable from
that structure at **100%**. This is stricter than people recovery because several
peoples intentionally share one housemark; asking architecture to recover the
proper name would contradict the compression.

The null is actionable: if a cell cannot be recovered, the pattern set is
decorative or hidden behind unreachable roles. Add no prose labels to rescue it;
change the relations or reject the design.

### H4 — the reading boundary holds

The locale-band selection census is exactly unchanged across every existing
`(built, cold)` combination. A mutation test makes one cultural predicate admit
the wrong housemark and proves H2 or H3 fails; it must first assert that the
target predicate exists so the mutation cannot pass as a no-op.

## 9. Testing and review surfaces

- Unit tests beside the housemark derivation cover every band boundary, both
  refusal gaps, out-of-range values, and the six-cell roster census.
- Pattern tests generate the housemark cross-product and extend the existing
  “no duplicate kind,” connectivity, role differentiation, and census tests.
- Brief tests prove a living people receives the society-derived housemark and
  an absent occupation receives none.
- One ignored world readout reports the five-seed living distribution and the
  rendered two-chamber signatures. It is rostered as a world builder rather than
  smuggled into the commit gate.
- The implementation runs focused vessel tests while iterating, then
  `make gate-commit`. Stage and merge gates remain canonical-box work.

## 10. What this campaign does not do

- No per-species pattern table and no promise of fifteen unique styles.
- No cultural change through time, borrowing, conquest, hybrid households, or
  preservation of an ended occupation's style. The live brief does not contain
  evidence for those claims.
- No district vocabulary (The Precincts/R3), structure topology (R2), materials,
  construction process, or building extent.
- No locale-band pattern change and no committed chamber mark.
- No sitting, hospitality, drinking, ownership, or new object affordance.
- No use of `status_basis` until a spatial consequence can be justified.

## 11. Record consequences

- `SOC-staple-ladder` advances only its R1 slice and points to this spec.
- `CLIENT-district-patterns` remains unbuilt; housemarks provide one future
  district axis but do not supply a district subject or composer invocation.
- The new `BENCH` kind is registered through the ordinary object roster and
  gains a handle only because the pattern and tests name it.
- If the implementation validates the design, decision 0746 records that a
  cultural architecture is a derived, modular admission signature over one
  shared inventory, never a people-indexed catalogue.

## 12. G3 flags

1. **No epoch is intended, but the classification is conditional on measured
   artifact output (§7).** Any world, ledger, metric, or census movement stops
   the campaign and returns as an epoch/fidelity decision.
2. **The radius bands are new authored policy (§2).** They are intentionally
   discontinuous around today's empty intervals and fail closed when a future
   people lands there.
3. **Culture is read from the living occupation only (§6, §10).** A ruin or
   conquered dwelling does not preserve its maker; this campaign refuses to
   invent that history from a current or absent occupant.
4. **One new object kind has no affordance (§3).** `BENCH` exists as spatial
   vocabulary only; interaction waits for a verb-led campaign.

## 13. Provenance

Derived from The Staple R1, The Hearth §6, The Blocking's shipped composer,
decisions 0021, 0069, 0084, and the live fifteen-row society registry on
2026-09-04. Ideonomy: two G1 passes (one material boundary addition, no
overturn), then one axis-selection pass (one representational improvement, no
overturn). Full rulings and discarded branches are in the campaign ledger.
