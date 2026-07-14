# 0049. The MAP-22 founder floor migrates to `demography`

**Status:** Accepted (2026-07-13) · **Decider:** Nathan

In the context of extracting `demography` from `settlement` and retiring the
suitability scatter (decisions 0047/0048), we decided that **the founder
floor — MAP-22's pigeonhole guarantee that every people keeps its single
strongest basin even where outcompeted — moves with the field it floors
over**, from `settlement/placement.rs` to `domains/demography/src/founder.rs`,
its behavior and test battery preserved.

**Context.** The founder floor was authored in The Branches (`LANG-5`) as
the in-scope unblock for a Pareto-dominance defect: a stronger authored
people could shove a weaker one off the map entirely under pure competitive
placement. It is an *allocation* rule — reserve each people its best
attractor before culling — and allocation belongs beside whatever it
allocates over. Under the retired mechanism that was suitability scores;
under flow-condensation it is attractors of the population flow. Only the
floor's inputs change (attractors instead of suitability-ranked cells); the
guarantee itself — and the reason it exists — is unchanged, so this is a
migration, not a redesign. It also gave the campaign its answer to a sharp
edge the calibration surfaced: a founder-floor attractor whose catchment
falls below the concentration threshold, or even below one whole person,
must never commit a peopleless settlement. `demography::founder` floors
that catchment at population 1 as a domain invariant (a unit test guards
it) — the one deliberate, documented exception to the field-level
conservation exactness (decision 0048), since the founder floor already
bypasses the threshold and so was never conserved-exact.

**Consequence.** `settlement/placement.rs` (and its suitability-driven
founder-floor tests) is gone; `founder_pass`'s logic and battery live in
`domains/demography/src/founder.rs` as `condense_tagged`, exercised against
`CellMap<f64>` carrying-capacity fields rather than suitability scores.
The founder floor is the *only* piece of the MAP-22 coexistence stack this
campaign keeps — full cross-species competitive exclusion, footprint-scaled
home ranges, and the rest of that stack's mechanisms are the MAP-22
campaign's own scope, building on this field.

**See also.** Spec: `docs/superpowers/specs/2026-07-13-population-field-design.md`
§1 ("Founder-floor migration"), §5 ("No peopleless settlements"); the MAP-22
coexistence-stack spec
(`docs/superpowers/specs/2026-07-13-the-coexistence-stack-design.md`);
decisions 0047/0048.
