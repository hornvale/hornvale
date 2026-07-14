# 0047. A `demography` domain is extracted from `settlement`

**Status:** Accepted (2026-07-13) · **Decider:** Nathan

In the context of population moving from a per-site draw to a physically
grounded field (the-gathering, MAP-7's field half), we decided that
**population-as-field is a distinct object from named-places** and gets its
own kernel-only domain crate, `domains/demography`, sibling to `settlement`
rather than a module inside it.

**Context.** `settlement` owned three different things at once before this
campaign: the naming/ledger-commit machinery, the ad-hoc suitability
formula that scored cells, and the greedy scatter that placed sites. The
suitability formula and the scatter it drove are retired (decision 0048);
what replaces them — a carrying-capacity field, an up-gradient flow
accumulation, and the conserved condensation that reads settlements off it —
is a different kind of computation (field derivation and flow, not naming
and ledger presentation) and a different kind of future (temporal
relaxation, era-ticked history, species dispersal all read this crate, none
of them touch naming). Keeping it inside `settlement` would have grown one
crate to own an ever-widening mechanism family; splitting it now, while the
seam is small, is cheap. `demography` depends on `hornvale-kernel` and
nothing else (the domain-layering rule, decision 0002): the composition
root (`windows/worldgen`) assembles the bare per-cell climate/terrain inputs
both crates need and hands them to each; `demography` and `settlement` never
depend on one another.

**Consequence.** `settlement::genesis` keeps the ledger-commit surface
(naming, the four settlement predicates, presentation) untouched, so every
downstream consumer (almanac, locale, historiography) sees no schema change.
`demography` owns `carrying_capacity.rs`, `flow.rs`, `condense.rs`,
`founder.rs` (decision 0049), and `render.rs`, plus the in-memory
`DemographyReport` the composition root and Lab metrics read. This is
additive — it supersedes nothing — and gives the later history campaign
(temporal relaxation, founding/growth/abandonment predicates) a domain to
build in without touching `settlement` at all, matching the frontier's
domain map (`DOM-4`).

**See also.** Spec: `docs/superpowers/specs/2026-07-13-population-field-design.md`
§1; decision 0002 (domains depend only on the kernel); decision 0048 (the
suitability scatter this split makes room for retiring); decision 0049 (the
founder floor that migrates into the new crate).
