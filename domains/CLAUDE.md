# CLAUDE.md — working in `domains/`

A domain models one slice of the world (astronomy, climate, terrain,
language, …). The layering that keeps them composable is constitutional and
enforced by `cli/tests/architecture.rs`. Read the root `CLAUDE.md`
"Architecture" and "Determinism" sections; this adds the domain-author rules.

## The one rule that must never bend

**A domain crate depends on `hornvale-kernel` and NOTHING else — never
another domain.** No `use hornvale_climate::…` inside `hornvale_terrain`.
Cross-domain communication happens only through the kernel's trace protocol:

- **Facts** — subject/predicate/object, append-only, contradiction-checked
  against the registry. Register your predicates per-domain; one concept
  name, one owner (decision 0025).
- **Phenomena** — the universal read: salience-ranked observations. A consumer
  (religion, say) must never be able to tell which system produced a
  phenomenon.
- **Fields** — typed functions over (space × time), the statistical prior.

If two domains need to share code, it goes down into the kernel (decision
0044), not sideways. Adding a new domain must never require editing an
existing one — that is the test of whether the boundary held.

## Determinism is stricter here than anywhere

Domains are where world-state is drawn, so the save-format contracts bite
hardest:

- **Every domain has a `streams.rs`** of `pub const` seed labels — permanent
  contracts. New label = safe; changed/reused label = an **epoch** (`/v2`
  suffix), never a rename. Retired labels are listed and never reused.
- **Stream consumption order is a contract.** A pin must consume the same
  draws in the same order as the unpinned path. The `*_properties.rs`
  integration tests (e.g. `genesis_properties.rs`, `tectonic_properties.rs`)
  hold the **pin-isolation** tests that catch a violation — run them after any
  change to a drawn quantity.
- **Coarse constrains fine, and epochs replace, tiers refine** (decision
  0039). A higher-fidelity provider may *refine* a coarser one but must never
  *contradict* it; a generator that contradicts an existing one is a new
  epoch, not a coexisting tier.

## Providers live at the composition root, not here

A domain defines its logic and its provider *trait*/implementations, but the
place where providers are *constructed and wired* is `windows/worldgen` — the
composition root. Don't reach across to build another domain's provider.

## Provider tiers coexist

The tier-0 constant provider (e.g. `ConstantSun`) and the generated one are
both valid; a world chooses. Keep tier-0 paths byte-identical when you touch
shared code — the `strongest`/`None`-branch comments in `terrain/crust.rs`
are a live example of code kept "instruction-for-instruction" unperturbed to
protect a byte-identity contract.

Subdirectories with extra guidance: `terrain/` (the most determinism-sensitive
domain).
