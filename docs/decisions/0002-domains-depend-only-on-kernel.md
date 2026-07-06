# 0002. Domains depend only on the kernel

**Status:** Accepted (2026-07-05) · **Decider:** Nathan

In the context of an ever-growing set of world-simulation domains (astronomy,
climate, terrain, settlement, culture, religion, …), facing the N×M coupling
trap where every domain risks depending on every other, we decided that **a
domain crate depends on `hornvale-kernel` and nothing else — never another
domain**, accepting that all cross-domain enrichment must be expressed through
the kernel's shared vocabularies rather than direct calls.

**Context.** The ontology trap — rigid inter-domain dependencies that make the
system impossible to extend — has personally derailed prior projects. Agility
and clean extensibility outrank elegance, performance, and completeness.

**Consequence.** Adding a new domain must never require editing an existing
one. The layering `kernel/ → domains/* → windows/* → cli/` is enforced via
`Cargo.toml`. A single composition root (`windows/worldgen`) is the only place
all domains meet. Cross-domain communication goes through the trace protocol
([0003](0003-trace-protocol-is-the-only-cross-domain-channel.md)).

**See also.** Constitution Principle 6 (spec §2); `CLAUDE.md` "Architecture"
section.
