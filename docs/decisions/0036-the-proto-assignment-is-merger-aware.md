# 0036. The proto-root assignment is merger-aware (zero core homophony)

**Status:** Accepted (2026-07-11) · **Decider:** Nathan

In the context of core concepts still colliding onto one modern word-form after
the injective proto-root assignment (epoch `root/v2`) — because a shared proto,
distinct at the proto level, can still merge once a daughter ages it through its
own lossy cascade and nativizes it, and tonogenesis repairs this only for
tone-capable species while the shipped peoples are atonal — we decided that
**the family proto-root assignment is merger-aware (epoch `root/v3`): a core
candidate is rejected not only when its proto form collides, but when its
*evolved* form would land on any already-placed core concept's evolved form in
any daughter**, accepting that the family assignment thereby depends on the
daughter roster.

**Context.** Core homophony is the functional-load-restricted number the
campaign targets at zero; the confusable (same-domain) subset is the
parsing-costly part. Measured over the shipped goblinoid family, the residual
was real — 179 of 200 worlds carried some core collision, bugbear worst. The fix
had to stay inside two invariants the earlier options broke: cognate descent (a
shared proto per concept, so a per-daughter re-draw is out) and Neogrammarian
regularity (no sporadic/lexical rule, so a recorded post-hoc dissimilation is
out). The merger-aware assignment satisfies both: `evolve` is untouched (every
rule still applies uniformly), and the proto stays shared — it is merely *chosen*
to survive the cascade distinct, the way a namer picks a name that reads
cleanly. Because every daughter's cascade is a pure, known function, the check
is a deterministic filter on candidates, not a new rule. Nathan chose the
stronger of two targets once the de-risk showed the cost was trivial: zero *all*
core homophony (domain-agnostic), not merely zero confusable.

**Consequence.** `assign_proto_roots` takes the family's daughters (their
cascades and phonologies); the composition root assembles them
(`worldgen::family_daughters`) and feeds the same set to `build_lexicon`, the
proto-goblinoid page, and the Lab's monophyly/outgroup metric, so all agree.
Measured (200 seeds × 27 core concepts): core-homophony pairs 791 → 0, worlds
with any core pair 179/200 → 0/200, mean core-root length 5.813 → 6.003
(+3.3%). The number Nathan targets is now exactly zero for every shipped people
on every seed, asserted as a Lab invariant. What we knowingly give up: the
assignment is now **roster-dependent** — adding a future daughter to a family can
reshuffle some core protos, weakening the insertion-stability the `root/v2`
assignment prized (a new concept still appends without reshuffling; a new
*daughter* may not). This is the inherent price of a cross-daughter guarantee,
and the cheaper roster-independent alternative (a proto-level structural
rejection) could only approximate zero, not guarantee it. Periphery mergers are
left alone — incidental homophony there is realistic linguistic texture, not a
defect.

**See also.** The phonology-epoch spec
(`docs/superpowers/specs/2026-07-10-phonology-tone-tonogenesis-design.md`);
0034 (the tonal-species repair
this generalizes to atonal peoples) and
0035; decision 0011 (studies are
data, metrics are code — the confusable/core split is measured);
`assign_proto_roots` in `domains/language/src/etymology.rs` and the Lab test
`core_homophony_is_zero_for_every_daughter_under_the_merger_aware_assignment`.
Slug filename per decision 0026.
