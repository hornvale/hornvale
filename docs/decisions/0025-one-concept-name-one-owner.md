# 0025. One concept name, one owner

**Status:** Accepted (2026-07-10) · **Decider:** Nathan

In the context of the concept registry (decision 0024's campaign, The
Words), where the `ice` biome's kebab name collided with climate's `ice`
substance concept — two domains' natural names for one word — we decided
that **concept ids are a single global namespace with exactly one owner per
name: the first semantically-broader registrant wins (the substance, for
`ice`), and any other domain whose natural name for a thing collides maps
to the existing concept instead of minting a homonym**, accepting that a
concept's `kind` reflects its owner's framing (Substance, for `ice`) even
where a second domain would have framed it otherwise (Terrain, for the
biome).

**Context.** Registration is order-independent by construction — each
registrant checks `registry.concept(name).is_some()` before registering —
so the rule is enforced mechanically at every `register_concepts` call, not
by convention. Semantically this is the right collapse, not a workaround: a
people steeped in ice-the-terrain knows ice-the-stuff; one word for one
referent is how the lexicon treats every other concept, and a homonym pair
(`ice`/`ice-biome`) would poison glosses, exposure classes, and the
dictionary with a distinction no speaker of the language makes. The
resolution shipped in The Words as a code comment and a pinned test
(`domains/climate/src/lib.rs`, `concepts_registered`); this record promotes
it from incidental to ratified.

**Consequence.** Future domains registering concepts must check-then-map,
never mint a suffixed homonym (`ice-biome`, `ice-2`). If two domains ever
genuinely need distinct concepts behind one surface word (true homonymy —
a `bank` of a river vs. a `bank` of trust), that is new information and a
new decision: the registry's names are meanings, not spellings, and the
collision should be resolved by renaming one meaning, not overloading one
name.

**See also.** Decision 0024 (settlement-name uniqueness is reference-time);
The Words spec §3 (`docs/superpowers/specs/2026-07-09-campaign-the-words-design.md`).
