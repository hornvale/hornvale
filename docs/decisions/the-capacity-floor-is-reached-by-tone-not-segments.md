# The capacity floor is reached by tone, not by adding segments to atonal species

**Status:** Accepted (2026-07-10) · **Decider:** Nathan

In the context of the phonology epoch guaranteeing each language enough
distinguishable syllables to carry its vocabulary, facing the choice of how to
raise a species that falls short of the floor, we decided that **the floor is on
distinguishable-syllable *capacity* (not segment count), and a species short of
it is raised by widening its *tone* inventory — never by adding
un-characteristic consonants; an atonal species is never widened at all, its
residual low capacity being the accepted realistic tail**, accepting that a few
cramped atonal worlds keep a small core-homophony tail we measure rather than
repair.

**Context.** A minimum-inventory-*count* floor would force a few-place species
(a future serpentine or avian people) to grow consonants it has no business
having — erasing the very character the articulation vector encodes. Capacity is
instead `|onsets| × |nuclei-fillings| × |codas| × |tones|`, and the same bar is
reachable through segments *or* pitch, so a few-place species "meets the bar with
pitch and keeps its character." The shipped humanoids are authored atonal
(`tonality = 0.0`); widening their inventory would change their names outside the
epoch reseed, contradicting the promise that their output changes only by the
reseed. Nathan's framing sharpened the accounting: homophony is not merely a
generation aesthetic — it taxes *comprehension*, and real speakers tolerate
homophones only because context disambiguates, which a deterministic sim cannot
lean on. But (as codon degeneracy shows) a collision is harmless when the
colliding meanings do not compete in the same context.

**Consequence.** `ensure_capacity_floor` widens a tone-capable species by
admitting contrastive tones until it clears the floor (`CAPACITY_FLOOR = 24`),
and leaves an atonal species untouched — its floor is the narrow
minimum-inventory guarantee (`MIN_CONSONANTS`) already applied. To keep "accept
the atonal tail" a *measurement* rather than a shrug, the Lab gains
`confusable-homophony-<species>`: the same-semantic-domain subset of core
homophony (two concepts a listener cannot separate by topic), whose complement
is FREE cross-domain homophony. Measured over 1,000 worlds, confusable is roughly
half of core (bugbear core 2.82 / confusable 1.34), so most of the residual is
free — the tail is defensibly benign. What we knowingly give up: a hard capacity
guarantee for atonal species (they may sit below the floor) and a hard
zero-core-homophony guarantee (the confusable tail is real, just small and
mostly non-competing). This is a metric change, not a generation-side sporadic
rule, so it stays constitutionally clean.

**See also.** The phonology-epoch spec (§2.3, §5, §10 Q3); decision 0011
(studies are data, metrics are code — the confusable split is a metric, the
domain partition is pack data); the sibling decision
`tonogenesis-is-a-regular-conditioned-merger-repair` (the tail's other guard);
`distinguishable_capacity` /
`ensure_capacity_floor` in `domains/language/src/phonology.rs` and
`confusable_homophony` in `windows/lab/src/metrics.rs`. Slug filename per
decision 0026.
