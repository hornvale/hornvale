# 0034. Tonogenesis is a regular conditioned sound change, not a homophony patch

**Status:** Accepted (2026-07-10) · **Decider:** Nathan

In the context of the phonology epoch adding a tone tier to repair the
post-evolution homophony the injective proto-root assignment (epoch `root/v2`)
left standing, facing the Constitution's ban on sporadic/lexical rules
(`evolve`'s founding invariant: *no rule is ever sporadic/lexical; every rule
applies uniformly wherever its conditioning environment occurs*), we decided
that **tonogenesis is a regular conditioned sound change — "a syllable that lost
feature X acquires tone Y," applied to every syllable whose environment matches,
collision or not — and homophony reduction is a consequence, never the
trigger**, accepting that where two roots lost the *same* conditioning feature
they still merge (real tonogenesis does not resolve every homophone either).

**Context.** Two rejected alternatives each broke a constitutional invariant.
Option A — record which words collided and dissimilate them — is a *sporadic*
lexical rule, forbidden outright. Option B — re-assign a colliding daughter's
proto-root — breaks cognate descent (the shared proto is what makes
`monophyly-goblinoid` and the divergence metrics true). Tonogenesis threads the
needle both missed: the rule reads the voicing of the consonant a *prior*
merging rule (`ClusterSimplify` onset-drop / `FinalLoss` coda-drop) removed —
from the derivation's own step history, never a stream draw — and writes the
corresponding tone (voiced-loss → Low, voiceless-loss → High, the historically
attested direction) onto the stranded nucleus. It is a pure, replayable function
of `(proto, cascade, ph)`, so `lexicon-regular-*` holds by construction and the
proto is untouched (cognates intact). This is diachronically real: tone in
Chinese and Vietnamese arose exactly this way, from lost segmental contrasts.

**Consequence.** Homophony reduction is a *byproduct*: two roots the segmental
merger would collapse stay distinct **iff** they differed in the tonogenetic
conditioning feature. The rule is subject to the same codomain constraint as
every other — the toned vowel takes only if it is in the phonology's inventory —
so an atonal language (Neutral-only vowels) sees it as identity, and the shipped
peoples' output changes only through the epoch reseed, never by gaining tone.
What we knowingly give up: tonogenesis is not a homophony *guarantee*; a
same-feature-loss collision survives, and that residual is the accepted realistic
tail (see the sibling decision, 0035).
`Tone::Mid`
is authored but banked — voicing is one bit, so the rule writes only `High`/`Low`
— awaiting a future three-way conditioning source.

**See also.** The phonology-epoch spec
(`docs/superpowers/specs/2026-07-10-phonology-tone-tonogenesis-design.md`, §2.2,
§4); decision 0009 (models author, dice roll — the per-species tone propensity
is authored, the assignment seeded); the `RuleKind::Tonogenesis` docs and the
`tonogenesis_preserves_a_contrast_two_roots_would_otherwise_lose_to_a_merger`
test in `domains/language/src/etymology.rs`. Slug filename per decision 0026.
