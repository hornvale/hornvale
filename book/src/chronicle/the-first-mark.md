# The First Mark

*The first time a player's own hand changed the world, and could be seen to have changed it.*

Until this campaign the possessed player was a ghost. The vessel let you
inhabit a minted agent and walk a frozen locale mesh — NPCs moved and felt
in a session-owned ledger clone, but your own acts touched nothing. You
could look, examine, ask *why*, and leave, and the world was byte-identical
whether you had come or not. The Casement made that walk live in a browser;
it did not make the walker *matter*. The First Mark is the first slice of
the game where the walker matters: a player commits a consequential act,
that act leaves a mark the world keeps, and the player can trace the
consequence back to their own hand.

The loop is small on purpose. Two opposed verbs, `provoke` and `soothe`,
commit the first **player-authored facts** — a signed `disposition-shift`
on a co-located NPC, distinguished from every fact a world system writes
only by its `provenance` string (`player: provoke`). Those facts accumulate
as that NPC's *grievance toward the player*: a pure additive fold, gated at
a fixed threshold. Antagonize someone across three days and, on the next
`wait`, they turn hostile — a discrete `turned-hostile` fact fires, pointed
back at the possessing agent, provenanced `player-provoked`. Put the world
down with `possess --out` and pick it back up, and the mark is still there;
the world remembers. Ask `why` of that NPC and the recount names your own
provocation and the hostility it grew into. Presence, consequence,
persistence, and the traceable wake — the whole thesis of the game, at its
smallest honest scale.

## The world at rest has no latent tension, so the mark had to *be* the tension

The design began elsewhere. The founding brainstorm framed the mark as a
*bias on a latent ambient tension* — the bugbear troop already hungry and
already near the village, the player's arrival only raising the odds an
already-loaded drama fires. *Bias latent tensions, never manufacture them*
was the honesty invariant, held emphatically. Building it revealed a fact
about worlds at rest that the framing had assumed away: **there is no latent
ambient tension at day zero.** Seed 42's settled NPCs sit three orders of
magnitude below their danger threshold; the one wild pack sits *above* it,
already firing. A drive is either satisfied or acting — a tension "just
below the threshold, mid-climb" is a dynamic state that only exists once
time is already moving. There was no near-threshold NPC for a mark to tip,
and any constant that made a cold one fire would have been manufacturing the
very drama the invariant forbade. The implementer refused to fake it and
stopped; the honest block was the right result.

The resolution was to see that two different channels had been conflated.
*Bias-not-manufacture* governs the world's **ambient** drama — the raid it
would have staged on its own — and that stays deferred, protected. But a
player antagonizing an individual until they turn on *the player* is not
ambient drama fabricated from nothing; it is **direct social causation**,
the player's own wake, exactly the "you can irritate people, you upset the
local balance" the game was always reaching for. So the mark stopped tipping
a drive and became the tension itself: grievance is one hundred percent
player-authored, which makes an un-provoked NPC byte-identical *by
construction* — the cleanest possible form of the additive-latent
discipline, an empty player-fact set that changes nothing because there is
nothing to change. The homeostatic drive layer was never touched.

## The determinism dividend, collected again

The mark cost the save format almost nothing. A player fact is an *ordinary*
fact — the `Fact` envelope already carried a `provenance` string, so
distinguishing "the player did this" from "the world did this" needed no new
field and no epoch; the two new predicates are purely additive, safe the way
a new seed-derivation label is safe. Forward integration stayed inside the
Lorenz guard-rail by construction: the consequence is a single *discrete*
committed fact, quantized at emit like any other, never a chaotic float
checkpoint, so a reloaded world re-derives from the trace rather than from
lossy state. The riskiest seam — could a reloaded played world re-fire its
consequence on a fresh `wait`? — is closed twice over: registering a
predicate is idempotent on an identical definition, and `next_entity` is
serialized, so a re-derived session mints fresh, higher ids whose NPCs carry
grievance zero, leaving the persisted consequence the only one. The whole
loop lives in `windows/vessel`; the CLI gained one flag.

What opens here is an arc, not a feature. This slice is the load-bearing
middle of the game's five-move spine — the quickening (presence forward-
integrates), the mark (the world remembers), and the butterfly (provenance
aimed at your own hand) — with the coarse spatial lens, the mode matrix, and
scale-of-possession all still ahead, layering on a foundation that no longer
has to be argued. For the first time, someone walks in the world and the
world is different for it.
