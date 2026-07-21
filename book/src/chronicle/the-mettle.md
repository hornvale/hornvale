# The Mettle

The Dread had given every creature the same fear. A cursed grove read the same to
a bugbear as to a timid thing; threat was threat, and all fled it alike. But
courage is a temperament, and two creatures on the same dangerous ground do not
feel it the same. This campaign added the third psychology dial, **boldness**, and
let a creature's mettle scale the fear that The Dread computes.

The dial cost almost nothing to find, because it was already there. A species'
`PsychVector` has carried, since long before any drive existed, a
`threat_response` — "flee at zero, stand at one" — authored per people and read
only by the culture layer, where a society's willingness to stand its ground
shapes the authority it builds. This is exactly how the first two dials arrived:
`deliberation_latency` and `time_horizon` were banked in that same vector for
campaigns before the drive layer reached up and read them as individual
temperaments. Boldness is the third of the trio, the same datum read at a smaller
scope — not how a society answers threat, but how one creature does.

The shape of the dial came from a negation. The obvious model — boldness *reduces*
fear, from steady down to fearless — is a one-sided thing, and negating it (what
would *amplify* fear?) revealed the pole it omits: a **coward**, who feels more
dread than is actually there and flees safe shadows. Fear, it turns out, is not a
scale from full down to none but an axis about a *center* — the point where a
creature feels a threat exactly as large as it is. Below that center lies the
coward; above it the bold, who feels less; further still the fearless, who feel
nothing. The scaling that lands the whole axis at once is a single line —
`effective = threat × 2·(1 − boldness)` — centered on one half, where the steady
creature, and the goblin who sits exactly there, feel the world unchanged.

Only the *feeling* is scaled, never the ground. The hazard's own gradient — the
push away from a worse cell that lets fear reshape a thirsty creature's path
around danger — is the world's physics, and it is left alone; what boldness
changes is how loudly the creature's mind weighs it. So a bold creature, feeling
less, weighs the hazard less, and will cross ground a timid one flees to reach the
water beyond it. The risk appetite falls out of the arbitration already there; no
new logic decides it. And because the whole thing is centered on the steady
baseline, the goblin who sits at that center, and every beast that inherits it by
default, feels exactly as it always did — the world is byte-for-byte unchanged for
them, and even the possessed hobgoblin, bolder than steady but never once near a
cursed place on its daily rounds, walks its transcript out unaltered.

There is a far shore to this axis the campaign did not sail to. Past fearless lies
a creature whose fear has gone *negative* — one drawn *toward* the hazard rather
than away, the moth to its flame, the berserker, and one day the hunter who does
not fear its prey because its prey is a meal. That is the reckless pole, and it is
the seed of predation-approach; it waits, reserved, for the food web to give a
creature something worth being drawn toward. For now the dial spans coward to
fearless, and the world has, for the first time, creatures that are braver and
creatures that are more afraid than their neighbours on the very same ground.
