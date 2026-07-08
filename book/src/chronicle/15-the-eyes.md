# Campaign 15: The Eyes

**July 2026 · 12 commits · outcome: complete, merged — the plain sequence
number decision 0017 promised at the next campaign boundary, planned and
built under the working name Y2-2; gives the world's two peoples their own
eyes**

## What was attempted

Campaign 14 gave the world a second people, but left it looking at one
undivided sky: every phenomenon the trace protocol offered was read
identically by every observer, so a goblin and a kobold standing in the
same valley noticed exactly the same things in exactly the same order —
salience was a property of the sky, not of the eye looking at it. The
frontier map had been blunt about the cost: an observer-independent
salience can only ever grow one religion, however many peoples a world
places, because "what is most noticed" was never a question with room for
a second answer. The Eyes set out to make salience a function of the
observer, and to cash that mechanism in the hardest, most legible way
available — a second pantheon, grown from the same sky, by a species that
was never designed to see it the way the first one does.

## What landed

**Two knobs, kept strictly apart.** Perception splits into what is seen
and what demands attention, and the campaign never let the two questions
answer each other. The characteristic hour decides what is seen: it is the
moment a species actually observes the sky, found by a deterministic scan
rather than assumed, so a night-stars phenomenon is only ever offered to
an eye that is actually standing in the dark. The lens decides what
demands attention, once something has been seen: three multiplicative
weights, one per venue — day sky, night sky, and the ambient world that is
always present but never watched — built from a species' own perception
vector. A species' perception is authored at exactly three dimensions, no
more: an activity cycle, a night-vision scalar, and a sky-attention
scalar, the same closed-vector discipline the psychology substrate already
kept at six.

**Identity at the goblin baseline, in the strongest sense a deterministic
simulation offers.** Every lens formula is constructed, not tuned, so that
goblin's authored values — diurnal, 0.5, 0.5 — reduce every weight to
exactly 1.0. The code path that applies an identity lens performs no
multiplication and no rounding at all, so a goblin-only world's observed
phenomena are byte-identical to a world generated before perception
existed. Kobold's lens, read off the same Dungeons & Dragons 5th Edition
System Reference Document that already gave the species its psychology,
comes out to `(0.52, 1.82, 0.70)` — a night sky that outweighs a goblin's
day sky by nearly two to one, which is the entire design intent of a
nocturnal, night-sighted, sky-attentive people stated as three numbers.

**Venue, declared by the producer that already knows it.** A phenomenon
now carries, alongside its kind and its period, where it lives — the
sun declares itself day-sky whether it is rising and setting or fixed
forever above a locked world's day side; the moons and the night-stars
declare themselves night-sky; the felt turn of the seasons and the ambient
air declare themselves always-present. This is character of exactly the
kind a phenomenon's period already was, not a new channel for one system
to learn about another — the trace protocol's central promise, that a
consumer never learns which system produced what it observes, survives the
campaign untouched.

**Religion, run twice, through two different pairs of eyes.** The
composition root now calls religion once per species that placed a
flagship, each time handing it that species' own characteristic hour, its
own lens, and a priesthood check that asks for that species' own
shaman-rung word — a kobold "keeper" is a priesthood exactly as a goblin
"shaman" is, no longer a literal string match blind to a second
vocabulary. The almanac grew one "The Gods" section holding two pantheons
instead of one, its first block still the exact bytes the pre-Eyes almanac
always produced — the identity contract cashed at the page a player
actually reads, not only in a test's assertion. The REPL learned to hop
between eyes: `phenomena --as kobold` shows a warren's own sky, and `why`
now answers honestly for either pantheon's head god.

**One sky, two pantheons, at the scale of ten thousand worlds.** The
census this campaign closes on is the sharpest version yet of the
project's founding demonstration. Every one of 9,972 goblin flagships
across a 10,000-seed sweep heads a solar pantheon — zero exceptions. Every
one of 8,490 kobold flagships on a world that carries at least one moon
heads a lunar pantheon — also zero exceptions, the two exact claims the
campaign's spec preregistered before any seed ran. On the 1,487 moonless
worlds, where a kobold has no moon to crown, the sun still wins most of
the time (90.4%) but a bright enough night-star wins roughly one world in
ten — not a rule with an exception, but a rule correctly reading a sky
that, on those particular worlds, honestly favors the sun. Strip every
name, epithet, and tenet away and ask a blind rule to guess which
structural signature belongs to which species: it is right 86.65% of the
time overall, a perfect 100.0% on every mooned world, and a genuinely
informative, if humbling, 10.44% on moonless ones — not because the
signal vanishes there but because it inverts. Night-stars never depart, so
a moonless kobold's pantheon is smaller and less cyclic than a goblin's
daytime one, whose periodic sun and turning seasons make the goblin the
more cyclic of the two — the exact reverse of the pattern the blind rule
leans on everywhere a moon exists. The 86.65% stands as the honest
measurement rather than being pushed toward a near-perfect score, because
the shortfall is not noise to be smoothed away but a fact about the world:
on a moonless sky the cyclic signal genuinely points the wrong way, and a
rule bent to hide that would be worse, not better, at the one thing it
exists to do.

## What was learned

- **Two knobs answer two questions, and merging them would have answered
  neither cleanly.** Keeping "what is seen" (the hour) and "what demands
  attention" (the lens) as separate mechanisms, rather than one combined
  reweighting, is what let each stay simple enough to build identity into
  by construction and verify independently — a night-stars phenomenon
  either exists for an observer or it doesn't, before any question of how
  much it should matter is even asked.
- **A rule that fails informatively is worth more than a rule adjusted to
  post a better score.** The campaign preregistered only that blind
  attribution would run well above chance, and it does — decisively so, and
  exactly on the mooned worlds the rule was built for. Where it falls short
  of a near-perfect score, the honest response was to find out why, report
  the real rate, and let a census explain the mechanism — a moon supplies
  the extra, more-cyclic deities the rule's fallback assumes exist, and an
  eternal night-star is exactly the case that fallback was never built to
  handle. Bending the rule to show a higher number instead would have
  hidden a genuine, nameable fact about how starlight and moonlight differ
  as objects of worship.
- **Identity, cashed at the page a reader sees, is a stronger proof than
  identity cashed only in a test's assertion.** The goblin-only byte
  contract was already provable in isolation; making the first block of a
  two-pantheon almanac page reproduce those exact bytes is the same claim,
  now visible to anyone who reads the Gallery rather than the test suite.

## Deferred, deliberately (spec §1)

A generated tongue to speak each pantheon's report in its own species'
voice, rather than a shared vocabulary of borrowed English words (The
Tongues); a distributional-twin control — a species carrying the goblins'
exact perception vector, which should score at chance under the blind
rule and thereby prove the rule reads structure rather than an accidental
correlate of generation order — defined this campaign but requiring a
third species to run (The Meeting); terrestrial phenomena rich enough for
a deep-dwelling people to raise an earthy pantheon of its own, rather than
only celestial and ambient gods; comparative studies across a shared
valley's two pantheons, once a third species exists to make "comparative"
mean something beyond a pair; and the perception tiers this closed vector
deliberately excludes — spectral response, hearing, smell, per-individual
variation — each real work for a later campaign, none a quiet widening of
a three-dimensional vector that happened to have room.

## Artifacts

[Study 007: The Census of Eyes](../laboratory/study-007.md) — the two
preregistered claims confirmed at 10k scale, and the moonless
blind-attribution inversion measured and explained. [The Perception
chapter](../domains/perception.md) — the closed vector, the venue
mechanism, the lens derivation. [The Gods of Seed 42](../gallery/the-gods-seed-42.md)
— the goblin and kobold pantheons of the same globe, quoted verbatim,
side by side for the first time. `cli/tests/eyes_identity.rs` and the
worldgen unit test proving a goblin-only observation reproduces the
unlensed path bytewise — the identity contract, CI-enforced on every
build.
