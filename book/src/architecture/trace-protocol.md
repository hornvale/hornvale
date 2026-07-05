# The Trace Protocol

The trace protocol answers the question that has historically killed projects
of this shape: how do a dragon, a religion, and an economy interact without
every system knowing about every other system? The naive answer — direct
interfaces — produces N×M coupling and a codebase where adding economics
means editing dragons.

The protocol's answer is an inversion:

> **Entities don't provide information to systems. Entities leave *traces*
> in a shared substrate, and systems *interpret* traces.**

A dragon is not "a source of geological and religious information." A dragon
is a source of *marks on the world*: a hollow mountain, a wealth
concentration, a periodic terrible light in the night sky. Geology, religion,
and economics each read those marks through their own lens — and the dragon
compiles knowing nothing about any of them.

## The three vocabularies

1. **Facts** — writes to the ledger, in the dumb envelope, using
   domain-registered predicates. *The dragon tunneled here, from year 1200
   to 1400.*

2. **Field contributions** — additive influence on named fields. The dragon
   raises `terror` and `wealth-concentration` near its lair. Crucially,
   consumers read aggregate field values and *cannot tell* whether the terror
   comes from a dragon, a plague, or a warlord — and that engineered
   ignorance is what lets meaning-making systems interpret without
   enumerating causes.

3. **Phenomena** — the universal read, and the protocol's biggest bet: one
   query, *"enumerate the phenomena salient to an observer at this place and
   time, with periodicity and character,"* consumed by every meaning-making
   system. The two moons and the dragon's monthly flight are both just
   phenomena — recurring lights in the sky with a period and an emotional
   valence. Religion mythologizes them, language names them, and someday the
   game's room descriptions render them, all through the same interface.
   Results are salience-ranked with fully deterministic ordering, because
   "what you notice first" must survive the determinism constitution too.

## Agents are level-of-detail'd like everything else

An unobserved dragon *is nothing but* its field contributions plus occasional
statistically-generated ledger events. An observed dragon is a full agent.
The live agent's behavior must refine — never contradict — its own coarse
statistical self. There is no always-on agent simulation, by design.

As of Campaign 1a, the vocabularies exist and are exercised end-to-end by the
integration suite (a miniature "religion" reveres the most salient phenomenon
a miniature "sun" emits, with neither knowing the other exists). The first
real producers and consumers arrive in Campaign 1b.
