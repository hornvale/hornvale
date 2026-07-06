# The Frontier — a non-binding vision map

**Status: SPECULATIVE. This document is not a specification and governs
nothing.** It records the maximum-ambition directions the project has
deliberately chosen to keep in view, each tagged with a confidence level, so
that sequencing decisions are informed and hard-won thinking is not
re-derived from scratch a year later. When any item here becomes real work
it earns a proper spec in `docs/superpowers/specs/`, and that spec — never
this map — is what binds. Where this document and a spec disagree, the spec
wins by definition. The public book (`book/`) describes only merged reality;
this file is the opposite end of the pipe, and stays out of the book.

Treat every idea below as a hypothesis with a confidence tag, not a promise.
The discipline that governs the code governs this document too: an idea
earns elaboration only when something concrete needs it, and the smallest
version that changes a downstream output is always preferred to the
elaborate one.

---

## The governing principle: ambition is bounded by verification surface

The deepest lesson of the project so far is that **the ceiling on ambition
is not cleverness but checkability.** Astronomy could be enormously ambitious
because physics supplies ground truth: mass–luminosity relations, Kepler's
third law, and stability inequalities are all things a world can be *wrong*
about, and tests can catch the wrongness. The most ambitious directions are
therefore the ones that maximize the number of checkable invariants, and the
honest way to read every confidence tag below is as an estimate of how much
of the idea can be verified rather than merely admired.

This produces a gradient. Physics-grounded systems sit at the top (a claim
either matches a formula or it doesn't). Systems grounded in a real
scholarly literature sit in the middle (a generated joke can be checked
against whether it cites norms the culture actually holds, even if whether it
*lands* is a matter of taste). Purely aesthetic goals sit at the bottom and
require a human in the loop rather than a bigger single ask — chart
legibility and prose quality live here, and so, honestly, does "is this
goblin funny." Naming which tier an ambition occupies is the first act of
designing it.

---

## The frontier map

1. **The Laboratory** *(high confidence; shipped in Campaign L0)*. A batch
   harness that sweeps seeds and pin-sets, extracts metrics, and publishes
   drift-checked distributions. It is listed first because it is the
   instrument that makes every other item on this list measurable: any
   claim that "cultures plagued by trickster gods develop more defensive
   architecture" becomes a study one can actually run.

2. **The epistemic layer** *(architecture-ready; whether it is fun is open)*.
   Player and scholar knowledge as a second, provenance-tagged ledger,
   distinct from the true generative ledger. Maps become documents with
   authors and errors; source criticism becomes a mechanic. The single
   biggest under-ambition risk, because the architecture already supports it
   and the payoff is large.

3. **Deep language** *(high confidence on mechanics)*. Per-culture
   proto-languages plus deterministic sound-change laws applied over
   historical time; place names as fossils, etymology as archaeology.
   Sound-change engines are solved technology. Prestige and liturgical
   conservatism act as a *damping field* on change, producing diglossia,
   frozen ceremonial registers, and dead languages.

4. **Myth phylogenetics** *(medium confidence; genuine research)*.
   Proto-myths as motif structures propagated through noisy cultural
   transmission, then reconstructed by comparative method and scored against
   the true generation tree. Nobody has a laboratory for this; Hornvale
   would be one.

5. **Models author, dice roll** *(high confidence; now a ratified
   constraint)*. Language models used offline as authoring amplifiers that
   produce committed data — grammars, motif libraries, lexicons, expert-
   system rule bases — while runtime generation stays purely deterministic.
   There is never a runtime model in the simulation core. See the ratified
   decision in the long-term-plan spec.

---

## The unification thesis

The deepest structural claim the project has arrived at: **science, magic,
theology, and roguelike identification are one mechanic — inference over a
hidden, seeded ruleset.** A culture investigating its sky, a mage probing a
second physics drawn per seed, a priesthood reading intent into phenomena,
and a player identifying an unknown potion are all doing the same thing at
different altitudes. This means the Laboratory's extractor-over-a-ledger
design is also the first sketch of a simulated scholar's cognition: an
epistemic agent is a lab-runner working from partial, biased data.

Two corollaries. Magic as a *second physics* promotes pins from parameter
selection to rule-system selection, making comparative metaphysics a
controlled science. And interventionist or trickster deities are adversarial
noise injected into a civilization's inference process — which turns the
vision book's "belief makes gods real" loop into a concrete feedback edge
between the ledger and its agents.

---

## The expressive-culture cluster

A family of systems that all reduce to the same shape: a small parameter
vector per culture or species, consumed by **one seeded grammar engine that
targets four media — text, image, music, and space.** The language
campaign's prose engine should be designed as this substrate from the start,
even if it ships with only one medium lit.

- **Music and tuning** *(medium; post-culture)*. The Pythagorean comma is a
  hidden ruleset that is *mathematically necessary* rather than seeded —
  every culture's tuning is a negotiated compromise with incommensurability,
  and the compromise cascades into both musical form and theology (pure
  intervals and "the gods keep their harmonies" versus equal temperament and
  its "indisputable deception"). Tuning stance is the reflexivity field
  applied to music, and a belief-ledger fact with theological provenance.
  Audio, like the First Light bitmap, would be hand-rolled deterministic PCM
  — a listenable, drift-checked gallery.

- **Species perception** *(medium; needs a species layer)*. Per-species
  visual profiles — photoreceptor channels, rod/cone balance, luminance
  range, grounded in comparative-vision literature. The load-bearing
  consequence: **salience becomes a function of both the phenomenon and the
  observer,** so different species see different night skies from identical
  heavens, and therefore build different astronomies and religions in the
  same valley, each empirically correct about what it sees. The
  `ObserverContext` extension point left in Campaign 1a is where this lands.
  Downstream: art palettes conditioned on species gamut and ambient
  illuminant; colour lexicons (Berlin & Kay) as evidence of a species'
  vision; a drawn atmospheric *scattering regime* (Rayleigh blue, Mie-dust
  warm, haze orange) feeding sky and twilight colour.

- **Humor** *(taste-gated; spot-checked, not automated)*. Benign-violation
  theory makes humour a *function* of a culture's norms, fears, body plan,
  and hierarchy — all parameters the project already plans to hold. Goblin
  jokes about tall folk striking their heads are body-plan incongruity plus
  status inversion; a humourless species is one whose threat baseline leaves
  the benign-violation set empty. Jokes are the fastest norm-probes in the
  game: a joke you must have explained measures your cultural distance from
  its teller.

- **Drama** *(medium; couples to lifespan)*. A performed narrative is an
  error-correcting code tuned to its culture's transmission channel, and
  **lifespan sets the channel.** Short-lived oral cultures need
  high-redundancy encodings (formulaic epic, communal spectacle); near-
  immortals have a near-lossless channel and inverted scarcity, so elvish
  opera might be performed *once, ever*, brief and dense because
  reinterpretability sustains a two-century discussion — and death becomes
  the exotic subject rather than the universal tragedy.

- **Architecture** *(high; the most tractable of the cluster)*. Settlement
  layout as constraint satisfaction over body plan, environment, social
  structure, and threat model — with a real pedigree in shape grammars
  (Stiny) and space syntax (Hillier). The parameter vector: a status-
  projection rule (which spatial axis encodes rank), a sacred-placement rule,
  a household unit, a commons rule, and a defense mode. Christopher
  Alexander's *A Pattern Language* is the authored pattern library this
  wants — "models author, dice roll" for space — and his "A City Is Not a
  Tree" supplies a quality criterion: living settlements are overlapping
  semilattices, not clean hierarchies, and overlap is measurable, hence a
  candidate Lab metric. **Sacred placement is derivable, not drawn:** order-
  legitimating (priestly) religion sites its temple at the center; spirit-
  mediating (shamanic) religion sites its shrine at the liminal edge (Turner;
  van Gennep) — so a player can read a culture's theology off its map.

  *Near-term hook:* when Campaign 4's settlement tiers are designed, tier 1
  should be a layout grammar, not a population count at a point.

---

## The species-psychology substrate — the layer beneath the cluster

*(High potential value; verification is weaker here — flagged.)*

The single idea that unifies the expressive-culture cluster rather than
sitting beside it: **culture is the scaffolding a species builds around its
own psychology.** Humor, drama, sacred architecture, and prestige-damping of
language all silently assume a psychology; making that psychology an explicit,
bounded, per-species parameter vector turns four separate systems into
consequences of one.

Candidate axes, to be kept minimal and promoted only on demonstrated
downstream need: threat response (freeze / flight / fight as a distribution —
fae going *tharn* versus goblins joining a brawl before parsing the sides);
deliberation latency (the impulse-to-action gap that makes the goblin mid-
brawl side-switch a *known cultural element*); sociality mode (eusocial /
pack / pair-bonded / solitary — the master axis, since it reshapes whether a
"bystander effect" is even a coherent notion); in-group radius; time horizon
(coupling to the lifespan parameter under drama); and status basis (which
feeds the architecture status-projection rule directly).

**Two honest cautions, louder here than anywhere else.** First, the
verification story is weaker: there is no real-world goblin to be wrong
about, so the checkable claim is *internal coherence and downstream
consequence* — does the goblin profile actually produce goblin culture,
architecture, and jokes that hang together? — not correspondence to ground
truth. The Lab is what makes even that testable. Second, this is where the
ontology trap is most dangerous: six axes is a design, sixteen is folk
psychology ossified into false science. An axis that changes no settlement,
joke, or myth has not earned its place.

*Sequencing:* this is a strong Year-2 candidate for its own small "species
substrate" campaign, sitting **upstream** of the culture campaigns (C4/C5) so
they consume a psychology layer rather than each reinventing one piecemeal.
It does not alter the Year-1 roadmap.

---

## Provenance interrogation — the `explain` verb

*(High confidence; small; no new architecture required.)*

Determinism makes derivation traces essentially free: because every derived
thing is a pure function of the seed and the ledger, justifications need not
be *stored*, only *replayed on demand* with tracing switched on. "Why is the
shaman's hut here?" re-runs that settlement's derivation in trace mode and
emits the actual decision path down to the stream draw that broke a tie. This
is a natural CLI verb — `hornvale explain --world w.json <thing>` — sibling
to `almanac` and `scout`, with a REPL "more info" affordance over it, and it
doubles as the tool for explaining a study's outliers.

The important distinction: the debug "why" and the player "why" are
different objects. `explain` reads the *true* generative trace, omniscient.
The player-facing counterpart is a generated **ethnography** built only from
the player-knowledge ledger, with its own evidence provenance (including
unreliable informants). The *difference* between the two documents — what the
player got wrong — is measurable, and therefore a score, a gameplay beat, or
both. Asking an NPC to explain a joke is this in miniature: the explanation
is a norms lesson, and needing it marks the asker an outsider.

---

## Intellectual lineage

So the reasoning is recoverable when it has gone cold: benign-violation
theory (humour); Parry–Lord oral-formulaic tradition (drama as redundancy);
Berlin & Kay (colour lexicons); comparative vision science (species
perception); Stiny shape grammars and Hillier space syntax (layout);
Christopher Alexander, *A Pattern Language*, *Notes on the Synthesis of Form*,
"A City Is Not a Tree" (patterns and the semilattice criterion); Victor
Turner, *The Ritual Process*, and Arnold van Gennep, *Rites of Passage*
(liminality and sacred placement); Mary Douglas, *Purity and Danger* (queued,
for taboo generation); James Ryan and the UCSC Expressive Intelligence Studio,
*Talk of the Town* / *Bad News* (character-scale knowledge and gossip);
Solodow, *Latin Alive* (prestige as a language parameter). The map moves when
the reading feeds it — which is the collaborator's role, demonstrated.
