# Religion

**Questions it answers:** What do these people believe, and *why*?

The second question is the project's soul, and religion is the first domain
built to answer it: every belief carries a committed record of which
phenomenon it mythologizes, so the REPL's `why` command can trace faith back
to observation. Ask Gruugish's people why they revere the Unblinking Eye and
the answer is genuine provenance, not flavor text.

**Tier 0 — one belief from the top phenomenon.** Genesis looks at the
salience-ranked phenomena visible from the village and mythologizes the
first. The templating is deliberately minimal — two shapes:

- An **eternal** phenomenon (no period) becomes a changeless watcher: *"the
  Unblinking Eye is a golden sun fixed at zenith; it has never departed and
  will never blink."*
- A **periodic** phenomenon becomes a cyclic deity whose absences are mourned
  and returns feasted — the life-death-rebirth structure from the vision
  book's *Golden Bough* chapter, waiting for a sky that actually cycles.

The epithet is drawn from a seeded pool, so different worlds venerate
differently-named gods of the same sun.

**The constitutional point.** Religion consumes *only* phenomena — it imports
nothing from astronomy, climate, or anything else. Its test suite proves this
interface-blindness directly: the tests hand-construct a fictional phenomenon
(a pale wanderer with a thirty-day period), which exists in no world and
which no tier-0 provider can emit, and feed it straight to genesis. Religion
produces a correct cyclic tenet from it anyway. To be clear, the *world*
contains no moon and assumes none — the point is about the code: religion is
written against phenomena in general, so it is already correct for kinds of
sky that do not yet exist. When Campaign 2 gives the sky periods and Campaign
5 deepens the templating, the *legibly different religions from different
skies* demo — year one's exit criterion — is a composition of parts that
already work, not a new invention.

**Honest limitations of tier 0:** one belief, two templates, no relationship
to social structure, no myth *narrative* — a tenet, not a story. The tier
ladder (Campaign 5) consumes seasonal structure and culture's social facts,
generates pantheons rather than single tenets, and begins the historiography
turn: beliefs as *accounts told by someone*, not facts about the world.

**Tier 1 — the pantheon (Campaign 5).** Religion stops minting one belief
and mints a **pantheon**: one deity per phenomenon the flagship's community
observes above a salience floor of 0.25, shaped by the eternal/cyclic
templates tier 0 already trusted. The interface grows by exactly one input —
a bare `SocietySummary { strata, has_priesthood }`, mapped at the
composition root from the flagship's committed castes (Campaign 4b) — and
nothing else; religion still imports no domain, and still learns nothing
about which system produced a phenomenon. Two levers, both already-derived
facts about the society, decide the pantheon's *shape*; the sky alone still
decides its *members*.

- **Membership, no cap.** Every phenomenon at or above the floor seats a
  deity — a spinning sky's sun, its seasons, and its moons all mythologize
  independently, with no ceiling on how large a pantheon can grow. A world
  is never godless while it observes anything: if nothing clears the floor,
  the single most salient phenomenon still seats one. Phenomena below the
  floor that do clear it in a richer sky (a faint moon, the ambient air)
  become minor spirits — **esoterica**, present but easy to miss, the same
  low-salience-but-real principle climate's ambient air demonstrated at
  tier 0. Each deity's tenet still follows the eternal-vs-cyclic split
  exactly as tier 0's two templates did: an aperiodic phenomenon yields a
  changeless watcher, a periodic one a life-death-rebirth cycle of absence
  and return.
- **Verticality, from social strata.** A flagship whose emergent structure
  (Campaign 4b) reaches four or more castes — the stratified size-5 towns —
  gets a **ranked** pantheon: its single most salient deity is flagged
  `high-god` and presides over the rest. A lean camp of two or three castes
  gets a **flat** pantheon of co-equal spirits. Verticality is a known
  function of a number the census already had, so it is exactly checkable,
  not merely plausible.
- **Priesthood, from the shaman caste.** A flagship whose structure includes
  a `shaman` caste tends its pantheon as an **organized** cult, its beliefs
  held by the priesthood rather than the whole community, which instead
  gets a **folk** cult, animism shared by everyone. This is the same
  society-summary input as verticality but a genuinely separate axis — a
  lean structure can carry a shaman without reaching four strata, so an
  organized cult over a flat pantheon is a real, observed combination, not
  a contradiction (Study 004 has the numbers).
- **Two new predicates.** `high-god` (a functional flag on the presiding
  deity, present only in ranked pantheons) and `cult-form` (functional
  Text, `organized`/`folk`) join the tier-0 vocabulary
  (`is-belief`/`tenet`/`held-by`/`derived-from-phenomenon`) unchanged.

**Provenance by replay: `why`/`recount`.** Every belief already carried a
committed `derived-from-phenomenon` record; Campaign 5 gives the world a
general way to read it back. `windows/historiography::recount(world,
entity)` replays any entity's committed facts, their provenance strings,
and the registry's predicate docs into a derivation sentence — for a belief,
a chain naming its phenomenon kind, its cult form, and who asserted it; for
any other entity (a settlement, a place), the same generic replay, because
`recount` is domain-agnostic and interprets no predicate specially. The
REPL's `why <id>` verb, previously wired straight to religion's own belief
lookup, now calls `recount` for any entity id — subsuming tier 0's `why`
without changing what a player already knew to type. This is
**historiography tier 0**: a flat, single-entity replay, deliberately not a
causal chain across entities. It is the seam the Year-2 event ledger and
fields-of-history will deepen, not a preview of them.

**The exit demo.** Seed 42's flagship, spinning sky, tends a pantheon headed
by **the Wheel-Turner**, a cyclic deity whose absences are mourned and
returns feasted; pin the same seed's sky to tidally locked and the head
becomes **the Still Crown**, an eternal sun fixed forever above the day
side — the only line in the pantheon that changes, because a locked world
still keeps its moons, only loses its seasons and its head deity's cycle.
The same globe, a different flagship under each sky (since Campaign Y2-0's
placement fix, the sky moves settlement too), yet two societies alike
enough to hand religion the same summary — so the theology moved exactly
where the sky moved and nowhere else. The two full "The Gods" sections are
quoted verbatim in the gallery: [The Gods of Seed
42](../gallery/the-gods-seed-42.md).

**The model card.**

- **Drawn:** each deity's epithet, from the same seeded pool tier 0 used
  (`religion/epithet`), now drawn once per pantheon member rather than
  once per world, skipping any epithet already used in that pantheon.
- **Derived:** pantheon membership from the salience floor; verticality
  from strata; cult form from priesthood presence; each tenet from its
  phenomenon's periodicity, exactly as tier 0's two templates already did.
- **Approximated (declared):** one flagship's pantheon, not comparative
  religion across the scatter; celestial-and-ambient phenomena only — no
  earth gods from tectonic unrest, no sea gods from tides, both explicitly
  deferred; **static theology** — the pantheon is computed once at genesis
  and never revises as the sky, society, or a religious history would
  later change (that history is the Year-2 event ledger); single species
  (goblin) — the comparative multi-species theology the vision book raises
  waits for Year 2's species-psychology substrate.

Laboratory: [Study 004, the Census of Faiths](../laboratory/study-004.md).
Chronicle: [Campaign 5, The Gods](../chronicle/campaign-5.md).

**The tier ladder ahead:** terrain and climate as phenomena sources (earth
gods from unrest, sea gods from tides); comparative religion and diffusion
across the full settlement scatter, not just the flagship; per-caste patron
deities; a religious history that revises instead of freezing at genesis,
once the event ledger exists; and the multi-species theology a second
species would make possible.
