# Campaign 5: The Gods — Design

**Date:** 2026-07-07
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-05-hornvale-longterm-plan-design.md` (Constitution §2 governs)
**Provenance:** The fifth and final Year-1 campaign. Follows Campaign 4 (The
People), which made a flagship settlement's subsistence and caste structure
emerge from its environment. C5 gives that society **gods**, and closes Year 1
with the exit demo: two worlds differing only in astronomy produce legibly
different religions.

---

## 1. Goal

Give a society **a pantheon** — a set of beliefs derived from the salient
phenomena its flagship observes, structured by the society itself — and give the
world a way to **account for its own facts** (`why <entity>`). The deliverable is
the **Year-1 capstone exit demo**: the same land and society under a different
sky yields a legibly different religion. This proves the enrichment thesis end to
end — astronomy → climate → biome → subsistence → social structure → theology.

Comparative religion across settlements, an event ledger, and fields of history
are explicitly **later** (Year 2). C5 builds the coarse religion tier on the
coarse social tier — "coarse constrains fine," the top of the Year-1 cascade.

## 2. Design principles

1. **Religion consumes phenomena and society, nothing else.** The trace protocol
   holds: religion never learns which system produced a phenomenon (spec §3.1.6).
   Tier 1 adds one input — a bare **society summary** (strata count, priesthood
   presence) — mapped at the composition root from the flagship's committed
   castes. Religion imports no other domain.
2. **Religion reflects both the sky and the society** (Durkheim-flavored, cheap-
   real). The pantheon's *members* come from the sky/climate phenomena; its
   *shape* (ranked vs. flat, organized vs. folk) comes from the society. High
   verification surface: the shape is a known function of the social structure.
3. **Derive, don't author.** Each deity is derived from an observed phenomenon
   via the existing eternal-vs-cyclic templates; the pantheon's structure is
   derived from the society. Determinism is constitutional.
4. **Provenance by replay.** The world can account for any entity by replaying
   its committed facts and their provenance. This is `why <entity>` (the existing
   belief-`why` generalized) — historiography tier 0, the seam the Year-2 event
   ledger and fields-of-history will deepen.
5. **Balanced depth.** With astronomy, land, and people at tier 1, religion rises
   to tier 1 too — the last domain to deepen in Year 1, and the one that consumes
   the most of the others.

## 3. Architecture

**One domain deepens; one thin window is added.**

- `domains/religion` becomes tier 1 (kernel-only): it consumes the salience-
  ranked `&[Phenomenon]` (as tier 0 does) plus a **`SocietySummary { strata:
  usize, has_priesthood: bool }`** (religion-owned; the root fills it from the
  flagship's castes). It generates a **pantheon**: one belief per salient
  phenomenon above a floor, structured by the society.
- `windows/historiography` is a new thin window (kernel-only): `recount(world:
  &World, entity: EntityId) -> Option<String>` renders any entity's committed
  facts + their provenance + the registry's predicate docs into a derivation
  sentence. The REPL's `why` verb calls it; religion's tier-0 `why` is subsumed.

**The composition root** (`windows/worldgen`) maps the flagship's committed
castes → `SocietySummary` (`strata = castes_of(flagship).len()`, `has_priesthood
= castes.contains("shaman")`) and passes it to `religion::genesis` alongside the
observed phenomena. Dataflow stays acyclic; religion is the sink of the cascade.

**Naming note (deliberate):** the provenance verb is **`why`** — already the
REPL's established word, generalized from `why <belief-number>` to `why <entity-
id>`. We do **not** introduce a broad `explain` verb; the capability's internal
name is `historiography::recount`. `why` is the narrow, established surface.

## 4. The pantheon model (`domains/religion`, tier 1)

- **Members.** For each phenomenon with salience ≥ `PANTHEON_FLOOR` (0.25;
  always at least the single most salient, so a world is never godless while it
  observes anything), mint a belief/deity: the existing eternal (aperiodic) vs.
  cyclic (periodic) epithet + tenet templates, tagged with its
  `derived-from-phenomenon` kind. **No size cap** — minor phenomena (a faint
  moon, the ambient air) become lesser spirits, *esoterica* that reward deeper
  interrogation. So a spinning world yields returning-sun + seasonal + moon
  deities; a locked world yields an eternal motionless-sun deity + moons + fixed
  night-stars, and **no seasonal deity** (locked worlds have no seasons).
- **New predicates** (minimal): `high-god` (a functional flag on the presiding
  deity, present only in ranked pantheons) and `cult-form` (Text:
  `organized`/`folk`). The existing `is-belief`/`tenet`/`held-by`/
  `derived-from-phenomenon` carry over.

**Model card.**
- *Drawn:* per-deity epithet pick (existing `religion/epithet` stream).
- *Derived:* pantheon membership (salience floor), structure (from the society),
  tenets (from phenomenon periodicity).
- *Approximated (declared):* one flagship's pantheon (not comparative religion);
  celestial+ambient phenomena only (no earth/sea gods yet); static theology (no
  religious history — the Year-2 event ledger); single species (goblin).

## 5. Society → structure (verticality + priesthood)

Two levers, both from C4's emergent structure:
- **Verticality.** `strata ≥ 4` (C4's stratified size-5 towns) → a **ranked**
  pantheon: the top-salience deity gets the `high-god` flag and presides over the
  rest. `strata ≤ 3` (lean camps) → a **flat** pantheon (co-equal spirits, no
  high god). (Threshold tunable; C4's structure sizes are bimodal at 2 and 5.)
- **Priesthood.** `has_priesthood` (a `shaman` caste exists) → `cult-form =
  organized`, beliefs held by the priesthood; else `cult-form = folk`, held by
  the whole community.

Both are legible in the exit demo: a stratified farming town runs a ranked state
cult with a priesthood; an egalitarian forager camp keeps a flat folk animism.

## 6. Historiography tier 0 — `why <entity>` (`windows/historiography`)

`recount(world, entity) -> Option<String>` reads an entity's committed facts,
their `provenance` strings, and the registry's predicate docs, and renders a
derivation. For a belief it chains richly: *"the high god, derived from the
motionless sun (which never departs); an organized cult tends it; asserted by
religion."* For a settlement: *"the place Torgna — a settlement of temperate-
rainforest on cell 8125; asserted by settlement."* It is domain-agnostic (reads
the ledger + registry generically), so it lives in a window and imports no
domain. The REPL's `why` verb accepts any entity id and calls it; `hornvale
new`/`almanac` are unaffected.

**Model card.** *Derived:* the recounted text (from committed facts +
provenance + registry docs). *Approximated:* a flat single-entity recount (no
multi-hop causal chains across entities — that is the Year-2 event ledger); no
natural-language variation (a fixed rendering).

## 7. The exit demo (the Year-1 capstone)

The showpiece pair: **seed 42 spinning vs. tidally-locked**, identical land and
society seed, different sky. Spinning → a pantheon of cyclic/returning deities
(the sun that departs and returns, the seasons, the moons); tidally-locked → an
**eternal motionless-sun high god** with moons and fixed night-stars and **no
seasonal festival** — the terminator culture's theology hangs on the unmoving sun
and its moons (the frontier's alien-religion note, cashed). `why` on each world's
high god reads differently. Artifact: a drift-checked gallery pair of the two
almanacs' "The Gods" sections — the visual capstone of the Year-1 arc.

## 8. The Lab — a Census of Faiths

New metrics: pantheon size (deity count), high-god periodicity (eternal/cyclic),
cult form (organized/folk), pantheon verticality (ranked/flat). **Calibrations
(built in):**
- **verticality ⇔ stratification**: a pantheon is ranked iff `strata ≥ 4` — a
  known function of the flagship's caste count.
- **high-god eternal ⇔ tidal lock**: the presiding deity's tenet is eternal iff
  the world is tidally locked — extending L0's belief⇔lock calibration to the
  pantheon head.

These are the fourth and fifth exact calibrations in the family (belief⇔lock,
band-count⇔rotation, subsistence⇔biome). Genuinely unknown numbers: pantheon-
size distribution, cult-form mix, verticality mix. A 10k `census-of-faiths` runs
at author time; the CI `census-lands-drift` grows the religion metrics.

## 9. CI

The artifact drift check gains nothing new to render beyond the exit-demo
almanacs (already regenerated for seed 42 spinning + locked); the `census-lands-
drift` rerun picks up the new religion metrics, folded into the existing `git
diff --exit-code` net. The 10k census is author-time only.

## 10. Testing

- **Determinism:** same seed + pins → identical pantheon, structure, and `why`
  output. `BTreeMap`/`Vec`, `total_cmp`, no wall-clock, no `HashMap`/`HashSet`.
- **Pantheon:** deity count equals the number of phenomena above the floor
  (min 1); a spinning world has a seasonal deity, a locked world does not; each
  deity's tenet matches its phenomenon's periodicity (eternal/cyclic).
- **Structure (calibrations, as tests):** ranked iff `strata ≥ 4`; `cult-form`
  organized iff a shaman caste exists; the high god is the top-salience deity.
- **Historiography:** `recount` round-trips a belief's chain (names its
  phenomenon kind, cult form, and provenance) and renders a non-belief entity
  from its facts; `recount` on an unknown/empty entity is `None`, never panics.
- **Exit demo (end to end):** two worlds differing only in `RotationPin` yield
  different pantheons (deity set or structure differs); the locked flagship's
  high god is eternal; the almanac renders "The Gods"; the REPL answers `why`.

## 11. Staging

Following the C3/C4 rhythm, C5 splits into two sub-plans, each a working
increment + artifact:
- **Plan 5a — The Pantheon.** `SocietySummary`; tier-1 `religion::genesis`
  (pantheon from the salience floor; verticality + priesthood; `high-god`/
  `cult-form` predicates); composition-root wiring; the almanac's "The Gods"
  section grows the pantheon; the exit-demo gallery pair; the exit-demo test.
- **Plan 5b — Provenance & the Year-1 close.** `windows/historiography` +
  `recount`; the REPL `why <entity>` generalization; the Census of Faiths
  metrics + the two calibrations; the C5 + **Year-1 capstone** book work
  (religion chapter to tier 1, Study 004, the Year-1 chronicle, cascade overview
  final).

Splitting is acyclic (5a's pantheon is 5b's `recount`/Census input). Either may
abort at a week boundary with the tier-0 cascade intact.

## 12. Book / Definition of Done

Chronicle entry (folding 5a + 5b and the Year-1 arc); the religion chapter
promoted to tier 1 with its model card; the exit-demo gallery pair (the two "The
Gods" sections); a Census-of-Faiths study chapter (Study 004) with comprehension-
gated analysis (the two calibrations + the pantheon-size/cult-form/verticality
distributions); an architecture note on provenance-by-replay (`why`/
`historiography`); regenerated stream manifest and concept registry; a
freshness sweep of the cascade overview (all five domains at tier 1) and a
Year-1 retrospective framing.

## 13. Explicitly deferred

- **Terrain/climate as phenomena sources** (earth gods from unrest, sea gods
  from tides) → a later religion/ecology tier; C5 draws only celestial+ambient.
- **Comparative religion / diffusion** across settlements (only the flagship
  gets a pantheon) → a later social tier.
- **Per-caste patron deities** → considered and set aside (verticality +
  priesthood is the tier-1 coupling).
- **The event ledger, retrospective confabulation, and fields of history** →
  Year 2; `why` tier-0 is the single-entity seam they deepen.
- **Multi-species theology** → the Year-2 species-psychology substrate.
- **Natural-language variation** in `recount` (a fixed rendering for now).

## 14. Risks

- **Scope: the capstone.** Religion tier 1 + a new window + the Year-1 close is a
  full campaign. Mitigation: pantheon-as-derived (no new phenomena sources), the
  5a/5b split, and aggressive deferral (§13).
- **"Convincing enough" pantheons** is a taste gate (like the biome map and the
  settlement map) — the two "The Gods" sections and Nathan's read of them are the
  gate. Mitigation: high verification surface (the two calibrations, the eternal-
  vs-cyclic templates already trusted from tier 0).
- **A thin pantheon.** If the flagship observes few salient phenomena, the
  pantheon is small. Acceptable — a small pantheon is a legible outcome, and the
  no-cap floor lets minor phenomena fill it out as esoterica.
- **`why` over-generalization.** `recount` must stay a flat single-entity replay,
  not creep toward the Year-2 event ledger. Mitigation: the model card declares
  the boundary; no cross-entity causal chaining in C5.

## 15. Exit criteria (the Year-1 exit criteria)

1. `hornvale new` produces a world whose flagship holds a pantheon derived from
   its observed phenomena and structured by its society; the almanac renders "The
   Gods"; the REPL answers `why <entity>` for a belief and a settlement.
2. **The Year-1 capstone:** two worlds differing only in astronomy produce
   legibly different religions — the seed-42 spinning vs. tidally-locked pair
   reproduces byte-identically and its two "The Gods" sections differ in pantheon
   membership and structure.
3. The Census of Faiths answers the pantheon-size/cult-form/verticality questions
   with numbers, and both calibrations (verticality⇔stratification, high-god-
   eternal⇔lock) match their known functions exactly.
4. The Laboratory and gallery additions are live in the published book, drift-
   checked; the full gate (test + fmt + clippy) and the artifact drift check are
   green; the book presents all five domains at tier 1 and frames the Year-1 arc.
