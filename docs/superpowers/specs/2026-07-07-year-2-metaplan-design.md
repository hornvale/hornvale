# Year 2 Metaplan: The World Speaks, and Is More Than One People — Design

**Date:** 2026-07-07
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-05-hornvale-longterm-plan-design.md` (Constitution §2 governs)
**Provenance:** Year 1 closed at 587ef62 with all five domains at tier 1 and the
Year-1 exit criterion met (two worlds differing only in astronomy → legibly
different religions; capstone `book/src/gallery/the-gods-seed-42.md`). The
Year-1 retrospective identified four candidate spines for Year 2; this spec
records the choice and sequences the campaigns behind it. Each campaign after
Campaign 0 still opens with its own spec; this document is the metaplan they
answer to.

---

## 1. The spine, and why this one

**Chosen spine: the world speaks, and is more than one people** — a
species-psychology substrate, observer-dependent perception, and language
tier 1, converging on comparative culture within a single world.

Alternatives considered and set aside (recorded so the choice isn't
relitigated without new information):

- **The world has a past** (paleoclimate → event ledger → fields-of-history):
  the spec's original Year-2 order and the top of the verification gradient,
  but slower to cultural payoff. It remains the presumptive Year-3 spine; the
  static-at-genesis weakness is deferred, not dismissed.
- **Populous & believable** (verisimilitude hardening + society across the full
  scatter): foundation work with no demo of its own. Its highest-value slice
  is promoted into Campaign 0 below; the rest is deferred.
- **Known, not just true** (the epistemic layer): highest ceiling, but riskiest
  on today's thin content — there is not yet enough world for scholars to be
  wrong about. Revisit once the world has a past.

The chosen spine attacks the retrospective's three deepest gaps — one
hardcoded species, no language at any tier, flagship-only culture — in
dependency order, and its verification story leans on the Lab muscle Year 1
already built.

## 2. Year-2 exit criterion

Year 1 varied the world and held the observer fixed. Year 2 inverts the
experiment:

> **Generate one world carrying two species that differ only in their authored
> parameter vectors (psychology, perception, articulation), and get legibly
> different languages and religions from the same sky.**

Falsifiability teeth: a **blind-attribution metric** — a code metric (not
taste), defined when perception lands in Campaign 2 and preregistered per
ADR 0016 — must attribute species-stripped artifacts (a pantheon, a name-set)
to the correct species above a preregistered accuracy across 10,000 worlds.
If the second species' culture reads as a reskin — same pantheon shape, same
social structure, different names — the thesis fails, and is allowed to fail.

One confound must be controlled: species draw from their own labeled streams,
so two species with *identical* vectors would still differ world-by-world
(different draws). The claim is therefore statistical and needs a **null
control**, preregistered alongside the main studies: a study population where
the second species carries the goblins' exact vectors must be
distributionally indistinguishable from goblins in aggregate (and score at
chance on blind attribution) — establishing that measured divergence is
attributable to the vectors, not to stream noise.

## 3. Campaign sequence

Five campaigns, strict dependency order; every campaign ends observable and
is abortable at a week boundary.

| # | Campaign | Delivers | Observable ending |
|---|----------|----------|-------------------|
| 0 | **Firm Ground** | hardening + new baselines | censuses re-run, placement degeneracy gone |
| 1 | **The Peoples** | species substrate + psychology vector | two social structures in one almanac |
| 2 | **The Eyes** | perception profiles, observer-dependent salience | two pantheons in one world (raw demo) |
| 3 | **The Tongues** | phonology, naming grammars, register | every proper noun generated, myths in voice |
| 4 | **The Meeting** | preregistered comparative studies, capstone | the polished demo artifact + book close |

Sequencing rationale: substrate-first follows the frontier's note that species
psychology sits upstream of all culture campaigns. Nothing is built
single-species and retrofitted (rejected: language-first, which retrofits
register once species exist) and no stub-era stream draws become permanent
save-format contracts (rejected: walking-skeleton, which binds throwaway
labels under ADR 0006).

## 4. Campaign 0: Firm Ground

Small and surgical; no new concepts, no new domains. This campaign is
specified fully here — it proceeds directly to an implementation plan without
its own spec.

1. **Placement de-degeneracy.** Coastal cells currently receive
   `freshwater = 1.0` (`windows/worldgen/src/lib.rs:426`), which combined with
   the suitability formula (`domains/settlement/src/placement.rs:52`) makes
   the flagship coastal in 100% of 2,000 censused worlds. Fix the freshwater
   input so inland river/lake cells can win the argmax. Success metric,
   Lab-asserted: flagship-coastal rate across the census falls from 100% to a
   value with genuine variance (the exact expected rate is set by the fixed
   model, then pinned as a calibration row).
2. **Dead subsistence modes revived.** Herding and foraging are currently
   unreachable (returned only for inland arid/cold cells, which the flagship
   can never be). Partly follows from fix 1; additionally audit the
   `subsistence()` rule table. Success: all four modes observed in the
   10,000-seed census.
3. **Biome skew diagnosed, not necessarily changed.** The ~60%
   frozen-dominant skew traces upstream to draw distributions (star class,
   obliquity envelope). Campaign 0 produces a written diagnosis; distributions
   change only if the cause is a defect rather than a modeling choice, and any
   change goes through epoch-suffixed streams (ADR 0006). No silent retuning.
4. **Re-baseline exactly once.** All four censuses re-run at 10,000 seeds, the
   drift study regenerated, calibration rows re-asserted. Every later Year-2
   study inherits these baselines; doing this first means the churn happens
   once.

Explicitly out of Campaign 0: the name/epithet pools (Campaign 3 dissolves
them), flagship-only culture scope (Campaign 1 revisits it), anything touching
static-at-genesis (Year 3).

## 5. Campaign 1: The Peoples

**A new domain crate, `domains/species`**, depending on `hornvale-kernel` and
nothing else. Opens with its own spec; the binding decisions:

- **Species registry:** an ordered (`BTreeMap`) set of species defined by
  authored data, not code. Year 2 ships two: goblins (the incumbent,
  parameterized) and a second species. The second species' identity is an
  authored taste call (Nathan's), not a design decision; the design requires
  only that its vectors differ meaningfully from the goblins'.
- **Psychology vector** — the frontier's minimal set, closed at six
  dimensions: threat response, deliberation latency, sociality mode, in-group
  radius, time horizon, status basis. Each is a small enum or bounded bare
  `f64` (ratios stay untyped per ADR 0008). Per the model-cards discipline,
  each dimension is declared authored vs. drawn; drawn values use labeled
  streams (`species/<name>/psych/...`).
- **Ontology-trap posture** (the frontier's twice-repeated warning lands
  here): the vector is closed and small. No extensible trait system, no
  per-individual variation, no inheritance hierarchy. A species is a name
  plus three small vectors (psychology; perception and articulation arrive in
  Campaigns 2–3). Widening any vector requires its own campaign.
- **Consumption without dependency:** the `SocietySummary` pattern. Culture
  and religion define their own small input structs (e.g. a `PsychSummary`);
  `windows/worldgen` populates them from the species domain. No
  domain-to-domain edge; no trace-protocol change. Species parameters are
  also committed as facts (~6–8 new registered predicates, value-kind
  enforced per ADR 0010) so `why` can recount them.
- **Two scatters, two flagships:** each species gets its own settlement
  scatter and flagship. Placement reuses the existing suitability machinery
  with species-specific weights derived from the psychology vector (high
  threat response weights defensibility; long time horizon weights farmland)
  so the flagships generally land in different places for legible reasons.
  Culture tier 1 runs per species-flagship.
- **Goblin identity holds:** parameterizing the incumbent must reproduce
  today's goblin outputs byte-identically except where Campaign 0 already
  re-baselined; asserted by test.

**Observable ending:** one almanac, two peoples, visibly different social
structures, each recountable by `why` to its psychology vector. Religion
still runs single-species here; pantheon divergence waits for The Eyes.

## 6. Campaign 2: The Eyes

Where the raw demo lands. Opens with its own spec; the binding decisions:

- **Perception vector**, closed at three dimensions, authored per species:
  activity cycle (diurnal / nocturnal / crepuscular), night vision (bounded
  scalar), sky attention (celestial vs. terrestrial perceptual weighting).
  Deliberately excluded: spectral response curves, hearing, smell — later
  tiers if ever.
- **The kernel seam:** phenomena queries take the `ObserverContext` left by
  Campaign 1a; salience becomes a function of (observer, phenomenon). A
  nocturnal species ranks moons and night-stars above the sun; a
  low-sky-attention species ranks seasonal and ambient phenomena above
  celestial ones. This is a kernel + composition-root change; no producing
  domain is edited, and consumers stay cause-blind — religion never learns
  what produced a phenomenon nor why the ranking is what it is.
- **Religion goes two-species:** religion tier 1 runs per species-flagship,
  consuming that species' phenomena ranking and summaries. Same code, two
  runs, two pantheons.
- **The blind-attribution metric is defined in this campaign**, not
  discovered in Campaign 4 — this is the named mitigation for the reskin
  risk.
- **Preregistered calibration** (ADR 0016), stated before the study runs:
  head-deity domain (solar / lunar / ambient) as a function of activity cycle
  × lock state, asserted row-by-row across 10,000 worlds.

**Observable ending:** the inverted Year-1 demo in raw form — one seed, one
sky, the goblins' pantheon beside the second species' differently-headed one,
divergence traceable through `why` to the perception profile.

## 7. Campaign 3: The Tongues

**A new domain crate, `domains/language`** (kernel-only). Opens with its own
spec; the binding decisions:

- **Scope line (bright):** naming + register. No lexicon, no syntax, no sound
  change. Names have shape and voice, not glossable meaning; place names with
  meanings ("Cold-Ford") and sound-change fossils belong with deep time and
  stay there.
- **Phonemes are feature-bearing segments; spellings are views.** The
  internal model is a segment inventory with articulatory features; the
  almanac shows a romanization, the book shows IPA — two renderings of the
  same truth. A `hornvale phonology` reference verb joins `concepts` and
  `streams` as a drift-checked book-page generator (per-species inventory
  tables, IPA transcriptions of sample names).
- **Articulatory envelope** — the species crate's third closed vector:
  authored per-species data declaring which segments the anatomy permits.
  Psychology gave species minds, perception gave them eyes, articulation
  gives them mouths — and the two languages differ for recountable reasons.
  Segments outside human IPA are permitted as authored extensions with a
  defined romanization and best-effort IPA-adjacent notation.
- **Phonology per species-culture:** phoneme inventory and syllable
  phonotactics drawn from labeled streams (`language/<species>/phonology/...`)
  constrained by the envelope, giving each language a consistent, distinct
  mouth-feel and a combinatorially large name space (replacing the
  10-syllable pool's ~1,100 names). In-world uniqueness via deterministic
  re-draw within the same labeled stream, identical on pinned and unpinned
  paths (pin-isolation-tested).
- **Naming grammars per name-kind:** settlement names, place names, deity
  names and epithets each get morphological patterns (compounding,
  reduplication, honorific affixation); deity-name morphology keys to the
  psychology vector's status basis, so a ranked society's god-names carry
  honorifics a flat society's don't.
- **Register:** belief statements and myth lines render through per-species
  voice parameters (formality, repetition, epithet density) — the species'
  myths differ in how they're told, not only in what they say.
- **Save format:** settlement names are committed facts, so regenerated names
  land under epoch suffix (`settlement/name/v2`) per ADR 0006; the old
  predicate is never renamed. The fixed epithet pools in `domains/religion`
  are deleted in favor of language-provided names, wired at the composition
  root.
- **TTS is an ungated offline convenience:** a small script (espeak-ng,
  driven at phoneme level; Kirshenbaum notation if Unicode IPA input proves
  awkward) in `scripts/` for hearing what an IPA character sounds like. Never
  in the CI gate; no committed audio artifacts.

**Observable ending:** a full almanac with zero English-template proper nouns;
the two species' pages legibly different in voice; the Bolzag×3 collision era
over.

## 8. Campaign 4: The Meeting

Deliberately light on new machinery; its job is to cash the year's check as
Campaign 5 did for Year 1.

- **The preregistered comparative study suite** (all hypotheses stated before
  running, ADR 0016): head-deity domain × activity cycle × lock state; social
  verticality × psychology (status basis, in-group radius); naming-morphology
  divergence × articulatory envelope; the blind-attribution metric at its
  preregistered accuracy threshold across 10,000 worlds; and the **null
  control** from §2 (identical-vector species must be distributionally
  indistinguishable and score at chance on blind attribution).
- **The capstone artifact:** the successor to `the-gods-seed-42.md` — one
  seed, one sky, two peoples side by side: two flagships placed for different
  reasons, two social structures, two pantheons, two languages, myths in two
  voices, every divergence recountable by `why` to a psychology, perception,
  or articulation parameter.
- **Book close:** Year-2 chronicle; model cards for `domains/species` and
  `domains/language` (each parameter declared derived vs. approximated vs.
  drawn vs. authored); freshness sweep; concept-registry review.

## 9. Cross-cutting

**Constitutional compliance, by constraint:** two new domain crates,
kernel-only deps (ADR 0002); all cross-domain flow via composition-root
summaries plus registered facts (0003); no new dependencies — phonology and
naming grammars hand-rolled on std (0004); all registries ordered (0005);
every new drawn quantity permanently labeled, regenerated names under epoch
suffix (0006); seeds never retried (0007); bounded dimensionless parameters
stay bare `f64` (0008); no ML at runtime — any LLM help authoring species
data or grammar tables is offline, committed, drift-checked (0009); new
predicates value-kind enforced, subject-kind refused (0010); new measurements
are Rust extractors (0011); config JSON (0012); DoD includes the book (0013);
all claims preregistered (0016).

**Testing spine:** per-campaign property batteries (same seed → byte-identical
two-species almanac; pin isolation for every new pin), calibrations asserted
row-by-row in the drift study, censuses re-baselined exactly once (Campaign 0)
and then held for the year.

**Risks, named:**

1. **Ontology trap** — mitigated by three small closed vectors and the rule
   that widening any vector needs its own campaign.
2. **Reskin failure** (the demo's real risk) — mitigated by defining the
   blind-attribution metric in Campaign 2 and preregistering its threshold.
3. **Goblin identity drift** — parameterization must reproduce today's goblin
   byte-identically except where Campaign 0 re-baselined; asserted by test.
4. **Scope creep at The Tongues** — the no-lexicon line is bright; glossable
   names are Year-3 bait.

**Explicitly deferred** (decisions, not omissions): society across the full
~58-settlement scatter and diffusion/relatedness studies; the event ledger
and all of deep time (presumptive Year-3 spine); the epistemic layer; sound
change and the proto-lexicon; per-individual variation.

## 10. Process

Campaign 0 proceeds from this spec directly to an implementation plan.
Campaigns 1–4 each open with their own spec (book chapter first, per
book-driven development), answering to this metaplan; binding decisions
recorded above carry into those specs unchanged unless superseded with new
information. Standing structural rules apply throughout: any campaign
abortable at a week boundary; no domain leads another by more than ~2 tiers;
nothing exists until observable.
