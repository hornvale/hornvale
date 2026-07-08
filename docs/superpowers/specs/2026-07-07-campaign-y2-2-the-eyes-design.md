# Campaign 2 (Year 2): The Eyes — Design

**Date:** 2026-07-07
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-07-year-2-metaplan-design.md` (§6 binds this campaign; Constitution §2 governs)
**Provenance:** The second spine campaign of Year 2, following Campaign Y2-1
(The Peoples, merged 2026-07-07). Y2-1 gave the world two peoples with
diverging placement and social structure; religion stayed goblin-only. This
campaign gives each people its own eyes, and therefore its own gods.
Downstream campaigns consume what it ships: The Tongues (myths rendered in
voice), The Meeting (comparative studies over the two pantheons).

---

## 1. Goal

Make salience a function of the observer. A **perception vector** (authored
per species, closed at three dimensions) derives a **perception lens** that
reweights phenomenon salience by venue; each species observes the sky at its
**characteristic hour**; religion tier 1 runs **per species-flagship** —
same code, two runs, two pantheons. The deliverable is the inverted Year-1
demo in raw form: one seed, one sky, the goblins' solar-headed pantheon
beside the kobolds' moon-headed one, the divergence traceable through `why`
to the perception profile.

The kobold perceptual identity is authored (Nathan's taste call, recorded
here): **moon-eyed stargazers** — nocturnal, high night vision, *high* sky
attention. The deep-dweller alternative (low sky attention) was considered
and set aside for this campaign: with today's phenomena the terrestrial
channel holds only climate's 0.15-salience ambient air, so an earthy
pantheon has no material until terrestrial phenomena are enriched (deferred,
§12).

Explicitly **later**: spectral response curves, hearing, smell (later
perception tiers, if ever); per-individual perception; sky-report prose
rendered through species eyes (The Tongues); the distributional-twin control
*run* (The Meeting; *defined* here, §9).

## 2. Design principles

1. **Venue is character, not cause.** The trace protocol survives intact:
   consumers still never learn which system produced a phenomenon. What they
   may now see is *where the phenomenon lives* — day sky, night sky, or the
   ambient world — which is character in the same sense as `period_days`.
   The producer is the only honest knower of venue, so producers declare it.
   This refines the metaplan's "no producing domain is edited" with new
   information discovered at spec time: sun and moons share the phenomenon
   kind `celestial-body`, so no kind→venue map (kernel- or root-side) can
   distinguish solar from lunar without a heuristic on salience or period —
   a load-bearing coincidence that breaks silently. Producer edits are
   limited to one venue declaration per phenomenon. Rejected alternatives,
   recorded: a composition-root kind→venue map (sun/moon disambiguation by
   `raw salience == 1.0`); splitting phenomenon kinds (`sun`, `moon`, …),
   which rewrites committed `derived-from-phenomenon` facts and breaks the
   goblin byte-identity contract.
2. **The hour decides what is seen; the lens decides what demands
   attention.** Two knobs, each with one job. Night-stars are emitted only
   when the producer sees night (or a locked sky); no reweighting can
   conjure a phenomenon that was never observed. So the composition root
   picks each species' observation moment (its characteristic hour), and the
   lens reweights what that moment yields. Rejected: merged day+night
   observation (dedup/tie-break machinery in the kernel's most
   determinism-sensitive path); reweight-only at day 0.0 (a nocturnal
   species on a spinning world never sees a night-star).
3. **Identity at the goblin baseline, byte-level.** The goblin perception
   vector (Diurnal, 0.5, 0.5) derives the identity lens, and the identity
   path applies **no arithmetic at all** — same moment, same salience
   doubles, same ordering, asserted by test. Every derivation below is
   identity at the baseline and monotone in one dimension, the Y2-1 pattern.
4. **Ontology-trap posture.** The perception vector is closed at three
   dimensions, all authored; this campaign draws nothing new except kobold
   epithets. No spectral curves, no per-individual variation, no extensible
   channels. Widening the vector requires its own campaign.
5. **Layering holds.** The lens and venue are kernel vocabulary
   (`hornvale-kernel::phenomena`); lens derivation and the characteristic
   hour live in the composition root (`windows/worldgen`); producing domains
   change only by declaring venues; religion is untouched except for
   species-qualified stream labels. Adding a future domain still edits no
   existing one — a new producer declares its own venues.
6. **5E is the authoring corpus** (the Y2-1 method). Kobold values derive
   from SRD lore through our dimensions — darkvision and sunlight
   sensitivity read as nocturnality and high night vision — each value
   carrying a one-line justification in the species model card. Values are
   Nathan's to retune; the design requires only meaningful contrast and
   identity at the goblin baseline.

## 3. The kernel seam

In `kernel/src/phenomena.rs`:

- **`Venue`** — `enum { DaySky, NightSky, Ambient }`, serialized like every
  kernel type. `Phenomenon` gains `pub venue: Venue`, producer-declared.
- **`PerceptionLens { day_sky: f64, night_sky: f64, ambient: f64 }`** —
  multiplicative venue weights. `PerceptionLens::identity()` is all-1.0, and
  `is_identity()` gates the transform: the identity path performs no
  multiplication, no clamp, no rounding — byte-identical output by
  construction, not by float luck.
- **`ObserverContext`** gains `pub lens: PerceptionLens` (the extension
  point left in Campaign 1a, spent as designed). A constructor
  `ObserverContext::at(place, time)` supplies the identity lens; existing
  struct-literal call sites (worldgen, domain tests) are updated
  mechanically.
- **`observe()`** applies `perceived = round2(clamp(raw × weight, 0, 1))`
  per phenomenon before the existing sort. Sort keys and tie-breaks
  (salience `total_cmp`, then kind, then description) are untouched. Clamp
  saturation (several night phenomena reaching 1.0 under a strong lens) is
  accepted; ties resolve deterministically as they always have. Exact lens
  constants are tuned at plan time so the seed-42 demo ranking is not a wall
  of 1.0s.

**Venue declarations for every existing phenomenon** (the authored table;
one-line edits in producers):

| Producer        | Phenomenon             | Venue    | Note                                                               |
| --------------- | ---------------------- | -------- | ------------------------------------------------------------------ |
| astronomy       | the sun (spinning)     | DaySky   |                                                                    |
| astronomy       | the fixed sun (locked) | DaySky   | it defines the day side                                            |
| astronomy       | moons                  | NightSky | salient at night, whenever seen                                    |
| astronomy       | night-stars            | NightSky |                                                                    |
| astronomy       | seasonal cycle         | Ambient  | felt through the whole world, not watched — authored call, tunable |
| climate         | ambient air            | Ambient  |                                                                    |
| kernel (tier 0) | ConstantSun            | DaySky   |                                                                    |

## 4. The perception vector

`PerceptionVector` in `domains/species`, closed at three, every dimension
**authored** (model card: nothing drawn):

| Dimension      | Type                                     | Goblin (baseline) | Kobold    | 5E derivation (kobold)                                                           |
| -------------- | ---------------------------------------- | ----------------- | --------- | -------------------------------------------------------------------------------- |
| activity cycle | enum `{Diurnal, Nocturnal, Crepuscular}` | Diurnal           | Nocturnal | darkvision 60 ft, sunlight sensitivity                                           |
| night vision   | `f64` [0,1]                              | 0.5               | 0.9       | darkvision; a life underground                                                   |
| sky attention  | `f64` [0,1]                              | 0.5               | 0.8       | omen-readers and dragon-watchers; the warren's night is spent under the open sky |

`Crepuscular` completes the closed vocabulary bound by the metaplan; no
Year-2 species uses it — declared idle in the model card (the Y2-1
deliberation-latency precedent), with its activity factor authored now so a
future species is a data change.

**Lens derivation** (composition root, identity at baseline, monotone per
dimension; constants tunable at plan time):

```text
activity_factor: Diurnal 1.0 | Crepuscular 0.7 | Nocturnal 0.4
day_sky   = activity_factor · (0.5 + sky_attention)
night_sky = (0.5 + night_vision) · (0.5 + sky_attention)
ambient   = 1.5 − sky_attention
```

Goblin: (1.0, 1.0, 1.0) — the identity lens. Kobold: (0.52, 1.82, 0.70) —
the big moon (raw 0.70) overtakes the sun (1.0 × 0.52), and on moonless
worlds the brightest night-star does.

## 5. Composition root: the characteristic hour and two religions

`windows/worldgen`:

- **Characteristic hour.** Per species, from the calendar: Diurnal → day 0.0
  (today's exact path); Nocturnal → the first non-daylight instant at or
  after day 0.0, found by a deterministic scan of the calendar's
  day/night boundaries; Crepuscular → the first light/dark boundary. Worlds
  without a day/night cycle (tier-0 constant sun; locked rotation) observe
  at day 0.0 regardless of activity cycle — a locked sky makes nocturnality
  moot, which the calibration matrix (§9) captures rather than hides.
- **Observation place** stays `terrain::places()[0]` for every species this
  campaign. Sources ignore place today, so anchoring at the species
  flagship would be a silent no-op pretending to be meaningful — recorded
  as a debt (§11), not smuggled in.
- **Religion runs per species-flagship**, registry order (goblin first,
  `BTreeMap` order), replacing the goblin-only gate: each species-flagship
  gets `observed_phenomena` through its own lens at its own hour, and its
  own `SocietySummary` (kobold castes exist since Y2-1). A species that
  placed no settlements runs no religion (the Y2-1 hostile-world rule).
- **Streams:** goblins keep the legacy `religion/epithet` label untouched.
  Non-goblin species draw epithets under **`religion/<species>/epithet`**
  (`religion/kobold/epithet`) — permanent manifest additions per ADR 0006.
  Without qualification the kobold run would replay the goblin epithet
  sequence from the same derived stream. This is the campaign's **only new
  randomness**; there are no new pins (`--species` already restricts; an
  authored vector has nothing to pin).

## 6. Facts, predicates, `why`

- Species entities gain three committed perception facts (~3 new registered
  predicates: activity cycle as Text, night vision and sky attention as
  Number; exact names settle at the concept-registry review; value-kind
  enforced per ADR 0010).
- Religion's committed fact shapes are unchanged. The kobold pantheon's
  beliefs use the existing predicates on new belief entities.
- `why <belief>` recounts a high god to its source phenomenon
  (`derived-from-phenomenon`, unchanged) and now also to the species'
  perception facts — "the warren's high god is the large moon because
  kobold eyes weight the night sky at 1.82 and the warren observes at
  first dark" is the target altitude for the recounting.

## 7. The byte-identity contract (keystone test)

- **Strict (the Y2-1 superset rule):** `hornvale new --seed 42 --species
  goblin` reproduces pre-C2 main as a **superset**: the almanac is
  byte-identical, and the ledger restricted to pre-C2 predicates is
  identical — new facts appear only under the three new perception
  predicates (which land on the existing species entities; no new
  entities, no new draws). This holds by construction: no kobolds → no
  kobold beliefs; the goblin lens is identity (no arithmetic); the goblin
  hour is day 0.0. Mechanism: fixtures
  (`tests/fixtures/pre-eyes-seed-42.*`) committed before the first code
  change; a CI test regenerates and asserts both properties.
- **Default two-species world:** goblin belief entities and facts are
  identical (kobold beliefs mint *after* all goblin beliefs), and the
  almanac's goblin pantheon section is textually identical. **Species-entity
  ids shift** (kobold beliefs mint before species genesis, which stays
  last): accepted, with the rejected alternative recorded — reordering to
  goblin beliefs → species entities → kobold beliefs would preserve ids at
  the cost of running the two species' religions at different pipeline
  stages, an asymmetry every future campaign must remember; and species
  facts change this campaign anyway (three new predicates).
- **Re-baseline once, at close:** committed artifacts (three seed-42
  almanacs, censuses, drift study) shift exactly once, after all code
  changes land, as the campaign's final act before the book close — the
  Y2-0/Y2-1 precedent.

## 8. Demo surface

- The almanac gains the kobold pantheon section beside the goblin one, each
  rendered with its species vocabulary (Y2-1's stopgaps, still in place
  until The Tongues).
- The REPL gains **`phenomena [--as <species>]`** — the salience-ranked
  perceived phenomena for that species (default goblin), one call into the
  same `observe()` path the religion run uses. Unknown species fail loudly
  listing the registry, like `--species`. The almanac's *existing*
  phenomena listing stays unlensed — the sky as instruments record it —
  which also keeps it byte-identical; the per-species view is the REPL
  verb's job.
- `why` recounts pantheon divergence per §6.

## 9. The Lab

Preregistered before any census runs (ADR 0016), over the standard census
population (generated skies; tier-0 worlds are out of scope — on a
sky-with-no-moons-or-stars world a kobold head deity is legitimately solar,
and the census does not build such worlds):

1. **Head-deity domain calibration** (the metaplan's named calibration):
   head-deity domain as a function of activity cycle × lock state, asserted
   row-by-row at 10,000 seeds. Domain is the venue of the head belief's
   source phenomenon: DaySky → **solar**, NightSky → **lunar** (moons and
   night-stars both — the night-sky domain, named per the metaplan),
   Ambient → **ambient**. Directional preregistration: the goblin head is
   always solar (the sun's raw 1.0 is untouchable under the identity
   lens); the kobold head is lunar on **every world with at least one
   moon** (a moon's raw ≥ 0.35 × 1.82 always beats the sun × 0.52). On
   moonless worlds the kobold head splits between night-stars and the sun
   by star brightness — deliberately *not* preregistered as "never solar"
   (a moonless world with faint stars legitimately yields a sun-headed
   kobold pantheon); the split is pinned as calibration rows after the
   model is fixed (Y2-1 practice).
2. **The blind-attribution metric** (the named anti-reskin mitigation,
   defined this campaign): strip every lexical channel (settlement and
   deity names, epithets, tenet text); represent each flagship pantheon by
   structure alone — head-deity domain, pantheon size, cult form,
   ranked-or-flat, cyclic-deity share; attribute species with a fixed,
   deterministic decision rule in metric code (metrics are code, ADR 0011;
   no ML in the sim core, ADR 0009). Preregistered, directional: attribution
   accuracy well above chance on the real vectors; the exact threshold is
   pinned as a calibration row after measurement. The **distributional-twin
   control** (a second species carrying the goblins' exact vectors must
   score at chance) is defined by this metric and *runs* with The Meeting's
   comparative studies.

New metrics: per-species head-deity domain, pantheon size, cult form.
**Study 007** records the two-pantheon baseline. All censuses re-run at 10k
at the re-baseline (§7).

## 10. The book (opens the campaign)

Per book-driven development, the campaign opens with
**`book/src/domains/perception.md`**: why salience must be a function of the
observer; the lens and the hour as two knobs with one job each; venue as
character, not cause; the closed-at-three vector and the ontology-trap
posture; the model card (every dimension authored, kobold values with their
5E derivations, Crepuscular declared idle). At close: the religion chapter
goes two-species, Study 007, chronicle entry, freshness sweep (the
"religion is goblin-only" notes across the book all fall), the campaign
retrospective (decision 0020), and the concept-registry review for the new
predicates.

## 11. Recorded debts

Two compromises this campaign consciously extends, each captured as an idea-
registry row so it can be found and fixed rather than fossilized:

- **Goblins are not a "default" race** (registry: PSY-2). The
  goblin-as-baseline convention — identity vector, unqualified legacy
  stream labels (`religion/epithet`, `settlement/name`) — is a measurement
  frame inherited from goblins being first, never a design commitment. The
  fix path exists: epoch-suffixed label migration (`settlement/name/v2`,
  ADR 0006) plus making the baseline an abstract reference vector no
  species need occupy. This campaign extends the convention (kobolds get
  qualified labels; goblins keep legacy ones) because renaming streams is
  save-format surgery that deserves its own campaign, not a rider.
- **Phenomena-path fidelity** (registry: SEQ-4). Generated worlds still
  observe through tier-0 `UniformClimate`; observation is anchored at
  `terrain::places()[0]` and genesis day 0 rather than the observer's own
  cell and era. A future hardening campaign (Firm-Ground-style) upgrades
  the observation path; this campaign deliberately does not widen its scope
  to include it.

## 12. Success criteria

1. Seed 42's almanac shows the solar-headed goblin pantheon beside the
   moon-headed kobold pantheon, and `why` recounts each head deity to its
   source phenomenon and its species' perception facts.
2. The `--species goblin` byte-identity fixture test (§7) passes in CI.
3. The head-deity-domain calibration is confirmed and pinned; blind
   attribution meets its preregistered threshold; the full gate
   (`cargo test --workspace`, fmt, clippy) and the artifact drift check are
   green.
4. The book carries the perception chapter, the two-species religion
   chapter, Study 007, and the chronicle — comprehension-gated as always.

## 13. Explicitly deferred

Spectral response curves, hearing, smell (later perception tiers, if ever);
per-individual perception; species-eyed sky-report prose (The Tongues);
terrestrial phenomena enrichment (the deep-dweller pantheon's prerequisite);
the distributional-twin control run and full comparative studies (The
Meeting); culture consuming perception; observation place/era fidelity
(SEQ-4); de-privileging the goblin baseline (PSY-2).
