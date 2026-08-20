# The Cant — design spec

**Myth thread, campaign 8.** The first campaign of the **evaluative-beliefs**
program: a derived, relational *snap-judgment predisposition* between peoples.
Autopilot engaged. Branched from main `ffc247f3`.

> A cant is a regime's self-justifying moral talk — but before a regime can
> cant, a creature must already *lean*. The Cant models the lean: the instinctive
> predisposition one people holds toward another, derived from what each people
> materially *is*, never authored.

## 1. The question

The Cupel found the myth engine's derived people-preference behaviourally
decorative, because belief was coarse-bucketed **and only ever factual**. The
evaluative-beliefs program attacks the second half: **contestable claims with no
true value** — a people is admirable or contemptible, a neighbour to be feared,
envied, pitied. Two peoples can hold such claims in genuine, unresolved conflict
where a remembered date cannot differ.

Decision **0021** forbids the engine an *authored* preference between peoples;
any valuation must be **derived from material conditions**, and it is emphatically
**not an alignment axis** — no people is good or evil. The Cant's first layer
asks: **do believable, relational, instinctive predispositions emerge from what
peoples materially are?** A human's recoil from a slime-skinned aquatic people,
a predator-descended people's wariness of all it could hunt or be hunted by — as
an *output* of attributes and the judger's own psychology, never a lookup.

This is **measure-first** (the Cupel discipline): the deliverable is a
believability readout, and a null — "the derivable axes alone do not produce
believable predispositions" — is a legitimate finding that would point at the
new-substrate axes (§6) rather than a failure.

## 2. The architecture (three layers; this campaign is layer 1)

```
  1  BASELINE  v(A->B)  — per-axis attribute distance, weighted by A's own
                         psychology, projected onto warmth × competence -> an
                         emotion. A DISCOVERED true distance; world-invariant.   <-- THE CANT (c8)
  2  PERTURBATION       — a seeded per-pair draw at genesis; usually small,
                         rarely large enough to flip a mild lean.               <-- c9+
  3  FEEDBACK           — a bistable cycle (vicious/virtuous basins, hysteretic)
                         through raid/tribute/alliance history; racecraft is the
                         INVENTED inflation of the true distance into an
                         inferiority-myth, held (decision 0100), false.          <-- c9+
```

The Cant delivers **layer 1 only** — the baseline predisposition and a
measurement of its believability. Perturbation, the bistable feedback that makes
worlds diverge, and the racecraft inflation are later campaigns.

## 3. The model

**The valuation is relational and derived:**

```
  v(A -> B) = Σ over axes of [ w_A(axis) · distance(A, B, axis) ]
```

where the **judging people A's own psychology sets the weights `w_A`**. This
single shape gives the properties 0021 and believability require: it is
**asymmetric** (`v(A->B) != v(B->A)`), a naturally insular people (`in_group_radius`)
dislikes everyone more, two similar peoples find little to dislike, and no people
is ranked in the absolute — a valuation is always *someone's*.

**Symmetric vs asymmetric axes.** Each axis is one or the other:

- **Symmetric** (`distance(A,B) == distance(B,A)`): mutual strangeness — both
  peoples find each other odd. Morphology-proxy, habitat, condition-niche,
  sociality, activity-cycle, reproductive tempo, religion, language.
- **Asymmetric** (`distance(A,B) != distance(B,A)`): directional — predation (I
  hunt your kind ≠ you hunt mine), size/threat, and (deferred) disease-cue
  susceptibility. This is where predator/prey and dominance dynamics live.

**The output is 2-D, not a scalar** (the Stereotype Content Model). Each axis
carries a *signature* on the **warmth × competence** plane; the weighted sum
lands the pair at a point, and the quadrant names the *emotion*:

```
              high competence
  ENVY  (fear+respect)  |  ADMIRATION (ally, kin)
  --------- low warmth --+-- high warmth ---------
  CONTEMPT (the racecraft |  PITY (harmless lesser)
            target)       |
              low competence
```

A scalar "like/dislike" cannot distinguish the envied overlord from the despised
vassal; the plane makes contempt, envy, pity and admiration fall out
mechanically — and it is the seam where layers 2-3 (tribute, raids) will later
*interact* (a vassal read as contempt, an overlord as envy), rather than root
anything.

**The weight-vector is a people's derived "prejudice personality."** The axes are
universal; a people's `w_A` is not. A predator-descended people weights
size/threat; an insular people (`in_group_radius`) weights every axis up; a
rigidly hierarchic people (`status_basis`, sociality) weights sociality-mismatch
("they keep no proper order"). `w_A` is derived from A's own committed
psychology/ecology — the 0021-clean locus of *whose* prejudice.

## 4. What the campaign builds, and where it lives

Layer 1 is a **pure derived field** over species × species:
`snap_judgment(A, B) -> (warmth, competence)` (and the emotion it classifies to),
computed from the peoples' committed attribute vectors plus religion/language
distances. It is **re-derivable, never committed, and holds no per-holder
belief** — the held, transmissible `Claim` (decision 0100) is layer 3's job, not
this campaign's.

**Consequences:**
- **No epoch, no save-format change.** The Cant adds no kernel `Value`/`Claim`
  variant and no committed predicate; it is a read over existing facts, like
  `windows/hearsay/src/derive.rs::crossing_penalty`.
- **Home: a window** (a cross-domain read; no domain may depend on a sibling, and
  this reads species + religion + language + niche at once). Candidates: a new
  `windows/sentiment`, or a module under `windows/worldgen`/`windows/lab`. **The
  home is a G3 decision** — leading candidate is a small new window, with the
  believability readout as a heavy `lab`/test battery over it.

## 5. The axis set for this campaign (derivable now)

Each axis derives from committed fields the substrate survey confirmed
(`domains/species/src/lib.rs` unless noted); the implementer verifies the exact
API at Task 0. **Every axis is a normalized distance in `[0,1]`.**

| axis | derives from | sym/asym | warmth/competence signature |
|---|---|---|---|
| habitat / domain | `HabitatRealm` (Surface/Subterranean), marine-forage + moisture `ConditionNiche` | sym | ↓ warmth (eerie realm) |
| diet / predation | `ResourceVector`, `guild_overlap`, predator/prey edges (`domains/demography`) | **asym** | ↓ warmth; predation ↑ threat-competence |
| condition niche | `ConditionNiche` 4-axis distance | sym | ↓ warmth (mild) |
| sociality mismatch | `SocietyVector` (Hierarchic/Communal, `status_basis`) | sym | ↓ competence ("no order") |
| activity cycle | `PerceptionVector` (diurnal/nocturnal, night-vision) | sym | ↓ warmth ("of the night") |
| reproductive tempo | `reproductive_tempo`, r-vs-K from `allometry` | sym | ↓ warmth ("breed like vermin") |
| religion | deity / doctrine / `Sentiment` distance (`domains/religion`) | sym | ↓ warmth (alien rites) |
| language | articulation-vector distance (`domains/language`) | sym | ↓ competence (unintelligible) |
| size / threat | `SPECIES_MASS_KG`, predation danger | **asym** | ↑ threat-competence, ↓ warmth |

**Weights `w_A`** derive from A's own psychology: `in_group_radius` (a global
multiplier — insularity), `threat_response` (weights predation/size), and
`status_basis`/sociality (weights sociality-mismatch). The exact weight law is a
Task-2 design detail, frozen after the Task-0 probe.

**Deferred to c9+ (need new derived substrate):**
- **Appearance / morphology** — there is *no* visual-appearance field today (only
  vocal-tract articulation vectors). "Slimy, fishlike" has no home yet. This is
  the most vivid axis and its absence is a real limitation of c8 (§7).
- **Disease-cue / purity** (the behavioral immune system) — no disease-proneness
  or cue-mapping exists. Its three-level derivation (A prone to disease D; cue X
  carries D; B resembles X) is a c9 substrate-building campaign of its own.

## 6. Task 0 — substrate probe (grounds the freeze, Cupel discipline)

Before the believability criteria (§7) are frozen, a probe measures whether each
axis **actually varies** across the 15 peoples enough to produce distances — a
degenerate axis (every people identical on it) contributes nothing and reading it
would be noise. For each axis: the spread of pairwise distances across the 15×15
matrix; how many axes are non-degenerate; and whether the *weight-vectors* differ
across peoples. **Viability:** more than one axis varies, and the weight-vectors
are not all identical. The probe reshapes §7's frozen criteria the way the
Cupel's did.

## 7. Preregistration — believability criteria (structural; specifics reported, not asserted)

*Frozen in this spec after Task 0 (decision 0016). The trap to avoid: authoring
the expectation. Asserting "human must dislike drow" would smuggle an authored
ranking back in, violating 0021. So we assert **structural** properties any
believable derived system must have, and **report** the specific pairs (human→drow,
elf→kobold, …) for face-validity without asserting them.* (Kuo-toa is Nathan's
illustration and is **not** a modeled people; the readout uses the real 15.)

Frozen structural criteria (exact thresholds set by Task 0):

- **Relational / asymmetric.** For the asymmetric axes (predation, size),
  `v(A->B) != v(B->A)` on the pairs where the underlying attribute is asymmetric;
  and pure-strangeness pairs are symmetric. Measured as the asymmetry of the
  matrix.
- **Non-degenerate — it likes as well as loathes.** All four emotion quadrants are
  populated across the 15×15 matrix; the valuation is neither all-negative (a mere
  hate-generator) nor all-zero. A believable system produces admiration and kinship
  toward similar peoples, not only contempt.
- **Similarity → warmth.** Low total attribute-distance pairs land in the
  positive-warmth half; the correlation of distance with (negative) warmth is
  strong and signed.
- **Distinct prejudice personalities.** Different peoples' rows are recognizably
  different: an insular people (`in_group_radius`) dislikes across the board; a
  people's dislikes correlate with the axes its `w_A` weights. Measured as row-
  profile dispersion and the weight→outcome correlation.

**Reported, not asserted** (face-validity): the full 15×15 emotion matrix, the
strongest contempt/envy/pity/admiration pairs, and per-people weight-vectors — so
a human reader can judge whether the numbers are *believable*, which is the one
thing no assertion can settle. A criterion falsified is a finding: if the
derivable axes alone cannot clear the structural bar, that points at the
appearance/disease substrate (§5 deferred), and the campaign says so.

## 8. Non-goals

- **Not** the held, transmissible belief — no `Claim`/myth-register work, no
  transmission or opinion-dynamics garble law (c9).
- **Not** the perturbation or the bistable feedback that makes worlds diverge (c9+).
- **Not** racecraft — the invented inflation of a true distance into an
  inferiority-myth (c9+).
- **Not** the appearance or disease axes — they need new derived substrate (§5).
- **Not** an alignment axis, and **not** an epoch — no authored ranking, no
  save-format change.

## 9. Decisions & risks

- **0021 compliance is the load-bearing constraint.** Every axis is a real,
  symmetric-or-directional attribute *distance*; the only thing that "ranks" is a
  people's own derived weight-vector applied to itself as judge. No constant
  encodes a preference between two peoples. The believability readout **reports**
  specific pairs and **asserts** only structure, precisely so no authored
  expectation leaks in.
- **The appearance/disease deferral is a real limitation, disclosed.** The most
  vivid prejudice cues need substrate that does not exist. c8 measures whether the
  derivable axes *alone* suffice; a weak result is a finding, not a failure.
- **Home** (new window vs worldgen/lab module) — decided at G3/plan.
- **Illustrative kuo-toa is not modeled** — the readout uses the real 15 peoples.
- Deliver as a heavy believability battery with re-derivable reported numbers
  (Cupel precedent), not a census metric.
