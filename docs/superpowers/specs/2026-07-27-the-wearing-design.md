# Campaign: The Wearing — Design

**Date:** 2026-07-27
**Registry rows:** LANG-55 (→ `shipped`), LANG-9 (→ `shipped`), LANG-11
(→ in-progress at the opacification phase), LANG-27 (constraint lifted by S1)
**Predecessors:** The Words (glossed names, the drawn stem), The Speakable
(attested tier, LANG-32), The Accession (`EPOCH_COHORTS`, additive concept
registration), The Residue (LANG-43, Zipf-length-derived leveling)
**Governing decision:** 0024 — settlement-name uniqueness is a reference-time
property
**Status basis:** brainstormed under campaign-autopilot; decision ledger in the
worktree's `.superpowers/sdd/decision-ledger.md`

## 1. Goal

Hornvale's place names are too long to say and too uniform to believe. After
this campaign a settlement is called something a person could repeat after
hearing it once, and its name means something about where it is.

The measured target: the mean-name-length metric's own declared buckets stop
at 10 characters, and **every world in the 1000-seed census overflows them**.
That metric encodes the design intent; the campaign's job is to make reality
meet it.

## 2. The defect, precisely

Three findings, all measured or read off the code rather than asserted.

**2.1 Length.** From `book/src/laboratory/generated/the-census/rows.csv`
(1000 seeds):

```
name-length-goblin: n=769  min=5.50  p25=11.12  med=13.15  p75=15.60  p95=18.92  max=25.38
name-length-kobold: n=769  min=3.91  p25=10.00  med=12.51  p75=15.62  p95=20.78  max=28.59
```

Declared buckets for both: `[2,3,4,5,6,7,8,9,10]`.

**2.2 The cause is morpheme count, not spelling.** Sampling the committed
gallery names: mean 14.4 characters over 4.3 syllables — **3.4 characters per
syllable**, which is unremarkable (Bristol 3.5, Winchester 3.3). The `ng`/`zh`/
`sh` digraphs are not the problem. **4.3 syllables per name is.** Real
toponyms run 1–3.

The syllables come from stacking, at `domains/language/src/naming.rs:245-280`:
a settlement name is 1–2 site-concept lexicon words (each possibly itself a
compound) **plus** a freshly drawn 2–3 syllable stem. Four to eight syllables
by construction.

**2.3 The descriptor space is two facts wide.**
`windows/worldgen/src/lib.rs:4659`:

```rust
let mut site_concepts: Vec<&str> = vec![biome_concept];
site_concepts.extend(presiding);   // the presiding sky phenomenon
```

Biome plus sky. Roughly a dozen biomes against a handful of phenomena. This
is why `-noaboo` appears verbatim in six different committed names: same
biome, same sky, same word, carried at full weight every time.

And the vocabulary cannot say more. `domains/language/src/packs.rs` quotes 51
lowercase identifiers, most of them concepts and a handful pack labels.
Toponymically usable are `mountain, mouth, sea, water, stone, rock, tree,
earth, wind, fire, light, dark, shadow, night, day`, the colours, and
`one/two/many`. **Absent:** hill, river, lake, valley, coast, island, ford,
marsh, spring, high, low, great, little, new, old, north, south, under, over.

The *facts* exist already. `domains/terrain/src/provider.rs` exposes
`elevation_at`, `is_ocean`, `drainage_at`, `water_kind_at`, `boundary_at`,
`cave_at`, `deposit_at`, `rock_at`, `appearance_at`, `waterfalls()`,
`deltas()`, `playas()` — all in scope at the naming site. Underhill, Seaside,
Redrock, Saltmarsh and Cavemouth are derivable today and unsayable today, and
the missing piece is vocabulary, not machinery. This is LANG-9's standing
claim: *"the naming engine already consumes whatever site facts the
composition root offers."*

## 3. Thesis: five language-wide constants become distributions

The three findings above are one defect wearing three faces. Every axis of
name construction is currently a **constant**, and the uniformity — not any
individual name — is what reads as generated.

```
  axis                        today                      after
  --------------------------  -------------------------  --------------------------
  name shape                  always stem + concepts     per-culture distribution
  nucleus size                every syllable, same        template set, position-
                              vowel count                 conditioned
  morpheme wear               none, ever                 wear proportional to
                                                          corpus frequency
  transparency                100%, by construction      a distribution
  site descriptors            biome + sky, always        a toponymic feature vector
```

That is the campaign in one line: **replace five constants with distributions.**

## 4. What this campaign does *not* do: defend a collision rate

Decision 0024 governs and is dispositive. Uniqueness is a property of a
reference, not of a name. 0024 states, in terms:

> "…no future work 'fixes' the collision rate by adding entropy."

and names the exact failure mode being corrected here:

> "…already foreclose every in-name remedy except stuffing more drawn entropy
> into the string, **which lengthens names without addressing the structural
> fact that meaning collides**."

So the campaign owes **no collision bound**, and must not buy one with length.
The `name-collision-rate` calibration row is a drift witness (exact
zero/nonzero/absent counts), not a bound; it is re-pinned like any other, with
the expected direction stated in advance (§7).

Two consequences worth stating plainly, because the brainstorm initially had
them wrong:

- **S2 does not exist to fund S3.** Descriptor breadth is justified by meaning
  alone — it is what makes a name say *Underhill*. It is not purchasing a
  collision budget, because 0024 already granted the shortening. S2 and S3 are
  therefore independent, not sequential.
- **0024 specifies the remedy this campaign should ship**: a render-time
  qualifier, which it calls "buildable any time — it is a view, so it touches
  no save-format contract." That becomes stage S5, and it is the reason
  shortening is safe.

*Chronology, checked so the sharper version of this claim is not made:* the
drawn stem landed `c49a0dd0` (2026-07-09 18:48) and 0024 landed in merge
`a43f76c8` the same day at 23:59, same campaign. 0024 was ratified **with** the
stem in place and blessed its rate as the baseline. The stem does not violate
0024; 0024 forbids what comes *after* it.

## 5. Stages

### S1 — The codomain subspace (LANG-55)

`assign_proto_roots` sorts accession epoch before `core_rank`
(`domains/language/src/etymology.rs:329-334`; its own comment: *"sorting by
epoch first makes a later-epoch concept land STRICTLY LAST"*), and
`PROBE_BUDGET` (`etymology.rs:401`) lengthens a candidate once the same-length
space saturates.

S2 registers ~20 new concepts in a new cohort. They are the **highest-frequency
morphemes in the entire name corpus**. Without S1 they are guaranteed the
longest forms in the language — the precise opposite of the campaign's purpose.

S1 ships LANG-55 as registered: reserve a phonotactic shape epoch-0 roots
cannot occupy and draw later coinages from it, so additivity holds by
construction of the form space rather than of the assignment order, and core
concepts keep their short forms.

**LANG-55's cost is not paid by this campaign's own additions** (owner call
2026-07-27, ledger #9). The reserved subspace marks later-epoch words as
audibly newer — correct for genuine neologisms, wrong for `hill`, `river`,
`ford`, `coast`, `high`, `low`. No language borrows "hill"; marking these as
loans would encode a gap in *our model* as a fact about *the world*,
permanently. And the additivity LANG-55 protects is moot **inside** this
campaign: `draw_candidate` (`etymology.rs:434`) calls `draw_syllables`, which
reads `ph.nuclei`, which S4 changes — so every proto-root in every language
reseeds regardless. The campaign is already a total regeneration.

So the campaign takes the epoch bump it is already paying for and **re-founds
the cohort baseline**: `ROOT_EPOCH` goes `v3` → `v4`, and `EPOCH_COHORTS[0]`
becomes the roster as of The Wearing — the current cohort 0 (76) plus cohort 1
(15) plus the 19 new concepts, 110 — every one of them sorting by `core_rank`
on merit. No false loanwords; LANG-27's Zipf ordering is restored rather than
deferred again.

The Accession's rule ("never edit an existing cohort") is preserved in spirit:
cohorts are frozen *between* epoch bumps, and an epoch bump is precisely when a
baseline is legitimately re-founded — the "deliberate regeneration uses an
epoch suffix" contract. `cohort_zero_stays_the_frozen_landing_roster`
(`domains/language/src/accession.rs:167`) is re-pinned from 76 to the new
baseline, with the bump as its stated justification. This touches an invariant
that landed the same day, which is why it was brought to Nathan rather than
auto-resolved.

LANG-55 still ships, and still does its job — it protects every concept
addition made *after* this campaign, when additivity is no longer moot.

### S2 — Descriptor breadth (LANG-9)

Two halves, both additive.

**The feature vector.** Widen `SiteConcepts` from `[biome, presiding]` to a
toponymic feature vector derived from facts already in scope at
`windows/worldgen/src/lib.rs:4658`. Candidate classes, in the cross-cultural
frequency order real toponymy exhibits:

- topographic / hydrographic — `elevation_at`, `is_ocean`, `drainage_at`,
  `water_kind_at`, `cave_at`, `waterfalls()`, `deltas()`, `playas()`
- relative position and size — derived from the cell's neighbourhood
- quality and colour — `appearance_at`, `rock_at`
- flora and fauna — where a biosphere supplies them

Founder- and function-class specifics (Birmingham's *Beorma*, Bridgeton) want
the history and demography layers and are deferred (§9).

**The vocabulary.** A new `EPOCH_COHORTS` cohort carrying the ~20 missing
concepts. Exposure gating already does the culturally-correct thing for free: a
landlocked people gets a `Gap` for `sea`, and now for `coast` too, while a
river people gets `river`. The vocabulary widening is automatically
differentiated per culture without anyone authoring that.

### S3 — Toponymic wear and name shape (LANG-11, opacification)

**Wear is keyed to corpus frequency, not to syntactic slot.** Keying on the
slot would *author* the generic/specific asymmetry; keying on frequency
*derives* it — the generic wears most because it recurs most, which is Zipf's
law of abbreviation and the actual mechanism behind OE *hām* → `-ham`, *tūn* →
`-ton`, ON *býr* → `-by`. It also gets right the case the slot rule gets wrong:
a *specific* that happens to be ubiquitous in a culture wears too.

Wear depth is a function of **how many settlements in that culture actually
used the morpheme** — an in-world quantity worldgen already holds, not a drawn
parameter. A rare generic stays whole; a ubiquitous one wears to a stub. That
is `-thwaite` against `-ham`, derived rather than authored.

**The mechanism already exists.** `evolve` (`etymology.rs:675`) is
`pub fn evolve(proto: &[Segment], cascade: &Cascade, ph: &Phonology) ->
Derivation` — pure and total over an arbitrary segment slice, not over lexicon
entries specifically. And `RULE_KINDS` (`etymology.rs:132`) already holds
`VowelShift`, `ClusterSimplify` and `FinalLoss`: precisely the three sound
changes that perform real toponymic wear. S3 runs the existing cascade over the
assembled compound under its own epoch. No new erosion machinery. The wear is
Neogrammarian-regular by construction, so it is testable by the same shape as
the shipped `lexicon-regular-*` metrics, and every worn name carries a
printable derivation like every other root.

**Name shape** is a per-culture weighted distribution drawn per settlement. The
idiom already exists, though **not** under the name the idea registry gives it:
grep finds no `WeightedCategorical::reduce`. What exists is
`Stream::weighted_index(&[f64])` (`kernel/src/seed.rs:254`) under
`hornvale_language::schemas::select_schema`
(`domains/language/src/schemas.rs:408`), which sharpens each weight by
`weight.powf(beta)` before drawing. That β is the "how stereotyped is this
people's toponymy" dial; S3 reuses this pattern, not a type that does not
exist. Shapes:
simplex (York), specific + generic (Oxford), and the fuller forms as a tail.
Pure per-settlement variety reads as noise; pure per-culture uniformity loses
the real within-culture tail; the weighted mix is what actual toponymic systems
look like.

### S4 — Phonotactic texture

`domains/language/src/phonology.rs:507` draws `nuclei = range_u32(1, 2)` as a
**language-wide obligatory count**. At 2, every syllable in the language is a
diphthong — which no natural language does, and which is most of what reads as
obnoxious in `Qvooshtvoagootao` and `Mjaogkoangjaogeetao`.

Replace with a nucleus **template set**, as onsets and codas already are, plus
**position-conditioned reduction**: full nuclei under prominence, reduced
elsewhere. LANG-18 records that stress is fixed on the first vowel today, so
the conditioning environment exists.

**S4 supplies the rule S3 runs.** Unstressed-vowel reduction *is* erosion; S3
and S4 are the same phenomenon at two time-scales. This is the strongest
result of the brainstorm's ideonomy passes and it simplifies both stages —
they share one rule rather than duplicating two.

Onset cluster density (`mj-`, `zhv-`, `ngj-`, `fnetzh-`) is examined in the same
stage but is secondary; the nucleus fix is the load-bearing half.

### S5 — Render-time qualification (decision 0024's deferred remedy)

Almanac and REPL disambiguate co-occurring same-named settlements from their
own site facts — *Ice-Home (taiga)*, *Ice-Home of the kobolds*. 0024 specifies
this and notes it is a view: **no epoch bump, no save-format contract, no
census regen of its own.** It is the stage that makes shortening safe, and it
is the cheapest stage in the campaign.

## 6. Save format, determinism, epoch (G3 flag)

Every stage but S5 touches a save-format contract. One epoch bump covers all
of them; that indivisibility is the argument for the single-campaign shape.

- **S1** bumps `ROOT_EPOCH` `"v3"` → `"v4"` and re-founds `EPOCH_COHORTS[0]` to
  the 110-concept roster as of this campaign (ledger #9). Epoch suffix, never
  a rename. Re-pins `cohort_zero_stays_the_frozen_landing_roster` 76 → 91 → 110.
- **S2** registers the 19 new concepts into that re-founded cohort 0, not into
  a fresh cohort. `cli/tests/accession.rs` asserts the table and the concept
  registry agree in both directions (`every_registered_concept_has_an_accession_epoch`
  and `every_accessioned_concept_is_actually_registered`); both must stay green.
  The concept registry is itself a determinism contract: registration feeds the
  lexicon proto-root walk.
- **S3** introduces a name epoch (a `/v3` leg on the naming stream). Stream
  consumption order is a contract: the pin-isolation tests in
  `domains/astronomy/tests/genesis_properties.rs` and
  `domains/terrain/tests/tectonic_properties.rs` are the pattern the naming
  equivalent must hold to.
- **S4** reseeds every phonology draw, hence every word in every language —
  the widest blast radius in the campaign.

Determinism obligations unchanged and non-negotiable: same seed plus same pins
yields byte-identical worlds; `BTreeMap`/`BTreeSet`/`Vec` only; no wall-clock;
quantize at emit only; names stay pure per-`(seed, species, kind, salt, site)`
functions with no re-draw and no shared "used" set.

**Amended 2026-07-28 (owner call, ledger #10): world-level pin isolation for
*glossed settlement names* is deliberately given up.** As first drafted this
paragraph continued "…so pin isolation holds by construction," and S3's
frequency-keyed wear (ledger #3) makes that false: a morpheme wears in
proportion to its share of the culture's **own name corpus**, so a settlement's
name depends on which *other* settlements that species placed. Measured across
seven seeds, wear changes names at four of them, so this is a live property,
not a theoretical one.

What is given up, and what is not:

- **Determinism — INTACT.** Same seed plus same pins still yields byte-identical
  worlds; verified by two fresh builds.
- **`naming.rs`'s own purity — INTACT.** The corpus arrives as an explicit
  read-only parameter computed by the composition root. There is no shared
  mutable set and no re-draw, which are the failure modes the original clause
  was written against: they cause order-dependence, and a read-only parameter
  causes none.
- **World-level pin isolation — GIVEN UP.** A pin that moves a species'
  settlement scatter can now move any glossed name of that species. The
  measured magnitude is small and seed-dependent, not wholesale: swapping the
  corpus for an empty one with the scatter held fixed changes 4 of 79 names at
  seed 777, 7 of 207 at seed 99, and 0 of 169 at seed 42. Deity and
  epithet names are unaffected (their name spaces are one-per-belief, and the
  corpus is a settlement-only input).

The alternative was re-keying wear to the compound's head slot, which preserves
the property exactly but *authors* the generic/specific asymmetry instead of
deriving it — and stops a ubiquitous *specific* from wearing, which real
toponymy does. Nathan took the trade on 2026-07-28.

Consequence for the evidence battery: the naming equivalent of the
`genesis_properties.rs` / `tectonic_properties.rs` pin-isolation tests, which
§5 S3 names as the pattern to hold to, **cannot** assert scatter-invariance for
glossed settlement names. It should instead assert the two properties that do
survive — determinism under repeated builds, and `naming.rs`'s argument purity.

**Census regen is a carve-out requiring Nathan's explicit authorization** and
is not assumed by this spec. Under decision 0063 the regen is local and takes
roughly 7 minutes (`HV_CENSUS=1 bash scripts/regenerate-artifacts.sh`), run
once at the pre-merge close.

## 7. Evidence battery

Preregistered, in the sense of ADR 0016 — stated here before implementation.

- **Primary.** `name-length-goblin` / `name-length-kobold` median falls into
  the metric's declared bucket range (≤ 10 characters). This is the campaign's
  falsifiable claim and the one it should be judged on.
- **Syllables.** A new metric: mean syllable count per generated name. The
  character-length metric cannot distinguish "shorter words" from "same words
  spelled tighter," and §2.2 shows spelling is not the defect. Target 2–3.
- **Wear regularity.** Every worn name replays byte-identically through
  `evolve` — the `lexicon-regular-*` metric shape, extended to names.
- **Gloss truth.** `name-gloss-true` continues to hold. Checked: it asserts the
  committed gloss is a truthful composition of the settlement's own re-derived
  site concepts; it does **not** assert the surface is readable. Erosion leaves
  it intact — the gloss survives as *etymology* rather than as a *reading*,
  which is what real onomastics reconstructs. Its doc string hardcodes "biome +
  presiding phenomenon" and needs updating for S2's wider vector: a doc and
  scope edit, not an invariant change.
- **Phonotactic validity.** `phonotactic-validity-*` continues to hold — every
  generated name still re-validates against its own phonology.
- **Collision rate.** Re-pinned, not bounded (§4). Expected direction stated in
  advance: **down**, because S2 widens the descriptor space far more than S3
  shortens the unique element. If it rises, that is reported as a measurement,
  not chased.
- **Transparency distribution.** A new metric: the share of committed names
  whose surface still contains its site-concept words verbatim. The target is
  explicitly *not* 100% (§8).

## 8. Success criteria

1. Median mean-name-length inside the metric's declared buckets (≤ 10 chars).
2. Mean syllable count per name in the 2–3 range.
3. Seed 42's gallery contains at least one name a reader would recognize as
   transparent (an *Underhill* or *Seaside*) and at least one fully opaque
   worn form — the distribution, demonstrated rather than asserted.
4. Every determinism and phonotactic invariant in §7 still green.
5. The four census metrics that pin naming re-pinned with stated causes.

Criterion 3 is taste-shaped and is Nathan's call at the merge stop, not
self-scorable. It is written down so that it is judged rather than assumed.

## 9. Explicitly deferred

- **Founder- and function-class specifics** (Birmingham's *Beorma*,
  Bridgeton, Kirkby). They want placed characters and a settlement-function
  layer; LANG-9 already predicts they arrive free as those substrates land.
- **Diachronic renaming** — LANG-11's fourth phase (conquest, commemoration,
  folk-etymological reanalysis). This campaign moves the system from baptism to
  opacification and stops there deliberately.
- **Deep-time differentiation of collided names** (MAP-3) — the chronic
  collision cure, distinct from 0024's acute reference-time posture.
- **Language-family topology** (LANG-53) — whether goblin/hobgoblin/bugbear
  should be dialects rather than languages. Independent of this campaign and
  deliberately untouched.
- **Onset-cluster redesign beyond the nucleus fix** — examined in S4, shipped
  only if the nucleus fix proves insufficient on its own.
