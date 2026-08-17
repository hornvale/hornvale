# The Burr — a tongue's character is typological, not parametric

**Campaign:** The Burr
**Branch:** `campaign/the-burr`
**Status:** spec, at G3
**Date:** 2026-08-16

A burr is a rolled /r/ — the exact segment seventeen of the eighteen shipped
tongues do not have — and also what we call a regional accent. Both readings
are the campaign.

---

## 1. What this campaign produces

A **typology bundle**: a named, authored, per-family datum that selects which
*rules* build a tongue's words, rather than which *values* a shared rule uses.
Four bundles ship, each exercised by at least one roster family.

Concretely, at merge:

1. `Typology` exists in `domains/language`, authored per family alongside
   `family_proto()`, and carries four independent internal fields —
   morphology, coda law, vowel harmony, orthography.
2. `Manner::Trill` is no longer gated behind the exotic tier, and a
   profile-conditioned sonorant floor guarantees a liquid where the bundle
   requires one.
3. The dwarf family builds words root-and-pattern (LANG-19); the elf family
   builds them open-syllable with vowel harmony; the draconic family builds
   them isolating and tonal, activating a tone tier that no shipped species
   has ever reached; goblinoid, plant, and the unfamilied kinds keep today's
   concatenative engine.
4. A **tongue classifier** metric lives in the Lab, with a baseline frozen
   before any generation code moves.

The point of the campaign is stated as a claim so it can be wrong:
**a tongue's perceived character is a discrete property of which rules build
it, and is not reachable by tuning the continuous articulation vector.**

## 2. Non-goals

- **Not a Tolkien pastiche.** "Khuzdul region" and "Quenya region" name
  *typological neighbourhoods* — root-and-pattern morphology, open-syllable
  phonotactics — never target word-forms. No output is compared to Tolkien's
  corpus, and no test asserts resemblance to it. If the dwarf tongue ends up
  sounding nothing like Khuzdul while being genuinely templatic, the campaign
  succeeded.
- **Not a fifth-plus bundle.** Fusional, polysynthetic, initial mutation,
  reduplication, and prosodic timing are captured as registry rows
  (`LANG-typology-fusional` and siblings) and deliberately excluded. The
  binding constraint is §4.5.
- **Not a rework of channel capacity.** LANG-27…30 stay where they are. This
  campaign asserts character is a *sibling* of capacity, and touching capacity
  is how you fail to test that.
- **Not the culture-derived variant.** `LANG-typology-from-culture` records
  the road not taken; it presupposes the species-psychology substrate.
- **Not a new render surface.** Names get a new *shape*, not a new consumer.

## 3. The findings, verified

Each of these was measured, not reasoned. The command is given because the
reasoning was in several cases confidently wrong before the command was run.

### 3.1 Seventeen of eighteen tongues have zero liquid-bearing words

```
awk -F'|' '/^## /{s=$0; sub(/^## /,"",s)} NF>6 && $4 ~ /[A-Z]/ {
  w=$4; gsub(/ /,"",w); if (w ~ /[lrLR]/) c[s]++; t[s]++ }
  END{for (k in t) printf "%-16s %3d/%d\n", k, c[k]+0, t[k]}' \
  book/src/reference/dictionary-generated.md
```

Every tongue returns exactly `1/N` except Kobold at `75/86`. The `1` is the
table's own `| Word |` header — so the true figure is **0 of ~85 for seventeen
tongues**, and Kobold is the sole exception. Kobold is also the only species
that drew `ExoticManner::Trill`.

**A caution this spec is required to state:** the first version of this
finding read `1/85` and was nearly written into the spec as "one liquid word
per tongue", which would have been a plausible, specific, and entirely
fictitious number. The header row is the whole difference. Any downstream
count taken from a Markdown artifact must exclude the header explicitly.

**And the same error was then made one level up, by the author of this
paragraph.** Every version of this finding through spec approval read "18 of
19", because the `awk` above emits one row per `## ` heading and the
dictionary has nineteen of them — the nineteenth being `## Cognates`, which
is a cross-family comparison table, not a tongue. The roster is **eighteen**.
Caught during Task 2's pre-dispatch verification, after the wrong figure had
already reached the spec, the plan, an idea-registry row, a board post, and
three commit messages.

Nothing about the finding changes: seventeen tongues with no liquid at all,
against one with 75 of 86. But the shape is worth recording, because it is
not the same mistake twice — it is the same mistake at two different scales,
and the second one was made while writing the warning about the first. The
generalisation both instances share: **a section count is not an entity
count, and a row count is not a datum count.** Whatever the loop iterates
over needs one explicit exclusion test, written down, per level of nesting.
`the_baseline_roster_is_eighteen_tongues` (Task 3) exists to hold this one.

### 3.2 The sonority penalty makes sonorants unreachable for quiet species

`domains/language/src/phonology.rs:450`:

```rust
let mut p = BASE_KEEP - LOUDNESS_PENALTY * son * (1.0 - env.voice_loudness);
```

with `BASE_KEEP = 0.7`, `LOUDNESS_PENALTY = 0.22`, and `sonority(Approximant)
= 4`. This is monotonically decreasing in sonority for every species with
`voice_loudness < 1.0`. There is no envelope under which a species *prefers*
sonorants. For the elf proto (`voice_loudness: 0.35`) an approximant survives
at `p = 0.128`; for the dwarf proto (`0.60`) at `p = 0.348`.

The consequence is directional and is the campaign's motivating defect: the
region "quiet **and** sonorant-rich" — which is most of what makes a
Quenya-like tongue sound the way it does — is not merely unlikely, it is
**unreachable by construction**.

### 3.3 /r/ is gated behind the exotic tier

`domains/language/src/phonology.rs:407`:

```rust
fn exotic_manner(manner: Manner) -> Option<ExoticSeg> {
    match manner {
        Manner::Trill => Some(ExoticSeg::Trill),
        Manner::Click => Some(ExoticSeg::Click),
        Manner::Ejective => Some(ExoticSeg::Ejective),
        _ => None,
    }
}
```

`permits()` therefore refuses every trill unless the species independently
authored `exotic: Trill`. Clicks and ejectives are genuinely marked segments;
an alveolar trill is present in a large majority of the world's languages.
Grouping the three is the proximate cause of §3.1.

### 3.4 The tone tier is built, tested, and unreached

```
grep -n "tonality:" domains/language/src/lib.rs | sort | uniq -c
```

returns `tonality: 0.0` on **all 23 authored rows** — every kind in
`articulation_registry` and every family in `family_proto`. Therefore
`draw_tone_inventory` always returns `{Neutral}`, `ensure_capacity_floor`
early-returns for every species, `RuleKind::Tonogenesis` can never be
effective, and `MAX_TONE_COUNT` / `CONTRASTIVE_TONES` are dead constants in
practice. Five non-zero `tonality` sites exist, all in tests with synthetic
envelopes — which is why nothing reddens.

This makes the isolating-tonal bundle the cheapest of the four to build: the
machinery exists and is covered; it needs a family to author a non-zero row.

### 3.5 Sonority sequencing is enforced; segment markedness is not

`order_by_sonority` imposes the Sonority Sequencing Principle on drawn
templates, so cluster *order* is well-formed. But `draw_manner_slots` picks
each manner uniformly from those present. Real languages overwhelmingly prefer
obstruent+liquid onsets; ours produce `stop+fricative` (`px-`, `dx-`, `tv-`)
and `sibilant+nasal` (`zŋ-`, `ʃŋ-`) because no liquid is in the inventory to
be picked. The SSP is satisfied and the result is still typologically strange.

### 3.6 Romanization reaches the ledger

`naming.rs:1353` does `roman.push_str(romanize(seg))`, and that string is what
is committed as a `name` fact. `Segment` and `Phonology` carry no `Serialize`
derive (verified: zero matches in both files), so the *feature bundle* is not
stored — but the *rendered string* is.

**The precise consequence, which an earlier draft of this spec got wrong:** an
orthography change moves **no** stream draw and **no** seed derivation, but it
rewrites every committed name string, and therefore every generated artifact
and any census metric reading name text. It is an **artifact-drift event, not
a stream-consumption epoch.** Those are different things with different
handling and the distinction is load-bearing in §5.

### 3.7 The proto's phonotactic templates cap every daughter's lexicon

Measured after Stage 2 landed, and the campaign's deepest defect — deeper than
either §3.2 or §3.3.

```
cargo run -q -p hornvale -- proto elf | sed -n '/## Inventory/,/## Phonotactics/p' | grep -i trill
cargo run -q -p hornvale -- proto elf | grep -E 'Onsets|Codas'
cargo run -q -p hornvale -- proto elf | awk -F'|' 'NF>4 && $4 ~ /\*/ {print $4}' | grep -c '[rR]'
```

Proto-elf's inventory **does** contain `r` after the sonorant floor. Its onset
templates are `sibilant, stop, nasal` and its codas `stop`. So **no template
slot any trill can fill**, and the third command returns **0** — not one
proto-elf root carries a liquid.

The consequence is the finding: **a segment in the inventory that no drawn
template can host is unreachable, and for inherited vocabulary the gating
templates are the *proto's*, not the daughter's.** Snow-elf's own inventory
carries `r` *and* its onset template is literally `fricative+trill`, and it
still shows zero liquid-bearing words — because its words are evolved from
proto roots that never had one. A people can be entirely able to pronounce a
sound and possess no word containing it.

Two corrections this forced, both recorded rather than absorbed:

1. **Stage 2 delivers no audible liquid, and never could have.** Liquid
   coverage after Task 5 is 3 of 18, byte-identical to Task 4, with no elf or
   dwarf among them. Advice given mid-campaign to "land Stage 2 and stop" was
   withdrawn on this measurement.
2. **`ensure_minimum_sonorants`' own doc comment said so** — "says nothing
   about whether the phonotactic templates will ever *use* the sonorant it
   adds" — and Task 5's dispatch prose asserted the opposite one screen later.
   The limitation was known, written down, and then contradicted by its author.

## 4. Design

### 4.1 A bundle is a named authored row, not a point in a product space

```rust
/// A named typology bundle: which rules build this family's words.
pub struct Typology {
    /// The bundle's name, for artifacts and diagnostics.
    pub name: &'static str,
    /// How words are built from roots.
    pub morphology: Morphology,
    /// What a syllable may end in.
    pub coda_law: CodaLaw,
    /// Whether vowels within a word must agree, and on what.
    pub harmony: Harmony,
    /// How this family's segments are spelled in the romanization.
    pub orthography: Orthography,
}
```

The four fields are genuinely orthogonal — morphology is word-formation,
coda law is phonotactic, harmony is a word-level constraint, orthography is a
view. Keeping them as separate fields keeps each legible and independently
testable.

**But the cross-product is never exposed.** Five morphologies × three coda
laws × three harmonies × three orthographies is 135 combinations; we ship
four. Authoring *rows* rather than admitting *combinations* means every
shipped configuration is one a human designed and a test exercises. This is
the same discipline `family_proto()` already uses, and the same discipline
decision 0011 applies to studies.

This resolves the category error flagged at design review: `Typology` is not
a bare enum pretending four unlike properties are one property, and it is not
four free knobs either.

### 4.2 The four bundles

| Bundle | Family | Morphology | Onset law | Coda law | Harmony |
|---|---|---|---|---|---|
| `templatic` | dwarf | consonantal skeleton × vocalic template | drawn | obstruent, obligatory | none |
| `sonorant-open` | elf | agglutinative, affixing | **second slot is a sonorant** | closed sonorant set, optional | front/back |
| `isolating-tonal` | draconic | isolating; tone carries contrast | single slot | open or single nasal | none |
| `concatenative` | goblinoid, plant, unfamilied | today's engine | today's draw | today's draw | none |

**The onset column is an amendment, added after Stage 2 measured why it is
needed (2026-08-17).** It was in the design as presented and approved, and was
lost in transcription to this table — a drafting omission, not a scope
decision. Stage 2 then made it load-bearing rather than merely nice:
`ensure_minimum_sonorants` put `/r/` into proto-elf's *inventory* and liquid
coverage did not move at all, because proto-elf's drawn onset templates are
`sibilant, stop, nasal` and its codas `stop` — **no slot any trill can fill**,
so zero of its roots carry one, and every daughter inherits that. A coda law
alone would fix the count (words ending in `-r`); it would not produce the
`Cr-`/`Cl-` onsets that are most of what the region sounds like. See §3.7.

`concatenative` exists to name the status quo, not to change it. Its handling
must be byte-identical to today wherever the other three changes do not force
movement — this is the campaign's own control, and §6 preregisters it.

### 4.3 The sonorant repair

Two changes, both independent of the bundles and therefore separately
testable:

1. **Ungate the trill.** `exotic_manner(Manner::Trill)` returns `None`. Clicks
   and ejectives stay gated. A trill then faces only the ordinary
   keep-probability draw.
2. **A profile-conditioned sonorant floor.** `ensure_minimum_sonorants`, in
   the same shape as the existing `ensure_minimum_consonants`, guarantees a
   bundle that declares a sonorant requirement gets at least one liquid,
   deterministically and draw-free — so no unlucky roll produces a
   liquid-free `sonorant-open` tongue.

The sign of the sonority term in `keep_probability` becomes bundle-conditioned
rather than universal. `concatenative` keeps today's sign, which is what makes
§6's control meaningful.

### 4.4 Root-and-pattern, concretely

For `Morphology::Templatic`, `assign_proto_roots` assigns each concept a
**three-consonant skeleton** injectively over the concept universe, and a form
is realized by threading a vocalic template through it. Two concepts never
share a skeleton; a paradigm slot (LANG-43 already ships Number and Tense
slots) selects the template.

This is the one place the campaign adds genuinely new generative machinery
rather than re-conditioning existing machinery, and it is why it is Stage 4
rather than Stage 2. It is also the piece most likely to be cut if the stage
budget runs out — see §9.

### 4.5 Why four and not more

**Never ship a bundle no family uses.** An unexercised bundle is an unmeasured
code path that reads to the next campaign as supported. This repo has been
bitten by that shape repeatedly — five vacuous guards in one campaign, an
allow-list gate that got measurably *faster* as it went blind — and a typology
enum is an unusually attractive place for it, because adding a variant is one
line and looks free.

Five families and four bundles means every bundle is exercised, with
`concatenative` covering the remainder. Deferred bundles live as registry rows
until a family wants one. Nathan's standing note is that the roster will grow,
and each new species is a home for a deferred bundle — that is the promotion
path, not a taxonomy built ahead of demand.

## 5. Save-format and determinism

**This campaign is an epoch event.** Every name in every world changes.

| Change | Kind | Handling |
|---|---|---|
| Trill ungating | inventory draw | `ROOT_EPOCH` `v3` → `v4` |
| Sonorant floor | inventory, draw-free | covered by the same epoch |
| Per-bundle phonotactic law | phonotactics draw | covered; new sub-stream label per bundle-conditioned draw |
| Templatic skeletons | root assignment | covered by `ROOT_EPOCH` bump |
| Orthography | **view only** | no epoch; artifact drift only (§3.6) |

Rules this campaign obeys without exception:

- **Epoch suffix, never rename.** `ROOT_EPOCH` (`etymology.rs:481`) goes
  `"v3"` → `"v4"`. Existing labels are never edited.
- **New draws take new labels.** Any bundle-conditioned draw derives its own
  `StreamLabel` leg so introducing it cannot perturb a sibling stream — the
  pattern `TONES` / `INVENTORY` / `PHONOTACTICS` already establishes.
- **Stream consumption order is a contract.** A `concatenative` family must
  consume the same draws it consumes today. Pin-isolation tests in the shape
  of `domains/astronomy/tests/genesis_properties.rs` cover this.
- **Quantize at emit only.** Nothing here touches the compute path.

**Census.** A refresh is required at close, on lefford, by the standing
procedure. Cost must be read from `docs/timings.md` (`grep '| census |'
docs/timings.md | tail`), never from CLAUDE.md's prose and never from this
spec — the last two campaigns to take a number from prose were wrong by 2.2×
and 6.7× in opposite directions.

## 6. Preregistered predictions

Frozen before Stage 2, per decision 0016. Stage 1 produces the baseline; these
predictions are written against it and are not revised after unblinding.

**The instrument.** A character-n-gram model trained per tongue over its
generated wordlist; the reported statistic is held-out **assignment
accuracy** — given a word, how reliably can the tongue that produced it be
identified. Deterministic, seeded, Lab-resident.

- **P1 (baseline, descriptive).** Assignment accuracy today is low relative to
  an eighteen-way chance floor of 5.6%. The exact figure was Stage 1's output
  and was *not* predicted here — predicting a number this spec could instead
  simply measure would have been theatre. Stage 1 measured it: **0.7201897018970189**
  (72.02%), on 2026-08-16, over the committed dictionary at commit
  `0e7d5757`. Recorded here after the fact, as this bullet promised, and it
  carries a consequence nobody could have known before the measurement ran:
  the metric had less headroom than assumed — **P2's headroom is 0.28, not
  ~0.95.**

  This is not a contradiction of the campaign's premise — it is the empirical
  form of it. A machine separates these tongues at 72%; a reader cannot
  separate them at all. Those were always different claims, and only one was
  ever in doubt. It is the third failure mode `LANG-character-is-not-capacity`
  records — genuinely diverse output whose whole reachable region is narrow —
  and this number is that row's evidence.

  **P2 is deliberately left as written.** Re-scoping a preregistered
  prediction after seeing the baseline is exactly what decision 0016 exists to
  prevent, so the honest course is to let it stand and report at close that it
  was a weak instrument for a reason Stage 1 could only discover by running.
  What this number *does* change is the weight §6's closing caution carries:
  the tongues are separable well above chance (0.72 on an 18-way task, about
  13× the 0.056 floor) while a reader cannot separate them at all, and are
  still not lovely, so "necessary but not sufficient" is now measured rather
  than argued. Stages 2–4 are justified by §3.2, §3.3 and §3.4 — defects that
  stand on their own — never by P2.

- **P2 (the campaign's central claim).** Accuracy rises significantly under
  the four bundles, and the rise is concentrated in the three non-control
  families. If accuracy rises uniformly *including* `concatenative`, the
  instrument is measuring the epoch bump rather than the bundles, and P2 is
  **not** confirmed.
- **P3 (sonorant fraction).** `sonorant-open` tongues reach a non-zero
  sonorant-segment fraction on every seed in the sweep. This is the one
  prediction with a hard floor: **any** seed producing a liquid-free
  `sonorant-open` tongue falsifies the §4.3 floor outright.
- **P4 (the control).** `concatenative` tongues' relative pairwise distances
  are unchanged beyond what the epoch bump forces. This is what distinguishes
  "we built typology" from "we perturbed the seed".
- **P5 (tone).** The `isolating-tonal` family draws a tone inventory of size
  > 1, and `RuleKind::Tonogenesis` becomes effective for it — the first time
  either happens for a shipped species (§3.4).

**Two cautions written into the spec so they cannot be quietly dropped:**

1. **LANG-45's warning applies.** Low inter-tongue variance can honestly
   reflect near-clone *inputs* rather than an inert engine. The baseline must
   be reported *against* the articulation-vector spread, never alone.
2. **The classifier is necessary, not sufficient.** It measures
   distinguishability, and the campaign's actual goal is aesthetic. A tongue
   family could become highly distinguishable and no lovelier. P2 going green
   is therefore **not** a claim that the campaign succeeded at what was asked;
   it is a claim that the tongues stopped being interchangeable. The
   sufficient half remains taste-gated, is acknowledged as unsolved, and the
   honest report at close says which half was measured.

## 7. Stages

Five, ordered so the preregistration is complete and committed before any
generation code moves.

| # | Stage | Deliverable | Epoch? |
|---|---|---|---|
| 1 | **The instrument** | Tongue classifier registered as a Lab metric; baseline measured and committed; §6 predictions frozen against it | no |
| 2 | **The sonorant repair** | Trill ungated; `ensure_minimum_sonorants`; bundle-conditioned sonority sign | yes |
| 3 | **The bundles** | `Typology` with four authored rows; per-bundle phonotactic law; harmony field live | yes |
| 4 | **Root-and-pattern** | `Morphology::Templatic` skeletons and vocalic templates (LANG-19) | yes |
| 5 | **Surface and close** | Orthography conventions; full artifact regen; census on lefford; book and chronicle | artifact drift |

Stage 1 moves no bytes, which is the point: it is the freeze, and it is the
only stage whose output is worthless if it lands after the code it measures.
Stages 2 and 3 are separable on purpose — Stage 2's defect (§3.2, §3.3) is
real independent of whether bundles ever ship, so it can stand alone if the
campaign is cut short. Stage 4 is the designated cut (§9).

Each stage boundary submits `make sluice-stage BRANCH=campaign/the-burr
REF=<full-sha>` and absorbs main, per the standing campaign rule — with the
one exception that applies here: **never absorb mid-measurement.** Stage 1's
baseline and Stage 2's readout must see the same physics, so the absorption
at that boundary happens after the readout, not before.

## 8. Acceptance criteria

- `Typology` exists with four authored bundles; every bundle is reached by at
  least one family, asserted by a test that fails if a bundle goes orphaned.
- `exotic_manner(Manner::Trill) == None`; clicks and ejectives still gated.
- A test asserts no `sonorant-open` tongue is liquid-free, across the seed
  sweep.
- A test asserts `concatenative` families consume the same stream draws they
  consume today (order contract).
- The tongue classifier is registered as a Lab metric; baseline committed in
  Stage 1 and unmodified thereafter.
- `ROOT_EPOCH` is `"v4"`; no existing stream label is renamed.
- Full artifact regeneration is green and committed: `make rebaseline`, the
  drift check over `docs/generated-paths.txt`, and the census refresh on
  lefford.
- Book: a chronicle entry, a freshness sweep, and a re-score of any
  Confidence Gradient bet this moves (decision 0030).

## 9. Risks

- **Stage 4 is the largest and the least certain.** Root-and-pattern is the
  only genuinely new generative machinery here. If the stage budget runs out,
  it is the designated cut: Stages 1–3 plus 5 deliver liquids, per-bundle
  phonotactics, an activated tone tier, and orthography, which is most of the
  audible change. Cutting it means `templatic` degrades to a coda-law-and-
  orthography bundle, and the spec must say so at close rather than quietly
  shipping a bundle that does not do what its name claims.
- **The epoch is wide.** Every name in every world moves, which makes review
  hard: a genuine defect and an intended change look identical in a diff. The
  mitigation is P4's control family — `concatenative` is the only place where
  "unchanged" is meaningful, so it carries most of the review weight.
- **Census cost is unknown at spec time.** Recent refreshes span 949 s to
  19,207 s. Read the ledger, not this line.
- **Four bundles is a claim about the roster, not a law.** If the roster grows
  substantially during the campaign, revisit §4.5 rather than defending four.

## 10. Definition of done

Per project process: merged plan, chronicle entry, freshness sweep,
Confidence Gradient re-score if moved, a retrospective in
`docs/retrospectives/`, registry rows repointed from `raw` to `shipped` or
`spec'd` as appropriate, and the census committed from lefford.

## 11. Decisions promoted from the ledger

To be minted as decision records at close; **0141 is the next free number**
(verified: `docs/decisions/` tops out at 0140).

- **0141 — a typology bundle is authored, not derived.** Character is stated
  per family, as `family_proto()` states an articulation vector. Records the
  exception explicitly so `LANG-typology-from-culture` has something to
  supersede.
- **0142 — never ship a typology bundle no family uses.** The anti-vacuity
  rule of §4.5, generalized: an unexercised variant of a closed enum is an
  unmeasured code path that reads as supported.
- **0143 — a trill is not an exotic manner.** Records the §3.3 regrouping and
  why clicks and ejectives stay gated.
