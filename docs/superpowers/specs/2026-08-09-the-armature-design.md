# The Armature — the declared causal frame, and whether it holds

**Status:** spec, awaiting G3 · **Date:** 2026-08-09 · **Branch:** `the-armature`
· **Base:** `de534b70`

Follows The Domesday (Part IV). Advances the Book programme's Part II by
building its claims in executable form first.

---

## 1. What this is

The Domesday reports **what does not vary**. It cannot report **what fails to
cause what**, because only one causal claim has ever been declared:
`studies/expectations.json` holds a single row.

This campaign writes the rest of the frame — roughly thirty declared links
spanning astronomy → climate → hydrology → biology → settlement → demography →
society — and adds the one thing D5 currently cannot see: **direction**.

The payoff is that a severed link becomes a *finding* rather than an absence.
"Nothing about a world reaches its creatures" is currently an inference from
fourteen frozen Biology metrics. With a declared frame it becomes six named
predictions that failed, each naming the pair that should have been coupled.

## 2. The gap that makes this more than data entry

`detect_d5` computes Pearson `r` and compares `band_of(r.abs())` against the
declared class. **It never looks at the sign.**

So a metric coupled in the *wrong direction* at the right strength passes
silently. A world where longer years make it *warmer* — a wire on the wrong
terminal — is exactly the sprint-debt shape this programme hunts, and the
detector is blind to it. That is the same could-not-fire family The Domesday
found four times inside its own detector.

**`Expectation` gains `direction`**, and D5 reports a direction mismatch as a
distinct, more serious finding than a strength mismatch.

## 3. Blinding — the campaign's methodological spine

**Nathan's ruling (2026-08-09): freeze all expectations from physics, then
measure once.**

No correlation may be computed while authoring — not by the controller, not by
an implementer. The table in §5 was written from physical reasoning against
metric *names and doc strings only*; no value from `rows.csv` was read.

Practically this means:

- §5's table is **frozen by this spec's commit**. It is the preregistration.
- The task that writes `expectations.json` is forbidden from running D5,
  computing `r`, or reading any census value. Its brief says so.
- The measurement happens **once**, in a later task, after the freeze is
  committed.
- **Expectations that turn out wrong are findings, not errors to correct.** A
  claim that the physics says X and the census says Y is exactly what this
  instrument is for. Do not revise the frame to match the data.

## 4. Design

### 4.1 `direction` on the expectation

```json
{
  "metric": "mean-land-temperature-c",
  "tracks": "year-std-days",
  "why": "a longer year means a wider orbit and less insolation received",
  "declared": "dominant",
  "direction": "negative"
}
```

- `direction` is `positive` | `negative` | `none`.
- `declared: "none"` **requires** `direction: "none"` — a claim of no
  relationship has no sign. Validated at load, like `DECLARED_CLASSES`.
- Any other pairing of `declared: none` with a signed direction is rejected.

### 4.2 What D5 reports

Three outcomes, ranked by seriousness:

| observed vs declared | finding |
|---|---|
| band matches, sign matches | silent |
| band matches, **sign opposite** | **`D5 direction`** — "declared *negative*, observed **positive** at \|r\| = …". The link exists and runs backwards. |
| band differs | `D5 strength` — "declared *dominant*, observed *weak*" (today's behaviour) |

A direction mismatch is rendered distinctly in the survey and ranked above a
strength mismatch, because a backwards coupling is a defect while a weak one
may only be an over-claim.

**Sign is only meaningful when something was measured.** When the observed band
is `none`, report the strength mismatch alone — the sign of a near-zero `r` is
noise.

### 4.3 Expect-none claims

Two rows in §5 declare `none`. These test for **spurious** coupling — a link
the physics forbids but the implementation might create. They are cheap and
they fire in the opposite direction from every other row, which is a useful
property in a frame whose author is also its subject.

## 5. THE FROZEN FRAME — 30 expectations

Written from physics against names and docs only. **No census value was read.**
Each row: the claim, the reason, the declared strength, the declared sign.

### Astronomy → Climate

| # | metric | tracks | declared | dir | why |
|---|---|---|---|---|---|
| 1 | `mean-land-temperature-c` | `year-std-days` | dominant | − | longer year ⇒ wider orbit ⇒ less insolation; the primary term in a radiative balance |
| 2 | `habitable-fraction` | `obliquity-degrees` | moderate | − | higher tilt ⇒ more extreme seasons ⇒ fewer cells tolerable year-round |
| 3 | `mean-land-temperature-c` | `brightening-per-gyr` | weak | + | faster main-sequence brightening tracks stellar mass, and so luminosity |
| 4 | `mean-land-temperature-c` | `day-length-hours` | **none** | none | day length sets the diurnal *range*, not the annual mean |

### Astronomy → Hydrology

| 5 | `shoreline-development` | `total-tide` | weak | + | stronger tides work the coast harder, cutting a more involved shoreline |

### Terrain → Climate

| 6 | `mean-land-temperature-c` | `ocean-fraction` | moderate | + | ocean lowers albedo and adds thermal inertia |
| 7 | `mean-land-temperature-c` | `mountain-coverage` | moderate | − | the lapse rate: more land held high is colder land on average |
| 8 | `habitable-fraction` | `mountain-coverage` | moderate | − | high ground is poor habitat |
| 9 | `fertile-land-fraction` | `mean-land-temperature-c` | moderate | + | warmth drives weathering and soil formation |

### Terrain → Hydrology

| 10 | `shelf-fraction` | `ocean-fraction` | moderate | + | more sea means more of it near sea level |
| 11 | `waterfall-count` | `mountain-coverage` | moderate | + | knickpoints need gradient |
| 12 | `shoreline-development` | `continent-count` | moderate | + | many separate landmasses carry more coast per unit land |
| 13 | `shoreline-development` | `largest-continent-share` | moderate | − | one dominant continent carries less coast per unit land |
| 14 | `endorheic-coverage` | `mean-land-temperature-c` | weak | − | colder, drier worlds leave more basins without an outlet to the sea |
| 15 | `karst-fraction` | `mean-land-temperature-c` | weak | + | carbonate dissolution runs faster warm and wet |

### Climate → Biology *(the set The Domesday predicts will fail)*

| 16 | `basal-metabolic-rate-w-goblin` | `mean-land-temperature-c` | moderate | − | a colder world costs more to thermoregulate in |
| 17 | `basal-metabolic-rate-w-kobold` | `mean-land-temperature-c` | moderate | − | as above, for the second species |
| 18 | `pace-of-life-goblin` | `mean-land-temperature-c` | moderate | − | warmth accelerates metabolism, and pace runs 0 = fast |
| 19 | `reproductive-tempo-goblin` | `habitable-fraction` | moderate | + | a forgiving world permits the slow, sparse end of the r–K axis |
| 20 | `reproductive-tempo-kobold` | `habitable-fraction` | moderate | + | as above, for the second species |
| 21 | `lifespan-years-goblin` | `mean-land-temperature-c` | weak | − | higher metabolic rate shortens life |

### Climate/Terrain → Settlement

| 22 | `settlement-count` | `habitable-fraction` | strong | + | settlements need habitable ground |
| 23 | `settlement-count` | `mountain-coverage` | moderate | − | mountains subtract settleable land |
| 24 | `settlement-count` | `ocean-fraction` | moderate | − | less land, fewer settlements |
| 25 | `pop-weighted-abs-latitude` | `mean-land-temperature-c` | moderate | + | a warmer world lets people live further from the equator |

### → Demography

| 26 | `total-population` | `fertile-land-fraction` | strong | + | food supply bounds population |
| 27 | `total-population` | `habitable-fraction` | strong | + | habitable area bounds population |
| 28 | `climate-displacement-events` | `habitable-fraction` | moderate | − | a harsher world drives more climate migration |
| 29 | `total-population` | `plate-size-gini` | **none** | none | how unevenly plates are sized should not reach demography |

### Demography → Society

| 30 | `tribute-relations-standing` | `settlement-count` | moderate | + | more polities, more room for standing subordination |

**Two of these are deliberate expect-none rows (#4, #29).** Twenty-eight are
signed claims. Six (#16–#21) target the coupling The Domesday found missing;
if the frame is right and Biology is genuinely world-invariant, all six fire.

## 6. Non-goals

- **Not fixing anything the frame reveals.** A severed link is a finding; the
  repair is a later campaign with its own preregistration.
- **Not revising the frame to fit the data.** See §3.
- **Not new metrics.** Every name in §5 is an existing census column.
- **Not Part II's prose.** These claims are Part II's content in executable
  form; the essays come later.

## 7. Preregistered success criteria

- **S1.** Every metric and driver named in §5 exists in the committed census; a
  name that does not fails a test.
- **S2.** `declared: "none"` with a signed `direction` is rejected at load,
  naming the offending row.
- **S3.** D5 reports a direction mismatch **distinctly** from a strength
  mismatch, and a synthetic pair coupled backwards at the declared strength
  fires the direction finding — demonstrated on a constructed case.
- **S4.** Sign is not reported when the observed band is `none` (a near-zero
  `r`'s sign is noise) — demonstrated.
- **S5.** The frame is committed **before** any correlation is computed. The
  commit that adds `expectations.json` contains no measurement, and the commit
  that first reports results is later in history.
- **S6.** Regeneration remains deterministic and drift-checked; the survey's
  rendered output carries the new findings.

**Falsification clause.** If the frame fires on nearly every row, the likely
cause is that the author's physics is wrong rather than the world's — report
that as the headline and publish the frame with its failures rather than
quietly pruning rows. **A frame that fires everywhere and a frame that fires
nowhere are both findings about the author.**

## 8. Risks

| risk | mitigation |
|---|---|
| Authoring contaminated by peeking | §3; the authoring task's brief forbids computing `r`; S5 checks commit order |
| The frame is mostly wrong physics | §7's falsification clause — publish it, do not prune |
| Direction mismatch drowned among strength mismatches | §4.2 ranks and renders them distinctly |
| A near-zero `r`'s sign reported as meaningful | §4.2's band-is-none rule, S4 |
| Rows quietly deleted when inconvenient | Deleting an expectation is a silently un-asked question; the plan forbids it without a recorded reason |

## 9. Open — for G3

- **The strength classes in §5 are the author's judgement.** They are the least
  defensible part of the frame: "moderate" for lapse-rate coupling is a guess
  at how strongly *this* simulation should express it, not a physical constant.
  An alternative is to declare only direction and let strength be reported
  without a verdict. I recommend keeping strength — an unfalsifiable claim is
  worth less than a wrong one — but it is Nathan's call.
- **Six of thirty target Biology on purpose.** That is deliberate loading
  toward a known-suspect subsystem. It makes the frame less a neutral survey
  and more a test of one hypothesis. Worth naming rather than hiding.
