# BIO-2 Life-History Allometry — retrospective

**Completed (implementation):** 2026-07-14 — code and book work done; **not
yet merged** (spec `docs/superpowers/specs/2026-07-14-bio2-life-history-
allometry-design.md`; the campaign's G6 merge gate is a separate,
Nathan-gated step this retrospective precedes). This is the **first
campaign run under the `campaign-autopilot` skill**, deliberately chosen
because its decisions are science-grounded and checkable rather than taste;
the autopilot validation section below is this retrospective's primary
purpose.

**A live collision changed the campaign's shape before a line of code was
written, and the ledger caught it cleanly.** Brainstorm opened against the
premise that no mass field existed on `SpeciesDef`; a check against the
parallel coexistence-stack branch found the opposite — mass, its kernel
newtype, and Kleiber home-range were already built there, 23 commits deep.
Nathan re-scoped the campaign on the spot to the layer coexistence had *not*
built (the life-history axis and the metabolic clade tag), rather than
either duplicating the mass authorship or stalling brainstorm on another
branch's merge state. The lesson generalizes: a brainstorm's stated premise
is worth one grep against active parallel work before it is treated as
settled scope, the same discipline `the-uncommon-ground`'s retrospective
named for open design questions.

**A duration-typing question surfaced at spec time was resolved cleanly at
plan time, with the right owner.** The spec flagged, as an open G3 item,
whether lifespan/maturity durations warranted a new kernel newtype or a
bare-`f64` type-audit waiver — species cannot depend on astronomy's
`StdDays`, so the two candidates were a real fork. The plan resolved it as
a kernel `Years` newtype, consistent with mass's own kernel-side promotion
and the typed-quantity discipline, and named it explicitly as the item
leading the eventual merge-gate digest (save-format-adjacent kernel
surface). Naming the leading item at spec time, before the plan existed,
kept the merge-gate preparation from being reconstructed after the fact.

## Autopilot validation

This is the skill's first live run, so the honest record of what it did and
did not decide matters more than the campaign's product.

- **Auto-resolved by the skill (the veto-check surface at G3):**
  - **Clade-dependent allometry** — whether a single normalizing constant
    would suffice, or a per-class `MetabolicClass` enum was needed. The
    skill answered yes (enum, universal exponents) against the
    minimal-axis-discipline precedent and the roster's own kobold/goblinoid
    split, without escalating to Nathan.
  - **Neutrality** — that the whole layer ships as pure derived functions
    over authored constants, rewiring no existing consumer and changing no
    committed byte, against The Ground's precedent.
  - **Sequencing** — that brainstorm through spec could proceed immediately
    against the mass foundation as a documented dependency, with code
    execution deferred until the mass foundation actually merges (flagged
    at G3 rather than silently assumed).
  - **Output surfaces** — read-only almanac and lab-metric surfaces only, no
    phenomena, no ledger facts, no consumer rewiring, decided from the
    ideonomy liveness-spectrum pass rather than asked.
  - **Post-G3 correction (#16)** — Task 6 shipped the six life-history lab
    metrics for the goblin only; review flagged that the spec's own
    cross-species contrast (an ectotherm reading long-lived-for-its-size)
    was un-queryable with one species, and the fix — extending to
    goblin+kobold, 12 metrics, matching the project's existing
    `tone-count-{species}` naming convention — was applied under
    spec-governs authority, not escalated.
  - Also auto-resolved without flagged risk: the calibration-anchor value,
    the reproductive-tempo framing (rate not litter size), the pace-of-life
    absolute-not-roster-relative correction, and the `Ametabolic`-nulls-all-
    biological-traits correction.
- **Made by Nathan directly (not auto):** campaign selection itself; the
  scope narrowing after the mass-foundation collision; the fidelity carve-out
  on ectotherm metabolic-rate temperature coupling (ship basal-only, defer
  the climate coupling); and the G3 review verdict itself.
- **G3 vetoes: zero.** Nathan's G3 review approved the spec package, every
  ideonomy-sweep correction, and the conservative lore-outlier call
  (allometry as baseline only, no override built) without reversing any
  auto-resolved item above. Read against the veto-check list, every
  candidate item held.
- **The one real process miss: ideonomy was under-applied early, then
  corrected.** The first two passes were narrowly targeted — one on
  campaign selection, one on the output-surface/liveness-spectrum question
  and the metabolic-fidelity gap — rather than a pass over the whole
  design. A third, explicitly whole-design sweep (negation, cross-domain
  reinstantiation, combination; notation and cycle organons) then surfaced
  six material corrections in one pass — the roster-relative pace-of-life
  bug, the `Ametabolic`-nulls-everything correction, the single per-class
  pace multiplier preserving fast–slow covariation, the heterotroph-only
  exponent scope, the lore-outlier baseline framing, and the
  forward-compatibility note — that a narrower pass had missed. A fourth,
  confirmatory pass found nothing further and declared convergence. The
  reusable lesson: two targeted ideonomy passes are not a substitute for
  one comprehensive pass over the finished design, and the comprehensive
  pass should run before spec write-up, not be added afterward once gaps
  are visible.
- **A second real process miss, caught by the task-review loop rather than
  ideonomy:** the plan under-specified the spec's own "per-species" and
  "cross-species contrast" language into a single-species (goblin-only)
  metric set for Task 6. This was not an ideonomy gap — it was a plan
  written against the letter of the spec's metric list without checking it
  against the spec's own headline claim. The review loop caught it before
  merge (#16, above); the fix belongs one level up from where it was
  caught, the same lesson `the-uncommon-ground`'s retrospective drew about
  plan-authored example code — a plan's derived detail should be checked
  against the spec's stated *claims*, not only its literal deliverable
  list.

## Follow-ups (captured, not built here)

- **Climate-coupled realized metabolic rate.** The shipped basal metabolic
  rate is explicitly a reference-temperature figure; an ectotherm's real
  burn rate should rise and fall with ambient temperature
  (`realized = basal × thermal_response(ambient, class)`). Deferred to
  preserve the species/climate domain boundary and this campaign's
  neutrality; the natural home is a `windows/worldgen` composition-root
  function where species meets climate, not a species-crate dependency on
  climate. Evaluate a frontier row (thermal ecology / Q₁₀ performance
  curves) when a consumer is spec'd.
- **Mortality/turnover as a demographic handle.** `1 / lifespan` is the
  clean consumer-facing turnover rate for population-renewal math (the
  coexistence carrying-capacity flow, or a future demography consumer). Not
  wired here — this campaign sets the life-cycle parameters, not its
  operation — named for whichever campaign reads it.
- **A `Mesotherm` metabolic class.** A fifth `MetabolicClass` for creatures
  that do not cleanly split (tuna-like regional endothermy; a metamorphic
  species whose larval stage is ectothermic and adult stage endothermic).
  Not needed by today's roster; the enum is already the extension point.
- **A per-species lore-outlier override.** Real allometry treats humans as
  an outlier against their own curve; Hornvale's future long-lived species
  (elves) will need the same treatment — a multiplier on the allometric
  baseline, not a replacement for it. Designed-for in the spec's framing
  (`life_history` is documented as the baseline, not the final word) but
  not built; belongs to the long-lived-species campaign.

### Seam-maintainability notes (from the final whole-branch review)

- **Autotroph metabolic exponent.** `basal_metabolic_rate_w` currently applies
  the endotherm 0.75 exponent to the `Autotroph` class, but that class's own
  doc says its energy capture is surface/area-limited (a different exponent).
  Harmless today (no autotroph on-roster); when the seam is activated, either
  guard it like `Ametabolic` or give it its own law. Pairs with the `Mesotherm`
  follow-up as "unfinished `MetabolicClass` seams."
- **`reproductive_tempo` vs `pace_of_life` are on different scales.** Both are
  documented 0…1 "slowness" scalars, but `pace_of_life` is divisor-normalized
  by `MAX_PACE_MULTIPLIER` (so 1.0 is reserved for the slowest class) while
  `reproductive_tempo` is not — so an endotherm's `pace_of_life` ceiling is
  ~0.667. Correct by design (the field doc could note the per-class ceiling),
  but a reader should not assume the two scalars are directly comparable.
