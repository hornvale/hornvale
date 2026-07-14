# Study 009: The Census of the Meeting

The capstone census of Year 2. The four campaigns before it built two
peoples layer by layer — psychology, perception, articulation — and each
proved its own layer with a preregistered calibration hanging off the drift
census. This study invents almost nothing new. Its job is to gather those
scattered calibrations into **one preregistration ledger**, so a reader sees
the whole falsifiability structure of "two peoples diverge from one sky" in a
single place, and to add the one control the year still owed: a **null
control** that rules out the alternative explanation — that the divergence is
not authored structure at all, but two different draws off one seed wearing
the costume of culture. The comparative suite says the two peoples differ
decisively; the null control says a people identical to the baseline does
*not*. Only both together close the year's exit criterion.

**A note on scale and provenance.** This study spans two committed
populations, both CI-drift-checked in the "Artifacts are current"
step of `.github/workflows/ci.yml`. The comparative suite (the ledger below)
is asserted over **`the-census`** — the shipped `{goblin, kobold}`
roster, 1,000 seeds (the census-as-data consolidation's rename and
doubling of what this page still calls `census-lands-drift` below, since
the ledger's headline figures were first measured and pinned at that
study's original 500-seed size and are not re-measured here). The null
control is asserted over **`census-of-the-meeting`**
(`studies/census-of-the-meeting.study.json`, generated summary under
`book/src/laboratory/generated/census-of-the-meeting/`): two *solo* rosters,
`[goblin]` and `[goblin-twin]`, over one shared seed range, unaffected by
that consolidation. The headline figures quoted below are the 500-seed
pinned counts first guarded by `cargo test`; the comparative-suite rows
have since been re-pinned to the 1,000-seed sample (census-as-data), the
null-control rows have not. As with Studies 006–008, a 10,000-seed author-time run of
`census-of-the-meeting` (the same study file bumped to 10,000 seeds) was
executed once by hand to confirm the null control holds at scale; it is
**not** committed and **not** part of CI, and its confirmation is noted where
it bears. Where a suite row was already measured at 10k by an earlier study,
that study is cited in the ledger's last column.

**A note on preregistration.** Per ADR 0016, every hypothesis states its
**direction** before the census that tests it runs; exact rates and counts
are pinned only *after* measurement, never tuned to pass (the Y2-2
precedent: the honest 0.875 blind-attribution rate was pinned and the rule
left untouched, not lowered to a floor it would clear). The directions in
the ledger below are quoted from §9 of the metaplan and §4/§6 of the
campaign spec
(`docs/superpowers/specs/2026-07-08-campaign-y2-4-the-meeting-design.md`),
committed before this study's census ran. This study **formalizes**; it does
not discover.

## Question

Taken together as one falsifiability structure: do the two peoples diverge
**decisively and recountably** across every authored axis — where their gods
sit in the sky, how their ladders stratify, how their names and epithets are
shaped — and does a third people carrying the goblin's *exact* vectors fail
to diverge, scoring at chance on blind attribution and within the sampling
bound on every structural distribution, so that the measured divergence is
provably a reading of the vector and not an artifact of stream noise or
generation order?

## The preregistration ledger

Each row is a comparative-suite hypothesis, its preregistered direction, its
measured/pinned result over the 500-seed drift census, the calibration in
`windows/lab/tests/calibration.rs` that `cargo test` asserts it by, and the
earlier study that measured it at 10,000 seeds. The suite is, in the main,
**already built** (campaign spec §6): The Meeting's contribution is to
present it whole, and to add the null control (last two rows).

| # | Hypothesis | Preregistered direction | Measured / pinned result (500 seeds) | Calibration test | Measured at 10k by |
|---|---|---|---|---|---|
| 1 | Goblin head-deity domain | Always **solar** (rank + diurnal eye) | 100% solar, zero exceptions | `goblin_heads_are_always_solar_and_mooned_kobold_heads_always_lunar` | Study 007 |
| 2 | Mooned kobold head-deity domain | Always **lunar** where a moon exists | 100% lunar on mooned worlds | *(same test)* | Study 007 |
| 3 | Moonless kobold head-deity domain | Not preregistered — measured split | **62 solar / 10 lunar** (bright night-star sometimes wins) | *(same test)* | Study 007 |
| 4 | Head-deity periodicity × lock | **Eternal ⟺ tidally locked** | Exact coincidence, all rows | `head_deity_is_eternal_exactly_when_tidally_locked` | Studies 004, 005 |
| 5 | Social verticality × stratification | **Ranked ⟺ stratified** (structure size ≥ 4) | Exact coincidence, all rows | `pantheon_verticality_matches_stratification` | Study 006 |
| 6 | The slave rung | slave ⟺ **rank ∧ surplus > 0.6 ∧ population > 300** | Exact, both species (goblin rows Rank, kobold rows ¬Rank) | `the_slave_rung_is_an_exact_function_of_rank_surplus_and_scale` | Study 006 |
| 7 | Kobold ladder ceiling | Kobold never enslaves, tops out with **elders**; goblin tops with **chief** | Exact, all present rows | `kobold_structures_never_enslave_and_top_out_with_elders` | Study 006 |
| 8 | Goblin flagship coastality | Measured / pinned after the placement fix | **498 coastal, 0 inland** | `goblin_flagship_coastal_split_is_pinned` | Study 003 |
| 9 | Kobold vs goblin coastality | Kobold flagships **less coastal** than goblin (insular in-group radius) | Kobold rate < goblin rate | `kobold_flagships_are_less_coastal_than_goblin_flagships` | Study 006 |
| 10 | Epithet honorific | Goblin epithets carry an honorific affix (**true**); kobold never do (**false**) | 100% goblin true, 0% kobold true | `epithet_honorific_is_true_for_goblin_and_false_for_kobold` | Study 008 |
| 11 | Phonotactic validity | **100%** of generated names well-formed for their own language | 100%, zero exceptions, both species | `phonotactic_validity_is_true_for_every_generated_name` | Study 008 |
| 12 | Name length | Measured / pinned per species | goblin **9.869**, kobold **9.799** chars | `name_length_distributions_are_measured_and_pinned` | Study 008 |
| 13 | Name collision rate | Small, measured / pinned (no re-draw) | **336** zero-collision / **164** nonzero worlds, mean **2.339%** | `name_collision_rate_is_measured_and_pinned` | Study 008 |
| 14 | Blind attribution (standard roster) | **Decisively above chance**; perfect on mooned worlds | **434 / 496 = 0.875**; **perfect (100%) on mooned worlds** | `blind_attribution_beats_chance_decisively` | Study 007 |
| 15 | **Null control** — blind attribution | **At chance**, mostly indistinguishable | **500 / 500 pairs indistinguishable** (decided = 0, twin picked = 0) | `null_control_blind_attribution_is_at_chance` | *(new, §4.1)* |
| 16 | **Null control** — distributions | Every distance **within the sampling bound** | All **0.0** except name-length **SMD ≈ −0.118** | `null_control_distributions_are_within_the_sampling_bound` | *(new, §4.2)* |

Rows 1–14 are the year's four vectors made falsifiable, in order:
**perception** decides where a people's gods sit in the sky (1–4) — a goblin
crowns the sun its diurnal eye ranks highest, a kobold crowns the moon its
nocturnal one lifts, and the sky's own clock (eternal under a locked sun,
returning under a spinning one) rides through both untouched; **psychology**
decides how a people's ladder stratifies and where it settles (5–9) — the
slave rung an exact gate on rank and surplus and scale, the ceiling a matter
of which vocabulary a species brings, the coast priced by an in-group
radius; **articulation** decides how a people's names and epithets are shaped
(10–13) — an honorific that keys to status basis, a phonotactics each name
re-validates against, a name space large enough that collision is the
exception. Row 14 is the integrative test: hand a blind rule two unlabeled
pantheons from one seed and it recovers which is the kobold's **0.875** of
the time overall — and *perfectly* wherever a moon exists to be crowned,
since the lunar head is the cleanest structural tell the two peoples leave.
The 0.875 rather than a rounder number is itself a preregistration honored:
the plan's original floor was 0.9, the first measurement came in at 0.875
(the miss entirely the moonless pairs, where eternal night-stars invert the
periodicity tier), and the honest rate was pinned rather than the rule
retuned.

Every one of these is a reading of the authored vector, recountable by
`why <id>` in the REPL to the committed species-vector fact behind it. The
capstone gallery page, [The Meeting of Seed 42](../gallery/the-meeting-seed-42.md),
walks a single seed through the whole ledger as one legible instance.

## The null control

Rows 15 and 16 are the year's final owed proof. The suite shows the two
peoples differ; a skeptic answers that *any* two draws off one seed differ,
so a decisive 0.875 could be stream noise dressed as culture. The control
that rules this out is a **distributional twin**: a third people,
`goblin-twin`, carrying the goblin's exact vectors — every scalar at 0.5,
every enumeration at its goblin variant. A vector identical to the baseline
cannot move a formula *constructed* to reduce to identity there, so the twin
must be structurally indistinguishable from the goblin. If it diverged
anyway, the divergence would be noise; that it does not is the proof the
formulas read the vector and nothing else.

**Why solo rosters.** The twin cannot simply be added to the shipped
`{goblin, kobold}` world as a third people. Two species carrying *identical*
placement vectors tie on every cell score, and placement breaks ties by
species order — so the second-listed twin would place nothing at all, and
there would be no pantheon to compare. The control therefore runs each
identical-vector species **alone**: two solo rosters, `[goblin]` and
`[goblin-twin]`, over one shared seed range. Placed alone, each lands in the
*same* cells (identical vectors, identical scores, no competitor to lose a
tie to), builds a pantheon from the *same* phenomena, and grows the *same*
ladder. The only thing that can differ between them is the one thing that
*should*: the names, drawn from streams salted by the species name. The solo
design isolates exactly that noise and nothing else — the twin is not a
world where two peoples compete, but a controlled re-run of the goblin's own
world under a different name-salt.

**The primary tooth — at chance.** The `pick_kobold` structural rule (spec
§4, reimplemented independently in the calibration) is run on each seed's
`(goblin-solo, goblin-twin-solo)` signature pair — a symmetric function of
domain, cyclic share, and pantheon size, with **no lexical channel**. Because
both signatures are byte-identical (same cell, same phenomena, same
structure), the rule never separates them: of the **500** solo pairs, **all
500 are indistinguishable** — `decided = 0`, and therefore `twin picked = 0`.
The preregistered direction (indistinguishable rate high, twin-pick rate at
0.5 among decided pairs) is not merely satisfied but *saturated*: there are
no decided pairs at all to compute a rate over. Read against row 14's
**434/496 = 0.875** on the standard roster — and its *perfect* separation on
mooned worlds — the contrast is total. The same rule that recovers a real
kobold most of the time, and a mooned kobold every time, recovers a fake one
never. The 10,000-seed author-time run confirms the picture holds at scale:
the twin's head-domain marginal (9,997 solar / 3 absent) and pantheon-size
distribution reproduce the goblin's exactly.

**The supporting tooth — within the bound.** Per-metric, the calibration
compares the twin's distribution against the goblin's: **total-variation
distance** for categorical columns, **standardized mean difference** for
numeric ones. The preregistered pass bound is derived from *sampling theory*
— the independent-two-sample envelope at the census's sample size — and the
correlation adjustment runs in the safe direction: because the two solo
builds share seed, cell, and phenomena, their signatures are **positively**
correlated, so the independence envelope is a conservative *upper* bound; the
true distances are smaller than independence predicts, never larger. The
measured distances are as small as they can be. Head-deity domain, cult
form, and pantheon size come in at **exactly 0.0** — structurally
byte-identical, as the shared cell and phenomena guarantee. Only name length
diverges, at **SMD ≈ −0.118**, well inside the ±0.2 envelope — the lone
structural trace of the two distinct names, and precisely the axis the noise
was designed to move. A distance *outside* the bound would have been a real
finding (the twin not behaving like a goblin), a STOP-and-report condition,
not a bound to widen. None was.

## Verdict: the suite is decisive, the null control is at chance

The two proofs stand back to back. On the standard roster the two peoples
diverge across every authored axis, and a blind rule recovers which is which
**0.875** of the time overall and **perfectly** wherever a moon exists. On
the solo rosters a people carrying the goblin's exact vectors is recovered
**never** — 500/500 pairs indistinguishable, every structural distribution
distance 0.0 but for a −0.118 name-length SMD that is the intended noise and
nothing more. Decisive separation where the vectors differ; at-chance
indistinguishability where they do not. That is the year's exit criterion
cashed: the measured divergence is a reading of the authored vector, not an
accident of the dice or of which species a world happens to generate second.

## Pinned calibration rows

Measured over the 500-seed `census-of-the-meeting` study (the CI-guarded,
exact-count solo population; `windows/lab/tests/calibration.rs`), first
measured 2026-07-09:

- `null_control_blind_attribution_is_at_chance`: of the 500 solo pairs,
  **`indistinguishable = 500`, `decided = 0`, `twin picked = 0`**. The null
  control is stronger than its directional floor (which asks only that most
  pairs be indistinguishable and decided pairs split near 0.5): *every* pair
  is indistinguishable, so there is no decided pair to split.
- `null_control_distributions_are_within_the_sampling_bound`: head-deity
  domain **TVD = 0.0**, cult form **TVD = 0.0**, pantheon size **SMD = 0.0**,
  name length **SMD ≈ −0.118235** — all within the conservative
  sampling-theory bound (TVD < 0.15, |SMD| < 0.2), the first three exactly
  zero and the last the intended name-salt trace.

The comparative-suite rows (1–14) are pinned in their own already-existing
calibrations, then over `census-lands-drift` and reconfirmed rather than
re-pinned by this study, now over `the-census` (census-as-data renamed and
doubled the sample after this study closed) — the same shape of exact-count
pin the Y2-1, Y2-2, and Y2-3 calibrations use: a deterministic population
asserted row-by-row in CI, never a threshold tuned to make a number look
good.

## What The Meeting added to the registry: nothing

Part of closing the year was confirming The Meeting shipped **no new
predicates**. The campaign introduced one piece of generative machinery — the
study-scoped species roster — but it lives entirely at the Lab's composition
root; no domain was edited, no fact vocabulary changed. The concept registry
(`hornvale concepts`) is byte-identical to its pre-Meeting form, verified by
the drift check on `book/src/reference/concept-registry-generated.md`. The
null control is measurement, not new ontology: the twin is authored data, its
distances are arithmetic, and nothing it produces is serialized into a
shipped world.
