# Rift-and-Fit — terrain epoch v4 (MAP-21)

**Status:** Draft — awaiting G3 (spec review before planning)
**Campaign:** rift-and-fit (suggested campaign name: *The Sundering*)
**Registry:** MAP-21 · **Baseline:** [The Census of Coasts III](../../../book/src/laboratory/census-of-coasts-iii.md)
**Inherits:** the open shoreline-development band (probe 7.3202 vs floor 9.51),
with its anchor-contamination supersession note.

## 1. Goal and mandate

Give every world a fake rifting history: its continents are fragments of an
assembled supercontinent, displaced along great circles, so that conjugate
coastlines — the two margins a rift once separated — visibly fit, the way
South America's shoulder fits the Gulf of Guinea. "Fake" is constitutional:
the history is drawn (a fracture set, a spreading rate), never simulated
forward through time; the ratified rejection of plate simulation stands.

The mandate is **two-sided**, and reaching 9.51 is deliberately *not* the
success criterion (ledger #1):

1. **The generator** — conjugate-margin fit plus the rift textures §5 banks,
   judged against the Census III baseline with the same probe instrument
   Sculpting's tuning season used.
2. **The evidence** — a preregistered probe (§6) measuring what each rift
   mechanism can actually do to the shoreline estimator *before* it is
   built, and a preregistered re-derivation procedure (§7) for the band's
   contaminated anchor. The campaign succeeds if it either closes the band
   honestly or replaces the band's floor with one anchored the way its five
   siblings are — on Earth, not on a dead generator's own median.

The trap this spec confronts up front (Sculpting's diagnostic, restated):
`D = L/(2√(πA))` rewards single-hex land/ocean alternation almost
exclusively; large-scale conjugate geometry — the campaign's signature
deliverable — is expected to move D **barely at all**. The fit is built for
the world's legibility (a reader of the map, an `explain`-style narration,
a future deep-time story), and the band work rides on the cell-scale
mechanisms of §5 and the measurement work of §6–§7. These are separable
deliverables and are staged separately.

## 2. Constitutional constraints

- **Terrain epoch v4** (ledger #2, per decision 0039): any moved byte on a
  default world makes this a contradicting generator — full one-way
  regeneration, no tier coexistence. Conservative-epoch stream discipline:
  one new drawn stream label, `terrain/rift`, appended after
  `terrain/microcontinents`; every existing label's consumption order stays
  byte-identical. Fixture re-pins land in the drifting commit (golden pins
  re-baseline per commit, never deferred); keystones refreeze at merge.
- **Seed is identity.** The fracture network, the spreading rate, and every
  banked-mechanism draw come from `terrain/rift` (per-pair geometry via
  `derive("seam-{i}-{j}")` sub-derivations, matching the lobing-kernel
  `craton-{id}` precedent, so seam enumeration order can never perturb a
  neighbor's draws). Pins fail loudly; generation never retries seeds.
- **The carve runs downstream unchanged** (decision 0056's pipeline).
  Rift-and-fit reshapes the *crust stage only*: the clipped crust field
  feeds elevation, induration, drainage, carve, sea-trim exactly as v3's
  crust field did. Consequence (ledger #10): the carve **erodes the fit** —
  wave-cut, wedge, and barriers modify each conjugate margin independently.
  This is honest (Earth's fit is imperfect for the same reason) and is
  measured, recorded, and never promised away; fit is a **pre-carve
  crust-field contract** (§8).
- **Censuses never regenerate locally** (decision 0046). The tuning
  instrument is the 100-seed probe study; the canonical readout is one AWS
  regeneration at close. Sequencing wrinkle flagged at G3: Census III's own
  canonical cells are still unfilled (the last regen predates Sculpting's
  merge), and if v4 merges first, v3 never gets a canonical census.
- **No new workspace crates; models/data author offline** (decision 0009).
  The Earth mask of §7 is authored by a script outside the workspace
  (type-audit precedent) and committed as a drift-checked fixture.

## 3. The generator: backward-derived assembly

Adopted at G1 (ledger #3) over a forward fragment-first rewrite and a
texture-only coda. The drawn craton set — budget, count, centers, radii,
ages, area normalization, repulsion — stays **byte-identical to v3**. The
rift history is derived *around* the drawn set:

1. **Assembly frame.** A deterministic, draw-free *contact configuration*:
   cratons in id order, craton 0 anchored; each craton i slides along the
   great circle toward craton 0 until its rim first touches an
   already-placed craton's rim (angular separation reaches
   `CONTACT_FACTOR × (r_i + r_j)`, factor slightly below 1 so lobed
   envelopes genuinely suture into one landmass). Termination is guaranteed
   (distance to craton 0 decreases monotonically). This is repulsion run in
   reverse — same slerp machinery, zero stream draws.
2. **Fracture network.** For each pair (i, j) in contact at assembly, the
   seam is the locus where the two caps' envelopes balance (a
   radius-weighted bisector curve), perturbed along its arc-length by
   seeded 1-D noise from `terrain/rift`'s per-pair sub-derivation. One
   curve per contacting pair; triple junctions fall out where three caps
   meet.
3. **The clip.** Each craton's cap, in the assembly frame, is clipped to
   its own side of every seam curve it participates in. The clip is a pure
   multiplicative mask on the craton's envelope with a short taper across
   the curve (the fracture coast reads as a steeper crust gradient than the
   lobed quartic taper — rifted margins and original outer margins get
   visibly different coastline character, for free).
4. **Forward rotation.** Craton i's displacement is the minimal great-circle
   rotation `R_i` taking its assembly center to its (unchanged, drawn) final
   center — an Euler-pole rotation with pole `a_i × c_i`; the free spin
   about the final center is fixed at zero (the boring canonical choice).
   The crust field evaluates craton i's clip in the assembly frame via
   `R_i⁻¹(p)`: a stateless function of position, the `Field` contract
   untouched. Both margins of a seam carry the *same* curve, so conjugate
   coastlines fit by construction.
5. **The narrative scalars.** One drawn global spreading rate (plus
   derived per-pair jitter via sub-derivation); each pair's breakup age is
   *derived* as relative displacement angle / rate. Age is narration and
   facts (§9), never a forward integrator.

**Integration note (plan-level care, spec-level honesty):** terranes,
microcontinents, and the other decorations currently read the raw craton
caps for margin placement. Under the clip, a rim position can now be
ocean. The plan must route decoration placement through the clipped field
(or measure and accept the drift); the tuning season's stayer bands and
the single-craton floor battery are the guards. A single-craton world has
no contacts, no seams, and no clip — it is the control case.

**Performance budget, preregistered:** the clip adds per-sample seam tests
in the crust hot path. Budget: ≤ 1.05× median per-world build time on the
probe study, measured at Stage 1 (the carve's doubled drainage precedent:
measured, then accepted or redesigned — never silently absorbed).

## 4. Pins

- **`--supercontinent` is superseded** (ledger #7): the pin now holds the
  world at the assembly frame — displacement zero, fracture seams drawn and
  visible as sutures, repulsion still skipped. A genuinely assembled
  supercontinent whose future rifts are legible cracks, replacing today's
  partial (0.75-pull) cluster. Pin isolation: pinned and unpinned worlds
  consume identical draws from every stream; the pin zeroes a derived
  transform, never a draw.
- **`--breakup-age <factor>`** (banked, cheap): scales displacement along
  each craton's rotation arc (0 = assembled, 1 = drawn-final). Falls out of
  the age-continuum framing; built only if scouting/lab demand appears
  in-season, otherwise a followup.

## 5. Cell-scale texture: banked mechanisms (0057 discipline)

The fit core (§3) ships unconditionally. Every texture mechanism below is
**banked** — built only when the Stage-0 probe (§6) demonstrates the band
needs it and predicts it moves the estimator, activation constants chosen
by sweep against worst seeds, never guessed (ledger #4):

| mechanism | what it is | expected D leverage (probe confirms) |
|---|---|---|
| fracture-line crenulation | a cell-scale noise octave on the seam curves; both conjugates inherit it | small–moderate |
| rift-shoulder sliver strings | 1–3-cell continental slivers stranded along rifted margins (Seychelles, Jan Mayen); seam-localized kin of the existing microcontinent machinery | **high** — this is the physically-motivated, margin-gated descendant of the very fragment-swarm mechanism the contaminated anchor was built on |
| failed rift arms (aulacogens) | 1-cell-wide bays running from triple junctions into a craton's interior — rifts that opened and stopped (the Benue Trough) | moderate |

Transform-offset staircase margins fold into crenulation (a low-frequency
offset term on the same curves). Heterogeneity comes from fields and drawn
geometry, never from regionally jittered parameters (the global-knobs
principle). Sliver cells classify through the generic lithology buffer,
inheriting the barrier-cell followup rather than duplicating it.

## 6. Stage 0 — the preregistered probe (before any generator code)

House style made explicit (ledger #5): a read-only instrument, kin to the
shoreline diagnostic, run and written up **before** the generator lands.

- Take real v3 seeds' land masks at L6. Inject each §5 mechanism
  *synthetically and in isolation*: sliver strings along swept fractions of
  coast, N aulacogen bays, crenulation at swept amplitudes. Measure the D
  delta each produces. Publish expected-vs-measured per mechanism.
- The activation thresholds for §5 derive from these measured numbers, not
  from hope: a mechanism whose ceiling (applied at plausible physical
  density) cannot move D materially is built for realism only or not at
  all — and the spec says which, per mechanism, once the probe reports.
- The probe also measures **post-carve fit degradation** on a synthetic
  conjugate pair (how much wave-cut/wedge/barriers blur a fitted margin),
  recorded as an unbanded number that calibrates §9's narration honesty.
- Deliverable: a lab page (Census-of-Coasts-IV prologue) recording the
  probe tables, committed before Stage 1 begins.

## 7. The anchor re-derivation (Earth-mask, preregistered)

Shoreline-development is the **only** band not anchored on Earth: its
9.51–21.95 came from 1.3×–3.0× a dead generator's own interim median
(7.315, v1@L6), and the supersession note records that anchor as partly
fragment-swarm noise. The re-derivation makes it a sibling of the other
five (ledger #6):

- An offline script (in `tools/`, outside the workspace) rasterizes public
  Earth coastline data onto the canonical L6 hex mesh → a committed,
  drift-checked land/ocean mask fixture.
- The **unchanged** estimator runs over that mask, giving `D_earth@L6` —
  Earth measured through Hornvale's own instrument. The estimator formula
  never changes mid-family (Census II discipline); instead a companion
  metric — the multi-scale coastline-roughness slope, D measured at
  L4/L5/L6 — is appended to the lab registry (metrics are code, decision
  0011), recorded **unbanded**, immune to the single-hex exploit, available
  to any future band conversation.
- **Preregistered proposal, ratification reserved to Nathan:** new band =
  `[D_earth/1.6, 1.6 × D_earth]`, the sibling bands' typical multiplicative
  generosity, committed here *before* the mask is built so the number
  cannot be fitted to the generator's output. Whether the supersession is
  adopted — before tuning, after tuning, or not at all — is Nathan's call
  at G3/G6, per the ledger-#11 ruling that no floor is invented or replaced
  unilaterally.

## 8. Acceptance and batteries

- **Bands:** the four stayers (hypsometric-bimodality, continent-count,
  largest-continent-share, plate-size-gini) and shelf-fraction must hold
  inside their bands on the probe across every iteration.
  Shoreline-development is judged against whichever floor governs at close
  (9.51, or the ratified §7 re-derivation) — recorded exactly as measured
  either way.
- **Fit battery (pre-carve, ledger #10):** both margins of a seam derive
  from one shared assembly-frame curve function — asserted by construction
  (same function, exact), plus a sampled-geometry check: rotate margin
  samples back by the stored `R_i` and assert coincidence within
  quantization.
- **Pin isolation:** `--supercontinent` and unpinned worlds consume
  identical draw counts per stream (extends the existing
  `tectonic_properties.rs` suite to `terrain/rift`).
- **Single-craton control:** `--continents 1` worlds have no seams; the
  crust field must show zero clip influence, and the single-craton
  `shelf_land_ratio > 0.05` floor battery (Nathan-ruled) holds throughout.
- **Determinism:** byte-identity of worlds/almanacs on repeated builds, as
  every epoch; quantization at emit boundaries only.
- **Escalation criterion, preregistered:** if the tuning season, with every
  probe-approved mechanism built and swept, still cannot reach the
  governing floor, the stop condition is a band-supersession conversation
  with the §7 evidence on the table — not a fifth unbanked mechanism.

## 9. Facts and narration (minimal)

Via the existing terrain facts machinery (ledger #11): per rifted pair, a
breakup fact (subject: the world's terrain entity or the pair; predicates
`terrain/rifted-from`, `terrain/breakup-age`, `terrain/spreading-rate`
following the concept-registry naming conventions), so `explain`-style
consumers and future deep-time work can narrate the history. Wording
carries the ledger-#10 honesty: margins fit *up to subsequent erosion*.
No new fact infrastructure, no narration verb (window work, out of scope).

## 10. Non-goals

- **No plate simulation over time** — ratified; the history is drawn.
- **Plates untouched** (ledger #9): the known incoherence between rifted
  (passive-reading) margins and independently-drawn plate motion is
  accepted and recorded; unification is registry row MAP-48.
- **No estimator change**; the companion metric is appended, not
  substituted.
- **No staggered multi-generation breakup** (nested fracture hierarchies —
  Pangea inside Gondwana): registry row MAP-49.
- **No local census regeneration**, ever (0046).

## 11. Staging sketch (for the plan)

- **Stage 0:** the probe (§6) + the Earth mask and `D_earth@L6` (§7).
  Measurement first; both are read-only against v3.
- **Stage 1:** assembly frame + fracture network + clip + forward rotation
  (fit core), fit battery, pin isolation, perf budget check.
- **Stage 2:** `--supercontinent` supersession; facts (§9); companion
  metric.
- **Stage 3:** tuning season — banked mechanisms activated per Stage-0
  demand, swept constants, iteration table in Census of Coasts IV.
- **Close:** epoch re-pins, AWS census regen (Nathan-authorized), chronicle,
  retro, registry flips (MAP-21; new rows MAP-48 plate/fracture unification,
  MAP-49 staggered breakup, MAP-50 multi-scale roughness as a future banded
  metric), Confidence Gradient re-score if the coast bet moves.

## 12. Decisions promoted from the brainstorm ledger

Ledger: `.superpowers/sdd/rift-and-fit-decision-ledger.md` (worktree, moved
from session scratchpad). Material entries: #1 dual mandate; #2 epoch v4;
#3 backward-derived assembly; #4 banked texture per 0057; #5 probe-first;
#6 Earth-mask anchor + companion metric, supersession reserved to Nathan;
#7 supercontinent-pin supersession; #8 census sequencing flagged; #9 plates
out of scope; #10 fit is a pre-carve contract; #11 minimal facts.
