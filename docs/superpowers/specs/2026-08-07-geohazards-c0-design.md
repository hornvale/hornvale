# The Repose — the hazard field, the named mountain, and what a people forgets

*Geohazards program, campaign 0 (the keystone). Named for the volcanological
**repose interval** — the quiet between eruptions, which is §6's axis and, when
it outruns memory, the thing this campaign is about.*

Frontier rows: `DOM-15` (geohazards), `UNI-15` (the omen-grade discipline),
`BIO-36` (disturbance as the composed field), `SOC-criticality` (the falsified
power laws), `TOOL-analytic-limiting-case`.

Decisions in force: [0100](../../decisions/0100-fact-phenomenon-myth.md) (the
three registers), [0011](../../decisions/0011-studies-are-data-metrics-are-code.md)
(studies are data), [0016](../../decisions/0016-studies-preregister-hypotheses.md)
(preregistration), and the Constitution's **models author, dice roll**.

---

## 1. Motivation

**Volcanism already ships in Hornvale, and it ships as a pure gift.**
`domains/terrain/src/lithology.rs` gives fresh volcanic parent rock the
`andosol` classification, commented *"Fresh volcanic, very fertile"*.
`prospectivity()` weights tectonic `unrest` at 0.3, so the same ground carries
the world's best ore. `windows/locale/src/substrate.rs` renders any land cell
above `unrest > 0.6` as `Basaltic` or `Ashen`. A world's most fertile and most
mineral-rich ground is its most tectonically violent ground, and **nothing in
the model ever charges for it**.

This campaign is therefore not "add a negative pole" — the framing `DOM-15`
inherited from `SKY-impacts`. It is **close the loop on a one-sided force**.

The shape to reproduce is historical and specific. Vesuvius in 78 CE was not a
known hazard; it was a fertile hill with towns on its flanks, with no eruption
in living memory and no working local category for what it was. What made
Pompeii possible was not ignorance of volcanoes in general but the gap between
a recurrence interval of roughly two millennia and a cultural memory of a
century. The interesting quantity is not the eruption. It is **the distance
between how often the mountain acts and how long anyone remembers that it
does**.

### What is missing, precisely

`GeneratedTerrain::unrest_at(cell)` (`domains/terrain/src/provider.rs:112`) is a
live `[0,1]` field. `BoundaryKind::Transform` is documented as *"unrest, little
relief"*. Volcanic edifices are modelled in `elevation.rs` with a decay length.
The **substrate is built**. What does not exist anywhere in the workspace is an
**event**: a grep for `earthquake|seismic|eruption|tremor` across
`domains/ windows/ kernel/ cli/` returns only adjectives on rock and soil.

## 2. Scope

### 2.1 In scope (C0)

1. A per-cell **hazard field** — recurrence rate derived from unrest, boundary
   kind, and edifice presence.
2. A derived, persistent **volcano identity** keyed on `(seed, cell)` — the
   same edifice, recurrence, style, and name on every recomputation.
3. A drawn **event stream** over a time window, magnitudes from an authored
   law.
4. **Knownness** — a people's decaying awareness of its own hazard, satisfying
   `UNI-15`'s source/sink requirement.
5. The **exposure readout** of §6, as a committed probe fixture.

### 2.2 Non-goals

Each of these is a later slice or a different campaign. Listed so a reader
cannot mistake an omission for an oversight:

- **Consequence facts (C1).** No settlement is destroyed, abandoned, or
  refounded by this campaign. C0 commits **nothing** to the ledger.
- **Walk-scale perception (C2).** No tremor, ashfall, or rumble reaches
  `windows/locale` or `windows/vessel`.
- **Disturbance → vegetation (C3).** This belongs to `BIO-36`'s hub, with
  volcanism as its first writer, and is explicitly *not* built here as a
  geohazard-specific coupling. See §5.
- **Cave-ins / anthropogenic ignition (C4).** The naturalness substitution
  found a man-made ignition already latent in the Deep Realm's chamber
  lattice. Registered, not built.
- **Tsunami.** Alone among `DOM-15`'s three children it is a *teleconnection*,
  arriving far from its source with no local ignition, so it cannot be answered
  by a per-cell field. Needs its own mechanism.
- **Aftershocks / ETAS.** See §3.3 and §7 — this is the trap, and it is
  declined deliberately.
- **Feedback into settlement siting.** Knownness is *measured* in C0, never fed
  back into placement. Feeding it back would move the census and requires its
  own preregistration.
- **Concept registration.** No new concepts, so no `Correspondent`/`Void::Gap`
  language machinery and no `concepts` dump churn. Religion is C1's consumer.
- **Any cross-species memory claim.** See §3.4 — the axis does not yet
  differentiate.

## 3. The mechanism

### 3.1 The hazard field — steady, never accumulating

`hazard(cell) -> Recurrence`, a pure function of `unrest_at(cell)`, the nearest
`BoundaryKind`, and edifice presence.

**It does not accumulate.** No stress builds toward a threshold; no state is
carried between events. This is not a simplification but a constitutional
requirement: `BIO-36` fixes tier-0 as a *drawn stationary regime* — *"invented
on demand and narrated backwards, never forward-simulated"* — and the Lorenz
guard-rail forbids the integrator alternative outright. The timeline of a
Hornvale catastrophe has no left half, and that is the design.

### 3.2 Volcano identity — derived, not committed

A volcano is a persistent derived object keyed on `(seed, cell)`: identity,
name, recurrence interval, eruption style. Recomputed on demand; never stored.

Decision 0100's test — *could I recompute this from the seed alone? If yes, it
is not a fact* — puts this squarely in the **phenomenon** register. It costs no
facts and no save-format change.

Identity is load-bearing rather than decorative, and the reason is §3.4:
**you cannot forget a field value.** Memory needs an object to attach to.

Earthquakes get **no identity**. Negating "localized" yields a belt with no
point of origin, which is what a quake is; nobody names an earthquake. They are
a field property plus drawn events.

`NameKind` today has exactly four variants — `Settlement`, `Deity`, `Epithet`,
`Person` — and **no landform in Hornvale has a name**. The widening copies
`Person`'s precedent verbatim: a new variant on *its own seed path*, so adding
it reseeds nothing that already exists.

### 3.3 The event stream — authored, never predicted

Events are drawn for a `(seed, cell, window)` query. Magnitudes come from an
**authored** distribution: Gutenberg–Richter for seismicity, a VEI-shaped law
for eruptions, Poisson inter-event times given the rate.

This is the campaign's highest-risk decision and it is made deliberately.
`SOC-criticality` predicted a power law and was falsified **twice** —
σ ≈ 0.051 (The Tumult), then σ ≈ 0.11 with the shape unmoved (The Tithe).
The standard aftershock model, ETAS, is a *branching process whose control
parameter is σ*: adopting it would not be a third power-law bet by analogy but
**numerically the same experiment on the same statistic**.

Authoring the distribution is the ratified posture ("models author, dice roll")
and it is the stronger epistemic position, not the weaker one.
`TOOL-analytic-limiting-case` exists because *"neither The Tumult nor The Tithe
could distinguish 'the mechanism is wrong' from 'the implementation is
wrong'."* An authored law **is** the analytically-solvable regime: we know in
closed form what must come out, so a mismatch has exactly one interpretation.

**Consequence for §6: the magnitude distribution may never be the headline.**
It is put in by hand; recovering it proves the implementation, not the world.

### 3.4 Knownness — the flow balance `UNI-15` requires

`UNI-15` requires a source and a sink so the stock is a computable equilibrium.
The hazard rate can be neither (§3.1). The quantity that legitimately flows is
knowledge:

```
  SOURCE   an eruption occurs        ->  knownness := 1
  DECAY    time passes               ->  knownness *= decay(half-life)
  SINK     no living memory remains  ->  knownness -> 0

  STOCK    a people's awareness of its own hazard
```

Derive-only, and it **carries a holder**, which decision 0100 requires of
anything in the myth register. It is explicitly permitted to contradict the
hazard field: a people may be wrong about its mountain.

**The half-life is derived from `generation_length`, and no cross-species claim
is preregistered.** The tempting move is elves-remember/humans-forget. Today
that would be false richness: `LifeSchedule::Paced` **ships with no occupant**
— `domains/species/tests/coverage.rs:284` asserts it sits at `Declared` with
zero witnesses, naming C2c (dwarves) as where the first authored long-lived
kind must land — so lifespan is currently a function of **mass**, and the model
would be claiming that a heavier people remembers its volcano longer. Coupling
to `generation_length` is correct and inert now and becomes real for free when
C2c lands, which is the same deliberate empty-channel pattern The Long Age
chose. Predicting on it before then would measure mass.

## 4. Components

| component | home | register | notes |
|---|---|---|---|
| `hazard(cell)` | `windows/worldgen` | phenomenon | pure read of `unrest_at` + boundary + edifice |
| `volcano_at(seed, cell)` | `windows/worldgen` | phenomenon | identity, recurrence, style, name |
| `events_in(seed, cell, window)` | `windows/worldgen` | phenomenon | drawn; authored magnitude law |
| `knownness(seed, people, cell, now)` | `windows/worldgen` | myth | source/decay/sink over the event stream |
| `NameKind::Landform` | `domains/language` | — | new variant, own seed path |
| exposure probe | `windows/worldgen/tests` | — | committed CSV fixture |

## 5. Architecture and layering

**Everything composes at `windows/worldgen`.** `BIO-36` already ruled that the
disturbance hub can never be a domain crate: terrain (volcanism), paleoclimate
(glaciation), climate (fire), settlement (clearing), culture (strife) and BIO-7
(grazing) all *write* it while vegetation alone *reads* it, so a crate would
import half the workspace. `DOM-ocean`, `DOM-cryosphere` and `DOM-fire` each
independently reached the identical resolution.

**`domains/terrain` gains nothing.** The substrate it already exposes —
`unrest_at`, `BoundaryKind`, edifices — is sufficient. `domains/language` gains
one `NameKind` variant and nothing else.

This is the tree finding that matters: **geohazards is a sibling, not a
parent.** Building "volcanism → vegetation" directly would make the sixth of
seven one-off couplings, each re-deriving the same field. Contributing a writer
to `BIO-36`'s hub instead makes the remaining siblings cheap.

### 5.1 The byte-identity invariant

**C0 is a pure read.** Nothing draws from an existing stream, so no existing
world moves a byte — the property that made The Freshwater the cheapest safe
save-format change. New stream labels only; per `domains/CLAUDE.md`, *new label
= safe, changed or reused label = an epoch*. `NameKind::label()` is an
exhaustive match, so the compiler enumerates the widening; per the
enum-widening rule no wildcard is used to silence it.

## 6. Preregistered measurement

**Frozen before any implementation exists** (decision 0016).

### 6.1 The question

> Do settlements over-occupy high-unrest ground relative to the land base rate?

### 6.2 Instrument and population

A **campaign probe test** in `windows/worldgen/tests/`, following the shipped
pattern of `occupancy_readout.rs` / `waterline_probe.rs` / `confluence.rs`,
writing a committed CSV fixture.

**Not a Lab metric.** Exactly **9** studies use `"metrics": "all"` (verified:
`grep -l '"metrics"[[:space:]]*:[[:space:]]*"all"' studies/*.json` → 9 files),
and `the-census` is **1,000 seeds × every registered metric**. Registering a
metric joins all nine permanently and triggers the two-fixture refresh.
Promoting a proven-cheap statistic to a census metric later is easy; removing
one is not.

Population, stated rather than implied:

- **seeds** `1..=30`, pooled — the precedent already used for the species
  elevation percentiles
- **filter**: *settleable land* — land above sea level with non-zero carrying
  capacity, the same predicate `ConditionNiche`'s corrected frame uses
- **mesh level**: full build depth, as `occupancy_readout.rs` uses

### 6.3 The statistic

Settlement share and population-weighted share per **unrest decile**, divided
by that decile's land-area share → an **exposure ratio** per decile.

Reported as the **full ten-element vector plus percentiles** — a distribution,
not a median. A median cannot see the tail, and the tail is the object.

**Also reported per people, never only pooled.** Actuarial practice on the
identical structure (flood-plain and wildfire-interface development) separates
its three standard diagnoses of an unpriced hazard — agents don't know, agents
externalize, the risk is negligible — by **variance across agent types**, and
pooling is what destroys that signal. A pooled 1.0 with every people at 1.0 is
a different world from a pooled 1.0 with peoples spread across it.

### 6.4 The confound, controlled in advance

Unrest correlates with two **opposed** things: volcanic soil (attracts) and
mountains (repels — island arcs and edifices are high ground). An unstratified
ratio can read ≈ 1 because the two cancel, which is indistinguishable from *no
effect*. The readout therefore **stratifies by elevation band**, and this is
fixed here, before the code.

### 6.5 Outcomes — all three informative

| result | what it establishes |
|---|---|
| ratio **> 1** in high-unrest deciles | the risk/reward tension is real; `andosol` fertility reaches siting; C1 has a job |
| ratio **≈ 1** | fertility never reaches siting — a finding about **The Ground's** shipped work, not about geohazards |
| ratio **< 1** | the elevation penalty dominates; volcanic ground is net-repellent, and the Pompeii shape exists only inside a band |

The null is a publishable headline, per the project's standing practice — but
only once §6.6 makes it decidable.

### 6.6 Disambiguating a null — the counterfactual arm

A reading of ≈ 1 has **five** possible causes, and the three sub-sections above
distinguish only two of them:

| reading of ≈ 1 | separated by | covered |
|---|---|---|
| true null — no effect exists | the counterfactual below | **§6.6** |
| cancelling effects — fertility vs. the elevation penalty | stratification | §6.4 |
| instrument blind — deciles too coarse | discrimination guard | §6.7 |
| **mechanism absent** — fertility never reaches the siting code path | the counterfactual below | **§6.6** |
| below resolution — effect real, `n` too small | per-people dispersion | §6.3 |

**True null and mechanism-absent are the pair that matters**, and they are
indistinguishable from the observed distribution alone — yet one is a fact
about worlds and the other is a wiring gap in The Ground. Shipping a null that
cannot tell them apart would reproduce, from a new direction, exactly the
ambiguity `TOOL-analytic-limiting-case` exists to remove.

**The arm:** rebuild the same seeds with `andosol` fertility neutralized and
measure whether siting moves **at all**.

- siting does **not** move → the mechanism was never wired; the finding is
  about The Ground, and is actionable there
- siting **moves** but the ratio stays ≈ 1 → the mechanism is wired and
  something opposes it; the finding is about worlds

This is a mutation on the **pipeline**, not on the derivation — the distinction
that has bitten before, where perturbing a derivation proved nothing about
whether the pipeline carried the authored value.

**The result has a shelf life.** "Mechanism absent" stays true only until
someone repairs The Ground, so this probe is declared a **regression
tripwire**: re-run after any campaign touching settlement siting or soil
fertility, and after any absorption of main that does. A one-shot readout that
silently goes stale is the documented failure mode.

### 6.7 Guards on the guard

- **Discrimination.** Assert the unrest deciles genuinely differ in andosol
  share. If they do not, the probe measures nothing and passes green
  regardless — The Benchmark's vacuous-and-green failure.
- **Ceiling as well as floor.** Assert an absurd-*high* exposure ratio also
  fails. A floor alone cannot catch a runaway.
- **Baseline first.** The reading is takeable on `main` **today** —
  `unrest_at()` and settlement placement both ship. Take it before writing any
  geohazard code, so a "≈ 1" answer is learned before anything is built on the
  premise.

### 6.8 The implementation check, separately

Draw a large sample and assert the magnitudes reproduce the **authored** law
within tolerance (§3.3). This is `TOOL-analytic-limiting-case` satisfied. It is
an implementation check, **not** a world finding, and the chronicle must not
present it as one.

## 7. Risks

| risk | mitigation |
|---|---|
| Reading a recovered magnitude law as a discovery | §3.3 and §6.8 state it is authored; the chronicle wording is a DoD item |
| Drifting into ETAS/aftershocks mid-campaign | Named as a non-goal (§2.2); it is σ, the twice-falsified statistic |
| The exposure ratio reads ≈ 1 and the campaign looks empty | §6.6's counterfactual arm makes the null decidable — world-fact vs. wiring gap — rather than merely reportable; the baseline is taken first |
| Knownness ships with no consumer and cannot be seen to be wrong | The readout consumes it — The Hollow's lesson |
| Scope creep into C1 via "just one fact" | C0 commits nothing; the byte-identity test in §5.1 enforces it |
| Probe cost reddens the gate | If slow, heavy tier with a **verbatim** `heavy:` ignore-reason token — matched exactly, not by prefix |

## 8. Definition of Done

- [ ] Baseline exposure reading taken on `main` before implementation
- [ ] Hazard field, volcano identity, event stream, knownness implemented at
      `windows/worldgen`
- [ ] `NameKind` widened on its own seed path; no wildcard used on the
      exhaustiveness error
- [ ] Byte-identity test: seed-42 almanac, census row, and scene output
      identical before and after
- [ ] Authored-law recovery test (§6.8)
- [ ] Exposure probe + committed CSV fixture, with both guards of §6.7,
      per-people dispersion (§6.3), and the counterfactual arm (§6.6)
- [ ] `make gate` green; `make rebaseline` run and
      `docs/audits/type-audit-report.md` diffed (any new `pub` boundary moves
      it)
- [ ] Book: chronicle entry + freshness sweep; Confidence Gradient re-scored if
      a bet moved (decision 0030)
- [ ] Retrospective in `docs/retrospectives/` (decision 0020)
- [ ] Registry rows updated: `DOM-15` status, and new rows for the deferred
      slices (§9)
- [ ] Chronicle states plainly that the magnitude law was authored

## 9. Decisions promoted from the ledger

No campaign worktree exists for this brainstorm, so the autopilot ledger is
promoted here directly; this section is the durable record.

| # | gate | decision | why | ideonomy |
|---|---|---|---|---|
| 1 | G1 | Decompose geohazards into a program: C0 keystone + C1 consequence + C2 felt + C3 disturbance + C4 cave-in | All four requested payoffs read one event stream; only C1 commits facts | 1 pass, 1 overturn |
| 2 | G1 | Payoff B reassigned out of geohazards into `BIO-36`'s hub | `BIO-36` already ruled the hub composes at the root; a direct coupling would be the 6th of 7 | (in #1) |
| 3 | Q | Volcanoes get identity, derived not committed | 0100's recompute test puts it in the phenomenon register; identity is load-bearing because knownness needs an object | 1 pass, 1 overturn |
| 4 | Q | Earthquakes get **no** identity | Negating "localized" yields a belt with no origin; nobody names an earthquake | (in #3) |
| 5 | Q | Magnitudes drawn from an **authored** law; ETAS declined | "Models author, dice roll"; ETAS's σ is the twice-falsified statistic; authoring satisfies `TOOL-analytic-limiting-case` | 1 pass, 1 overturn |
| 6 | Q | Readout is a probe test, not a Lab metric | Verified: 9 studies use `"metrics": "all"`; `the-census` is 1,000 seeds | (in #5) |
| 7 | Q | Knownness in C0, but no cross-species memory claim | `UNI-15` requires the sink; but `LifeSchedule::Paced` has no occupant, so the axis does not differentiate | (in #3) |
| 8 | G2 | C0 registers no new concepts | Keeps the artifact blast radius to the type-audit report; religion is C1's consumer | — |
| 9 | Q | §6 amended: counterfactual arm, per-people dispersion, declared shelf life | A bare ≈ 1 cannot separate *true null* from *mechanism absent*; the actuarial and immunological re-instantiations of the same abstract shape both say don't infer from the resting state | 1 pass, **1 overturn** |

### Capture manifest

Promoted to the idea registry as part of this campaign's DoD:

- **C1 consequence**, **C2 felt**, **C4 cave-in (anthropogenic ignition)**,
  **tsunami as a teleconnection** — new rows.
- **`DOM-15`** — status updated, decomposition recorded.
- **The shared "rare sourced-and-sunk episodic event" machine** — `UNI-15`,
  `SKY-impacts`, `CLIM-hazards` and `DOM-15` are four independent descriptions
  of one machine. Registered as an observation; deliberately *not* built as an
  abstraction (YAGNI).
- **Intraplate seismicity** — the empty branch of the spatial-distribution
  tree; the surprising, far-from-boundary quake. Row only.

### Rejected branches

- **Stress accumulation toward a threshold (sandpile).** Forbidden by the
  Lorenz guard-rail; `BIO-36` fixes tier-0 as drawn.
- **Committing volcanoes as ledger entities.** Richest and most queryable, but
  0100's test argues against it — the whole object is seed-derivable.
- **Feeding knownness back into settlement siting in C0.** Moves the census;
  needs its own preregistration.
