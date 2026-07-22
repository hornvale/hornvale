# The Tumult — Living-Community C3, Slice 1: The Sandpile

**Status:** design (G3 review)
**Program:** The Living Community engine (campaign 3 of ~5), conflict-as-criticality (SOC-criticality)
**Slice:** the bare sandpile — crowding drives displacement onto occupied land; the cascade is the avalanche; the conflict-size distribution is the falsification metric. A genesis epoch.
**Base:** origin/main @d9f6a55e (contains The Sundering @6ede5833).

---

## 1. The payoff

Conflict that no one floored. Today the deep-history bake carries a raid mechanic that never
fires — a displaced people always finds empty ground, so raids, flees, and resettles all
measure **zero** (The Sundering). This slice supplies the missing ingredient — **crowding** —
and lets war *emerge*: when the habitable land fills, a people driven off its cell by the ice
or by hunger can no longer flee to vacant ground, so it takes an **occupied** cell instead,
and the evicted occupant must in turn find a home — a **cascade** rippling across the moving-sea
connection graph, the Sea-Peoples' collapse in miniature.

The tell that this is genuine *self-organized criticality* and not noise is the **shape of the
cascade-size distribution**. A pile of sand fed one grain at a time avalanches at every
scale — mostly tiny, rarely enormous — a power law (Richardson's scale-free war sizes). If the
emergent conflict sizes obey a power law, the dynamics self-organized to the critical point on
their own. If instead they form a bell curve or a single catastrophic spike, the bare sandpile
is **falsified**, and that failure is the honest motive for the richer drivers (cohesion,
grievance) that later slices add. The census-floor tool The Sounding used to *force* raids
becomes, here, the instrument that *tests* whether they self-organize.

## 2. Context — the program, and the keystone The Sundering handed over

The Living Community (C1) grew settlements from a deep history; The Connection Graph (C2 s1)
derived the transport topology; The Sundering (C2 s2) routed the dynamics over a time-varying
version of it. Each measured the same thing about conflict: **there is none**, because the
world has vacant land to spare — a frozen-out community migrates to empty ground rather than
fight (C1), and even under the moving sea raids/flees/resettles stay at zero (The Sundering).
The Sundering's closing finding named the lever precisely: the diaspora's *volume* — and its
conflict — is a matter of **crowding/pressure**, handed to this campaign.

SOC-criticality is a multi-slice campaign. This is **slice 1: the bare sandpile** — the minimal
mechanism that could produce a power law. Deferred to later slices, by design: **cohesion**
(ʿasabiyya rising under adversity and decaying in comfort — a second slow variable), **grievance
/ feud** (a fold over the ledger of past raids crossing a collective-liability threshold),
**shock-timing** (seasonal/astronomical modulation of when the pile is perturbed), and built
roads / diffuse coupling.

## 3. Architecture (constitutional layering)

- **`windows/worldgen` — `history_bake.rs`** (the bake). The cascade lives here, in the two
  places a displaced community currently vanishes for want of vacant land:
  `step_community` (a climate-shock eviction with no reachable vacant cell → Famine) and
  `raid` (an evicted community with no vacant refuge → lost). Both become: **displace the
  nearest occupied cell over the graph, and recurse on the evicted occupant** — a bounded
  cascade. The cascade tallies its size into the `BakeCensus`.
- **`windows/lab`** — the falsification metric: a census metric over the baked skeleton (or the
  `BakeCensus`) reporting the cascade-size distribution and a power-law goodness measure, plus
  the calibration/gate that adjudicates it.
- **`domains/*`** — unchanged; the cascade is a composition-root dynamic over the existing
  history data model and the topology graph. No new domain, no new predicate required (a
  cascade is a sequence of the existing `Fled`/`Ended::By` occupation records).

**Derived-vs-committed.** The cascade produces ordinary occupation records (a raid closes the
evicted community with `Fled`/`ended_by = By(displacer)`, opens its refoundation elsewhere) —
the same committed shapes C1 already emits. **No new committed field, no new predicate.** The
cascade-size distribution is *read back* from the committed skeleton for the metric; it need not
be committed itself.

## 4. The mechanism — the cascade

### 4.1 The drive to criticality

The slow drive already exists: population growth (logistic toward capacity) and daughter
founding multiply communities until the habitable graph **saturates**. No new drive is added.
Whether seed-42's bake reaches saturation is **measured** (§8): if cascades fire at volume, the
drive suffices; if not, the density is calibrated (founding density / `SETTLERS_PER_CAPACITY` /
bake span) — a fidelity knob brought to Nathan, never a floor. The Sundering's sundered
landmasses already concentrate peoples (isolation → local saturation), so cascades are expected
to ignite on the small, full landmasses first.

### 4.2 The relaxation — displacement onto occupied territory

The single new rule: **a community that must relocate and finds no vacant habitable cell
reachable over the era graph displaces the nearest *occupied* cell instead of collapsing.** It
raids the occupant (the existing `raid` seize/flee shapes), takes the site, and the evicted
occupant re-enters the same relocation logic — which may again find no vacant cell and displace
*its* nearest occupied neighbour, and so on. The recursion is the avalanche; it terminates when
a displaced community either reaches vacant land, or is absorbed (a small remnant lost when the
whole reachable component is full). Applied at both current dead-ends:

- `step_community`, climate-shock branch (`eff == 0`, `nearest_dest` → `None`): today → Famine;
  now → displace nearest occupied, cascade.
- `raid`, evicted-community branch (`nearest_dest` → `None`): today → lost; now → displace
  nearest occupied, cascade.

"Nearest occupied" is resolved by graph distance with the same total, deterministic tie-break
the existing `raid_target`/`nearest_dest` use (`f64::total_cmp`, ascending `CellId`).

### 4.3 Boundedness and determinism (Lorenz-safe)

- **Bounded cascade depth.** A hard cap on cascade length guards non-termination and the
  size-risk (a runaway cascade cannot exceed the occupied-cell count; the cap is a measured
  ceiling, per the Sounding's OOM lesson). The cap is high enough not to clip real avalanches
  (measured in the cost gate).
- **Deterministic, seed-replayed, no forward-integration.** The trigger is the *committed*,
  seed-replayed climate eras and deterministic over-pressure — **never** a stochastic forward
  integration of a chaotic pressure variable (the Lorenz guard-rail: the cascade reads the
  world's state, it does not integrate a chaotic ODE). The cascade order is a total function of
  the frozen epoch state (graph distance + `total_cmp`), so same seed ⇒ byte-identical skeleton.
  No new seed draw beyond the existing raid path's.

### 4.4 The cascade-size tally

Each relaxation records its **size** — the number of displacements in one cascade — into the
`BakeCensus` (a new histogram/tally field, read back by the metric). This is the raw material of
the falsification test.

## 5. The falsification metric (the headline)

A `windows/lab` metric reports, over the bake, the **distribution of cascade sizes**, and a
gate adjudicates its shape:

- **Power law** (many small, rare large; a heavy tail over ~1.5 decades) — SOC confirmed; the
  bare sandpile self-organized to criticality.
- **Bell curve / single spike / no cascades** — falsified; recorded as the honest finding that
  crowding + cascade alone are insufficient, motivating cohesion/grievance (later slices).

The metric is preregistered on a named axis (the log-log slope / a heavy-tail test over the
cascade-size histogram), measured across a seed sample, not a single world. **Either outcome
ships** — a confirmed power law is the payoff; a documented falsification is a real result that
sharpens the next slice. This is the measure-don't-narrate spine of the campaign.

## 6. The epoch

A genesis epoch. The cascade changes which communities survive and where, so the committed
skeleton moves: **byte-identity vs the prior main deliberately breaks; the census regenerates on
lefford** (decision 0063 — carve-out, authorized at G6). No new committed field, no new stream
label (the cascade uses the existing `history/bake` draws), no new predicate — a cascade is a
chain of the existing `Fled` occupation records. The seed-42 keystone refreezes at merge.

## 7. Determinism

Same seed + pins ⇒ byte-identical skeleton. The cascade is a total function of the frozen epoch
state (graph distance, `f64::total_cmp` tie-breaks, no `HashMap`/RNG, no wall-clock). Triggers
are seed-replayed (committed eras) and deterministic (over-pressure); no chaotic forward
integration (Lorenz guard-rail). Bounded cascade depth. The cascade-size tally quantizes only at
any emit boundary.

## 8. Success criteria — measure, don't narrate

1. **Conflict fires at volume.** With crowding, raids/flees/resettles rise from zero to a
   genuine signal — cascades occur. If they do *not* (the world never saturates on seed-42),
   that is a density-calibration finding brought to Nathan, not a floor.
2. **The map is not depopulated (the recurring risk).** Cascades must not empty the world:
   alive-at-`now` stays in the walkable band and the collapse (Famine) share stays under a
   preregistered ceiling. Runaway cascades that starve the map are a fidelity finding for
   Nathan, never patched with a floor.
3. **The falsification metric (headline).** The cascade-size distribution is measured and its
   shape adjudicated (power law vs bell/spike) across a seed sample. Power law ⇒ SOC confirmed;
   otherwise ⇒ documented falsification. Both ship.
4. **A cost gate** bounds the cascade wall-time and the maximum cascade depth actually reached
   (measured, per the Sounding's OOM lesson).

## 9. Non-goals (§9 — read before assuming scope)

- **Cohesion (ʿasabiyya)** — a second slow variable (rise under adversity, decay in comfort) —
  a later slice.
- **Grievance / feud** — a fold over the ledger of past raids crossing a collective-liability
  threshold — a later slice.
- **Shock-timing** — seasonal/astronomical modulation of the perturbation — a later slice.
- **Built roads / diffuse coupling** — the connection-graph program's later slices.
- **A new committed field, predicate, or stream label** — the cascade uses the existing raid/
  occupation shapes and draws.

## 10. Definition of Done (per CLAUDE.md)

- The cascade replaces both no-vacant dead-ends in `history_bake.rs`; bounded depth;
  deterministic; the `BakeCensus` tallies cascade sizes.
- The three §8 gates plus the cost gate pass (or the falsification is documented, labelled).
- Census regenerated on lefford (authorized at G6); the census-close cascade re-pinned; the
  seed-42 keystone refrozen from main's tip.
- Chronicle, retrospective, book freshness sweep, Confidence Gradient re-score (the
  conflict/SOC bet moves from `raw`), registry flip (SOC-criticality → elaborated/slice-1), full
  gate + artifact drift.
