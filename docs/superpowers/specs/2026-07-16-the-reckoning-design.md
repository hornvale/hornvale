# The Reckoning — Design

**Date:** 2026-07-16
**Status:** COMPLETE — closed 2026-07-17, awaiting merge. G3 RATIFIED 2026-07-16 (T_MAX = 15 Gyr; full epoch incl. Capture).
**Corrected at close** (this spec governs, so where execution overturned it the text below is amended in place, with the supersession marked rather than the history erased):
- The epoch shipped **TOTAL**, not the partial epoch §5.3 and §7 designed — the eclipse-geometry correction (ledger #14) perturbs every mooned world, not only captured ones.
- The blast radius is **measured: 633/1000 seeds** (§7, §9.4). It was an open question at ratification; it is not any more.
- The AWS census regen was **declined by Nathan**; the resulting 633-seed staleness is **accepted debt** (§9.4).
Chronicle: `book/src/chronicle/the-reckoning.md`. Retrospective: `docs/retrospectives/the-reckoning.md`.
**Tickets:** none yet — this is campaign **A** of the arc split out of [hornvale#4](https://github.com/hornvale/hornvale/issues/4); campaign **B** is the parked `2026-07-16-the-moons-design.md` (moon surfaces), which is blocked on this.
**Parent contracts:** `2026-07-14-the-long-count-design.md` (main-sequence brightening — this supplies the zero point it lacks), `2026-07-14-eclipse-seasons-design.md` (dates eclipses from moon inclination + node — **this campaign moves those**), decision 0009 (models author, dice roll), the Campaign 2 astronomy model card.
**This is an epoch.** See §7.

---

## 1. Problem

**The star has a clock with no zero point.** The Long Count shipped
main-sequence brightening — `brightening_per_gyr(star) = 0.10 · M^2.5`,
Sol-calibrated, scaled by the main-sequence lifetime `t_MS ∝ M^-2.5` — and
`luminosity_at(star, t)` anchored so that `t = 0` is genesis. The model
therefore knows how fast the star brightens and can integrate *forward*, but
it has no idea how far along the star already is. It reasons about `t_MS`
without ever placing the star on it.

**The moons have no origin story.** Each moon draws a mass, a distance, an
inclination (0–10°), and a node longitude, and those quantities float free of
each other. Nothing says *where the body came from* — and in reality that one
fact predicts almost all of them.

The immediate consequence, which is what surfaced this: The Moons (campaign B)
cannot honestly derive a moon's density, because the generator draws mass but
never composition. It was going to assume lunar density for every body in
every world. That assumption is a stand-in for a missing concept.

## 2. Goal

Draw the star's age. Derive the planet's. Draw each moon's **formation
mechanism**, and let it drive that moon's age, density, mass, distance, and
inclination — so those quantities stop floating free and start agreeing with
one another. Update the astronomy model card. Re-pin the eclipse batteries.

## 3. The containment rule (the most important decision in this spec)

**Age must not retroactively change genesis luminosity.**

`Star::luminosity` is derived `M^3.5` and `habitable_zone` derives from it;
orbit admission derives from the zone; climate derives from insolation. If age
were allowed to correct luminosity toward a proper ZAMS-plus-brightening
value, **every world's habitable zone, orbit, and climate would move** — an
epoch across the entire simulation, orders of magnitude beyond what this
campaign is for.

So: `luminosity` **remains the genesis-epoch value it already is**, and
`M^3.5` remains the declared approximation it already is. Age describes
**pre-genesis history only**: it says how long the star has been on the main
sequence before day 0. Anyone wanting the ZAMS luminosity can recover it
backward as `L / (1 + b·age)`; nothing in the sim consumes it today.

**The epoch is confined to the moons.** That is a deliberate boundary, and
this spec's §7 blast-radius analysis depends on it holding.

## 4. Stellar age

```
t_MS(M)  = 10 Gyr · M^-2.5          (already implicit in brightening_per_gyr)
T_MAX    = 15 Gyr                   (ratified — see below; NOT 13.8)
age      = U(0.05, 0.95) · min(t_MS(M), T_MAX)
```

drawn from a new stream, `star-age`. The `0.05–0.95` guard rails keep the star
off the pre-main-sequence and post-main-sequence edges, where none of the
model's physics applies.

Across the drawn star-mass span (0.6–1.4 M☉) the lifetimes are:

```
  0.6 Msun (K)  ->  t_MS = 35.9 Gyr    brightening  2.8 %/Gyr   (T_MAX binds)
  1.0 Msun (G)  ->  t_MS = 10.0 Gyr    brightening 10.0 %/Gyr   (t_MS binds)
  1.4 Msun (F)  ->  t_MS =  4.3 Gyr    brightening 23.2 %/Gyr   (t_MS binds)
```

**`T_MAX = 15 Gyr` (Nathan, ratified at G3). Note what 15 is not: it is not
13.8.** A 13.8 Gyr cap would have imported *our* cosmology wholesale — every
Hornvale world would then sit in a universe with our Big Bang. 15 Gyr is a
**bound without a cosmology**: it says no star here is older than 15 Gyr while
declining to say when the universe began. That keeps UNI-2 (metaphysics)
genuinely open, rather than settling it as a side effect of a stellar-age
formula. A generator constant should not quietly answer a metaphysical
question the project has deliberately left open.

The cap still buys its physics. A 0.6 M☉ K dwarf (t_MS 35.9 Gyr) is bounded at
~42% of its main-sequence life, so it has **necessarily brightened little** —
which is true of real K dwarfs, and *emerges* from the bound rather than being
asserted anywhere.

**Planet age** is *derived*: `planet_age = max(0, star_age − 0.05 Gyr)`.
Terrestrial planets finish accreting within ~30–100 Myr of their star, so the
difference is under 1% and far below anything the sim observes. It is modelled
only so the number exists and is honest about its own precision.

## 5. Moon formation mechanism

### 5.1 The mechanisms, and which ones a terrestrial world can have

Three real mechanisms (fission — Darwin's proposal that Earth spun off the
Moon — is discredited and is not modelled):

| | **Giant impact** | **Co-accretion** | **Capture** |
|---|---|---|---|
| exemplar | Luna | Galilean moons, Titan | Triton |
| age | coeval (−~50 Myr) | coeval | **decoupled** |
| mass | **large** rel. to primary | moderate, often several | usually small |
| density | **iron-poor ~3.3** | primary-like, graded | alien, often **icy ~1.5–2.0** |
| orbit | prograde, low inclination | regular, equatorial, resonant | **irregular: high inclination, often retrograde, distant** |

**Co-accretion is not modelled**, and the reason is physical rather than
budgetary: it needs a massive circumplanetary disk, which is a **giant-planet**
mechanism. Hornvale's anchor is a terrestrial world. Modelling it would be
modelling something these worlds do not have.

So v1 draws from `{GiantImpact, Capture}`.

### 5.2 What mechanism drives

This is the point of the campaign — these stop being independent draws:

| Quantity | GiantImpact | Capture |
|---|---|---|
| `age` | `planet_age − U(0.03, 0.10) Gyr` — coeval | **independent**: `U(0.05, 0.95) · planet_age` — the body formed elsewhere |
| `density` | 3.34 g cm⁻³ — **derived, not assumed**: re-accreted mantle debris, no iron core (this is exactly why Luna is 3.34 against Earth's 5.51) | drawn from `{rocky 3.0, icy 1.6}` — a different reservoir |
| `inclination` | `U(0, 10)°` — **the current formula, unchanged** | **`U(20, 160)°`** — irregular; **>90° is retrograde** |
| `mass`, `distance` | **not conditioned** — see §5.3 | **not conditioned** — see §5.3 |

**Radius** then follows from mass *and real density* (`r = (3M/4πρ)^{1/3}`)
rather than from mass and an assumption — which is the honesty upgrade
campaign B was blocked on.

### 5.3 The mechanism draw — after admission, and why that is *more* honest

**Amended at plan time (Nathan-ratified).** An earlier draft drew mechanism
*before* mass and distance and let it bias both, on the theory that causal
conditioning beats post-hoc conditioning. Reading `generate_moons` killed that
on two counts.

**The physics does not support conditioning mass.** The draft claimed captured
moons are small. **Triton falsifies it** — it is the seventh-largest moon in
the solar system and it is captured. Mass is not a capture signature.
**Inclination is**: a high, often retrograde inclination is the *defining*
signature of an irregular satellite. Conditioning mass would have asserted a
correlation the solar system contradicts.

**And the file already documents the right discipline.** `generate_moons`
draws count, mass, and distance in the admission loop, sorts by distance, and
*then* draws inclination and node, each from its own stream — with the reason
in a comment: "so every pre-eclipse draw (count, masses, distances) is
byte-identical and the draws are index-stable." SKY-6 and Eclipse Seasons both
added per-moon quantities this way.

So mechanism is drawn **per moon, after the distance sort, from its own new
stream `moon-formation`**, and is **weighted by distance** — which is both
physical (an impact child forms close and tidally recedes; irregular
satellites are distant) and free, since distance is already in hand.

The consequence is the campaign's best property:

```
  GiantImpact -> inclination = incl_stream.next_f64() * 10.0          <- IDENTICAL to today
  Capture     -> inclination = 20.0 + incl_stream.next_f64() * 140.0  <- differs
```

Same stream, same **single** draw per moon, so index-stability holds exactly as
SKY-6 established. Therefore:

- **Masses and distances never move, in any world.** *(Survives. This half of
  the property is intact and load-bearing.)*
- ~~**A world whose moons all draw `GiantImpact` is byte-identical to today** —
  same moons, same inclinations, same nodes, same eclipses.~~
  **SUPERSEDED at execution (ledger #14).** What survives is narrower and the
  distinction matters: an all-`GiantImpact` world's **inclinations** are
  byte-identical — the branch draws the same roll through the same formula —
  but its **eclipse dates are not**, because the eclipse-geometry correction
  (§7) changed how *any* inclination maps to an ecliptic latitude. **The
  property holds at the draw level and fails at the world level.**
- ~~Only worlds that actually receive a captured moon change.~~ **SUPERSEDED**:
  every world with **any** moon changes. Only **moonless** worlds are
  byte-identical.

~~This is a **partial epoch**, not a total one (§7), and it shrinks what the
census regeneration must cover.~~

**This is a TOTAL epoch** (§7). The partial-epoch design above was correct
about its own mechanism — the mechanism draw really is free — and was
falsified by something else entirely: the eclipse model's small-angle forms
were only ever valid for the `[0, 10)` inclinations that existed before this
campaign, and correcting them perturbs low-inclination moons too. The deeper
reading, which is the campaign's own lesson: **the partial-epoch property was
never a property of the world — it was a property of an approximation error.**
All-impact worlds were byte-identical only because a wrong formula was applied
uniformly to them. The cheapness was an artifact of the inaccuracy, and it was
always going to be repaid.

## 6. The model card delta

**Derived (real formulas):** planet age; moon age for the coeval mechanism;
radius from mass and density; the ZAMS-luminosity back-derivation (§3) —
recoverable by inverting `luminosity_at`, not a function the sim calls;
nothing consumes it today.

**Derived — added at execution (ledger #14): the exact eclipse geometry.** A
moon's ecliptic latitude is now the exact spherical form

```
  β = asin(sin i · sin u)          (u = argument of latitude, i = inclination)
```

replacing the small-angle `β = i·sin(u)` in `moon_ecliptic_latitude_deg`,
`node_crossing_chance` (whose statistical twin becomes
`(2/π)·asin(sin(threshold)/sin(i))`), and `series_returns`. It is bounded by
`±min(i, 180−i)` for any inclination, and it restores the symmetry the linear
form silently broke: orbits at `i` and `180−i` eclipse identically, as
`sin i = sin(180−i)` requires. **Note the direction of travel — a quantity
moved from *approximated* toward *derived*.** That is rare on this model card,
and it is the reason the epoch became total (§7).

**Drawn:** star age (guard-railed fraction of `t_MS`); formation mechanism per
moon; capture-mechanism density class, age, mass, distance, inclination.

**Approximated (declared):**
- `t_MS = 10 Gyr · M^-2.5` is the existing Sol-calibrated scaling, not a
  stellar-structure model.
- `L = M^3.5` **stays the genesis-epoch approximation it already is** (§3).
  Age does not correct it. This is a deliberate containment, and the spec says
  so rather than pretending the mass–luminosity relation is age-aware.
- The mechanism weighting (§5.3) is a plausibility rule, not a population
  synthesis of satellite formation. **Calibrated at execution (ledger #12) to**

  ```
    p_capture = clamp(frac³, 0.02, 0.85)      (frac = distance as a fraction
                                               of the admitted range)
  ```

  It encodes only the physical intuition that an impact child forms close and
  tidally recedes while irregular satellites are distant — but it now carries
  what a plausibility rule usually lacks: **an empirical anchor, and therefore
  a falsifier.** The rule must classify the model's own exemplar correctly.
  Luna sits at 384.4 Mm from Earth (`frac ≈ 0.386`); under the originally
  planned **linear** map, `clamp(frac, 0.10, 0.85)`, the real Earth–Moon
  system — the `GiantImpact` variant's own namesake — read as a **capture 39%
  of the time**. Cubing suppresses the middle of the range hard
  (`0.386³ ≈ 0.057`) while pinning both ends, and Luna reads as an impact
  child **94%** of the time. This paragraph previously called the weighting
  unfalsifiable by construction; that is no longer true, and the upgrade is
  the point.
- Densities are two representative classes (rocky/icy), not a composition
  model.
- **Capture is modelled as an outcome, not an event.** No encounter dynamics,
  no binary-exchange capture, no post-capture orbital evolution — the sim
  draws that the body *was* captured and gives it irregular-satellite
  statistics. A real capture requires energy dissipation the model does not
  simulate.

## 7. The epoch, and its blast radius

**This is a TOTAL epoch.** *(Corrected at close. This section shipped claiming
a PARTIAL epoch; ledger #14 supersedes it. The original claim and the reason it
failed are both kept below, because the failure is the more instructive half.)*

The partial-epoch design was right about the mechanism draw and wrong about the
model it fed. `inclination_deg` does indeed change **only** for a moon drawn as
`Capture`; masses and distances never move; a `GiantImpact` moon's inclination
uses the identical formula off the identical stream draw. All of that survives.
What was not foreseen is that **the eclipse model could not accept the
inclinations this campaign introduced.** `moon_ecliptic_latitude_deg` and
`node_crossing_chance` were linear in `i` — small-angle forms, declared as such
in their own doc comments, written when `inclination_deg` was always `[0, 10)`.
At `i = 160°` the old form claimed an ecliptic latitude of **160°**, which is
geometrically impossible (the maximum is 90°), and it made orbits at `i` and
`180−i` differ by **8×** where physics demands they eclipse identically.
Correcting it (§6) is unconditional, and the exact form perturbs
**low-inclination moons too** — by up to **0.0185°**, four orders of magnitude
above the 8-significant-digit quantization floor. So:

- A **moonless** world: **byte-identical.** *(Survives — 0 of 155 moonless
  seeds moved. This is the only surviving byte-identity claim at the world
  level.)*
- ~~A world whose moons **all** draw `GiantImpact`: byte-identical.~~
  **SUPERSEDED.** Its moons' **inclinations** are byte-identical — the property
  holds at the **draw** level — but its **eclipse dates** are not. It fails at
  the **world** level. **260 of 627** all-impact-or-moonless seeds moved from
  the geometry correction alone.
- A world with **any** captured moon: that moon's inclination moves, and its
  eclipses with it. *(Survives, and saturates: all 373 such worlds move.)*

~~The blast radius is therefore *a fraction of seeds*, not all of them.~~
**MEASURED (Task 6), against the 1000-seed census set, release-mode, real
`windows/lab` eclipse-cadence metrics:**

```
  633 / 1000 seeds move
    = 373  from the inclination epoch (worlds receiving a captured moon)
    + 260  from the eclipse-geometry correction ALONE
             (of 627 all-impact-or-moonless seeds; 0/155 moonless moved)
```

Triangulated three ways against a `git revert`-built isolation variant (epoch
present, geometry fix reverted): the variant is identical to the baseline on
all 627 — confirming the inclination epoch alone really is a no-op there — and
differs from HEAD on exactly those 260. The arithmetic closes: 373 + 260 = 633.

**Every prior estimate undercounted, including this spec's own instinct that
the correction might cost nothing.** The a-priori argument was that the
correction's O(i³) term shifts small-`i` moons by `<1e-9°`, below the
quantization floor, so the "total" epoch might be free in practice. It is not:
a systematic ~0.005–0.01° shift applied across thousands of threshold checks in
a 100-year scan flips a majority of the low-inclination population's day-level
counts. The error in the reasoning was generalising from the curve's **peak**
(`u = 90°`, where the exact and small-angle forms agree *identically*) to the
whole curve. **Task 6 existed precisely because nobody had the number** — §7
said so — **and it earned its place by overturning the estimate of the person
who commissioned it.**

Known blast radius:

1. **Eclipses move.** Eclipse Seasons dates real eclipses from inclination +
   node. A retrograde or 40°-inclination moon eclipses on a completely
   different cadence — often *never*. The dated-eclipse batteries re-pin.
2. **The census moves.** `windows/lab/src/metrics.rs` runs a 100-year
   dated-eclipse scan (`scan_century`, `century_cadence`) for its cadence
   metrics. Those rows change ⇒ **an AWS census regeneration is required**, and
   that is an explicit-authorization carve-out (§9.4).
3. **Climate does NOT move** — by the §3 containment rule. If it does, the
   containment leaked and the campaign has become something else; that is a
   stop-and-report condition, not a re-baseline. *(Verified: it held. §8's
   containment battery passes, and it was mutation-tested — forcing `age` into
   `luminosity` makes it fail, so its passing means something.)*
4. **The pantheon may move on mooned seeds.** Eclipse Seasons noted that
   pantheons re-derive on mooned seeds. Anything keyed to eclipse phenomena is
   downstream.
5. **`scene/moons/v1` moves** — *added at close (ledger #11); this spec could
   not have listed it, because the contract did not exist when the spec was
   written.* Absorbing main pulled in **The Faces**, which shipped
   `scene/moons/v1` deriving `radius_km = 1737.4·mass^(1/3)` and
   `surface_gravity_ms2` **at an assumed constant lunar density (3.34)** — the
   exact assumption this campaign exists to retire. Task 5b unified it onto the
   real density rather than leaving the repo with **two radius answers for one
   quantity**. Consequences: the `scene/moons/seed-42` golden re-pins;
   `bright-icy` keys off real density instead of a hash-derived albedo; the
   reference page's constant-density caveat retires; and **the orrery — a
   released consumer — renders changed radii for captured moons** (an icy body
   at ρ=1.6 is ~28% larger than scene previously reported).
6. **The eclipse-geometry correction itself** — *added at close (ledger #14).*
   Not a downstream consequence of the epoch but a **cause** of it, and the
   larger cause at that: **260 of the 633 moved seeds are its alone**, worlds
   the epoch proper never touches. Any consumer reading
   `moon_ecliptic_latitude_deg`, `node_crossing_chance`, or `series_returns` —
   the almanac among them — moves with it.

**Epoch discipline** (CLAUDE.md): deliberate regeneration takes an epoch
suffix, never a rename. The affected stream labels get `/v2` suffixes rather
than new names, so an old save's derivation is still legible.

## 8. Testing

- **Stellar age**: within the guard rails; never exceeds `t_MS` (nor `T_MAX`);
  a 0.6 M☉ star's brightening-since-ZAMS stays small (§4's emergent claim,
  asserted).
- **Containment (the spec's most important test):** for a battery of seeds,
  `star.luminosity`, `habitable_zone`, and the anchor's admitted orbit are
  **byte-identical to pre-campaign**. This is what proves the epoch stayed in
  the moons. It should be written first and watched hardest.
- **Mechanism conditioning**: giant-impact moons are prograde and low-
  inclination; captured moons land in the irregular band and ~half are
  retrograde; densities fall in their declared classes; radius matches
  `(3M/4πρ)^{1/3}`.
- **Coevality**: giant-impact moon age is within 100 Myr of planet age;
  captured moon age is decoupled (a distribution test across seeds, not a
  per-seed assertion).
- **Pin isolation** (`domains/astronomy/tests/genesis_properties.rs`): a
  `--moons` pinned world consumes the same draws as the unpinned path — the
  existing battery, extended for the new streams.
- **Determinism**: same seed ⇒ same mechanisms, ages, densities, byte-identical.
- **Re-pins in the drifting commit**, never deferred to the close: the eclipse
  goldens move in the commit that moves them.

## 9. G3 outcome (ratified 2026-07-16)

1. **`T_MAX` = 15 Gyr — RATIFIED.** The question was never stellar physics: a
   0.6 M☉ K dwarf's `t_MS` is 35.9 Gyr, longer than our universe has existed,
   so *any* answer commits the project to a cosmology. 15 Gyr bounds without
   committing (§4). UNI-2 stays open.
2. **Full epoch, including `Capture` — RATIFIED.** The cheap variant (ages +
   giant-impact only, in which the epoch mostly evaporates — inclination stops
   moving, the eclipse batteries mostly hold, likely no regen) was offered and
   **declined**, with the cost understood. Nathan's rationale, which belongs in
   the record because it is the campaign's actual purpose: *"part of the point
   here is 'to explore strange new worlds'… this can contribute to making
   worlds that are more expressive and more interesting."* The thing that makes
   this expensive — capture, and the irregular, retrograde, high-inclination
   moons it admits — **is the deliverable**. Uniformity was never the goal.
3. **Epoch + save-format contract (stands, by carve-out rule).** New streams:
   `star-age`, `moon-formation`. Changed derivation for the existing per-moon
   draws ⇒ epoch suffixes (`/v2`), not renames. Every seeded world's moons
   change; every world's climate does not (§3).
4. **AWS census regeneration — RESOLVED AT CLOSE: DECLINED. The staleness is
   accepted debt.** *(This section shipped reading "STILL AN OPEN CARVE-OUT …
   the one thing that can still block the close." It did not block the close;
   it was answered. Recorded here rather than deleted, because both halves of
   the question resolved in ways the spec did not predict.)*

   - **The spend: Nathan declined the regen.** Authorising the epoch was never
     authorising the spend, and the two decisions came apart exactly as this
     section anticipated they might. The campaign closes with the census
     fixtures (`book/src/laboratory/generated/*/rows.csv`) **knowingly stale
     for 633 of 1000 seeds**, and main red on the census tests until a future
     campaign batches that spend. **This is the authorized state, not an
     oversight** — it is the project's standing tolerated-lag policy (CLAUDE.md:
     "that lag is the chosen trade") applied to a measured number rather than an
     estimated one.
   - **The sequencing: answered by events, not by adjudication.** "Two epochs,
     one census budget, and the sequencing between them is unresolved" —
     **rift-and-fit's regen already ran** (`945f62b`), absorbed into this branch
     with main's 68 commits. Terrain went first. The question this spec flagged
     as potentially close-blocking dissolved without anyone having to rule on
     it, which is worth recording precisely because the spec was right that it
     was a real contention and wrong about how it would end.
   - **The honest input the spec asked for now exists.** §7 said the affected
     fraction "must be measured … and nobody has it yet." It is **633/1000**
     (373 + 260; see §7). That number, not an estimate, is what a future regen
     campaign inherits.

### 9.1 Registry context (scanned 2026-07-16)

The frontier registry already banks much of this neighbourhood, and the spec
should cross-link rather than restate: `SKY-22` **retrograde rotation is
already shipped** (`Rotation::Spinning` carries a `retrograde: bool`) — moon
retrogradation here is the *satellite* case, not the anchor's. `SKY-rings`
banks ring systems *and* their Roche-limit origin, which is where a
disintegrating moon would land — adjacent to this campaign's Roche/Hill
bounds, deliberately out of scope. This campaign's own products are the newly
filed `SKY-minimoon` (temporary capture — the honest sibling of §5's permanent
capture) and `SKY-quasi-satellite`.
