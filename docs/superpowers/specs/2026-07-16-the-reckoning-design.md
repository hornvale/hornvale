# The Reckoning — Design

**Date:** 2026-07-16
**Status:** G3 RATIFIED 2026-07-16 (T_MAX = 15 Gyr; full epoch incl. Capture). Planning next. The AWS census regen remains an open carve-out — see §9.4.
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

- **Masses and distances never move, in any world.**
- **A world whose moons all draw `GiantImpact` is byte-identical to today** —
  same moons, same inclinations, same nodes, same eclipses.
- Only worlds that actually receive a captured moon change.

This is a **partial epoch**, not a total one (§7), and it shrinks what the
census regeneration must cover.

## 6. The model card delta

**Derived (real formulas):** planet age; moon age for the coeval mechanism;
radius from mass and density; the ZAMS-luminosity back-derivation (§3) —
recoverable by inverting `luminosity_at`, not a function the sim calls;
nothing consumes it today.

**Drawn:** star age (guard-railed fraction of `t_MS`); formation mechanism per
moon; capture-mechanism density class, age, mass, distance, inclination.

**Approximated (declared):**
- `t_MS = 10 Gyr · M^-2.5` is the existing Sol-calibrated scaling, not a
  stellar-structure model.
- `L = M^3.5` **stays the genesis-epoch approximation it already is** (§3).
  Age does not correct it. This is a deliberate containment, and the spec says
  so rather than pretending the mass–luminosity relation is age-aware.
- The mechanism weighting (§5.3) is a plausibility rule, not a population
  synthesis of satellite formation.
- Densities are two representative classes (rocky/icy), not a composition
  model.
- **Capture is modelled as an outcome, not an event.** No encounter dynamics,
  no binary-exchange capture, no post-capture orbital evolution — the sim
  draws that the body *was* captured and gives it irregular-satellite
  statistics. A real capture requires energy dissipation the model does not
  simulate.

## 7. The epoch, and its blast radius

**This is a PARTIAL epoch** (narrowed by §5.3's amendment). `inclination_deg`
changes **only** for a moon drawn as `Capture`. Masses and distances never
move; a `GiantImpact` moon's inclination uses the identical formula off the
identical stream draw. So:

- A **moonless** world: byte-identical.
- A world whose moons **all** draw `GiantImpact`: byte-identical.
- A world with **any** captured moon: that moon's inclination moves, and its
  eclipses with it.

The blast radius is therefore *a fraction of seeds*, not all of them — which
is what §5.3 bought. **The affected fraction is measurable before merge and
must be measured** (Task 6): run the mechanism draw across the census seed set
and report what share of worlds move. That number is the honest input to the
regen conversation, and nobody has it yet.

Known blast radius:

1. **Eclipses move.** Eclipse Seasons dates real eclipses from inclination +
   node. A retrograde or 40°-inclination moon eclipses on a completely
   different cadence — often *never*. The dated-eclipse batteries re-pin.
2. **The census moves.** `windows/lab/src/metrics.rs` runs a 100-year
   dated-eclipse scan (`scan_century`, `century_cadence`) for its cadence
   metrics. Those rows change ⇒ **an AWS census regeneration is required**, and
   that is an explicit-authorization carve-out (§9.3).
3. **Climate does NOT move** — by the §3 containment rule. If it does, the
   containment leaked and the campaign has become something else; that is a
   stop-and-report condition, not a re-baseline.
4. **The pantheon may move on mooned seeds.** Eclipse Seasons noted that
   pantheons re-derive on mooned seeds. Anything keyed to eclipse phenomena is
   downstream.

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
4. **AWS census regeneration — STILL AN OPEN CARVE-OUT.** Authorising the epoch
   is not authorising the spend. The eclipse-cadence metrics move, so a regen
   is required *at merge*, and it needs explicit authorization at the point of
   running it. **rift-and-fit is a terrain epoch v4 already at G3 with its own
   census-sequencing flag** — two epochs, one census budget, and the sequencing
   between them is unresolved. This is the one thing that can still block the
   close.

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
