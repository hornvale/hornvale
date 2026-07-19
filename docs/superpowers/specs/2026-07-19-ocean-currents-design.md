# Ocean Currents + Wind Vector Fields (The Gyre) — design

**Date:** 2026-07-19 · **Status:** SHIPPED (2026-07-19) — ocean-current vector field + Living-Globe advection; see [chronicle](../../../book/src/chronicle/the-gyre.md).
**Program:** Weather Program campaign 2 (after The Turning's diurnal temperature).
**Sibling:** The Turning (an additive, byte-identical `scene/tiles/v1` layer +
Living-Globe payoff); this follows the same shape for a vector field.

## 0. Program context — where this sits, and the level it climbs

The Weather Program is: diurnal (shipped) → **wind + ocean-current vector
fields (this spec)** → precipitation → drawn storms, governed by the Lorenz
guard-rail (deterministic fields + drawn events, never a forward-integrated
storm engine; the client advects for the felt motion — MAP-23, 0022/0023).

An ideonomy pass (abstraction-lift + cross-domain; axes hierarchicalness /
cyclicity / source) found that a flow field sits at one of three coupling
levels, and that **cyclicity**, not "new fields", is the real trigger for the
`CLIM-operators` refactor:

```
level  what the field does                       cyclic?  moves the mean?  needs CLIM-operators?
-----  ---------------------------------------    -------  ---------------  ---------------------
 0     drawn/derived, rendered as itself          no       no               no
 1     advects a passive tracer, ONE PASS         no       no (diagnostic)  no
 2     advection FEEDS BACK, relaxed to a         YES      YES              YES
       fixed point (coastal SST moderation)
```

**This campaign is level 0.** Level 1 (a "current-warmed coast" diagnostic the
biome field ignores) is a **grounding trap** — the Faces/Reckoning failure
class (a rendered field the model's own classification does not believe), so
it is rejected, not chosen. Level 2 (grounded SST moderation) moves
`mean_temperature` → biomes/census → it is a **later, deliberate epoch
campaign** and carries `CLIM-operators` + a relaxation solver. The refactor
follows the *loop*, not the *field count* — correcting The Turning's spec §7,
which pencilled the refactor into "the winds campaign": vector fields alone
do not earn it.

## 1. The ocean-current model

A per-cell surface-current **tangent vector**, zero on land, derived one-pass
from the band winds the producer already computes — the same altitude as the
banded-wind heuristic, not a dynamical solve.

```
current(cell) = coast_project( ekman_deflect( prevailing_wind(cell), hemisphere ), cell )   // ocean only
```

- **Ekman deflection.** Surface water moves at a Coriolis-signed angle to the
  wind — to the **right** in the northern hemisphere, **left** in the
  southern. Rotating the alternating band winds (equatorial easterlies,
  mid-latitude westerlies) by that angle turns the zonal belts into the
  rotational structure of the **subtropical gyres**: anticyclonic (clockwise)
  in the north, the mirror in the south. This is "gyres from band winds ×
  Coriolis".
- **Coastline deflection.** A current cannot flow into land, so where an ocean
  cell borders a coast its vector is **projected onto the local shore
  tangent** — the flow turns to run along the coast, producing boundary
  currents. Uses the neighbor topology + ocean mask the geosphere/terrain
  already carry.
- **Zero on land; requires bands.** Land cells carry `[0,0,0]`. A tidally
  locked world (no circulation bands — `circulation.rs`) has a zero current
  field, mirroring how the wind overlay is `null` on locked worlds.
- **One-pass, derived, no relaxation.** Like drainage and the rain-shadow
  trace — a single computation over the geosphere, no iteration to a fixed
  point, no stream draws.

**Deferred fidelity (each with a home; Nathan's call to pull forward):** this
is a *kinematic* gyre heuristic (right qualitative circulation at the
banded-wind altitude). A full **Sverdrup/streamfunction** solve (perfectly
closed gyres, quantified western intensification) is deferred — it borders the
relaxation solver the feedback (level-2) campaign owns. Currents stay **static**
(seasonal reversal is `CLIM-monsoon`).

## 2. Producer contract (Approach A — producer owns the current field)

The producer ships only `circulation_bands` today, and the orrery
*reconstructs* the zonal wind arrows client-side (`windAt`, decision 0022 —
"the sim ships a band count, not arrows"). Winds stay that way. The **current
field is new, coastline-global, and identity-bearing**, so the producer owns
and emits it (Approach A; B — client-reconstructs — was rejected as duplicating
global physics client-side; C — a new `scene/flow/v1` schema — as heavier than
one tiles layer for the one field that is new).

- **`domains/climate/src/currents.rs`** (new): `ocean_current(geo, is_ocean:
  impl Fn(CellId) -> bool, cell, bands) -> [f64; 3]` (tangent vector, zero on
  land) + `ocean_current_field(...) -> CellMap<[f64; 3]>`. Reads
  `prevailing_wind` (circulation.rs); applies the hemisphere-signed Ekman
  rotation; projects along the coast using neighbor topology + the ocean mask.
- **Provider (`provider.rs`)** precomputes the field once (like the diurnal
  amplitude) and exposes `current_at(cell) -> [f64; 3]`.
- **`scene/tiles/v1` gains two additive per-cell fields — `current_east` and
  `current_north`** (the tangent-frame components, quantized at emit — decision
  0033; zero on land). Two components so the client has direction and
  magnitude to advect. Additive-within-v1, byte-pin re-pinned in the same
  commit; **no epoch**. No new wasm export (rides `hw_scene_tiles`);
  **world-wasm-v9** carries the richer scene.
- **Almanac (small):** a one-line "the seas" note at a coastal sample site —
  the direction of the dominant offshore current. Light, text-side.

## 3. The orrery consumer (`the-gyre` branch)

- `parseTiles` (`src/sim/scene.ts`) adds `currentEast` / `currentNorth`
  (strict, quantized, house style — the `t_diurnal_amp_c` pattern).
- A **currents toggle** in the HUD (`src/ui/hud.ts`), beside "winds".
- **Advection overlay** (`src/views/currents.ts`, sibling to `winds.ts`):
  particles seeded over ocean cells and swept along the current field, faded
  and re-seeded — the Living Globe pattern (MAP-23). Client-side, explicitly
  **non-deterministic eyecandy** (0022/0023); never touches world identity.
  Reuses the ocean mask to stay off land; null on locked worlds.
- **No committed golden.** The client *reads* the current vectors and renders
  them (recomputes nothing), so the vendored-binary fixture test is the whole
  contract; a golden would only drift (the parse-vs-recompute rule).
- **Re-pin:** `wasm:release` → **world-wasm-v9** at G6.

## 4. Determinism and safety

- **Byte-identical** worlds/ledger/census: the current field is a pure derived
  read that touches no temperature (level 0), so `mean_temperature`, biomes,
  and the census are unchanged. No epoch, no new draws, no reordered labels.
- `scene/tiles/v1` gains two fields → its byte-pin fixture
  `tiles-seed-1-w16.json` **re-pins in the same commit** (additive precedent:
  The Turning's `t_diurnal_amp_c`, The Isotherm's `t_mean_c`/`t_swing_c`). Not
  a v2 epoch.
- Quantize at emit only; all trig via `kernel::math` (0041). The Lorenz
  guard-rail is not in play (one-pass derived field, no integrator).
- Layering: `domains/climate` reads only the kernel (geosphere/terrain via the
  composition root's ocean predicate); no cross-domain dependency, no
  `DOM-ocean` crate (contested, deferred).

## 5. Testing

Producer property tests (mutation-verified; the physics questions lead):
- **Zero on land** — every land cell's current is exactly `[0,0,0]`.
- **Tangent + nonzero** — orthogonal to the cell position (a surface vector
  field); nonzero over open ocean.
- **Hemisphere-correct rotation** — Ekman turns the wind right (N) / left (S)
  so gyres circulate clockwise (N) / counter-clockwise (S). Mutation-verify:
  flip the sign → this test fails (the "gyres backwards" guard).
- **Coast tangency** — an ocean cell bordering land has its into-land component
  suppressed relative to open water.
- **Locked → empty** — a locked world (no bands) has a zero current field.
- **Determinism** — byte-identical rebuild; trig via `kernel::math`.

Scene & byte-identity: schema + length tests for the two fields; the byte-pin
re-pinned additive; frozen seed-42 world + census byte-identical.

Client: `parseTiles` field test; the currents overlay builds over ocean, null
on locked; **visual verification (The Lens rule)** — render a spinning seed,
toggle currents, confirm gyres rotate correctly, hug coasts, stay on water,
vanish when locked. Open the PNGs (a hemisphere-sign error passes every unit
test and only shows as backwards gyres).

## 6. Non-goals (the fence — each deferred with a home)

- **No SST feedback / coastal temperature moderation** — level 2, a later
  *epoch* campaign; currents here advect nothing.
- **No `CLIM-operators` / kernel Field-trait refactor** — its trigger is the
  feedback loop (cyclicity), not vector fields.
- **No full Sverdrup/streamfunction gyre solve** — kinematic heuristic only.
- **No time-aware / seasonal currents** (`CLIM-monsoon`); **winds unchanged**
  (still client-reconstructed zonal band arrows; not re-emitted, not made
  time-aware).
- **No `DOM-ocean` crate** — currents stay a climate field.

## 7. Flagged for G3

1. **Save-format (leads):** additive `current_east`/`current_north` on
   `scene/tiles/v1`. **No epoch** — worlds/ledger/census byte-identical (level
   0 touches no temperature); the scene byte-pin **re-pins within v1** (the
   `t_diurnal_amp_c` precedent). Confirming this reading is the main ask.
2. **world-wasm-v9 release + orrery re-pin + origin pushes** — G6 carve-outs.
3. **Fidelity:** the kinematic gyre heuristic (Ekman + coast) vs a full
   Sverdrup solve, and static-vs-seasonal currents — fidelity cuts are yours;
   flag any to pull forward.
4. **The level-0-vs-level-2 cleave:** confirm the SST feedback (coastal
   moderation) is correctly deferred to its own epoch campaign, with
   `CLIM-operators` moving there, and that level 1 (a diagnostic-only warmed
   coast) is correctly rejected as ungrounded.

## 8. Decisions (promoted from the campaign ledger)

- **Level 0, vector fields only** — currents drawn/advected, no SST feedback.
  Byte-identical, no epoch, no census/AWS. Ideonomy: abstraction-lift +
  cross-domain (list; hierarchicalness/cyclicity/source), 1 pass — reframed the
  binary fork into a 3-level hierarchy, unmasked level 1 as a grounding trap
  (Faces/Reckoning), and identified cyclicity (not field count) as the
  `CLIM-operators` trigger. 0 overturns of "vector fields only".
- **Approach A** — producer computes + emits the current field as a
  `scene/tiles/v1` layer (coastline-global, identity-bearing); winds stay
  client-reconstructed. B/C rejected.
- **Defer `CLIM-operators` + the Field-trait refactor + the SST feedback**
  together to the level-2 feedback campaign; correct The Turning spec §7.
- **Kinematic gyre heuristic** (Ekman-deflected band winds, coast-projected,
  one-pass) — the banded-wind altitude; full Sverdrup deferred.
- **The current field is parsed, not recomputed, client-side → no committed
  golden** (the wasm fixture test is the contract).
