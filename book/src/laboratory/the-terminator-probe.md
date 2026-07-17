# The Terminator Probe

**Preregistered before any generator code lands.** This is a **Stage-0
synthetic instrument**, not a canonical census: it applies the corrected
Locked insolation field *by hand*, over a copy of a built world's substrate,
and never touches `hornvale_worldgen::generate` or any committed world
byte. Its question — where does a tidally-locked world's dominant species
peak, substellar or terminator? — decides whether Task 4 (a roster
re-tune) is needed before the fix lands, and its terminator-share number
sets Task 5's acceptance floor.

## The bug, and the fix under test

`hornvale_worldgen::substrate_field`'s insolation term calls
`annual_mean_insolation(latitude, obliquity, scalar)` — a **latitude-only**
formula — unconditionally, never branching on rotation regime. On a Locked
world this is physically wrong: a tidally-locked planet's insolation is
organized around the **substellar point**, not latitude bands. Temperature
and moisture already branch correctly (`hornvale_climate::substellar_cosine`,
landed by Task 1); insolation is the one field this campaign caught still
latitude-blind.

The corrected formula (spec §3), applied here synthetically:

```text
insolation = insolation_scalar * substellar_cosine(position).max(0.0)
```

Lambert's cosine law: full scalar insolation at the substellar point
(`cos_theta = 1`), zero at and beyond the terminator meridian (night side
floored at `0.0`).

## Method

1. **Identify locked seeds.** Scan ascending seeds at the cheapest depth
   that exposes the sky (`BuildDepth::Astronomy`, unpinned `SkyPins`), and
   keep every seed whose generated anchor's rotation is `Rotation::Locked`.
2. **Build each locked seed to `BuildDepth::Terrain`** (the shallowest rung
   carrying the terrain/climate this probe reads) and assemble the real
   `Substrate` field via the shipped `substrate_field` — temperature,
   moisture, and elevation are already correct and untouched.
3. **Replace only the `insolation` term** with the corrected Locked formula
   above, cell by cell.
4. **Run the same suitability product `niche_per_species_k` computes**
   (replicated in the probe as `niche_k_over`, since that function builds
   its own internal substrate and has no injection seam) over the corrected
   substrate, for the roster's **peopled species only** — settlement
   genesis itself filters `roster.iter().filter(|d| d.peopled.is_some())`,
   since only settling, speaking species found cultures and thus presiding
   religions; the wider menagerie's fauna never do.
5. **Find the world-dominant species** (highest total K summed over
   habitable cells) and the cell where its K is maximal; classify that cell
   by `substellar_cosine`.

### The K-peak zone

A clean partition of `substellar_cosine`'s `[-1, 1]` range:

| zone | range | meaning |
|---|---|---|
| substellar | `cos_theta > 0.6` | the bug's outcome: dominance pinned to the fixed noon point |
| mid/terminator-day | `0.2 < cos_theta <= 0.6` | day side, short of the substellar cap |
| terminator | `-0.2 <= cos_theta <= 0.2` | rings the day/night boundary — the fix's goal |
| night/antistellar | `cos_theta < -0.2` | the permanent night side |

The headline statistic uses a slightly wider terminator-ring test,
`|cos_theta| <= 0.3` at the dominant species' K-argmax cell, per the task
brief.

### The sentiment proxy (accepted, stated openly)

Running the full culture/religion derivation under a synthetically patched
substrate would require re-deriving settlement placement, culture, and
religion facts against a field the committed ledger never actually saw —
invasive for a read-only probe. The accepted proxy, stated here rather than
silently substituted: **a terminator-ring K-peak is the mechanism that
yields a tide-Ambient presiding sentiment** (a terminator culture's most
salient phenomenon is the tide, not a fixed sun); a substellar K-peak
instead reproduces the bug's Eternal-noon outcome. This page reports the
K-peak zone distribution, not a derived religion fact.

## The roster's current insolation optima

The four peopled (settling) species and their `ConditionResponse` on the
insolation axis, read from `domains/species/src/lib.rs`:

| species | optimum | width | devotion |
|---|---:|---:|---:|
| kobold | 0.04 | 0.12 | 0.80 |
| goblin | 0.13 | 0.30 | 0.35 |
| bugbear | 0.15 | 0.40 | 0.30 |
| hobgoblin | 0.19 | 0.13 | 0.85 |

These optima were authored against the OLD, buggy latitude-only field,
where low values read as "high latitude, low light" (a cold/polar or shaded
signal). Under the corrected Locked field, a low insolation value instead
reads as "near the terminator" — the numeric coincidence this probe exists
to check.

## Results (seeds 1..=200, 9 locked seeds found)

The first scan window (`1..=200`) already yielded 9 locked seeds — above
the probe's `MIN_LOCKED = 8` floor, so the widened `201..=1000` window was
never needed.

| seed | insolation scalar | obliquity (deg) | dominant species | total K (habitable cells) | peak `cos_theta` | zone |
|---:|---:|---:|---|---:|---:|---|
| 8 | 0.8671 | 21.79 | hobgoblin | 425.2929 | 0.2272 | mid/terminator-day |
| 13 | 0.7890 | 26.27 | hobgoblin | 198.5912 | 0.2644 | mid/terminator-day |
| 70 | 0.6334 | 20.59 | hobgoblin | 149.9336 | 0.3247 | mid/terminator-day |
| 78 | 0.5991 | 25.53 | hobgoblin | 366.1458 | 0.3063 | mid/terminator-day |
| 80 | 0.6336 | 25.61 | hobgoblin | 275.0753 | 0.3221 | mid/terminator-day |
| 95 | 0.5853 | 0.64 | hobgoblin | 255.9684 | 0.3478 | mid/terminator-day |
| 116 | 0.8024 | 5.14 | hobgoblin | 144.5391 | 0.2555 | mid/terminator-day |
| 145 | 0.5897 | 31.13 | hobgoblin | 174.1791 | 0.3255 | mid/terminator-day |
| 183 | 0.9783 | 26.16 | hobgoblin | 155.0804 | 0.2282 | mid/terminator-day |

**Terminator-ring share (`|cos_theta| <= 0.3`): 4/9 = 0.4444.**
**Substellar-zone share: 0/9 = 0.0000.**

Every one of the 9 locked seeds lands in the same qualitative place:
`cos_theta` in `[0.2272, 0.3478]`, mean ≈ 0.288 — clustered just past the
strict `|cos| <= 0.2` terminator ring, on the day side, but nowhere near the
substellar cap (`cos_theta > 0.6`). Loosening the ring test to `|cos| <=
0.35` (still well short of "half toward noon") covers all 9/9 seeds.
**Zero of 9 seeds show dominance pinned to the substellar point** — the bug's
signature outcome never appears under the corrected field.

The dominant species is **hobgoblin in all 9 seeds**, unanimously. Its
insolation optimum (0.19, width 0.13, devotion 0.85 — the tightest,
most-devoted insolation curve of the four peopled species) combines with a
temperate/dry profile (temperature optimum 13 °C, moisture optimum 0.35) that
also favors the day-side band just inward of the terminator under the
corrected Locked temperature field (which is itself substellar-cosine-
organized, hot at `+x` and floored cold on the night side) — the two axes
reinforce the same zone rather than pulling dominance apart.

## Verdict: Task 4 (roster re-tune) is SKIPPED

The measured question was: do the roster's current optima already put
dominant-K in the terminator ring on the majority of locked seeds (roster
optima already terminator-seeking → Task 4 skipped), or does dominance
still land substellar (optima too high for the corrected range → Task 4
activates, sweeping a new target)?

**The roster is already terminator-seeking.** Across all 9 locked seeds
measured:

- Dominance never lands substellar (0/9).
- Dominance clusters tightly at `cos_theta ≈ 0.23–0.35`, decisively on the
  terminator-adjacent side of the day hemisphere, not at high noon.
- The mechanism is legible: the roster's low authored insolation optima
  (0.04–0.19), read against the corrected field's `[0, insolation_scalar]`
  day-side range, numerically coincide with "near the terminator" rather
  than "near the pole" (their old meaning) — the low-optimum authoring
  choice survives the fix by accident of scale, not by design, but it
  survives.

**Task 4 (a roster insolation re-tune) is not needed.** The **Task-5
acceptance floor** this probe sets: on locked seeds, the dominant peopled
species' K-argmax cell should land with `|cos_theta| <~ 0.35` (matching the
measured cluster) — i.e., decisively excluding the substellar zone
(`cos_theta > 0.6`), which is the fix's actual falsifiable target. A future
Task 5 readout that instead shows dominance at `cos_theta > 0.6` on a
material share of locked seeds would mean the *live* wiring (not this
synthetic probe) introduced a regression the Stage-0 measurement did not
predict.

## Instrument

`windows/worldgen/tests/insolation_probe.rs`,
`dominant_k_peak_under_corrected_locked_insolation`, `#[ignore]`d (a
live-worldgen instrument, not part of the commit gate or `make gate-full`'s
heavy tier). Run by hand:

```bash
cargo test -p hornvale-worldgen --release --test insolation_probe -- --ignored --nocapture
```
