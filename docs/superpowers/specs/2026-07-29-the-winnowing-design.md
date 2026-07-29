# The Winnowing — design

**Campaign:** The Winnowing
**Date:** 2026-07-29
**Status:** spec, awaiting G3 review

The tiles document carries nineteen per-tile layers to a client that reads
ten. Let the caller say which it wants.

## 1. The problem

`scene/tiles/v1` at width 512 is **17,728,743 bytes**. Measured composition
of the seed-42 document, by field:

```
field                      bytes   share     count   B/elem
unrest                 1,675,090    9.4%   131,072     12.8
t_swing_c              1,561,328    8.8%   131,072     11.9
snow_fraction          1,466,989    8.3%   131,072     11.2
elevation_m            1,417,331    8.0%   131,072     10.8
t_mean_c               1,380,257    7.8%   131,072     10.5
precip_mm_yr           1,297,204    7.3%   131,072      9.9
current_east           1,223,220    6.9%   131,072      9.3
current_north          1,213,894    6.8%   131,072      9.3
cloud_fraction         1,148,650    6.5%   131,072      8.8
t_diurnal_amp_c          962,488    5.4%   131,072      7.3
weather_propensity       886,009    5.0%   131,072      6.8
moisture                 789,436    4.5%   131,072      6.0
ocean                    694,812    3.9%   131,072      5.3
drainage                 529,567    3.0%   131,072      4.0
biome                    356,955    2.0%   131,072      2.7
plate                    306,902    1.7%   131,072      2.3
water / precip_regime / cloud_type   262,145 each  1.5% each
features                  31,203    0.2%       329     94.8
(everything else)             <1 KB combined
```

Nineteen parallel arrays of 131,072 elements are **99.8%** of the document.

### 1.1 Nine of them are never read

The Orrery's `parseTiles` (`orrery src/sim/scene.ts:484`) extracts fields by
name. Nine per-tile arrays are not referenced anywhere in its `src/`:

`t_diurnal_amp_c`, `drainage`, `snow_fraction`, `precip_mm_yr`,
`precip_regime`, `cloud_fraction`, `cloud_type`, `weather_propensity`,
`current_east`, `current_north`.

Together: **9,252,311 bytes — 52.2% of the document.** `JSON.parse` builds
them, the client discards them, and nothing ever reads them.

Measured parse cost of the full document (node 
warm, three runs): **168.9 / 148.9 / 151.3 ms**. That is a proxy for the
browser, not a browser measurement — see §4.

### 1.2 Why this is not a fidelity question

The producer computes all nineteen layers either way. Nothing is being made
less accurate, and no client loses access to anything: this changes **what
crosses the wire**, not what the sim knows. Decision 0022 says the sim emits
data and clients render; a caller stating which layers it will render is
that decision working, not an exception to it.

The precedent is `BuildDepth` (`windows/worldgen/CLAUDE.md`): *"A
metric/consumer that needs only astronomy must not force a `Full` build."*
The scene layer has no equivalent. This is that idea one ring outward.

### 1.3 The document is unusually easy to project

Nineteen independent parallel arrays over one index space, with no
cross-field structure. Any subset is a coherent document, and — critically —
**each field's bytes do not depend on which other fields were requested.**
That property is what keeps the golden story from exploding combinatorially
(§3.3).

## 2. Non-goals

- **No change to any field's bytes.** A requested field serializes exactly as
  it does today. The projection omits; it never re-encodes.
- **Not a re-encoding.** Base64 typed arrays would beat JSON's ~10 bytes per
  float badly, but that is a `v2` schema and a far larger change. **Projection
  first, encoding later** — and projection composes with a later encoding
  change rather than competing with it. Named as a followup, not attempted.
- **Not the `ocean` boolean waste.** 5.3 B/element for a bool (`true,` /
  `false,`) is ~430 KB of pure syntax, but fixing it moves bytes and needs a
  schema change. Followup.
- **No reduction in `width`.** That is a fidelity cut and a client-side
  choice; it is not on this campaign's table.
- **No skipping of the sampling work.** The projection lives at the
  serialization boundary only (§3.2). Skipping the *build* of unrequested
  layers is a larger, more invasive change; measure first, and only if §5's
  numbers justify it.
- **No client-side (Orrery repo) work.** The catalog gains the capability;
  adopting it is a separate, client-side change.

## 3. Design

### Item 1 — a field set, expressed as names

A `TileFields` selector in `windows/scene`: an explicit set of per-tile layer
names, with a `TileFields::all()` that reproduces today's document exactly.

Names, not a bitmask: the wire already speaks these names, a bitmask would
need its own stability contract, and an unknown name must be a loud error
rather than a silently ignored bit.

**An unknown field name is a hard error** (`SceneError`), not a no-op. A
client asking for `elevation` when the field is `elevation_m` must be told,
not handed a document silently missing the layer it will then fail to find.

The non-array metadata — `schema`, `seed`, `width`, `height`, `sea_level_m`,
`season_period_days`, `locked`, `circulation_bands`, `biome_legend`,
`water_legend`, `features`, `waterfalls` — is **always emitted**. It is
under 32 KB combined, several parts of it are needed to interpret any layer
at all, and making it selectable would buy nothing while adding ways to
produce an uninterpretable document.

### Item 2 — projection at the emit boundary, and nowhere else

`scene_json_selected(scene: &TilesScene, fields: &TileFields) -> String`
beside today's `scene_json`, which becomes `scene_json_selected(scene,
&TileFields::all())`.

The projection is a **filter over serialization**, not a different
serializer: field order is unchanged, and a present field's bytes are
byte-for-byte what `scene_json` emits today. This mirrors the kernel's
quantize-at-emit-only discipline — the compute path is untouched and only
the emit boundary changes.

Consequence worth stating plainly: `tiles_scene` still *builds* all nineteen
layers. This campaign buys serialization time, wire bytes, and client parse
time — not sampling time. §5 says what that is worth.

### Item 3 — the catalog export

`hw_scene_tiles_selected(width: u32, len: usize) -> i32`, reading a JSON
array of field names from `INBUF` — the same input-buffer idiom
`hw_new_pinned` already uses (`clients/world-wasm/src/lib.rs:157`).

`hw_scene_tiles(width)` is unchanged and still emits everything. A client
that says nothing gets today's behaviour; the winnowing is opt-in.

### Item 4 — the golden story

§1.3's independence property is what makes this tractable. Three pins:

1. The existing full-document golden, unchanged — proves `TileFields::all()`
   is today's document.
2. **A per-field independence test:** for each of the nineteen layers, assert
   that the field's bytes inside a single-field projection are identical to
   its bytes inside the full document. This is the property the whole design
   rests on, and it is the one thing that must not be assumed.
3. One committed golden of a representative subset (the Orrery's ten), so the
   composed shape — braces, commas, key order with holes in it — is pinned
   and not merely reasoned about.

That is nineteen assertions plus two goldens, instead of 2¹⁹ documents.

## 4. Verification

1. **Byte-identity of the default path first.** `scene_json(scene)` must be
   byte-identical to today for seed 42 at width 512, before anything else is
   trusted. The existing goldens plus a fresh checksum baseline.
2. Item 4's per-field independence assertions.
3. The measured size and time reduction for the Orrery's ten-field set (§5).
4. `make gate`; `make world-check` (the catalog gains an export — report the
   wasm size delta, measured **at a constant build path**: the binary embeds
   `#[track_caller]` panic locations, so path length changes the number).
5. `make gate-full`.
6. `make rebaseline` + `git diff --exit-code` on the artifact directories.

**On the parse-side claim:** §1.1's 150 ms is node, warm, on this box. The
honest browser measurement lives in the Orrery's own harness (The Frame
Budget built one: a scripted run in headless Chromium sampled over the debug
protocol), which is a different repo and out of scope here. This spec claims
a **byte** reduction, which is measured, and a **parse** reduction, which is
inferred from a proxy. Do not let the chronicle state the second as if it
were the first.

## 5. Expected result

For the Orrery's ten-field set, dropping 9,252,311 bytes:

| | now | expected |
|---|---|---|
| document | 17.73 MB | ~8.48 MB (−52%) |
| serialize | ~553 ms | ~265 ms (if proportional to bytes) |
| `JSON.parse` proxy | ~150 ms | ~72 ms (same assumption) |
| `tiles_scene` build | ~600 ms | unchanged — §3.2 |

**Both time figures assume cost is proportional to bytes, which is an
assumption and not a measurement.** Serialization of a float array is
dominated by float formatting, so it should hold roughly; parse may not
behave the same way. The profiler prints the truth, and if it comes in
materially under, that is the finding — not a number to quietly write down.

Net on the client's cold load: roughly **300–400 ms** off a path that also
carries ~1.8 s of genesis. Real, worth having, and smaller than the previous
two campaigns' wins — which is the honest framing, not a disappointment.

## 6. Flagged for review (G3)

1. **This is the first campaign to make the wire contract parameterized.**
   Every scene document so far has been one fixed shape. A caller-chosen
   projection means "the tiles document" is now a family. §1.3's independence
   property and §3.4's per-field pins are what keep that honest, but it is a
   genuine change in the character of the producer/consumer contract and
   deserves your eye rather than mine.
2. **Nine fields risk becoming untested on the wire.** If the Orrery adopts a
   ten-field projection, the other nine only ever ship in the default path.
   Mitigated by keeping the full document the default and golden-pinned —
   but worth naming, because a layer nobody transports is a layer that can
   rot quietly.
3. **The win is the smallest of the three campaigns** (~300–400 ms against
   The Cistern's ~15 s). If you would rather go straight at the encoding
   change — base64 typed arrays, a `v2` schema, roughly 4 bytes per float
   instead of ~10 — that is a bigger prize and a bigger campaign, and this
   one becomes unnecessary rather than a prerequisite. **That is a real fork
   and I would like your call on it** (§7).
4. **No epoch, no new seeded draw, no change to any committed artifact's
   bytes** — the default path is byte-identical by construction.

## 7. The fork worth deciding before planning

Three routes, in ascending order of prize and cost:

- **A — projection only (this spec).** ~52% fewer bytes for the Orrery's set,
  no schema change, no fidelity question, ~300–400 ms. Composes with B later.
- **B — re-encode the arrays** (base64 typed arrays or a binary side-channel).
  ~4 bytes/float against ~10, so ~60% off *every* field, and it largely
  removes `JSON.parse`'s array-building cost rather than reducing it.
  Requires `scene/tiles/v2`, a catalog version bump, and coordinated Orrery
  work. Strictly bigger than A on every axis including risk.
- **C — both, in that order.** A is one week of small, reversible change; B
  lands on top without conflict, since projection is orthogonal to encoding.

**My recommendation is C, starting with A** — mostly because A is where the
measurement lives. Until something has actually halved this document and
measured the client's response, B's prize is an estimate too. But A alone is
a modest win, and if you would rather spend the effort once, B is the more
honest target and I will re-spec for it.
