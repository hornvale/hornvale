# 0028. The bare-ok rubric for primitives at API boundaries

**Status:** Accepted (2026-07-09) · **Decider:** Nathan

In the context of the type audit (spec; decision
0027) requiring a verdict on
every primitive crossing a public boundary, facing the question of which bare
primitives are *permanently* fine versus deferred newtypes, we decided the
**bare-ok rubric**: a primitive at a `pub` boundary is permanently acceptable
only if it falls in one of eleven named classes; anything else that is a
coherent physical quantity or a confusable identity is a deferred newtype
(`pending(wave-N)`) or a sourced `waiver`.

**The eleven bare-ok classes.**

| Class | A bare primitive is permanently fine when it is… |
|-------|--------------------------------------------------|
| `ratio` | a dimensionless ratio or fraction (decision 0008 says these *stay* bare) |
| `count` | an honest cardinality — cell/plate/moon counts, octaves, generations |
| `index` | a position into a structure whose *type* carries the meaning (e.g. `CellId`, whose own doc says it indexes a mesh) |
| `constructor-edge` | a newtype's sanctioned raw edge — **both** a physical-quantity newtype's `new(value)`/`get()` **and** an opaque handle / PRNG's wrapped primitive (`Seed(pub u64)`, `EntityId`, `Stream::next_u64`) |
| `envelope` | a field of the kernel's deliberately-dumb trace protocol — `Value` and `Fact` (spec §3.1.6) |
| `identifier-text` | a name, label, or **key** whose contract is being plain text (stream labels, predicate keys — decisions 0006, 0015) |
| `prose` | free-form human-readable text: descriptions, doc strings, `Fact.provenance`, genesis-note sentences stored as `Value::Text` |
| `artifact` | a finished produced output: rendered ASCII/PNG bytes and colour escapes, **and** serialized documents (`to_json`/`from_json`, emitted scene/study JSON) |
| `diagnostic-value` | a raw *rejected* value echoed back in a validation error (e.g. `UnitError.value`) |
| `render-internal` | screen-space pixel/glyph *math* and dimension constants in render modules (`MAP_WIDTH`, `ASCII_HEIGHT`) |
| `flag` | a `bool` unambiguous at *every* call site (a `bool` that reads as `foo(true, false)` at call sites is `pending`, not `flag`) |

`ratio`, `count`, `index`, `constructor-edge`, `envelope`, `identifier-text`,
`render-internal`, and `flag` were the initial set (spec §4). `prose`,
`artifact`, and `diagnostic-value`, and the **clarification** that
`constructor-edge` covers opaque handles/PRNG as well as physical newtypes,
were ratified during the audit's astronomy/kernel pass (Campaign 27). The
rubric classifies by *reason*, not shape: two structurally identical id
newtypes get different verdicts (`CellId` → `index` because it indexes;
`EntityId` → `constructor-edge` because it is a minted handle).

**Waivers** cite a source: `waiver(decision-0014)` (`Fact.day` stays a bare
`Option<f64>`), `waiver(elevation-convention)` (terrain's documented bare-f64
elevation — including a verbatim cross-crate passthrough, e.g. scene
re-emitting `terrain.elevation_at`). An untraceable waiver is a review failure.

**Pending.** Anything that is a coherent physical quantity or a confusable
identity and is in no bare-ok class gets `pending(wave-N)`: meters, days,
Kelvin, degrees/radians, AU, masses, probabilities-used-as-thresholds, a
`u32` that is secretly an id. Wave map (spec §7): **wave-1** kernel; **wave-2**
terrain + climate; **wave-3** settlement, species, language, religion, windows.
An **already-typed** case — a bare primitive where an in-crate newtype already
exists (`class_luminosity`, `hill_radius_mm`, `max_entity_id`) — stays
`pending` (this campaign changes no production code), noted as a trivial,
high-priority conversion for its wave.

**Application note — mixed tuples.** The tool judges one verdict per audited
*position*, and a whole `Vec<(&str, &str)>` return (e.g. `stream_labels()`,
label paired with description) is a single position with no sub-tuple
granularity. Such a position takes the verdict of its **load-bearing** element:
`stream_labels()` is `identifier-text`, because the label is the save-format
contract (decision 0006) and the paired description is inline documentation of
it. A future tool with per-tuple-position granularity could split these.

**Consequence.** The rubric is enforced mechanically by `tools/type-audit/`
via `type-audit:` doc-comment tags; the committed
`docs/audits/type-audit-report.md` tallies the standing verdicts. A new class
is added only by *superseding this decision* (never editing it), after
ratifying a contested case. The remediation waves (spec §7) convert every
`pending` tag; each wave's diff deletes the tags it resolves.

**See also.** Decision 0027
(the tool); decisions 0008 (typed quantities), 0014 (`Fact.day`), 0006
(labels are permanent contracts); the type-audit spec §4; the elevation
convention (Campaign 3 plan). Slug filename, not a number, per decision 0026
(slugs, not numbers).
