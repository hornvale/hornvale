# 0778. A sign has one implementation

**Status:** Accepted (2026-09-05) · **Campaign:** The Warp · **Decider:**
Nathan · **Relates:**
[0011](0011-studies-are-data-metrics-are-code.md),
[0121](0121-ordinal-fields-may-band-a-blend-nominal-fields-must-partition.md),
[0123](0123-disclose-a-resolution-rather-than-refine-a-field.md),
[0776](0776-legibility-is-measured-from-the-walkers-side.md)

In the context of an instrument that measures what the prose *tells* a
walker — where a threshold copied into the measuring code would make the
instrument measure something the sentence does not say — we decided that
**every sign has exactly one implementation, in `hornvale_worldgen::warp`,
which the prose renders as a word and the laboratory tabulates as a
discriminant**, accepting that a window's rendering layer must depend on the
composition root for a partition it could have restated in two lines.

## Context

The Rill named a second copy of a partition as its own failure mode: a
grounded axis whose threshold lived in the renderer and again in whatever
read it. This campaign is exactly the shape that hazard bites. The claim
being measured is "the walker is told *this*"; if the lab cuts wetness at
0.33 while the sentence cuts it somewhere else, the reading is about two
different worlds and no test would notice, because both halves would be
internally consistent.

The wetness word made the risk concrete before the campaign started: its
±0.33 cut lived privately inside the descriptive grammar, and the laboratory
had no way to ask for it except by writing `0.33` again.

## Decision

`windows/worldgen/src/warp.rs` is the single home:

| item | what it is |
| --- | --- |
| `rock_word(RockClass) -> &'static str` | one authored phrase per rock class, exhaustive, no wildcard arm |
| `steepness_sign(slope) -> Steepness` | `tanh(\|slope\| / GORGE_SLOPE)` cut at `STEEP_LO` / `STEEP_HI` |
| `steepness_word(Steepness) -> &'static str` | the rendered word for the enum |
| `wetness_sign(axis) -> Wetness` | the `MICRO_WORD_THRESHOLD` cut, now public rather than private to the grammar |

The prose renders the enum's word; the instrument keys on the enum. No
threshold has a second copy anywhere, and the constants themselves
(`MICRO_WORD_THRESHOLD`, `STEEP_LO`, `STEEP_HI`) are exported rather than
inlined.

**The rule is enforced by a test, not by intention:** a test renders a real
room and re-derives its sign tuple through the same functions, requiring the
words to match. That is the mechanism the Rill's warning lacked.

## Why

**The rock word is a nominal read and must partition** (0121): `RockClass` at
the room's dominant corner vertex, categorical nearest-corner, never
blended — the same read `reflectance_mixture_with_weights` performs for
colour and `FabricContext::at` performs for building fabric. It is a
per-vertex word and reads constant across a vertex's ~4,096 rooms. That is
disclosed rather than hidden (0123): a document finer than the model behind
the field would be inventing detail.

**The steepness word saturates exactly as overhang's own recipe saturates
it.** Overhang's cause is `induration × steep`; with the rock word (induration
is a property of the class) the walker holds both halves of overhang's sign,
as they hold both halves of spring's (limestone × wet). A second saturation
constant would have made the word and the cause disagree by construction.

**It is the studies-are-data rule pointed at prose** (0011). The corpus of
what a walker is told is the renderer's own output; the resolver reading it
must not carry its own copy of the vocabulary.

## Consequences

**`windows/vessel` depends on `hornvale-worldgen` for the sign functions.**
That is inside the layering — a window may depend on the composition root —
and it is the price of the guarantee.

**A future sign is added here or it is not a sign.** A rendered token whose
threshold lives in the renderer alone cannot be measured against the walker's
channel, so it falls outside 0776's definition by construction rather than by
policy.

*Ledger: `docs/superpowers/ledgers/2026-09-05-the-warp.md` #7; spec §4.1,
§4.2, §4.4. Sources: `windows/worldgen/src/warp.rs`,
`windows/vessel/src/warp_prose.rs`, `windows/vessel/tests/suite/the_warp.rs`.*
