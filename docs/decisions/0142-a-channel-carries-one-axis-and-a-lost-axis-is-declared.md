# 0142. A rendering channel carries one axis, and a lost axis is declared

**Status:** Accepted (2026-08-18) · **Decider:** Nathan · **Relates:**
[0022](0022-sim-emits-data-clients-render.md),
[0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md),
[0041](0041-libm-for-portable-transcendentals.md),
[0055](0055-external-clients-consume-a-versioned-wasm-catalog.md),
[0123](0123-disclose-a-resolution-rather-than-refine-a-field.md),
[0141](0141-compass-navigation-is-an-overlay.md)

In the context of a chart whose glyph channel had been carrying two
different things at once — an ordinal ladder of relief *and*, through a
`faded()` substitution, the observer's memory of a cell — we decided that
**a rendering channel carries exactly one measurement axis, and a surface
that lacks a channel loses that axis and says so in its caption**, because
every alternative recovers a lost axis by borrowing a channel that already
means something else, which makes the picture assert a thing that is not
true and gives the reader no way to tell which claim they are looking at.

## Context

**The construct that forced the decision was four characters long.**
`surrounds_ascii::faded` mapped a remembered `.` to `,` and a remembered
`:` to `;`. That is harmless while the land glyphs are an unordered set. It
is a false claim the moment they are a *ladder*, because `,` and `;` sit
between the rungs: a remembered cell would render one step up the ordinal
from what was actually observed, on exactly the cells the character can
least verify. The hazard had been recorded in the idea registry
(`CLIENT-faded-collides-with-an-ordinal-ladder`) before this campaign
existed, with a note that the tempting move is to reuse `faded`.

The registry row named the collision. What it did not name is that the
substitution was already wrong *before* the ladder — it was an epistemic
claim written in the substance channel — and that the same construct had a
twin in `clients/vessel/src/pane_chart.ts` doing the identical thing in a
client that had a weight channel available the whole time.

**Three axes were in play and only two had homes.** A cell answers three
different kinds of question: *what is this* (nominal — a surface cover),
*how much of it* (ordinal — how hard the ground is to cross), and *how sure
is the observer* (epistemic). Colour was carrying the first badly (it read
the bedrock beneath, not the surface you would see), glyph was carrying the
second and half of the third, and nothing carried epistemic on its own.

## The decision

> **A rendering channel carries one measurement axis, and a client that
> lacks the channel loses the axis and says so.** Colour carries the
> nominal axis, glyph the ordinal, and weight modulates both with the
> observer's confidence. A client may not recover a lost axis by
> reallocating another channel. The wire ships adjacent rungs of the
> signal-to-pixel ladder — the post-eye signal and the sim's own projection
> of it — so that each client chooses where to cut without any client being
> required to implement optics.

Three clauses, each doing separate work.

**One channel, one axis.** Colour means the surface a character would see:
a mixture over `bare`, `chlorophyll`, `litter`, `snow`, `sand` and `silt`,
weighted by cover and modulated per room by `micro`. Glyph means
*impedance* — `relief + 0.5·canopy + 0.5·roughness` — one ranked answer to
"how hard is this to cross", reusing the five characters the relief ladder
had already spent rather than minting any. Weight means confidence, and
nothing else.

**A lost axis is declared, never reallocated.** The escape-free `terrain`
lens — the surface `Eyes::Off` picks, and the posture a screen reader takes
— has no weight channel at all. It therefore loses the epistemic axis
outright and prints `epistemic: this lens carries no weight channel, so
remembered cells draw identically to sensed ones — N of M placed`. The
colour lens prints the mirror sentence when every placed cell is bare. The
count is the point: a caption a reader can check against the picture in
front of them, rather than one they must trust.

Note what `faded()` *was*, exactly: the forbidden reallocation, recovering
epistemic on the glyph channel because weight was unavailable there.
Deleting it was correct; replacing it with an escape sequence in an
escape-free surface would have violated the same rule from the other side.

**The wire ships adjacent rungs, additively.** `scene/surrounds/v2` gained
the post-eye `signal`, the categorical `cover`, and — as one landing — the
calibration that makes `signal` interpretable: `channel_roles`,
`projection_slots` and the per-observer `projection_norms`. No `v3` was
minted. `pane_chart.ts` allowlists exactly `scene/surrounds/v2` and refuses
every other tag, so minting a version darkens every client until all of
them update in one commit, which 0055 explicitly does not require.

## Consequences

- **A client can reproject, and this is demonstrated rather than asserted.**
  `(signal[slots[i]] / norms[i]).clamp(0, 1)` followed by the sRGB transfer
  reproduces the carried `color` byte-for-byte, recomputed from wire values
  alone with no kernel lookups, over all 186 signal-and-colour cell pairs in
  three regenerated fixtures: **0 mismatches**.
- **The calibration is three fields, not two, and the near-miss is worth
  recording.** A landing that shipped `channel_roles` and `projection_slots`
  without `projection_norms` would have published a field no client could
  interpret — into a contract that cannot un-publish one. The norms are
  per-observer (the standard observer's are 1.98/3.51/3.95); a species
  observer's differ.
- **The reprojection claim is photopic only.** `Observer::to_srgb`'s
  scotopic branch reads `SCOTOPIC_GAIN`, `SCOTOPIC_NORM` and
  `PHOTOPIC_THRESHOLD`, which are *global kernel constants* rather than
  per-observer calibration. They were deliberately not shipped — 0 of 186
  checked cells reached that branch, including one at `sun_altitude_deg:
  −56.01` — and the doc comments say so instead of overclaiming. A dark cell
  falls back to the carried `color`, which is present on every such cell.
- **A wire declaration that had become false was corrected.**
  `grid_resolution_fields` announced `["biome", "color", "water"]`, meaning
  those fields cannot vary below grid resolution. Making colour a surface
  mixture made that a falsehood on a cross-repo contract. `"color"` was
  removed. It is a value change, not a shape change: the key stays, its
  contents shrink.
- **The glyph budget did not grow.** Eleven characters total — `@ & # ~ =
  + _ . : ^ A` — measured by grepping every character literal
  `terrain_glyph` can emit, not asserted. Deleting the seven memory twins
  paid for the ordinal ladder exactly.
- **No epoch.** No stream label moved, no draw changed, no world moved. The
  committed `vessel/session/v2` golden drifted in 465 leaf values, every one
  a `color` RGB or the `grid_resolution_fields` shrink, with zero diffs
  elsewhere — verified by a full key-path walk before the rebaseline was
  accepted.
- **The colour work is invisible at the view a new player opens first, and
  that is a fact about the world rather than a defect.** The flagship walk
  band is 100% river on all five seeds sampled (42, 13, 7, 1, 100) — a
  property of settlement siting — and the lens withholds tint from every
  non-ground glyph, so the default outdoor chart reads `0 tinted, 31
  withheld`. No committed artifact reaches the colour lens either.
  Registry row `RENDER-colour-is-invisible-where-the-game-starts`.
- **The ordinal claim is confirmed for the sim's ASCII renderer only.**
  Preregistration named three renderers; the two client renderers were a
  deliberate scope cut, and the narrowing is stated rather than assumed.
- **One projection is now evaluated by three transcendental
  implementations** — the portable `libm` crate, the platform libm, and V8 —
  two of them on the client side of 0055's boundary. Probably legitimate,
  and nothing in the repo can currently answer it: the cross-renderer
  agreement test runs on one platform. Registry row
  `RENDER-three-evaluators-one-projection`.

## See also

`windows/locale/src/surface.rs` (`CoverClass`, `cover_weights`,
`cover_class_at`), `windows/locale/src/lib.rs`
(`reflectance_mixture_at`), `windows/scene/src/surrounds.rs` (`Sight`'s
`channel_roles`/`projection_slots`/`projection_norms`, `SurroundsCell`'s
`signal`/`cover`/`bearing_deg`/`distance_rad`, `cover_legend`),
`windows/scene/src/surrounds_ascii.rs` (`impedance_glyph`, `box_rank`, the
`placement:`/`colour:`/`epistemic:` caption lines),
`kernel/src/room.rs` (`bearing_to`, `distance_rad_to`),
`kernel/src/color.rs` (`Projection::rgb`),
`clients/vessel/src/pane_chart.ts`, `clients/game/core/src/chart.rs`,
`docs/superpowers/specs/2026-08-17-the-illumination-design.md` §2, §3, §4,
§5, [the chronicle](../../book/src/chronicle/the-illumination.md).
