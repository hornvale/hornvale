# 0122. When a view is finer than the model behind a field, the document discloses the resolution rather than the field inventing detail

**Status:** Accepted (2026-08-11) · **Decider:** Nathan · **Relates:**
[0038](0038-identity-computes-on-the-canonical-grid.md),
[0055](0055-external-clients-consume-a-versioned-wasm-catalog.md),
[0076](0076-the-situated-pole-is-egocentric-and-knowledge-limited.md),
[0120](0120-ordinal-fields-may-band-a-blend-nominal-fields-must-partition.md),
[0121](0121-an-emit-gate-is-not-a-grain-gate.md)

In the context of *The Grain* finding that a walking-depth chart reports one
biome and one water kind across a whole neighbourhood, facing the fact that this
reads as the chart contradicting the room's own prose ("open water" one line
above "buttressed canopy, shaded, in a hollow"), we decided that **a document
whose view is finer than the model behind one of its fields declares that field's
resolution**, rather than the field synthesizing sub-model detail — accepting
that a consumer must then caption a limitation instead of receiving variation it
would have preferred.

## Why disclosure is the fix and refinement is not

The uniformity was never the defect. A field decided per canonical grid cell is
*correctly* constant across `4^6 = 4096` rooms; the document simply never said
which of its fields those were, so a reader had to guess, and guessed wrong. Two
campaigns' worth of diagnosis went into a contradiction that did not exist.

Refinement was tried and is illegal for the field in question
([0120](0120-ordinal-fields-may-band-a-blend-nominal-fields-must-partition.md)):
thresholding a blend of a nominal field's underlay deletes a category, and it
deleted 29% of seed 42's fresh water. So the choice was not between a flat field
and a varied one. It was between a flat field a reader could interpret and a flat
field a reader could not.

The project already had this move and had not generalized it. `Sight::preserves`
states what a colour projection does *not* carry — *"the short-to-long
opposition; the red–green axis is not carried"* — so a two-chromatic-channel eye
emitting triples whose red and green are equal by construction is legible instead
of looking broken. `resolution` is the same move for spatial resolution.

## The decision

1. **A situated document that can be built below the resolution of any of its
   fields carries a resolution disclosure.** `scene/surrounds/v2`'s is
   `resolution: { grid_level, depth_below_grid, grid_resolution_fields }`, always
   present.
2. **The disclosure names field keys, and only fields that are genuinely
   constant below the stated resolution.** `grid_resolution_fields` is
   `["biome", "color", "water"]`. `relief` is deliberately excluded because it
   bands a three-corner blend and does vary; `micro` is excluded for the opposite
   reason, being the finest-grained field the document carries. Two exclusions
   with opposite justifications is the test of whether the list means anything.
3. **A field whose granularity is mixed is not listed**, because listing it would
   misstate the document. `marks` mixes a grid-resolution kind (`"cave"`) with
   per-room kinds (`"settlement"`, `"agent"`), so neither `"marks"` nor a
   fabricated `"cave"` key appears — and a test pins that absence, so it reads as
   a decision rather than an oversight.
4. **Disclosing a resolution is not refining it, and must not be read as a step
   towards refinement.** The disclosure is the finished answer for the field it
   names. Where genuine sub-model detail is wanted, it comes from a model that has
   it (`MAP-64`'s flow graph for water), never from re-reading the coarse field.
5. **The disclosure is additive.** It appends a key, describes existing fields
   rather than changing them, and therefore mints no new schema version under
   [0055](0055-external-clients-consume-a-versioned-wasm-catalog.md)'s
   additive-or-versioned rule.

## Consequences

- **A consumer can caption instead of infer.** The intended reading of a flat
  view is *"grid resolution — every room here reads one coarse cell"*, and the
  reference page says so in those words.
- **The next reader does not repeat the diagnosis.** That is the measurable
  payoff, and it is the reason this is a decision rather than a feature: the cost
  of the omission was paid twice before anyone noticed the document could simply
  say.
- **Cheapness is part of the argument.** The block is three fields of already-known
  values, computed from the query's own parameters. There is no reason for a
  situated schema to omit one.
- **It creates an obligation on future fields.** Adding a field to a document
  that carries a disclosure means deciding which side of the disclosure it falls
  on — and saying so — rather than leaving the list stale. A stale
  `grid_resolution_fields` is worse than none, because a consumer would trust it.

## See also

`The Grain` spec §5 and its [chronicle](../../book/src/chronicle/the-grain.md);
`Resolution` in `windows/scene/src/surrounds.rs`; the "Resolution: what a uniform
field is telling you" section of
[`scene-surrounds-v2.md`](../../book/src/reference/scene-surrounds-v2.md); the
registry row `GRAIN-disclose-resolution-not-refine-it`.
