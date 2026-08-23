# The Wick — design

**Date:** 2026-08-21
**Status:** draft (G3 review pending)
**Predecessor:** The Chroma (2026-08-21); builds on The Lantern's light model (2026-08-07)
**Campaign worktree:** `.claude/worktrees/the-wick` (`campaign/the-wick`)

## 0. Summary

Two changes to the possessed chamber's light model, right-sized as one
campaign because they touch the same seam and move the same fixtures:

1. **A bigger flame.** The implicit torch's emitted intensity scales ×4.
   The inverse-square falloff (`ATTENUATION`, its shape and its value) is
   untouched — the H4a fence guards the gradient *shape*, and this moves
   brightness away from the guarded failure mode on the owner's product
   call.
2. **No more uncoloured fabric.** A cell no flame reaches claims the
   fabric's colour under a dim ambient (skyglow) illuminant instead of
   emitting `color: None` — closing [[CLIENT-unlit-is-uncoloured]], the
   inversion The Chroma found where lit cells render darker than ignorant
   ones.

Both are producer-side (`windows/vessel`), so this campaign moves
committed fixtures and owes a rebaseline.

## 1. Ground truth (verified on the tree)

- The torch is an implicit `Source { at: observer's cell,
  illuminant: blackbody(TORCH_KELVIN=1900), radius: SIGHT_RADIUS }`
  (`session.rs::chamber_sources`). Its brightness at distance d is
  `1/(1 + ATTENUATION·d²)` with `ATTENUATION = 1.0` — at distance 4 that
  is 5.9% of source, which through a dichromat eye lands near `[8,8,0]`.
- `colour_of` (`plan.rs`) withholds colour three ways; ours is
  `shading.light.get(&cell)?` — **unlit means absent from the light map**,
  documented as "a cell absent here gets no colour at all rather than a
  black one."
- `Illuminant` is band-vector newtype; `attenuate` already does per-band
  scaling, so source scaling needs only a small local helper.
- The H4a fence ("may not be tuned", spec §4.2 §11 risk 2) was written
  against *manufacturing darkness* after symmetric shadowcast made the lit
  set equal the FOV set. This campaign raises brightness and adds an
  ambient floor; it does not touch the gradient shape.

## 2. Design

### 2.1 Torch intensity ×4

`chamber_sources`'s torch source gains a ×4 scale on its emitted
illuminant (a local `Illuminant::scaled(k)` helper in `light.rs` if the
kernel has none — prefer vessel-local; do not add kernel API for this).
Physical reading: carry more candles. Effects by distance (fraction of
source): d1 0.5→2.0 (clamps at sRGB white — fine), d2 0.20→0.80, d3
0.10→0.40, d4 0.059→0.235. The hearth keeps its own level (it was already
"a bed of embers"; revisit only if the ×4 torch now outshines it
everywhere — check, don't assume).

### 2.2 The skyglow floor

Cells the light map does not reach fall back to a dim ambient illuminant —
the world's own night sky as seen through the doorway model, i.e.
`eyes::daylight_at`'s spectrum scaled far down (target: the fabric renders
visible-but-clearly-darker than flame-lit neighbours; pick the constant by
rendering seed 42's chambers, not by theory). Implementation shape:
`Shading.light` lookup falls back to the ambient rather than `None`; the
ambient rides on `Shading` so the withholding story stays in one type.

**What is deliberately kept:** the other two withholdings stay. Fabric
that cannot be derived still claims nothing; an absent observer still
withholds everything. And the client side does not change at all —
The Chroma's reader already honours whatever claim arrives.

**Epistemics note:** the plan palette already reveals cell kind by glyph,
so a dim skyglow claim reveals nothing the glyph did not. The Pigment
constraint (night vision = channel sensitivity, not gain) binds the *eye*;
this is incident light in the world, which legitimately scales.

### 2.3 Scope boundary

Walk band untouched (verify: its cells arrive lit by the world star's
field; confirm no `color: None` population outdoors before assuming).
No schema version bump — values move, keys don't. No epoch: light is not
a seed label and no stream consumption order changes.

## 3. Testing

1. `lantern_light.rs`'s battery updated: the "unlit wall carries None"
   assertions flip to "carries the ambient claim" — these are the tests
   that pin today's behaviour, so their RED is the campaign's TDD entry.
2. Gradient-shape pin: a test asserting relative brightness between two
   distances still follows `1/(1+d²)` exactly (the fence, made executable).
3. Fixture rebaseline: session fixtures' plan palette moves wholesale;
   regen via the sanctioned path and eyeball the diff direction (brighter,
   non-neutral).
4. `make game-check` + full gate at close; visual pass by Nathan.

## 4. Out of scope

- Explicit ownable/droppable torch (deferred; implicit torch is good for now).
- Walk-band lighting changes; eye sensitivity modelling; any client change.
