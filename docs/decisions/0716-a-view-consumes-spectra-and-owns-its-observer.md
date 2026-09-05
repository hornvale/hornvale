# 0716. A view consumes spectra and owns its observer; the sim never emits a colour

**Status:** Accepted (2026-09-04) · **Decider:** Nathan (autopilot, spec §8.1) ·
**Relates:**
[0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md)
(quantize at the emit boundary only — this record is that discipline applied
to ink rather than to numbers),
[0389](0389-a-glyph-carries-order-or-identity-never-category.md) (what each
channel may carry; unamended by this record),
[0055](0055-external-clients-consume-a-versioned-wasm-catalog.md) (clients are
outside determinism, which is why the collapse belongs to them),
[The Wash](../../book/src/chronicle/the-wash.md)

In the context of *The Wash* moving the world map onto the spectral colour
substrate the room-scale view already used, we decided that **a view consumes
spectra and owns its observer, and no code in the sim emits a colour** —
accepting that every consumer must carry an observer of its own, and that two
views of the same place may legitimately render it differently.

## The decision

The sim answers two questions and refuses the third:

| question | answered by | shape |
| --- | --- | --- |
| what does this material return? | the sim | a `Reflectance` spectrum |
| what light falls on it? | the sim | an `Illuminant` spectrum |
| what should be displayed? | **the view** | the observer's collapse |

A `Reflectance` is a fact about a material and is identical in a cave and at
noon. An `Illuminant` is a fact about the light. Neither is a colour. Colour
exists only where a third thing — an observer with sensitivity curves —
collapses the arriving mixture, and **that observer belongs to whatever is
doing the looking.**

## Why this is not new, and why it needed recording anyway

`kernel/src/color.rs` already made this claim in its own module doc, and
`windows/vessel` already honoured it: it resolves an observer per *creature*,
so species vision falls out of the same operation.

What was missing was any statement that this binds **views**, not only
creatures. The world map had a six-entry RGB table indexed by elevation band,
predating the substrate, and nothing in the record said it was wrong. A
principle honoured in one place and unrecorded is indistinguishable from a
local implementation choice — which is precisely how the map came to have a
parallel, worse colour model living beside a better one for as long as it did.

## Consequences

- **Degradation is an observer, not a fallback.** `NO_COLOR`, a 16-colour
  terminal, a 256-colour cube and a colour-vision variant are four observers
  over one pipeline, not four special cases in a renderer. `None` from a
  collapse is a legal result meaning *this observer shows no colour*, and the
  caller falls back to a channel that does not depend on colour — which is
  what [0389](0389-a-glyph-carries-order-or-identity-never-category.md)'s
  "nothing a reader must trust may live only in colour" already required.
- **The quantization to a display's budget happens at the client.** A hue
  count is a fact about a terminal, not about the world, and choosing one in
  the sim would be quantizing in the compute path — the error
  [0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md)
  names for numbers.
- **Two views may differ, and that is correct.** The map and the room view
  collapse the same spectra through different observers at different scales.
  Neither is authoritative about "the" colour of a place, because there is no
  such thing.
- **This does not forbid RGB everywhere in the workspace.** `windows/vessel`'s
  plan renderer and `windows/scene`'s ASCII fallback both emit triples, and
  both are themselves *views*. The rule is that a producer of world-state does
  not decide appearance — not that no crate may ever name a colour.
