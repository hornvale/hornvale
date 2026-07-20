# The Idioms

The Orrery had one way of drawing the world — a photoreal relief sphere — and a
row of *lenses* that only changed which datum it coloured. This campaign added a
second, orthogonal axis: the render **style**, *how* the world is drawn, and
shipped four of them for the joy of it. The lens chooses the subject; the style
chooses the hand.

## One world, every hand

The organising idea is that every pixel on this globe has real meaning behind it —
an elevation, a biome, a coastline, a settlement — so a style need not be a blind
screen-space filter. It can be a *faithful re-documentation of the same world in a
different representational idiom*. The invariant is the planet; the variable is the
tradition looking at it. And because the world is a deterministic simulation, each
rendering tells the truth in its own grammar: the pixel-art is the world as its own
sixteen-bit era would draw it, its palette distilled from the planet's actual biome
mix; the engraving is the world's own Age of Exploration, sepia hatching thickening
in shadow across cream paper; the cel-shaded globe is a modern illustrated atlas,
its coastlines inked; the watercolour is a naturalist's wash. The same seed, five
hands, every one of them faithful.

## The architecture: a skin over the frame

A style is a chain of screen-space passes — a small `EffectComposer` wrapping the
globe's renderer — swapped by a picker that mirrors the lens picker exactly.
Photoreal is the *identity*: an empty chain that renders straight to the screen,
byte-for-byte the old planet. Because a style transforms the finished frame, it
composes with any data lens for free, and it never touches the geometry pipeline
the previous campaign had just spent so long getting right. The one subtlety was
alpha: an intermediate render target does not carry the globe's opacity the way a
direct draw does, so the pipeline clears its target to opaque black — space stays
black, the globe keeps its colour, and each style reads a frame it can trust.

## What the screen-caught that the tests could not

The lesson of the whole campaign is written in the difference between *green* and
*correct*. Every one of the four shaders passed its unit tests and its type-check
and its build, and every one of the first two rendered a **black globe** anyway —
because the failures lived in the one place a headless test cannot see: the GPU.
The pixel-art's resolution uniform silently updated a dead copy (a shader clones
its inputs, and the update had captured the original), so its pixelation snapped
every sample off the edge of the image into blackness. The cel shader named a
variable `flat` — a reserved word in the shading language — and the *entire*
program failed to compile, wordlessly, to nothing. Each was found the same way:
not by a passing test, but by rendering the real globe through a real browser and
*looking* — then bisecting the black with a passthrough, an isolated pass, a solid
red, until the one broken line confessed. The tests were not wrong; they were
looking in the wrong place. What guards this now is a smoke test that renders each
style and asserts the frame is neither black nor unchanged — the visual pass,
finally, mechanised into a tripwire.
