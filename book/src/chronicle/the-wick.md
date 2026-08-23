# The Wick

A wick is the smallest part of a lamp and the one you trim to make the flame
brighter. The Chroma gave the TUI game colour and immediately found two
things wrong with what the colour showed: the torch was too dim, and the
rooms the torch did not reach were brighter than the rooms it did. This
campaign trims the wick and fills the dark.

## Four times the candles, same physics

The implicit torch now burns four times brighter at its source. The
inverse-square falloff — `ATTENUATION` and its shape — is untouched, and a
new test pins that shape as executable: light at Chebyshev distance 1 over
light at distance 2 must equal exactly (1+4)/(1+1) = 2.5, before and after
any future change. The Lantern had documented the attenuation constant as
un-tunable (the H4a fence, written to stop someone manufacturing darkness);
The Wick honours the fence by moving brightness at the source — physically
"carry more candles" — rather than flattening the gradient. In the seed-42
chamber palette the far torch-lit floors went from `[124,124,36]` to
`[231,231,75]`; near cells clamp at sRGB white, which is what a flame
beside a wall does.

## The dark claims a colour

The Chroma's finding ([[CLIENT-unlit-is-uncoloured]]): "an unlit cell is
absent from the light field" meant `color: None`, which the terminal renders
as its bright default — so ignorant rooms outshone torchlit ones. The Wick
gives every fabric a floor: cells no source reaches are bathed in a skyglow
ambient, `eyes::daylight_at`'s own spectrum at 0.02 scale, chosen by
rendering (wall fabric reads `[29,28,23]` there, against `[64,50,13]` for a
torch-lit neighbour at distance 4 — visible, clearly darker, honest). The
level is evidence in the constant's own doc comment. The other withholdings
stand: fabric that cannot be derived still claims nothing, which is why
thresholds — a doorway has no fabric — remain uncoloured, and an absent
observer still withholds everything.

The client changed not at all: The Chroma's reader honours whatever claim
arrives, which was the point of building it faithfully first.

## What the rebaseline showed

One fixture moved: the chamber session snapshot. The walk-band chart did
not (its light comes from the world star, not the torch), the gallery
transcripts did not (drawn under `Lens::Off`, unlensed by construction),
and the almanacs did not. The prediction was written before the
regeneration and the diff matched it.
