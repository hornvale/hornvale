# The Chroma

Chroma is colour itself — the Greek root behind *chromatic*, the word the
eye documents use when they count their channels. `clients/game` had every
other channel a roguelike is supposed to be best at: glyph carried identity,
the grid carried position, weight carried attention, and every drawn cell
named its provenance. What it did not have was ink. The `Ink` enum shipped
with one variant (`Plain`) whose own doc comment read "Monochrome for this
campaign; colour is deferred." This campaign collects the deferral.

## The data was already on the wire

The campaign's defining discovery was negative: there was almost nothing to
build on the sim side. `PaletteEntry::color` had ridden `vessel/plan/v1`
since it was emitted, mirrored by the client and read by nobody. The
walk-band chart arrived through `surrounds_scene_colored_in` — tinted, per
cell, through the possessed creature's actual `Sight`, with the sight block
riding along since The Beholding. Both facts were verified against committed
fixtures before the plan was written: seed 42's turn-0 chart carries colour
on all 31 cells; the chamber palette carries it on 32 of 37 entries. The
whole campaign lives in `clients/game`, outside the cargo workspace and
outside determinism (decision 0055) — zero workspace changes, zero schema
bumps, zero epoch risk.

## One gate, everywhere

`Ink::from_wire` is the single place a wire colour claim becomes ink:
absent means "no colour claimed here," never black (the producer's own rule
in `session.rs::tint`, now mirrored client-side); `NO_COLOR` set means the
reader declined colour, and the buffer itself goes monochrome at cell-build
time — degradation is observable in the cell grid, not silent at render
time. The terminal backend emits truecolor unconditionally, for the reason
the producer already documented: a terminal that cannot show truecolor
degrades to an uncoloured glyph, never to a wrong one.

The withholding rules mirror the producer exactly: the `@` mark and every
mark draw untinted, because marks are identity and identity belongs to
glyph — colour carries substance, and is the channel
that may fail).

## The caption is the honesty

A coloured chart names what it claims to be. When the walk band renders
tinted, the map strip appends a disclosure read off the document's sight
block — for seed 42's possessed bugbear: *seen through a bugbear's eyes —
yellow-blue sight; the red-green axis is not carried.* The lens rule —
aimed at the caption: nothing on screen is exempt from having been chosen.
The caption suppresses with the colour (`NO_COLOR`), because a fidelity
claim above a monochrome render would be the dishonesty it exists to
prevent.

## What colour found

The first thing full-colour rendering discovered was a finding about the
sim, not the client: **lit cells render darker than ignorant ones.** The
producer's "an unlit cell is absent from the light field" emits no colour
claim, so never-seen rooms draw in the terminal's bright default while the
torchlit chamber you stand in draws near-black. Both rules are honest;
their composition is backwards. Registered as
[[CLIENT-unlit-is-uncoloured]] — a producer-side light-model question for a
future campaign, deliberately not fixed here.

A diagnostic sketch rode along: `cargo run -p hornvale-game --example
vision -- --vision natural|rainbow|gray` renders either band under
reference mappings that make no fidelity claim — for checking what the
glyph and weight channels carry alone, or simply seeing the map.
