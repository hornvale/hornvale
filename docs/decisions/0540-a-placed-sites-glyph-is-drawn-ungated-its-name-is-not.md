# 0540. A placed site's glyph is drawn ungated; its name is still withheld

**Status:** Accepted (2026-09-02) · **Decider:** Nathan (direct ruling) ·
**Relates:**
[0022](0022-sim-emits-data-clients-render.md),
[0536](0536-the-enterability-gate-is-a-site-not-built.md),
[0537](0537-a-placed-site-is-re-sited-to-a-facet-by-a-seeded-draw.md),
[0539](0539-a-sites-tier-is-placed-or-derived.md)

In the context of The Prospect making caves and exotic sites enterable, and of
a world map showing endless undifferentiated biome, we decided that **a placed
site's glyph is drawn whether or not the player has discovered it, while its
proper name remains withheld until discovery**, accepting that drawn-ness and
named-ness stop moving together.

## Context

The map drew only discovered sites. With 874 caves and 103 exotic sites on seed
42 all undiscovered at genesis, the map showed unbroken tropical seasonal
forest — nothing to walk toward. Nathan's ask was to break that up:

> I think it'd be cool to show placed sites on the world map. Just don't show
> their labels or reveal the labeling information about that. Right now we need
> to break up the endless expanses of tropical seasonal forest.

And, on what the withholding covers:

> show the kind glyph and withhold the name. We can say it's a cave, a village,
> etc, just don't give its name. We might have some descriptive text even ('a
> small village in the rolling hills of Blorble') at some point, but not its
> proper name or specific details about it.

## Decision

Two gates that used to be one:

| surface | undiscovered placed site |
| --- | --- |
| map glyph (`draw_feature_layer`) | **drawn** — the kind is visible |
| cursor readout (`resolve_chain_at` → `format_chain`) | **name withheld** |
| walk-band prose | names it — you are standing there |
| chamber prose | names it — you have entered |

Regional context is fair game; the site's own proper name is not.

**The two gates were already separable, which is why this is cheap.**
`ChainLink.name`'s own doc already said the name is *"never withheld or
replaced here; `format_chain` is what decides whether the reader sees it"* — so
withholding lives in one place, per link, and ungating the glyph does not reach
it. `MapSite` carries no name field at all, and `draw_feature_layer` writes
only a `char` and an RGB triple.

## Landscape extents are NOT included, and the reason is structural

A volcano stays discovery-gated, and the rationale first written for that was a
non-sequitur ("the terrain has no other way of saying it is there" argues for
*un*gating under this decision's own intent).

The real reason: **a volcano is a landscape extent that participates in the
cursor chain.** Its `FeatureId` flows through `resolve_chain_at`, so its
drawn-ness and its readout are coupled — ungating the glyph would move a
feature whose discovery semantics predate this campaign and reach Gate B, which
this decision deliberately does not touch. A placed site has no such coupling:
it is absent from the feature index entirely.

So the rule is not "drawn-ness follows internal representation" — that is a
description of the code, not a principle. The rule is: **this decision covers
placed sites. Ungating a landscape extent is a separate decision with its own
blast radius**, and nobody has argued it yet.

## Consequences

**Drawn-ness and named-ness are now independent axes**, and a test asserting
one must not be read as covering the other. The first test written for this
pair asserted *invariance* (`before == after`) rather than *absence*, and a
mutation leaking the name unconditionally left the entire suite green — because
a name that leaks in both states is invariant. Assert absence.

**"Co-location is not discovery" applies in both directions along the chain.**
Seeing an outer link tells the reader nothing about an inner one, and drawing a
glyph tells them nothing about a name.

**A site's kind is now public information and its identity is not.** That is a
deliberate asymmetry: the map's job is to make the world look worth walking
into, and a kind glyph does that without spending the discovery.
