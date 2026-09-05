# 0736. The provider-tier doctrine is retired

**Status:** Accepted (2026-09-04) · **Decider:** Nathan (autopilot, spec §7) ·
**Supersedes:** [0039](0039-epochs-replace-tiers-refine.md)'s remaining
provider-tier carve-out

In the context of *The Zenith* removing astronomy's `ConstantSun`, facing a
provider-tier doctrine whose last instance no longer represented two useful
fidelities of one truth, we decided that **coexisting, world-selectable
provider tiers are retired** — accepting that fidelity changes now happen
inside one provider or as an explicit epoch, never by retaining a second
provider as a selectable coarse world.

## Context

Decision [0039](0039-epochs-replace-tiers-refine.md) had already excluded
contradicting generators: they are epochs, not tiers. Astronomy was the
doctrine's last carve-out, but its purported tiers 1–3 all shipped inside
`GeneratedSky`; worlds chose only between that provider and the tier-0
`ConstantSun` stub. The four-rung ladder was therefore never four coexisting
providers. Once the stub stopped serving a live consumer, 0039's carve-out
became empty.

## The distinction that survives

Retiring providers does not retire acyclicity. `rotation=locked` remains a
supported pin on the generated provider, so a world with no day/night cycle
is still a derived Hornvale world. What disappears is the claim that such a
world needs a separate lower-fidelity provider.

The Constitution's eternal-noon question is correspondingly a census
question, not a tier comparison. The proper instrument is a locked pin set
over `census-of-faiths`' 10,000 worlds, read through
`pantheon-cyclic-share`. The mechanism exists, but **no committed study
currently uses a non-empty pin set**: every committed `pin_sets` entry has an
empty `pins` array. Adding `rotation=locked` is therefore future
preregistered measurement work under [0016](0016-studies-preregister-hypotheses.md),
not a result this decision claims has already been measured.

## Load-bearing consequence and scope

There is one astronomy provider. A refinement may add structure within that
provider without contradicting its established claims; a generator change
that changes world identity remains an epoch under 0039. Neither case
licenses another world-selectable provider tier. This retires the
Constitution's provider-tier doctrine across Hornvale, while leaving ordinary
resolution ladders and observation-time refinement untouched.
