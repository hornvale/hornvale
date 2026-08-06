# The Tilth

A carrying-capacity field stood under every settlement in every world, and it
did not know who was standing on it. The land had a number; the number was the
same number for a kobold and for a human. Every people was then placed by
comparing its authored preferences against a value that had already been
computed without reference to any of them.

The Tilth gave capacity a second index. A cell is no longer worth *an amount* —
it is worth an amount **to someone**, and the someone is part of the arithmetic
rather than a filter applied afterward.

## What a shared field cannot say

The defect is easiest to see from its consequence. A people authored for desert
could be genuinely present in the only desert on its world and still rank below
peoples with no affinity for desert at all, because capacity is a supply term
spanning orders of magnitude multiplied by a condition product bounded in the
unit interval. An authored niche can modulate the primary-production signal. It
cannot select against it. A species that belongs somewhere loses to a species
that merely tolerates it, whenever the ground is productive enough.

The Vacancy had recorded that shape a campaign earlier. What was still missing
was the observation that the field being modulated is *the wrong object* — not
mis-weighted, but under-indexed. There is no weighting of a species-blind field
that recovers a species-specific answer, for the same reason no rescaling of a
scalar recovers a vector.

So `per_species_capacity` computes `K(s, c)`: for each people, a full field. The
bake holds `caps[people]` indexed densely, resolved once at the moment a
community opens rather than looked up by name on every step, and the siting rule
changes from "is this ground habitable" to "is this ground worth anything to
*them*". A cell where a people has no capacity is not a refuge for them, whatever
it is for someone else.

## Two attempts to fix a gate, both reverted

Two stages of this campaign were landed, measured, and taken back out.

The response curves in `ConditionNiche` sit as four peers under a single
minimum: temperature, moisture, insolation, elevation. Three are floored by a
sovereignty term so a well-defended people is never wholly excluded; elevation
was passed a bare zero. Stage 6 floored elevation to match the others. Stage 7
went the other way and unfloored temperature. Each fixed the axis it aimed at
and created a new defect on the axis it left alone.

The reason is worth stating as a rule, because it is not obvious and it cost two
implementations to learn: **mixing floored and unfloored axes under a minimum is
unstable by construction.** A floored axis can never bind. Whichever axis is
bare therefore decides the outcome everywhere it dips below the others' floor,
and the model's answer is determined by a bookkeeping detail rather than by
ecology. Stages 6 and 7 are the same bug on two different axes, and no
arrangement of floors resolves it — the defect is the flatness of the
constraint structure, not the height of the floors. The successor is a two-tier
split, gates times modifiers, which The Tense specifies and which sits in the
tree unwired.

The doc comment on `tolerance_liebig` records both attempts with their numbers,
so the next reader who thinks of either does not have to pay for them again.

## A gauge that re-measured its own anchor

One instrument nearly falsified this campaign's arithmetic in the quietest
possible way. `tilth_probe` re-derives the capacity scale constants `K_m` and
`V_max` against a reference, and it recomputed that reference on every run. A
gauge whose target moves with the thing it measures cannot report drift; it
reports zero, always, and looks healthy while doing it. It would have silently
re-gauged `V_max` from 140.2 to 118.9 and called it agreement.

It is now frozen at a fixed anchor with the drift reported beside it, and
`V_max = 140.2` was re-derived rather than assumed. The general lesson —
evaluate the curve, not the constant, and never let an instrument's zero float
— is the same one decision 0106 reaches from the direction of provenance: a
justification that cannot fail is not a justification.

## What it left

Per-species capacity is a strictly finer object than what it replaced, and it
made the next question askable. If a cell's worth depends on *who*, the obvious
neighbouring question is whether it also depends on *when* — and the answer
turned out to reorganise the whole model. That is [The Tense](./the-tense.md),
which ran as this campaign's later stages rather than as a branch of its own.
