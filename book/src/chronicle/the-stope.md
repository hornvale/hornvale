# The Stope

*A stope is the void a working opens as it takes ore out. It is the shape a
mine leaves behind, and it grows.*

[The Underworld](./the-underworld.md) gave every chamber real conditions — a
depth rung, a rock stratum, a water table, a routed temperature, a named
community. [The Adit](./the-adit.md) gave a chamber an interior: a partition
tree carved by four generators, flooded and stitched, derived fresh from a
seed on every call. What neither gave it was **extent**. The underworld shipped
as a five-storey building with four stairwells, where each storey was exactly
one room, and the address carried an `entrance` field that every caller in the
tree passed `0`.

This campaign gives the rock somewhere to go. A branch owns a character; a
character owns a run of floors; an engine owns a run. Interleaving is
impossible because an engine cannot reach across a branch boundary — that is
the whole design, and everything below is what it cost and what it measured.

## The epoch, and why it was one epoch rather than three

The chamber address is the seed-derivation key. `chamber_key` formats the whole
address into a stream label, so *any* change to the address relocates every
chamber in every world. Three changes rode one epoch, `chamber/v3`:

- `ChamberAddr` gained a **`floor`** — the rung the lattice was missing. A band
  used to be one interior-less point per column, so a key that did not spell
  `floor` derived one stream for every floor of a run.
- **`slot` became `branch`.** "Slot" reads as a position and the thing is an
  identity: a column that persists downward, which is NetHack's Gnomish Mines
  relation and had been sitting in the address unused since the beginning.
  `SLOTS_PER_BAND` became `BRANCHES_PER_SYSTEM` with it — the old name's "per
  band" had never been true.
- **`Sunless` became `Nadir`.** The word was carrying a promise the ladder does
  not make. The rung is the open-ended bottom bin above an *authored*
  habitability ceiling, so it means "past where the model stopped", not
  "extraordinarily deep" — and leftover buckets are large by construction:
  38.65% of cave systems terminate there, pooled over the seed panel.

  **The spec predicted this rung would be rare, and the refutation was already
  sitting in the tree when the prediction was written.** The ladder's own
  module documentation had carried the per-seed shares — 24.5% / 43.2% /
  42.3% — since The Underworld laid the ladder down three days earlier
  (`eb9921af7`), and Task 0's measurement reproduced them to the unit. The
  only arithmetic the campaign added was the pooling. A measurement that can
  end a campaign was, in this instance, a table nobody had re-read.

An **epoch suffix**, never a rename in place. The label is the contract: a
world is a seed plus a ledger and everything else is re-derived, so renaming
`chamber/v2` would leave two incompatible worlds answering to one name with
nothing able to tell them apart. `chamber/v1` and `chamber/v2` are retired and
must never be reused. And the rung rename was free *only* if it rode this
epoch — `chamber_key` spells the rung's **name**, through an explicit match
table rather than a `Debug` impl, so renaming it at any later date costs a
second full epoch.

**The keystone did not move, and the reason is the interesting part.** Every
chamber in every world relocated, and the committed seed-42 world JSON is
byte-identical: `lens_purity`, `repose_byte_identity` and `deep_realm_rehome`
— the fixture's only three consumers — pass unchanged across the epoch. A
chamber is never stored. Existence and content are pure functions of an
address, so moving all of them moves no serialized byte. This is the clearest
demonstration the project has taken that deriving rather than storing actually
buys something: an epoch in the underworld's addressing cost the save format
nothing at all.

One prediction in the spec was wrong and is recorded as a null. It said the
drow seating would move, because seating reads chamber addresses. It does not:
seating reads the rung and the column and never asks whether a chamber exists,
so every seating figure is byte-identical across the epoch.

## The headline: the campaign passed its own falsification test

The criterion was frozen before the code that would move it. *A world that is
90% generic cave has not solved the oatmeal problem* — so if one character took
more than 80% of branches, the campaign had failed and that was the headline,
not something to re-weight away afterwards.

```text
share of branches by character        denominator: 6,136 realized branches
  seed        WildCave     FungalGardens      DrowTier
  42       917  65.13%      420  29.83%      71   5.04%
   7      1761  64.79%      815  29.99%     142   5.22%
  1234    1326  65.97%      597  29.70%      87   4.33%
  POOLED  4004  65.25%     1832  29.86%     300   4.89%
```

The largest share is **65.25%**, stable to 1.2 points across the panel, well
under the ceiling. The criterion is cleared.

**"Solved" is a stronger claim than the criterion makes**, and the campaign
says so in its own readout rather than letting the pass stand unqualified. Two
thirds of every branch in every world is still one thing; and 65 / 30 / 5 is
almost exactly what the character draw's authored weights say, which means the
number is a *setting* the campaign chose and not a structure it produced. What
the measurement establishes is that the lattice is not reshaping the draw — the
variety survives contact with the world — and that is worth knowing, but it is
not the same as the world having generated variety on its own.

## The finding that outranks it

The same probe measured how a branch reaches the bottom, and the answer was
the campaign's real result.

```text
over all 6,136 realized branches
  realizes a Nadir chamber in the lattice   1928 / 6136   31.421%
  reaches the Underdeep BY WALKING            27 / 6136    0.440%
  reaches the Nadir     BY WALKING            22 / 6136    0.359%
```

Nearly a third of branches have a chamber at the bottom of the ladder. About
one in three hundred can walk to one. The deep exists almost everywhere and is
almost nowhere reachable.

**Both Nadir figures were wrong when first published, and the error was a
conditioning one.** The counters were incremented inside the test for having
reached the Underdeep and then printed against the *unconditional* branch
population — a conditioned numerator over an unconditional denominator. The
walk rate read `0.163%` and the lattice rate `31.160%`; the true unconditional
figures are `0.359%` and `31.421%`. The nesting is *correct* for the
conditional question that was separately preregistered, whose denominator is
conditioned the same way, which is exactly why one counter could serve two
questions and be right for only one of them.

**What the conditioning was deleting is better than the correction.** Of the 22
branches that reach a Nadir chamber by walking, **12 never reach an Underdeep
chamber on the same branch.** You arrive at the bottom without passing through
the layer above. That is the majority of the population, and the conditioned
counter had been erasing precisely it.

## The mechanism was wrong too, and the true one is a lever

The first account of the 0.359% was a closed form: the descent rule makes a
band's exit its last realized floor, so traversing an *n*-floor run costs about
`0.5^(n-1)` against the fixed existence coin. Percolation. It is a checkable
story, and it fails two independent checks — which is why the evidence is now
printed on every run rather than asserted in prose.

**Route ablation.** Restrict the walk to the surface head, which is the only
route the closed form describes:

```text
  nadir reached, all mouths                  22 / 6136 = 0.359%
  nadir reached, entrance 0 only              2 / 6136 = 0.033%
  nadir reached, no lateral moves            12 / 6136 = 0.196%
  nadir reached, entrance 0 AND no lateral    0 / 6136 = 0.000%
```

**91% of Nadir reaches never descend from the surface at all**, and the last
line is the sharpest form of it: the pure spine-descent route the closed form
describes delivers *exactly zero* branches. The two the head-only walk does
find get there by stepping sideways onto another branch first.

**Density sweep.** `0.5^(n-1)` predicts a constant elasticity equal to the
number of floors that must exist — at least 10 by the frozen ranges, and 19 to
28 for a typical spine. Measured, across existence densities 0.3 / 0.5 / 0.7 /
0.9, the elasticity runs **1.75 → 4.41 → 9.37**: not constant, and every value
three to fifteen times below what the form requires. The absolute prediction
misses by five orders of magnitude.

The true route is a **door**. A side entrance does not descend; it maps to its
branch's root floor, and that root band is picked *uniform over the parent's
realized bands* — so roughly one side entrance in five opens directly into the
Nadir band. Of 680 drawn side-branch mouths, 137 (20.15%) sit at the Nadir
band and 12 survive the existence draw.

> **The deep underworld is not nearly unwalkable. It is essentially only
> reachable through a door that opens directly into it.**

The difference matters because the second sentence names a **dial** and the
first names a constant. Weighting the root-floor pick toward the shallow bands
makes the bottom rarer without touching the geology, the ladder, or the
existence coin; leaving it uniform is the only reason the endgame is reachable
at all today. Nothing was retuned on the strength of it — no frozen table
covers this quantity, so it is reported with a ratchet band around the rate and
no invented verdict.

## What the frozen tables could not ask

Two of the preregistered questions turned out to be unanswerable, and both are
published rather than quietly dropped.

**The access gate had no consumer.** One table froze *P(Nadir access | branch
reached Underdeep)* with an intended answer of 0–10%, as though something gated
it. The barrier — one drawn scalar per branch, four named states from *sealed*
through *open* — is that gate in the design, and the same amendment that
introduced it deferred every *effect* of it to a later campaign. The shipped
access predicate consults neither the barrier nor the character eligibility
table; the barrier has no production consumer anywhere. So the question landed
on its "the gate is not gating" arm structurally, at 66% by lattice existence
and 70% by the character table, and could not have landed anywhere else however
the draws fell.

**The eligibility table does not gate existence, and the joint table shows it.**
The campaign's stated product is the joint distribution of character against
terminating band, and reading it against the eligibility table exposes
something the table was supposed to prevent: `FungalGardens` declares itself
eligible for the top three bands only, and **858 of its 1,832 branches (47%)
terminate below them**; `DrowTier` declares the bottom two and 49% of its
branches terminate shallower. That is not a contradiction in the data. It is
the same absence restated: eligibility constrains where a character may be
*met* through the junction network and constrains nothing about where its
branch's chambers *exist*. Both readings are defensible; the tree ships the
second; a reader taking the joint table as "where each character lives" would
be wrong for 47% of one row, and nothing else in the tree would tell them.

## The witnesses

Before this campaign, no path in the drift-checked artifact list carried one
byte of chamber content. You could have deleted every chamber from every world
and the whole regeneration would have produced a byte-identical tree — which
means three of the campaign's own tasks would have ended with "regenerate and
see what moved", seen nothing, and meant nothing. So a witness landed early: a
committed page rendering the chamber lattice of three worlds through the
shipped entry points, run by run, with the real derivation key printed in the
transect so the epoch label sits in the artifact's bytes as well as in its
draws.

It moved on every subsequent task, and twice it moved because it was wrong.

The first version walked each run's floors from zero to *the run's own drawn
length* — the same value the existence gate compares against — so the gate it
was built to witness could never fire on any address it asked about. Deleting
that gate outright, which deletes the whole of one task's contribution,
produced a byte-identical page. The walk now runs the lattice's own ceiling and
reports the drawn length as its own column, with a glyph for "past the run's
length" that reads zero in a healthy tree.

The second was the junction line. Junctions derive — never draw — the chambers
of neighbouring cave systems reachable at a shared band, so a shortcut is a
fact about the geology rather than a die roll on top of an epoch. The first
readout counted *ordered* links against *unordered* pairs, inviting the reader
to divide 1844 by 735; the truth is 922 links over 735 pairs. And "largest
network 57 systems" had unioned all five bands into one adjacency map before
sweeping for components — but a junction never crosses a band, so that
component is not one anything can walk. Per band, the largest is **27**, and
not one of the 735 pairs joins at every band. The word "walkable" is gone from
the page.

## What this is not

The barrier ships as a dial with no effects: what a thin barrier *does* — the
cursed frontier society, the incursion, the civilization-ending open portal —
is a later campaign. The reserved thaumic axis stays pinned at zero and the
test that holds it there stays green, because barrier thinness is a property of
a place in the underworld and that axis is a property of rock. Nothing
underground is charted; a possession still teleports to a point rather than
walking a level. And the shortcut network is band-local — the promise that two
points far apart on the surface can be close below is delivered *within* a
band, and a cross-band shortcut is the half still owed.
