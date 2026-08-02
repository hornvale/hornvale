# The Witness

Column 34 of the committed census reads `0`. Not "usually zero" — every one
of a thousand generated worlds, without exception, the same value. The column
beside it, `karst-fraction`, takes a different value on every single row.
A census metric that never moves once in a thousand seeds is not measuring a
rare event; it is measuring an event the model cannot produce. `Hydro::Aquifer`
and its sibling `Hydro::Spring` had been unreachable since the lithology
substrate shipped, and the census had been saying so the entire time — the
column was there, drift-checked, published, and nobody had read it closely
enough to notice that "rare" and "zero" are different claims.

That was one of three branches this campaign found could never fire, and all
three turned out to share a cause: **a check whose input was authored rather
than derived.** A hand-built test object, a fixture ordered to hide a missing
condition, a hand-maintained list nobody updated — each one certified a
function while saying nothing about whether the world's real machinery ever
called it with the values that would exercise it.

## F5 — a threshold authored on the wrong rock's scale

The first hypothesis was that `porosity` was missing terms: it summed a
carbonate-dissolution component and a metamorphic-recrystallisation component
and nothing else, so a *non-carbonate* rock with high porosity — sandstone,
the textbook aquifer — did not exist in the model. Adding a grain-packing term
seemed like the fix, and a sweep was run to calibrate its coefficient.

The sweep came back **BLOCKED**. Across the swept range the ceiling on
non-carbonate porosity worked out to `0.325 + 0.423·k_g`; reaching the
`porosity > 0.5` gate needed `k_g ≥ 0.414`, nearly outside any value that
would leave the rest of the model recognisable. No coefficient in the
sensible range could ever cross the line. That failure was itself the clue:
measuring the realised distribution directly showed clastic porosity
maxing out at exactly `0.325`, carbonate porosity running `[0.35, 0.65]`,
and the two classes **never overlapping**. `0.5` did not sit above the
clastic range by a little; it sat inside the *carbonate* range, on the far
side of a branch — `Karst`, gated at `carbonate > 0.5 ∧ porosity > 0.4` —
that pre-empted it on every cell that could ever reach it. The threshold
was authored at a carbonate scale and silently governing clastic rock; the
window it was meant to select was empty by construction, not by bad luck.

The repair that shipped adds the grain term for a different reason than the
one first proposed — not to *cross* a threshold, but to give clastic
porosity a **range** at all (without it, porosity is one value, `0.325`, on
about 90% of clastic land, because the inputs feeding it are themselves
nearly binary). A threshold placed mid-band then selects old, coarse,
weakly-cemented crust, which is what an aquifer geologically is. The first
cut of this repair shipped a threshold at the low end of the new band and
put **69.64% of land** into `Aquifer` before anyone noticed, because the
guard written for it only checked that the branch could fire — a floor, not
a ceiling. The corrected threshold sits near the middle of the band instead
(aquifer 16.4% of land, springs a sparse 3.69%, forming lines along aquifer
margins the way a real spring line does). `Spring` itself moved out of the
pointwise petrophysics entirely: it is no longer a threshold at all, but a
**descending contact** — an `Aquifer` cell with a lower, non-aquifer
neighbour — computed by the provider once the geosphere is in hand, on the
same precedent a prior campaign set for splitting pointwise readings from
their geometric promotion. The drainage gate it replaced, `> 500`, had never
once been reachable either: land drainage tops out at 219.

## F7 — a rule that cannot condition itself, and two that could never fire at all

`evolve`, the function that walks a drawn sound-change cascade over a word,
opens with no pending tone conditioning and only sets it when an earlier rule
in the same cascade actually merges two segments. A `Tonogenesis` — the rule
that turns a lost consonant's voicing into pitch — drawn *before* any such
merger therefore has nothing to condition on and returns its input unchanged,
on every word, in every language, provably, from the function's own first
line. In the settled cascade that wastes a slot among two to four; in the
one-to-two-rule wear cascade that governs how place names erode, it can be
the cascade's *entire* content. The repair makes the draw itself
position-aware: a cascade may not pick `Tonogenesis` at a position where no
merger has yet been drawn. Draw *count* is unchanged — one `pick` costs the
same regardless of how many kinds it can land on — so only the drawn values
move, not the shape of the draw.

Building the witness guard for that repair — sweep a small seed set, require
every rule kind to be seen changing a word, derived from the kind enum itself
rather than a hand-written list — found something the fix did not touch.
**Every currently shipped species has `tonality: 0.0`.** Tonogenesis needs a
toned vowel in the phonology's inventory to write to, and a phonology only
admits one above zero tonality; for the entire placed bestiary, the write
never lands, position notwithstanding. A second, unrelated finding turned up
in the same sweep: `VowelShift` needs the phonology's vowel inventory to
admit two adjacent heights at the same backness, which only happens once a
species' vowel-space trait clears roughly `0.7` — and every shipped species
sits at `0.5` or below. Two of the cascade's six rule kinds were decoration
for every language the world currently generates, for two unrelated reasons
that had nothing to do with the position bug. The repair widens to match: a
cascade may not draw a rule it cannot condition, *or* that the drawing
phonology cannot host, checked against the same facts the rule's own
application logic reads. Confirming either kind can still fire at all needed
a hand-built probe species — real tonality, real wide vowel space, never
placed in a world — because no seed of any real, shipped species could ever
supply one.

## F13 — teaching the second opinion what the first one learned

The lab keeps its own, deliberately independent restatement of which
concepts a world's worth of terrain and culture actually expose to a
people — independent on purpose, because a check that called the production
classifier back would be an echo of the thing it exists to check. That
independence has one failure mode, and this campaign found it running for
the third time in as many campaigns: the restatement's *roster* — which
concepts it even considers — had not learned six staple crops a recent
campaign added. `exposure-sound-{goblin,kobold}` read false on 767 and 759
of a thousand worlds, true only where a species happened not to be placed at
all; the worlds were correct the whole time, and the check was reporting a
defect in itself as a defect in them. The repair teaches the roster the
staples. Its own guard proved, by injection — remove a staple, watch the
test name it, restore it, watch the test go quiet again — that the check can
actually fail, which several of this campaign's own predecessors had not
verified about themselves.

## The keystone: derive the checklist from the type

Three repairs sharing one cause invite one guard, not three, and the shape of
that guard is what the campaign is actually named for: `windows/worldgen
/tests/exposure.rs` already sweeps a fixed seed set to prove a hand-written
concept list is real, and its own comment already names the gap this
campaign closes — nothing checked whether a rule that *list membership*
claimed actually fired anywhere. Generalised, that becomes three guards, each
deriving its checklist from a type rather than from an author:

- every `Hydro` variant must be witnessed on a real, unmodified derivation,
  swept at the production mesh resolution, not merely constructible by a
  test;
- every rule kind must be witnessed *changing a word*, not merely drawn;
- the lab's independent roster must consider the same concept set worldgen
  can classify, while continuing to compute its own answer for each —
  parity of roster, independence of predicate, the same principle a sibling
  campaign reached independently from a different defect (phenomenon-kind
  glossing rather than exposure) and ratified as a decision. Two campaigns
  arriving at one rule from two unrelated failures is corroboration, not
  coincidence.

A guard built this way cannot be satisfied by a hand-built fixture, because a
hand-built fixture is exactly what let each of the three bugs ship
undetected in the first place — a synthetic `MaterialBuffer` can encode a
porosity/carbonate pairing the real derivation never produces, and a synthetic
cascade fixture can put a merger first every time. Where the real generator
genuinely cannot supply a witness — no shipped species can host tonogenesis
or a vowel shift — the guard falls back to a hand-built *probe*, built once,
never placed in a world, existing solely to give the real drawing machinery a
channel to exercise a rule kind that cannot currently occur in play. That the
same test file needed both moves — sweep the real generator, then admit one
synthetic probe for the two kinds it structurally cannot reach — is itself
evidence the two questions are different, not the same question answered
twice.

## What moved, measured on one tree

One prediction was frozen before any repair landed: removing the unconditioned
tonogenesis draw would raise the wear cascade's match rate, and with it,
toponymic name survival. The registry's published headline for this — 14
surviving names out of 650, measured "across four sampled worlds" — turned out
to be unreproducible: no seed list was ever recorded, and no committed
instrument reproduces it. A new one was built to give the prediction something
to be evaluated against, on a named, committed seed list, restating the
cascade limb alone from public API so it measures the same quantity the
prediction was actually about rather than a blend of the cascade and an
unrelated positional-reduction step downstream of it.

The clean comparison — one merged tree, the same four seeds, this campaign's
two draw-time gates switched off and then on, nothing else different — reads:

```
rung 3  the drawn cascade ALONE alters:      117 (27.8%)  ->  250 (59.4%)
rung 4  rejected by the survival guard:       31          ->   96
rung 5  carry surviving wear:                 86          ->  154
```

The cascade's match rate a little more than doubles, and both the survival
guard's rejections and its acceptances grow in step with it, exactly as the
mechanism predicts. **H1 is supported.** An earlier cross-tree comparison of
the same instrument had shown a much larger jump — most of which turned out to
be an unrelated reseed picked up when the branch absorbed main mid-measurement,
not this campaign's own gates. Re-measuring on one tree, baseline and readout
under the same physics, is what separated the two; the rule that measurement
must not straddle an absorption is not decorative.

## What is not done

Two threshold questions were deliberately left for later rather than folded
into this campaign's own measurement: whether `Karst` should gate on flow
rather than porosity (moving both sides of one branch at once would have made
this campaign's own result impossible to attribute), and whether every other
terrain threshold sits inside the range its input actually realises, the way
this one did not. Terrain still has no versioned save-format label to record
that its derivations moved at all — a saved world's stamp says nothing about
whether the ground beneath it changed. Whether `spring` quietly duplicated
`river` for long enough that something downstream was calibrated against the
duplicate rather than an independent signal is worth a grep before it is
assumed innocent. And a wide-seed measurement of how much of a prior
campaign's isolate-versus-family divergence result survives having two dead
rule kinds removed from the roster that fed it is still open — three of four
of that result's original seeds hold up comfortably; the fourth, closest to
the boundary already, does not, and the difference is now known to be the
kind of confound this campaign exists to name. All five are carried forward
as follow-ups in the retrospective.
