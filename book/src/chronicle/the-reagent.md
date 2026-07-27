# The Reagent

*Campaign 1 of The Crucible — the material ground truth, and nothing that
believes anything about it.*

## What a substance is

A substance in Hornvale is now a bundle of eight **latent qualities** — fixity,
volatility, combustibility, solubility, malleability, density, causticity,
vitality — each a dimensionless ratio. A material is a bundle of them in the
same sense that a phoneme is a bundle of articulatory features, and for the same
reason: an atomic list of named substances would be a lookup table, and a lookup
table generates nothing. The composition is where the generativity lives.

Nothing in the simulation reads a quality. What can be perceived are five
**manifest signs** — heft, grain, lustre, odour, hue — each a partial function
of the qualities, and each with a deliberately different faithfulness. Heft is
very nearly density itself. Hue is not: it conflates causticity with vitality,
so a violently caustic mineral and an inert living thing present exactly the
same colour. The collision is exact rather than approximate — both compute to
0.45, with no floating-point tolerance required — and it is pinned by a test
carrying an instruction not to repair it.

This is the doctrine of signatures, mechanized. A practitioner who reasons *the
root is red, so it treats blood* will be wrong; and the wrongness was not
authored. Nobody wrote down a superstition. Somebody wrote down what is true,
arranged for perception to be lossy in a particular way, and the error follows.

## The grammar

Six processes — grind, calcine, dissolve, distil, ferment, amalgamate — and
seven productions written against them. A production names its preconditions on
the quality axes, what comes out, and which sign channel the reaction manifests
in. Admissibility is mass balance: the summed bulk of the outputs, fume and
residue included, must equal the input count. That single predicate is the
mundane tier's only invariant, and it is precisely what a later, gated campaign
would relax to admit magic — a production is a production whether or not it
violates a law the simulation enforces.

The chemistry is universal; every world has the same rules. Worlds differ in
what they are *made of*, and therefore in which productions they can reach — a
small delta in primitives becoming a combinatorial delta in what is reachable.

## Where it lives

The layering law forbids a domain from depending on a sibling, and a material
layer plainly needs to know about rocks. The resolution was already in the tree:
`hornvale_language` defines its own `Envelope` rather than importing the species
domain, and the composition root carries species data across with `envelope_of`.
So `domains/alchemy` defines `Substrate` — its own copy of the material
dimensions — and knows about qualities, not geology. `windows/worldgen` performs
the translation, mapping terrain's rock classes, soil orders, and ore deposits
into substrates, and deriving a world's substance samples from its generated
terrain and climate.

The domain draws nothing. No stream label, no seed, no randomness — it is a pure
projection over state other domains already drew, and so it adds no save-format
contract at all. That claim is now enforced by a test which reads the crate's
own sources and fails if a `Seed` appears, rather than by a promise in a
comment.

## Two things the campaign measured about itself

**The confound reaches appearance, but not behaviour.** Ore grade — the one
drawn quantity that reaches alchemy — moves causticity, and causticity moves
hue. It moves nothing else: no production requires causticity, so poor ore looks
different and behaves identically. Sweeping every commodity across the full
grade range leaves the admitted-production set invariant. The campaign's
governing image was a recipe that works in one valley and fails in the next;
what shipped is a reagent that *looks* like it should behave differently and
doesn't. Both are errors a practitioner can make, but they are not the same
error, and coupling grade to behaviour is the first task of the campaign that
follows.

**Reachable productions saturate.** At default terrain pins every seed reaches
all seven productions, because four ubiquitous source categories cover the whole
table — living matter alone unlocks four of them, and any silicate rock adds a
fifth. Genuine divergence appears only on a deliberately sparse globe, and there
it is one production wide. The measure that looked like it would distinguish
worlds mostly does not, which is worth knowing before a later campaign
preregisters a hypothesis against it.

Neither finding was visible from the design. Both came out of asking what would
happen if the code were wrong, and discovering — repeatedly — that nothing
would.
