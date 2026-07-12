# The Confidence Gradient

Not everything in Hornvale is equally understood, and this book would mislead
if it presented the settled and the speculative in the same voice. This
chapter is the standing map of which is which — and, unlike the world it
describes, it is meant to be **re-scored as campaigns resolve its questions**,
not written once and left.

The honest axis is not *how established the pattern is* — precedent was never
the thing at risk. It is **whether the world can grade itself on the claim, or
whether the claim rests on a human's judgment.** A bet the Laboratory can
score — generate the evidence, measure it, drift-check the number — is a bet
the project can drive to a verdict on its own. A bet that resolves only
against taste stays open for a structural reason, not for lack of effort.
Confidence still runs bottom-up; but the gradient it runs along is
*checkability*, and any claim about the top of the stack made with the
assurance of the bottom should be distrusted.

## What the world can already check itself on (high confidence)

**The kernel substrate.** Hash-based seeding, coherent noise, append-only
event-sourced storage, deterministic serialization, triple-shaped facts.
Decades of precedent, thousands of implementations, and now stress-tested
under nine domains and five windows without cracking. This layer was
deliberately chosen to be boring, and the choice paid.

**The divergence method** — once the year-one research bet, now the project's
own instrument of proof. Generate two worlds differing in a single pin, hold
everything else, and measure whether the downstream culture differs *legibly*.
Year 1 varied the sky: the same land and society under a spinning sky crowned
the cyclic [Wheel-Turner](./gallery/the-gods-seed-42.md), and under a tidally
locked twin crowned the eternal Still Crown — a world with no seasons to
mythologize ([Campaign 5](./chronicle/campaign-5.md)). Year 2 inverted it,
varying the observer and holding the sky: two species differing only in their
authored parameter vectors grew different languages and religions, verified by
a 500/500 null control and a blind-attribution metric pinned honest at 0.875
([The Meeting](./chronicle/18-the-meeting.md)). This is no longer *the actual
research*; it is how the research checks itself, and it is applied afresh to
every new layer.

**The phenomena interface generalizes.** The bet that one salience-ranked
observation interface could serve religion, perception, and historiography
without any consumer learning which system produced a phenomenon has held
across every domain that has tested it. One caveat corrects the original
forecast: *room description* was expected to ride the phenomena channel too,
and instead took a cleaner road — the semantic query surface, where the sim
emits quantities and the client renders them ([The Scene
Window](./chronicle/21-the-scene-window.md)). The interface is more general
than feared; it is also not the only interface, and that turned out to be the
right shape.

## Precedented but nontrivial (moderate confidence)

- **Lazy retrospective generation** — committing detail only on observation,
  consistent with a statistical prior. *Caves of Qud* and *Ultima Ratio
  Regum* prove pieces of this can work; nobody has done it against a
  fields-plus-ledger substrate at this scope, and the observe-then-commit loop
  is not yet built. Its self-scorable half is named below.
- **Coarse constrains fine.** The design principle — a `ConstantSun` and a
  generated star system are both valid; higher fidelity refines and never
  contradicts lower — *shipped*, and holds from astronomy through religion's
  tiers. Crust sharpened it into a stated contract (decision
  `identity-computes-on-the-canonical-grid`): the terrain quantities that are
  *pointwise* — crust thickness and age — are stateless `Field`s any grid may
  resample, while the *mesh-bound* ones (sea level, drainage, placement)
  compute once on the world's canonical grid. So the pointwise half of the
  substrate is now genuinely resolution-free: the render lens samples the
  elevation field below cell scale, and the crust field byte-agrees across
  nested grid levels. The *Dwarf Fortress* move it is sometimes conflated with —
  runtime level-of-detail, refining an *active region* on the fly with the seams
  kept invisible — was the mesh-bound half the field/grid line isolated as the
  remaining work, and [The Room Mesh](./chronicle/the-room-mesh.md) has now laid
  its foundation. A room is a triangular face of the *same* icosphere refined
  deeper, so a level-7 room literally *is* a level-7 triangle: the seam problem
  that made active-region refinement look risky is dissolved structurally, not
  patched, and the dissolution was oracle-validated to `max|Δ| = 0` across all
  327,680 faces of a level-7 globe. Local detail is now summonable per-address
  at arbitrary depth for zero global cost, through an O(1) integer neighbour walk
  and coarse-field inheritance hooks. What is *not* yet built is the layer that
  consumes this substrate: the runtime active-region swap itself, its delta
  store, and the spike-validated adaptive-depth walk that lifts the uniform-depth
  restriction — all deferred, all resting now on a substrate that exists. The bet
  has moved from *no mechanism* to *mechanism shipped, composition pending*.

## Genuinely open — split by whether the world can grade itself

The remaining low-confidence bets do not sit at one altitude. Each has a
**self-scorable half** the Laboratory could close on its own, wrapped around a
**taste-gated half** that waits on a human read. Naming the seam is most of the
progress.

1. **Refinement at scale.** Generating detail consistent with fields *and* a
   large committed ledger, with aesthetic requirements on top, is constraint
   satisfaction plus taste — and the two halves have very different horizons.
   *Consistency* is self-scorable today: a generated detail either violates a
   committed fact or a field prior or it does not, and that is a metric, not a
   judgment. *Aesthetic quality* is taste, and it is the half that is years
   away and may need ideas that don't exist yet. The honest move is to build
   the consistency tier — checkable now — and stop letting the taste half make
   the whole problem look untouchable.

2. **Emergent economics that don't degenerate.** The mermaid-bone-farm
   problem: static value tables meeting exploitable production collapse into
   absurdity, and most game economies are faked precisely because real ones
   misbehave. Here too the bet splits. *Degeneracy* is self-scorable — an
   exploit detector is a Lab study: run the production loops, measure whether
   any yields unbounded value divergence. Whether prices flood, crash, and
   recover *legibly* is the partly-taste remainder. The economics campaign
   still begins with a literature phase (experimental economics, auction
   theory, virtual-world economics), but that phase now designs the apparatus
   that would falsify the claim, rather than standing between the project and
   knowing how it will grade itself.

3. **Historiography worth reading.** The systems half is no longer
   architecture-less: any entity's committed facts already replay into a
   derivation, physical deep time now lays down glacial strata and fossil
   shorelines ([Deep Time](./chronicle/deep-time.md)), and the past is being
   made queryable so that `why <ghost-town>` can recount the ice age that
   emptied it. Its measurable properties — focalization, sparsity,
   unreliability — are the guardrails. But *worth reading* is honestly
   taste-gated, and this is the one place the project refuses to fake a metric:
   *Dwarf Fortress* generates accurate history that glazes eyes, and no amount
   of architecture guarantees the sparse, focalized, unreliable account with a
   teller that would not. The human read is the real gate, and it is allowed
   to withhold a pass.

## The standing horizon

Year 1 varied the world and held the observer; Year 2 varied the observer and
held the sky. The current research varies **time**: two worlds identical at
genesis but differing only in their deep-time forcing, and a legibly different
present — different glacial strata, fossil shorelines, refugia, ghost-town
lineages, and history-derived myths — every divergence recountable through the
event ledger to its cause in the past. The falsifiability teeth are the same
shape as before: a blind-attribution metric over many thousands of worlds,
against a zero-forcing null control whose present must be indistinguishable
from its own genesis. It is a bet at the top of the checkability gradient, and,
like the two before it, it is allowed to fail.
