# The Confidence Gradient

Not everything in Hornvale is equally understood, and this book would mislead
if it presented the settled and the speculative in the same voice. This
chapter is the standing map of which is which. The rule of thumb: confidence
runs bottom-up, and any claim about the top of the stack made with the
assurance of the bottom should be distrusted.

## Assembled from well-established patterns (high confidence)

The kernel substrate: hash-based seeding, coherent noise, append-only
event-sourced storage, deterministic serialization, triple-shaped facts.
Decades of precedent, thousands of implementations. Campaign 1a moved fast
*because* this layer was deliberately chosen to be boring.

## Precedented but nontrivial (moderate confidence)

- **Lazy retrospective generation** — committing detail only on observation,
  consistent with a statistical prior. *Caves of Qud* and *Ultima Ratio
  Regum* prove pieces of this can work; nobody has done it against a
  fields-plus-ledger substrate at this scope.
- **Multi-level-of-detail simulation** — *Dwarf Fortress* proves offscreen
  abstraction can work; keeping the seams invisible under our stricter
  consistency invariant is harder than what it attempts.

## Genuinely open (low confidence — the actual research)

1. **Refinement at scale.** Generating detail consistent with fields *and*
   a large committed ledger, with aesthetic requirements on top, is
   constraint satisfaction plus taste. Tier 0 is fifteen honest lines; the
   real thing is years away and may need ideas that don't exist yet.
2. **The phenomena vocabulary's generality.** The bet that one observation
   interface serves religion, language, historiography, *and* room
   description. Stress-tested against every new domain; if it cracks, it
   gets split by observer kind before it ossifies.
3. **Emergent economics that don't degenerate.** The mermaid-bone-farm
   problem: static value tables meeting exploitable production collapse into
   absurdity, and *most game economies are faked precisely because real ones
   misbehave*. Whether fields plus facts can host prices that flood, crash,
   and recover legibly is unknown. The economics campaign begins with a
   literature phase (experimental economics, auction theory, virtual-world
   economics), not with code.
4. **Historiography worth reading.** *Dwarf Fortress* generates accurate
   history that glazes eyes; the goal here — sparse, focalized, unreliable
   accounts with tellers — is a writing problem as much as a systems problem,
   and no amount of architecture guarantees it.
5. **The enrichment thesis itself.** That upstream richness (two moons, no
   axial tilt) produces *legibly different* downstream culture rather than
   noise. This is the year-one exit criterion: generate two worlds differing
   only in astronomy and see whether their religions differ in ways a reader
   can articulate. It is a falsifiable bet, and it is allowed to fail.
