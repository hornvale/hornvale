# 0127. An identity key and a discrimination key are different kinds

**Status:** Accepted (2026-08-11) · **Decider:** Nathan

In the context of `domains/history` deriving several `u64` keys from an
occupation record — one of which names every founder in every world — facing a
measurement that refuted the ratified design for widening that key, we decided
that **a derived key is either an *identity* key ("is this the same thing?",
which must read founding-side facts only) or a *discrimination* key ("give me a
distinct deterministic draw", which may read anything the record carries), and
that a key's definition says which it is**, accepting that the founder handle
is the second kind and therefore still moves when a community's ending moves.

**What forced it.** The Ell's spec ratified a narrower `founder_handle`: the
founding triple plus one hop of ancestry, *excluding* everything after the
founding, on the rationale that a founder's name must not depend on how their
community later died. Measured over seeds 0–999, that key collides in the
promoted cast of **732 worlds** and costs **1582** dropped founders, against
**2 / 2** for the key it was replacing and **0 / 0** for the widening that
shipped. It is 366× worse by colliding worlds and 791× worse by founders lost,
at the exact defect it existed to fix. The mechanism is not a hash accident: a
founding raided and closed in the year it opened, and its same-year successor at
the same site from the same parent, are identical in **every founding-side field
there is**, so no depth of ancestry separates them — a two-hop key produces
bit-identical values because the grandparent is identical too. Only a
post-founding fact can tell them apart.

**The resolution is structural, not a retreat to the old key.**

```rust
handle = fold(founding_key(occ, parent), ended, peak_population, FOUNDER_ROLE)
```

The identity step is a call to the named identity function, not arithmetic that
happens to agree with it; the discrimination tail sits visibly on top. The
restructure is byte-neutral — proved by a zero-diff rebaseline, by the world
golden passing without one, and by re-running the 1000-seed sweep rather than
carrying its result over.

**What this reclassifies rather than abandons.** "A founder's name must not be a
function of how their community later died" is `founding_key`'s rule, and it
holds there without exception. It was never `founder_handle`'s to hold:
`founder_handle` was being asked to be a discrimination key while wearing an
identity key's name and inheriting its rationale — one name, two meanings, no
marker, which is the same defect this campaign's unit repair addressed, one
level up and inside the repair itself.

**Consequence — the real, testable cost, named rather than waved at.** Every
post-founding field in the discrimination tail is a field a future campaign can
recompute, and each recomputation is another forced epoch that renames every
founder in every world. The time-asymmetry objection is *not* the cost: the
whole history is baked before anything is named, so nothing is future from the
key's point of view. The coupling is. Trimming was measured and refused on
evidence: `ended` alone leaves 5/1000 colliding worlds and `peak` alone leaves
5/1000, on nearly disjoint failure sets, with seed 447 holding two separate
pairs — one separable only by `ended`, one only by `peak`. Neither field alone
reaches zero, so there is nothing to trim.

**Consequence — the residual is documented, not asserted away.** Seeds 2634 and
2898 still collide under the shipped key (their colliding pairs' *parents* are
themselves twins). The fidelity cut that drops a colliding founder therefore
stays; restoring a fatal assert would stop two legal seeds from building and
reinstate the liveness bug it was authorized to remove.

**Consequence — the vocabulary is not yet applied everywhere.** Four derived
keys in `domains/history` fall under this taxonomy and two are labelled.
`material_key` is a discrimination key by this definition; `layer_key` is an
ordering key, a third kind this record does not attempt to define from one
example. A guard entailed by the key it guards is the shape to watch for while
sweeping: `founder_collision.rs`'s twin-parent assertion cannot fire while the
identity step folds the parent, and is a tripwire for a future narrowing rather
than a detector of a new collision shape.

**See also.** Decision 0051 (names are salted by stable identity, never by a
mint counter — whose specific hazard The Signet removed by making ids
lineage-derived; this key does not read an id at all); 0094 (a deliberate
duplicate shares its roster, never its derivation); 0089 (when an epoch
freezes); `domains/history/src/record.rs` (`founding_key`),
`domains/history/src/flesh.rs` (`founder_handle`);
`book/src/chronicle/the-ell.md`.
