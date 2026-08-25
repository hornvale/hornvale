# 0256. A host's testimony is fallible by construction, and the gap is the deliverable

**Status:** Accepted (2026-08-25) · **Decider:** Nathan · **Relates:**
[0226](0226-a-possessed-host-is-co-present-not-displaced.md) (the co-presence
this reads back),
[0258](0258-introspective-access-is-bounded.md) and
[0259](0259-conceptual-deficiency-is-derived-not-authored.md) (the two gaps),
[0197](0197-co-location-is-not-discovery.md);
[The Confidant](../../book/src/chronicle/the-confidant.md)

In the context of a possessed body that now computes its own drives, mode and
affect every tick with no route to the player, we decided that **the verb which
opens that route reports what the host can perceive and can name, never what
arbitration computed** — accepting that the player is given an account she must
interpret rather than a reading she can trust.

## Context

The obvious first version is a truthful host: surface the `AffectLabel`
arbitration reached, render it in first person, ship. It is smaller, it has no
new failure modes, and the gap can be added later.

It ships nothing. `needs` already reports each co-located creature's felt state
*through the same arbitration that drives it* — an omniscient read with no
evidence layer beneath it. A host that reports its arbitration accurately is
that same oracle wearing a pronoun. The player learns exactly what she already
knew, by a second route.

So the gap is not a refinement layered onto working plumbing. **It is the
entire reason to prefer testimony over the oracle**, and a version without it
has not shipped a smaller thing — it has shipped the thing that was already
there. This governs sequencing as much as design: no arc may land a truthful
host as an intermediate state and defer the divergence to a successor.

## Consequences

- **The arbitration is never printed to the player.** `ask` renders through
  `testify` (`windows/vessel/src/testimony.rs`), which resolves the host's true
  state to the culture's own word or to its nearest available substitute. The
  substitute is reported as the *substitute*; a mutation replacing the reported
  concept id with the true label leaks the truth and reddens the guarding test,
  which is what proves the divergence is real rather than nominal.
- **What lands in the player's knowledge store is the reported concept**, under
  `{body}::feels`, never the arbitration's own label. That is deliberate: a
  later measure comparing what was said to what was true compares concept ids
  to concept ids, and cannot confuse an untranslated string with a genuine
  misreport.
- **The listener end was already cut and needed no change.** `knowledge.rs`
  excludes `heard` entries from the ground-truth check by design, so a false
  statement can already be held as a belief. Testimony is the first producer to
  use that seam for its intended purpose.
- **What we give up:** the player has no in-world instrument that reads a
  creature's true affect through the host's own account. She infers the gap, or
  misses it. Recovering the truth is the world's job by other means, and the
  campaign deliberately did not build a second oracle to grade the first.
- **Two things this does not decide.** A host that *declines* to answer, or
  that answers falsely on purpose, needs a disposition-toward-rider model and is
  out of scope; so is a host that volunteers unprompted, which needs a selection
  policy and puts an unmeasured delivery layer on the critical path.

## See also

Spec `docs/superpowers/specs/2026-08-24-the-confidant-design.md` §2.1 (the
inversion pass that overturned the truthful build) and §3.1 (the two-stage
pipeline).
