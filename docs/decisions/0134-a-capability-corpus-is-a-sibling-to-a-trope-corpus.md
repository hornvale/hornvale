# 0134. A capability corpus is a sibling to a trope corpus, never a member

**Status:** Accepted (2026-08-15) · **Decider:** Nathan · **Relates to:**
[0011](0011-studies-are-data-metrics-are-code.md),
[0016](0016-studies-preregister-hypotheses.md),
[0022](0022-sim-emits-data-clients-render.md),
[0095](0095-a-corpus-is-an-instrument-never-a-standard.md)

In the context of scoring Hornvale against external catalogues, facing the
question of whether an external catalogue of *game-system capability* — a
tutorial, a feature list, a mechanics inventory — belongs in `tropes/`, we
decided that **it opens a second corpus family at `systems/`, with its own
resolver, its own verdict vocabulary and its own artifacts**, accepting the
duplication of a corpus loader, a report renderer and a byte-ratchet across
two families that will never share code.

**The two families measure different subjects.** `tropes/` asks whether a
**world** can represent a situation. `systems/` asks whether a **program**
implements a capability. Sharing a resolver would force one of those questions
to be asked in the other's vocabulary, and one resolver with two incompatible
reading modes is the failure this record exists to prevent.

**They also have incompatible resolution substrates, and the wrong one fails
quietly.** The trope resolver scores requirement bundles against the concept
registry — predicates, concepts, phenomena. Roughly a third of a roguelike
tutorial is renderer work: colour, a sidebar, particle effects, bloodstains,
console layers, menu art. None of it has a concept-registry token and none of
it ever will, because decision 0022 puts rendering outside the ledger on
purpose. Resolved against the registry, every one of those chapters would read
*blocked by a dangling bundle* — a plausible number, near zero, and a category
error. A capability item resolves against **repository facts** instead: the
in-force decision index, the idea registry, and the source tree.

**Two schema commitments are ratified with the family**, because both are
cheap now and are format migrations later.

1. **The item unit is generalized from the first commit.** A corpus declares a
   `unit` and each item a `kind`. A tutorial's items are chapters; a feature
   inventory has no chapters at all. Keying the schema on `chapters` would
   force a migration on the first corpus that is not a tutorial — the exact
   corpus this family exists to admit.
2. **An ordinal reading requires a declared ordering.** A corpus states
   `ordered`, and the resolver may make ordinal claims — *first unmet item*,
   *longest satisfied prefix* — **only** for a corpus that declares it, and
   must refuse them otherwise rather than silently ranking by identifier. A
   tutorial's items form a pedagogical ladder where each item assumes the one
   before it, which is what makes *the first item this project cannot
   replicate* a meaningful sentence; a feature list has no such order, and the
   same sentence asked of one would be an artefact of sorting.

**What is not in this family.** A published spell list, a monster manual, or
any other catalogue read for *parameter calibration* — how many named units a
satisfying space carries, and at what composition depth — is a third thing
again. Same discipline under 0095, different job, different output type. It
does not join `systems/`, and merging the two would reintroduce the
two-reading-modes failure at one remove.

**Consequence.** `systems/*.system.json` is data and `cli/src/systems.rs` is
code, per decision 0011, exactly as the trope family splits. A corpus is frozen
before its first measurement and its item count asserted in a test, per
decision 0016, so changing it is a deliberate act. Verdict tallies render to
`docs/audits/system-matrix.md`, one row per corpus, beside — never inside —
the trope matrix. The cost accepted is real and permanent: two loaders, two
renderers, two ratchets, and a standing obligation on any future reader not to
"unify" them.

**See also.** `docs/audits/system-coverage-wolverson-2021.md` (the first
column); [The Compendium
chronicle](../../book/src/chronicle/the-compendium.md); [decision
0135](0135-a-coverage-verdict-cites-a-checked-anchor.md) (the verdict and
anchor rules this family resolves under); `CLAUDE.md`'s `systems/` directory
guide.
