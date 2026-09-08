# The Seedbed — decision ledger

Campaign: **The Seedbed** — a fourth corpus family measuring whether the world
*grows* known macro-regularities, seeded from Epstein & Axtell, *Growing
Artificial Societies* (1996).

Autopilot engaged (`campaign-autopilot`). G3 and G6 are hard stops.

---

## #1 [G1] — What shape does "can you grow it?" measurement take?

**Question.** Three candidate homes: (A) a fourth corpus family with its own
resolver; (B) an extension of the Domesday's `studies/expectations.json` +
`windows/lab/src/domesday/detect.rs`; (C) additional lab metrics only.

**Decision.** (A), a fourth family, following decision 0135's template — its
own directory, resolver, verdict vocabulary and artifacts — plus one thing
0135's family did not need: a regression guard.

**Why (precedent cited).** Decision 0135 opens a family when the *resolution
basis* differs: `tropes/` resolves against the concept registry, `systems/`
against repository facts, `sentences/` against the grammar. A macro-regularity
resolves against **measurement over the committed census**, which is a fourth
basis. 0135 explicitly accepts duplicated loader/renderer/ratchet across
families that will never share code.

**Alternatives discarded.**

- (B) rejected on provenance. Ideonomic negation of "frozen *external*
  catalogue" lands precisely on `expectations.json`: 30 claims Hornvale makes
  *about itself*, checked for internal coherence. A growth corpus imports a
  regularity from outside and asks whether the world reproduces it. Same
  machinery, opposite provenance — 0095's instrument/standard distinction makes
  them complements, not competitors. Folding one into the other would give
  `expectations.json` a provenance field it has no business carrying.
- (C) rejected on the whole precedent chain 0330 / 0577 / 0581 / 0583: a number
  recorded with no claim attached cannot be wrong, and every family has been
  forced to add a demonstration after shipping without one. `rank-size-slope`
  is the live instance — see #3.

**Ideonomy: 1 pass (negation + organon-construction/list; reversibility,
naturalness, direction), 0 overturns of the top-line, 3 material design
changes:**

1. (B) is the *negation* of (A), so the spec states the division of labour
   rather than arguing (B) down.
2. **Reversibility asymmetry.** A built parser stays built; a *grown*
   regularity dies the moment the history bake is retuned. Coverage in the
   other three families is a ratchet; here it can silently decay. This family
   needs a two-way regression guard the others never needed.
3. **The deferred set is principled.** On the direction axis the census records
   one steady endpoint per world, while the book's regularities are
   *oscillating* (migration waves, boom–bust) and *accumulating* (wealth
   concentration). The census is structurally blind to exactly those two, so
   trajectory items are deferred-with-anchor rather than omitted.

**Capture actions.** Discards routed in #4.

---

## #2 [Q] — What ultimately verifies a corpus verdict?

**Question.** Nathan, unprompted, during brainstorming: *"The eventual intended
form of verification of these corpuses is the documentation Hornvale provides
about its own capabilities. When you think about it, nothing else really
suffices."*

**Decision.** Adopted, with one restriction that is load-bearing: the anchor is
**generated, drift-checked** documentation, never hand-written prose. Mechanized
as a new anchor kind, `doc:<path>`, which resolves only if
`docs/generated-paths.txt` gives that path a generator — its second column names
the path's author, and `none(hand-written prose, never regenerated)` is refused.

**Why (precedent cited).** The directive extrapolates a trend the project has
already walked five steps of, each step forced by the previous one being
falsified: registry-token naming (0577) → hand-maintained declaration list
(0330) → `path:` anchor → `test:` anchor (defeated by `#[ignore]`) →
realization witness (0581/0583). Steps 1–5 are the program certifying itself to
itself; generated documentation is the first surface a reader outside the loop
can falsify. `windows/explain` is the existing instance of the principle: it
narrates by reading only the ledger, "which is how it validates that the ledger
is self-describing."

**Alternatives discarded.** Hand-written book prose as an anchor — rejected as a
restatement of the `IMPLEMENTED_DEMANDS` failure 0330 names: a declaration that
moves the score without moving the world.

**Ideonomy: 1 pass (tree-finding + timeline organon; complexity, symmetry), 0
overturns, 2 design changes:**

1. **Arity picks the surface.** A growth regularity is a property of a
   *population* of worlds; `explain`/almanac narrate *one*. A within-world claim
   can be narrated per-world; a cross-world claim belongs in the Domesday, which
   is already generated prose over the census. Two documentation homes, chosen
   by the arity of the claim.
2. **The honest bound, stated in the spec so nobody later mistakes it for a
   second witness.** A passage generated from the same census the verdict is
   computed from is the same fact rendered twice: it buys *reader-
   falsifiability*, not independent evidence. Its teeth therefore depend on the
   passage carrying the **criterion and the verdict**, not just the number —
   which is a change to the Domesday renderer, not merely a new anchor kind.

**Capture actions.** Generalizing `doc:` to the other three families is a
cross-family change this campaign will not make silently — `growths/`
demonstrates it first; the generalization goes to G3 flagged and, if approved,
to a decision record of its own.

---

## Follow-ups

*(populated as they occur)*
