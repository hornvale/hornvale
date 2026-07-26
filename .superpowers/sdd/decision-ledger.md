# Decision ledger — The Hearth (Rose Window campaign 1, as amended)

Autopilot engaged. G3 (this spec) and G6 (merge) are Nathan's.

**Ideonomy: seven passes**, run against the metaplan's three remaining open
questions (which rung, the first catalogue, the program name). Three overturned
earlier conclusions — two of them my own from the immediately preceding pass.
Pass 7 attacked the model by negation across four core claims and moved none of
them, which is the convergence signal I stopped on.

---

#1 [G1] — **Which rung of the furnishing ladder?** · **Decision: rung 1, where a
"pattern" is a relational fragment, not a room template.** · *Why:* pass 1's
*modularity* prompt caught that I had written rung 1 as "authored room template,
seeded pick" — which is precisely the catalogue-not-a-language failure
`CLIENT-language-not-catalogue` exists to prevent, arriving on the first rung.
Alexander's patterns are smaller than a room; a room is a composition. **This
also corrected the ladder's axis**: coordinates are always solved, so what
changes as you climb is *who decides the relations*, and old rungs 1–2 collapse
because variation falls out of composition. · *ideonomy 7 passes / OVERTURN of
the amendment's own ladder.* · *capture:* `CLIENT-furnishing-ladder` needs its
rung text corrected at close.

#2 [Q] — **Where does the pattern architecture come from?** · **Decision: copy
`domains/language/src/phonology.rs` — one authored inventory, per-culture derived
selection, an admissibility validator.** · *Why:* re-instantiating "authored
vocabulary + composition rules → infinite instances" lands on generative grammar,
and this repo has already built and validated that shape (a phoneme inventory,
an `Envelope` gating the draw, a `permits` predicate). Author the inventory,
derive the selection — decision 0009 exactly, and the same move The Bane made
deriving a threat niche from what a creature already is. Supporting precedent:
shape grammars generated plausible Palladian plans from ~8 rules, so rule counts
are small. · *ideonomy 7/0.*

#3 [Q] — **What is the sim's spatial substrate inside a room?** · **Decision: an
anchor GRAPH, not a lattice — 5–10 nodes, `SearchSpace` over
`kernel/src/astar.rs`.** · *Why:* pass 4's *symmetry* prompt showed the fine
layer is the coarse layer one scale down (rooms:ways :: anchors:relations), so a
room's interior is a very small room-graph and needs no lattice **and no
coordinate solve** for movement at all. Creatures don't think in (3,7); it turns
out they don't need to walk on it either. · *ideonomy 7/0.* · *consequence:*
movement is two-level and the doorway is the seam — an anchor that is also a
room-graph edge, which is why the threshold appeared unprompted in the first
catalogue.

#4 [Q] — **Which derived geometry is causal?** · **Decision: the anchor graph is;
the metric layout is not. Adopt the rule "outcomes read topology, never
metrics."** · *Why:* this scopes decision 0072 rather than contradicting it —
emergent concealment, cover and overhearing are all topological facts about the
pattern-composed graph, so they survive, while the render solve stops being a
determinism contract and becomes freely retunable. Stress-tested by negation
against thrown objects, missile reach and fire spread; each resolves
topologically, and no forced counterexample could be constructed. · *Standing
correction:* I called risk 2 moot, retracted it as too strong, and now partly
un-retract it — the resolution is that "moot" holds *only* under this rule, which
nobody had stated until pass 4. · *ideonomy 7/0.* · **0072 wants a refining
record (not an edit) — flagged at G3.**

#5 [Q] — **Do items need a third persistence tier?** · **Decision: no.** Items
are entities whose *position folds over custody events*, exactly as a creature's
folds over `agent-at`. · *Why:* pass 3 negated "items are committed" and found a
letter is derivable from committed events (who wrote, when, about what) — the
same way names, genealogy and history already work. What commits is the events,
not the object; and the metaplan already lists inventory among its folds. ·
*ideonomy 7/1 — OVERTURNS pass 2's own "committed items tier" finding.*

#6 [Q] — **How do committed room-modifications survive a furnishing epoch?** ·
**Decision: promotion on touch — an anchor is derived until interacted with, then
promoted to a committed entity that deltas reference.** · *Why:* a delta ("the
door is barred") that references a derived slot orphans when an epoch regenerates
the base. Promotion bounds the committed set to exactly what was interacted with
and is The First Mark's discipline (a mark commits; everything else re-derives). ·
*ideonomy 7/0.* · *flagged at G3:* v1 barely exercises it.

#7 [Q] — **Where does the relation vocabulary come from?** · **Decision: borrow
it. RCC-8 for topology (8 relations, JEPD, published composition table); the
topological/proximal/**projective** split from prepositional semantics; Allen's
interval algebra as the time analogue.** · *Why:* pass 6's *dictionary* organon —
this is a solved problem with verified composition tables, which is what "declare
the algebra" (pass 4) actually requires. Projective relations are frame-relative,
which is pass 3's per-observer finding arriving independently from linguistics.
**JEPD is demanded as a criterion** because a partition cannot be padded, which
structurally prevents catalogue sprawl. · *carve, by reversibility:* the predicate
algebra is the least-reversible piece and the object catalogue is purely
additive, so vocabulary is specified first and ships topological → proximal →
projective. v1 ships topological. · *ideonomy 7/0.*

#8 [Q] — **What is missing from the model?** · **Decision: anchors emit FIELDS
(warmth, light, sound), decaying over graph distance.** · *Why:* pass 5 crossed
the model against the five named activities and "read a letter" needs *light*,
which was nowhere — and light is not a relation, it is emitted and read at a
remove. That is `alarm_field`'s shape, already shipped three times. Warmth then
lands on the existing thermal drive, giving v1 its measurable outcome; and light
gates which relations an observer can read, giving pass 3's
concealment-as-perceptual-asymmetry its physical cause. · *ideonomy 7/0.*

#9 [Q] — **First catalogue contents.** · **Decision: the hearth, plus threshold,
bed, water vessel, and one pure sight-blocker — chosen so existing drives can act
on them.** · *Why:* the campaign must be *measurable* the day it lands, not merely
renderable, so first objects are ones shipped drives already consume (thermal,
fatigue, thirst, danger via the hazard vector). The hearth is simultaneously the
measurable case and the social one. · *ideonomy 7/0.*

#10 [Q] — **The program name.** · **Decision: keep `the-rose-window` for the
program; name this campaign THE HEARTH.** · *Why:* a rose window is many
fragments composing one image radially around a centre, which is what a presence
bubble is — the metaphor survived Amendment 1 better than expected. Hornvale's
convention names a campaign for its object (The Bane, The Mettle, The Haunt). ·
*ideonomy 7/0.*

#11 [G2] — **Spec self-review.** Placeholder scan clean; internal consistency
checked (§2.1's non-causal render vs §8's zero blast radius; §5's v1-topological
vs §9's scope). Every repo claim verified by command before writing:
`kernel/src/astar.rs:13` (`trait SearchSpace`), `kernel/src/registry.rs:117`
(`register_predicate`), `domains/language/src/phonology.rs` (inventory +
`Envelope` + `permits`), `alarm_field`'s halo decay, The Bane's derived threat
niche. **One claim removed rather than trusted:** I had asserted "Alexander's
pattern 181" and cannot verify a pattern number offline, so the number is gone
and only the title remains. Scope check produced G3 flagged item 5 (this is large
for a first rung; the natural split is vocabulary+graph, then patterns+fields).

---

## Capture manifest

**At close, to `book/src/frontier/idea-registry.md`:**
- `CLIENT-furnishing-ladder` — **correct the rung text** (entry #1): rung 1 is
  authored *patterns*, not room templates, and the ladder's axis is who decides
  the relations.
- `CLIENT-relational-fine-layer` — add the anchor-graph realization and the
  topology-not-metrics rule.
- New rows: **the anchor graph as the coarse layer one scale down**;
  **promotion on touch**; **borrowed spatial calculi (RCC-8 / Allen)**;
  **anchors emit fields**; **the learnable grammar** (a player predicting where
  the hearth will be — a `UNI-1` surface, and the strongest argument yet for
  `CLIENT-discovery-pane`).

**To The Action Clock's spec, before it is written** (spec §12): maintenance
conditions for interval actions, and Allen's interval algebra as its vocabulary.

**Rejected branches:** room-template rung 1 (#1); a third persistence tier for
items (#5); inventing a relation vocabulary (#7); a Cartesian lattice for
movement (#3).

---

## Post-G3 (approved 2026-07-25; residual sub-questions resolved from precedent)

#12 [Q] — **Enforcing "outcomes read topology, never metrics" (G3 item 3).** ·
**Decision: mint the decision record now; the CHECK is owed at the moment
coordinates first exist, not in v1.** · *Why:* v1 has no metric quantity at all —
the coordinate solve is explicitly out of scope — so the rule is **vacuously
enforced by construction** here, and a tag-based check would be machinery
guarding an empty set. The project's consistent instinct is structural
enforcement over discipline (`clippy.toml` disallowed-types for
`HashMap`/wall-clock, the architecture test for layering, type-audit default-deny
for typed quantities), and decision 0073's own lesson is that unenforced rules
decay — so the record is minted now so Campaign 4 *inherits* it, and the check
becomes that campaign's entry obligation the moment a coordinate type appears. ·
*ideonomy 0 passes* (an application of settled precedent to a scoping question,
not a design choice).

#13 [Q] — **Ship or defer promotion-on-touch (G3 item 4).** · **Decision: defer
the mechanism; keep the design recorded in the spec.** · *Why:* nothing in v1's
scope modifies a room, so promotion has no consumer — building it now is
speculative generality. The risk deferral normally carries (the first
modification campaign rediscovering epoch-orphaned deltas under pressure) is
already retired, because §4 records both the problem and the answer. Build when
there is a caller. · *ideonomy 0 passes.*

#14 [G4-pre] — **The split (G3 item 5): one campaign, two plan stages.** ·
**Decision: Stage A = vocabulary + anchor graph; Stage B = patterns + fields.** ·
*Why:* the owner approved the spec as written, which names the split as natural
without carving a second campaign; staging inside one plan is the least
presumptuous reading and keeps the campaign coherent. The ordering is forced by
reversibility (ledger #7): the predicate algebra is the least-reversible piece
and the object catalogue is purely additive, so vocabulary lands first and
everything else composes onto it. Stage A is independently testable (JEPD, the
composition table, A* over the graph) with no patterns and no fields at all.

#15 [G4] — **v1 is a substrate, not an observable outcome — spec criterion
corrected (owner: Option A, 2026-07-25).** · *Found while writing the plan, not
at spec time:* nothing derives an `Interior` from a real room, and creatures have
no anchor position, so every live site passes `warmth: None` and the headline
outcome is demonstrated at unit level rather than observed in the sim. · *Why
Option A:* adding derivation + occupancy means new per-creature positional state
with its own entry rules, which moves behaviour, surrenders byte-identity, and
makes the health battery the gate rather than a check — plausibly doubling the
campaign while removing the property that makes it safe to land. Precedent: The
Snapshot shipped campaign 1 with nothing newly playable. · *Actions:* success
criterion rewritten to say "demonstrated, not yet observed"; spec §9.1 added
naming the follow-up campaign (derivation + occupancy) and its first two tasks;
the branch is pushed so a parallel session can read the spec without waiting for
the merge. · *ideonomy 0 passes* (a scoping consequence discovered by
construction, surfaced to the owner rather than resolved by me).

#16 [G5] — **Upstream review from the parallel campaign The Threshold, acted on
before T4 was dispatched.** That campaign (branch `the-threshold`, off
`the-hearth`) brainstormed the derivation+occupancy half and returned five
findings against this one. Disposition:
- **`compose` was degenerate — ADOPTED, T4 rewritten.** Hub composition put
  every anchor one hop from the first, so graph distance was 1–2, field decay
  had nothing to decay over, and the cold-creature demonstration was a
  single-step route barely exercising `route_within`. T4 now gives each pattern
  an `Attach` (Hub / Beside(kind) / Within(kind)) and an optional `requires`
  (Alexander's "patterns complete other patterns", made checkable), with an
  **anti-hub test** demanding a route of ≥3 hops. This is the campaign's own
  criterion turned on it: if composition rules do not carry the weight, it is a
  catalogue.
- **Inventory size before arming — ADOPTED.** 5 → 9 patterns; growth after The
  Threshold makes furnishing live costs an epoch regardless.
- **Key the seeded draw by NAME, not position — ADOPTED** as a documented
  constraint on `selection` (an id-as-offset bug one scale up).
- **live vs reachable — ADOPTED** into spec §9.1.
- **The threshold modelling error** (one Threshold anchor per room, but rooms
  have ~6 mesh neighbours, so two doorways share one anchor) — **absorbed by
  The Threshold, no edit here**; our `Threshold` becomes the narrow-case landing
  site and the wilderness test is correct as written.
· *Also:* Stage A pushed to `origin/the-hearth`, because their plan's BLOCKING
PRECONDITION was written against a branch that then carried "a spec and a plan
and zero code commits."
· *ideonomy 0 passes* (adopting another campaign's grounded review, not a fresh
design choice).
