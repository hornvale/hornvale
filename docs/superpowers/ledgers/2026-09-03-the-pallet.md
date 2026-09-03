# The Pallet — decision ledger

Campaign B of the `MAP-one-kind-model` arc: **a body chooses where to sleep**,
with The Plumb's Group A (the local-day family) folded in per Nathan's triage.

---

#1 [Q] — **Campaign name: The Pallet.** A pallet is the humblest bed — straw on
a floor — which is exactly the span this campaign models: a body that would
prefer a bed, will take bracken, and can pass out in the road. `the-pallet` has
no prior reference in `docs/retrospectives/` or `book/src/chronicle/`;
`the-settle` was the first choice and is unusable (62 substring collisions with
"settlement"). No ideonomy pass — a name is not a design decision.

#2 [Q] — **What may be recorded is settled by decision 0069, and the answer is
the KIND, not the place.** 0069 makes an entity's persisted position its
**room**; anything finer "exists only inside the presence bubble and is never
serialized". `windows/vessel/src/interior/anchor.rs` says the same in its own
words: "an anchor has no coordinate, and its identity within a room is
positional, not persisted."

So "the body slept at anchor #3" is unrecordable **by constitutional design**,
and no campaign should try. But The Wicket already made the durable half
first-class: `Anchor { kind: KindId, within }`. **A `KindId` is a registered
concept and a stable string.** So `slept-on = bed` is fully 0069-legal — it
records *what* a body slept on, never *where*.

Mechanism, checked against `kernel/src/ledger.rs` rather than assumed:
`Value::Text(String)` exists, and `Fact.place: Option<EntityId>` is the
room-granular location 0069 permits. A **new** predicate `SLEPT_ON` carrying
`Value::Text(kind)` with `place: Some(room)` is additive — it does not touch
`SLEPT`'s existing object (the span), so no save-format contract moves.

Precedent: predicates are single-valued here, so a second attribute takes a
second predicate rather than a compound object.

#3 [G1] — **OVERTURNED BY THE IDEONOMY PASS: the choice is THREE RUNGS, not one
preference function, and the body must be allowed to choose badly.**

Tuple drawn: operators `dimension-identification` + `combination`, organon
`lattice`, dimension-prompts `discovery-vs-invention`, `side-effect`,
`autonomy`. One pass, **one overturn** — the pass changed the recommendation
materially rather than enriching it, which is the rarer outcome.

**What I was going to propose:** a single preference function over available
kinds, plus a quality grade feeding the recovery rate.

**What the pass produced.** Combining the sleep decision with The Plumb's own
rung ladder shows the quantity is not one thing but a lattice, and Nathan's
own words already name all three rungs:

| rung | the quantity | Nathan's phrasing |
|---|---|---|
| `per-species` | the GRADE of a kind — how well this body recovers on it | a xorn does not want a bed |
| `per-people` | the PREFERENCE ordering among available kinds | "whatever their people tends to use" |
| `per-individual` | the idiosyncrasy | "some people just really like sleeping bags" |

This makes The Pallet **the first campaign to use the ladder as a design
instrument rather than an audit** — and it sets the scope honestly: the
per-people rung needs kind-to-kind edges (Campaign C) and the per-individual
rung needs `Lineage`-derived values (Campaign D), **neither of which exists.**
So The Pallet ships the `per-species` rung and leaves the other two as
*declared, tagged seams* — `plumb: per-people(...)` / `per-individual(...)`
findings in the roster, not TODO comments — which is the mechanism decision
0586 exists to provide.

**The `autonomy` prompt produced the constraint I would otherwise have violated.**
Nathan: *"a creature sleeping in an unsafe or unrestful place would be a useful
indicator that something needs tuning."* A design that clamps the body to the
best available site **destroys that diagnostic**. The choice must therefore be
sane-by-default and **capable of being wrong**, and a bad choice must be
visible in the ledger rather than prevented in the chooser. This inverts the
obvious implementation (argmax over grade) into something weaker on purpose.

**The `side-effect` prompt found the argument for recording a kind rather than
a grade.** `windows/historiography` is domain-agnostic by construction — it
replays any entity's facts against the registry's predicate docs. So committing
`slept-on: bed` yields narrative **for free**, on a surface nobody has to
write; committing only a number would not. `windows/lab/src/health.rs` already
reads `SLEPT`, so the distress read gains the same for free.

**The `discovery-vs-invention` prompt sharpened one distinction worth keeping:**
a body *knows* innately that a bed out-rests a floor (species-level instinct),
but *cannot know* whether a particular site is safe without being there. So
**preference is innate; safety is situational** — which argues against folding
threat into the grade, and for leaving it as a separate term a later campaign
supplies.

**Discarded, with reasons:** (a) recording a bare quality *number* — loses the
free historiography and the tuning diagnostic, which both need the kind;
(b) making sleep *gated* on finding a site — Nathan ruled this out explicitly
("we don't want things to just wander out of the room to find the nearest
bed"); (c) an anchor-identity record — unconstitutional under 0069.

Capture: Campaign C and D dependencies are recorded here rather than as new
registry rows, since `MAP-one-kind-model` already carries both additions.
