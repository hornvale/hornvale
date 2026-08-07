# The Sighting: creatures in the plan, and sight through apertures — Design

**Campaign:** The Sighting · **Date:** 2026-08-06 · **Branch:** `the-sighting`

Rose Window metaplan's next rung after The Blocking and The Panes.
`CLIENT-tile-view` records the plan as shipped and says "sight is next"; this
is that.

## 1. The state of play, verified rather than assumed

**The possession already moves cell-to-cell.** The Blocking shipped it and
`a_compass_step_indoors_never_touches_the_walk_band` pins it: `go n` indoors
moves the CELL and nothing else. Half of the requested demo is already in.

**Creatures already move, and already have a fine position — just not a
coordinate.** `liveness::Occupancy` maps `EntityId -> (RoomAddr, AnchorId)`,
is populated every tick by `step_with_occupancy`, and evaporates with the
bubble exactly as decision 0069 wants.

**What is missing is the join.** `windows/vessel/src/lattice/occupancy.rs`
opens with "No creature stands in a cell until The Sighting," and that is
literally true — `grep Cell windows/vessel/src/interior/*.rs` returns nothing.

## 2. The keystone: Hornvale has two fine spatial layers that have never met

- **Relational.** `Interior { anchors: Vec<Anchor>, adjacency:
  BTreeSet<(AnchorId, AnchorId)> }`, where `Anchor { kind, within }` and
  `within` is RCC's non-tangential proper part. Containment and touching. No
  coordinates. **This is where creatures actually stand.**
- **Metric.** `Lattice { extent, cells: BTreeMap<Cell, CellKind>, doorways,
  dof }`. Cartesian cells. **This is what gets drawn.**

`Structure` is `{ threshold, chambers, links }` and knows no anchors;
`Lattice` never mentions `Interior`. Yet `CellKind::Wall`'s own doc asserts
"a place in its own right — an alcove, a screen or a fireplace is an anchor AT
one of these." The intent was written down. The mapping was never built.

Lifted, the problem is not about anchors or cells: **a relational structure
must be embedded into a metric space so that metric queries become answerable,
while the relational structure stays authoritative.** That is graph drawing,
and its other instances — molecular conformation, VLSI place-and-route,
multidimensional scaling, the London Tube map — all share one discipline:
topology is truth, geometry is a licensed convenience.

Hornvale already knows this and already instruments it. `Lattice.dof` is "how
many independent choices the embedder made — one per stream draw it consumed,"
and the checker compares it against what the anchor graph leaves free, because
inventing "is the one thing an embedder may not do." **Anchors are discovered;
cell geometry is invented, and `dof` counts the invention.**

### 2.1 The decision, and the line it draws

Embed anchors into cells. Then:

- **FOV narrows the `sensed` channel, sim-side.** This is structural
  redaction, and `CLIENT-redaction-panes` is explicit that a client deciding
  what to withhold is a cheat pane. The embedding is load-bearing here, and
  that is correct.
- **FOV does NOT gate `Knowledge` this campaign.** Decision 0069 lets the fine
  layer "regenerate differently forever without corrupting a world" precisely
  because nothing stored points into it. Letting sight-derived knowledge
  accumulate would start pointing at it — agent *belief* would depend on the
  embedder's free draws. Named, deferred, registered.

The distinction is the whole design: the embedding may decide **what a client
is shown**; it may not decide **what an agent comes to believe**.

### 2.2 Faithfulness, and how it is checked

An embedding that scattered adjacent anchors to opposite corners would make the
drawn plan lie about the building. So the embedding is **faithful**: anchors
adjacent in `Interior.adjacency` are placed at cells with a passable path
between them that crosses no third anchor's cell.

This is checkable rather than asserted, and `dof` is the instrument that
already exists for it: a placement consuming more draws than the anchor graph
leaves free is an embedder inventing, which the checker already forbids.

## 3. What ships

- **`anchor_cells`** — a derivation placing each of a chamber's anchors at a
  lattice cell, faithful per §2.2, seeded and deterministic.
- **Creatures on the plan.** `liveness::Occupancy`'s `(RoomAddr, AnchorId)`
  resolves through `anchor_cells` to a `Cell`; `lattice::Occupancy` is
  populated from it, its refusing `place` finally load-bearing.
- **Symmetric shadowcasting** over cells, integer-only.
- **`marks` on `vessel/plan/v1`** — the field The Panes deliberately omitted
  because nothing wrote it. This campaign is its first writer. Shape as
  specified there: `{noun, kind, datum, salience}` plus the cell, deliberately
  the focalizer's `Focalized.nouns` shape, so a creature on the map and a
  creature in the prose are the same examinable thing.
- **The pane draws them.** ~~and draws only lit cells.~~

> **Correction (2026-08-07, at the close).** The struck clause is wrong and was
> never built. There is no unlit concept for a *cell* at the wire: `plan_of`
> emits the whole wall/floor/threshold grid unconditionally, `CellKind` is
> closed at three variants with an explicit warning in its own doc against
> widening it, and only `marks` are gated by sight. The implementation plan's
> own step said "emit only lit **creatures** as marks", so the error is in this
> sentence and nowhere downstream of it; the implementer declined to build
> blank-rendering for a wire value that cannot occur and flagged it instead.
> **What shipped: the map shows the full floor plan; sight hides creatures, not
> geometry.** See [the chronicle](../../../book/src/chronicle/the-sighting.md).

## 4. Symmetric, deliberately

`Interior.adjacency` is a `BTreeSet` of pairs — symmetric by construction.
Ordinary recursive shadowcasting is famously asymmetric: A sees B while B
cannot see A. In a sim where perception feeds belief that is a modelling
commitment, and an asymmetry the relational layer does not have would be an
artifact of the embedding, which §2.1 exists to prevent. So: **symmetric
shadowcasting**, and the property is a test, not a comment.

## 5. Cost

Rose Window measured integer LOS at **3.5 µs for 24²** native and called it
"not a budget item" against the then 4.75 ms turn floor. The Panes has since
measured the real turn: `handle(verb)` **1.071 ms** native, `snapshot()+json`
**1.249 ms**. FOV is noise beside both.

**Pathfinding is the term to watch**, and it is not this campaign's: creatures
move anchor-to-anchor on the action clock, which is a small graph, not a cell
A*. Rose Window's warning — "a great hall with ten creatures re-pathing every
turn is a real fraction of a turn" — applies to cell-level pursuit, which is
out of scope here.

**Owed measurement:** nothing has ever timed a turn through the wasm ABI. Every
browser figure in the repo is native × an extrapolated 3.6–3.8× ratio, which is
what `CLIENT-four-clocks` explicitly forbids ("re-measure, do not
extrapolate"). `windows/vessel/examples/turn_cost.rs` exists and holds a
matched pair; extending it through the ABI is task 1, before any of this.

## 6. Non-goals

- **Knowledge gated by sight** (§2.1). The deferral is the point, not an
  omission.
- **Possession-hopping** — a separate campaign. `release` ends the session
  today; hopping needs release-as-detach, a live `LocaleContext`, an
  `Agent`↔`Npc` reconciliation, and a decision about vacated bodies.
- **Cell-level pursuit or combat.** Creatures move anchor-to-anchor.
- **Vitality.** Rose Window §6.5, and still ordered before any combat work.
- **Walk-band FOV.** `scene/surrounds/v2` already carries a `state` of
  `here`/`sensed`/`remembered`; this campaign does not touch it.

## 7. Decisions

Full reasoning in `.superpowers/sdd/decision-ledger.md`.

1. Anchors embed into cells; the relational layer stays authoritative.
2. FOV narrows `sensed` sim-side; it does not gate `Knowledge`.
3. The embedding is faithful in a checkable sense, with `dof` as the
   instrument.
4. Symmetric shadowcasting.
5. `marks` joins `vessel/plan/v1` additively, with its first writer.

Candidate for `docs/decisions/`: **"the fine layer is relational; the lattice
is a faithful embedding of it."** Number unminted here to avoid colliding with
parallel sessions.

## 8. Risks

1. **A faithful embedding may be infeasible for some anchor graphs** in a 19×19
   extent. Rose Window flagged that "the real constraint solve remains
   unmeasured — the spike's 'solve' column is a placement scan, not a solver."
   Mitigation: measure before building; if some graphs cannot be embedded
   faithfully, that is a finding and the fallback is a *stated* relaxation,
   never a silent one.
2. **`marks` makes the plan payload creature-dependent**, so its size now
   varies with occupancy where §3.4 of The Panes' spec proved it bounded.
   Re-measure; the cost gate at `cli/tests/session_cost.rs` has ceilings to
   ratchet.
3. **The deferral in §2.1 is a rule nothing enforces.** Nothing stops a later
   campaign committing a sight-derived fact. Mitigation: state it in the
   schema's doc where a knowledge campaign will read it — and note that The
   Panes made exactly this mitigation and its final review found the constraint
   was NOT where a replay campaign would look. Put it in the registry row too.
4. **`chartRows` validates no schema** (ledger #4) — a latent defect on main
   today, swept here.

## 9. Testing

- **Faithfulness is a property test**, not an example: for every structure the
  generator produces, every adjacent anchor pair has a passable path between
  their cells crossing no third anchor's cell.
- **Symmetry is a property test**: for all lit pairs, A sees B iff B sees A.
- **Occupancy refuses**: two creatures cannot hold one cell; the existing
  `Refusal` path finally gets a caller.
- **Determinism**: same seed, same verb sequence → byte-identical snapshot
  sequence, `marks` included.
- **A negative control on the join**: perturbing the embedding must move what
  is DRAWN and must not move what is KNOWN. This is §2.1 as a test, and it is
  the campaign's central invariant.

## 10. Definition of done

Chronicle, retrospective, freshness sweep, Confidence Gradient re-score if a
bet moved, registry flips (`CLIENT-tile-view` off "sight is next"), `make gate`
and `make vessel-check` green, artifact drift clean.
