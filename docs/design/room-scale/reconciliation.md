# Reconciliation — the room-scale corpus meets The Walk & the frontier

**Status:** Record + cross-link. Written 2026-07-12, after The Walk metaplan and
the Crust epoch landed on `main` in parallel with this corpus.
**Why this exists.** The room-scale design (`docs/design/room-scale/`) and The Walk
metaplan (`docs/superpowers/specs/2026-07-11-the-walk-metaplan-design.md`) are two
*independent* explorations that converged on the same architecture. This file
records the convergence, triages every room-scale claim against the authoritative
work, and cross-links the two so the next session doesn't re-derive either.

---

## The convergence, and why it was forced

Two explorations dug toward each other from opposite ends — this corpus through
**space** (mesh, rooms, biomes), The Walk through **time and mind** (loop-closure,
event ledger, belief) — and met at the coarse↔fine seam, agreeing to an uncanny
degree. Independent derivation is *validation*, not waste (Newton–Leibniz): the
seam design is very likely right because two paths reached it. And the convergence
was **near-inevitable**, because the constitution forced it: *coarse-constrains-fine*
+ *determinism* + *no wall-clock* + *serde-only* leave essentially one shape —
an append-only `Fact` log with everything else re-derived, a pure-field coarse prior,
and order-independent lazy refinement. The two corpora share the constitution as a
**merge base**, which is exactly why reconciliation is cheap: most of it auto-merges,
with a single real conflict.

The near-identity worth naming: The Walk §4.1's refinement protocol (*resolve coarse
fields + committed facts → check the ledger ratchet → seed from `hash(location,
entity, epoch)` → elaborate → constrain, never contradict → project → write back only
divergences*) **is** P2 + P1 — coarse-field inheritance, integer-address seeding, the
ratchet, constrain-never-contradict, write-back-consequences-only — down to the
edge-agreement rule ("depend only on continuous coarse fields + own seed, never on
neighbours' committed detail").

## The triage — every room-scale claim, and its verdict

Four states, four moves (the state-machine the reconciliation reduces to):

```
  CLAIM (room-scale corpus)            STATE         MOVE          AUTHORITY IT MEETS
  ----------------------------------   -----------   -----------   --------------------------------
  P2 spatial subdivision (mesh,        ADDITIVE      ADOPT ->      The Walk §4.1 seam is abstract
    adjacency, RoomId)                               into Walk     about WHAT a locale is; P2 is its
                                                                   concrete spatial instantiation
  Biome bestiary + strangeness         ADDITIVE      ADOPT         no counterpart anywhere; pure gain
    engines (cycles 01-02)
  Field-refinement / palimpsest        ADDITIVE      ADOPT         extends Crust's canonical-grid
    (bound to canonical grid)                                      decision + The Walk §4.1 step 5
  The chunk-partitioned LSM ledger     CONFLICTING   YIELD ->      The Walk §3.6: in-memory Vec<Fact>,
                                                     defer         MEM-1-bounded, no LSM; ours = the
                                                                   deferred out-of-RAM form only
  Cycle-03 verbs / affordances         DUPLICATE     FOLD ->       frontier MAP-27 (verb-as-reaction)
  Cycle-03 culture / factions / feuds  DUPLICATE     FOLD ->       frontier SOC-9 (social graph)
  Cycle-03 encounters / trophic /      DUPLICATE     FOLD ->       frontier PSY-6 (GOAP motivation),
    inhabitants as agents                                          UNI-20 (derived-view architecture)
  Cycle-03 player as promoted agent    DUPLICATE     FOLD ->       The Walk §3.1 (PC = promoted NPC)
  Store-irreversible/derive-reversible SHARED        (agree)       The Walk §3.4 = P2 two-tier state
  The whole concurrency analysis       OUT-OF-SCOPE  PARK          past The Walk Milestone 2; reserved
    (multi-writer, untrusted, sagas)                               for Living-Globe; not near-term
```

**The one real conflict** is the ledger, resolved by [decision
`the-room-tier-ledger-is-chunk-partitioned`](../../decisions/the-room-tier-ledger-is-chunk-partitioned.md)
(re-scoped): near-term is The Walk's in-memory model; partitioning is the deferred
out-of-RAM form §3.6 sets aside. Everything else is additive, duplicate-to-fold, or
parked — no other contradiction.

**The correction this triage made** to an earlier, too-submissive framing: the room-
scale corpus is not uniformly the junior partner. It *yields* the ledger, but it
*contributes* the spatial instantiation of the seam and the biome bestiary — claims
with no counterpart in The Walk or the frontier. Reconciliation is mutual and
per-claim, not one-directional.

## Cross-links (this side; reciprocal side is a follow-up)

- P2 (`p2-subdivision-design.md`) is the **spatial instantiation** of The Walk §4
  refinement seam; §7 now carries Crust's canonical-grid constraint.
- The living layer (`cycle-03-the-living-layer.md`) elaborates the same space as
  frontier UNI-20 / SOC-9 / PSY-6 / MAP-27 and The Walk §3.1 — treat those rows as
  the home; cycle 03 is spatial/ecological colour on them, not a rival.
- **Follow-up — done 2026-07-12.** The reciprocal side is now in place. The corpus
  is promoted into the registry as three rows — **MAP-28** (the room mesh / P2),
  **MAP-29** (room-scale variety / the biome bestiary), **MAP-30** (palimpsest &
  field-refinement) — and the four folded cycle-03 rows (**MAP-27, SOC-9, PSY-6,
  UNI-20**) now carry back-links to `cycle-03-the-living-layer.md`. The Walk spec
  §4.1 names P2 as its spatial instantiation. (`docs/CLAUDE.md` drift-check green.)

## The standing lesson (the reason it collided at all)

This corpus was generated **without the registry-first discipline** `docs/CLAUDE.md`
mandates — *grep the frontier idea-registry before proposing, cross-link into it at
merge*. That is the idea-layer analogue of the project's code habit (`make preflight`,
absorb-main-at-boundaries), and skipping it is why two corpora drifted into the same
space uncoordinated. The durable fix is not this one reconciliation; it is running
**registry-first on future design cycles** so convergence is caught at the source, as
a small absorption next to its cause, instead of a five-artifact reconciliation after
the fact.
