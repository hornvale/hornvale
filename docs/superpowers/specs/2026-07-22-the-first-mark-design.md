# The First Mark — presence leaves a traceable mark

## What this is

The first slice of *the game* — the smallest playable loop that proves the
game's central thesis end to end. A player possesses an agent, takes a
consequential action, that action **leaves a mark on the world that survives
being put down**, and the player can **trace the consequence back to their
own hand**. Human scale, one region, text-only, one mode. No tiles, no
combat, no mode selection, no transmigration.

This is deliberately one notch above what already ships. `windows/vessel`
today lets you *possess* an agent and walk a **frozen** world (The Casement,
The Walk): NPCs evolve in a session-owned ledger clone, but the player's own
acts are never committed and nothing is written back (`session.rs:2`,
`:517`–`:520`, flagged there as "rides player-acts-mutate, Campaign IV").
The First Mark is that campaign, cut to its bones: the **first player-
authored fact**, made to **persist**, made **traceable**.

It is NOT the whole game. The game is a multi-campaign arc (see *The arc it
serves*); this campaign proves the arc's riskiest claim at minimum scale so
everything else can layer on without re-litigating the foundation.

## The arc it serves

The game's spine, sketched and converged in the founding brainstorm (five
moves; this spec builds the load-bearing middle three):

1. **The lens** — map and prose are two grains of *one* lens over one query
   surface, joined by attention. Not two games; one world at two zooms.
   (RENDER-4/9; deferred.)
2. **The world** — a living sandbox *read from noise*; no authored scenarios.
   (Already true of the sim.)
3. **The quickening** — *presence induces forward-integration.* Unobserved,
   the world is a statistical prior (drives, weather, aggression are
   **sampled**, not computed forward). Possessed, a local region is promoted
   from *sampled* to *integrated*. **← this campaign**
4. **The mark** — *the world remembers.* The player's meddling freezes into
   facts that condition all future sampling. **← this campaign**
5. **The butterfly** — provenance, aimed at the player's own hand, traces
   what they set in motion. The wake *is* the content. **← this campaign**

## Decisions already made (owner-collaborated in brainstorm + ideonomy)

These are the campaign's binding invariants. Numbers 1–3 are the honesty and
determinism spine; 4–7 are principles this slice honors minimally and later
campaigns deepen. Provenance: the founding brainstorm plus a three-pass
ideonomy convergence (decision-ledger #2).

1. **A player mark is an ordinary fact in the world's own vocabulary.** The
   player may only leave mark-*types the world already makes on its own*
   (`agent-at`, an affect shift, a death — never a player-only predicate).
   The forward-integration bubble is **the world's normal event machinery,
   locally switched on**, not a special player-log. Constitutional: "sim
   first, game as lens" (Principle 1) — anything the player experiences is a
   query any tool could make. A player-authored fact is *just a fact*,
   contradiction-checked against the same registry.

2. **Presence biases latent tensions; it never manufactures them.** The
   bugbear troop was already hungry and already near the village in the
   prior; the player's arrival *raises the probability that an already-loaded
   tension fires*, and hands them the controls while it does. It never spawns
   a tension that was not latent. Mechanically this is the **additive,
   threshold-gated** shape of the additive-latent byte-identity pattern: the
   bias is an additive signal on an existing latent quantity, gated at a
   threshold, so a world with no player action is **unchanged by
   construction**. (See *Determinism*.)

3. **Irreversibility within a life.** The mark, once made, does not undo.
   Permanence is the mechanic. (No in-session undo of committed facts.)

4. **The world remembers by hysteresis.** *Principle; this slice implements
   the degenerate case.* Shallow marks relax back toward the noise prior;
   deep marks permanently move the attractor and never fully heal. Decay-rate
   = f(mark depth, site receptivity). Resolves "remember vs breathe" into one
   mechanism. This campaign ships the limit case — a committed mark simply
   *persists* — and defers the decay curve (UNI-33).

5. **See-your-butterfly is contact-tracing on the provenance graph.** The
   organ already exists (`why`, the historiography window, the Connection
   Graph). This slice aims it at the player's own committed facts and their
   downstream chain. The wake is the content, not a reward for play (UNI-34).

6. **Intent ≠ outcome.** The game promises *consequence*, never *control*.
   A small **opposed** action vocabulary (aggravate / soothe) is the minimum
   that lets a well-meant act still tip a tension the wrong way.

7. **Consequence has a map (soft/inert ground).** *Principle; deferred impl.*
   Consequence concentrates where tension is already latent — the "soft
   clay." Not all sites are equally markable (UNI-35).

## Scope of this campaign — the minimal loop

One region, one possessed agent, text-only, one (implicit) mode: the mortal
who leaves marks. The loop:

1. **Possess** an agent (exists).
2. **Act** with a two-verb opposed vocabulary against a co-located NPC —
   working names `provoke` / `soothe` — each committing **one ordinary fact**
   (an affect/disposition shift on that NPC) into the session-owned ledger.
   This is the **first player-authored fact**.
3. **The bubble integrates.** The player's committed fact feeds an
   **additive, threshold-gated bias** on one already-latent NPC tension
   (e.g. an aggression or flight drive already near its threshold in the
   prior). If the bias tips it past threshold, the tension **fires** and
   commits its own discrete consequence fact on the next `wait` tick —
   forward integration, one hop.
4. **Release.** The mark **persists**: the player-authored facts and the
   consequence they triggered survive being put down and picked back up
   (reload). *(How persistence is stored is the campaign's central
   save-format decision — see Determinism / G3 flag.)*
5. **See the butterfly.** `why <consequence>` traces the fired consequence
   back through the bubble to the player's own act — the provenance chain
   `player act → bias → threshold crossing → consequence`, rendered as
   diegetic recounting by the historiography window.

Success = a player, in one sitting, can provoke an NPC, watch a latent
tension fire a hop later *because* of it, put the world down, pick it back
up with the mark intact, and trace the whole causal chain back to their own
deed.

## Components

- **The act verbs (`windows/vessel`).** Extend `Session::handle` with
  `provoke`/`soothe` (working names). Each resolves a co-located NPC and
  commits a single disposition-shift fact into the session-owned ledger
  (`session.rs:60`, `:89`). This is the seam `session.rs:517`–`:520` reserves.

- **The bias + firing seam (`domains` drive layer + `windows/vessel`).** The
  NPC drive tick (`DriveMovements`, `liveness.rs`) already arbitrates
  homeostatic drives against fields and commits `agent-at` divergences. Add
  an **additive term** to one drive's pressure sourced from the player's
  disposition-shift facts, **gated at the existing decision threshold**. No
  player fact → zero added term → identical arbitration (byte-identity by
  construction — the Alarm/Wilding lineage of the additive-latent pattern).

- **Persistence (kernel save-format + cli).** Today the session ledger is a
  clone, never written back (`session.rs:59`); `session_ledger_json`
  (`:191`) exists only for determinism tests. The First Mark must let the
  mark *outlive the session*. **This is the campaign's load-bearing
  save-format decision** (see Determinism). Minimal shape recommended: on
  release, the player-authored and player-triggered facts are folded back
  into a saved world (via `possess --world <in> --out <out>`), so the mark is
  carried by the ordinary `World { seed, registry, ledger }` save — the same
  serialization boundary, quantize-at-emit unchanged.

- **The butterfly-reader (`windows/historiography` + `why`).** No new engine:
  point the existing provenance recounting at the player-authored facts and
  walk the forward chain to the fired consequence. The `why` verb already
  recounts an NPC's dated `agent-at` history; extend its subject set to the
  player's own committed facts and the consequence they seeded.

## Determinism (the hard part — leads the G3 flag)

Forward integration is a **different computational regime** than noise-
sampling, and a **chaotic** one (drives, arbitration). CLAUDE.md's Lorenz
guard-rail is directly on point: *never seed a chaotic forward-integrator
from quantized ledger floats; a chaotic checkpoint needs its own full-
precision format.* This campaign stays inside the guard-rail by **keeping
the integration shallow and discrete**:

- The bias is an **additive term on an existing drive pressure**, gated at an
  existing **threshold**. The forward step commits a **discrete** outcome
  fact (a threshold crossing), not a continuous chaotic trajectory. Discrete
  facts quantize at emit like every other fact; there is no chaotic float
  checkpoint to preserve.
- **Re-derivation on reload replays from the fact trace, not from quantized
  state.** The player's action facts are the lossless record; the world is
  re-derived from seed + those committed facts. This respects the guard-rail
  (resumption re-derives; we never forward-integrate *from* a quantized
  checkpoint).

**Byte-identity contract (unchanged worlds):** a possession with **no** act
verb used must leave every committed artifact byte-identical to today's —
the additive-latent construction guarantees it (zero added term). Test on
seed 42, as with prior additive-latent seams.

**Save-format flag (G3, leads):** persisting the mark means a saved world may
now carry **player-authored facts** in its ledger. This touches the save-
format contract:
- Does play write into the **canonical** `World.ledger`, or a **separate
  play-save / branch**? (Recommendation: fold into `World.ledger` via an
  explicit `--out`, so a "played" world is still just `World`; the *input*
  world is never mutated in place.)
- Do player-authored facts need a **provenance marker** (a source tag) so the
  butterfly-reader — and contradiction-checking — can distinguish "the world
  did this" from "the player did this"? (Likely yes; this is additive to the
  Fact envelope and must be epoch-clean.)
- No stream-consumption-order change: player facts are appended post-genesis,
  consuming no genesis draws.

## Error handling

- Act verbs with no valid co-located target fail diegetically ("there is no
  one here to provoke"), commit nothing.
- `--out` persistence failures fail loudly (the world is not silently
  dropped); the input world is never written in place.
- A `why` query on a subject with no player-authored provenance recounts the
  ordinary (non-player) history, never fabricates a chain.

## Testing

- **Byte-identity:** seed 42, possession with no act verb → every artifact
  identical to `main` (drift check + `session_ledger_json`).
- **Determinism of the mark:** same seed + same action trace → byte-identical
  session ledger and persisted world (`committed_*_count` + serialized
  ledger).
- **The firing is real:** a property test that the additive bias, above
  threshold, tips a *latent* (near-threshold) tension and does nothing to a
  cold one — proving bias-not-manufacture.
- **Persistence round-trips:** `possess --world w --out w2`; reload `w2`; the
  mark's facts are present and `why` reconstructs the chain.
- **Intent≠outcome exists:** at least one seed where `soothe` still tips a
  tension the "wrong" way (the opposed vocabulary earns its keep).

## Out of scope (indexed, not lost)

Routed to the idea registry; each is a later rung, and none is a prerequisite
for the others once this foundation lands:

- Tiles / 16-bit render as a *coarse lens* (RENDER-4).
- Combat as a high-salience temporal mode (not a separate subsystem).
- The mode matrix — hardcore / normal / creative as binding configurations of
  the possession primitive (UNI-36, cross-links UNI-18).
- Transmigration / the soul that moves between lives (UNI-18).
- Scale-of-possession — the same loop at settlement / culture / civilization
  scale (UNI-37; vessel's `enter`/`exit` refusal is its seam).
- Hysteresis decay curve; recognition / immune-memory reactivity (UNI-33).
- Soft/inert ground as a spatial receptivity field (UNI-35).
- Superspreader-of-consequence nodes; world-cycle consequence amplifiers.

## Definition of Done (per decision 0020 / 0030)

- Code + tests green through `make gate`; `make gate-full` before merge (a
  save-format touch is a boundary change — full gate, per memory).
- Type-audit clean (any new pub-boundary primitive carries a verdict tag).
- Chronicle entry (`book/src/chronicle/the-first-mark.md`).
- Freshness sweep of chapters the mark touches (the game/possession chapter,
  the determinism chapter if the save-format contract moves).
- Idea-registry rows UNI-32..37 created/flipped; if this resolves or moves an
  open-questions bet, re-score that chapter (0030).
- One-page retrospective (`docs/retrospectives/`).
