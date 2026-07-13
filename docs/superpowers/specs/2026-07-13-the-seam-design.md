# The Seam — Design

**Status:** Ratified by brainstorm 2026-07-13
**Date:** 2026-07-13
**Roadmap slot:** The Walk metaplan, Chunk 0 ("The Game Seam",
`2026-07-11-the-walk-metaplan-design.md` §9), widened to the walkable
variant. The first game-layer campaign: after this merges, Hornvale is a
thing someone walks in.

## Problem

The Walk metaplan carved Chunk 0 as the foundational, CI-cheap deliverable:
five interfaces — Agent, Projection, Vantage-query, Focalizer, Local-space —
each with a trivial tier-0, wired end-to-end so `possess` works on day one
and every later chunk refines one stub without touching the others. The
metaplan was written before the Room Mesh and the Locale Window shipped;
its "Local-space tier 0: a single room with no exits" is now *below* the
shipped floor — `windows/locale` already derives a walkable room graph with
typed exits from any world. Stubbing what has shipped would be a regression
dressed as discipline.

This campaign therefore lands the **walkable Seam**: Chunk 0's five
interfaces, with Local-space bound to the real locale window and a minimal
read-only verb loop riding along, so the exit criterion is *walking a
frozen world that re-describes as you move* — not printing one line.

## Scope decisions (ratified in brainstorm)

1. **Walkable Seam** — Chunk 0 + the minimal verb loop; not strict Chunk 0,
   not Seam+Vessel, not the full Milestone 1.
2. **Front door: both, shared core** — a new `hornvale possess` subcommand
   *and* a repl `possess` command, entering the same I/O-generic loop.
3. **Prose bar: templated is fine** — one honest templated line per room
   from real fields/facts. Repetition across rooms is acceptable; The
   Uncommon Ground (MAP-29, planned) buys variety and absorbs into this
   exact surface later.
4. **Walk extent: one scale** — ratified as "one locale"; reconciled with
   the shipped mesh at planning time. The room mesh has no locale
   boundary — lateral walking is globally lazy and uniform cost — so the
   bound the choice was protecting (no fresh derivation surface) is
   realized by *scale*, not area: the walk stays at the canonical walk
   depth (`globe_level + 6`, the locale CLI's default), lateral compass
   exits traverse freely, and the **vertical** `Enter`/`Exit` exits
   (`ExitKind::Vertical`, the actual seams) render diegetically but
   refuse. Scale travel is a later chunk.
5. **Layering: window-first, graduate later** — all five interfaces and
   their tier-0 implementations are born in one new window crate. Nothing
   touches the kernel or any domain. When The Vessel (Milestone I)
   validates the projection's shape against a real second consumer, the
   subset contract graduates toward the kernel *with evidence* — not
   before. The five signatures are named by this spec, **not frozen** by
   it (metaplan §9).

Six adoptions from the ideonomy passes (2026-07-13), folded in below:
day-parameterized possession (§ Verb loop), the scripted-replay transcript
and walker battery (§ Determinism and testing), breadcrumbs in the
transcript (§ Verb loop), the perception-vector slot in the Agent mint
(§ The five interfaces), the examine contract (§ Verb loop), and
session-accumulating knowledge (§ The five interfaces, Projection).
Explicitly *skipped*: free choice of incarnation site (`--at LAT,LON`) —
the mint is flagship-only this campaign.

## Architecture

One new crate: `windows/vessel` (crate `hornvale-vessel`). Dependencies,
respecting the constitutional layering (windows may depend on domains and
on other windows):

- `hornvale-kernel` — `RoomAddr`, `WorldTime`, `World`, ledger reads,
  `Stream` (for the walker battery's deterministic walks).
- `windows/locale` — Local-space, consumed exactly as shipped
  (`LocaleContext::build`, `describe(&RoomAddr, WorldTime)`, `Exit`,
  `Compass`, `ExitKind`). No changes to it.
- `domains/settlement` — the flagship settlement for the tier-0 mint
  (`village_info`).
- `domains/species` — `PerceptionVector` for the Agent's perception slot.

The CLI (`cli/`) wires `hornvale possess`; `cli/src/repl.rs` gains a
`possess` command handing off to the same core. Worlds are built through
`hornvale-worldgen` as always (the CLI already holds one).

The core entry point mirrors `repl::run`:

```rust
/// Run the possession loop over a frozen world until release or EOF.
pub fn run(
    world: &World,
    opts: PossessOpts,
    input: impl BufRead,
    output: impl Write,
) -> std::io::Result<()>
```

`PossessOpts { day: WorldTime, script: Option<String> }` (the script is
the pre-read command text, keeping the core I/O-generic and file-free) —
defaults: day 0, interactive stdin.

## The five interfaces

Signatures are this spec's design deliverable (sketched at design altitude;
the plan freezes field-level detail):

- **Agent** — an addressable individual, **derived lazily, game-side, never
  committed to the ledger** (metaplan §3.4 reversibility rule: possessing a
  world must not change it).

  ```rust
  pub struct Agent {
      pub id: AgentId,            // deterministic from seed + mint inputs
      pub species: SpeciesId,
      pub perception: PerceptionVector,
      pub position: RoomAddr,
  }
  pub fn mint_flagship(world: &World) -> Result<Agent, VesselError>
  ```

  Tier-0 mint: the flagship settlement's argmax cell → `RoomAddr` at the
  canonical globe level; the settlement's majority species supplies the
  perception vector. The perception slot is signature discipline — one
  value ships, but the goblin-in-a-caste-ladder and the solitary dragon
  are already the same type (the metaplan's generality test). Mint failure
  (a world with no settlements) fails loudly with the physical reason,
  `GenesisError`-style.

- **Projection** — `knowledge = project(ground_truth, vantage, perception)`.

  ```rust
  pub trait Projection {
      fn project(&self, world: &World, agent: &Agent, at: WorldTime) -> Knowledge;
  }
  pub struct IdentityProjection;   // tier 0
  ```

  Tier-0: the identity projection — the coarse facts and field values at
  the agent's location. **Knowledge accumulates over the session**: the
  possession loop folds each visited room's projection into the session's
  `Knowledge`, so `knows` is the observation *history* of the walked path,
  not a snapshot of the current room. This is the metaplan §3.2 definition
  taken at its word ("what an agent *has observed*"), session-local and
  deterministic given the path. The **subset contract is the interface**:
  the accumulated `Knowledge` is provably a subset of ground truth,
  asserted path-dependently by the walker battery. False belief,
  inference, fog, forgetting: The Vessel's job, not this one.

- **Vantage-query** — "what is observable from here, now, to this agent."

  ```rust
  pub fn observable(world: &World, agent: &Agent, at: WorldTime) -> Result<Vantage, VesselError>
  ```

  Bundles the locale room (via `LocaleContext::describe`), its exits, the
  facts overlapping the position, and the field values at (position, day).
  This is the refinement protocol's steps 1 and 6 only — no elaboration,
  no salience ranking beyond what phenomena already provide.

- **Focalizer** — render a vantage as prose.

  ```rust
  pub trait Focalizer {
      fn render(&self, vantage: &Vantage) -> String;
  }
  pub struct TemplateFocalizer;    // tier 0
  ```

  Tier-0: one honest templated line from real data — biome, settlement
  proximity, and the sky/season for the possession day. Total: every
  reachable vantage renders non-empty prose.

- **Local-space** — `windows/locale` as shipped. Not modified, not
  wrapped; the vessel consumes `Locale`/`Exit` directly.

## The verb loop

All verbs are read-only against the frozen world; no verb mutates
anything, so the verb-chemistry engine (frontier MAP-27) is correctly
*absent*, not stubbed:

- `look` — re-render the current vantage through the focalizer.
- `go <direction>` — traverse a lateral compass exit and re-focalize.
  Vertical `Enter`/`Exit` exits render diegetically but refuse to
  traverse ("the grain of the world resists" — wording is the
  implementer's).
- `examine <thing>` — **the examine contract**: the examinable nouns are
  exactly the vantage's named constituents, and the focalized line is
  their catalog — *if `look` mentions it, you can examine it.* No hidden
  nouns, no unexaminable scenery ("LOOK SUN → I don't see any such thing
  here" is the classic text-adventure realism break this contract exists
  to forbid). Tier-0 `examine` renders the datum behind the noun as one
  templated line.
- `whoami` / `knows` — dump the agent and its projected knowledge: the
  seam's epistemics made visible at the prompt.
- `back` — retrace the last step (sugar over the breadcrumb trail).
- `release` / `quit` — end possession. Entered from the repl, `release`
  returns to the scholar loop; from the subcommand, it exits.

Invocation:

```
hornvale possess (--world w.json | --seed N) [--day D] [--script walk.txt]
```

`--seed N` builds the world in memory through the composition root — the
metaplan's exit criterion (`hornvale possess --seed 42`) works verbatim.

- `--day D` sets the possession's `WorldTime` — the freeze point, not a
  clock. Same room, different day: the sky and season in the focalized
  line change. This threads world-time through the whole seam at flag
  cost and pre-stages Milestone IV's plumbing without simulating anything.
- `--script` feeds the verb loop from a file and emits a transcript.

**Breadcrumbs:** the transcript records each step's room id alongside the
prose — the walked path, diffable at the room level rather than only the
prose level. (`--at-room` resume permalinks: deferred, the ids in the
transcript already carry the information.)

## Determinism and testing

Everything byte-deterministic; three tiers:

1. **The gallery transcript** — a committed scripted walk over seed 42,
   published as a `book/src/gallery/` artifact ("a first possession
   transcript"), regenerated and drift-checked by CI alongside the
   almanacs. This is the metaplan's Chunk 0 exit criterion, verbatim.
2. **The walker battery** — deterministic pseudo-random walks seeded from
   a kernel `Stream` (stream labels declared in the crate's `streams`
   module and published into the manifest, per the save-format contract),
   asserting four invariants along every walk:
   - **projection subset** — the session's accumulated `Knowledge` (the
     union over the walked path) is derivable from ground truth at every
     step;
   - **exit reciprocity** — `go d` then the reverse direction round-trips
     (the room mesh's seam-gluing, asserted from above);
   - **focalizer totality** — every room renders non-empty prose without
     panicking;
   - **examine totality** — every noun the focalizer emits resolves
     through `examine` (the examine contract, enforced mechanically).
   Bounded to one locale this should be fast-gate cheap; if it measures
   otherwise, it takes a `heavy:` ignore-reason token and moves to
   gate-full.
3. **Buffer-driven loop tests** — the `repl::run` pattern: feed command
   sequences, assert output; including the diegetic Exit refusal, `back`,
   `--day` variation, and repl-handoff `release` returning to the scholar
   loop.

No new dependencies. `BTreeMap`/`BTreeSet`/`Vec` only. Quantization at
emit boundaries only, as everywhere.

## Out of scope (the fence)

- **No liveness, no events, no time-stepping** — `--day` parameterizes the
  freeze; it does not thaw it. Milestone IV/V.
- **No scale travel** — the walk stays at the canonical walk depth;
  vertical `Enter`/`Exit` exits refuse diegetically.
- **No prose variety** — The Uncommon Ground's job; it runs independently
  (feeds, not blockers) and absorbs into the Focalizer surface.
- **No anti-phenomenon reads, no sense-verbs** (`listen`/`smell`/`touch`)
  — noted as empty branches for Milestone II / Focalized Sight.
- **No projection beyond identity** — fog, inference, false belief are
  The Vessel's.
- **No kernel or domain edits** — the graduation of the subset contract
  into the kernel is Vessel-gated.

## Exit criteria

- `hornvale possess --world <seed-42 world>` prints a focalized
  description of where the flagship agent stands; `look`/`go`/`examine`
  walk the locale and re-describe; `whoami`/`knows` dump projected
  knowledge, proven a subset of ground truth.
- The repl's `possess`/`release` round-trip works.
- The gallery possession transcript is committed and drift-checked.
- The walker battery and loop tests pass in the commit gate
  (`make gate`).
- Book Definition of Done: chronicle entry, freshness sweep (the Walk
  metaplan chapter and open-questions re-score if a bet moved), campaign
  retrospective.

## Interplay with in-flight campaigns

Independent of The Uncommon Ground (MAP-29, planned unexecuted), the
Gathering, census-as-data, and the parked Datum/Temperature. The only
shared surface is `windows/locale`, which this campaign consumes
read-only. Standard absorption discipline applies: absorb main at every
plan-stage boundary via `make preflight`.
