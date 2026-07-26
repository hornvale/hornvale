# The Retainer — design

**Working name** (blessed at G6). A performance campaign: stop re-deriving a
world's terrain/climate providers repeatedly within one unit of work. Motivated
by the `possess` profiling dig — the interactive game seam takes **5.4 s** to
run an 11-line walk, almost entirely re-deriving the tectonic world.

## Goal

Providers (`terrain_of`, `climate_of`) are pure functions of `(seed, pins)` and
are **not memoized** — each call re-runs the full tectonic `generate` (~0.6 s).
Worse, `climate_of(world)` re-derives terrain *internally*:

```rust
pub fn climate_of(world) -> ... { let terrain = terrain_of(world)?; climate_from(world, &terrain) }
```

So every site needing both — `LocaleContext::build` (locale) and lab
`WorldView::build` (census) — calls `terrain_of` **and** `climate_of` and builds
terrain **twice**. Layered over the possession's other derivations, `possess`
pays for terrain ~3–4×. Reuse the derivation within its scope.

## The design — scope reuse to the unit of work, never a global cache

The census builds each world **once** (one-shot per world), so a global or
LRU cross-world provider cache would help the census *nothing* (its redundancy
is within one world, not across) while risking OOM on thousands of worlds — and
it would sit as a second source of truth against the constitution's "a world is
a seed plus a ledger; everything else re-derived." So: **no cross-world cache.**
`possess` is *episodic* (many observations of one frozen world) → reuse within
the session. Three scoped fixes:

### Fix A — universal within-world dedup (the flagship; helps possess AND census)

At every site that derives both terrain and climate, derive terrain **once** and
build climate from it:

```rust
let terrain = terrain_of(world)?;
let climate = climate_from(world, &terrain)?;   // not climate_of, which re-derives terrain
```

**Byte-identical by construction:** `climate_of(world)` *is*
`climate_from(world, &terrain_of(world))` — same climate from the same terrain.
Sites: `LocaleContext::build`, lab `WorldView::build`, and any other
`terrain_of`+`climate_of` pair (audit — grep the workspace). Low effort, no
cache, and the *only* fix that helps the census (removes its double-terrain).

### Fix B — `possess` session-scoped provider reuse

After Fix A the possession still derives terrain more than once (the profile
shows ~2.7 s in `terrain_of`; Fix A removes the `LocaleContext` double, and
execution pins the residual — candidate: the world-load path and any per-`vantage`
derivation). Derive the frozen world's providers **once** for the session and
reuse across observations. Scope = the session (bounded; census-irrelevant).

### Fix C — memoize `sky_report` for the frozen day

`observable` calls `sky_report(world, at)` on every observation, but the
possession day is **frozen** (`self.day`), so it recomputes an identical sky
each step. Cache it in the session for the frozen day.

## Byte-safety

Every fix returns byte-identical providers (pure functions of `(seed, pins)`;
`climate_from` from the same terrain gives the same climate; a memoized
`sky_report` for the same day is the same string). Not an epoch — providers are
derived, never serialized; no draw/label/format change. Gates: `lens_purity`;
the committed **possession transcripts** (`possession-seed-42.md`,
`possession-over-time-seed-42.md`) must regenerate byte-for-byte (the real
end-to-end proof); full `make gate`; census drift (Nathan's out-of-band regen).

## Scope

**In:** Fix A (the terrain+climate dedup, all sites); Fix B (possess session
reuse); Fix C (sky_report memo). Each with the artifact-drift proof.

**Out (with reasons):**
- **A global / LRU cross-world provider cache** — rejected: the census (one-shot
  per world) gets zero benefit and risks OOM; it violates the seed+ledger
  single-source-of-truth constitution. (Ledger #1.)
- **`build_world` returning a derived-provider bundle** — larger change to the
  World build contract; a followup if Fix A/B prove insufficient.
- **Memoizing `terrain_of`/`climate_of` globally** — same census-OOM problem.

## Non-goals

No new dependency, no physics/format/draw change, no cross-platform claim. The
providers stay pure derivations; this only changes *how often* they run.

## Decisions (from the ledger)

- **Scoped reuse (within-world dedup + session reuse), not a cross-world cache**
  — the census is one-shot per world (ledger #1, matrix + cyclicity).
- **Fix A is byte-identical by construction** (`climate_of ≡ climate_from ∘
  terrain_of`) — verified.
- **Not an epoch**; possession-transcript + census drift are the gates.

## Definition of Done

Fix A applied at all audited sites; Fix B/C landed; possession transcripts and
all artifacts regenerate byte-for-byte; `possess` re-measured; full `make gate`
+ census drift; chronicle + retrospective; followups (build-bundle;
census-runner further threading if any) promoted.
