# Campaign 2a: System Genesis

**July 2026 · 18 commits · outcome: complete, merged**

## What was attempted

The generator half of Campaign 2 (The Sky): first the ratified Phase-0
chores — kernel ergonomics, provider construction centralized to a single
file, and every seed-derivation label declared as a constant and published
in a new drift-checked [stream manifest](../reference/stream-manifest.md) —
then the anchor-first star-system generator: a main-sequence star with a
derived habitable zone, an anchor world placed *in that zone by
construction*, moons admitted only past stability inequalities, and a night
sky of notable neighbor stars. Everything pinnable, everything from the
seed, nothing yet visible to worlds: wiring the generator into world
genesis, the calendar, and the almanac is Plan 2b.

## What landed

All of it, guarded by a 128-seed property battery: habitable-zone
containment, Kepler's third law round-tripping to nine decimal places,
every moon re-checked against the Roche floor, the Hill cap, mutual
spacing, and the tide budget — plus pin-isolation tests proving that
pinning one quantity leaves every sibling stream untouched.

## What was learned

- **Seed 10 refused a moon, and the refusal is the system working.** Its
  drawn three-moon configuration has *no* stable third orbit at any search
  budget — the first two moons plus the tide cap close the space entirely.
  The ratified resolution: drawn configurations degrade honestly (the
  system simply cannot hold another moon), while *pinned* counts still fail
  loudly, naming the physical conflict. This is the Krynn question from the
  design conversation answered mechanically: three moons either fit or they
  don't.
- **The final review ran the generator and caught three pin-isolation
  defects empirically** before merge: a negative pinned year slipped
  through unsigned (Kepler squares the year, erasing the sign), and two
  pins quietly shifted their stream's draw sequence, which would have
  broken the "same seed, same pins, except one" experimental contract the
  moment worlds shipped. All three were fixed while fixing them was free;
  after 2b ships worlds, each would have been an epoch bump.
- **Process: trust arrived with verification.** Two implementation agents
  skipped the format gate and one claimed falsely to have run it; reviewers
  caught every instance, and dispatches now demand raw command output
  rather than pass/fail claims.

## Deferred, deliberately

Wiring the generator to worlds (Plan 2b: calendar layer, the generated-sky
provider, pins as CLI flags and ledger facts, the moons-0-vs-2 exit demo);
graded moon pins with genesis-note refusal records and the `scout` verb
(spec amendment of 2026-07-06, from the post-merge design review).

## Artifacts

The [stream manifest](../reference/stream-manifest.md) — the complete,
generated, drift-checked registry of every random stream in the project,
now including astronomy's nine. The generator itself remains invisible to
worlds until 2b; its almanac debut is the next chronicle entry's job.
