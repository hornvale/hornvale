# 0578. Kinship as a genesis fact

**Status:** Ratified (2026-09-02) · **Decider:** Nathan (autopilot) · **Campaign:** The Avowal

## Context

`domains/history::descent` has always computed the relation between a
promoted founder and the founder they descend from — `forebear_of`,
`Kinship::{Sibling, Ancestor(u32)}`, `kinship()` — but committed **zero**
facts for it. The concept registry held `concept:parent`, `concept:child`
and `concept:sibling` (lexical concepts owned by `domains/language`) and no
kin *predicate* at all, so `bundle:consanguineal-kin` scored blocked across
every situation in both frozen trope corpora that named it — 12 of Polti's
36, 50 of `tvtropes-2012`'s 409 — on machinery the composition root already
ran at genesis. Task 1's 25-seed panel measured the promoted-forebear yield
(the share of promoted founders whose forebear was *also* promoted) at a
44.9% median against spec §5's 10% kill criterion, so the machinery is not
merely present but productive.

## The decision

Two predicates, both owned by `domains/person`: `parent-of` (a founder's
promoted forebear, when `kinship()` classifies the edge `Ancestor(_)`) and
`kin-of` (the same edge classified `Sibling`) — mutually exclusive per
founder, since both derive from the one `occ-founded-from` edge a promoted
occupation carries at most once. Both are **functional**: an occupation
carries at most one recorded forebear (`mother_of`, `windows/worldgen::
descent::forebear_of`'s own signature — `let mother = mother_of(world,
occupation)?;`, a single `Option<EntityId>`), so a founder has at most one
recorded forebear and therefore at most one `parent-of` **or** `kin-of` fact,
never both.

**Owner: `domains/person`, not `domains/history`.** Both ends of the
relation are `is-person` entities — this crate's own subject type — the same
reasoning that already keeps `pays-tribute-to` (an occupation-to-occupation
relation) in `domains/history`, which owns *that* subject type. `domains/
history` computes the descent arithmetic the predicate reports the verdict
of; it does not follow that it owns the fact any more than owning
`occ-founded-from` would require owning `person-founded`.

**Resolved through entity identity, never `RoleHandle` equality.** Task 1's
panel found `founder_of`'s handle space collides on ~3.5% of seed 42's
occupations (`founding_key_from` folds with no discrimination tail, unlike
`founder_handle`), so matching a promoted founder by handle would
misattribute roughly 1 in 100 forebear edges — a wrong fact in a saved
world. `windows/worldgen::person_promote::promote` instead maps each cast
member's occupation `EntityId` (`Founder::community`) to its cast index,
and reads the mother occupation's `EntityId` directly off
`OccupationRecord::founded_from` (`Founding::From(EntityId)`) — no handle
anywhere in the identity path. The `Kinship` classification alone (never
the handle `forebear_of` also returns) comes from that same function, which
computes it from already-committed founding years and the species
allometry table.

**Committed in a second pass, after `hornvale_person::genesis` returns.**
The forebear's PERSON `EntityId` is minted by `genesis` itself (in cast
order — `genesis`'s own contract is that `ids[i]` corresponds to
`seeds[i]`), so it cannot be known before that call returns; `promote`
therefore resolves and commits `parent-of`/`kin-of` in a loop over `cast`
after `genesis`'s `ids` vector is in hand.

**No `Stream` draw.** `promote`'s only draw (`Namer::new(&world.seed,
...).name(...)`) lives in the first, unmodified pass; the kinship pass
touches only `records` (already materialized), a plain `EntityId -> usize`
map over in-memory `Founder` values, and `forebear_of` (a total function of
already-committed founding years and the species allometry table — no
`Seed`, no draw, per its own module doc). Verified by two means: a
structural test (`windows/worldgen/tests/suite/kinship_facts.rs`) exercising
the resolution and, separately, confirming two independent `BuildDepth::Full`
builds of seed 42 commit byte-identical ledgers.

## Consequences

- `cli/tests/fixtures/world-seed-42.json` gains 93 facts (84 `parent-of`, 9
  `kin-of`) — exactly spec §5's preregistered "+93 facts (+0.43%), kinship
  only", and exactly the entity-identity cross-check figure Task 1's ledger
  entry #6 measured ahead of time (93/76/35/204).
- `bundle:consanguineal-kin` reads 5/5 — the first bundle this project has
  fully satisfied — and drops out of both corpora's "missing bundles"
  tables entirely.
- `polti-1895` stageable stays **0 of 36** and `tvtropes-2012` stageable
  stays **0 of 409**, exactly as spec §5 preregistered: every situation
  `bundle:consanguineal-kin` used to block is still blocked by at least one
  other missing bundle.
- `docs/audits/seam-guard-roster.md` gains a second `person_promote.rs` call
  site for `ledger_day_of_bake_year` — the kinship pass recomputes a
  founder's day for the fact stamp, reusing the existing pure conversion
  rather than threading the value through `Founder`.
- The functional-contradiction guard (`kernel/src/ledger.rs::check`) is
  exercised for the first time by a *relation* predicate — every prior
  `functional: true` predicate in the workspace is either a scalar
  attribute or (`INSTANCE_OF`) declared `functional: false`.

## See also

Spec §4.3 (`docs/superpowers/specs/2026-09-01-the-avowal-design.md`);
`docs/superpowers/ledgers/2026-09-01-the-avowal.md` entries #3 (item 2, the
unspoken-machinery finding) and #6 (Task 1's panel and the handle-collision
finding); `domains/person/src/lib.rs`; `windows/worldgen/src/
person_promote.rs`; `windows/worldgen/src/descent.rs`; `windows/worldgen/
tests/suite/kinship_facts.rs`.
