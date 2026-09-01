# The Tableau — staging a situation, instead of hunting for one

**Campaign:** The Tableau
**Date:** 2026-08-31
**Decision block:** 0526-0535
**Status:** spec, awaiting G3
**Predecessors:** [The Repertory](2026-08-30-the-repertory-design.md), [The Company](2026-08-30-the-company-design.md)

## 1. The premise this corrects

The Company closed by reporting that `the-orange` — *a goblin has an orange, a
drow wants it* — is blocked by `SOC-one-creature-per-settlement`, and that this
was a settled ruling.

Both halves of that are wrong, and the campaign exists because of it.

**The binding constraint was not the world.** It was The Company's own spec
section 6, *"scenes are found, never staged"*, written and recommended by the
controller and approved on that recommendation. The world's one-creature-per-
settlement shape bites only because of that choice.

**And the choice was a category error.** Two products were forced into one
instrument:

| what a scene is for | how it must be built |
| --- | --- |
| a **capability audit** — does the world produce this? | **found**; a staged pass proves nothing about the world |
| a **drama you want to watch** | **staged**; the point is to see it |

There was one good argument — a staged `AUTHORED` is evidence about the
staging — and it was applied universally. The audit's requirement won a fight
the drama was never allowed to enter.

**The second half is systemic and worth recording separately.** The decision
log is append-only and supersede-never-edit, which is right for correctness and
misleading in tone: a record written to be durable *reads* as settled whether or
not it was. `SOC-one-creature-per-settlement` is a scale-management compromise —
it exists so the world does not carry a billion simulated creatures — and it was
read as a statement about what the world may contain. Nothing in the record's
format distinguishes a ruling from a stopgap-pending-cost. That gap gets a
`PROC-` row; this campaign does not fix it.

## 2. What this builds

A **tableau**: a declaratively-constructed situation, assembled without hunting
for a world that happens to contain it.

Permissive by ratification — anything a roguelike or a work of interactive
fiction could set up, we can set up. A bare room, two creatures of chosen
species, an orange in one of their hands, a light source or none.

Two customers, and the second carries the compounding value:

1. **Watching a drama.** The exit criterion is the orange, staged and watchable.
2. **Every test that currently pays genesis and hopes seed 42 contains what it
   needs.** That is the larger prize, and it makes *cheap* a hard requirement
   rather than a preference.

## 3. A tableau is a diff, not a world

A tableau **overrides named layers of an otherwise-derived session**. It does
not build a world from nothing.

This is what makes it cheap, and the mechanism already ships:

> `Session::start_in(ctx, opts)` — *"Begin a possession from an already-derived
> `WorldContext`, **paying none of its cost**. Every session started from one
> `ctx` is byte-identical to one started by `Session::start` over the same
> world."*

Every expensive layer — sculpted terrain, fitted climate, the species roster,
the demography fit — lives in that `WorldContext`. Build it **once per
process**; a hundred tableaux after it each cost a session start.

It also rules out the obvious alternative. A from-scratch world would have to
synthesize a `LocaleContext`, whose fields are private and typed to real
`GeneratedTerrain` and `GeneratedClimate`. That would be a second way to make a
locale, and two paths that can disagree about what a place is. Unnecessary,
besides: the drama is a **chamber** scene, and a chamber is not read through
the walk band's locale the way an outdoor room is.

And diff-not-scratch makes **partial** tableaux legal, which is what a
variation family — *the same room, unlit* — actually needs.

## 4. The builder is the artifact; the file is a front-end

The primary artifact is a **builder, in Rust**. The declarative file is a thin
serialization over it.

The two customers want opposite things. A test wants type-checking,
refactorability, and no parse cost. The corpus and a human sketching a scene
want a file. Data-first would make every test pay JSON to build a fixture and
put the fixture's shape outside the compiler; code-only would leave the corpus
unable to hold a staged scene. A serialization over one builder serves both and
keeps a single construction path.

**This overturns an assumption worth naming**, because the four existing corpus
families make it look obvious that a new capability arrives as a JSON format.
`tropes/`, `systems/`, `sentences/` and `repertory/` are **measurements**, and
their data-ness is decision 0011. A tableau is a **fixture**. 0011 does not
reach it.

## 5. Unspecified means empty, for anything you could have written down

The default is the whole campaign, so it is stated as a rule rather than left
to the implementation:

| layer | can it be fully stated? | unspecified means |
| --- | --- | --- |
| cast (who is here) | yes | **empty** — nobody |
| things, and who holds them | yes | **empty** |
| chamber geometry and light | yes | **empty** — a bare room |
| day | yes | **a stated default**, not the world's |
| terrain under the room | no | **inherit** |
| climate, species roster, sky | no | **inherit** |

If an unspecified cast inherited `derive_npcs`, then a tableau that stipulates a
chamber but not a cast would depend, silently, on whatever the seed happened to
place. That is precisely the complaint this campaign answers, and it is
factory_bot's **mystery guest**: a test passes because of something the factory
created that the test never mentioned. Hermetic-by-default for every stateable
layer is what makes a tableau reproducible **without a seed**.

**Corollary, and it must survive into the serialization: absent and empty have
to stay distinguishable.** A *negative* tableau — this room, but with nobody in
it, and unlit — is how a shadowcasting test isolates a variable. JSON's
missing-versus-null is the classic way to lose that distinction, and losing it
would quietly delete the ability to stage an absence.

## 6. Where the injection happens

At the two derivation calls a session already makes. Nothing is restructured.

| seam | what exists today |
| --- | --- |
| **the cast** | **the one real gap.** `Session::start` derives its roster internally; `bodies()` is read-only. `Body` is a `pub` struct whose `species` is a plain `String`, so a drow needs nothing but constructing one. |
| **the chamber** | `Interior` is **already a hand-buildable builder** — `Interior::new()`, `push(kind, within)`. Arbitrary geometry needs no new construction machinery, only an injection point where `interior_of(room, terrain)` is called. |
| **the things** | **no new seam.** `thing::promote` mints, `located_in_holder_fact` places, and `Session::place_thing_in_hand` (The Company) already composes them. |

**The tableau rides `PossessOpts` rather than inventing an idiom.**
`PossessOpts` is already a record of optional overrides applied at session
start, and `SkyPins` / `TerrainPins` / `SettlementPins` are the same pattern at
the genesis layer — three existing instances. A tableau is that pattern extended
past the point where the world stops being derived. Adding a field inherits
every existing caller and creates no second way to start a session.

## 7. Keeping the corpus honest

Repertory scenes gain `provenance: found | staged`, and the two verdicts read
differently:

- **staged `AUTHORED`** — the machinery carries this drama.
- **found `AUTHORED`** — the world produces this drama unprompted.

Conflating those is section 1's original error and must not be re-committed one
level down. It also makes `UNWITNESSED` **more** meaningful: it stops meaning
*we could not build it* and starts meaning *nothing assembles this on its own*,
which is the gap actually worth closing.

## 8. Exit criterion

**A goblin holding an orange, a drow who wants it, in a room, and Nathan can
watch it happen.** Committed to `repertory/` as a `staged` scene, so it is a
thing that keeps working rather than a thing that worked once.

A second criterion, for the other customer: **one test that today pays genesis
and asserts on a seed-dependent arrangement is rewritten against a tableau**,
with its cost measured before and after. One is enough to prove the path; a
sweep is a followup.

## 9. Non-goals

- **No verb addresses another creature.** The drow wanting the orange is staged
  and watchable; the drow *asking* for it is not this campaign.
- **No synthetic `LocaleContext`.** Terrain and climate inherit (section 3).
- **No sweep of existing tests onto tableaux.** One conversion, measured.
- **A tableau proves the machinery, never the world.** That is not a limitation
  to fix; it is why `found` scenes stay in the corpus beside these.

## 10. Flagged for review

1. **A tableau is outside the determinism contract by construction**, and that
   is the principled reason its ceremony can be low: it is a consumer of the
   sim, not a producer of worlds. It cannot corrupt a seed, move a golden, or
   change what a world means. Same reasoning as the board lane (decision 0129).
   **But it must not become a way to write a committed artifact** — a golden
   generated from a staged situation would assert about a world that never
   existed. No tableau output may enter `docs/generated-paths.txt`.
2. **`PossessOpts` gains a field, which touches every construction site.**
   Additive with a `Default`, so existing callers are untouched; flagged
   because the type is widely constructed.
3. **The cast seam changes what `bodies()` can contain.** A body no longer
   necessarily corresponds to a settlement. Anything that assumes the roster is
   settlement-derived needs finding — this is the campaign's main risk, and the
   plan must grep for that assumption rather than trust it is absent.
4. **Low confidence, no precedent either way:** whether a staged session may
   write facts to a ledger that is later saved (`possess --out`). Deferred to
   the plan; the conservative default is that a staged session refuses `--out`.
