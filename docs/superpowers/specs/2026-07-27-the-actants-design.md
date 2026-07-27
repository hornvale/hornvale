# The Actants — every thing a noun, every act a verb

**Campaign:** The Actants
**Date:** 2026-07-27
**Status:** DRAFT — at G3, awaiting review. **§3–§4 revised 2026-07-27** after
the ordering defect was diagnosed; the naming is no longer expensive.
**Depends on:** The Accession (`2026-07-27-the-accession-design.md`), which
must land first. Stage B is byte-identical behind it and world-perturbing
without it.
**Decisions in force:** 0025 (one concept name, one owner), 0073 (epoch
granularity is declared), 0063 (census regen is local-canonical), 0011,
0020.
**Registry:** MAP-27 (the verb as reaction), UNI-21 (the self-reflective
ledger — the capability schema as a derived view), LANG-55 (the codomain
subspace — The Accession's successor).

## 1. The gap

The question that opened this campaign: is the game's dictionary still 1:1
with the game's things and acts? Measured against the tree, no — and the
shape of the shortfall is not the one the question assumes.

```
  76 registered concepts    lexeme   covered 40  gap 36
                            percept  covered 10  gap 66
                            cognition covered 2  uncognized 74
```

Those gaps are sound: all four `cli/tests/correspondence.rs` checks pass,
every absence is a reason-bearing `Void`, and no dictionary row lies. The
forward audit works.

The defect is on the **reverse** side. `kernel/src/manifest.rs` reconciles
concept → ledger; only one reverse direction (phenomenon → concept, the
orphan-phenomena line in `cli/src/concepts.rs:152`) was ever built. Four
sibling directions were never generalized:

```
  reverse audit          status   orphans today
  --------------------   ------   -------------
  phenomena -> concept   BUILT    4  (heliacal-rising/-setting,
                                      seasonal-cycle, wandering-star)
  species   -> concept   MISSING  12
  actions   -> concept   MISSING  4
  predicates-> concept   MISSING  unmeasured
  prose     -> concept   MISSING  ~70+ literals
```

**Species.** `domains/species/src/lib.rs:1344` registers `*-kind` concepts
from a hardcoded four-entry literal, while `biosphere_registry()` holds 16
kinds. Twelve creatures — treant, twig-blight, giant-elk, woolly-mammoth,
giant-goat, otyugh, xorn, rust-monster, white/red/black-dragon, owlbear —
are simulated, placed, and narrated to the player as `format!("a wild
{species}")` (`windows/vessel/src/liveness.rs:3965`) with no concept, no
dictionary row, and no word in any language.

This is drift, not design. Git archaeology: the concepts were introduced
2026-07-09 (`4b989e5c`) when the roster held exactly two species, and both
were named. Hobgoblin and bugbear arrived later and were named. **The
Menagerie (`10c01ebe`, 2026-07-14) added the twelve fauna and named none** —
and neither its chronicle nor its spec mentions concepts at all. The
invariant held for five days and broke silently because nothing enforced it.

**Actions.** `Action` (`windows/vessel/src/liveness.rs:4058`) is `{MoveTo,
Drink, Rest, Eat}`. Checked against the 76: `drink`, `rest`, and `move` are
not registered concepts. Conversely `die` is a concept with no action.
`ConceptKind` has no verb/act variant at all, so the three verb-ish concepts
the Swadesh packs do carry (`eat`, `sleep`, `die`) are filed as `Quality` —
"an abstract property." The dictionary types the world's verbs as
adjectives.

## 2. The vocabulary cannot state the defect

The two absences wear the same clothes and are not the same thing:

- **unnamed (in-world)** — the registry holds a concept; a culture has no
  word for it because its speakers never met the thing. Exposure-gated,
  per-species, seed-dependent. Natural, deliberate, correctly modelled. 36
  of 76.
- **unnamed (out-world)** — the engine simulates a thing; no author
  registered a concept. Global, seed-independent, invisible to every ledger
  column. Synthetic. Authoring debt. 12 creatures, 4 acts.

`Void` can express only the first. The twelve fauna have no manifest at all,
so there is no row in which to record their absence — which is precisely why
no check could catch The Menagerie. **Closing the gap therefore requires a
mechanism outside the manifest, not a new `Void` variant.**

## 3. Registering a concept was expensive; that was a separable defect

The obvious fix — add twelve manifests with honest `Gap` lexemes — measured as
world-perturbing when this spec was first drafted: 70 place-name facts moved
and 11 artifacts churned. Diagnosing *why* turned out to be the more valuable
finding, and it is not this campaign's to fix.

`assign_proto_roots` orders the concept universe `(core_rank, id)` and walks
it, each concept drawing a proto-root and probing when its form is taken. A
concept's assignment therefore depends only on concepts sorted at or before
it — so an addition is free **iff** it sorts last, which ordering by id does
not arrange. Ten of the twelve kinds were free; `treant` cost 5 facts and
`otyugh` 65, summing exactly to the 70 measured when all twelve land at once.
Additivity was a coin flip.

That is **The Accession**'s defect, now specced separately: order by
`(epoch, core_rank, id)` so a new concept always lands strictly last.
Verified by spike — all 16 kinds registered, ledger **byte-identical**,
artifacts 4 files `+127 / -7` (added rows only), no almanac churn, no census.

```
                    before Accession   behind Accession
ledger              70 facts moved     BYTE-IDENTICAL
artifacts           11 files           4 files (added rows)
census              regen required     not required
```

**Consequence for this campaign:** the naming is no longer a costed "naming
day" needing census authorization. It is an ordinary additive change, so
Stage B folds back in — behind The Accession, and only behind it.

## 4. What this campaign does

Two stages, both zero world-drift, the second conditional on The Accession
having landed.

### Stage A — the reverse audit generalized

Mirror the orphan-phenomena mechanism, which is precedent-exact for "realized
thing, no concept names it," onto the two registries that lack it. Stage A is
independent of The Accession and could land in either order.

**A1. Orphan species.** In `cli/src/concepts.rs`, alongside `orphan_phenomena`:
kinds in `hornvale_species::biosphere_registry()` for which no `{kind}-kind`
concept is registered. Rendered as a new backlog line.

**A2. Orphan actions.** Same, over the action roster. `Action` has no
reflection, so it gains an exhaustive `all()` guarded by the house destructure
tripwire — a new variant then **cannot compile** without being listed, a
strictly stronger guard than species can have (whose roster is data-driven).
Each variant maps to its concept name (`MoveTo`→`move`, `Drink`→`drink`,
`Rest`→`rest`, `Eat`→`eat`); those with no registered concept are orphans. All
four are orphans today.

**A3. Prose language.** The backlog line and the page's framing name the third
reverse direction as *unaudited*, with its count, and stop there. Auditing it
needs a design line between "a nameable thing" and "texture"
(`windows/locale/src/grammar.rs`'s 32 relief descriptors — "a boulder field",
"erg dunes" — are arguably not nouns the dictionary owes a row). Followup, not
built.

**A4. Tests.** Mirroring `manifest_render_lists_orphan_phenomena`. Non-vacuity:
a test asserting the orphan sets are *non-empty* would invert the moment Stage
B lands, so the assertion is on **derivation** — that the line is computed from
`biosphere_registry()` / `Action::all()` and not a literal. The guard is that a
new species or action changes the generated page, which CI's drift check then
fails.

### Stage B — the naming

Register all 12 species concepts and the 4 act concepts at epoch 1, adding
`ConceptKind::Act` for the verbs. Byte-identical behind The Accession;
asserted, not assumed (success criterion 3). Stage A's orphan lines go empty
for species and actions, which is exactly why A4 asserts derivation rather
than population.

Two sub-questions it settles:

- **Doc text.** `format!("a {kind}")` yields "a giant-elk". Prefer an authored
  gloss per kind ("a giant elk").
- **Owner.** Decision 0025 forces one owner per concept name. `eat` is already
  owned by `language`, which argues for `language` for the act concepts, and
  for re-kinding `eat`/`sleep`/`die` from `Quality` to `Act` in the same
  change.


## 5. Explicitly not in scope

- The MAP-27 verb-chemistry DSL. Stage B gives verbs *nouns*, not a
  property-bag reaction system. The GOAP action set stays four hardcoded
  Rust variants.
- A fourth `Manifest` edge. Considered and rejected: the manifest reconciles
  a concept outward across ledgers; "this act has a concept" is the reverse
  direction, which is the orphan mechanism's job. Adding an edge would also
  force a decision for all 76 existing concepts.
- Auditing prose vocabulary (A3 names it; a design line is owed first).
- Predicate → concept reconciliation (the fourth empty branch; unmeasured).
- The ordering fix itself — that is The Accession. This campaign consumes it
  and must not reimplement or work around it.

## 6. Success criteria

1. `hornvale concepts --manifest` lists orphan species and orphan actions,
   derived from `biosphere_registry()` / `Action::all()` rather than literals.
2. Adding a 17th species, or a fifth `Action`, changes the generated page —
   and for `Action`, fails to compile until listed.
3. **Stage A** moves no world: seed 42's ledger byte-identical to the merge
   base, and `scripts/regenerate-artifacts.sh` touches exactly one file
   (`concept-manifest-generated.md`).
4. **Stage B** moves no world either: with The Accession landed and the 16
   concepts registered at epoch 1, seed 42's ledger is byte-identical and the
   artifact diff is added rows only (measured at spike: 4 files, `+127 / -7`).
   `otyugh-kind` is the regression case — the 65-fact offender before the
   ordering fix.
5. After Stage B the orphan-species and orphan-action lines read `none`, and
   the tests still pass **unchanged** — the proof that A4 asserted derivation
   and not population.
6. `make gate` green; type-audit clean (pub-boundary change in vessel).
