# The Actants — every thing a noun, every act a verb

**Campaign:** The Actants
**Date:** 2026-07-27
**Status:** DRAFT — at G3, awaiting review
**Decisions in force:** 0025 (one concept name, one owner), 0073 (epoch
granularity is declared), 0063 (census regen is local-canonical), 0011,
0020.
**Registry:** MAP-27 (the verb as reaction), UNI-21 (the self-reflective
ledger — the capability schema as a derived view).

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

## 3. Registering a concept is a cosmological edit (verified)

The obvious fix — add twelve manifests with honest `Gap` lexemes — is the
expensive half, and the reason is load-bearing enough to state as a finding.

`domains/language/src/lexicon.rs:279-289` assigns proto-roots over a
universe that is **every registered concept**, Steeped/KnowsOf/Unknown
alike, deliberately: it keeps roots world-independent so cognates match
across worlds that expose different concepts. The assignment is injective
and merger-aware, resolved by open addressing. So the *cardinality and
alphabetical membership* of the registry is load-bearing: inserting a
concept changes the occupied-form set seen by every concept sorting after it,
and roots reshuffle.

Measured, not reasoned — spike on this branch (all 16 kinds registered from
`biosphere_registry()`, then reverted):

```
$ cargo run -q -p hornvale -- new --seed 42
  base : 7466 facts, village Qvooshtvoagootao
  spike: 7466 facts, village Qvooshtvoagootao
  seed equal: True     ledger equal: FALSE
  positions differing: 70   predicates affected: [('name', 70)]
    "Zhvekngokngaknoenoanoaboo" -> "Zhvekngokngakbaonoanoaboo"
    "Roroqrraxoxo"              -> "Rarraroqrraxoxo"

$ bash scripts/regenerate-artifacts.sh   (censuses SKIPPED)
  11 artifacts changed, 361 insertions / 241 deletions
  3 almanacs, connections, settlement, the-book, scene-tiles,
  concept-registry, concept-manifest, dictionary, proto-goblinoid
```

Seventy place names move. Existing *saved* worlds keep their names (names are
committed facts; manifests are `#[serde(skip)]`), so nothing corrupts — but
the same seed stops reproducing the same world, which is the determinism
contract that matters for goldens. The census and the calibration battery
would need rebaselining on top of the 11 artifacts; **neither was run — that
is a carve-out requiring authorization (decision 0063).**

Three consequences:

1. **Excluding gap concepts from the universe is not available.** It would
   make root assignment world-dependent and break cognate safety — the exact
   invariant the comment at `lexicon.rs:281` exists to protect.
2. **The cost is per-batch, not per-concept.** One reshuffle, one regen,
   whether the batch is 1 concept or 40. Drip-feeding naming across
   campaigns is therefore the maximally expensive policy — and it is the
   policy currently in force by default.
3. **The registry's cardinality is a physical constant of the world.** The
   intended effect of registering a concept is a dictionary row; the actual
   effect is that every place name in every world changes. Growing it
   incidentally inside an unrelated campaign is the error.

## 4. What this campaign does

Take the teeth now at zero world-drift; schedule the naming as its own
authorized batch.

### Stage A — the reverse audit generalized (zero drift)

Mirror the orphan-phenomena mechanism, which is precedent-exact for
"realized thing, no concept names it," onto the two registries that lack it.

**A1. Orphan species.** In `cli/src/concepts.rs`, alongside `orphan_phenomena`:
kinds in `hornvale_species::biosphere_registry()` for which no
`{kind}-kind` concept is registered. Rendered as a new backlog line.

**A2. Orphan actions.** Same, over the action roster. `Action` has no
reflection, so it gains an exhaustive `all()` guarded by the house
destructure tripwire — a new variant then **cannot compile** without being
listed, which is a strictly stronger guard than species can have (whose
roster is data-driven). Each variant maps to its concept name
(`MoveTo`→`move`, `Drink`→`drink`, `Rest`→`rest`, `Eat`→`eat`); those with
no registered concept are orphans. All four are orphans today.

**A3. Prose language.** The backlog line and the page's framing paragraph
name the third reverse direction as *unaudited*, with its count, and stop
there. Auditing it needs a design line between "a nameable thing" and
"texture" (`windows/locale/src/grammar.rs`'s 32 relief descriptors — "a
boulder field", "erg dunes" — are arguably not nouns the dictionary owes a
row). Registered as a followup, not built.

**A4. Tests.** Mirroring `manifest_render_lists_orphan_phenomena`: assert
the orphan-species line contains a known fauna kind and excludes a peopled
kind; assert the orphan-actions line is derived from `Action::all()` and not
a literal. Non-vacuity: a test that fails if the orphan sets are *empty*
would invert on Stage B, so the assertion is on **derivation**, not
population — the guard is that a new species or action changes the generated
page, which CI's drift check then fails.

Stage A changes one generated artifact (`concept-manifest-generated.md`,
two added lines). No world moves. No census.

### Stage B — the naming day (NOT built here; costed for authorization)

Register all 12 species concepts and the 4 act concepts in **one** batch,
with `ConceptKind::Act` added for the verbs, paying one reshuffle. Requires:
an epoch declaration per decision 0073, a full artifact regen, a local
census regen, and a calibration rebaseline. Presented at G3 as a decision
for Nathan, not executed. Two sub-questions it must settle:

- Doc text for hyphenated ids: `format!("a {kind}")` yields "a giant-elk".
  Prefer an authored gloss per kind ("a giant elk").
- Whether the 4 act concepts belong to `language` (Swadesh-adjacent, joining
  `eat`/`sleep`/`die`) or to a new owner. Decision 0025 forces one owner;
  `eat` is already owned by `language`, which argues for `language` and for
  re-kinding `eat`/`sleep`/`die` to `Act` in the same batch.

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

## 6. Success criteria

1. `hornvale concepts --manifest` lists orphan species and orphan actions,
   both non-empty today, both derived from their registries.
2. Adding a 17th species, or a fifth `Action`, changes the generated page —
   and for `Action`, fails to compile until listed.
3. `git diff` after `scripts/regenerate-artifacts.sh` touches exactly one
   file, and no `name` fact in seed 42 moves (the Stage-A drift bound,
   asserted by rebuilding seed 42 and diffing the ledger).
4. `make gate` green; type-audit clean (pub-boundary change in vessel).
