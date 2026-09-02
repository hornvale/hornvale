# The Avowal — the words for what the world already knows

**Date:** 2026-09-01 · **Branch:** `campaign/the-avowal` (based on `18f63ebfa`)
· **Decision block:** 0576-0585 · **Ledger:**
`docs/superpowers/ledgers/2026-09-01-the-avowal.md`

To avow is to declare openly what is already the case. This campaign does not
teach Hornvale anything new about drama. It gives the world words for
capabilities it already has, and it repairs the instrument that could not see
them.

---

## 1. The problem, measured

Two numbers sit next to each other and disagree.

```
  program capability   wolverson-2021    27 present · 5 refused · 26 deferred
                                         · 11 absent · 5 inapplicable  (of 74)
  world representability  polti-1895      0 of 36 stageable
                          tvtropes-2012   0 of 409 stageable
```

Hornvale builds worlds well and represents no dramatic situation at all.

### 1.1 The leverage curve, computed against the live registry

Greedy over both corpora (Polti weighted 4x for its smaller denominator),
resolved against the 395 tokens the registry holds today:

```
 #  bundle                      new preds  cum  tv/409  po/36
 1  norm-and-transgression              3    3       1      0
 3  agent-knowledge                     3    9       2      0
 5  felt-affect                         3   15       8      0
 7  interpersonal-violence              2   19      23      0
 8  speech-act                          2   21      36      0
 9  intent                              2   23      52      1
10  consanguineal-kin                   2   25      59      3
12  pair-bond                           3   30      95      4
15  personal-rank                       3   38     140      6
```

38 predicates would take tvtropes from 0 to 140/409. **This campaign
deliberately does not chase that curve**, for reasons §2 establishes.

---

## 2. Why the number is not the target

`cli/src/tropes.rs::resolve` tests **registry membership and nothing else**:
it builds `held` from `registry_tokens()` and calls a situation stageable when
every expanded token is present. No producer, witness, or instance is
consulted.

Registry membership is append-only, so the metric is **monotone by
construction**: it can never fall, and it cannot distinguish a capability from
a name. Adding 38 rows would move it to 140/409 with nothing behind them.

Decision **0330** already ruled this exact hazard on the sibling sentence
corpus — *"a token added on optimism moves the score without moving the
grammar, which would make the instrument worse than no instrument"* — and
answered it with a **realization witness**: a committed artifact demonstrating
the capability, checked by a test that must be able to fail. The trope corpus
has no equivalent. This campaign builds one.

### 2.1 The deeper defect: the audit reads one of three homes

A capability in this architecture can live in three places, and they are not
interchangeable:

| home | keyed by | persisted | example |
|---|---|---|---|
| the ledger | `EntityId` | yes, in the save | `is-person`, `occ-founded-from` |
| the component layer | `KindId` | **no** — build-state | `psyche`, `family_of`, `lexicon` |
| session state | `EntityId` | only when asked (0366, 0368) | a latched door, an agent's position |

`registry_tokens()` reads the `ConceptRegistry` alone. It cannot see
`WorldComponents` and cannot see session state. **A capability the world
genuinely has scores as absent whenever it lives anywhere but the ledger** —
which, under 0001, 0346 and 0366, is most of this architecture.

That is not a hypothetical. `bundle:felt-affect` reads 0/3 while
`windows/sentiment::snap_judgment` computes exactly its three demands, because
`snap_judgment` is kind-keyed component data and the audit only reads facts.

**Left unfixed, this instrument actively misleads: it rewards committing what
0346 and 0366 say should be derived.**

---

## 3. What is already built and unspoken

Verified by running commands, not by reading (the four entries of ledger #3):

- **Kinship.** `domains/history/src/descent.rs` exposes `Kinship`, `remove()`,
  `kinship()`, `ancestor()`, with tests, and commits **zero** facts.
  `windows/worldgen/src/descent.rs::forebear_of(world, occupation) ->
  Option<(RoleHandle, Kinship)>` already returns both the forebear and the
  degree. `person_promote.rs` turns a remembered founder into a real entity.
  The registry holds `concept:parent`, `concept:child`, `concept:sibling` and
  **no kin predicate**.
- **Affect.** `windows/sentiment` exposes
  `Judgment { warmth, competence, emotion }` and `snap_judgment(judger,
  target)`. `grep -rn 'Fact\|predicate\|commit' windows/sentiment/src/*.rs`
  returns nothing. Its own module doc: *"**No world is built**: every distance
  here is a function of two peoples' authored attribute vectors alone,
  computable before any seed exists."*
- **Staging.** `windows/vessel/src/tableau.rs` (The Tableau, 2026-08-31)
  states its own purpose in the terms this corpus needs: *"A tableau proves
  the MACHINERY, never the world. A scene staged here is evidence that the sim
  can carry a situation, and never evidence that any world produces one."*
  That is verbatim what `Outcome::Stageable` means. Its surface is
  `Tableau { cast: Vec<StagedBody>, things: Vec<StagedThing> }` — cast and
  props, **no relations**.

### 3.1 What is NOT built, stated so the plan cannot assume it

- **Combat does not exist.** `grep -nicE
  'attack|strike|slain|kill|damage|wound'` over `windows/vessel/src/liveness.rs`
  and `session.rs` returns 3 hits, all prose. `bundle:interpersonal-violence`
  (#7 by leverage, 129/409) is new world machinery, not vocabulary. **Out of
  scope.**
- **No act is addressable.** There is no `EventId` in the tree; `occ-cause`
  and `occ-ended-by` are `Text` labels, not references. Meanwhile 16,002 of
  seed 42's 21,635 facts carry a non-zero day: the ledger knows *when*
  everything happened and cannot say *that anything happened* as a thing.
- **No entity-keyed component store exists.** `grep -rn
  'ComponentStore<EntityId'` returns nothing; every `WorldComponents` field is
  `ComponentStore<KindId, _>`.

---

## 4. Design

### 4.1 The capability manifest (decision 0576)

A declared table mapping each corpus token to the resolver that serves it,
spanning all three homes:

```
  token                     home         resolver
  predicate:parent-of       ledger       ledger read over `parent-of` facts
  predicate:affect-kind     component    windows/sentiment::snap_judgment
  predicate:witnessed       session      derived read over the act view
```

`resolve` consults the manifest instead of `registry_tokens()` alone. A token
with no manifest row is **missing**, exactly as today — the change is
default-deny in the same direction, widened to see two homes it was blind to.

**The direction this check enforces, stated per the standing rule:** the
manifest asserts *declared ⊆ served*. It is structurally blind to a capability
that exists and is undeclared, and that blindness is deliberate — an
undeclared capability scores absent, which is the safe error.

### 4.2 The witness (decision 0577)

A situation earns `Stageable` only when a committed tableau places its actants
and every token its requirements name **resolves through the manifest against
that staged scene**.

`Tableau` gains a stated-relations layer, obeying its existing rule that
unspecified means empty.

**What this bar proves, stated precisely — overstating it is the failure this
project documents most.** `PredicateDef` is `{ name, functional, doc }` with no
object-type constraint, so a staged fact is not hard to satisfy. The witness
does **not** prove any world produces the situation and must never be described
as if it does. What it proves that membership does not:

1. the actants must exist as entities — the tableau has to place them;
2. `functional` must be declared correctly (a `feels-toward` wrongly marked
   functional breaks the instant one actant regards two targets);
3. the facts must pass contradiction-checking *together*, not one at a time;
4. **the cost of a false claim rises from zero to a file** — 140 claimed
   situations means 140 authored tableaux.

(4) is the load-bearing one, and it is what makes §4.4's grain ruling hold.

**It ships red.** Per 0330, the witness is built against a situation it should
*fail*, demonstrated failing, before anything is made to pass. A green witness
that has never been red is not evidence.

**Migration cost is zero and will never be this low again**: 0 situations are
stageable today, so there are no existing claims to retrofit.

### 4.3 Kinship — a genesis fact (decision 0578)

`parent-of` and `kin-of` are committed by the bake alongside `is-person`,
`name`, `person-born`, `person-founded`. They are entity-keyed and
world-specific, so the ledger is their home; 0366's "derive, never store"
governs *play-time* change, not the bake's own output.

Measured on `cli/tests/fixtures/world-seed-42.json`:

```
promoted founders                                204
  forebear occupation ALSO has a promoted person  93   <- parent-of edges
  forebear exists but is not promoted             76
  no entity forebear (root)                       35
promoted-ancestor chain depth: 0:111 1:47 2:22 3:12 4:9 5:2 6:1
```

**The 76 misses are the design, not a defect.** A forebear nobody remembers
has no entity and gets no fact — `RoleHandle`'s own documented intent ("never
materialized until something actually observes one"). The ledger says what is
remembered.

This completes `bundle:consanguineal-kin` at 5/5 — the first bundle this
project has ever fully satisfied.

### 4.4 Affect — component data, never a fact (decision 0579)

`snap_judgment` is kind × kind and world-invariant. Committing it would store
~840 facts identical in every world ever generated — precisely the second
source of truth 0366 objects to. It is declared through the manifest against
the component layer and **no fact is committed**.

**The grain ruling (Nathan, at brainstorm).** The corpus's `feels-toward` is
person-to-person; `snap_judgment` is people-to-people. `feels-toward` is
**not registered by this campaign**. `affect-kind` and `affect-intensity` are
grain-neutral and are, so `bundle:felt-affect` reads **2/3 and stays blocked**.
The trope number does not move until a person-scale producer exists.

This implements the ruling without minting a `people-feels-toward` near-synonym,
which decision 0326 warns against; the grain is carried by what the relation
points at, not by the predicate's name.

### 4.5 Acts — addressable without being stored (decision 0580)

An act gets a derived identity — an `ActHandle`, a pure function of its
constituents, the same shape as `RoleHandle` and `barrier_of`. Addressability
does not imply storage: `witnessed`, `deed-of`, `act-precedes` and
`act-occurred-on` are **derived reads over session state**, and persist only
when a snapshot asks (0366, 0368).

This is the section most likely to be split into its own campaign at G3.

---

## 5. Preregistration (decision 0016)

Frozen before any code. **The headline prediction is a null**, deliberately:

| claim | prediction |
|---|---|
| `bundle:consanguineal-kin` | 3/5 → **5/5** |
| `bundle:felt-affect` | 0/3 → **2/3**, still blocked |
| `bundle:witnessing` | 0/2 → **2/2** if §4.5 ships, else 0/2 |
| `polti-1895` stageable | **0 of 36 → 0 of 36** |
| `tvtropes-2012` stageable | **0 of 409 → 0 of 409** |
| seed-42 ledger growth | +93 facts (+0.43%), kinship only |

A stageable count above 0 is **a finding requiring investigation, not a
success** — under §4.2's bar it would mean a situation was witnessed, which
none of these bundles alone should achieve.

**Required probe before Task 2 commits to §4.3.** The 46% promoted-forebear
yield is measured on one seed, and one world is an anecdote. The implementer
measures the yield across a seed panel and reports the distribution. **Kill
criterion:** if the median yield is under 10%, §4.3 does not ship and the
campaign says so. The implementer chooses the panel and the discriminating
check from inside the code; this spec does not prescribe one.

---

## 6. Non-goals

- **Any claim that a world produces a situation.** The witness proves
  machinery. §4.2 says so twice on purpose.
- **Combat** (§3.1). `bundle:interpersonal-violence` stays blocked.
- **Person-scale affect.** Registered as an idea-registry row, not built.
- **Chasing the §1.1 curve.** Bundles are chosen by whether a producer exists,
  never by fan-in.
- **Real alphabets and writing systems** — Nathan's standing boundary; nothing
  here touches it.

---

## 7. Definition of Done

- Manifest, witness (demonstrated red first), and every tier in scope.
- `make rebaseline` **and** `make rebaseline-goldens`; whole-tree diff
  inspected. Registering predicates moves the keystone golden
  `cli/tests/fixtures/world-seed-42.json`, which `rebaseline` does **not**
  write.
- `docs/audits/trope-*.md`, `trope-matrix.md`, `docs/digest/`,
  `docs/audits/type-audit-report.md` regenerated in the same commit as the
  change that moves them.
- Chronicle entry; Confidence Gradient re-scored (0030) — §2.1 is a direct
  instance of its "an unpaired check scores as unchecked" clause.
- Retrospective in `docs/retrospectives/`; index row added.
- Idea-registry rows for person-scale affect and anything §4.5 defers.
- Decisions 0576-0580 written from the reserved block.
