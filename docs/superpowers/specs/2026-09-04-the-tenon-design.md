# The Tenon — design

**Campaign C of the `MAP-one-kind-model` arc: kind-to-kind edges.**
Predecessors: The Wicket (a kind is a row), The Plumb (every constant declares
its axis, decision 0586), The Pallet (a body chooses where to sleep and commits
what it chose; decisions 0696/0697/0698).

Decision block: **0726–0735**. Ledger:
`docs/superpowers/ledgers/2026-09-04-the-tenon.md`.

A tenon holds because two pieces were each cut to suit the other, and neither
piece names the other. That is the campaign's thesis in one word.

---

## 1. What is being built, in one paragraph

Today the world can say *how much an afforded site helps a body of this
species* — a one-dimensional table keyed on the sleeper (0697). It cannot say
*is a bed better than bracken, for a drow*, because that is a relation between
two kinds and no such relation exists anywhere in the tree. This campaign
builds one, as a **derived edge**: each side carries traits, and the value of
the pair is a function of them, so nothing anywhere holds a `(species, thing)`
cell. It also adds the sleepable kinds without which the relation would have
nothing to relate — today `bed` is the only carrier of
`ObjectProperty::SupportsRest`, and it requires a room that is both built and
cold, which is 18.6% of built rooms at the median world and 0% of wild ones.

## 2. Decided before the design began

| # | question | answer | where |
|---|---|---|---|
| G1a | does this campaign add sleepable kinds, or is that a prerequisite? | **adds them**, epoch accepted | Nathan, ledger #2 |
| G1b | authored matrix, derived function, or general typed graph? | **derived function** | Nathan, ledger #3 |

Both were brought to Nathan rather than resolved from precedent: the first is a
fidelity/scope call and the brief names it as one; the second is the campaign's
core architectural bet.

## 3. The rank of the relation, and why it is 2

The lift that produced §2's answer also produced the axis the design has to
choose a point on. An authored M×N matrix of rank *r* **is** *r* latent traits
per side (matrix factorization), so "table or function" was never the question;
*rank* is.

```
 rank | form                              | can express          | cannot
 -----+-----------------------------------+----------------------+-----------------
  0   | one global constant               | "a bed helps"        | any variation
  1   | species scalar x surface scalar   | a shared ladder,     | ANY reversal
      |                                   | scaled per species   |
  2   | + one contrast both sides carry   | a reversal on that   | a reversal on a
      |                                   | contrast             | second contrast
 full | authored (species, thing) matrix  | anything             | (rejected, G1b)
```

**Rank 1 is disqualified by the campaign's own name.** A rank-1 model is
*separable*: the surface scalar multiplies uniformly and the species scalar
multiplies uniformly, and nothing about the *pair* is stored anywhere. That is
not an edge; it is two independent kind-level components, of which the tree
already has **95** (`grep -rn 'ComponentStore<KindId' --include=*.rs
domains/*/src windows/*/src | wc -l`; a whole-tree grep answers 103 by
counting test files, and the figure that matters is authored tables). An edge carries information not present in either endpoint
alone, which begins at rank 2.

**Rank 2 is what the campaign's motivating sentence demands.** *A dwarf sleeps
better on stone than on straw* is a **reversal** — for a human the order runs
the other way. A model that cannot invert an ordering between two species
cannot say it.

**Rank 2 is where this stops**, and the reason is `ObjectProperty`'s own rule:
each axis exists because a shipped consumer reads it, and an axis no consumer
would consult is cut rather than kept. A third contrast has no consumer today.

### 3.1 The exemplar changes actor, and this is a finding rather than a detail

**This roster's dwarves are not subterranean.** `habitat_realm_registry`
(`domains/species/src/lib.rs`) is sparse and holds exactly three rows —
`rust-monster`, `xorn`, `drow` — and its own comment records that The Delvers
"briefly added two subterranean PEOPLES here and withdrew them." Nothing else
in `biosphere_registry` separates `hill-dwarf` from `human`: both are
`Endothermic`, both `Settled`, both under a tonne, and both take
`MADE_FOR_THE_BODY` in `sleep_grade_registry`.

So *a dwarf prefers stone* is **not producible** by any derived model over
today's traits, and would require authoring a new species axis whose only
consumer is this campaign. The roster does supply the same sentence with a
different actor: **drow**, whose subterranean row is described in its own
comment as "Drow's ONLY authored separation from the surface elves… deliberately
its only one," authored so that a mechanism could find it. Drow settlements
reach real worlds — seed 42's `settlement-seed-42.md` reads *"The chief drow
settlement, Poogpao, holds 25 souls amid temperate-forest."*

**The campaign therefore ships the sentence with drow as its subject and does
not author a dwarf axis to rescue the original phrasing.** Flagged at G3
because it changes the headline example Nathan wrote.

## 4. Where the relation lives — the layering answer

The brief called this the campaign's sharpest structural question. Under a
derived edge it dissolves: **no crate ever names both rosters.**

```
  domains/species     substrate_response_for(KindId) -> ConditionResponse
                        derived from habitat_realm_registry
                        names SPECIES keys only
                        sleep_grade_registry            [SHIPPED, unchanged]

  domains/thing       (unchanged -- see 4.1)

  windows/vessel      ObjectTraits.rest: Option<RestSurface>
                        names THING keys only

  windows/vessel      sleep_grade_for(species, kind)
                        the combining function
                        names NEITHER roster's keys
```

There is no sibling-domain dependency to justify and no composition-root
exception to claim. `windows/worldgen` is not involved.

### 4.1 The surface offer goes in the row the marker is in, and this is not a style choice

The obvious shape — `ObjectProperty::SupportsRest` stays a marker in
`object_registry`, and a second `KindId`-keyed table carries the scalar — is
**the exact structure that has already drifted one file away**, and the
campaign must not rebuild it.

`ObjectProperty::RadiatesHeat` is a marker with two carriers (`hearth`,
`brazier`). Its scalar lives elsewhere, in `warmth_at`
(`windows/vessel/src/interior/field.rs:73`), as a literal
`if interior.anchor(id).kind != kinds::HEARTH { continue; }`. The marker grew a
second carrier and the scalar did not follow. `object_registry`'s own doc
records this as live: *"no other kind contributes a degree of warmth however
many rows this table grows."*

So `RestSurface` is a **field on `ObjectTraits`**, in the same row as the
property set, with a two-way agreement invariant asserted in both directions:

- every kind carrying `ObjectProperty::SupportsRest` has `rest: Some(_)`, and
- every kind with `rest: Some(_)` carries `ObjectProperty::SupportsRest`.

A one-directional check here would be blind to exactly the half that broke
`RadiatesHeat`, and would still read as total to the next reader — the failure
mode The Wicket's six registry checks were written to name.

`domains/thing` is deliberately untouched. Its own module doc states the rule:
*"The property vocabulary lives in `windows/vessel`'s `ObjectTraits`, and this
crate deliberately holds no second copy of any of it"* — and records that
`ThingTraits::portable` was **deleted** rather than kept once `ObjectProperty`
carried the same fact. Adding rest data there would mint the disagreement that
deletion prevented. `domains/thing` still gains the new kinds' `KindId`s,
`THING_KINDS` entries, `thing_registry` display rows and concept
registrations — that is what a new kind costs and it is unrelated to this rule.

## 5. The combining function

```
  RestSurface {
      offer:     f64  in [0,1]   fraction of a fully-offering site's benefit
      substrate: Substrate
  }

  Substrate = Made               a surface built by, and for, whoever built it
            | Natural(hardness)  hardness in [0,1]; 0 = yielding, 1 = rock

  fit(species, substrate) = 1.0                                  if Made
                          = substrate_response_for(species)
                                .eval(hardness, FIT_FLOOR)       if Natural

  FIT_FLOOR is ConditionResponse::eval's `floor` argument: the suitability
  a species retains at any hardness, so a badly-matched surface degrades
  toward bare ground rather than becoming worthless. Authored, tagged.

  grade(species, kind) = 1.0 + (S[species] - 1.0) * offer(kind) * fit(species, kind)

  where S = hornvale_species::sleep_grade_registry   [39 rows, decision 0697]
```

### 5.1 Why this shape and not an additive one

An additive reconstruction of the shipped ladder is **inadmissible under
decision 0586**, and this is worth stating because it is the design I queued
first. The ladder is not additive: `CONTACT_ONLY` 1.20 plus the insulation
step (1.35 − 1.20 = 0.15) plus the fit step (1.30 − 1.20 = 0.10) gives 1.45,
while both-halves is 1.50. Reconstructing it additively needs a `+0.05`
interaction constant, and that constant's only honest source is an author
balancing a table — the *institutional-design* answer 0586 exists to refuse. A
`plumb:` reason for it could only say where the number came from, which is the
neighbouring question 0697 was itself minted to correct.

The multiplicative form has no such constant. `offer` is an interpretable
fraction ("this surface gives 70% of what a made bed gives") and `fit` is a
suitability in [0,1] that an existing kernel type already computes.

### 5.2 The bed column is byte-identical by construction, and that is the load-bearing property

`bed` is `Substrate::Made` with `offer = 1.0`, so `fit = 1.0` and

```
  grade(species, bed) = 1.0 + (S[species] - 1.0) * 1.0 * 1.0 = S[species]
```

for **every one of the 39 rows**, exactly. Decision 0697's ceiling ruling —
*no kind in any world gains more from a bed than it did before this table
existed, and the peoples keep their number byte for byte* — survives untouched,
and the existing table is reinterpreted rather than re-authored: it now means
*the grade on a fully-offering made surface*, which is what it always was.

`Made` yielding `fit = 1.0` **universally**, including for a wild endotherm, is
correct rather than a shortcut: `S` already encodes whether a species can
collect the fit half at all (`INSULATION_ONLY` 1.35 is precisely "endotherm,
cannot collect fit"). Applying a second fit penalty on a made surface would
charge that species twice.

### 5.3 The two substrate curves

`substrate_response_for` is a function over `habitat_realm_registry`, not a new
39-row table — the same discipline `sleep_grade_registry` follows in reading
its rows off `biosphere_registry` rather than inventing biology. Two authored
`ConditionResponse` curves, each `per-species` with a stated biological source:

- **Surface** (the default, every kind not in the sparse store) — optimum at
  the yielding end.
- **Subterranean** — optimum at the hard end: a kind that lives in rock is
  habituated to lying on it.

`hornvale_kernel::ConditionResponse { optimum, width, devotion }` and its
`eval(field, floor)` already exist (`kernel/src/ecology.rs`) and already carry
this exact semantics for temperature niche. `devotion` is the field that makes
the model rank-2 without an inadmissible interaction constant: it is *how much
this kind's rest depends on the substrate at all*.

**The numbers are not fixed here.** Each is an authored constant needing a
`plumb:` rung tag naming its axis, and the implementer authors them against the
properties §7 preregisters, not against values copied out of this document.

## 6. The fold reads what was committed

Decision 0698 committed `SLEPT_ON` and recorded that *"nothing reads the
predicate yet, deliberately… Grading a body's outcome on which kind it found is
the `per-people` rung 0697 defers."* **This is the campaign that reads it.**

`rest_timeline` (`windows/vessel/src/liveness.rs`) currently tags every bout
`SiteGrade::Bare` and then upgrades it by an ordered merge against the position
timeline, asking `room_affords_rest` — a room-level boolean. It gains a second
ordered merge, against the entity's `SLEPT_ON` facts by day, in the same
`O(bouts + facts)` cursor idiom the position merge already uses. A bout with a
`SLEPT_ON` fact is graded on **the kind that was committed**; a bout without one
falls back to today's room-level read.

The fallback is not vestigial and must not be deleted: `Action::Rest` bouts and
every pre-Pallet ledger have no `SLEPT_ON` fact, and a fold over committed
history must still grade them.

**This does not touch decision 0069.** The grade becomes kind-granular, never
anchor-granular: it reads a registered concept and a stable string, which is
exactly the line 0698 drew.

## 7. The kinds added, and what must be measured before they are frozen

Three surfaces are proposed. **The set is a proposal, and the plan must not
freeze it before the reachability property below is measured** — decision
0398's finding is that a gate whose predicate is false everywhere is not a
gate, it is a deletion, and it cost that campaign a strongbox no world could
contain.

| pattern | kind | band | why |
|---|---|---|---|
| `the-bracken` | `bracken` | wild (`built: false`) | the wild band's first sleepable surface; today 100% of wild rooms offer nothing |
| `the-rushes` | `rushes` | built, not cold | the ~81% of built rooms that are warm; **and co-occurs with the bed in cold rooms**, which is the first room in any world offering a body a choice |
| `the-ledge` | `ledge` | built | a cut stone shelf — the hard-substrate surface the reversal needs, reachable wherever anything is built |

All three are `at_locale: true`. **This is the epoch**, and it is the epoch
Nathan accepted at G1a: a locale-band pattern feeds the composed interior a
creature's drives read, so committed history moves and every world regenerates.
Appending is the only legal position (`INVENTORY`'s rule 1: reordering or
inserting is always an epoch, because the array order *is* the grammar's
dependency order); all three attach `Beside(kinds::GROUND)` with
`requires: None`, so appending is also dependency-correct.

### 7.1 The epoch's blast radius — a branch table, not a prediction

Three `at_locale: true` appends move a creature's composed interior, therefore
its drives, therefore committed history. **What follows is a decision rule per
observed outcome, not a forecast of the diff.** A forecast can be wrong; a
branch table covering the responses cannot, and this campaign has no business
predicting a diff it has not run.

Run `make rebaseline`, then
`git diff -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)`,
and read what moved:

| what moved | what it means | what to do |
|---|---|---|
| `book/src/gallery/` transcripts | a walked chamber or locale now shows new furniture | expected — review the prose reads well, commit in the same commit |
| `docs/audits/` | a `pub` boundary or an authored constant changed | expected — `type-audit` and `plumb` reports drift on this campaign by construction; commit with the change |
| `docs/digest/` | a decision record was added or superseded | expected once §10's records land |
| `clients/game/core/tests/fixtures/` | `snapshot_json` or an embedded `hornvale_scene` float moved | **investigate before committing** — this campaign changes no scene surface, so a move here is unexplained |
| `book/src/laboratory/generated/the-census/` | a census column moved | **STOP** — a census is authored on lefford only; this is a refresh, not a rebaseline. See the census block in the root `CLAUDE.md` |
| `book/src/domesday/` | reads the committed census | moves only when the census does; same STOP |
| `book/src/reference/concept-registry.md` | the new kinds registered concepts | expected |
| `book/src/reference/stream-manifest.md` | **a new seeded draw was introduced** | **STOP** — this campaign introduces none. A manifest move is a save-format contract change nobody asked for |
| nothing at all | the artifacts are blind to the change | **STOP** — this is an epoch; something must move. An empty diff is a positive-control failure, not a clean bill |

The byte-goldens are the trap the brief names: they are invisible to both
`make rebaseline` and `make gate-commit`. Run them **by name** after the
behaviour change, not by trusting a green gate. Decision 0697 recorded that the
affect-trace golden is blind to the afforded path at seed 42; that null was
established with four controls and must not be assumed to still hold once the
world has bracken in it.

### 7.2 Preregistered — the properties, not the constructions

Frozen before any constant is authored (decision 0016). Each names a property
the implementer must demonstrate; **none prescribes a fixture, a seed or a
mutation.** The arc's own lesson is that naming what a test must demonstrate
has beaten specifying how, every time it has been tried here.

- **P1 — the bed column is unmoved.** For all 39 species,
  `grade(species, bed) == sleep_grade_registry[species]` exactly. A byte
  comparison, not a tolerance.
- **P2 — a reversal exists and is world-reachable.** Some pair of species and
  some pair of surfaces order oppositely, *and* the surfaces both occur in
  rooms a real world composes. A reversal only demonstrable on hand-built
  rooms fails this: The Pallet already recorded that its affordance path "has
  no world-scale witness at all," and repeating that is the failure this
  campaign was scoped to avoid.
- **P3 — every added kind is reached.** For each of the three, a real world
  composes at least one room containing it. Measured over a seed sweep, with
  the sweep width and the count reported — an existence claim at n=1 is what
  decision 0097 converted to a rate for exactly this reason.
- **P4 — the choice is consequential.** At least one room in a real world
  composes two rest surfaces whose grades differ for the body standing in it.
  Without this the edge exists and never fires.
- **P5 — bare ground is still reachable and still worse.** Adding surfaces
  must not make `SiteGrade::Bare` unreachable; a body must still be able to
  sleep in the road. The share of bouts graded bare is reported, not asserted
  to any value.
- **P6 — the null is publishable.** If the measured share of afforded bouts is
  unchanged, or if no reversal reaches a world, that is the campaign's
  headline result and ships as one. It is not a reason to retune a constant
  after unblinding.

**P3 and P4 are answers to a question nobody has ever asked of this world**:
whether any body in any world has ever taken an `Afforded` bout is currently
unmeasured. Decision 0697 records that the affect-trace byte-golden is blind to
the afforded path, and `PSY-rest-site-is-a-tuning-indicator` records the
reachability test as unbuilt. **The baseline must be measured before the kinds
land**, or the campaign cannot say what it changed.

## 8. The chooser prefers, and the diagnostic survives

`select_sleep_site` currently takes the first `Sleep`-offering anchor by
ascending `AnchorId`, and its doc calls that "a placeholder, not a preference."
This campaign makes it prefer the higher grade, ties still breaking on
ascending `AnchorId`.

**That doc also carries a warning this campaign must answer rather than step
over.** It says a future chooser "must preserve a body's ability to end up on a
worse site than the room actually offers," and that "what must never happen is
a version of this chooser that is *guaranteed* to find the optimum — that
guarantee is exactly the thing that would erase the tuning signal."

Within-room argmax *is* guaranteed to find the room's optimum, so the letter of
that warning is not satisfied. Its purpose is, and the reason is the rule
directly above it in the same doc: **the chooser never proposes movement and
never looks past this room's anchors.** A body sleeps on bracken while a bed
stands in the next room, and that is exactly the observation Nathan asked for —
*a creature sleeping in an unrestful place is a useful indicator that something
needs tuning.* The diagnostic lives in the no-travel rule, not in the tie-break.

This is a decision, not an inference, and is recorded as one rather than
resolved silently inside a doc comment. **Flagged at G3**: it amends a warning
The Pallet wrote deliberately.

## 9. What this campaign does not do

- **No general typed-graph edge mechanism.** Form C is deferred with its
  blocking condition now named: a second `EdgeKind` with a live consumer. The
  `symmetry` finding travels with it — `grows-on` is anti-symmetric,
  `family_of` is symmetric-transitive, and `rests-on` is neither (the *fit* is
  symmetric, the *payoff* is not), so a single undirected edge abstraction
  would silently merge all three. Captured as an idea-registry row.
- **No per-individual rung.** `Lineage`-derived variation stays Campaign D.
- **No sparse authored override.** Considered and declined at G1b: a second
  source of truth for one question is the disagreement `domains/thing`'s own
  doc records deleting a field to prevent.
- **No material layer.** A rest surface's quality is not routed through a
  material registry. `domains/terrain::material_registry` holds two rows
  (granite, limestone) and is terrain's; reaching it from `domains/thing` would
  be a sibling-domain dependency, which is constitutionally forbidden.
- **`FURNISHING_COLD_C` is not converted.** The brief names it as the finding
  this mechanism would unblock, and it is genuinely adjacent — but it is a
  `per-people` fact about *which rooms get furnished*, not about *what a body
  rests on*, and it sits in the furnishing predicate rather than the grade.
  Folding it in would widen an already epoch-bearing campaign. Recorded as a
  follow-up with the reason.
- **Neither open finding from the brief is folded in.**
  `TOOL-plumb-walk-blind-to-let-bindings` and
  `TOOL-lab-run-simulation-swallows-commit-errors` stay their own rows. The
  second is a live hazard for this campaign rather than a task in it: it
  truncates a simulation on any commit error, and this campaign registers no
  new predicate — but it does change what `SLEPT_ON` is read for, so any
  calibration run must be checked for early termination rather than trusted.

## 10. Decisions this campaign expects to record

From the reserved block 0726–0735:

| # | subject |
|---|---|
| 0726 | a kind-to-kind edge is derived from traits both sides carry, never authored per pair |
| 0727 | the rank of a derived relation is set by whether a reversal is wanted |
| 0728 | a scalar belongs in the row of the marker it depends on |
| 0729 | the recovery fold grades on the committed kind, not the room's boolean |
| 0730 | the chooser prefers; the no-travel rule is what preserves the diagnostic |

Numbering within the block is not binding; gaps cost nothing.
