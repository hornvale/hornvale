# The Avowal Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the ledger words for capabilities the world already computes, and
repair the coverage instrument that could not see them.

**Architecture:** A capability *provision table* lets the trope resolver resolve
a corpus token against any of three homes — the ledger, the `KindId`-keyed
component layer, or session state — instead of the concept registry alone. A
realization witness, built on The Tableau, requires every `Stageable` verdict to
be demonstrated by a staged scene. Kinship then lands as a genesis fact, affect
as component data committing nothing, and acts as derived identity persisting
only on snapshot.

**Tech Stack:** Rust 2024, std + `serde`/`serde_json`/`libm` only.
`BTreeMap`/`BTreeSet`/`Vec`, never `HashMap`/`HashSet`. `cargo nextest`.

**Spec:** `docs/superpowers/specs/2026-09-01-the-avowal-design.md`

## Global Constraints

- **Layering** (`cli/tests/suite/architecture.rs`): `kernel/` → `domains/*` →
  `windows/*` → `cli/`. A domain depends on the kernel and **nothing else**.
  `windows/worldgen` is the composition root.
- **Dependencies:** no new crates. The allowlist is `ALLOWED_EXTERNAL` in
  `cli/tests/suite/architecture.rs`.
- **Determinism:** no wall-clock; no `HashMap`/`HashSet` (enforced by
  `clippy.toml`); float sorting via `total_cmp`. Quantization at emit only.
- **Save format:** seed-derivation labels and **stream consumption order** are
  contracts. A change that consumes a draw the unpinned path did not consume
  corrupts every world.
- **Docs:** every crate is `#![warn(missing_docs)]`; every public item, field
  and variant gets a one-line doc comment.
- **Type audit:** every primitive at a `pub` boundary carries a `type-audit:`
  tag. Adding `pub` items drifts `docs/audits/type-audit-report.md`, which must
  be regenerated **in the same commit**.
- **Naming:** the capability table is `Provision`, **never `Manifest`** —
  `kernel/src/manifest.rs` already owns that word for a different object.
- `cargo fmt` is the final step before every commit; `make gate-commit` before
  every push.

## Before dispatching each task

**Verify the brief against the code immediately before that task, one task
ahead — never in a batch now.** Three minutes of `grep` repeatedly catches wrong
counts, incoherent instructions, and steps that structurally cannot work. Every
count and signature below was true at `18f63ebfa`; several will move.

**Implementers outrank this plan.** If a step is wrong, refuse it and say why in
your report. The best outcomes on this project have been refusals.

**Why the test steps below name behaviours rather than supply code.** The
writing-plans skill asks for literal test code in every step. This plan
deliberately does not, and the reason is measured: on this project every
substantive defect of several recent campaigns originated in the controlling
session's own plan text, none in implementer code, and both prescribed mutations
of one campaign were nulls that the implementer replaced with discriminating
ones found from inside the code. A plan author does not know which derivations
share a stream; the implementer does, after reading. So each step names **the
property the test must demonstrate** and the file to read first, precisely
enough to be unambiguous, and leaves the code to whoever has the module open.
Where a signature IS given below it was read from the tree at `18f63ebfa`, not
recalled.

---

### Task 1: The promoted-forebear yield probe (kill criterion)

Pure measurement, no production code. This decides whether Task 5 ships at all,
so it runs first.

**Files:**
- Create: `windows/worldgen/tests/suite/promoted_forebear_yield.rs` (probe,
  `#[ignore]`d with a reason naming its cost). **Not "per PROC-6"** — that
  guard is `windows/lab/tests/suite/preregistration_guard.rs` and its own doc
  says its crate root is `windows/lab`, so it does not reach this crate.
  Nothing mechanically enforces the reason here; follow the local convention
  instead, e.g. `approach_ease_calibration.rs:249`:
  `#[ignore = "calibration: run by hand, prints the approach_ease quantiles"]`.
- Modify: `windows/worldgen/tests/suite.rs` (register the module)

**Interfaces:**
- Consumes: `hornvale_worldgen::descent::forebear_of(world: &World, occupation: EntityId) -> Option<(RoleHandle, Kinship)>`; `world_builder::build_world`.
- Produces: a reported yield distribution. No API.

- [ ] **Step 1: Read the producers before writing anything**

Read `windows/worldgen/src/descent.rs` (`forebear_of`, `founder_of`) and
`windows/worldgen/src/person_promote.rs` (`Founder`, `MEMORY_DEPTH`, `promote`).
Confirm how a promoted founder links to its occupation — the seed-42 fixture
reaches it via `person-founded`, but the in-memory path may differ, and the
probe must use the real one.

- [ ] **Step 2: Write the probe**

**Define yield exactly, because two defensible denominators differ by 9
points.** For this probe:

```
  yield = (promoted founders whose forebear is ALSO promoted)
          / (all promoted founders)
```

i.e. roots and unpromoted-forebear cases both count against it. On seed 42's
fixture that is 93 / 204 = **45.6%**. The other reading — 93 / 169, excluding
the 35 roots — is 55.0% and is **not** the criterion. Report both, label them,
and apply the kill criterion to the first.

Over a seed panel you choose (at least 20 seeds; state the panel and why that
many in the test's doc comment), compute per world: promoted founders; of those,
how many are roots; how many have a forebear that is **also promoted**; and the
distribution of promoted-ancestor chain depth. Report the median yield across
the panel, and print the full per-seed table so a reader can audit it — a single
aggregate is not enough.

- [ ] **Step 3: Positive control — the probe must be able to report a LOW number**

A probe that can only report a high number is not evidence. Construct a case
where the yield **must** be near zero (a panel where `MEMORY_DEPTH` is
effectively 1, or worlds with a single occupation per people) and confirm the
probe reports it. **A negative result from an instrument nobody has seen move is
not evidence.** Record what you did and its output.

- [ ] **Step 4: Run and record**

Run the probe with ignored tests enabled, capturing to a file, then read the
file. Record the full output in
`docs/superpowers/ledgers/2026-09-01-the-avowal.md`.

- [ ] **Step 5: Adjudicate the kill criterion**

Spec §5: **if the median yield — as defined in Step 2, denominator "all
promoted founders" — is under 10%, Task 5 does not ship.** Write the verdict and
the number into the ledger. If it fails, STOP and report; do not proceed to Task
5 and do not renegotiate the threshold. Seed 42's fixture measured 45.6%, so
there is wide margin; a panel median anywhere near 10% is itself a finding worth
reporting, because it would mean seed 42 is unrepresentative.

- [ ] **Step 6: Format, gate, commit**

`cargo fmt`, then `make gate-commit`, then commit the probe and the ledger
entry together. Use a commit-message file, not an inline heredoc — an
apostrophe in `-m "$(cat <<'EOF' ...)"` breaks the shell.

---

### Task 2: A tableau can state a relation

**Files:**
- Modify: `windows/vessel/src/tableau.rs`
- Modify: `windows/vessel/src/session.rs` (apply staged relations)
- Test: `windows/vessel/tests/suite/tableau.rs` (exists; registered at
  `windows/vessel/tests/suite.rs:100`)

**Interfaces:**
- Consumes: `Tableau { cast: Vec<StagedBody>, things: Vec<StagedThing> }`;
  `StagedThing { kind: String, held_by: usize }`, where `held_by` indexes `cast`.
- Produces: `Tableau.relations: Vec<StagedRelation>`, a `StagedRelation` naming
  a predicate and two cast indices. **Task 4 constructs these and relies on
  these exact field names.**

- [ ] **Step 1: Read `tableau.rs` in full before designing the type**

Especially the module doc's two standing rules the new layer must obey:
**unspecified means EMPTY** (a tableau with no relations states none, and never
inherits the world's), and **absent and empty are deliberately the same thing**
for every layer a tableau can state.

- [ ] **Step 2: Write the failing tests**

Four behaviours, each its own test:

1. a tableau with no `relations` stages none — and specifically does **not**
   inherit any relation the derived world would have had;
2. a stated relation between two cast members is readable from the session after
   staging;
3. a relation naming a cast index that does not exist is refused, with a message
   naming the index — mirror how `StagedThing.held_by` handles this; read it
   first, and if it does not validate, say so in your report rather than
   inventing a different convention;
4. `Tableau::from_json` round-trips `relations`.

- [ ] **Step 3: Run to verify they fail**

Scope to the vessel suite's tableau tests. Expected: FAIL — no `relations` field.

- [ ] **Step 4: Implement**

Add the field and its application path. Every new public item and field needs a
doc comment, and primitives at a `pub` boundary need a `type-audit:` tag —
follow `StagedThing`'s existing tag as the pattern.

- [ ] **Step 5: Run to verify they pass**

Expected: PASS, 4 tests.

- [ ] **Step 6: Format, regenerate the type-audit report, gate, commit**

Adding `pub` fields **always** drifts `docs/audits/type-audit-report.md`.
Regenerate it with the `report` subcommand redirected to that path — the
redirect is what writes the file, and running the command bare regenerates
nothing while leaving a diff that reads as "no drift". Commit it with the change.

---

### Task 3: The provision table, and the resolver reads it

**Files:**
- Create: `cli/src/provision.rs`
- Modify: `cli/src/tropes.rs` (`resolve`), `cli/src/lib.rs` (module)
- Test: `cli/tests/suite/provision.rs`; register in `cli/tests/suite.rs`

**Interfaces:**
- Consumes: `tropes::resolve(&Corpus, &ConceptRegistry) -> BTreeMap<String, Outcome>`; `Outcome::{Stageable, Blocked(Vec<String>), Inapplicable(String)}`; `ConceptRegistry::{predicates, phenomenon_kinds, concepts}`.
- Produces: a provision lookup answering, for one namespaced token, whether some
  home serves it. Tasks 5-7 add rows. **Task 4 consumes whatever resolver
  signature you leave behind.**

- [ ] **Step 1: Decide reuse-or-re-derive, and ledger it**

Read `kernel/src/manifest.rs` in full. Its `Correspondent<T, V>` —
`Present(payload) | Absent(reason)`, where an absence must name why — is the
discipline this table wants. Decide whether to reuse it or define a sibling and
**write the decision and its reason into the ledger** (spec §4.1). Either answer
is acceptable; an unrecorded one is not.

- [ ] **Step 2: Write the failing tests**

1. **Default-deny is preserved.** A token with no provision row resolves
   `Blocked`, exactly as today. The check enforces *declared ⊆ served* and must
   not invert.
2. **The ledger home still works.** Every token resolving today still resolves —
   run both corpora and assert the outcome map is unchanged against the
   committed artifacts.
3. **A declared-but-unserved token is refused.** A row whose resolver answers
   "no" leaves the token missing. Without this the table is just a second
   registry.
4. **An absence names a reason.** A token declared absent from every home carries
   a reason string, and a reasonless absence is a construction error.

- [ ] **Step 3: Run to verify they fail**

Expected: FAIL — `cli::provision` does not exist.

- [ ] **Step 4: Implement the ledger home only**

Component and session homes are Tasks 6 and 7. Make those two variants *exist
and be unreachable-by-construction* rather than defaulting to "served" — a home
answering yes by default would make Tasks 6 and 7 vacuous before they are
written.

- [ ] **Step 5: Run green, then confirm NO artifact moved**

Run the provision tests, then `make rebaseline`, then diff `docs/audits/`.
Expected: PASS, and **`docs/audits/trope-*.md` unchanged.** This task rewires the
resolver without changing any verdict, so a moved trope artifact means a
behaviour change you did not intend. If one moves, STOP and report which and why
— do not accept it.

- [ ] **Step 6: Format, type-audit report, gate, commit**

---

### Task 4: The witness, demonstrated red first

**Files:**
- Create: `cli/tests/suite/trope_witness.rs`; register in `cli/tests/suite.rs`
- Modify: `cli/src/tropes.rs` (`Stageable` requires a witness)

**Interfaces:**
- Consumes: Task 2's `Tableau.relations`; Task 3's provision lookup;
  `hornvale_vessel::Tableau::from_json` (`tableau.rs:102`); **`PossessOpts.tableau:
  Option<Tableau>`** (`windows/vessel/src/lib.rs:217`) — the struct is
  `PossessOpts`, NOT `SessionConfig`; an earlier draft of this plan had the line
  number right and the name wrong.
- Produces: a witness table keyed by situation id, in the shape of decision
  0330's `MERCHANT_WITNESS`. Read `cli/tests/suite/sentence_corpus.rs` for that
  precedent before designing this one.

- [ ] **Step 1: Capture the behavioural RED before touching `tropes.rs`**

Pick a situation, hand-author a tableau that should **fail** to witness it, run
the witness, and capture the failure output verbatim into the ledger. **A RED
from a compile error proves nothing about an assertion** — if the type does not
exist yet, get the behavioural red from the live surface first.

- [ ] **Step 2: Write the failing tests**

1. a situation with no witness cannot be `Stageable`, whatever the provision
   table says;
2. a witness whose tableau fails to stage the actants is refused;
3. a witness whose relations do not all resolve is refused;
4. the witness roster and the `Stageable` set are equal in membership — neither
   can silently outgrow the other.

- [ ] **Step 3: Run to verify they fail**

- [ ] **Step 4: Implement**

- [ ] **Step 5: Demonstrate the witness catches its own target**

Per 0330's own correction, being red once at the start is **not** sufficient.
Find a perturbation **from inside the code** — do not use one prescribed here —
under which an implementation could satisfy every assertion while not actually
exercising the machinery a witnessed situation is credited for. Apply it,
demonstrate the red, restore, and verify the restoration by checksum.
**Restoring a mutated file can leave a stale binary**; confirm the restored
build is the one you test.

- [ ] **Step 6: Run the full cli suite, regenerate, inspect**

The trope artifacts move here, **but as PROSE, not as a verdict.** Zero
situations are stageable, so gating `Stageable` on a witness changes no
situation's outcome. What must change is the report's own header, stating that
it now measures witnessed capability and that the number is **not comparable
across this boundary** (a G3 flagged item) — write that into the report itself,
not only the chronicle.

**If any situation's VERDICT changes, STOP and report it.** A verdict moving
here would mean the witness gate admitted or refused something the registry
lookup did not, which is a finding, not a success.

- [ ] **Step 7: Format, type-audit report, gate, commit**

---

### Task 5: Kinship as a genesis fact

**Gated on Task 1's kill criterion.** Do not start until its verdict is in the
ledger.

**Files:**
- Modify: whichever domain crate owns `kin-of` / `parent-of` — **you decide,
  with both files open.** `domains/person/src/lib.rs` owns the fact's SUBJECT
  (`is-person`, `person-born`); `domains/history/src/lib.rs` owns the descent
  arithmetic that computes the RELATION. Both already have `register_concepts`,
  and `register_predicate` takes only `&str`s, so either works mechanically and
  neither needs its sibling's types. The choice is semantic. Justify it in the
  campaign ledger.
- Modify: the promotion emit path — read `windows/worldgen/src/person_promote.rs`
  first; the emitter may live elsewhere
- Test: `windows/worldgen/tests/suite/kinship_facts.rs`

**Interfaces:**
- Consumes: `forebear_of`; `Kinship::{Sibling, Ancestor(u32)}`;
  `registry.register_predicate(name, functional, doc) -> Result<(), RegistryError>`.
- Produces: two registered predicates and their facts.

- [ ] **Step 1: Settle the mapping from inside the code**

Spec §4.3 states the property, not the mapping. Read `forebear_of`'s signature
to determine whether a founder can have more than one recorded forebear —
**that answer, not an assumption, decides whether `parent-of` is `functional`**.
A wrongly-`functional` predicate fails at commit time on the second object.
Record the mapping and its justification in the ledger.

- [ ] **Step 2: Write the failing tests**

1. a founder with a promoted forebear carries `parent-of` naming that entity;
2. a founder whose forebear is **not** promoted carries **no** `parent-of` — the
   ledger says what is remembered (spec §4.3);
3. a root founder carries none;
4. a `Sibling` edge does not render as descent;
5. **stream consumption order is unchanged** — emitting these facts must draw no
   `Stream`. Assert it the way the existing pin-isolation tests do; read
   `domains/terrain/tests/suite/tectonic_properties.rs` for the pattern.

(5) is a save-format contract. If emitting kinship consumes a draw, STOP.

- [ ] **Step 3: Run to verify they fail**

- [ ] **Step 4: Implement**

- [ ] **Step 5: Run green, then regenerate BOTH baselines**

`make rebaseline` does **not** write byte-goldens. Registering predicates and
adding facts moves the keystone golden `cli/tests/fixtures/world-seed-42.json`,
which only `make rebaseline-goldens` accepts. Run both, then inspect the
whole-tree diff and branch on what you see — **do not treat any count as
predicted**:

- **`world-seed-42.json` gains `parent-of` facts and census batteries redden** →
  expected; accept, and report the count you actually got.
- **The count is far from the ~93 the fixture probe suggested** → a finding, not
  an error. Report it with the number; Task 1's panel is the reference, and a
  large gap means the emit path differs from the probe's reading.
- **`book/src/gallery/` moves** → STOP. That is an epoch event, not this task.
- **Only `docs/audits/` moves** → regenerate and commit in the same commit.
- **Nothing moves at all** → STOP and report. Registering two predicates and
  emitting facts cannot leave the keystone golden untouched; a null here means
  the emit path never ran.

- [ ] **Step 6: Add a provision row and confirm the bundle completes**

`bundle:consanguineal-kin` should read 5/5 — the first bundle this project has
fully satisfied. Confirm `polti-1895` stageable is **still 0 of 36**: spec §5
preregisters that null, and a non-zero here is a finding to investigate, not a
success.

- [ ] **Step 7: Format, type-audit report, gate, commit**

---

### Task 6: Affect through the component home

**Files:**
- Modify: `cli/src/provision.rs` (component home)
- Modify: the crate that will own `affect-kind` / `affect-intensity` — **read
  first.** `windows/sentiment` is a window and cannot own registry vocabulary a
  domain must declare. Decide the owner and justify it against the layering rule
  in the ledger.
- Test: `cli/tests/suite/provision.rs` (extend)

**Interfaces:**
- Consumes: `hornvale_sentiment::{Judgment, Emotion, snap_judgment}`;
  `Judgment { warmth: f64, competence: f64, emotion: Emotion }`.
- Produces: component-home provision rows.

- [ ] **Step 1: Write the failing tests**

1. `affect-kind` and `affect-intensity` resolve through the **component** home;
2. **no fact is committed** — assert the ledger is byte-identical before and
   after, on a real world. This is spec §4.4's whole point;
3. `feels-toward` does **not** resolve — deliberately unregistered (Nathan's
   grain ruling, ledger #2);
4. therefore `bundle:felt-affect` reads **2/3 and stays blocked**.

(3) and (4) are the campaign's honesty guarantee. If a later change ever
completes `felt-affect` without a person-scale producer, these tests must be
what stops it.

- [ ] **Step 2: Run to verify they fail**

- [ ] **Step 3: Implement**

- [ ] **Step 4: Run to verify they pass**

- [ ] **Step 5: Confirm the null holds**

Regenerate and diff. Expected: `bundle:felt-affect` moves 0/3 → 2/3;
**`tvtropes-2012` stageable stays 0 of 409.**

- [ ] **Step 6: Format, type-audit report, gate, commit**

---

### Task 7: Acts — addressable without being stored

**The natural split point.** If this becomes its own campaign, stop after Task 6
and close. `campaign/the-wicket` is mid-flight in `windows/vessel` with ~4,700
insertions (`liveness.rs` +2,248) — **absorb main before starting this task** and
re-read the collision surface.

**Files:**
- Create: `windows/vessel/src/act.rs` (`ActHandle` and the derived act view)
- Modify: `cli/src/provision.rs` (session home)
- Test: `windows/vessel/tests/suite/act.rs`

**Interfaces:**
- Consumes: `RoleHandle`'s derived-identity pattern
  (`domains/history/src/flesh.rs`) and `barrier_of`'s derived-state pattern.
- Produces: `ActHandle`, and session-home provision rows for `witnessed`,
  `present-at`, `deed-of`, `act-precedes`, `act-occurred-on`.

- [ ] **Step 1: Read the two precedents before designing `ActHandle`**

`flesh.rs`'s `RoleHandle` (identity without materialization) and
`worldgen::barrier_of` (state without storage). `ancestor()`'s doc comment
records a hazard you must not repeat: *"a fixed permutation iterated has fixed
points, and `(RoleHandle(0), Seed(0))` was one"* — and `Seed(0)` is a reachable
world seed.

- [ ] **Step 2: Write the failing tests**

1. an act's handle is a pure function of its constituents — same inputs, same
   handle, across processes;
2. **distinct acts get distinct handles**, including the degenerate all-zero case
   that bit `ancestor()`;
3. **nothing is committed** — the ledger is byte-identical after a session that
   generates many acts (spec §4.5; decisions 0346, 0366);
4. a snapshot **does** persist them (0368: live-play facts persist only when
   asked for);
5. `witnessed` and `present-at` resolve through the session home.

- [ ] **Step 3: Run to verify they fail**

- [ ] **Step 4: Implement**

- [ ] **Step 5: Run to verify they pass**

- [ ] **Step 6: Confirm `bundle:witnessing` completes, and check the null**

Both tokens present → 2/2. Report the stageable count; if it is now non-zero,
that is a **finding to investigate against the witness** per spec §5 — a
situation is stageable only if Task 4's witness actually staged it.

- [ ] **Step 7: Format, type-audit report, gate, commit**

---

### Task 8: Definition of Done

- [ ] **Step 1: Decisions 0576-0580**

From the reserved block, in `docs/decisions/`. One record each: the provision
table (0576), the witness (0577), kinship as a genesis fact (0578), affect as
component data (0579), acts addressable but unstored (0580). Titles must match
filenames — `docs_consistency` enforces it.

- [ ] **Step 2: Chronicle and Confidence Gradient**

Write `book/src/chronicle/the-avowal.md`. Re-score `book/src/open-questions.md`
(decision 0030): spec §2.1 is a direct instance of its "an unpaired check scores
as unchecked" clause, and this campaign pairs the generator with a verifier for
the trope corpus.

- [ ] **Step 3: Retrospective**

`docs/retrospectives/the-avowal.md`, plus its row in
`docs/retrospectives/README.md` — **hand-unioned, no writer, not in
`generated-paths.txt`**. Keep both sides' rows on any conflict.

Worth recording: this campaign found **four** built-and-unwired mechanisms —
`descent.rs::ancestor()` ("reserved and currently unconsumed"), `snap_judgment`
(commits nothing), `kernel/src/manifest.rs` ("nothing constructs a `Manifest`
yet"), and the tone tier (`tonality` is 0.0 on all 23 authored rows). That is a
pattern, not four coincidences, and it is what this campaign is really about.
[Corrected by the campaign's own close: `kernel/src/manifest.rs` was never
unwired — `register_manifest` is the live, sole concept-registration path,
called by nine domains every genesis. See the chronicle's "What stays
unwired" section and the retrospective's opening section.]

- [ ] **Step 4: Idea-registry rows**

`PSY-affect-grain-is-what-it-points-at` landed at brainstorm. Add rows for
anything Tasks 6-7 deferred. **600-char budget per Idea cell** —
`registry_idea_cells_are_within_budget` enforces it and caught a 1015-char row
during this campaign's own brainstorm.

- [ ] **Step 5: Full regeneration and the drift check**

Run `make rebaseline`, then `make rebaseline-goldens`, then the drift check that
reads its path list from `docs/generated-paths.txt` (never from prose).

- [ ] **Step 6: Stage gate, then merge**

Push the branch. Ask `scripts/sluice-mouth.sh` **before** `make sluice` — it
refuses in milliseconds and saves a queue slot. Then
`make sluice-stage BRANCH=campaign/the-avowal REF=<full-sha>`, and the merge
after. A merge needs a `Sluice-Headline:` trailer in the same block as
`Claude-Session:`, **no blank line between them**, or it is refused.
