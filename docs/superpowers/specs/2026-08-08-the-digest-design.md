# The Digest — codifying the project's knowledge of itself

**Status:** spec, awaiting G3 · **Date:** 2026-08-08 · **Branch:** `the-digest`
· **Base:** `64e8c667`

Registry rows this campaign advances: UNI-29 (the self-describing program),
UNI-21 (the self-reflective ledger), UNI-28 (the View discipline), PROC-11
(the unmix doctrine), PROC-10 (`make doctor`, shipped).

---

## 1. The problem, measured

Hornvale's governing prose is **2,072,357 words** across 112 decisions, 208
retrospectives, 226 specs, 211 plans, 218 chronicle entries, and 13
`CLAUDE.md` files (counted at `64e8c667`, 2026-08-08). No session — human or
agent — reads that. What actually *governs* a given decision is `CLAUDE.md`,
plus memory, plus whatever `grep` happened to surface. Everything else is
latent: retrievable, authoritative-looking, and surfacing by luck.

Three symptoms follow, and they are one symptom:

- **Relitigating.** Not because a decision is unrecorded, but because 112
  decisions are grep-able and 2M words are not.
- **Superseded patterns beating their superseders.** Both are equally
  retrievable, and the older one has had longer to accumulate inbound
  references.
- **Two ID schemes in one registry** — 1,402 numeric (`PREFIX-123`) against
  583 distinct slug-style (`PREFIX-word-word`). What a convention change looks
  like when the corpus is not rewritten: both remain locally correct.

### 1.1 Two live instances, from this campaign's own brainstorm

**`docs/timings/test-baseline-MacBookPro.tsv` was 1,055 commits stale.** Last
written 2026-07-30 at `1f862cde`; `windows/book/src/lib.rs` had changed
+1,082/−455 lines since. It is committed, specific to the millisecond, and
carries an internal commit stamp that nothing compares to `HEAD`. Reasoning
from it produced a 3.8× cost misattribution (`hornvale-book` measured 9.4% of
suite cost against the 36% the baseline implied) and a wrong campaign
direction, caught only by re-measuring.

**`make doctor` — the repo self-map — is drifted right now.** `scripts/doctor.sh:19`
hard-codes `external deps allowlist: serde, serde_json`. The enforced value in
`cli/tests/architecture.rs:11` is `ALLOWED_EXTERNAL = ["libm", "serde",
"serde_json"]`; `libm` was admitted by decision 0041 and the self-map never
learned. `doctor.sh` is 20 hand-written `echo` lines against 10 derived
commands — roughly two-thirds authored prose asserting facts that live in
source.

Neither is a lapse in discipline. Both are the predicted output of a system
whose process skills all say *capture* and none says *retire*.

## 2. The missing position

Along the axis *how much of a system's self-description is derived*:

    0%    hand-authored prose, unchecked
   10%    authored + drift-checked examples
   25%    authored governing text + generated indexes    <== HORNVALE IS HERE
   50%    CODIFICATION                                   <== THIS CAMPAIGN
   75%    generated governing text (PROC-11 fully applied)
   90%    the system queries its capability schema to plan (UNI-21)
  100%    the system writes/explores/deletes its own frontier (UNI-29)

UNI-29 sits at 100%; Hornvale sits at 25%; **no intermediate position was ever
named.** Every time the idea was picked up, the only move on offer was the
whole north star, which is unbuildable — which is why it was repeatedly parked
as `raw`. This campaign names and builds the 50% position.

**Codification** is the law analogy: a *code* (current, consolidated, small,
authoritative for what the law is) plus *reports* (archival, vast, never
authoritative). It is not generation-of-everything, which PROC-11 explicitly
warns against.

## 3. Non-goals

- **Not migrating the corpus.** The 2.07M words are declared archival and left
  in place. See §7.
- **Not generating `CLAUDE.md` or the idea registry.** Follow-on rows.
- **Not restricted engineering English / NLG.** v1 *assembles* authored text
  objects; it does not generate prose from triples.
- **Not UNI-29.** This is the 50% position, not the 100% one.
- **Not a dashboard, service, or time-series store** — foreclosed by 0088.

## 4. Design

### 4.1 The collection

A **non-`World` fact ledger** in `hornvale-kernel`'s shape:

    Fact { subject, predicate, object, place, day, provenance }
    + a ConceptRegistry of project predicates

`provenance` records whether a fact was **asserted** (authored) or **scanned**
(derived), and from which file.

### 4.2 The ledger carries no time

`place` and `day` are **always `None`**. The project ledger stores no
timestamp at all; project time is git's.

This dissolves UNI-21's constitutional exclusion rather than arguing past it.
UNI-21 scoped itself out of the development process on no-wall-clock grounds;
a ledger with no time field cannot violate that rule in letter or in spirit.
Compaction plus `git log` supplies every temporal question, which is 0088's
ruling already.

### 4.3 Compaction is the functional-predicate rule

`PredicateDef { name, functional, doc }` already exists in
`kernel/src/registry.rs`. A functional predicate admits at most one object per
subject — so asserting a new one **replaces** its predecessor. That is the LSM
compaction, with no new mechanism.

**The one difference, stated explicitly:** the world ledger *rejects* a
contradiction; the project ledger *replaces* on one. Same shape, same
registry, different commit policy. Non-functional predicates give multi-valued
facts for free, which `supersedes` requires.

A superseded fact leaves the committed artifact entirely and survives only in
git. This is strictly stronger than answering "what is in force" with a query
over an append-only log: a stale fact that remains in the file **will be
found** by a grepping reader. Absence beats deprioritisation.

### 4.4 What is stored vs what is scanned

PROC-11's existing rule governs: **store only geological-rate facts;
derive-on-read anything faster.**

| stored (asserted) | scanned (derived on read, never stored) |
|---|---|
| decisions, ratification, rationale | crates, layers, dependency graph |
| supersession | predicates, stream labels, metrics |
| intent, status, campaign goals | tests, gates, committed artifacts |
| archival pointers | the enforced `ALLOWED_EXTERNAL` |

This retro-explains §1.1: `test-baseline-<host>.tsv` is a fast-drifting fact
that was **stored**, which is exactly why it went stale invisibly. PROC-11
predicted that failure class before it occurred.

### 4.5 Serialization — the archaeology constraint

0088's corollary binds: *"a baseline whose rows churn on noise is not
archaeology."* If compaction rewrites the artifact noisily, `git log -p` stops
being readable and the entire justification for compaction dies.

Therefore: **JSONL, one fact per line, stable-ordered by `(subject,
predicate)`**, so a single assertion is a one-line diff. Deterministic
serialization is an existing repo strength; this is a hard requirement, not a
nicety.

### 4.6 Placement — `tools/digest/`, outside the workspace

Precedent: `tools/type-audit/` (decisions 0027/0028). A repo tool is not a sim
component and must not become a workspace dependency. It is neither a domain
(models no slice of the world) nor a window (presents no domain).

**Verified by probe, not asserted** (2026-08-08): a crate outside the
workspace with an empty `[workspace]` table can path-depend on
`hornvale-kernel` and construct a `Fact`. `cargo run` green.

The empty `[workspace]` table is **mandatory**, and `type-audit`'s manifest
comment says why: without it, a checkout nested under `.claude/worktrees/`
makes cargo walk past the package's own workspace root and bind to the outer
one. Add `tools/digest` to the root manifest's `exclude` list alongside
`tools/type-audit`.

Note: `EntityId` wraps `NonZeroU64`, so project entity ids start at 1.

### 4.7 Generation and drift-check

Generated views regenerate through `scripts/regenerate-artifacts.sh` — the
existing single source of truth, called by both `make rebaseline` and CI, so
they cannot silently diverge — and are drift-checked with `git diff
--exit-code`, exactly like every other committed artifact.

### 4.8 The MCP surface

An MCP tool wraps read (query facts) and write (assert / retract), giving
ergonomic authoring and vocabulary enforcement at the API boundary.

**MCP is ergonomics, not substrate.** The store remains plain committed JSONL,
readable and editable with an editor and `git diff`. If the only way to read
project state is a running server, the result is more fragile than the prose
it replaces.

The tool is **in scope for v1**, not a nice-to-have: if asserting a decision is
onerous, decisions stop being asserted, and the outcome is a beautiful empty
ledger beside a resurgent pile of prose.

### 4.9 Intent and reality are held apart

Generated documentation *cannot disagree with the code*, and disagreement is
diagnostic: a spec claiming "we intended X" while the code does Y is evidence
of drift nobody noticed. Collapsing intent into reality hides drift rather
than fixing it.

The collection therefore stores **intent** as asserted facts and **reality** as
scanned facts, under distinct predicates, and a view reports the delta. This is
what distinguishes codification from generation, and it is the reason the
`libm` drift in §1.1 becomes a *reported finding* rather than a silent wrong
line.

## 5. v1 scope

Two generated artifacts:

1. **`make doctor`'s self-map** — replaces a hand-maintained artifact with a
   generated one. PROC-10 already ships it, so blast radius is minimal and the
   before/after is directly comparable.
2. **The in-force decision index** — where relitigating actually happens.
   Superseded decisions fall out of the generated index without anyone editing
   `docs/decisions/`.

Together these exercise the whole pipeline: assert → compact → serialize →
scan → generate → drift-check → MCP write path.

## 6. Preregistered success criteria

Frozen before implementation (decision 0016). A falsified prediction is a
finding, not a failure.

- **S1.** The generated `doctor` output states `libm, serde, serde_json`,
  derived from `ALLOWED_EXTERNAL`, with no hard-coded allowlist anywhere in
  the generator.
- **S2.** Mutating one fact in the ledger makes the drift-check exit non-zero.
  *The check must be demonstrated RED on command* — a green drift-check that
  cannot fail is this repo's documented recurring failure mode, and this
  campaign will not ship without the mutation proof.
- **S3.** Asserting a fact whose functional predicate already has an object
  produces a **one-line** diff in the committed JSONL. Measured, not assumed.
- **S4.** Superseding a decision removes it from the generated in-force index
  and from the committed ledger, and `git log -p` still recovers it.
- **S5.** The delta view reports at least one real intent≠reality gap on
  existing repo state. (The `libm` case is the known instance; if the view
  reports *only* that, S5 passes but weakly — note it.)
- **S6.** `scripts/doctor.sh`'s 20 hand-written `echo` lines drop to zero
  asserted-fact lines; any remaining prose is an assembled text object stored
  in the ledger.

**Falsification clause.** If the generated `doctor` output cannot reproduce
the hand-written one's *usefulness* — if a fresh session orients worse from
the generated map — the campaign reports that as its headline and stops at v1
rather than proceeding to `CLAUDE.md`.

## 7. Migration: compact, do not migrate

The 2.07M words are **not** converted. v1 asserts what is true **now** — on
the order of a few thousand facts for 112 decisions plus the doctor surface —
declares the existing corpus archival, and lets git and the existing files
hold the history.

This is compaction applied once, at the start, and it is what makes the
campaign finite: days of authoring rather than a 975-document migration.

## 8. Risks

| risk | mitigation |
|---|---|
| Authoring friction kills adoption | MCP tool in v1 scope (§4.8) |
| Noisy diffs destroy the archaeology that justifies compaction | Stable ordering, one fact per line, S3 measures it |
| The generated map is worse than the hand-written one | §6 falsification clause; stop at v1 |
| A second parallel artifact that also rots | v1 *replaces* `doctor`; it does not sit beside it |
| Layer/architecture surprise | Probed and green (§4.6) before the spec was written |

## 9. Decisions promoted from the ledger

Full ledger: `.superpowers/sdd/decision-ledger.md` (scratch; dies with the
worktree).

1. The collection is the kernel's `Fact` shape in a non-`World` ledger
   (Nathan, G1).
2. Compaction, not append-only-with-queries (Nathan; generalizes 0088).
3. The ledger carries no time; project time is git's (G2 — dissolves UNI-21's
   constitutional exclusion).
4. Compaction is the existing functional-predicate rule; world rejects,
   project replaces (G2).
5. `tools/digest/` outside the workspace, path-depending on kernel (G2;
   precedent 0027/0028; probe-verified).
6. Stored vs scanned follows PROC-11's drift-rate rule (Q).
7. v1 ships `doctor` + the in-force decision index (Nathan, G1).

## 10. Open — for G3

- **The intent≠reality constraint (§4.9)** is a design position, not a settled
  decision. It is the ideonomy pass's dark-polarity finding and deserves
  explicit ratification, because it is what stops this campaign from becoming
  the thing it exists to prevent.
*(Both items above were ratified by Nathan at G3, 2026-08-08. The third,
below, was open at G3 and is now decided — ledger #10.)*

## 11. Predicate renames: isolation, not versioning

**Decided (ledger #10).** Project predicates carry **no epoch suffix**, but a
rename **must be its own commit, touching nothing else**.

The reasoning matters more than the rule, because it corrects the answer this
spec originally proposed. Stream labels carry `/v2` epochs because a rename
**corrupts every world** (decision 0006). A project predicate rename corrupts
nothing — there is no save. But it rewrites every fact carrying that
predicate, producing a whole-file diff that destroys `git log -p` for that
compaction. That is 0088's churn corollary a third time, and archaeology is
the entire justification for compaction (§4.3, §4.5).

So the hazard is real but it is a **diff-noise** hazard, not a corruption
hazard, and the mitigation is isolation-in-history rather than
versioning-in-name. Epoch suffixes are additionally wrong here: they would
leave dead `predicate/v1` rows in a store whose defining property is that
superseded things *leave*.
