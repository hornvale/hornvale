# 0135. A coverage verdict cites a machine-checked anchor

**Status:** Accepted (2026-08-15) · **Decider:** Nathan · **Relates to:**
[0026](0026-slugs-not-numbers.md),
[0031](0031-the-frontier-is-published-in-the-book.md),
[0095](0095-a-corpus-is-an-instrument-never-a-standard.md),
[0131](0131-refuted-is-a-seventh-registry-status.md),
[0134](0134-a-capability-corpus-is-a-sibling-to-a-trope-corpus.md)

In the context of a coverage matrix that scores this project against an
external catalogue, facing the fact that a hand-maintained control matrix does
not go stale so much as begin to **lie**, we decided that **every verdict
except `absent` must cite an anchor the resolver re-checks on every run, and
that an unmet item must distinguish `refused` from `deferred` from `absent`**,
accepting that the idea registry thereby becomes a **gated interface**.

**An unmet capability is not one fact but three.** It may be deliberately
refused by a ratified decision, planned but unbuilt, or genuinely nobody's yet.
An instrument that cannot tell them apart publishes a deficiency list that is
mostly false. Decision 0095 already ratifies the same argument one step
earlier — without `inapplicable(reason)`, "every difference between Hornvale
and a nineteenth-century French dramaturgical taxonomy reads as a deficiency."
Applied to a program-capability catalogue, the reasons an item is unmet are
themselves load-bearing, so the vocabulary is five:

```
  present       cites a MECHANISM anchor  (test name, or a path)
  refused       cites a DECISION anchor   (in force, not superseded)
  deferred      cites a REGISTRY anchor   (a row that exists, not `shipped`)
  absent        cites NOTHING             (the honest red)
  inapplicable  cites a REASON            (0095's third verdict)
```

**The anchor is the evidence, so the resolver checks it.** A decision anchor is
membership in the generated in-force index — superseded decisions are absent
from it by construction, so "is this refusal still in force?" needs no new
machinery. A registry anchor is a parsed row identifier, using the same parse
and the same status normalization the registry's own drift check uses, never a
second rule. A mechanism anchor is resolved **without building**: a path check,
or a source scan for a test definition that is not `#[ignore]`d. Resolution
must not shell out to `cargo` — the ratchet would then build inside a build.

Four conditions turn the committed artifact red: **DANGLING** (the anchor
stopped resolving), **STALE-DEFERRED** (the cited row now reads `shipped`, so
the verdict has become a lie), **UNJUSTIFIED** (a non-`absent` verdict with no
anchor — a parse error, not a warning), and **NOVELTY** (the `absent` count
rose). None is invented: STALE-DEFERRED is the mutation guard's
stale-declaration verdict, and NOVELTY is the ratchet shape the trope audit,
the type audit and the duration baseline already use. A gate that failed on the
mere existence of an unmet item would go red on day one and train everyone to
ignore it.

**The accepted cost: the idea registry becomes a gated interface.** Ratified by
Nathan at this campaign's G3 review, 2026-08-15.

The registry's IDs and statuses were **already** machine-consumed before this
record, and stating otherwise would overstate the change. Three committed
tests read them: `the_waiver_list_only_shrinks` (`docs_consistency.rs:489`),
which fails by design when a waived row is renamed; `no_new_numbered_registry_ids`
(`:536`), enforcing decision 0026 against a frozen list; and
`every_refuted_row_cites_its_evidence` (`:432`), which reads a *normalized
status* and arrives with decision 0131.

**What is new is narrower and stronger: a row's status now reddens a committed
artifact outside the registry's own drift check, authored by an instrument its
editors have no reason to know exists.** Every prior consumer lives in
`docs_consistency.rs` — the same file, the same run, guarding the registry on
the registry's own behalf, so a row's editor meets it immediately and in
context. A `deferred` verdict in `systems/` is a claim made *about* a row by a
document in another directory, on a schedule the row's editor does not
control: flipping a row to `shipped` now reddens `docs/audits/`, and renaming
or retiring one breaks a check whose failure text is the only thing that will
explain why. That is the intended mechanism rather than a side effect — it is
precisely what stops the matrix quietly becoming a lie — but it is a real
constraint on every existing and future row, and it lands on sessions that did
not ask for it. Decision 0026's permanent-ID rule gains a consumer outside the
file that has always enforced it.

**So diagnosability is part of the decision, not a nicety.** A DANGLING or
STALE-DEFERRED failure must name, in the failure text itself: the anchor that
stopped resolving, the corpus item that cited it, what kind of change would
have caused it (a supersession, a rename, a status flip), and the two
legitimate repairs — re-verdict the item, or restore the anchor. A red whose
message is only "the artifact moved" is affordable for the trope ratchet
because a trope corpus has no external editors. This one has many: anybody
editing the registry is editing this instrument's inputs without knowing it.

**Consequence.** `present` remains the verdict the instrument is least entitled
to — a path that exists is not a working feature, and a resolvable test name is
not proof a capability is met — and the report prints that above its own tally
rather than in a footnote. `refused` and `deferred` are strongly checked;
`absent` is unfalsifiable by construction and is the only verdict a reader must
take on the author's word, which is why `NOVELTY` ratchets its count and
nothing else. An item whose real position is a refusal recorded only in a
registry row, not a ratified decision, must read `absent`: the anchor kinds are
not interchangeable, and widening `refused` to accept a registry anchor would
let an unratified opinion masquerade as a project position.

**See also.** `cli/src/systems.rs` (the resolver); `cli/tests/system_coverage.rs`
(the ratchet and the anchor audit); `docs/audits/system-coverage-wolverson-2021.md`;
[The Compendium chronicle](../../book/src/chronicle/the-compendium.md);
[The Compendium
spec §5](../superpowers/specs/2026-08-15-the-compendium-design.md).
