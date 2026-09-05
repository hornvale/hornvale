# The Coda — reconcile the campaign record

**Status:** complete · **Date:** 2026-09-04 · **Campaign:** The Coda ·
**Ledger:** `docs/superpowers/ledgers/2026-09-04-the-coda.md`

## 1. Purpose

Hornvale has durable records for the life of a campaign: specifications,
plans, committed decision ledgers, chronicles, retrospectives, decisions, and
the idea registry. Their individual quality is generally high, but their
cross-record closure is intentionally only partly mechanical. The current
documentation test suite proves important local facts—29 checks passed on
2026-09-04, including the spec-and-plan-to-ledger relation—but its exact-name
matcher deliberately leaves 54 plans outside that population, and 244
pre-ledger campaigns are exempted from the ledger requirement.

This campaign performs a repository-wide reconciliation. Its job is to find
the exceptional campaigns and unresolved residues that lack a durable home,
then repair their disposition without treating historic evidence as clutter.

## 2. Boundaries

The audit population is every Markdown record presently under
`docs/superpowers/specs/`, `docs/superpowers/plans/`,
`docs/retrospectives/`, and `book/src/chronicle/`: respectively 341, 320,
325, and 336 files at the baseline above. Decisions and ledgers are evidence;
the idea registry is the destination only for enduring speculative product
directions.

The campaign does not delete, relocate, or rewrite a completed campaign's
historical record merely because it is complete. It does not promote ordinary
bugs, mechanical cleanup, or process lessons into the idea registry. It does
not claim that filename coincidence establishes campaign identity.

## 3. Design

### 3.1 A reconciliation ledger names the relationship explicitly

Add `docs/audits/campaign-reconciliation.tsv`, one row for each audit unit.
An audit unit is a campaign whose related records have been established by
evidence, not merely a matching filename. The table carries the canonical
campaign key; each relevant spec, plan, ledger, chronicle, and retrospective
path; a disposition; the evidence that establishes it; and, where relevant,
the one durable home of its unresolved residue.

The ledger's permitted dispositions are:

- `active` — live work with a current campaign home;
- `shipped` — merged work whose historical record remains in place;
- `partial` — a bounded shipped portion and an explicitly routed remainder;
- `superseded` — replaced by a cited later record or decision;
- `abandoned` — deliberately not pursued, with a cited reason;
- `unresolved` — a confirmed exception awaiting its routed repair.

The row is a shelf-mark, not a second chronicle. It names evidence and links;
it never retells a campaign.

### 3.2 The audit is evidence-led and alias-aware

The first pass derives the mechanically exact spec/plan/ledger population and
the frozen unmatched-plan and pre-ledger-exemption populations already named
by `docs_consistency.rs`. The second pass resolves aliases and umbrella-plan
relationships manually from headers, chronicles, retrospectives, ledgers,
and merge history. A loose stem similarity is a lead, never proof.

For each confirmed completed campaign, evidence ranks as follows: merged code
and tests; an in-force decision or merged chronicle; a committed retrospective
or ledger; then status prose and plan checkboxes. Conflicting evidence keeps a
row `unresolved` until the conflict is explained; no status header is changed
to make the table look complete.

### 3.3 Residue has exactly one home

Each non-shipped residue is classified before any edit:

- A future-facing, durable product direction becomes an existing or new
  `raw` idea-registry row, unless an already-open row covers it.
- An actionable, bounded continuation becomes a named successor campaign or a
  committed ledger follow-up while it is not yet a campaign.
- A changed governing rule becomes an append-only superseding decision.
- A process lesson or observation remains in the originating retrospective
  (or a corrected retrospective), rather than being misfiled as a product
  idea.

No residue receives two primary homes. A registry row is not evidence that
the associated campaign completed; it records only the future direction.

### 3.4 Repairs preserve provenance

Repairs are minimal and local: status headers gain a disposition and direct
links where the evidence supports them; existing registry rows are repointed
or status-flipped; missing `raw` rows are added only after deduplication;
retrospective deferred tables gain an explicit destination when one was
missing. Decisions are superseded, never substantively edited.

Historical specs and plans remain discoverable at their existing paths. The
audit ledger is the index over their relationship, not an archive directory.

### 3.5 The reconciliation ledger is checked, not trusted

Add a focused `docs_consistency` check that parses the TSV and verifies:

- canonical keys are unique and all cited local paths exist;
- every row uses the closed disposition vocabulary;
- a `partial` or `unresolved` row has one non-empty residue destination;
- a `shipped`, `superseded`, or `abandoned` row has no residue destination;
- registry destinations resolve to real rows and are not already `shipped`;
- every currently known unmatched-plan and ledger-exemption item is either
  represented by a reconciliation row or listed as explicitly out of scope
  with its reason.

This check guards the audit's own claims. It does not pretend to establish
whether prose is honest or a campaign's evidence is complete; those remain the
human review performed by the campaign.

## 4. Success criteria

1. Every record in the four audit directories is either attached to one audit
   unit or is explicitly classified as shared infrastructure/non-campaign
   documentation.
2. Every currently unmatched plan and ledger exemption receives an evidence
   disposition; no current known blind spot remains anonymous.
3. Every confirmed uncompleted product direction has exactly one registry or
   successor-campaign home; process lessons and ordinary fixes do not inflate
   the registry.
4. No historical campaign artifact is deleted for being complete.
5. The existing documentation suite and the new reconciliation check pass.

## 5. Delivery stages

1. Define the TSV schema and write its red-first parser tests.
2. Build the inventory and reconcile exact matches, aliases, exemptions, and
   unmatched plans into the TSV.
3. Triage every residue and make the minimal source/registry/decision repairs.
4. Add the permanent audit check, regenerate any documentation artifacts it
   requires, and run the documentation suite.
5. Write the chronicle and retrospective, then submit the documentation-only
   campaign through the normal close path.

## 6. Decision rules at execution

| Observation | Action |
| --- | --- |
| A record has no defensible campaign relation | Leave it ungrouped, record why, and do not guess an alias. |
| Closure evidence conflicts | Mark `unresolved`; cite both sides; repair source prose only after resolving the conflict. |
| A deferred item is a future product direction | Deduplicate against the registry, then add or repoint one `raw` row. |
| A deferred item is a process observation or ordinary defect | Keep it in the retrospective or its owning successor, not the registry. |
| A historic record is stale or incomplete | Add a visible correction or superseding record; do not erase its original claim. |

## 7. Verification performed while drafting

`cargo test -p hornvale --test suite docs_consistency::` passed all 29
documentation-consistency tests (0 failed, 280 filtered) on the campaign's
baseline. The source of the two known ledger-audit blind spots was read in
`cli/tests/suite/docs_consistency.rs`: the exact matcher documents its 54
unmatched plans, and the exemption ceiling documents 244 pre-ledger records.
