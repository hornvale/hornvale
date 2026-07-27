# The Accession — a word added is a word appended

**Campaign:** The Accession
**Date:** 2026-07-27
**Status:** SHIPPED (2026-07-27) — all four stages implemented (`d1ef9e2d`).
Seed 42 byte-identical (whole world), zero artifact drift, `make gate` green
(2218 passed). Chronicle: `book/src/chronicle/the-accession.md`;
retrospective: `docs/retrospectives/the-accession.md`.
**Successor:** The Actants (`2026-07-27-the-actants-design.md`) stacks behind
this and cannot land byte-identically without it.
**Decisions in force:** 0033 (quantize at emit only), 0073 (epoch granularity
is declared), 0063 (census regen is local-canonical), 0009 (models author,
dice roll — and the one place it does not apply; §2).
**Registry:** LANG-55 (the codomain subspace, §6 — registered by this campaign); LANG-27 annotated (the epoch sort now outranks frequency-weighted length).

## 1. The defect

`assign_proto_roots` (`domains/language/src/etymology.rs:284`) states its own
intended property in a comment at line 288:

> the id tiebreak keeps the order stable — a concept added to the universe
> later slots in without reshuffling the words already assigned ahead of it

and `assign_proto_roots_is_insertion_stable_for_earlier_sorting_concepts`
(line 1042) asserts it, with the goal spelled out:

> the registry can grow without reshuffling committed vocabulary

**The property is real but only holds for the last position, and the sort key
does not put new concepts there.** The universe is ordered `(core_rank, id)`
and walked, each concept drawing a proto-root and probing to a fresh draw when
its form is taken. A concept's assignment therefore depends only on concepts
sorted at or before it — so an addition is free *iff* it sorts last. Ordering
by id lands it wherever the alphabet says, usually mid-order, where it can
take a form some later concept would have drawn and force that concept to
probe. Everything derived from the displaced word then moves.

The test passes because it only ever exercises the position where the promise
holds ("`zzz-late` … lands strictly last"). It reads like a guarantee of
append-only growth and proves a guarantee of append-*at-the-end*-only growth.

**Measured** (seed 42, registering the 12 unnamed species kinds one at a
time — see The Actants §1 for why those twelve):

```
treant       PERTURBS   5 facts     all 12 at once: 70 facts, all `name`
otyugh       PERTURBS  65 facts     11 artifacts, +361 / -241
other 10     identical              5 + 65 = 70 — effects are independent
```

Ten of twelve were free. Additivity today is a coin flip, and nothing in the
gate reports which side a change landed on.

## 2. Why the constitutional fix is unavailable

The house answer to "stop recomputing something fragile" is decision 0009 —
author it offline, commit it, drift-check it. That is **closed here by
construction**: `assign_proto_roots` takes `seed`, so the assignment is a
function of `(seed, family)` and there is no finite table to commit. The
ordering must be computed, which is why the defect exists at all. Worth
stating in the spec so the option is visibly closed rather than overlooked.

## 3. The fix

Order by `(epoch, core_rank, id)`, where `epoch` is a concept's registration
generation: `0` for everything registered today, `1+` for anything added
later. A new concept then always lands strictly last — the case the existing
test already proves is free — so it cannot displace any prior assignment.
Additivity becomes structural rather than probabilistic.

**Verified by spike** (all 16 species kinds registered, epoch-last ordering,
then reverted):

```
                    naive             epoch-last
ledger              70 facts moved    BYTE-IDENTICAL (LEDGER EQUAL: True)
artifacts           11 files          4 files
lines               +361 / -241       +127 / -7  (added rows only)
almanacs, settlement, scene-tiles, connections, the-book: untouched
census              regen required    not required
```

### 3.1 Where the epoch lives

An authored side table owned by `language`, keyed by concept name, defaulting
to `0` — **not** a `ConceptDef` field, so the serialized registry does not
change and no save-format epoch is owed.

### 3.2 The fix must ship with its own reverse audit

An authored table has exactly one failure mode: a forgotten row. A concept
registered without an epoch entry silently defaults to `0`, sorts into the
epoch-0 block mid-alphabet, and reintroduces the churn this campaign removes —
**the same class of silent drift as the defect it cures.** So the table ships
with a parity check asserting every registered concept has an entry, in the
same shape as the orphan audits The Actants builds. Without this the fix rots
the way the `*-kind` roster rotted.

### 3.3 The accepted cost (owner call, 2026-07-27)

`core_rank` exists so core words win the short forms — `length ∝ rarity`,
Zipf. Putting `epoch` above `core_rank` means a future **core** (Swadesh) word
forfeits short-form priority to arrival order. Nathan took epoch-last with
this understood. Zipf fidelity and additivity are in genuine tension, and only
for a newly-arriving *important* word; every concept currently queued is
periphery, so nothing today pays this.

### 3.4 Churn becomes deliberate, never accidental

The epoch counter only accumulates, so over many campaigns later coinages get
progressively longer forms. The remedy is not to renormalize continuously —
that is the churn being removed — but to allow a **declared** re-coinage at a
deliberate epoch bump, the discipline `streams.rs` already applies to seed
labels. The goal is not zero churn; it is that no churn is ever a side effect.

## 4. Success criteria

1. Seed 42 is **byte-identical** to `origin/main` with the ordering change
   alone (no concepts added): ledger equal, zero artifact drift.
2. Registering a concept at epoch 1 leaves the ledger byte-identical — the
   property asserted directly, with a test that adds a mid-alphabet concept
   (`otyugh-kind`, the 65-fact offender, is the natural regression case).
3. The existing insertion-stability test is **strengthened, not replaced**: it
   currently proves only the last position; it gains a case proving a
   mid-sorting epoch-1 concept is also free.
4. Every registered concept has an epoch entry (§3.2), asserted.
5. `make gate` green; type-audit clean.

## 5. Not in scope

- Registering any concept. This campaign changes ordering only; The Actants
  does the naming behind it.
- The codomain subspace (§6).
- Re-coinage / epoch consolidation (§3.4 names the discipline; nothing
  implements it, and nothing needs it yet).

## 6. The successor, wanted sooner rather than later

Every option in §3 orders the **domain** — who is assigned when. The
alternative found by cross-domain re-instantiation (Dewey decimal
classification: `573.2` slots between `573` and `574` and no book is ever
renumbered) subdivides the **codomain** — reserve a phonotactic shape that
epoch-0 roots cannot occupy and draw later coinages from it. Then a new
concept cannot collide with an old one at all, additivity holds by
construction of the form space rather than of the order, and **core keeps its
short forms** — dissolving §3.3's tension instead of trading it off. Its cost
is that newer words are audibly marked as newer, which is loanword and
neologism phonology, i.e. not a cost.

It is a short step from machinery that already exists: `PROBE_BUDGET`
(`etymology.rs:376`) already lengthens a candidate by a syllable when the
space saturates, so the code already knows how to move into a different region
of form-space.

Nathan's call (2026-07-27): take epoch-last now, move to the codomain subspace
sooner rather than later, but not in this campaign. Registered as an
idea-registry row and a followup.
