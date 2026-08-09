# Corpus two and the matrix — design

**Status:** Approved · **Date:** 2026-08-06 · **Repo:** `hornvale/hornvale`
**Relates to:**
[0095](../../decisions/0095-a-corpus-is-an-instrument-never-a-standard.md),
[0011](../../decisions/0011-studies-are-data-metrics-are-code.md),
[0016](../../decisions/0016-measurement-is-preregistered.md)

Take the second reading, and make the disagreement between the two corpora a
generated artifact rather than something a reader has to assemble by eye.

## Why

Decision 0095 committed to three things: provenance emitted rather than
documented, a third verdict for what the world deliberately lacks, and **the
output being a matrix over corpora rather than a score**. It closed with:

> Adding a corpus means adding a column, and the first genuine matrix claim
> waits for corpus number two. Until then the report says *reach against this
> catalogue*, never *coverage*.

Corpus number two has existed since 2026-08-03 —
`ndouglas/tvtropes`'s `corpus/tvtropes-2012.trope.json`, 409 character tropes
frozen before first measurement. It has never been ingested here. The matrix
claim 0095 deferred has been available and untaken.

The reading has now been taken, read-only, with no change to this repository:

```
hornvale tropes --corpus <path>/tvtropes-2012.trope.json report
```

**Stageable 0 of 409, 62 inapplicable**, against Polti's 0 of 36 with 1
inapplicable. Both columns read zero, which is the expected baseline the
existing report already explains. **The finding is not in the score. It is in
the demand**, and one column could not have shown it.

## What the second column shows

Bundle demand as a share of each catalogue, the seven that diverge most in
each direction:

| Bundle | `tvtropes-2012` | `polti-1895` | Δ |
|---|---|---|---|
| `agent-knowledge` | 53% | 19% | **+34** |
| `identity-and-recognition` | 29% | 6% | **+23** |
| `norm-and-transgression` | 38% | 22% | +16 |
| `reputation-and-dishonour` | 30% | 14% | +16 |
| `speech-act` | 27% | 14% | +13 |
| `interpersonal-violence` | 32% | 25% | +7 |
| `impaired-reason` | 10% | 3% | +7 |
| … | | | |
| `relinquishment` | 5% | 11% | −6 |
| `witnessing` | 10% | 17% | −7 |
| `self-judgement` | 5% | 17% | −12 |
| `felt-affect` | 30% | 44% | −14 |
| `erotic-desire` | 13% | 28% | −15 |
| `consanguineal-kin` | 12% | 33% | −21 |
| `intent` | 25% | 50% | **−25** |

**Polti's 1895 catalogue is about will, blood and love.** What a person wants,
who they are related to, whom they desire, how they judge themselves.

**The 2012 fan taxonomy is about knowledge, identity and standing.** What a
character knows, whether they are recognised, which norms they break, how they
are regarded.

Neither is more complete. They are different instruments, which is the whole
claim 0095 makes and the reason a single column was never enough to support it.

Two consequences worth the backlog's attention, and both are new information:

- **The columns agree on what to build first, unanimously.**
  `bundle:individual-persons` is required by **every situation in both
  catalogues** — 36 of 36 and 409 of 409, and 100% of the blocked situations in
  each. Two instruments built a century apart, for different media, by
  different methods, agree without exception on the first thing the world
  lacks.
- **They fork immediately after.** Polti's #2 is `intent` (50% against 25%);
  TVTropes' is `agent-knowledge` (19% against 53%). The single-column report
  could not surface that choice, and it is a choice, not a ranking artefact.

**Inapplicable runs 15.2% here against Polti's 2.8%** — a fan taxonomy of
modern screen media is substantially about cameras, casting and reception, and
the corpus's own provenance predicted this. Under 0095 that gap is a finding
rather than a deficiency.

## What this changes

### The corpus lives here

`tropes/tvtropes-2012.trope.json`, copied verbatim from the frozen artifact.
Hornvale must be self-contained: a gate cannot depend on a sibling checkout.

Its situation count is asserted at **409**, matching the assertion
`cli/src/tropes.rs:599` already makes for Polti. Per `tropes/`'s own guidance —
a corpus is frozen before measurement and its count asserted, so changing it is
a deliberate act — the copy is data, and this repository must notice if it
moves.

### `check` keys off the corpus, not a hardcoded path

`cmd_tropes`'s `check` arm currently reads `docs/audits/trope-coverage.md`
unconditionally, so `--corpus X check` compares X's render against Polti's
artifact and can only ever fail. The artifact path becomes
`docs/audits/trope-coverage-<corpus>.md`, derived from the corpus's own
`corpus` field — `polti-1895`, `tvtropes-2012`.

Deriving from the data rather than a second flag means a caller cannot pair the
wrong corpus with the wrong artifact, which is the failure the current arm
would produce silently if the paths were merely made configurable.

### Polti's artifact is renamed

`docs/audits/trope-coverage.md` → `docs/audits/trope-coverage-polti-1895.md`.

Enumerated blast radius: `scripts/regenerate-artifacts.sh:394` (one line becomes
three), `cli/src/main.rs`, `cli/tests/trope_coverage.rs`, `docs/README.md:64`.

Keeping the unsuffixed name for Polti and suffixing only newcomers would avoid
this, and is rejected: within a month the asymmetry reads as an accident rather
than a decision, and the next corpus inherits the confusion.

### The matrix is generated

New mode `hornvale tropes matrix`, rendering both corpora into
`docs/audits/trope-matrix.md`, drift-checked exactly as the columns are.

**Not authored.** `docs/README.md:64` describes `audits/` as "Generated,
drift-checked reports … Never hand-edited"; a hand-written matrix would be the
only untrusted document in that directory, and its figures would drift from the
columns it summarises the first time either moved.

Contents: the per-bundle share table above, sorted by divergence; each
catalogue's stageable and inapplicable counts; the bundles both rank first; and
the point at which they fork. It states, as the columns do, that it measures
reach against these catalogues and is not a verdict on the world.

## Validation

**Per-corpus byte identity.** `cli/tests/trope_coverage.rs`'s whole-file check
becomes one per corpus, so a moved predicate is caught in whichever column it
moved.

**The matrix cannot drift from its inputs.** One test asserting the matrix's
per-column figures equal the per-corpus reports' own. Both derive from the same
`resolve()` output, so this is cheap and it closes the one gap a generated
summary can still have.

**The existing divergent-corpus guard survives.** `cli/tests/trope_coverage.rs`
already proves `check` fails against a corpus that cannot match its artifact;
under path derivation that test must keep failing for the same reason, not pass
because it now looks for an artifact that does not exist.

**`hornvale tropes report` with no `--corpus` still defaults to Polti** and
still reproduces its artifact byte-for-byte, so the rename is the only change
visible to an existing caller.

## Out of scope

- **Re-emitting or re-freezing either corpus.** Both are frozen before
  measurement; 0095 as extended by 0016 makes changing one after unblinding a
  new column rather than a correction. `ndouglas/tvtropes` remains the source of
  record for how `tvtropes-2012` was built and what it excludes.
- **Any change to `resolve` or the bundle vocabulary.** The 20 unsatisfiable
  requirements the TVTropes corpus declares are deliberate and documented in
  its provenance; they are not a defect to fix here.
- **Acting on the fork.** Whether to build toward `intent` or `agent-knowledge`
  is a backlog decision this document exists to inform, not to make.
- **A third corpus.** The machinery this adds is per-corpus rather than
  two-corpus, but nothing here proposes one.
