# The Confidant, Task 1: running the insertion-stability claim

Campaign: The Confidant (Arc III of The Bridle). This note ships no code —
it records an experiment run against `domains/language/src/accession.rs`,
`etymology.rs`, and `lexicon.rs` to check the spec §2.2 claim the campaign's
scope rests on: **appending a new accession cohort displaces nothing, so
registering felt-state concepts costs no world-generation epoch.** That
claim reversed the campaign author's own earlier prediction and, before
this task, had never been run — only read.

## Verdict: insertion-stability HOLDS, on real, decisive evidence — but not
## from the black-box experiment this task's brief specified

The brief's literal Steps 1-4 (build seed 42, append a throwaway cohort,
rebuild, diff; then move the same probe to cohort 0 as a positive control)
turned out to be **structurally incapable of testing the claim**, for a
reason worth recording carefully because it will bite the next campaign
that reaches for the same instrument. The claim IS true, but it is
established below by the codebase's own existing test suite, run for the
first time as part of this task, not by the JSON-diff experiment as
literally specified.

## Step 1: baseline

```
$ cargo run -q -p hornvale -- new --seed 42 --out /tmp/conf-base.json
world of seed 42 written to /tmp/conf-base.json (12534 facts; village: Doaba)
$ shasum -a 256 /tmp/conf-base.json
554792ab079b365f200980de48fd0ad44d84aa004d34cd01ed02ec9b702a550e  /tmp/conf-base.json
```

## Step 2: append a throwaway cohort (unregistered nonsense concept)

Appended one temporary cohort to the end of `EPOCH_COHORTS`:

```rust
&["zzzz-throwaway-nonsense-concept"],
```

Rebuilt seed 42:

```
world of seed 42 written to /tmp/conf-append.json (12534 facts; village: Doaba)
sha256: 554792ab079b365f200980de48fd0ad44d84aa004d34cd01ed02ec9b702a550e
```

**Byte-identical to baseline** — same fact count, same sha256, `diff`
empty.

## Step 3: decision rule

Per the brief's rule, an unchanged output with no unrelated fact moved would
mean insertion-stability HOLDS. But per the brief's own Step 4, a null result
is uninterpretable without a working positive control — so read the verdict
below only after Step 4.

## Step 4: the positive control — and where it broke

Reverted, then moved the SAME unregistered concept from a new trailing
cohort to cohort 0 (duplicated into `EPOCH_COHORTS[0]`'s array):

```
world of seed 42 written to /tmp/conf-control.json (12534 facts; village: Doaba)
sha256: 554792ab079b365f200980de48fd0ad44d84aa004d34cd01ed02ec9b702a550e
```

**Also byte-identical.** Per the brief's own decision rule this means the
instrument is blind and Step 2's null proves nothing on its own — and
reading the code explains exactly why, deterministically, for ANY seed:

`concept_epoch` is only ever consulted for names in the `concepts: &[&str]`
argument `assign_proto_roots_with_epoch` sorts, and that argument is always
`hornvale_language::lexicon::proto_root_universe(exposures)` — built from
`exposures`, whose key set is *exactly* `world.registry.concepts()`
(`windows/worldgen`'s own doc comment on `exposure_of_impl`, confirmed by
`cli/tests/suite/accession.rs`'s two-directional parity test between
`EPOCH_COHORTS` and the registry `register_all` builds). `register_all`
constructs the registry from each domain's own static concept list —
**entirely independent of `EPOCH_COHORTS`'s contents.** So a name added to
`EPOCH_COHORTS` that is not independently registered elsewhere is never
looked up by `concept_epoch` at all, regardless of which cohort holds it.
Appending or moving such a name is a no-op **by construction**, for every
seed, not just seed 42 — the JSON-diff experiment as specified could not
have detected displacement even if the accession mechanism were completely
broken.

**Supplementary attempt (not in the brief, added to chase a working
control):** duplicated two *already-registered* concepts — `"wait"` (a real
epoch-11 action-suite concept) and then `"hill"` (a real epoch-4 TOPONYMIC_CORE
landform, the kind of concept that feeds glossed settlement naming) — into
cohort 0, one at a time, each fully reverted before the next. Both builds
were **also byte-identical to baseline** (12534 facts, same sha256, empty
diff) at seed 42. This is not further evidence of instrument blindness in
the same deterministic sense as the unregistered-concept case: the existing
test suite (below) documents that early-cohort insertion is *probabilistic*
even in a phonology deliberately built to collide (`cramped_phonology()`'s
own test comments record specific seeds where a mid-alphabet, epoch-0
insertion displaces **zero** assignments — "the vacuous case" — and record
having had to search seeds to find ones that reliably displace). A real,
roomy phonology at a single real seed is a weak, unreliable instrument for
this question even when the mechanism is working correctly.

**Conclusion on the JSON-diff method:** do not use `cargo run new` +
whole-file diff to probe accession-epoch effects. `World { seed, registry,
ledger }` is what's saved (`CLAUDE.md`: "everything else is re-derived
deterministically") and, empirically, no combination tried here moved a
single byte of it — glossed names appear to be far less collision-prone at
a single seed than the property-test suite's adversarial fixtures, and an
unregistered probe concept can never reach the mechanism regardless of
seed. Neither failure mode is what Step 2's original null looked like from
the outside; both defeat the instrument silently.

## The real evidence: the codebase's own tests, run for the first time as part of this task

`domains/language/src/etymology.rs`'s `assign_proto_roots_with_epoch` is
`pub(crate)` specifically so the insertion-stability property can be
exercised over a synthetic universe (its own doc: "the real table is a
`const`, and a test cannot append a cohort to it"). These tests already
existed and had never been confirmed passing as part of this task's
diligence:

```
$ cargo test -p hornvale-language --lib -- \
    assign_proto_roots_is_insertion_stable_for_earlier_sorting_concepts \
    a_later_epoch_concept_is_insertion_stable_from_any_alphabetical_position \
    the_same_mid_sorting_insertion_at_epoch_zero_still_displaces
test etymology::tests::the_same_mid_sorting_insertion_at_epoch_zero_still_displaces ... ok
test etymology::tests::a_later_epoch_concept_is_insertion_stable_from_any_alphabetical_position ... ok
test etymology::tests::assign_proto_roots_is_insertion_stable_for_earlier_sorting_concepts ... ok
test result: ok. 3 passed; 0 failed; 0 ignored; 0 measured; 230 filtered out
```

The third of these is its own working positive control (`cramped_phonology()`,
a seed searched specifically to guarantee collision): moving a concept to
epoch 0 DOES displace existing assignments there, proving that fixture's
comparison is not blind.

More decisively, `domains/language/tests/suite/accession_properties.rs`'s
`appending_the_elf_cohort_displaces_no_existing_proto_root` runs the exact
claim at real scale: the **real, full `EPOCH_COHORTS` table** (176
concepts, no synthetic substitute), a real drawn phonology for the
`"goblinoid"` family, across 8 real seeds (`Seed(1)..=Seed(8)`):

```
$ cargo test -p hornvale-language --test suite -- \
    appending_the_elf_cohort_displaces_no_existing_proto_root
test accession_properties::appending_the_elf_cohort_displaces_no_existing_proto_root ... ok
test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 23 filtered out
```

This test asserts two things in one run, over all 8 seeds:

1. **Additivity**: appending the (already-shipped) six-concept elf cohort
   changes not one existing concept's proto-root, compared to a control
   universe without it.
2. **Anti-vacuity** (its own positive control): the SAME six concepts,
   *folded into cohort 0* instead of appended, DOES move at least one
   existing proto-root on at least one of the 8 seeds. If this assertion
   ever failed, the additivity assertion above would be proven vacuous —
   and it did not fail.

This is the real positive control the brief's Step 4 was reaching for, at
the scale that matters (the real table, a real family, multiple seeds), and
it passed with its own anti-vacuity check intact.

## Verdict

**Insertion-stability HOLDS**, established by
`appending_the_elf_cohort_displaces_no_existing_proto_root` (real
`EPOCH_COHORTS`, real phonology draws, 8 seeds, additivity confirmed,
anti-vacuity confirmed) plus the three unit-level insertion-stability tests
in `etymology.rs`. The mechanism is sound: sorting `concepts` epoch-first
before drawing means a later-epoch concept's assignment can only ever
depend on concepts already placed, and the forward single-pass `used`-set
loop never revisits an earlier concept once placed — so appending a cohort
at the end is additive by construction, exactly as the module doc claims.

**The brief's own black-box JSON-diff instrument (Steps 1-4 as literally
specified) is separately confirmed BLIND** for an unregistered probe
concept, deterministically and for structural reasons independent of seed
choice — this is a genuine, reportable finding about the *test method*,
not about the claim. Any future task that wants an end-to-end, real-seed
demonstration of an accession effect should drive
`hornvale_language::etymology::assign_proto_roots`/`assign_proto_roots_with_epoch`
directly (as the existing property tests do) rather than diffing
`cargo run new`'s saved JSON, which does not reliably surface
language-domain effects at a single seed even for a real, registered
concept.

## Step 4 supplement — positive control, quoted result

> Same unregistered nonsense concept, moved from a trailing cohort to
> cohort 0: 12534 facts, sha256
> `554792ab079b365f200980de48fd0ad44d84aa004d34cd01ed02ec9b702a550e` —
> **identical to both the baseline and the Step 2 append.** The instrument
> did not report displacement. Diagnosed as a structural false negative
> (see above), not evidence against insertion-stability, which is
> independently confirmed by the existing test suite run above.

## Step 5: independent universe re-derivations

`hornvale_language::lexicon::proto_root_universe`'s own doc records one
live historical instance (`windows/lab`'s `monophyly-goblinoid` metric,
fixed): it once rebuilt the universe from every registered concept with no
exclusion filter at all, which was invisible while the excluded cohort
(the nine spectral classes, epoch 6) sorted last, and broke — a false
monophyly-break report on 14/1000 seeds — the moment epoch 7 (the compass)
sorted after it.

Grepped every call site of `assign_proto_roots`/`assign_proto_roots_with_epoch`
and every reference to `proto_root_universe` across the workspace. Every
site that constructs a `universe`/`concepts` argument from a real world
(rather than a synthetic list local to a unit test) is listed below.

| Site | Calls `proto_root_universe`? | Notes |
|---|---|---|
| `domains/language/src/lexicon.rs::build_lexicon` | **Yes** | The canonical site — this IS the source of truth `proto_root_universe` exists to serve. |
| `windows/lab/src/metrics.rs::family_proto_assignment` (feeds `monophyly-goblinoid`/`-dwarf`/`-elf` and the clean-outgroup checks) | **Yes** | The historically-buggy site; now fixed, doc comment records the fix and the 14/1000-seed false positive it caused. |
| `windows/worldgen/tests/suite/radiation_language.rs::TestWorld::family_proto_assignment` | **Yes** | Correct. |
| `cli/src/proto.rs::render_proto` (the `hornvale proto` reference page) | **No** | Builds its own `universe` by filtering `world.registry.concepts()` through a locally-defined `is_unnameable` (reads `Correspondent::Absent(Void::Unnamed(_))` straight off the registry) and `hornvale_language::is_extradiegetic` (the shared `extradiegetic_pack` membership check). This is a **second implementation of `proto_root_universe`'s exclusion rule**, not a call to it. |
| `windows/worldgen/tests/suite/proto_goblinoid_golden.rs::render_root_table_snapshot` | **No** | Same pattern, deliberately: its own doc says it "mirrors `cli/src/proto.rs`'s `is_unnameable` (duplicated rather than imported: layering)". Also a second implementation, not a call. |
| `domains/language/src/phonology.rs` (`sonorant_open_proto_roots_actually_contain_a_liquid` and similar) | N/A | Builds a fully synthetic `probe-concept-N` list, not a re-derivation of any real world's universe. Out of scope. |

**Finding:** two sites — `cli/src/proto.rs::render_proto` and
`windows/worldgen/tests/suite/proto_goblinoid_golden.rs::render_root_table_snapshot`
— independently re-derive the universe *rule* rather than calling
`proto_root_universe`, exactly the pattern `proto_root_universe`'s own doc
warns against ("An independent re-derivation may legitimately redo the
*draw*; it must not redo the *universe rule*"). They currently compute the
same set `proto_root_universe` would, but only because of an *asserted*,
not type-enforced, invariant: that `Unnameable`/`Extradiegetic` exclusion is
species-invariant, so reading the registry's `Void::Unnamed` marker and
`extradiegetic_pack` membership directly gives the same answer as classifying
through `exposure_from` for some arbitrary daughter species and filtering
through `proto_root_universe`. Unlike the `monophyly-goblinoid` bug (a flat
omission of any filter), these two are not currently wrong — but they are
structurally the same risk category: nothing keeps them in sync with
`proto_root_universe` if a future campaign (this one, plausibly, if a
felt-state concept needs a new `GapReason` variant) adds a new exclusion
category. A new variant would update `proto_root_universe` and silently
leave these two rendering a stale universe. Recorded here as a pre-existing
condition or a follow-up item for whichever task in this campaign next
introduces a `GapReason` variant — not a Task 1 blocker, since neither site
is wrong today.

## Method notes

- All experimental edits to `domains/language/src/accession.rs` were made
  from a `cp`-backed copy (`/tmp/confidant-task1/accession.rs.orig`) and
  restored with `cp` (never `git checkout --`), verified byte-identical
  after each revert and via `git status --porcelain` (empty) before this
  note was written.
- No code changes ship from this task. This file, and the `docs/findings/`
  directory it introduces, are the only additions.
