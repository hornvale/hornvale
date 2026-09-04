# The Housemark — retrospective

*Process lessons. The chronicle carries the product story; the design and
decision records carry the technical contract.*

## The acceptance population was more valuable than the feature fixture

H3 asked for structural recovery over every living dwelling in five full
worlds. Its first denominator was 1,275 living `(vertex, rung)` occupation
records, and only 996 survived the existing production lookup with the same
people. The failure was not weak cultural differentiation. It was a pre-existing
address reversal: a settlement room on the cube-sphere mesh was sent through
`containing_vertex` as though that operation inverted its original icosphere
vertex. It returned a direct neighbour for 531 records, producing 222 missing
and 41 wrong occupants; 16 records intentionally shared a room.

The useful discipline was refusing to filter those 279 rows out. A 996/996
headline would have been true of the surviving subset and false of the player
surface. Restating the data flow as `settlement -> exact room -> brief` exposed
the unnecessary reverse leg. The fix keyed both names and living occupations
by the exact production room and used one ordered first-settlement reduction
for both. H3 then accounted for 1,259 distinct rooms and recovered all 1,259
inhabited housemarks.

General lesson: when two spatial representations are documented as
non-invertible, do not reconstruct an identity by nearest geometry. Carry the
exact address across the seam, and make a population acceptance test account
for its rejected rows as carefully as its accepted ones.

## Artifact classification must follow the diff

The design initially expected a chamber-only reading with no epoch. Adding
`BENCH` disproved that expectation: although composition is unserialized and
uses no stream, the registered kind enters every saved world's serialized
concept registry. The measured seed-42 world grew from 5,587,823 to 5,587,963
bytes. Nathan approved concept accession epoch 20, analogous to the prior
`door` accession, and the additive world and language-root artifacts were
re-pinned.

This sharpened the boundary usefully. “No chamber fact and no stream” is a
derivation guarantee; it is not a promise that a newly named concept leaves a
world file byte-identical. The final review found the old promise still stated
in the normative pattern documentation and design header, and the fix split
composition, rendered output and concept accession into three separate cases.

The artifact sweep also needed a review correction. Its first supposedly
complete inventory omitted `world-build-sites.tsv` and the type-audit and
plumb reports. All three were real campaign movement with named producers: one
guard golden for H3's five-world build site and two generated audit projections.
The durable ledger now classifies them explicitly. The lesson is literal:
derive “complete” from the changed-path set, not from the list of artifacts one
remembers regenerating.

## Reviews repeatedly turned plausible checks into discriminating ones

- Task 2 replaced six copied housemark fixtures with the generated 2 × 3
  product, and replaced pattern-name assertions with checks of actual kind,
  attachment and prerequisite.
- Task 3 found that `go` mutated time, trail, ledger and position before a
  fallible destination brief was validated. The destination brief is now
  derived before the first write, and a production regression proves an empty
  occupation cannot acquire the manikin's housemark.
- Task 4 corrected the stage tracker, made the signature read the actual
  composed anchor kind, and corrected an artifact account that had called a
  prose-only change spatial.
- Task 5 added the three omitted artifact paths and corrected the provenance of
  sixteen line-only plumb shifts from a nonexistent `BTreeMap` import to the
  actual `OccupationRecord` import and adjacent reflow.
- Final review found H3's recovery counter was tautological and its extractor
  consulted the label it claimed to recover. The shipped proof composes all six
  candidates without that label, requires exactly one match, then compares the
  recovered class with the production brief. Making the screen universal
  produces the predicted two-candidate ambiguity.

Across those rounds, the recurring improvement was from enumerating examples
to generating the space, and from checking metadata about an output to reading
the output itself.

## Three attempts means three attempts at one issue

Task 4 invoked the commit gate three times and met three different generated
audit obligations: a missing type tag, a stale type report and a stale plumb
report. Work stopped under the repository's three-attempt rule. The controller
ruled that the breaker is per issue, not per command name: each failure named a
new generated file, and each prior issue passed on its next run. Regenerating
the named plumb report and continuing was therefore legitimate; repeated
experimentation against one unchanged failure would not have been.

That distinction should stay explicit. A gate is a sequence of obligations,
and reaching a later obligation is evidence of progress. Count attempts
against the unresolved cause, while preserving every failure and remedy so
the distinction can be audited rather than asserted.

## A shell in the right worktree does not bind every editor

At the start of Task 4, an initial relative patch reached the shared root
checkout rather than the campaign worktree. Only the two intended test paths
moved, both were immediately reversed, and those root paths were verified
clean before the task continued. The agent's shell had already printed the
right worktree and branch; that evidence did not prove the patch mechanism was
resolving relative paths from the same root.

The operational lesson is narrower than “check the branch”: use absolute
worktree paths for edits when the editor's resolution rule is not the shell's,
then inspect `git status` in both the intended worktree and the shared checkout
after the first write. A correct change in the wrong checkout is still a
failed change, however quickly it is noticed.

## The lexicon caught the same naming mistake three times

Three tasks independently called the six-way cross-product a `cell`: once in
the H1 map, eight times in H2, and once in H3 assertion prose. The repository's
lexicon reserves that word for a spatial vertex sense and rejected every
return. Each repair was small (`class` or `combination`), but the recurrence is
the lesson: consult the enforced vocabulary before naming a repeated concept,
then carry the accepted term into the next task brief. A guard can catch a
mistake reliably while the process keeps manufacturing it.

## Scratch routing and deferred outcomes

The close sweep read the complete task briefs and reports, progress ledger,
review reports, and every capped/full review package. The packages contain
copies of reviewed diffs rather than independent rulings. No ruling was found
only in scratch; the H3 correction and the per-issue attempt interpretation
were recorded contemporaneously in the campaign ledger.

Every review minor has an outcome:

| Finding | Outcome and durable home |
|---|---|
| Production unoccupied brief lacked a manikin regression | Fixed in `windows/vessel/src/brief.rs`; review outcome in the campaign ledger's Task 3 section |
| H3 stored selected pattern kind rather than composed anchor kind | Fixed in `windows/vessel/tests/suite/housemark_readout.rs`; Task 4 ledger section |
| Fixture report described prose drift as spatial drift | Corrected in the campaign ledger's artifact classification |
| Decision 0750 described decision 0084 as epoch granularity | Corrected in `docs/decisions/0750-bench-is-concept-accession-epoch-twenty.md` |
| Plumb provenance named the wrong import | Corrected in the campaign ledger's artifact classification |
| H3 recovery was label-dependent and tautological | Fixed in the readout; final-review outcome recorded in the campaign ledger |

The speculative limits also have durable homes. Inherited or hybrid
architecture now has the exact measured gap in `CLIENT-housemark-provenance`,
with `CLIENT-ruin-signature` and `CUL-6` as its cause-of-end and household
neighbours. Changing culture through time remains the work of `CUL-14` and
`CUL-18`. `SOC-staple-ladder` keeps R2, R3 and the dynamics arc open. A bench's
absent sitting action is a scope boundary, routed to the existing object/action
work in `MAP-19` and `MAP-27`, not a promise hidden in the chronicle.

The Confidence Gradient is **N/A**. The campaign proved a product-level
distinguishability claim and strengthened its mutation control, but it neither
resolved nor moved a standing bet in `book/src/open-questions.md`.

## Close state

At the time these artifacts were written, no census, merge or push had
occurred. The non-census artifact regeneration and goldens had already been
classified during implementation. `SOC-staple-ladder` remains
`elaborated (R1 shipped)` — only the Housemark reading rung advanced. The
temporary `IMPLEMENTATION_PLAN.md` was completed and deleted; the durable plan
and design are marked complete.
