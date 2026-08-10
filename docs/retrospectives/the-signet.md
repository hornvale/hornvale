# The Signet — retrospective

**Completed:** 2026-08-10 (slug-named per decision 0026; spec
`docs/superpowers/specs/2026-08-09-the-signet-design.md`, plan
`docs/superpowers/plans/2026-08-09-the-signet.md`, seven tasks). Ran under
campaign-autopilot. Process lessons only; the product is in
`book/src/chronicle/the-signet.md`.

## The plan's own acceptance assertion was vacuous, and only a mandatory mutation proof found it

The campaign's headline test asserts that inserting an extra entity-minting
stage moves no unrelated id. The plan froze the assertion in the form that reads
most naturally — `plain_ids.difference(&extra_ids).is_empty()` — and **that form
survives the exact mutation it exists to catch.** Restore the counter and the
inserted entity takes id `n`, every later id shifts up by one, the unperturbed
arm's set becomes a strict subset of the perturbed arm's, and nothing reads as
missing. The implementer ran it under the mutation and watched it pass green;
the reviewer, not told which form had failed, built a probe implementing the
plan's sketch and independently watched it pass green (`plain=406 extra=407`).

The shipped test compares the minted ids **elementwise, in derivation order**,
and then goes red at both minting seams under the same mutation. The set
difference is retained beneath it, labelled as the weak form, because it still
catches an id that vanished rather than moved.

**The generalisable rule: a set-difference formulation of "nothing moved" cannot
see a monotone renumbering — compare elementwise, in derivation order.** Other
"nothing moved" guards in this repo may carry the same weak shape; the phrase to
grep for is `.difference(` in a test whose claim is stability.

The wider process point is that this was caught by the *mandatory* mutation
proof, not by care at authoring time. The defect was in the plan text, written
by the same author who reasoned about what the test would catch — the failure
mode this project has recorded before, and the counter-measure is procedural
rather than intellectual.

## The counter was hiding a live defect, and finding it required new API

Under a counter every minted id is new by construction, so re-deriving entities
on an already-populated ledger cannot be detected. Opening a session on a world
that had been played and saved re-ran the NPC derivation against a ledger that
already carried those NPCs and minted a **duplicate per settlement on every
reload**. Lineage-derived ids turned that into an immediate mint-time collision
panic, which is how it surfaced at all.

**Named as a spec deviation:** the fix required `Ledger::reuse_or_mint_entity`,
an idempotent-derivation form the spec and plan did not specify. It is
documented as explicitly *not* a softer `mint_entity`, and a unit test proves
that reuse leaves the collision visible to the strict form. The reviewer was
told it was new API rather than left to notice.

The transferable observation: **a scheme that cannot express a collision cannot
report a duplicate.** Replacing an always-fresh identifier with a derived one
converts a silent accumulation into a loud failure, and the first campaign to do
so pays for every prior instance at once.

## A guard whose watch-list is authored by hand is only as complete as that authoring

P3 was falsified: a third prose file moved. The file itself is a small finding.
The shape is the real one — `cli/tests/no_entity_id_values_in_prose.rs` scans a
`PROSE_PATHS` constant listing **four source files**, and the channel that moved
(`kernel/examples/first_light.rs:161`, which prints two ids inside a sentence)
is a fifth that no list contains. The Salt built the list from a sweep; the
sweep missed a file; the guard inherited the gap and has been green ever since.

The prediction is what made this visible, and it was written in the form that
made it useful: *anything else that moves is a channel that was missed.* A
prediction with an explicit "and here is what a surprise would mean" clause pays
for itself even when — especially when — it fails.

**Candidate fix, recorded as a registry row:** invert the scan. Default-deny
over every file that can emit prose, with an explicit allow-list of files
permitted to print an id, rather than an allow-list of files that must not.

## An inherited handoff figure travelled into a spec unverified

Spec §6's P2 predicted the keystone test would be "unchanged at 3 material
groups / 29 founding-key groups", quoting The Salt's handoff, which states those
figures **for seed 42**. `id_shift_invariance.rs` has not built seed 42 since
The Tolerance — its witness has moved four times and is seed 5 today. So the
frozen expectation named the wrong world, and no arithmetic on the real one
could have matched it.

Measured at the close, on this tree: witness seed 5 carries **3 colliding
material-core groups (6 records) and 33 colliding founding-key groups (66
records)**; seed 42 carries 1 and 19. The property P2 exists to check is
verified, and verified the strong way — the same probe was re-run with both mint
paths reverted to the counter and returned **identical counts**, so the
derivation demonstrably reads nothing the grouping reads. But the spec's
predicted numbers were never checkable.

**A handoff figure is a claim with a world attached.** Re-measure it, or quote
the world it was measured on, before freezing it as a prediction.

## Two tasks could not be separate commits, and the reason is worth knowing in advance

Task 2 (the ledger takes a `Lineage`) and Task 3 (every call site supplies one)
are one compile unit: threading a required parameter breaks ~190 call sites
until all of them are converted. The pre-commit hook runs `make quick`
**workspace-wide**, so Task 2 could not land alone without `--no-verify`, which
is not on the table. Nathan ruled: land 2+3 as one commit, review them
separately. That worked, and no verification was lost — but the plan should have
seen it, because the constraint is structural rather than incidental. **A task
boundary that falls in the middle of a signature change is not a commit
boundary.**

## The brief named a dead function as the live insertion point

Task 6's brief specified inserting the extra mint "before the settlement stage",
meaning `hornvale_settlement::genesis`. That function is **retired** — worldgen's
own comments say so at two sites, nothing in the live pipeline calls it, and it
remains `pub` with a lineage-minting loop inside it. `build_to` has no injection
seam either, and adding one would be the "burn extra mints behind an env var"
shape a previous campaign considered and rejected.

The implementer inserted at the two seams a test can actually reach — the
history emission (root lineages) and the NPC derivation (child lineages) —
covering both lineage shapes, and said so rather than papering over it. The plan
had read the pipeline's *comments* rather than its *call graph*.

## The compiler was the enumeration, and the brief's file list was not

Task 3's brief listed the files with mint sites. It omitted six, including one
with 79 sites (`windows/vessel/src/liveness.rs`) and one with 10
(`windows/lab/src/synthetic.rs`), plus `windows/historiography`,
`windows/worldgen/src/chorus.rs` and three lab test files. The implementer
worked `cargo check --workspace --all-targets` to zero errors instead of trusting
the list. For a required-parameter change the compiler is a complete
enumeration, and a hand-written file list in the plan is at best a hint.

## A throwaway ordinal keyed on a constant is a collision waiting for a test run

~175 test sites needed a lineage. Literal ordinals were written first and were
**wrong**: one test mints through a helper and then mints again on the same
world, so two sites both derived ordinal 0 and collided. Keying the throwaway
ordinal on the ledger's own accession count (`test_lineage(ledger.entity_count()
as u16)`) makes a collision impossible by construction on any ledger in any call
order. The alternative was discovering each clash one four-minute test run at a
time.

## Three briefs contained code that did not compile against the real file

- The `stream_labels!` arm shape in Task 1's brief matches none of the macro's
  three accepted forms; the file's flat-form neighbours were followed instead.
- The `type-audit:` tags were placed on each field's own doc comment; the tool
  requires them on the struct's doc comment, naming each primitive field.
  `type-audit check` is not part of `cargo test -p <crate>`, so this would have
  surfaced only at a later gate.
- Task 4's snippet for the colocated-NPC lookup resolved the handle straight
  against the full roster, which would have silently reopened a sight-withholding
  invariant three earlier fix rounds had closed. The implementer added the
  `here`-membership filter and flagged the deviation instead of transcribing.

The instruction that made all three benign was "follow the file, not the brief,
and say so" — worth keeping verbatim in dispatches that include code blocks.

## A lint heuristic fired on a variable name

`make gate` went red on `cli/tests/claim_shape.rs` because `seed_shaped()` treats
the single-letter binding `s` as seed-shaped, and three new closures used
`.map(|s| …)`. None of the tests iterates seeds. The honest fix was to rename the
bindings rather than attach a `claim:` tag that would have misdescribed the
tests. Noted because the failure names a decision number and reads, at first
glance, like a real preregistration violation.

## Book freshness sweep (decision 0030)

**No Confidence Gradient bet moved.** `book/src/open-questions.md` was grepped
for identity / entity / mint-order / accession; the only hits are incidental
prose, and no tracked bet is about entity identity. The schema epoch
(`vessel/session/v1` → `v2`) sits inside the client-seam bet's evidence, and it
*confirms* that bet's existing statement — a wire meaning changed, the version
moved with it, every consumer moved in the same change, no external contract was
touched — so its confidence is unchanged. Checked rather than assumed, as The
Scaffold and The Salt both recorded before it.

**Two present-tense sentences were corrected** in that chapter (lines 777 and
796), which spoke of `vessel/session/v1` as the live schema. Chronicles were
left alone: they are history and were accurate when written. The same stale
present tense remains a live hazard wherever a version number appears in prose —
`windows/vessel/src/snapshot.rs` and the root `CLAUDE.md` were fixed during Task
5's own review, and `clients/game/core/src/cell.rs:74` was retroactively
falsified by the mechanical v1→v2 swap (it describes work that happened against
v1) and is listed below.

## Deferred, and promoted here because the campaign's scratch dies with it

Registry rows were minted for the five with design content (see
`book/src/frontier/idea-registry.md`). The rest are recorded here and nowhere
else:

1. **`reuse_or_mint_entity` turns a 48-bit path collision into a silent identity
   merge** rather than a panic. The only backstop is that the NAME fact is
   functional, which is coincidence rather than guard. One sentence in its doc
   would close the gap in the record; a real fix needs the strict form to
   distinguish "same lineage" from "different lineage, same hash".
2. **`ensure_index` rebuilds `minted` from `Fact::subject` only, not
   `Fact::place`.** An entity appearing exclusively as a place is invisible to
   the rebuild, so its lineage re-mints silently after a load. Latent today
   because every entity in the corpus is also a subject somewhere.
3. **`pub fn test_lineage` ships a deliberately-wrong lineage** (`parent: None`,
   role `"test"`) in the kernel's public API, kept out of production by a doc
   comment alone. It is `pub` because the tests needing it live in other crates.
   A default-deny source scan is the right shape.
4. **The new `MalformedKind` guard on the people-ordinal lookup is unreachable
   through any existing caller and untested.** Sibling `MalformedKind` guards
   each pair with a corrupted-registry test.
5. **The acceptance test's population comment overstates one assertion.** Its
   `len() + 1` check is described as catching "a derivation that moved ids while
   keeping the old ones around" — the counter is exactly that, and passes it.
   Same overstatement class the file's header corrects elsewhere.
6. **Only two of the pipeline's minting stages are perturbed** by the acceptance
   test. A domain reintroducing positional identity in *lineage construction*
   (`ordinal: ledger.entity_count()`, say) is a different property and is
   uncovered. If a future campaign adds a legitimate optional pipeline stage,
   that stage becomes a free third arm.
7. **The anti-vacuity floors are loose** (50 of ~400 occupations, 10 of ~143
   NPCs) and would not notice a 90% roster collapse. Seed 42's occupation count
   has already halved once under an unrelated campaign.
8. **`emit_history` may be called at most once per world** — an occupation's
   ordinal is its index in `History.records`, so a second call re-derives ordinal
   0 and panics. Correct, load-bearing, and undocumented on the function.
9. **The ordinal is `u16` and every production site truncates with `as u16`.**
   Past 65,536 siblings of one role the cast wraps and the collision assert fires
   loudly. No site checks or documents the ceiling.
10. **`hornvale_settlement::genesis` is retired but still `pub`**, still carrying
    a lineage-minting loop, and still named in prose as though it were the live
    settlement stage. Nothing outside its own unit tests calls it. Someone should
    decide whether it stays.
11. **`clients/game/core/src/cell.rs:74` describes work that happened against
    `vessel/session/v1`** and was retroactively falsified by the v2 swap.
12. **The precision scanner does not descend into escaped JSON.** The `known`
    channel carries `locale/room/v2` documents as strings; nothing in there is an
    id today (room ids are ~30 bits), but a future channel that escapes one slips
    past. Documented at the helper.
13. **No decision record was written for "an envelope bump does not cascade into
    its embedded schemas."** The rationale lives in `snapshot.rs`'s module doc.
    If the project wants it as a citable rule, it deserves a `docs/decisions/`
    entry.

## Handoff

**The Particular resumes at its Task 3.** The six fixtures it moved are the
measurement that opened this campaign, and they should now hold still under any
change that does not reorder a lineage. If they move again, the first question is
*which lineage's ordinal source changed*, not *what got minted ahead of them*.

**The peoples-as-entities work should follow this, not precede it** — it was
already sequenced that way in the registry, so that peoples receive semantic ids
without a second churn.

**The keystone test remains off-limits to accommodation.** It was not edited
during this campaign (its last change predates the branch) and both of its tests
were green at every stage. If a future derivation reddens it, the derivation is
wrong.
