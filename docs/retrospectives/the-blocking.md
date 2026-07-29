# The Blocking — Retrospective

Process lessons only; the product is in
[the chronicle](../../book/src/chronicle/the-blocking.md).

## The headline: the approved package's leading risk was reversed by measurement

The G3 package the owner approved **led** with "byte-identity breaks and the
health battery becomes the GATE rather than a check — the first time in this
program." It did not happen. Task 6 measured **RE-PIN**: exactly one committed
file moved (the seed-42 possession transcript), no metric golden and no census
golden moved, `room/furnishing` stayed at v1, and `make gate` came back green at
2413/2413 as a *check*.

The process lesson is not "we got lucky." It is that **the plan was written to
measure the epoch rather than to survive it**, and that shape is what made the
reversal legible instead of confusing. Task 6 as originally drafted asserted the
break; had it shipped that way, a green health battery would have read as *an
epoch survived* when what it actually meant was *no epoch occurred*. Those two
readings look identical from the outside and imply completely different things
about what the campaign owes the next one.

Getting there required vocabulary the spec had blurred into one word. Negating
each definitional property of "epoch" produced five distinct things — **re-pin**,
**epoch**, **empty** (a bump on a label with no draw site), **latent** (an
inventory grown behind closed gates), and **undeclared** (a derivation that moved
with no bump). Only after the words existed could the plan branch on the answer.
Generalizable: **a plan cannot measure something the spec has only one word
for.**

The polarity flip that produced the *latent* branch is worth keeping too. The
spec treated the epoch as a cost bravely borne, so avoiding it read as pure win.
Inverted: an epoch is the only mechanism by which a world frozen by its own
goldens is allowed to improve, so deferring one while banking its vocabulary can
be the **worst** of the three outcomes rather than the best. That inversion is
why the latent response was made mandatory rather than optional, and it is
ratified in decision 0084.

## Six implementers in a row disarmed a check to prove it discriminates — and it should be a rule

This is the campaign's most valuable process finding, and it became the norm
**without anyone writing it down**.

Task 1's implementer, unprompted, deleted a type-audit tag to confirm the tool
actually read the new file rather than passing by omission (`vessel:45: untagged
primitive at return (contains)`, exit 1). Every implementer after that did the
same thing to their own checks:

| Task | Disarmed | What it proved |
| ---- | -------- | -------------- |
| 1 | `Rect::contains`'s tag | the audit reads a new file, not just old ones |
| 3 | — | rule 7 passed first run; two embedder defects found by the checker |
| 4 | rule 3's control; the locale-seed control | "eight different built locales derived the SAME floor plan" fires |
| 4b | rule 8, twice (in-suite seal + a temporary disarm) | a sealed pocket is detected, and the seal must be the *last* floor cell |
| 5 | five controls | `committed_fact_count()` as a ledger total catches a `disposition-shift` a per-predicate accessor would have missed |
| 6 | three, incl. `at_locale: true` on the-high-seat | "the LOCALE band's warm built composition moved: this is an epoch" |
| 7 | four (`what_moved`, `#[serde(default)]`, `stamp`, the struct tag) | an unstamped world fails to load with `missing field derived_under` |

**It caught something every time.** Including three of the controller's own
claims that were self-consistent and wrong:

1. Rule 3 could not fail on derived input at all — it asserted the
   contrapositive of the wall derivation's own exemption condition over the same
   ownership map (ledger #19), and rule 3(ii) as worded was a *type-level* truth
   while `passable()` is `!matches!(_, Wall)` (#30).
2. `every_destination_the_plan_depicts_is_command_reachable` compared the wrong
   two numbers — the plan draws every doorway of the **structure** while the
   footer names at most two ways out of **one** chamber, so at four chambers it
   asserted `3 <= 2`, and it passed at seed 42 only because `1 <= 2` (#21).
3. `growing_is_pure_and_reads_the_seed` used a structure with **zero** residual
   freedom, so it would have failed with "the seed is ignored" — a false
   accusation against correct code (#16).

**The rule this owes: a check introduced by a task must be shown able to fail,
in the same task, and the disarm's output pasted in the report.** Six
implementers in a row inferred it from context. The seventh might not, and the
practice is too valuable to leave as folklore. Worth noting where prose already
proved thin: Task 6's implementer correctly declined to run `cargo test
--workspace` separately because `make gate`'s target is a strict superset — the
right call, made by an agent the (not-yet-live) guard could not have stopped.
Prose worked that time. It is still not what to bet on.

## Eight appearances of one trap family, and its unchecked prose sibling

A `type-audit:` tag is **dead** — silently, with no error — in four distinct
positions, and this campaign hit all four:

1. on a **field's own doc comment** (only the struct's attributes are read);
2. on a **non-`pub`** item (`extract.rs` gates on `is_bare_pub`);
3. on a signature with **no tracked primitive** (an opaque `-> Rect` or
   `-> Lattice` return, where the extractor never pushes an item — one such tag
   called a `Lattice` a *count*, a verdict the tool never gave);
4. on an **enum variant** rather than its payload slot.

Eight appearances across seven tasks. The failure mode is not a false pass so
much as a **verdict that was never rendered**: the tag reads like an audited
decision and is inert. Two facts banked by disarm rather than by reading:
`BTreeMap<String, String>` *does* carry a tracked primitive, because
`primitives.rs` recurses into generic args; and a tag on `Brief`'s struct doc
attrs *is* live (dropping it gives `vessel:44: untagged primitive at
peak_population`) — the seventh check of the family and the first that came back
clean.

**And the family has a prose sibling that nothing checks at all.** `brief.rs`
said "Three fields are read" and then named four, omitting that `cold` survives
only as a `debug_assert`. `streams.rs` pointed a reader at `docs/followups.md`,
which does not exist in this repository. Neither is catchable by fmt, clippy,
type-audit, or any test. A tag in the wrong position is at least *machine
checkable in principle*; **a count or a path inside a doc comment is checked by
nobody**, and the campaign's whole verification posture is aimed at code and
artifacts. The book has a doc-link check; source comments have none. Given that
the machine-checkable half of this class appeared eight times, the invisible half
is probably larger.

## Sixteen plan defects, and three of one shape

The plan carried most of the campaign's defect mass again, as The Lintel's
retrospective predicted it would when a plan is written in verbatim code. What is
new is that **three of the controller's plan defects were the same reasoning
error**, and naming the shape is worth more than the three fixes:

- **Reasoning about rects while the grower makes blobs** (#17). `region_of` was
  specified to scan bounding rects; grown regions *overlap* (at seed 2, regions 0
  and 1 both start at x=0 spanning 5 and 10 columns), so rules 1–4 would have
  measured rectangles rather than blobs. It survived review because rect-scanning
  and true ownership **agree on every rectilinear lattice** and disagree only on
  grown ones. A near-agreement that holds for the method you happen to read first
  is how a wrong helper passes review.
- **Reasoning about the state at seeding while the defect lives at completion**
  (#29). "Seed chamber *i*+1 two cells from chamber *i*, so exactly one wall cell
  sits between them" is necessary and nowhere near sufficient: by the time the
  flood finishes, a *third* blob can be sitting beside that gap, so opening it
  joins chambers the graph does not link. Generalized: **a claim that a
  construction is sufficient is a claim about every interleaving of that
  construction**, and the tell is a sufficiency argument that examines only one
  moment in time.
- **Reasoning about a roster's rows while its semantics are "everything ever
  declared"** (#36, and the worst planning defect of the campaign). The epoch
  stamp was designed by reading the manifest **renderer**, and a renderer is
  right to show retired rows. `language/<family>/lexicon/root/v2/<concept>` is
  marked retired only in its prose and is listed *after* its live successor, so
  both strip to the same key and a last-wins insert would have stamped **v2** —
  the retired epoch, in every world, silently. The mechanism built to make epochs
  honest would have been lying from its first commit, and the lie would surface
  only at the exact moment it was needed. Fixed by highest-version-wins compared
  *numerically*, sound because epoch suffixes bump and never rename.

The common shape: **a data source inferred from a presentation, or a property
inferred from one moment, or a structure inferred from the case you read first.**
All three are correct-where-you-looked.

**A global constraint contradicted a success criterion, and that resolves itself
badly** (#11). The plan said "Tasks 1–5 must be byte-identical" while §9 required
the floor plan to appear in the committed seed-42 gallery — which means adding
lines to `scripts/possession-walk.txt`, which is the input that *produces* that
gallery. A task cannot both extend the walk script and leave the gallery
unchanged. Corrected to: Tasks 1–3 clean, Tasks 4–5 move transcripts only, Task 6
the only task that may move a metric golden. The failure mode is the point:
**a constraint that contradicts a success criterion gets resolved in favour of
whichever one the implementer reads second**, silently, and either way the report
looks compliant. It is followup 15's lesson (a green mechanical gate hiding a
hollow headline) running in the other direction — here the mechanical constraint
would have suppressed the headline rather than the headline hiding behind the
constraint.

Two smaller ones worth their names. **Three test snippets in one task were
mutually contradictory** — two asserted keys retain the version, one only makes
sense with it stripped (#37a). A plan whose own tests disagree about the data
shape is a plan that never ran. And a plan's own **verification command hid the
result it asked you to read**: `--lib` is required, or the integration binaries'
empty blocks push the real test count off the tail (#15).

## Two numbers asserted without measuring, in the same task

Recorded as a process hit against the controller (#29), because the campaign's
own autopilot rule is *verify a quantitative claim before it goes in a plan*:

- "Roughly 20% of the extent is exterior shell." Measured: **36% at one chamber,
  28% at two, 19% at three and four.** A ring is a perimeter against an area, so
  the *smallest* plan pays nearly twice the quoted figure, and the assertion
  written from the plan's number went red against the code. One line of
  measurement would have caught it.
- The sufficiency claim above, which is subtler and produced both grower defects.

The spec now states the measured range and a test prints the table.

## A cost measurement corrected three times, and the rule that caught it

`allocate`'s cost was recorded four times and superseded three:

| Where | Number | Profile | Fate |
| ----- | ------ | ------- | ---- |
| #15 | 6.79 µs (16×16) | release | superseded by `owner` landing |
| #18 | 27.6 µs (16×16) | release | superseded by the wall-as-cell rework |
| #28 | 182.9 µs (19×19) | **unstated** | identified as *debug* |
| #31 | **~9 µs** (19×19) | release (174.6 µs debug, ~19×) | **stands** |

The 3.3× regression at #18 was real (256 `BTreeMap` inserts for the per-cell
owner map) and was **re-measured rather than left as a false record**. The #28
entry is the one that matters procedurally: the report stated a number with no
build profile and compared it against a *debug* figure, so it was almost
certainly debug — which meant the release number the spec needed was unknown at
that point, and Task 5 was asked for it. **Followup 22's rule — a timing claim
must name its profile — has now caught a task report *and* a doc comment**, on
top of The Lintel's near-miss where it caught a spec. The stale "27.6 µs
release" claim in `ALLOCATE_BUDGET_MICROS`' own doc was corrected in the same
commit.

The corollary the campaign also adopted: **a gate-run timing assertion must be
keyed to the debug ceiling**, because that is the profile the gate runs in. A
ceiling keyed to the release figure looks rigorous and is flaky.

## Corpus size is a check's resolution

Task 4b's first grower defect appeared at roughly **1 seed in 12**, and the
corpus was **24** — right at the check's own resolution, where a green run
reports luck rather than correctness. Widened to 192. Generalizable: **a corpus
that would miss a 1-in-*N* defect at its own size is not a check, it is a
sample**, and the corpus should be chosen against the failure rate it is meant to
catch rather than by feel.

Relatedly, two structural ways a check reported nothing:

- **A fallback that returns a plausible value hides a fidelity failure from a
  shape test.** `meeting_cell` returned a valid-*looking* cell for a link it had
  failed to realize, and Task 2's tests asserted only
  `doorways.len() == links.len()`, so a grower that could not embed the chain
  passed. A fallback in a derivation path should return something a checker will
  reject.
- **A filter that silently narrows its input is worse than one that errors.** A
  test helper filtered picture rows by the plan's glyph alphabet, so adding `@`
  dropped the row the mark stood in and corrupted every row and glyph count
  **without failing a single test**.

## Two spec sentences were aspirational in the present tense

Third instance of one class this campaign (#6, #19/#30, #32). §6.1 said "only
chamber-to-chamber and locale-to-locale movement touches committed state," which
reads as a statement about today and is not one — **nothing commits at chamber
granularity at all**, which is the campaign's own §5.1 constraint and precisely
why bumping `room/chambers/v1` is still free. A threshold crossing is no more
`COMMIT`-tier than a cell step; what distinguishes it is that it *re-renders*,
which is a rendering difference and not a persistence one.

Why it matters beyond one sentence: **writing the model you are heading toward
in the present tense is how a closing window gets mistaken for a debt already
owed.** The same error appeared in the controller's own spec draft at the start
of the campaign (#6, the claim that player marks are keyed to chamber addresses —
false; the session's committed facts carry `place: None`).

## The owner's mid-campaign model change was cheaper than deferring it

A wall became a cell at the owner's direction after Tasks 1–4 had shipped the
boundary model. It reworked the lattice core and cost ~20% of the extent to
exterior shell — and it was still the right time, because the alternative was
Task 5 inheriting a `(2w+1)` coordinate mapping, The Sighting inheriting blocked
edges instead of blocking cells, and The Panes inheriting a translation layer. It
also retired a bound relaxation granted one task earlier (#22 became moot) and
**deleted** a test rather than adding one (#24).

Two process notes. The implementer who found that the doubled render was needed
**correctly declined to reverse a Task 1 constant on its own authority and
escalated instead**; the ledger entry is the authorization. And the controller's
own stated cost of the change was overstated in the direction that would have
discouraged it (the totality invariant is re-founded one type up, not lost) —
which is a reminder that a cost quoted to the owner during a design conversation
gets the same verification burden as a number in a plan.

## Follow-ups, promoted from the scratch register

`.superpowers/sdd/followups.md` is gitignored and dies with the worktree, so it
is promoted here in full. Numbering follows the register; the register's own
numbering duplicated 35–37, so the duplicates are folded and marked.

### Discharged during the campaign

- **1. Two ledger entries owed their ideonomy pass.** Discharged at G3.
- **2 / 2a. Measure before speccing; every cost claim names its build profile.**
  Discharged and generalized — see the cost-measurement section above. The spike
  numbers (`interior_of` 0.666 µs/visit, `route_within` 1.0 µs/query, integer
  shadowcast 2.1/3.5/7.0/24.2 µs at 16²/24²/48²/96², stand-in cell A\*
  32/61/325/1975 µs) remain uncommitted spike figures and are **release**,
  ~10× faster than debug. Anything through the wasm ABI is still an
  extrapolation (~3.6–3.8×), labelled as one.
- **5. `enter`/`exit`'s fixed refusal sentence.** Handled: `exit` still answers
  the old sentence byte-for-byte; `enter` refuses with a physical reason.
- **13. `chamber_nouns`' half-stale doc comment.** **Fixed** — the comment now
  explains that apertures are named by direction and that the catalogue bounds
  what a chamber's prose may *say*, not what the player may type.
- **35 (first). Five patterns are LATENT and the gate is named.** Discharged into
  decision 0084 and the registry.
- **38 / 36 (second). Two doc defects asserting an uncheckable count and path.**
  Fixed; the *class* is followup 38 below.

### Open, and inherited

- **3. `make vessel-check` is red on `main`** (seed 43 returns `NoSettlement`;
  `clients/vessel/wasm/drive.mjs` asserts seed-43 possession succeeds).
  **Inherited from The Purview via The Lintel and left unfixed by this campaign
  too.** Three campaigns is well past the point at which "inherited" stops being
  a sufficient reason.
- **4. `sum::<f64>()` over an empty iterator serializes as `-0.0`,** and
  `quantize` does not normalize it; 45 call sites workspace-wide. **Inherited and
  left.** The Rose Window register argues for closing the class centrally in
  `kernel/src/quantize.rs` with a pinning test.
- **6. The lat/lon → unit-sphere conversion is hand-copied five times.**
  Inherited from The Purview, still open.

### Open, product

- **7. The band notation needs a durable home.** Captured as a registry row, and
  the registry governs nothing (decision 0031). **Proposed as a decision record
  at G6** — see below.
- **8. The census re-pin question.** Answered: no census golden moved, so no
  re-pin was needed or requested.
- **10. Duplicate anchor kinds render ungrammatically** ("holding a hearth and a
  hearth"). Verified unreachable in production, but `Interior::push` is `pub` and
  has no uniqueness constraint. The right fix is *counting* ("two beds"), not
  deduplication.
- **12. You can walk into a structure step by step but not back out.** `leave()`
  clears `inside` entirely, so a structure is **one-way-in in practice**. The fix
  is a named backward aperture (`further out`), which is a decision rather than a
  tweak, since `out`'s meaning is pinned by tests.
- **16 / 17. Tech as a material span cap, and a durable extent.** Both wait on the
  ruin signature the brief omits on purpose. Registry rows added
  (`CLIENT-span-cap`, `CLIENT-durable-extent`).
- **18. A content-addressed stream label.** Rejected but names the residual hazard
  in decision 0083 — a manual bump is forget-prone. Cheaper form: pin
  label → output for one fixture. Registry row added.
- **20 / 36 (first). `Brief::from_parts` has 17 positional call sites, not 11.**
  The plan's count predated the seven `lattice/` fixtures. It is fine at 7
  parameters and unpleasant at 9; `brief.rs`'s module doc promises adding a field
  is cheap and it is now measurably not. Do a builder or a `Default` before the
  eighth field.
- **21. `walls_between` was `extent.area()` × a linear scan, 3× per cell pair.**
  Superseded in substance by the single `walls_around` derivation, but the
  quadratic shape is what the DOF budget assertion really guards; noted in the
  budget constant's doc comment.
- **23. Rule 7 counts draws, not effects.** A grown lattice at one chamber spends
  two seed-cell draws that cannot change the result. Recorded rather than fudged.
- **24. `allocate` regressed 3.3× when `owner` landed.** If `CHAMBER_SIDE` rises
  or a second consumer re-derives per frame, a row-major `Vec<usize>` for `owner`
  is the obvious fix and costs one helper.
- **26. Wall cells could bound warmth diffusion.** Warmth is modelled as pure
  anchor-distance decay with nothing to stop it at a boundary. Registry row added
  (`CLIENT-walls-bound-warmth`).
- **27. A wall cell's condition could degrade with time since abandonment.**
  Pairs with the breach coinage; wants the ruin signature. Registry row added
  (`CLIENT-breach-and-rubble`).
- **28. `AnchorKind::Threshold` and `CellKind::Threshold` are one fact at two
  grains.** Ruling: the lattice is authoritative for *where*, the interior for
  *what is there*. Revisit if a third grain appears (a door that opens is a state
  on the cell, not a second anchor).
- **29. `chamber_prose::WALL_DETAIL` survived the model change by luck.** Its doc
  described a wall as "definitionally a non-adjacency" while its player-facing
  sentence happens to read correctly of a thing with thickness — parity preserved
  by accident, not design. Noted in the file.
- **32. `examine you` names the walk-band room while you are indoors.** Correct
  under §6 (one self-description, never two) and slightly odd in a chamber.
  Fixing it means making `whoami` band-aware, which is a byte-pinned string.
- **33. Any new plan glyph must be added to `picture_rows` in the same commit** —
  and the helper should arguably reject an unknown glyph rather than filter it.
- **37 (first) / 35 (second). `peak_population` has exactly one reader.**
  `the-strongbox` is scoped to `Role::Store` alone, so a populous *seat's hall*
  has nothing to lock up while a populous waypoint's storeroom does. Defensible
  but unstated: either widen the strongbox to `STORING_ROLES` or say in
  `Pattern::needs_populous` why `Store` alone gets it.

### Open, tooling

- **9 / 34. `tools/type-audit` should error on a tag it cannot attach.** A
  misplaced tag is silence today, in four distinct positions (above). A tag the
  tool cannot attach should be an **error** naming the correct position. Cost one
  implementer detour in The Lintel and eight appearances here.
  `Session::walk_depth` carries a pre-existing dead tag; a sweep for others is
  owed.
- **11. Every chamber of a structure had identical prose.** **Closed by this
  campaign** — recorded here because the spec gap it named (The Lintel's §2 froze
  the vocabulary and never stated the consequence) is discharged in both
  chronicles.
- **14. `make gate-full` always dirties a drift-checked path.**
  `windows/chronicle/tests/sounding_sweep.rs` writes wall-clock timings into
  `book/src/laboratory/generated/the-sounding/`, so the close's own drift check
  reports drift after every run and the closing agent must **revert, never
  re-pin** (precedent: last pinned 748 commits ago at The Sounding's close). The
  fix is to commit the *fitted exponents* — which are stable and are what the
  preregistered hypotheses test — and leave the raw timings out of the tree.
  Done again this close.
- **15. A green mechanical gate can hide a hollow headline.** An
  artifact-driven success criterion needs a check that the artifact **contains**
  the thing, not merely that it regenerated without drift. Applied *before*
  writing Task 6 this time rather than discovered after (#12).
- **19. `walls_between` exempted a doorway cell entirely.** Closed: a threshold is
  now a *pair*, and one `walls_around` derivation runs over the authoritative
  ownership map for both methods. The root cause was two passes deciding
  independently what a boundary is.
- **25. See the fallback lesson above.** Left as a standing rule.
- **30 / 31. See the sufficiency-claim and corpus-size lessons above.**
- **37 (second). `cli/tests/lens_purity.rs` reports "world identity drifted —
  that is a terrain or sky epoch" for purely additive metadata.** Task 7's
  `derived_under` key on the seed-42 fixture tripped it, and the message points at
  an epoch that did not happen. Better: diff structurally and say "additive key"
  when that is all it is.
- **38 (second). A retired stream label is indistinguishable from a live one
  except in prose.** Task 7 had to infer liveness from highest-version-wins; any
  future consumer of the roster faces the same ambiguity and may not notice. A
  `retired: bool` on the manifest row would make it a fact rather than a
  convention. **This is the direct cause of the campaign's worst planning
  defect** and is the highest-value tooling followup on this list.
- **38 (class). A path or a count inside a source doc comment is checked by
  nobody.** The book has a doc-link check; source comments have none. Cheap first
  pass: lint source comments for repo-relative paths that do not resolve.

## A third inherited red, found by this close's own `make gate-full`

`make gate-full` came back **red in the heavy tier**, and the failure is
**inherited from `main`**:

```
thread 'history_gates_full_world_and_cross_seed' panicked at
  cli/tests/history_battery.rs:151:9:
seed 2 displacement inert: 0 < 5
```

Verified rather than assumed. The commit tier is green at **2428/2428**, and the
heavy tier run with `--no-fail-fast` is **27 run, 26 passed, 1 failed** — this one
and nothing else. The failure was then reproduced **at `main`'s tip
(107a8045)**, in a detached probe worktree, with the identical seed and the
identical value. Nothing in this campaign can reach it: the only non-vessel,
non-CLI, non-docs change is a `const` **hoist** in `domains/history/src/flesh.rs`
(`HAMLET_POPULATION_CEILING`, byte-identical behaviour) and an additive
`#[serde(default)]` field on `World`, and the campaign's census-fixture live
probe and every metric golden regenerated unchanged.

**It is a known-unverified floor, and the record predicted this exact outcome.**
The Tumult's plan (2026-07-22) flagged it: `SWEEP_MIGRATION_FLOOR = 5` and
`MIGRATION_FLOOR = 20` "are heavy-tier (`#[ignore]`, outside the commit gate) and
were never run against the corrected `migration_events` query, which more than
halved on seed 42 (133 → 58). **The cross-seed sweep minimum is unmeasured.**"
The floor was left at a value nobody had measured against the corrected query,
in a tier the commit gate does not run — so the first campaign to run
`gate-full` inherited the red, four campaigns later.

The generalizable lesson is about *where* an unverified number is allowed to
sit. A floor that is both **unmeasured** and **outside the commit gate** is
invisible until someone pays for the heavy tier, and the cost lands on an
unrelated campaign that cannot tell inherited red from its own. Either measure
the floor when you write it, or put the assertion where the gate will run it.
**This campaign did not fix it** — the honest reading of seed 2 producing zero
displacement events is either a floor set too high or a real inertness in the
migration model, and adjudicating that is a history-domain question with its own
measurement, not a number for a closing agent to move. Lowering a floor to green
a gate is precisely the seed-shopping decision 0016 forbids.

## Inherited and outbound debt

**Inherited and left, deliberately and with the reason stated:** followups 3
(`make vessel-check` red on `main`) and 4 (`-0.0` from an empty `sum::<f64>()`),
plus the heavy-tier `history_battery` floor above — three inherited reds, none of
them this campaign's doing and none of them fixed here.
Both were named in this campaign's own register as "inherited, and now in scope",
and neither was fixed. Followup 3 is now three campaigns old. Neither is a
consequence of this campaign's changes; both are things this campaign shipped
against rather than repaired, and saying so plainly is the point of this section.

**Outbound, product.** A named backward aperture (`further out`); counting rather
than repeating duplicate anchor kinds; `whoami` band-awareness; the strongbox's
role scope; a durable extent and its span cap, both gated on the ruin signature;
wall cells bounding warmth; the breach/rubble state machine, which hands The
Vestige a collapse mechanism.

**Outbound, epoch.** Five patterns are **latent**: gated `at_locale: false` and
unreachable from any live read. The gate is the first mark committed inside a
chamber, and on that day all five become an epoch retroactively, costing
`room/furnishing/v1 → v2` plus whatever the health battery and the censuses then
read off chamber compositions. `room/chambers/v1` is free to bump **today** and
will not be after that same first mark. Both are recorded in decision 0084 rather
than left in a register.

**Outbound, tooling.** The type-audit tool should error on a tag it cannot
attach; the manifest roster should carry `retired: bool`; the lens-purity check
should distinguish an additive key from an epoch; the sounding sweep should stop
writing wall-clock timings into a drift-checked path.

**Outbound, calibration.** `history_battery`'s `SWEEP_MIGRATION_FLOOR` (and
`MIGRATION_FLOOR` beside it) need the measurement The Tumult deferred, and seed 2
returning **zero** displacement events needs adjudicating as either a
mis-set floor or a real inertness in the migration model. Owed by a
history-domain campaign with a measurement, not by a closing agent with a
smaller number.
