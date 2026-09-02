# The Hallmark — retrospective

Process, not product. The chronicle carries what was built:
[The Hallmark](../../book/src/chronicle/the-hallmark.md). The criterion is
[decision 0517](../decisions/0517-the-kernel-membership-criterion.md); the
rulings are in the campaign ledger
(`docs/superpowers/ledgers/2026-09-01-the-hallmark.md`).

## Plan-time verification falsified three claims before any task ran

This campaign was specified from a survey, and the survey was mostly right.
The pre-dispatch verification pass — reading the source each claim rests on
rather than the claim — falsified three things, all of which would otherwise
have reached an implementer as instructions.

1. **`domains/person`'s `f64` days were filed as migration residue. They are
   `waiver(decision-0126)`.** The tag says the DTO is deliberately bare — each
   field becomes a `WorldTime` in `fact()`. A `waiver` and a `pending` are
   *opposite speech acts*, and the survey had summed them into one count. Had
   the spec kept them, a task would have migrated a field whose own tag
   declares the bareness intentional, against a decision record.
2. **The bake path writes a bake YEAR into `EraClimate.day`, a field whose
   contract is absolute standard days.** The plan's Task 5 was a
   straightforward retype until this was read. It became diagnosis-gated with
   an explicit STOP branch — the change that saved the campaign a bad landing.
3. **`Formation`'s cave variants carry corpus spellings that genuinely differ
   from the kernel roster's legend.** The plan's embed step became an
   adjudication with two named exits and a decision rule rather than a
   prescribed edit.

The general form: **a survey produces a count, and a count is where two
opposite speech acts get added together.** The tag vocabulary exists precisely
to distinguish "this is deliberate" from "this is debt"; a residue tally that
does not read the verdict word has thrown away the only information that
matters.

## The plan text carried one defect, and the implementer refused it correctly

Task 4's Step 2 said to add a `serde` dependency to `domains/climate`. The
architecture test asserts a domain's dependency list equals
`["hornvale-kernel"]` *exactly*, so the instruction was unlandable. The
implementer refused it and used a field-level `serialize_with` helper in
`windows/locale` instead — the correct shape, since locale is the crate that
needs the serialization and may depend on climate.

Two things about how it was handled are worth keeping. The controller accepted
the substitution as a **plan defect** rather than an implementer deviation,
which is what it was. And the reviewer was instructed to verify *both* halves —
that the exact-match constraint claim is real, and that the helper is
byte-exact for all five variants. Verifying only the fix would have accepted an
implementer's diagnosis of the plan on the implementer's word; the constraint
claim was the load-bearing half, and it held.

The reusable part, worth stating plainly because the plan got it wrong and a
future plan will want it: **a domain crate can never take a normal dependency
on `serde` (or any other allowlisted external), whatever `ALLOWED_EXTERNAL`
says, because the architecture test requires a domain's dependency list to
equal `["hornvale-kernel"]` exactly. A field-level `serialize_with` in the
window is the escape hatch** for "a window needs to serialize a bare domain
enum". The allowlist and the layering test answer different questions, and
reading the first as permission for the second is what produced the defect.

## A diagnosis-gated task lets a wrong plan fail cheaply — and the STOP's output was reused

Task 5 STOPped. That is the outcome the branch table was written for, and it
cost one task rather than a landed migration and a revert.

The part worth generalizing is what the STOP *produced*. Its output was not
"blocked"; it was a trace — every producer, every consumer, every comparison,
with line citations — and a consumer table saying what each reader of the
field actually needs. When the decider reversed the deferral at the merge stop
and ordered the fix, the fix task was tractable *because that table already
existed*. It also **overruled the ruling's own anticipated mechanism**: the
ruling assumed a convert-at-the-named-crossing repair, and the consumer table
showed that one reader needs a unit and the other needs only an ordering,
which selects a different design (a bake-owned parallel year axis) and rejects
the conversion as storing a true unit with a false referent.

So: **a task whose gate is a diagnosis rather than a change converts "the plan
was wrong" from wasted work into the input the repair needs.** The
corollary is that a STOP must be written as evidence, not as a verdict.

## `file:line` anchors in a dossier rot within hours — three fix rounds

Three separate review rounds in this campaign existed only to correct line
numbers, every one of which was correct when it was taken:

| citation as written | correct line | caught by |
| --- | --- | --- |
| `ecology.rs:432` | `:417` (and `:359-410`) | Task 12 fix round |
| `packs.rs:1023` | `:1011` | Task 12 fix round |
| `strata.rs:126` | `:142` | final whole-branch review |

The first two came from the survey dossier, travelled through the dispatch,
and landed in committed idea-registry rows. The third was in a registry row
*and* the campaign ledger, and pointed at a comment two lines from the
assignment it described.

A fourth was found at close and fixed differently, which is the recommendation
made concrete: `windows/vessel/tests/suite/submerged_before_arm.rs` cited
`facets.rs:305` for an `unreachable!` arm that a comment edit in the same
campaign had pushed to `:303`. The final fix wave saw it and deliberately left
it as out of scope, which was a reasonable call about a fix round and would
have left a decaying citation in the tree. It now names the symbol —
`BiomeExpr::biome`'s arm — and carries no line number to rot.

None of these was carelessness. A line number is a claim with a timestamp, and
the surrounding file was edited between the reading and the writing — in one
case by this campaign's own tasks. **A citation should name a symbol and a
file; the line number is a convenience that decays**, and where a line number
is genuinely wanted, it has to be re-read at the moment of writing rather than
carried forward from a dossier. Nothing mechanical checks these: the docs
consistency test resolves *links*, not offsets.

## The campaign's own defect class landed on its own work, twice

The charter here is placement records that outlive their subject. Both
instances were caught by human review; no gate could see either.

- **The delta review found a doc comment outliving its subject.**
  `delve_seating.rs`'s `genus_of` still described itself as "the third leg of
  decision 0094's duplicate roster" and cited the enforcement pattern of
  `cave_kind_correspondence.rs` — a file this campaign had deleted four
  commits earlier. The roster had two legs, not three, and the citation
  pointed at nothing. This is the same shape as the retired-clients paragraph
  in the root `CLAUDE.md`: a record that outlives its subject does not sit
  inert, it produces wrong answers from readers acting in good faith.
- **Task 10's review found the ratchet's marker matched unanchored.** The tag
  parser looked for `placement:` as a substring anywhere on a doc line, so
  prose *mentioning* a placement tag would have satisfied the check —
  a detector built to demand adjudication, satisfiable by discussing
  adjudication. Fixed to anchor at the doc-line start, with tests pinning both
  directions.

The pairing is the lesson: the campaign that generalized the placement rule
also wrote a stale placement record and a parser that could be fooled by prose
about placement. **Building the instrument does not immunize the builder**, and
the only thing that caught either was a reviewer reading for the class rather
than for correctness.

## `gate-commit` cost: the folklore number was off by 7x, on the wrong axis

Three measurements from this campaign, all on the Mac, warm tree:

| edit | measured wall |
| --- | --- |
| Task 2 — `UnitError` in `domains/astronomy` + `domains/paleoclimate` | **614 s** |
| final wave — doc comments only, `domains/climate` + `windows/vessel` | 259.8 s |
| delta wave — doc comments only, `windows/worldgen` | 274.2 s |

The root `CLAUDE.md` prices a domains-layer edit at ~84 s and a warm-tree gate
at ~25 s. The comment-only runs are the sharper datum: a doc-comment change
alters no behaviour and no signature, and still cost four and a half minutes,
because touching a file invalidates its crate and everything downstream of it.
That block already carries the correct general statement — the cost is
compilation units, not tests and not the layer name — and these numbers sharpen
it one more turn: **the predictor is how many crates sit downstream of the
files you touched, and the semantic weight of the edit is irrelevant to it.**
A doc-only edit in a domain is as expensive as a signature change in the same
domain. Recorded as measurement with n=1 per shape, from one machine, with
cache state unrecorded — do not price a gate from this table either.

## Deferred, each with a home

- **`rock_rank`'s hand-written ordinal** (`windows/vessel`'s underworld
  readout) could read the kernel `Horizon`'s own `Ord`/`all()` now that the
  roster is shared — the natural follow-up to Task 7, not attempted in it.
- **Year-shaped `.day` values in `history_bake`'s test fixtures.** The numbers
  are small integers in a field whose contract is now deep-time days. Every
  read of them in those tests is an ordering read, and the helper's doc says
  so, so nothing is wrong — but a reader will mistake them for days.
- **`Formation::Cave(CaveKind)` is write-only.** No production code
  constructs it; every reference wildcards. This is pre-existing (no
  constructor existed for the three variants it replaced either), and the
  variant is waiting for its first producer.
- **`WorldTime::GENESIS` as a `None` sentinel in `strata::extract`.** The same
  committed value the old `0.0` sentinel produced, and unreachable in every
  world today — but "no peak era" and "the peak era is at genesis" now spell
  the same.
- **Uneven `expect` message style** in one retyped test — cosmetic, noted by
  Task 15's reviewer.
- **Two error-convention outliers, from ledger #9.**
  `domains/terrain/src/crust.rs`'s `Result<CrustKm, String>` and
  `windows/worldgen/src/harvest.rs`'s `LatError`. Neither is a shape twin the
  detector sees, so neither is covered by the ratchet; the second dissolves
  with the queued angle/latitude family.
- **`serialize_ground_kind`'s variant strings are hand-duplicated**
  (`windows/locale`, the helper that replaced the deleted `Substrate` shim).
  An *added or removed* variant is a compile error, but a **renamed** one
  compiles fine and silently changes the emitted JSON spelling; only the
  pinning test and downstream goldens would notice. The risk is inherited from
  the enum this replaced, not introduced here — recorded so it is not
  rediscovered as new.
- **The bake's `EraClimate` dependency is vestigial.** `history_bake` reads
  exactly two things off the paleoclimate type: `.ice` (which the bake fills
  all-false on every production path, and which the code itself calls
  "currently inert") and `.day` (an ordering key). A future campaign could give
  the bake its own small era container and drop the paleoclimate type from
  `history_bake` entirely, which would make the axis question *structurally
  unaskable* rather than merely answered. Flagged by two successive
  implementers and out of scope for both; this line is its only home, and it is
  registry-row-shaped if anyone wants it there.
- **The detector's subset blindness** (ledger #6). Member-set matching is
  exact, so a roster mirrored *inside* a larger enum — the very shape Task 7
  repaired by hand — is invisible to it. Accepted for v1 on the nag-budget
  argument: a "shares ≥ k members" detector is noisy in proportion to k's
  arbitrariness. Revisit only if a real subset mirror recurs.

## Close notes: keystone refreeze and the census, both N/A with a reason

**No fixture was refrozen and none should have been.** The campaign is
byte-identity preserving: a seed-42 world is sha256 `e70ca3d0…` before and
after, verified three times across the axis repair and the `WorldTime`
migration, once independently by a reviewer. Every drift-checked artifact is
unmoved except the type-audit report and the digest's decision index, which
any pub-boundary change and any new decision record must move.

**No census column can have moved**, for the same reason: the census is a
function of generated worlds, and no world byte changed. There is no
measurement here to re-pin.

**One committed file did move and is recorded rather than smoothed over.**
`windows/locale/tests/fixtures/column_before.txt` — 810 lines of `{:?}` output
where `stratum: Regolith` became `stratum: Rock(Regolith)`. It is a debug pin,
not a declared generated path, and its rebaselining was proved bidirectionally
(810 lines out, 810 in, same cells, same order). Worth saying out loud because
0517 refuses promotions that change a committed spelling without an epoch, and
every *member* here kept its spelling — the thing that changed was the
nesting, visible only through `Debug`. A campaign claiming "no committed
spelling changed" should still be able to name the committed file that changed.

## What went right

- **Both G6 reversals were the decider's, unpacked, and neither was absorbed.**
  The two execution-time deferrals (the day/year axis, `Formation`'s cave half)
  were controller adjudications, not spec constraints, and were presented as
  such. The transferable half: an adjudication that defers on a cost reading
  should say **what it weighed**, so the decider can see what it did not. The
  `Formation` deferral was correct on its own terms — no spelling table exists,
  so the promised payoff was absent — and the decider was valuing structural
  correspondence as a good in itself, which the deferral had not priced.
- **A determinism claim was made by construction rather than by measurement.**
  The axis repair argues control-flow identity: the same doubles reach the same
  comparison, so every `<=` outcome is bit-for-bit unchanged including exact
  grid alignments. The seed-42 byte comparison was then run as the *check* on
  that argument. Both were done, in that order, and the ordering is the
  discipline — a byte comparison alone cannot distinguish "unchanged" from
  "unchanged on this seed".
- **A migration's own justification was corrected in the direction that made
  it weaker.** The `WorldTime` retype was first justified by "ticks→days
  round-trips losslessly at these magnitudes", which is true and is not the
  direction the argument needs. The load-bearing conversion is days→ticks,
  which always rounds and is a no-op here only because every era day is an
  exact whole-day integer. The corrected note says a future fractional step
  needs the argument re-verified.
- **A gate placement was decided by measurement against a rule frozen before
  the tool existed.** The threshold (within 2x the type audit's warm cost) was
  written into the plan; the tool measured 4.5–5.8 s; it joined the commit
  gate. The expectation and the measurement agreed, and only the measurement
  was load-bearing — which is the whole reason the rule was written as a
  decision rule rather than a prediction.
- **A rename sweep verified its token against the type, not against the
  token.** `GenesisOutcome`'s payload rename (`system`/`globe` → `value`)
  touched roughly fifteen sites, and the *same tokens* name unrelated struct
  fields elsewhere — `GeneratedSky.system`, `GeneratedTerrain.globe`,
  `AstronomyView.system`, `TerrainView.globe`, each with its own type. A
  blind `sed` would have been silently wrong in four places. Each site was
  resolved against its type before the edit, and a `cargo check --workspace
  --all-targets` afterwards found the two downstream sites the compiler had
  not yet reached.
- **A fake decision number in a doc example is a dangling citation.** Task 10
  wrote `decision 0500` as a placeholder in a doc-comment example and two unit
  tests; the decision-cite drift linter flagged all four, correctly, because it
  does not know an example from a claim. Placeholders in example text have to
  be non-numeric (`spec anchor S-9`) — cheap to know in advance, a red gate to
  discover.
- **A deleted test was justified as a dissolved claim, not as cleanup.**
  `cave_kind_correspondence.rs` asserted that three cave kinds reach three
  distinct formations — true by construction once the embed landed, so the
  test had become an assertion about the type system. Its surviving coverage
  (the corpus-spelling join, pinned in both directions in `windows/worldgen`)
  was named in the commit message and verified to be independent of
  `Formation`'s shape both before and after.
