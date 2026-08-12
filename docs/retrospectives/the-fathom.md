# The Fathom — retrospective

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-fathom.md): a column accessor
that was free to build, a duplicate of it found one crate over, and two
preregistered measurements — one confirmed strongly, one falsified on a
threshold nobody had checked against a real distribution.

## The count: seven times, a document asserted something about code it had not read

Every one of the seven was caught by running a command — grep, a compiler
error, a test failure, a second reading of the same line. None was caught by
re-reading the prose that made the claim.

1. **The spec named a new accessor that already existed.** The first draft
   proposed `biome_expr_at(cell, stratum)`. `GeneratedClimate::biome_expr_at
   (cell)` already existed, one argument short, at `provider.rs:481` — the
   struct even stored *both* `biome` and `biome_expr` maps already. The draft
   would have been a breaking signature change to a public function it never
   looked up. Caught while writing the plan, before any code existed.
2. **The spec called four sites live that were mostly latent.** The first
   draft of §3 named four call sites as re-key targets. Three read a cell's
   *surface projection* (`CellMap<Biome>`), which stays well-posed under
   §2's architecture forever — the underworld is a stratum *beneath* a cell,
   never a cell's own biome. Writing re-key tasks for them would have
   produced three no-ops. Caught by reading what each site actually
   dereferenced, not by re-deriving the claim from its own prior sentence.
3. **The plan told an implementer to put tests in a place that does not
   compile from where they'd run.** It said the column tests belonged in
   `provider.rs`'s in-module `#[cfg(test)] mod tests` block, next to the
   crate's own fixture helper. That helper is `#[cfg(test)]`-gated and
   therefore invisible to an integration test, which links a separately
   compiled, non-`cfg(test)` copy of the library. An integration test needed
   its own fixture.
4. **The controller's own mid-campaign correction repeated the same
   mistake, in the same file, on the same unread line.** Told that
   `test_support` was a `pub mod` (true) and therefore reachable from an
   integration test (false — the `#[cfg(test)]` attribute sits on the line
   *above* the `pub mod` line, invisible to a grep for `pub mod`), the
   correction asserted the opposite of #3 with equal confidence. The
   implementer overrode it, correctly, having actually read the attribute.
5. **The plan wrote two custom `heavy:` ignore-tag reasons where the tier
   requires one canonical string, verbatim.** The heavy-tier scanner matches
   token-for-token; a paraphrase that says the same thing in different words
   is invisible to it, not merely wrong.
6. **The plan ran a test suite against a crate excluded from the cargo
   workspace.** `-p hornvale-game-core` fails with "package not found," not
   a red test — `make game-check` is that crate's actual gate, and the plan
   had not checked which gate a claim about that crate needs.
7. **The deepest one: neither the spec nor the plan ever grepped for whether
   the column already existed.** The campaign's entire premise — "the
   accessor is behind the type, and nothing derives the column" — was true
   of the one function the spec named and false of the project as a whole.
   `windows/locale/src/lib.rs` had shipped `water_column_at` and
   `expr_at_stratum` before this campaign opened, doing the identical
   derivation under the identical doc-comment argument. See §1 below.

Seven is a lot for a campaign this small, and the shape is not new — it is
the same failure mode this project's retrospectives keep naming (see
`defects-originate-in-plan-text` in the operator's own memory index). What
is worth adding here is the *texture*: three of these seven (#3, #4, #6) are
not architectural claims at all, they are claims about which directory a
file lives in or which tool builds it — the cheapest possible thing to
verify and the easiest to skip because it feels beneath verifying.

## 1. A duplicate one crate over, invisible to the review that should have caught it

This is the finding worth the most, because it is not a mistake anyone made
carelessly — it is a mistake the campaign's own safety mechanism was
structurally unable to see.

Task 1 built `strata_at`/`biome_expr_at_stratum` on `GeneratedClimate`,
proved them consistent with the existing single-value accessor at every
cell, and shipped with a clean review. The review checked the diff against
the spec, against the plan, and against the crate's own tests. It could not
have checked the diff against `windows/locale/src/lib.rs`, because nothing
in the diff touched that file — and a duplicate implementation living
somewhere the diff never goes is exactly the shape a diff-scoped review
cannot see. It surfaced only because Task 3's pre-flight step happened to
read the neighboring window before writing the delegation task, on a hunch
rather than a rule.

**The generalizable version:** before adding a derivation of something the
type system already carries the raw material for, grep the *consumers* of
that raw material, not just its producer. `BiomeExpr` is produced in one
crate (`hornvale-climate`) and consumed in several (`hornvale-locale`,
`hornvale-worldgen`, `hornvale-vessel`); this campaign grepped the producer
exhaustively and the consumers not at all. A "does this already exist"
check scoped to the crate being edited is not the same question as "does
this already exist," and the difference cost a duplicate implementation
that a clean review could not catch.

The resolution — delegate rather than delete either — is worth naming as
the right shape for next time this happens: the newer, more general
function became the single source of truth, and the older function's exact
contract (including a fallback this campaign independently found to be
wrong) was preserved byte-for-byte behind it, because fixing that fallback
would have moved client-facing output and this campaign's only currency was
not moving anything.

## 2. A preregistration bundled four failure meanings under one stop condition

H-1's four clauses do not mean the same kind of thing if they fail. Clause 3
(single-rung share) failing means the *threshold* might be miscalibrated —
which is exactly what happened, diagnosed after the fact as an unmeasured
guess against Earth's own continental-shelf fraction. Clause 4 (no height
over 90%) failing would have meant something categorically worse — a
depth field so flat it carries no information at all. The plan wrote one
stop condition for all four, so a miscalibrated-but-informative measurement
(clause 3) triggered the identical halt a genuinely degenerate world (clause
4) would have.

It happened to cost nothing here — the controller correctly recognized
clause 3's failure as a threshold problem rather than a world problem, and
the campaign was not derailed. But that recognition depended on a human
reading the printed distribution and reasoning about it after the freeze,
which is exactly the step preregistration exists to make unnecessary. **The
generalizable rule:** clauses that would mean different things if falsified
need different stop rules — a hard halt for the clause whose failure would
indicate a broken world, a preregistered-not-met carry-forward for the
clause whose failure would indicate a bad ceiling. Write that difference
into the preregistration itself, not into the after-the-fact reading of
which kind of failure this one turned out to be.

## 3. A guard that would have been vacuous shipped for one review cycle

Task 4's fix collapsed a two-line `#[ignore]` reason (joined by a `\`
continuation) to one line, because `cli/tests/heavy_tier.rs`'s
`ignore_reasons()` scanner is line-by-line and same-line-only: it matches
`#[ignore = "..."]` as a single token on a single line, and a reason that
spans two source lines is invisible to it. The untokenised-deferral guard
this test exists to keep honest was therefore passing **vacuously** —
proving nothing about the reason it was supposedly checking, for as long as
the reason spanned two lines.

Caught in review, fixed, and — the part worth keeping — the fix was
verified the same way this project verifies every guard fix: mutate the
live reason, watch the specific assertion go red, revert. A guard that
merely *looks* fixed and a guard that has been *proven* to fire are
different claims, and only a mutation closes the gap between them. This is
the same discipline `a-guard-a-comment-can-satisfy` and
`a-mutation-proves-only-what-it-perturbs` already name in the project's own
process memory — this campaign is one more instance rather than a new
lesson, and is recorded here because it is the instance where the fix and
its verification both happened inside the campaign that shipped the bug,
not in a later one that discovered it.

## 4. What held up well

Two things worked and are worth stating so they are not lost among the
findings above.

**The byte-identity discipline held with zero drift, including in the parts
that were not directly tested for it.** `make rebaseline` at close produced
an empty diff across every committed artifact path — the seed-42 world, the
gallery, the reference dumps, the domesday survey, the digest, and the
client-facing session fixtures — with no exception needed even in
`docs/audits/`, because the type-audit report had already been regenerated
in the same commit that introduced the new `pub` functions, per the
project's own standing rule. A campaign whose entire premise is "this
changes no world bytes" is only as credible as the artifact diff that backs
it, and this one backs it cleanly.

**Refusing to retune after falsification, twice, in the same campaign.**
H-1's clause 3 measured 5.85% against a <5% ceiling and the ceiling was not
moved — it is carried as `PREREGISTERED, not met`, with the diagnosis
(shelf fraction, not a broken world) recorded rather than smuggled in as a
threshold change. Nathan's own ruling on it states the project's operating
principle plainly: a threshold moved after unblinding is worth less than a
falsification kept on the record. The reviewer of Task 2 independently
recomputed the H-1/H-2 arithmetic and confirmed the 5% figure was original,
not adjusted — an independent check on the "nothing was retuned" claim,
not just a repetition of it.

## Follow-ups

Ten items were found by reading during the brainstorm that opened this
program and were deliberately not acted on — each is separable from The
Fathom itself, and each was posted to the board (`notice a720436e`) before
this campaign began. Promoted here before the worktree that held them is
torn down.

- **F-1 — a shipped guard is vacuous against the change campaign 2 must
  make.** `windows/worldgen/tests/deep_realm_chamber.rs`'s
  `an_addresss_meaning_does_not_depend_on_which_other_chambers_exist` varies
  depth only, holding cave kind fixed, while the invariant it guards is
  documented more broadly (content is a pure function of `(addr,
  overrides)`, never of the gating `Cave`). A change making chamber content
  read `CaveKind` would leave this test green and proving nothing. Campaign
  2 must refine the invariant — chamber content may depend on the cave's
  kind, never on its depth budget — and add same-kind/different-kind
  positive controls the current fixtures cannot provide.
- **F-2 — `SoilOrder` is derived and unread by the biome path.**
  `domains/terrain/src/lithology.rs:727` derives WRB soil orders from parent
  rock, temperature, moisture, slope, and depth; nothing in the biome path
  reads it. This is the edaphic axis, already computed and unused — Campaign
  1's substrate axis should read it rather than invent one.
- **F-3 — the overworld has one stratum; the sea and rock have five each.**
  `Medium::AirOverRock => &[Surface]` against five-rung ladders for water and
  rock. Canopy, cliff face, and air are unexpressible — a skyworld is not a
  new realm, it is the unbuilt upper strata of the one that exists, so
  building it once would serve both a canopy layer and an air realm. Out of
  this program's near-term scope; new content breaks a byte-identity proof.
- **F-4 — `Realm::strata()` keys on medium, not on the realm.**
  `facets.rs:91`. An underground sea (`Realm { medium: Water, access: Dive }`
  at rock depths) collides with `WATERWORLD` and is handed the pelagic
  ladder — told it is 200 m below a sunlit surface that is not there. Small
  fix (key on the realm instead); belongs to whichever campaign first needs
  a second realm at the same medium.
- **F-5 — `ChamberOrigin::Made` has no writer.** `chamber.rs` defines
  `Found`/`Made` with an absorbing rule and a resolver; nothing in the
  workspace ever produces `Made`, and both live `ChamberOverrides` consumers
  construct an empty map. The *made* world — hall, mine, sewer, vault — has
  a type and no producer. Campaign 4's to pick up.
- **F-6 — a settlement commits the biome of the wilderness it replaced.**
  `domains/settlement/src/lib.rs:28` registers `biome` as "biome of a
  place"; nothing in `domains/climate` classifies a settled cell
  differently, so a city in a temperate forest reports `temperate-forest`
  forever. There is no anthropogenic community anywhere in the taxonomy —
  the same defect shape as the underworld's missing communities: the
  process exists in the ledger, the community it produces has no name.
  Campaign 1 (as axis values) and campaign 4 (as ruins).
- **F-7 — the underworld's variant pool is empty, with a documented "not
  yet."** `variants.rs:733` names the missing thing directly: cave
  formations get a variant pool once something distinguishes karst/lava-
  tube/fracture interiors by prose. Campaign 2 is that something.
- **F-8 — `MicroField`'s four room-grain axes consult the world for
  nothing.** `windows/locale/src/micro.rs`'s `relief`, `aspect`, `wetness`,
  `openness` are each a pure noise draw. The Rill's Task 5 is grounding
  `wetness` now; `openness` is canopy closure and `aspect` drives light, so
  two more are ungrounded axes of the coming vocabulary. Grounding one must
  keep its draw and spend it as variation about the grounded value — The
  Rill's own task text names removing a draw as a save-format break, not a
  style choice.
- **F-9 — the column's `.expect()` invariant is by-inspection, not
  by-type.** `strata_at` and `biome_expr_at_stratum` both `.expect()` that a
  cell's stratum is always on its own realm's ladder. True today because
  nothing constructs an `UNDERDARK` expression yet. Campaign 2 is the first
  thing that will, and if it ever pairs a realm with a rung from another
  realm's ladder, these panic. Not introduced by this campaign — the brief
  authored the invariant — but campaign 2's to discharge, ideally by making
  the mispairing unconstructible rather than by widening the `.expect()`.
- **F-10 — `LocaleContext::expr_at_stratum`'s below-floor fallback answers
  open water for solid rock, and the tie-break that would make it reachable
  is not provably absent.** Preserved byte-for-byte on purpose (fixing it
  moves behaviour, forbidden by this campaign's whole claim). Reachability
  verdict from Task 3's review: the below-floor stratum reaching a live
  `describe_*` call would require `windows/vessel/src/session.rs`'s
  `column_here()` — the only source of a possessed session's `submerged`
  stratum — to resolve a *different* cell than `windows/locale`'s own
  `dominant_corner`, which the two live `describe_*` callers use.
  `column_here()` tie-breaks via `max_by_key` (last-element-wins);
  `dominant_corner` tie-breaks to the lowest `CellId`. Both read the same
  three `(CellId, weight)` corners in the same vertex order
  (`RoomAddr::corner_weights` does not sort them), so on an exact
  corner-weight tie between two of the three corners, the two selections are
  **not provably identical** — one could resolve a rung valid in its own
  cell's column but below the other cell's floor. No test in the workspace
  currently exercises this tie case; the fallback's reachability rests on
  this code-reading argument, not a captured failing world. Candidate fix
  for whichever campaign takes this: unify the two tie-break rules, then
  decide what a genuinely below-floor query should answer (likely a refusal
  narrated as solid rock, not open water).

A concrete, already-traced item sits beside these ten and is not a
follow-up so much as a landmine marked for the next campaign to step around
rather than into: a rock stratum reaching `expr_at_stratum`'s preserved
fallback produces `BiomeExpr { formation: OpenWater, .. }` at a rock rung,
and `BiomeExpr::biome()` declares a cave formation `unreachable!()` by
design. Nothing constructs that value today. Campaign 2 — the first thing
that gives the underworld a community — will be the first thing that can,
confirmed by two independent traces during this campaign's own review.
