# The Gossan — retrospective

Rung 1 of the Underworld Larder. Process lessons, not product; the product is
in [the chronicle](../../book/src/chronicle/the-gossan.md).

## The headline: six defects, all in plan and spec text, none in implementer code

Every defect this campaign produced was in **my own prose** — the plan, the
spec, or a dispatch brief. Not one was in code an implementer wrote. That is
the same distribution as The Quire and The Benchmark, and it is now the
project's most reliable prediction about where a campaign will go wrong.

| # | Defect | Caught by |
|---|---|---|
| 1 | Task 3's test module specified inside `mod tests`, where `use super::*` is a private glob and the import cannot resolve | pre-verifying one task ahead |
| 2 | Blast radius reported as ~100 sites / 12 files; actually 221 / 34 / 7 crates | pre-verifying one task ahead |
| 3 | `is_ametabolic` specified to take both axes — a signature with **zero possible callers** | Task 4's landing |
| 4 | Task 5's commit-message template contradicting its own Step 5 | the implementer, who flagged rather than chose |
| 5 | Spec §8 lists a docs/registry task the seven-task plan never had | the final whole-branch review |
| 6 | "every read site feeds allometry" — a parenthetical asserting a property of a population without measuring it | the final whole-branch review |

Three of the six are the same shape: **an imperative with an assertion hidden
inside it**. "Add at the end of the existing test module" asserts the import
will resolve there. "Delete `MetabolicClass`" asserts nothing depends on it.
"Every read site feeds allometry" asserts a completeness nobody counted. The
campaign-autopilot skill already names this failure and I produced three fresh
instances of it underneath that warning.

## What actually caught things, ranked by yield

**1. Verifying one task's brief against the code immediately before dispatching
it.** Two defects, both structural, both invisible from inside the task that
would have hit them. The value comes from the timing: checking a brief against
the tree the implementer will *actually find*, one task ahead, is a different
and much easier job than checking five briefs at plan-authoring time against
code nobody has opened yet.

**2. Reviewers asked a specific question rather than "review this".** Every
review in this campaign got named questions, and the yield tracked how sharp
they were. The single most valuable finding — `prey_pressure_from` asking the
thermal axis a trophic question — came from asking "did the bijection hold at
*every* site?" rather than "does this look right". The structural answer to
"does the pair table really read the trophic axis" (no two sanctioned rows
share a thermal key, so a trophic-only deviation cannot match another row) was
better than the empirical answer I asked for.

**3. Mutation controls, every time, with the failure message pasted.** Four
tasks carried one. Each was the difference between "the test passes" and "the
test can fail". Task 2's assertions described behaviour that already held —
a green run proved nothing until the mutation reddened it by name.

## A tooling trap worth carrying forward

**A `cargo check` that fails early has enumerated nothing.** Two probes ran in
the same session with the same command. One added an enum variant and reached a
clean workspace; its figure (3 match arms) was sound. One split a struct field,
reported *"14 errors remaining in 5 files"*, and I read that as a total. It was
the first wave — a compile error in an early crate means every downstream crate
is never checked at all. The real number was 221 sites across 34 files,
including an entire crate (`windows/hearsay`, 11 files) and a third struct
carrier nobody had found.

**Nothing in the output distinguished the two runs.** Size a refactor with
`git grep -c`, which cannot fail early, or iterate the compile loop to zero
before quoting a count. Task 4's implementer needed five passes to reach zero,
each surfacing a crate the previous failure had masked — direct confirmation
from the one place it could be observed.

## Two corrections that arrived with better reasons than mine

**The three-carrier asymmetry.** `BiosphereTraits` and `PeopleTraits` carry
both axes; `Body` carries only the thermal one. The implementer justified it as
"there is no pair guard at that layer" — but that applies equally to
`PeopleTraits`, so it does not separate them. The reviewer supplied the argument
that does: `PeopleTraits` is a 1:1 **mirror** that copies the species traits
wholesale, so carrying both is faithfulness to its role; `Body` is a narrow
**projection** that carries what it reads. Not a defect; it tracks a real
distinction.

**The census schema.** My findings file warned that census `schema.json` files
would keep stale text until a census run. The fixer read the regeneration path
and found the opposite — `regenerate-artifacts.sh` re-derives census *schemas*
from the live metric registry unconditionally, outside the census gate, because
building no world costs nothing. My reading of the render path was right and my
conclusion was wrong.

## A claim that survived two refutations

An implementer reported `docs/audits/type-audit-report.md` as stale in Task 4,
and again in its fix round, after a reviewer and I had each independently
regenerated and diffed it and found nothing. It took **four** runs of the same
command to retire. Then in Task 5 the same claim was *correct*, for a real new
`pub fn` tag — and that implementer reached it from an actual gate failure
rather than by reasoning.

Same words, opposite truth value. The difference was never confidence; it was
whether a command had been run.

## What the whole-branch review saw that no task review could

Both of these were clean, and neither is visible inside a single task's diff:

- **The out-of-workspace clients.** The branch renames a `pub` field on a type
  two client crates depend on, and *no local rung builds them* — the repo's own
  documented blind spot. All four build clean.
- **Adjacent damage.** It diffed the kind table at both ends with the metabolic
  lines stripped from each side, proving no other field on any of the 39 kinds
  moved. That class of defect is invisible to both campaign instruments and no
  control looks for it.

It also found that **the spec over-generalised its own vacuity argument**. §5.1
correctly showed the *life-history* half of the artifact surface is blind to
this change, then concluded the artifacts as a whole were — which is false, and
cost the campaign an instrument it already had. The committed seed-42 session
fixtures cover `prey_pressure_from`, the trophic axis's only production reader,
and neither campaign instrument touches them.

## The merge is where a clean auto-merge lied, twice in two days

Absorbing 32 commits of `main` at the close produced a **duplicate registry
row** with no conflict marker: my correction on one line, main's rename on the
other, both "added". Caught by `registry_ids_are_unique`, not by git. This is
the second time in two days — the idea registry is a hand-edited table, so the
never-text-merge-an-aggregate habit does not fire for it.

The same merge conflicted on `docs/audits/type-audit-report.md`, which **must
be regenerated, never text-merged**. The regenerated file differs from *both*
sides by two lines, so a text resolution would have shipped an aggregate that
is a valid merge of neither and that no test reads.

## Deferred, with homes

- The two `rise_at` assertions for `Absent` are logically redundant under the
  current implementation (the arm does not branch on temperature). By design,
  matching the endotherm/ectotherm pattern; recorded so a later auditor does
  not read it as an oversight.
- 13 subfloor-roster keys still encode the old vocabulary. **Left alone
  deliberately** — the roster selects by exact test name, and a rename silently
  drops a test from the commit gate.
- A 117-character unwrapped doc line in `windows/worldgen/src/knownness.rs`.
  Cosmetic; rustfmt does not object.
- The Menagerie provenance clause and the attribution clause did not survive
  the move onto `Unmodelled`'s doc. The first landed in
  `metabolic_pairs.rs`'s rot account; the second survives in spec §7.

## For the next rung

The pair table's guard rests on a property the final review had to
*demonstrate* rather than read: it pins trophic-given-thermal only because no
two sanctioned rows share a thermal key. Rung 2's success condition — adding a
`Chemotrophic` row — is exactly the edit that breaks it, and every other guard
stayed green in the demonstration. There is now a test asserting that
distinctness whose failure message tells rung 2 what it is breaking and to
replace it with a per-kind pin rather than delete it.

And the question rung 3 depends on is still unmeasured: lithology's six axes
vary *within* a world, and nobody has measured whether they vary *between*
worlds. Rung 3's entire "not every world is the same apocalypse" promise rests
on the second statistic. This campaign's parent line made exactly that mistake
about ore prospectivity and found 75% of all land inside a band 0.0067 wide.
