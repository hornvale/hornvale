# The Tableau — retrospective

Process, not product. The chronicle carries what was built.

## A controller's own spec became a constraint nobody re-examined

The Company's spec said *scenes are found, never staged*. One campaign later
that sentence was reported to the project owner as a fact about the **world** —
that Hornvale would not put a drow and a goblin in a room — when it was a fact
about a document the controller had written the day before and the owner had
approved on the controller's recommendation.

Nothing in the workflow catches this. A spec is reviewed once, at G3, against
the campaign it governs; it is never re-read as a *constraint* by the campaign
that inherits it. The Company did not question the rule because the rule was
the premise it had been handed, and the controller did not flag it because the
controller had written it and no longer saw it as a choice.

**The tell, in hindsight, was in the language.** "The world declines to put a
drow and a goblin in a room" attributes to the simulation a refusal that lived
in a markdown file. Any sentence that says *the world will not* deserves the
question *or is it that we decided not to?*

## A durable record reads as settled whether or not it was

`SOC-one-creature-per-settlement` is a scale compromise — one simulated
creature per settlement so the world does not carry a billion — and it was read
as a ruling on what the world may contain.

The decision log is append-only and supersedes rather than edits, which is
correct and which also means every record wears the same clothes. There is no
way to write *this is a stopgap pending cost* and have a later reader see it.
Recorded as its own registry row; this campaign does not fix it.

## Three plan-text defects, and the pattern is now specific

1. **The chamber seam named the wrong object.** The plan said inject an
   `Interior` (a room's furniture); shadowcasting runs on the `Lattice` (the
   cells). Written from outside the code, and the name was plausible.
2. **The type audit fired where it was not predicted.** The Company's plan
   guessed `PresentEntry.carrying`; this one guessed nothing and was surprised
   three times — `from_json`'s `text`, `with_thing`'s `held_by`,
   `derive_staged_npcs`'s `cast`, and then a *stale* tag on `kind`, which is
   `impl Into<String>` and not a bare primitive at all.
3. **Absent-versus-empty was specified and turned out vacuous.** Spec section 5
   required the distinction be preserved; section 5's own rule made both mean
   empty for every statable layer, leaving no third state to differ into.

The pattern across all three, and across The Company's four: **plan text names
objects the author has not opened.** The remedy that keeps working is not more
review — it is that every plan step demands executable proof, and the
implementer is free to override the plan and say so.

## Grep the observable, not the function you opened

`examine goblin` did not show the key after the fix that should have made it.
The edit was correct and in the wrong one of three call sites — `examine`,
the chart marks, and the focalized nouns all render a creature, and only one
had been taught.

This is a *known* defect shape in this repository: "examine had two matchers;
teaching one never taught the other, twice." It was known, written down, and it
still cost a debugging round, because the way it presents is a passing test and
an unchanged output. What broke the loop was checking the **snapshot** — the
data was right, so the fault had to be in a renderer, which narrowed it in one
step. All three now share `thing::carried_nouns`.

## Inserting above a function detaches its doc comment. Twice.

`place_creature_at_me` lost its doc comment to a new method inserted between
the two, and hours later `held_by` lost its own the same way. Both surfaced as
`missing_docs` warnings rather than as anything about the real change.

The first time was a mistake. The second time, in the same session, with the
lesson already written down, is the finding: a scripted insertion anchored on
`pub fn NAME` lands *inside* the previous item's documentation, and the
anchor reads as obviously correct. Anchor on the end of the preceding body
instead, or insert and then verify the doc is still attached.

The first instance had a silver lining worth recording: the displaced comment
was how the campaign discovered that its headline measurement had already been
taken by The Hand and written down.

## Scoping a test run hides its consumers

`cargo nextest -p hornvale-vessel` was green while `cli` did not compile: nine
`PossessOpts` literals needed the new field, seven in vessel and two in cli
using shorthand `target,` that the first patch's regex missed. Only a workspace
clippy found them. Adding a field to a widely-constructed struct is a
workspace-scope event even when the struct lives in one crate.

## What was deferred, and the reason recorded

The chamber seam. Not for cost — because it was specified against the wrong
object, and correcting it opens a real fork (a lattice supplied wholesale
versus constraints an embedder honours) that deserves a deliberate answer.
A half-injected chamber that worked for simple cases would look like the
feature without being it.

## Followups

- **A day-parameterised placement seam.** `world_where_an_unsensed_creature_arrives`
  costs 233.72 s, fails on all 64 seeds, and leaves `!wait`'s arrival narration
  with no witness of any kind. Its own ignore reason names the remedy, and a
  tableau is most of it — what is missing is the day parameter.
- **The chamber/lattice seam**, sized against shadowcasting.
- **`world_that_draws_a_creature` is dead** — 0 live call sites, and it builds
  up to 64 worlds when called. Delete or convert.
- **Wild labels carry an article** and the prose prepends its own determiner,
  so `needs` reads "The a wild carrion-crawler looks lost" on main today.
  Pre-existing; blast radius is committed goldens and book galleries.
