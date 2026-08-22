# The Stride — retrospective

**Campaign:** a walk input mode for `clients/game` — `Focus::Walk` as the
startup default, arrows and `<`/`>` as movement commands.
**Merged:** pending.

## What went well

The `Focus` enum is exhaustively matched, so growing it from two variants to
three made the compiler enumerate every site that needed a decision. The
plan said so explicitly — "the exhaustive matches this change breaks are the
point" — and the prediction held: no site was missed and none was papered
over with `_`. This is the enum-widening lesson working as intended.

Registry-first paid its rent. The one followup this campaign surfaced
(`map out N` zooms the sim's prose but not the client's plate) was already a
registered row from The Grain, with the mechanism stated more precisely than
this campaign would have stated it. The correct action was to corroborate
the existing row from a new angle, not to mint a second one.

## The defect: a spec that stated two rules for one predicate

The mode-entry paragraph said, in consecutive sentences, that the driver
"splits off the first token … and if it equals `map`" flips focus, and that
the rule is an "exact whole-line match after trim." Those disagree on
`map out 2`. The implementation took the first reading; the test written in
the same commit took the second. The suite went red.

**The cheap part:** it was caught by a test, in the same commit that made
it, at zero cost beyond one red run. That is the good outcome for this class
of defect and worth saying plainly.

**The expensive part is where it came from.** This is the fifth-plus
instance of the standing finding that plan and spec *text* is where defects
originate, not implementer code, and it has a shape worth naming separately:
the two sentences were not a contradiction anyone would notice while
reading, because **both are true of the example given.** The spec's worked
examples were `" map "`, `"map x"` and `"examine map"`. First-token and
whole-line agree on `" map "` and on `"examine map"`, and the one case they
disagree on — an argument form like `map out 2` — appeared in **neither the
spec's examples nor the test's table.** A reader checking the rule against
the examples finds them consistent, because the discriminating case is
absent from both.

**The rule this suggests:** when a spec states a predicate, the examples
must include a case that *fails* it for each candidate reading — not just
cases that pass. An example set that every plausible reading agrees on
proves nothing about which reading was meant, and it is exactly the set an
author writes when they have only one reading in mind.

## The tie was broken by source facts, not by preference

Worth recording because the resolution was cheap and the temptation was to
pick by taste. Two greps settled it:

- `Session::map` takes `&self` and returns `Turn::Out` — **no argument form
  of `map` can move the plate**, so "it drew a chart, so focus it" describes
  something that does not happen.
- `spread::compose` dispatches the plate on `Spatial` every turn regardless
  of what was typed — so submitting `map` is a mode gesture, not a fetch.

And the precedent that made it a citation rather than a judgment call: the
sim already splits bare-from-argument with `rest.is_empty()` on its own
`map` and `eyes` arms, so the client mirrors an existing convention.

## A check found blind, incidentally

`docs_consistency`'s "only The Frontier part may cite the registry" ban is
implemented by `find_registry_id`, which requires an ASCII digit right after
the prefix. It therefore matches only the 403 grandfathered numbered IDs and
is **structurally blind to every slug ID** — the only kind decision 0026
permits for new rows. A chronicle citing a slug ID passes it. The check gets
weaker every time the registry grows, while reading as total. Registered as
`PROC-registry-id-ban-cannot-see-slugs`; the campaign's own chronicle was
rewritten to obey the rule the check could not enforce.

## The close found the stale decision, not the sweep

The book freshness sweep caught The Portolan's stale mode model. What it did
**not** catch was that the shipped-registry row for the typed command line
cited **decision 0159**, whose consequence bullets enumerate two focus states
and name `Esc` as the sole route into the map — both false after this
campaign. That surfaced only at the close checklist's "registry flips" step,
by reading the row's **Where** cell rather than its Idea text.

The lesson is about where staleness hides: a freshness sweep naturally scans
*prose chapters*, and decision records do not read like prose that can go
stale — they are dated and append-only, which makes them feel immune. They
are not. A decision's **consequences** can be falsified by a later campaign
while its **rule** stands, and that is the case that needs an amending record
(0160 here), not a superseding one. Grep `docs/decisions/` for the surface
you changed, not only `book/`.

## A cwd reset put three files in the wrong tree

Writing to memory reset the shell's cwd from the campaign worktree to the
main checkout, silently. The next three edits — decision 0160, the registry
repoint, the digest regeneration — all landed in **main's** working tree, and
the `docs_consistency` run that reported them green tested main's tree rather
than the branch. Caught by a routine `git log` in the worktree showing an
older HEAD than expected.

Two things made it recoverable rather than expensive: main's checkout was
otherwise clean, so the stray edits stood out immediately; and the decision
file was untracked there, so it moved rather than needing reconstruction. The
standing rule (re-anchor cwd before verifying or committing) is not enough on
its own — the reset happened *between* commands, with a one-line notice that
is easy to read past. **The check that actually works is confirming the
expected HEAD in the tree you think you are in, before trusting any gate
result from it.**

## Process: the scratch was gone

The worktree carried no `.superpowers/sdd/` at all when this session picked
the campaign up mid-execution, so the G1/G3 decisions behind the design are
unrecoverable — the ledger here starts at entry #4. This is the known
per-worktree-scratch hazard and it is already a registered row; the new
datum is that it bites *within* a campaign at a session boundary, not only
at teardown. The ledger was reconstructed from the commits and the spec,
which recovers the *what* and loses the *why* and the discarded
alternatives — precisely the half the ledger exists for.

## Follow-ups

- `PROC-registry-id-ban-cannot-see-slugs` — narrow or widen the check
  deliberately, and state its direction in its own doc comment.
- The manual smoke (plan Task 4, step 3) needs a human at a terminal: an
  agent's stdin is at EOF, so `possess` cannot be driven from an agent
  session.
