# Retrospective — The Named (hornvale#1)

One page, process only. Product is in the chronicle
(`book/src/chronicle/the-named.md`) and the spec
(`docs/superpowers/specs/2026-07-16-the-named-design.md`).

## Headline lesson

**A deferred ticket whose trigger is "when X lands" needs a test that fails
when X lands — not a prose note asking a future campaign to remember.**

hornvale#1 did everything right on paper. It analysed the bug correctly,
identified the exact precondition (a third species), prescribed a fix
direction, named the code sites, and closed with an explicit instruction:
*"Link this from the third-species campaign spec so it can't be
forgotten."* The Campaign Y2-2 retrospective independently recorded the
same debt and named its mechanism: *"No pinned seed exercises this path, so
it shipped undetected."*

Then four peopled species landed. No link was made. The bug went live on
roughly one seed in six and stayed live, through several campaigns that
touched adjacent code, until someone asked in passing whether the ticket
was still valid.

Two prose warnings, in two separate documents, both accurate, both
predicting the exact failure — and neither fired, because prose does not
fire. The only artifact that would have fired is a test asserting that
every pantheon in a multi-people world carries attribution. That test costs
about ten lines. It did not exist because the condition to make it fail did
not exist *yet*, and "write the test that will fail later" is not a habit
the process currently has.

**Concrete change to try:** when a campaign defers a bug behind a
precondition, land a test that encodes the precondition and passes
vacuously today — `if roster.len() >= 3 { assert!(...) }`, or an
`#[ignore]`d test whose ignore-reason names the trigger. The roster growing
then turns the note into a failure. If that is impractical, the deferral
should be recorded as an idea-registry row with the trigger as its own
scannable field, not as prose in a retro nobody greps.

## What the process caught, and what it didn't

**Ideonomy earned its keep, and specifically on the "obvious" question.**
The autopilot rule says a precedent-grounded answer is the *input* to a
pass, not a substitute for one. This campaign is direct evidence. Pass 1
re-instantiated the problem in archival cataloguing, surfaced the
distinction *anonymous ≠ unattributed* (anonymity is a positive claim of
unknowability; being unattributed just means nobody printed the name), and
recommended exactly what the ticket prescribed. That recommendation was
wrong. Pass 2 found that the People section had already answered the same
question by cardinality — deliberately, in a doc comment — and overturned
it. Had the campaign stopped at "the ticket already told us the fix," it
would have shipped a Gods section that names goblin two paragraphs below a
People section that doesn't.

The overturn came from the *second* pass on a question that felt settled
after the first. Convergence means a pass that returns nothing, and the
first pass returning something confident is not that.

**The reviewers measured instead of agreeing.** Both task reviewers ran the
tools rather than reading the diff and nodding. Task 1's reviewer ran the
type-audit binary and found a malformed tag; Task 2's reviewer reproduced a
clippy failure by temporarily reverting an expression, and read CI's actual
"Artifacts are current" step to check the regen commands. Both findings
were against *the plan's own text*, not the implementer's work.

**The plan was the weakest artifact in the campaign.** Two of the three
defects found during execution originated in plan text I wrote, and both
were invented detail presented with the same confidence as the verified
parts:

- a `type-audit:` tag naming a position (`species leg of return`) the tool
  does not have — the tool names return positions literally `return`;
- Step 12's artifact-regeneration commands, which did not parse and mapped
  the wrong flags to the wrong gallery files.

Both were in code blocks. The writing-plans skill's "no placeholders" rule
pushes hard toward writing complete, concrete code — which is right — but
concrete-and-wrong reads exactly like concrete-and-right to an implementer
told the brief carries "the exact values to use verbatim." The implementer
followed the bad tag verbatim, as instructed. The Task 2 implementer did
*not* follow the bad commands, because it checked them against
`scripts/regenerate-artifacts.sh` and found them wrong — the better
outcome, arrived at by ignoring the instruction to use the brief's values
verbatim.

**Concrete change to try:** in a plan, distinguish values *verified against
the repo* from values *composed while writing the plan*. Anything in the
second class either gets verified before the plan is committed, or gets
marked as "confirm against <source> before using." Commands that invoke
project tooling should cite the script or CI step that owns them rather
than being retyped from memory.

## Gap found in the gate

`make gate` — the commit gate — does not run `tools/type-audit`. It is a
separate CI step, and CI is manual-only (decision 0042). A malformed
type-audit tag passed `cargo fmt`, `cargo clippy -D warnings`, and the full
`cargo nextest` suite; only an explicit run of the tool caught it. Any
campaign that adds a `pub` boundary can ship a broken tag and see green
locally.

See followup 5. This is a real hole, not a nit, and it bit this campaign
twice. Besides the malformed tag, the committed audit report
(`docs/audits/type-audit-report.md`) went stale — adding one `pub` function
moves its counts, and nothing in `make gate` notices. It surfaced only
because the close ran CI's artifact-drift step by hand. Both failures are
the same shape: the type-audit tool is the authority on a committed
artifact, and the commit gate cannot see it.

## Follow-ups

Carried from `.superpowers/sdd/followups.md`:

1. **The no-cult-form lead hole.** `render`'s unattributed branch emits its
   lead only when `cult_form` is `Some`; a legacy block with no recorded
   cult form renders its beliefs under no lead at all, while attributed
   blocks always emit one. Preserved deliberately — it is the legacy
   byte-identity contract — and now covered by a test that names it as
   intentional rather than leaving it to be rediscovered as a bug.
2. **Retire the anonymous branch with pre-species saves.** The legacy
   fallback serves only saves predating `peopled-by` facts. When those stop
   being supported, the anonymous lead and this entire class of
   positional-attribution bug retire with it.
3. **hornvale#1's text is stale.** It says the block-0 owner is "the
   registry-first species (goblin)". The registry is alphabetical
   (`domains/species/src/lib.rs:783`) and bugbear now sorts first. Correct
   this when closing the ticket.
4. **The deferral mechanism itself** — the headline lesson above.
5. **Fold type-audit into `make gate`** (or at minimum `make gate-full`) —
   the gate gap above.
6. **Minor, deferred deliberately:**
   `a_lone_pantheon_is_named_when_another_people_placed`'s docstring
   describes a builder-level scenario ("two peoples placed") while its body
   constructs a render-level one. Correct as a render unit test — it proves
   `render` reacts to `attribution` alone, not to block count — but the
   docstring describes the scenario that motivated it rather than what it
   builds.

## What went right, briefly

The campaign found the bug live before writing a line of code (sampling
seeds, not reasoning), verified the two carve-out-adjacent claims — no
epoch, no census regen — rather than assuming them, and shipped a
three-line artifact diff whose every changed line was checked against the
People section it had to agree with. The gate stayed green throughout and
the AWS carve-out was never invoked.
