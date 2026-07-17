# Retrospective — The Named (hornvale#1)

One page, process only. Product is in the chronicle
(`book/src/chronicle/the-named.md`) and the spec
(`docs/superpowers/specs/2026-07-16-the-named-design.md`).

## Headline lesson

**A drift check pins output against change. It has no opinion about whether
the output was ever right — so it can hold a bug in place indefinitely, and
report green while it does.**

The defect was visible, in plain English, in
`book/src/gallery/almanac-seed-42.md` — a committed artifact that CI
regenerates and byte-compares on every run:

```
An organized priesthood tends a pantheon:
...
The legion of **Foanjaovaaboenoagoo** keeps its own folk pantheon:
```

One pantheon named, one not, in the project's flagship fixture, for eight
days. The drift check was green throughout, and green was *correct*: the
bytes it regenerated matched the bytes on disk. It did not miss the bug. It
froze it — and every campaign that regenerated artifacts re-ratified the
wrong output as the baseline.

This is a structural blind spot, not an oversight. Byte-identity is
constitutional here and the drift check is how it is enforced, so the
machinery is doing its job. But it means committed artifacts get exactly one
substantive reading — the moment a human looks at the rendered page — and
after that they are only ever compared to themselves. **Anything wrong at
the moment of first commit is invisible forever after.**

**Concrete change to try:** when a campaign rebaselines an artifact, the
review should include reading the changed region as prose and asking whether
it is *right*, not only whether it changed as predicted. This campaign did
do that — every rebaselined lead line was checked against the People
section's goblin flagship — and it is worth making explicit rather than
leaving to conscientiousness.

## The lesson this retro originally recorded, and why it was wrong

The first draft of this page led with: *"a deferred ticket whose trigger is
'when X lands' needs a test that fails when X lands, not a prose note asking
a future campaign to remember."* It was built on a misdiagnosis that
survived the spec, the plan, the implementation, and both task reviews, and
was caught only by the whole-branch review.

The campaign believed the bug was dormant until a third species landed, then
fired when the roster reached four (bugbear sorting ahead of goblin in the
alphabetical registry). The record refutes this:

- `i == 0` landed **2026-07-08** (5589df2, *"one Gods section, two
  pantheons — first block byte-stable"*), when the roster was already
  goblin + kobold. Bugbear landed **2026-07-10**, two days *later*.
- The bug needed only a **second pantheon** — the very feature that commit
  shipped. It was live from that day, on ~90% of seeds (27 of 30 in 1–30),
  including seed 42.
- Seed 2, the campaign's own headline example, disproves the story: bugbear
  does not place there, block zero *is* goblin's, and it rendered stripped
  regardless. Had alphabetical order been the cause, the unattributed
  pantheon would have been bugbear's.

There was no trigger, so there was nothing for a trigger-test to wait for.
The ticket and the Y2-2 retro didn't fail because "prose does not fire" —
they failed because both described the wrong failure mode (*"a world where
goblin places no pantheon"*, a mode that likely never fired at all), and the
conditional framing is exactly what persuaded everyone it hadn't happened
yet. Including this campaign, which inherited the ticket's framing wholesale
and spent its entire analysis confirming a story the git log contradicts.

**The real lesson about deferral:** a ticket that says "benign, do not fix
yet" is an *assertion about the present*, and it decays. This one was wrong
when filed. Re-deriving the claim from the current code cost one afternoon;
trusting it cost eight days of shipped-wrong output. When a deferred ticket
is picked up, the first move is to verify its premise, not its fix
direction — and the campaign should measure the blast radius itself rather
than adopting the ticket's estimate. "Roughly one seed in six" persisted into
the spec and the plan because it was measured against the wrong population
(three-pantheon worlds) — the actual figure is ninety percent, and the tell
was sitting in plain sight the whole time: *all three* seed-42 galleries
changed.

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
4. **Verify a deferred ticket's premise before its fix direction** — see
   "The lesson this retro originally recorded" above. hornvale#1's stale
   "registry-first species (goblin)" text is followup 3; its *"benign (do
   not fix yet)"* status assertion was wrong when filed, which is the more
   expensive error.
5. **Fold type-audit into `make gate`** (or at minimum `make gate-full`) —
   the gate gap above.
6. **Minor, deferred deliberately:**
   `a_lone_pantheon_is_named_when_another_people_placed`'s docstring
   describes a builder-level scenario ("two peoples placed") while its body
   constructs a render-level one. Correct as a render unit test — it proves
   `render` reacts to `attribution` alone, not to block count — but the
   docstring describes the scenario that motivated it rather than what it
   builds.
7. **SKY-25 corrected at close (someone else's row).** The Terminator's
   registry row asserted *"bugbear gets the first pantheon on every seed"*.
   Measured on the merged epoch-v4 tree: bugbear places a flagship on **0 of
   30 seeds**; goblin is the alphabetically-first placer on all 25 that place
   anyone. The row's mechanism (alphabetical registry order gates the
   presiding belief, not dominance) is real and untouched; only the species
   is corrected, in place, per the never-delete-a-row rule. SKY-25 stays
   `raw`/`high` — this campaign did not fix it.
8. **`book/src/SUMMARY.md` on main carried committed conflict markers**
   (from `de7ddbe`, The Terminator's absorb merge) — mdbook would have
   rendered them. Resolved in this campaign's merge because the merge had to
   touch that region anyway. Not caused here; flagged because a broken merge
   reached main and nothing caught it.

## The bugbear inference, made twice, independently, on one day

Worth its own heading because it is the campaign's most transferable finding.
Two campaigns — The Terminator (SKY-25) and this one — independently reasoned:
*the species registry is alphabetical → bugbear sorts first → bugbear is the
one that renders/presides first.* Both shipped it into a durable artifact
(a registry row; a spec, plan, and chronicle). Both were wrong for the same
reason: **bugbear sorts first but never places.** Registry-first is not
placed-first, and the sort order is right there in the code to be read while
the placement outcome requires generating worlds.

The inference is seductive because its premise is verifiable and true, and
the false step is invisible — you never notice you assumed the roster and the
world agree. The counter is cheap and neither of us reached for it: generate
worlds and look. This campaign only caught it because a reviewer read the git
log; the registry row was caught only because the close ran the mandated
grep. Neither would have surfaced from more careful reasoning about the same
evidence.

**Concrete change to try:** any claim of the form "species X is first/
dominant/absent" is a measurement, not a deduction. If a spec asserts one
without a pasted command output, treat it as unverified.

## What went right, briefly

The campaign confirmed the bug was live by generating worlds rather than by
reasoning, verified the two carve-out-adjacent claims — no epoch, no census
regen — instead of assuming them, and checked every rebaselined lead line
against the People section it had to agree with. The gate stayed green
throughout and the AWS carve-out was never invoked.

The reviews are what saved this campaign, and specifically the reviewers who
**ran things instead of reading them**. Task 1's reviewer executed the
type-audit binary and found a malformed tag. Task 2's reviewer reproduced a
clippy failure by temporarily reverting an expression, and read CI's actual
regen step to prove the plan's commands wrong. The whole-branch reviewer
mutation-tested the new tests — finding that one of them was vacuous and
that the campaign's headline rule had no builder-level coverage at all — and
then checked the git log, which is what overturned the root-cause narrative
three documents had already repeated. Every one of those findings was
against text I had written and was confident in. None would have surfaced
from a reviewer who read the diff and agreed with it.
