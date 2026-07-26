# The Reagent — Retrospective

Process lessons only; the product is in
[the chronicle](../../book/src/chronicle/the-reagent.md).

## What worked

**Mutation-testing every deliverable test, as a standing reviewer instruction.**
Five separate tests passed under mutation of the exact behaviour they existed to
protect: `hue` replaced by a constant; `admits` `.all` → `.any`; `permits`
without `.abs()`; a whole authored table row swapped; and four of five sign
channels replaced by constants. Every one was caught in review, and none would
have been caught by reading the diff. The reviewer that found the `permits` gap
went further than asked and discovered that a **mass-destroying** production
would have passed admissibility, because the only unbalanced test case was
over-balanced — a one-directional test for a two-directional invariant.

The instruction that produced this was cheap: tell the reviewer to *break it*,
and require the fix to be verified by watching the new test fail under the
mutation before seeing it pass. "A fix you have not seen fail is not verified"
turned out to be the single highest-leverage line in the dispatch prompts.

**Implementers self-reporting gaps they were not asked to close.** After the
first review found a mutation gap, later dispatches carried a short note about
it. Two implementers then flagged the same class of gap in their own work —
`admits`' single-requirement blind spot, and the sort/dedup contract — without
being asked and without silently expanding scope. Passing a review finding
*forward* into the next dispatch cost one paragraph and bought two findings.

**Reviewers verifying tool behaviour in the tool's source.** When an implementer
claimed the type-audit tool rejects a tag split across two lines, the reviewer
read `tools/type-audit/src/tag.rs` and confirmed it rather than accepting the
claim. The same reviewer checked that `bare-ok(ratio: return)` was established
convention elsewhere rather than invented. This is the habit the autopilot spec
asks for and it held here.

## What to do differently

**Check that a campaign name is free before using it.** This campaign was named
*The Assay* at G3 and carried that name through spec, plan, five tasks, and
nineteen commits — until the close discovered `book/src/chronicle/the-assay.md`
already existed from a shipped campaign about species potency. Renaming touched
seven files plus the registry. The check is one `ls`, and it belongs in the
ledger entry that picks the name.

Worth noting the preflight could not have caught it: its collision check flags
slugs minted on *both sides* since the merge base, and this name predated the
merge base entirely. A name is an identifier under decision 0026, so the
uniqueness check has to happen at naming time, not at merge time.

**A spec's central mechanism deserves an end-to-end trace before it is
approved.** The provenance confound was designed as grade → causticity →
behaviour, and shipped as grade → causticity → appearance, because no production
in the authored table ever required causticity. The gap was in the spec from the
start and survived G3, a plan, five task reviews, and a whole-branch review; it
was found only when a reviewer asked what the drawn quantity actually reaches.
Tracing each design-critical quantity from its source to its consequence would
have caught it on the page, before any code existed.

**Two of the campaign's own evidence claims were overstated in the spec.** §8
promised that two seeds yield materially different reachable production sets;
in fact default worlds saturate the table and the claim holds only at sparse
pins, one production wide. Writing an evidence claim is easy; the discipline the
project already applies to generated-artifact claims — run it and paste the
output — should extend to "this measurement will show X" before it goes into a
spec.

## Follow-ups

- **Campaign 2's first task, before any practitioner work:** couple grade to
  behaviour with a causticity-gated production, so provenance determines whether
  a recipe works. Recorded in the spec's §2a by owner decision at G6.
- **Before campaign 2 preregisters its hypothesis:** measure an alchemist's
  reach locally (a settlement catchment, not a globe) and use a finer
  heterogeneity measure than production count — distinct material categories, or
  ore-grade dispersion. Recorded in the spec's §8a.
- **Open mutation gap, not closed here:** deleting the entire `substrate_of_life`
  push from `substances_of_world` leaves every test green. The biosphere carry is
  named in the spec but nothing at the world level would notice its removal.
- **Design risk for campaign 2:** `qualities_of` is `pub`, so a practitioner
  could read exact latent truth in one call and collapse the gap the program
  rests on. Nothing does today only because nothing reads qualities at all. The
  fix is an observer type yielding only a `SignVector`, and it belongs in The
  Signature's spec.
