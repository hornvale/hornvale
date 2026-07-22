# Retrospective — The Standing Offer (PROC-12)

One page of process, not product. The product is chronicled; this is what
the close learned that the code does not record.

## What went well

- **The tree-finding ideonomy pass found two real, load-bearing
  exclusions the initial two-tier framing missed, before any code was
  written.** The original registry idea named two mechanisms (union
  merge; a custom regenerate driver) without mapping which files needed
  which. Walking the tree of "generated + committed" down and across
  found that census data isn't append-only (so union is wrong for it)
  AND is authorization-gated by this session's own standing policy (so
  auto-regenerating it would violate that policy outright), and — more
  importantly — that golden/keystone test fixtures must never be
  auto-regenerated on conflict at all, because a conflict on one of
  those is the entire signal the fixture exists to produce. Both
  exclusions were found by research, not discovered the hard way in
  execution.
- **A `.gitattributes`-driven mechanism, once merged, protects every
  future campaign automatically** — no session needs to remember it
  exists; git resolves the driver from the repo state itself the moment
  two branches with a real generated-artifact conflict get merged. This
  is a different shape of payoff than most campaigns tonight: not a new
  capability the world can be observed doing, but a standing structural
  guard against a specific, previously-manual failure mode this exact
  session hit twice (closing PROC-18 and PROC-19).

## What was awkward — a pattern that recurred a THIRD and FOURTH time tonight

- **The integration test's own implementer found two real bugs in the
  plan's test design, neither silently patched.** First: the plan's
  "break the build" setup for the failure-path scenario modified a
  source file on only one of two test branches, so git auto-merged it
  cleanly and the build check ran against still-working code — the test
  never actually exercised the failure path it claimed to. Second, after
  fixing that: the plan's assertion for what a failed driver leaves
  behind (literal `<<<<<<<` diff3 markers) was itself wrong — verified
  empirically that git does NOT write textual markers for a path
  governed by a custom `merge=<driver>` attribute on failure; it leaves
  the path unmerged in the index with the pre-merge "ours" content, a
  materially different on-disk shape than git's default undriven
  fallback. Both were caught, both were fixed, both are correct now — but
  count them: this is the **third and fourth instance in three
  consecutive campaigns tonight** of "a claim about what a generated
  artifact or a tool's behavior will look like was asserted in a spec or
  plan before being checked against the actual output," after
  [[the-single-saying-campaign]] (a manifest-diff claim) and
  [[the-compound-word-campaign]] (the SAME claim's own correction being
  incomplete). That's not a coincidence worth attributing to any one
  campaign's carelessness anymore — it's a real gap in how specs and
  plans get written in this project, at least under the current process.
- **The final whole-branch review caught two MORE instances of exactly
  this same pattern**, after both prior corrections had already landed:
  the spec's own embedded code snippet had drifted from the actually-
  shipped script (a stale `repo_root` derivation left over from before a
  mid-execution edit), and the test script's own prose (header comment,
  echo lines, pass message) still said "falls back to conflict markers"
  in three places even after the ASSERTION two lines away had been
  corrected to check the right thing. **This means the pattern recurs
  not just in first-draft specs, but in the CORRECTIONS to specs** — a
  narrower, sharper version of the same lesson: when you fix a claim in
  one place, grep the whole document set for every OTHER place the same
  wrong claim was restated in prose, not just the one assertion that
  happened to fail a test.

## Follow-ups

- **A concrete, actionable proposal, not just "be more careful"**: a
  spec-review or plan-review checklist item — "does every claim about
  generated output, tool behavior, or a diff's scope in this document
  have a command next to it that was actually run to produce that
  claim?" — would have caught at least three of tonight's four
  instances before a subagent had to find them empirically. This is
  worth a real proposal (possibly its own small PROC idea) rather than
  another memory note repeating the same lesson a fifth time next
  session.
- Gallery artifacts needing an ephemeral built world (Tier B's natural
  extension) remain banked, not built — see spec §3.2 and decision
  ledger #4, matching this session's now-well-established small-campaign
  discipline.
