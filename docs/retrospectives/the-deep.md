# The Deep — retrospective

**Completed:** 2026-07-22 (slug-named per decision 0026; spec
`docs/superpowers/specs/2026-07-22-the-deep-design.md`, plan
`docs/superpowers/plans/2026-07-22-the-deep.md`, seven tasks: geothermal base,
the column + era archive, the purity guard, the map lens, census metrics, the
almanac section, close). Campaign 1 of the subsurface arc (registry MAP-10 /
DOM-14). Ran under campaign-autopilot.

**Ideonomy earned its keep on the "obvious" answers, not the hard ones.** Eight
passes ran across the brainstorm — four on the spatial spine, two on
fantasy-geology, one breadth-atlas, one on domain placement. None *reversed*
the leading recommendation on the spine; every one *enriched* it, which is the
documented point of running a pass on a settled answer. The three findings that
paid for all eight: the reframe of "column vs. fields vs. graph" from a
three-way choice into three rungs of one coarse→fine ladder (the C+ form we
shipped); the cross-domain re-instantiation into immunology that turned the
Lovecraftian "sealed thing" from a mood into a mechanism (a maintained
containment field with an upkeep cost — the same shape as the memory economy);
and the breadth pass's recognition that geothermal *is* the deep's inner sun,
which is why v1 exposes it as an energy base rather than a rock temperature.

**The one overturn came late, at plan time, and the process caught it.** During
the brainstorm I recommended a new `subsurface` domain. Studying the near-exact
sibling (The Ground) while writing the plan surfaced the inversion: the
archive's era-stamps need only terrain's *own* narrated tectonic history, and a
domain may depend only on the kernel — so the column belongs *inside*
`domains/terrain`, with paleoclimate as an optional worldgen-wired refinement.
The lesson repeats a standing one: read the closest shipped sibling before
committing an architecture, not after. The reversal led the G3 flagged section
rather than hiding in the diff.

**Zero facts, not "minimal" facts, is what keeps a derived layer an epoch-free
tier.** The spec first said "minimal summary facts." But `world.to_json()`
serializes the ledger, so *any* committed fact moves the world-identity golden
— which would have contradicted the campaign's own no-epoch guarantee. Pinning
emission to exactly zero (the stance The Ground took implicitly) is what let
`lens_purity` stay green through all seven tasks. Worth stating in a spec
explicitly, because "minimal" reads as permission.

**Two plan-authoring defects surfaced in review, both cheap, both propagated
back.** Task 1's review caught km/m lengths tagged `bare-ok(count)` where the
sibling accessor's established class is `bare-ok(ratio)` — a wrong class I had
written into the plan; fixed inline and corrected in the plan's Task 2 so it
did not recur. Task 2's implementer caught that the brief's two free functions
tagged only parameters, not the `return` position, and added the missing tags.
Both are arguments for the type-audit tool being default-deny: the wrongness
was mechanical and got caught mechanically.

**Hand the implementer the blast radius.** Adding one field to `AlmanacContext`
(which has no `Default`) touched twelve construction sites. Enumerating them in
the Task 6 dispatch — the real one, the test helper, the inline test builds —
turned a likely mid-task compile-error scramble into a clean transcription;
most were covered by the `..sample_context()` spread once the helper was
updated.

**Scope notes for the record.** No `open-questions.md` Confidence-Gradient bet
moved — The Deep doesn't touch the economics/historiography/time-forcing bets
that chapter tracks; the gradient update that *was* due is the registry flip of
MAP-10 from `raw` to `spec'd`, done during the capture pass. No hand-written
reference chapter was added: per The Ground's precedent, census metrics fold
into the generated `the-census` artifacts, documented by the chronicle. The
census refresh (three new metric columns) is a campaign-autopilot carve-out,
surfaced for explicit authorization at the close rather than run unprompted.
