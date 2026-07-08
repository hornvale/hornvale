# Campaign Y2-2 (The Eyes) — retrospective

**Merged:** 2026-07-08

**Recurring findings.** Y2-1's standing rule — a controller or reviewer
independently reruns the gate at every haiku task boundary rather than
trusting the self-report — held all campaign: zero evidence-fabrication
incidents, against Y2-1's three. It still earned its keep. Task 2's
independent regeneration diff caught an artifact-integrity failure a content
grep could not: the almanac fixtures had been polluted by stderr redirected
into the generated-text stream, a byte-level defect invisible to anyone
reading the fixture for plausible content rather than diffing it against a
fresh regeneration. The lesson generalizes past "did the implementer lie"
into "does the verification method match the failure mode" — a content
review checks meaning, a byte diff checks a save-format contract, and this
campaign needed the second kind at least once.

**Plan-authored defects caught downstream, not at plan review.** Three
defects in the plan's own code sketches passed plan review and were caught
only during implementation or its immediate review. Task 3's identity-test
constant (0.15) was round2-invariant — it could not fail even against a
broken lens derivation — caught by the Opus reviewer and strengthened to a
value proven protective with mutation-test evidence, the discipline Y2-1
already set as standard for identity-test constants. Task 9's sketches for
the REPL argument tokenizer and the banner line were both broken, caught by
the implementer while turning sketch into working code. None reached main;
all three say a plan's code sketches are a draft, not a spec, and that the
mutation-test-evidence bar this project applies to shipped identity tests
should reach plan-authored sketches too.

**Estimate deltas.** The book close ran to the shape Y2-1's retrospective
predicted: the freshness sweep touched seven living pages (religion,
species, perception, the cascade overview, the introduction, the seed-42
gods gallery, and Study 004's limitations) once code-vs-prose drift was
checked line by line, not the two or three a "write the chronicle" estimate
suggests. Unlike Y2-1, no census needed re-running by hand at close time —
Study 007 was written and its 10k numbers pinned one task earlier, the
"regenerate at the task that changes the inputs" discipline Y2-1 asked for,
followed and paying off.

**Spec vs. reality.** The blind-attribution rule was preregistered only for
direction ("well above chance"); a 0.9 accuracy figure lived at plan level,
and reality measured 0.875 at 500 seeds, 0.8665 at 10k. The gap was not
noise but structure: on moonless worlds a kobold's night-sky weight has
fewer phenomena to reweight, so its pantheon runs smaller and less cyclic
than the goblin's, and because night-stars carry no period, the cyclic-share
fallback points backwards exactly where the rule's primary signal (a moon)
is absent. Restricted to mooned worlds the rule is perfect (100.0% at both
sample sizes); the entire gap lives in the moonless remainder at 10.44%,
worse than chance because the heuristic is actively wrong there. Owner
decision (2026-07-08): keep the rule unchanged, pin the honest rate rather
than chase the higher figure — Study 007 records the mechanism. A smaller
gap surfaced in the almanac renderer: the "first pantheon block reproduces
the legacy section byte-for-byte" rule assumes the registry-first species
(goblin) holds beliefs; a world where it does not would render the surviving
pantheon with the anonymous legacy lead, silently dropping species
attribution. No pinned seed exercises this path, so it shipped undetected —
recorded here as a spec note for whichever campaign next touches the Gods
renderer.

**Do differently next time.** Keep this campaign's improvement over Y2-1 —
write the census that anchors the book close inside the task that changes
its inputs, not as a sweep discovery two tasks later. Extend the
mutation-test-evidence bar to plan-authored code sketches, not only
implementer-authored identity tests: three sketch defects, all caught
downstream, is a pattern worth a plan-review checklist item.
