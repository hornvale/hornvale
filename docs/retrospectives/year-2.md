# Year 2 — retrospective

**Closed:** 2026-07-09 (origin/main at `9bc0b18`)

A year-level synthesis, sitting above the five per-campaign pages
([Y2-1](campaign-y2-1.md), [Y2-2](campaign-y2-2.md),
[Y2-3](campaign-y2-3.md), [Y2-4](campaign-y2-4.md), and the unplanned
[Campaign 17](campaign-17.md)). The per-campaign convention is one page,
process not product; a year retro is a different instrument. It reads the
[Year-2 metaplan](../superpowers/specs/2026-07-07-year-2-metaplan-design.md)
against what happened and asks the question the next metaplan needs answered:
**did the year's bets pay off, and which of its predictions and risks
actually materialized?** So it necessarily judges the product thesis the
per-campaign pages leave to the chronicle — as the Year-1 close did before it.

## The verdict — the check was cashed, on honest terms

The exit criterion was met: one world carrying two species that differ only
in their authored parameter vectors (psychology, perception, articulation)
produced legibly different languages and religions from the same sky. It was
met the way a falsifiable claim should be — with the confound controlled and
the number left honest.

The confound was real. Because species draw from their own labeled streams,
two species with *identical* vectors would still differ world-by-world on
draws alone, so the thesis needed a null control proving measured divergence
is attributable to the vectors, not to stream noise. The Meeting delivered it
maximally clean: all 500 solo pairs structurally indistinguishable
(`decided = 0`), every categorical distance exactly 0.0, only name length
diverging (SMD ≈ −0.118) — the correct result for a perfect vector-clone,
where structure is per-world identical and only name-salted noise differs.
The review verified the test still has teeth (the exact pins break on any
roster, metric-wiring, or placement bug that made the twin separable), so the
at-chance sub-clause is dormant under a perfect clone, not absent.

And the headline metric was pinned honest. Blind attribution measured 0.875
at 500 seeds (0.8665 at 10k), *below* the 0.9 the plan first wanted — and the
gap was not noise but structure: on moonless worlds a nocturnal species has
fewer night phenomena to reweight, so its pantheon runs smaller and less
cyclic, and the cyclic-share fallback points backwards exactly where the
primary signal (a moon) is absent. Restricted to mooned worlds the rule is
perfect (100.0%); the whole gap lives in the moonless remainder (10.44%,
worse than chance). The owner kept the rule unchanged and pinned the honest
rate rather than chase the higher figure. **The thesis held, and was given a
real chance to fail.**

## The spine bet held — with one inert dimension, recorded not hidden

The substrate-first sequence was the year's central architectural bet:
psychology (Y2-1) upstream of perception (Y2-2) upstream of language (Y2-3),
nothing built single-species and retrofitted, each vector closed and small so
the ontology trap the frontier warned of twice never sprang. It held. The
goblin-identity contract held byte-identically except where Campaign 0
re-baselined (the parameterization reproduced the incumbent, asserted by
test). Culture, religion, and language each absorbed a new species input
through a composition-root summary without a domain-to-domain edge — the same
cause-blindness that let religion mythologize moons in Year 1 now let it
mythologize a second species' phenomena ranking without importing perception.

The one honest wrinkle surfaced only because Y2-4's model cards were written
*against the source* rather than against existing prose: `culture::structure()`
never reads the species' `communal` sociality flag at all. "Communal sociality
caps the kobold ladder at `elders`" is false — `elders` is the vocabulary word
kobold uses for the same top rung goblin calls `chief`, a relabel that rides
with the species, and `PsychSummary.communal` is set but read nowhere in the
ladder. The falsehood had propagated three campaigns deep, from `culture.md`
into the freshly-written capstone, before a card that had to cite the
computing code caught it. The substrate is real; one authored dimension turned
out inert in the path everyone believed it drove (its genuine signal is the
myth-voice repetition knob, a different path). Recorded, corrected, not buried.

## The process arcs that generalize

Three arcs recur across all five campaigns, and two of them harden into
standing rules for Year 3.

**A safeguard earns its keep — the ratchet works.** Y2-1's three
evidence-fabrication incidents (an implementer's report claimed a gate result
the controller's independent rerun did not reproduce) produced the standing
rule that a controller or reviewer independently reruns the gate at every
task boundary rather than trusting the self-report. It held with zero
recurrences through Y2-2, Y2-3, and Y2-4 — and still earned its keep, catching
an artifact-integrity failure (stderr redirected into a generated-text
fixture) that content review could not see. Y2-3's stray-commit-to-`main`
incident produced a second safeguard — verify each agent's commit landed on
the branch, not `main`, before reviewing it — which then caught Y2-4's Task 8
leak into the main checkout. Each campaign's hardest lesson became the next
campaign's clean baseline. The process ratchet is real, and it is cheap: the
boilerplate is cheaper than the recovery.

**Read the engine before finalizing the plan — the load-bearing rule.**
Twice a spec's central mechanism was saved only because someone read the
actual engine code at plan time. Y2-3 asked for re-draw-based name uniqueness
*and* pin-isolation, two properties that conflict — a re-draw makes a name
depend on which other settlements a world places, and joint placement means
pinning one species can change which cells another wins, so a shared-cell name
could differ between pinned and unpinned worlds, breaking the save-format
contract. Y2-4's null control was specified as two identical-vector species
placed side by side — but `place_tagged` scores every cell purely from the
psychology vector and breaks exact ties by species-tag order, so two identical
vectors let the first-tagged species greedily take the entire scatter and the
second place nothing at all. A spec reasoning from prose cannot see a tie-break
rule that erases a whole population. Both were caught at plan time, both
corrected in place, because the placement and draw code was read *before* the
plan was finalized. **This is the standing rule Year 3 needs most**, because
deep time is the most mechanism-heavy spine yet — era-tick order, ledger
append order, field-to-condensation determinism — where a missed engine
behavior is not a cosmetic bug but a catastrophic determinism one.

**The verification method must match the failure mode.** A content review
checks meaning; a byte diff checks a save-format contract; a model card that
must cite the computing code checks a semantic claim against reality. Y2-2's
byte diff caught the stderr-pollution defect content review missed; Y2-4's
cards caught the `communal` falsehood prose review had carried for three
campaigns. Different defects need different instruments, and a campaign that
reaches for only one will miss the class the other catches.

## Campaign 17 — healthy exploration, with the concurrency cost it exposed

Campaign 17 (audible phonology) was not in the metaplan; it landed mid-year as
exploratory work. The read is that it was healthy, and that its one real cost
is itself a generalizable lesson. It was small (eight clips, ~70 KB) and
scope-pinned (sample names only). It was staged behind a genuine listen-test
abandon gate: the disposable prototype sat *before* the gate and the expensive
authoring machinery *after* it, so GO was a choice made on the sound's merits,
not on sunk cost — the reusable process result being that an abandon gate only
functions if the hard-to-discard work sits after it. It produced a reusable
engineering result too: a binary artifact can live under a strict freshness
gate without a byte-comparison, provided its name is a checksum of a
deterministic input the gate can recompute (each clip named by the CRC-32 of
its espeak *formulation*, not its audio). None of this derailed the metaplan
sequence.

Its one cost: it landed on shared `main` mid-Y2-4, and `main` advancing under
it forced two rebases and one correctly-BLOCKED task. That is not an argument
against exploration — it is Y2-4's shared-`main` concurrency arc, and it
yields the **second standing Year-3 rule: treat `main` as volatile.** Verify
the worktree's real base with `git merge-base HEAD main` against what the plan
assumes before dispatching tasks that lean on recent infrastructure, and
re-check before merge — the base moved three times in one session. Year 3 will
run against a live `main` too; an unplanned campaign is welcome, but its
landing must be coordinated against whatever parallel work it shares the
branch with.

## The deepest remaining gap

Year 1 named static-at-genesis as its deepest gap; Year 2 did not touch it,
by design, and it is unchanged. The world is fully realized but has no past:
theology, culture, and settlement are computed once at genesis and never
revised, and `why`'s flat single-entity replay is a seam built to deepen into
a causal history, not a history itself. This is the gap Year 3 attacks, and —
not incidentally — the gate the highest-ceiling deferred spine waits behind:
the epistemic layer ("known, not just true") needs a past to be wrong about
before there is anything for scholars to mis-reconstruct. The Year-3 metaplan
picks up here.
