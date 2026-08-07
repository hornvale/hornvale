# Retrospective — The Warren

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-warren.md): a kind may now declare
which realm it lives in, and the world decides where that realm exists.

## The campaign exists because a scoping question got asked before a spec did

C2c was specified as five dwarves, two of them subterranean. The first thing
this session did was check whether "subterranean" was a thing the model could
express — and it was not: `subterranean_substrate` had exactly one consumer in
the workspace and it was a test.

Authoring Mountain and Duergar dwarves under that condition would have meant
authoring them with a low-insolation surface curve, **which is precisely the
fake the previous campaign spent itself removing from xorn and rust monster**.
The defect would have been recreated by the campaign that inherited the fix, in
a programme whose spec names that exact anti-pattern as finding F5.

The generalisable habit is small and cheap: **before refining the details of a
specified campaign, check that its central noun is expressible.** One grep for
the consumer of the function the previous campaign shipped was the whole cost.

## Rung 2 arrives in the campaign *before* the one that depends on it

The peoples programme's probe-validity ladder puts "expressible, unread" second
from the bottom and warns it is "the trap a campaign walks into by accident."
Two consecutive campaigns hit it now:

- **The Long Age** shipped an authoring channel with zero occupants — knowingly,
  and answered it with mutations.
- **The Deep Realm** shipped `subterranean_substrate` with one test consumer —
  unknowingly, and this campaign is the answer.

The pattern is sharp enough to plan against: **a campaign that builds a
producer and defers its consumer lands on rung 2 by construction.** The Deep
Realm's own retrospective says a field nothing reads cannot be seen to be
wrong; the missing half of that rule is that a *function* nothing calls cannot
be seen to be unwired. The check is mechanical — after shipping a `pub`
derivation, grep its callers and count the non-test ones. If the count is zero,
say so in the chronicle, or the next campaign inherits a half-connection it
cannot see.

## Main moved three times in one session, and the third one collided

This session ran two campaigns back to back. Main moved under both:

| when | what landed | collision |
|---|---|---|
| during The Long Age | The Panes (26 commits) | none — zero file overlap |
| between campaigns | The Benchmark | — |
| during The Warren | The Benchmark's tail | **yes, semantic** |

The Benchmark changed the suitability layer's elevation term to score against
*height above sea level* rather than the raw isostatic reading. The Warren
added a cave-availability factor to **the same expression**. Both were correct;
the merge conflicted on one line, and the resolution keeps both. Had it
auto-merged, one of the two would have silently vanished.

The Long Age's retrospective had *just* recorded the fix — "check
`git log HEAD..origin/main` at each task boundary, not each session boundary" —
and this campaign still did not, discovering The Benchmark only when a
`git diff main` showed its own branch apparently deleting another campaign's
chronicle. **Writing a lesson down is not the same as installing it.** The
version with teeth is a command, not a principle: run
`git log --oneline HEAD..origin/main` as a literal step in the plan's task
template, so it happens without anyone deciding to do it.

Worth noting what *did* work: reading The Benchmark's chronicle rather than its
diff is what made the elevation collision legible in thirty seconds, because
its chronicle opens by explaining what `elevation_m` is and is not.

## The 188-line diff that was three things

Re-pinning the affect trace moved 188 lines. The implementer's report described
a numeric drift on one creature; the controller's review found the file
decomposes into six untouched species, one genuinely-changed target, one
neighbour effect, and **a re-index caused by the xorn leaving the trace
entirely**. The whole-file label-frequency comparison — *Content* 54 → 19 —
is almost entirely the re-index, comparing different creatures at the same
index.

This is The Deep Realm's "a compound rate is not a measurement," restated for
diffs: **before believing a diff's size, ask what it mixes.** The decomposition
took one script and changed the campaign's account of itself from "the affect
model drifted substantially" to "six of nine species are untouched."

It also argues for a specific habit: when a re-pin touches a fixture that is a
*list of entities*, check the entity roster first. A roster change re-indexes
everything below it and inflates every downstream comparison.

## Preregistering a magnitude you refuse to guess

The spec said world identity would move and deliberately declined to predict by
how much, on the grounds that the previous campaign's drift had travelled a
long causal path and guessing the size would invite defending the guess.

**P3 was falsified: identity moved in zero of twenty-five seeds.** The
mechanism is structural — settlement genesis packs only *peopled* species, so
re-scoring fauna cannot reach the ledger at all.

Two things went right here. Refusing to predict the magnitude meant there was
no number to be attached to. And sequencing the measurement **before** the
re-pin — Task 4 measures, Task 5 re-pins — is what preserved the finding; a
re-pin first would have silently absorbed it. That ordering was deliberate and
stated in the plan as load-bearing, and it is the transferable part.

## Confidence Gradient — and the N/A I nearly filed

**A bet moved, and my first draft of this retrospective said it did not.**

I wrote "no bet moved — N/A" on the reasoning that this campaign touches only
fauna and nothing peopled changed. Then I ran the grep the closing skill
insists on — it warns explicitly against "re-scoring nothing because no bet
obviously moved" — and it landed on the passage holding
`BIO-supply-drowns-niche`: *capacity is a supply term spanning orders of
magnitude multiplied by a condition product bounded in the unit interval, so an
authored ecological niche can only modulate the primary-production signal,
**never select against it**.*

The cave gate is a counterexample. It is a hard `0.0` on eighty-eight percent
of land — not a bounded tolerance, and no supply magnitude recovers the
excluded cells. The chapter is re-scored with a qualifier rather than a
reversal: an authored *tolerance* can only modulate; an authored *realm*
selects.

**The lesson is about the shape of my error, not the outcome.** I reasoned from
"who changed" (fauna, not peoples) when the bet is about *how a trait reaches
capacity* — a mechanism question, and the mechanism I added is the first one
that bypasses the product entirely. **A bet can be moved by a campaign that
shares none of its nouns.** The grep is not a formality to confirm an N/A
already decided; it is the step that catches exactly this.

## A perf detour, and three claims of mine the measurements refuted

Nathan reported `generalist_distinctness` monopolising a core for 10+ minutes.
The fix was real and is committed: the battery asked three questions and
answered each with its own full 30-seed sweep, but **a world build does not
depend on the niche** — it enters at `per_species_suitability`, long after
genesis. Ninety world builds to look at thirty worlds. One sweep now answers
all three, and every reported number is unchanged to four decimals and to the
cell (`170.93s → 69.87s`, 2.45×).

What is worth recording is the three things I asserted around it that turned
out to be wrong.

**1. "The heavy tier runs in debug, so the penalty is large."** True that it
runs in debug — `--profile heavy` is a *nextest* profile, not a cargo one, and
there is no `--release` in that path. But the penalty is **1.80×**, not the
3–5× I implied, because `TOOL-hot-crate-opt` already sets `opt-level = 2` in
the **dev** profile for kernel, language, terrain, climate and worldgen. The
expensive 80 % of that idea was implemented campaigns ago. I read the absence
of `--release` and inferred a cost without checking whether the dev profile was
tuned.

**2. "The release switch is a bigger win than this refactor."** Measured, it is
smaller: 1.80× against the refactor's 2.45×. And it would disable
`debug_assert!` across every heavy battery, which is a real safety net to trade
away for less benefit than the change already made.

**3. "Extending the opt list to the remaining hot crates should help."**
`hornvale-demography` and `hornvale-species` are the two crates the batteries
call per cell per species and the only hot ones the original five missed.
Adding them at `opt-level = 2` made the battery **measurably slower** —
`user` time 94.7 s → 141.4 s, on a matched pair, with `user` chosen precisely
because the box was contended and wall time was not trustworthy. **I do not
know why**, and it is recorded here as a measurement with an unknown mechanism
rather than dressed in a plausible story. Reverted.

That third one has company: `noise-is-at-its-floor` records Vec-indirection,
fdiv, vectorisation and inlining all tested and refuted on this codebase's hot
path. **Perf intuitions here have a poor record and the matched pair is
cheap.**

**One fact worth keeping from the detour**: debug and release produce
**identical** results on a live-worldgen battery — same coefficients of
variation to four decimals, same cell counts. Determinism holds across
optimisation level, which is what the pure-Rust `libm` routing was supposed to
buy and had not previously been checked at this granularity.

## Follow-ups

- **`BIO-supply-drowns-niche` is now testable in a second frame.** That row
  records that supply magnitude dominates the condition niche. A subterranean
  kind is the first case where the *supply* is surface-fed while the
  *conditions* are not, which is a cleaner separation of the two terms than any
  surface kind offers.
- **The cave gate is binary.** A sealed cave counts as habitat. Defensible — a
  sealed void still houses what is inside it — but when C2c places a *people*
  underground, "can a creature live there" and "can a walker get there" stop
  being safely separable, and `MAP-underworld-reachability` becomes that
  campaign's problem rather than a deferred note.
- **Carrion-crawler's affect change is an evidenced hypothesis, not a cause.**
  Walking `hornvale_demography::coexist::pack` by hand for that cell and seed
  would settle it. Cheap, and nobody has done it.
- **C2c can now author Mountain and Duergar honestly**, and should assert their
  realm rows in the coverage table rather than only in the biosphere registry.
