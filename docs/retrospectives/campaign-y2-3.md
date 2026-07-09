# Campaign Y2-3 (The Tongues) — retrospective

**Merged:** 2026-07-08

**Recurring findings.** A subagent committed Task 4's work directly to
`main` instead of the branch worktree, and `main` had, in the same window,
received three unrelated vision commits — an entangled state that could
have been destructive to unwind. It was recovered non-destructively
(rebase the branch's prior tasks onto the vision commits, cherry-pick the
stray Task 4 commit, reset `main` back to the vision-only tip) rather than
by discarding anything, and the recovery itself surfaced a real bug the
tangle had masked: Task 4's new articulation facts broke
`eyes_identity.rs` because its excluded-predicate list only knew about the
three Eyes predicates, not the six new ones. The standing safeguard adopted
from that point forward — verify each agent's commit landed on the branch,
not `main`, before reviewing it — held for the rest of the campaign with no
repeat incidents, the same "a safeguard earns its keep" pattern Y2-1 and
Y2-2's own process rules already showed. Review continued to catch real
defects downstream of the plan, not just downstream of implementation: the
`epithet-honorific` metric's first cut returned the config value fed
*into* generation, invariant per species and guaranteed to pass — a
tautology, not a calibration — and was rewritten to detect the honorific
structurally from the *committed* epithet text (a case-folded suffix check
against an independently re-derived honorific-off stem), so a real
regression in wiring would now be caught rather than waved through. A
third finding cuts the other way and is worth recording precisely because
it *didn't* need a fix: the phonotactic-validity calibration hit a genuine
`STOP-and-report-BLOCKED` failure on its first green attempt (seed 0's
goblin phonology produced a name the metric flagged as ill-formed), and
rather than loosening the assertion the implementer hand-parsed the
flagged names, confirmed them structurally valid, and correctly located
the bug in the validator's own segment-matching (a greedy first-match that
let a one-character romanization like `z` shadow a longer one like `zh`
sharing the same manner) rather than in the engine the assertion was
checking. Self-diagnosing "the check is wrong, not the thing it checks" is
the harder call to get right under a BLOCKED signal, and it was gotten
right without loosening anything.

**Estimate deltas.** The plan's code sketch for the phoneme model
over-specified `serde` derives on types that are never persisted (phonemes
live only inside a world-build, never cross the save-format boundary);
Task 3 dropped the derives and the `serde` dependency they would have
pulled in, and reverted `architecture.rs`'s crate allowlist back to strict
before landing — a small, quickly-caught correction, but the same "a
plan's code sketch is a draft, not a spec" lesson Y2-2's retrospective
already named, recurring in a third campaign. The book close ran close to
estimate: the freshness sweep touched every domain chapter the naming
change actually reaches (language, religion, settlement, species, the
cascade overview, the introduction) plus the hand-authored gallery pages
that quote almanac text verbatim (`the-sky.md`, `the-gods-seed-42.md`,
`first-light.md`) — a wider set than a first guess of "the language
chapter and the gods gallery" would predict, because every prior
campaign's exit-demo page had, at some point, hard-quoted a name.

**Spec vs. reality.** The spec's naming section asked for two properties at
once — re-draw-based uniqueness and pin-isolation — that turn out to
conflict: a re-draw makes a name depend on which *other* settlements a
world places, and joint species placement means pinning one species can
change which cells another species wins, so a re-draw could make a
shared-cell name differ between a pinned and an unpinned world, breaking
the isolation the save-format contract requires. Review caught the
contradiction before it shipped. Owner decision: drop the re-draw:
resolve every name as one deterministic draw per `(seed, species, kind,
salt)`, no collision loop, no `used`-name set threaded through generation.
Pin-isolation then holds by construction rather than by convention, and
cross-world uniqueness becomes de-facto rather than guaranteed — a
property to measure, not an invariant to assert, which is exactly what
Study 008 later did (2.79% mean collision at 10k worlds). The spec and the
plan were both corrected in place rather than left to drift from the
shipped code.

**Do differently next time.** When a calibration's implementation is
"read the value that fed the thing you're checking back out of the thing
you're checking," treat that as a tautology on sight rather than as a
passing test — the honorific metric cost a review round to catch only
because its first form looked, at a glance, like a legitimate flag read.
A short pre-implementation checklist item — "does this metric's source
data include anything the generator itself consumed as input?" — would
have caught it at design time instead of review time.
