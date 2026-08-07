# Retrospective — The Handle

**Campaign:** every named thing answerable · **Closed:** 2026-08-07 · **Branch:**
`the-handle` · Spec `docs/superpowers/specs/2026-08-06-the-handle-design.md`,
plan `docs/superpowers/plans/2026-08-06-the-handle.md`.

Process lessons only. The product is in
[the chronicle](../../book/src/chronicle/the-handle.md).

## 1. Two matchers for one question, found twice

`examine` has two resolvers — the prose catalog and the chart legend. Task 1
taught the first to match by word; `examine bugbear` still failed, because the
second was untouched. Task 2 taught the first that qualifiers are not nouns;
`examine hollow` still answered, because the second was untouched.

Both times the plan's own text asserted the fix was complete. Both times an
implementer found it was not.

**Do:** when a change teaches a rule to a resolver, grep for the *observable*
that rule produces — here the shared refusal string `"You see no"` — and confirm
every site that produces it learned the rule. Enumerating by the function you
happened to open finds one of them. This is the same lesson as The Manikin's six
incomplete greps, with a sharper handle: **the shared output string is a better
index of a behaviour's sites than any call graph.**

## 2. A mutation test must prove it mutated

The first mutation run against the qualifier guard reported green, which would
have meant the guard was worthless. The guard was fine; the *mutation* was
worthless — `cargo fmt` had rewrapped the target across seven lines, so a
single-line string replacement matched nothing and silently changed nothing.

**Do:** assert the target text exists before substituting it. Two lines:

```python
assert old in s, "MUTATION TARGET NOT FOUND — aborting"
```

Without it, a no-op mutation and a genuinely robust implementation produce the
same green, and the whole point of the exercise is lost. This bit within an hour
of the campaign adopting mutation testing as its verification method.

## 3. A gate can be blind to a direction and look total

The word-resolution gate asserts *declared ⊆ resolvable*. Mutating a declared
noun phrase back to its display name makes **more** words resolve — an
over-admission — and the gate stayed green. A separate regression test caught it.

The spec had claimed the mechanical half plus the structural half covered the
space. They cover two directions of three. That claim was written before either
was built, and nothing forced it to be re-checked once they were.

**Do:** for any check introduced as *the* guarantee, name the direction it
enforces in its own doc comment. The gate now says so. A check that states its
direction cannot be silently read as total.

## 4. Two git-hygiene failures, same species

Both were assertions about shared state made without looking at the real thing.

- **`git fetch . main` is a self-fetch.** It contacts the repository itself, not
  `origin`. On that basis the branch was reported as current when `origin/main`
  was **72 commits ahead**, including a seed-42 flagship rename that moved every
  fixture and every prose example the campaign had produced. Nathan caught it by
  asking "even origin/main?".
- **`cd` into the shared primary checkout persists.** A later merge ran
  `git checkout main` there and then merged main into main — a no-op — while
  another session had its own branch checked out. The reflog shows the switch was
  harmless only by timing.

**Do:** `git fetch origin` before any claim about whether main has moved, and
**never `cd` into the primary checkout from a campaign worktree.** Merging
`origin/main` directly into the worktree branch does identical work and cannot
disturb a parallel session. There is no task in a campaign that requires the
shared checkout.

## 5. The world moved under the campaign's own evidence

Three absorptions landed 95 commits from four other campaigns. One renamed the
seed-42 flagship and moved it to a different biome, so `examine forest` — the
literal symptom in the bug report — became a *correct* refusal, because "forest"
is not a word in "rainforest" and the resolver matches words, not substrings.

The demonstration had to be re-taken against the new world, and the new room is
the better witness: it carries a comma-qualified descriptor at the starting
position, so the qualifier discrimination is visible without walking anywhere.

**Do:** a campaign's worked example is evidence with an expiry date. Re-run it
after every absorption rather than quoting the transcript captured at the start;
and prefer examples derived from the live world over hardcoded names, which the
tests here did by construction and the prose did not.

## 6. Every conflict was in a generated artifact, and none was hand-merged

Three absorptions, nine conflicts, all in the same three vessel goldens.
Resolution each time: take main's side as the base, regenerate from merged
source, then verify **by parsing** that both sides' contributions survived —
main's rename present, this campaign's field present, nothing removed,
resolution words absent from the wire.

Hand-merging a generated file invents a state no code produces. This is now
three campaigns' worth of the same move working; it is a habit, not an
improvisation.

## 7. Plan-text defects: the class shifted

Seven across two campaigns, all mine, none in implementers' code. But the shape
changed. The Benchmark's were claims about the *world* that only running
something could refute. The Handle's were **stale API details** — a call to
`terrain_of()` that decision 0092 bans via clippy, an assertion about
`delve_at`'s output prefix that was simply wrong, a `MIN_WORD` doc contradicting
its own constant.

Cheaper, caught instantly, and mechanically preventable: a plan that names a
function should have that function's signature read at drafting time. That
belongs in the `campaign-autopilot` verification section alongside the
compile-claim and invariant-claim rules.

## 8. One instance was never reproduced until the task that fixed it

The underworld defect was established structurally — by reading a dispatch that
special-cased one band and not another. No cave was reachable within 400 walked
steps and the direct path was crate-private, so the controller could not
demonstrate it. Task 4's first job was the reproduction, and it produced
`You see no rock here.` on the first run.

**Do:** a structurally-established defect is a hypothesis with a good argument.
Ship the task that fixes it with the reproduction as its first step, and say in
the spec that it has not been seen — rather than letting a confident structural
reading read as an observation.

## Numbers

- 15 commits; `make gate` 3085 passing; `make vessel-check` green.
- 6 of 7 significant words unresolvable before; 0 after.
- 95 commits absorbed from four campaigns across three absorptions.
- No epoch: keystone identity fixtures byte-identical to main's tip.
