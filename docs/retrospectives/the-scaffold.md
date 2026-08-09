# The Scaffold — retrospective

**Completed:** 2026-08-01 (slug-named per decision 0026; spec
`docs/superpowers/specs/2026-08-01-the-scaffold-design.md`, plan
`docs/superpowers/plans/2026-08-01-the-scaffold.md`, three tasks: the
`BakeId`/`EntityId` type split → `layer_key`, the material comparator →
measurement, artifacts, and the gate). Ran under campaign-autopilot.

**A measurement with no predicted value is still worth preregistering —
it froze the *definition*, and that is what made three drift checks
possible later.** M1 had an explicit "no predicted value" (an earlier draft's
5–25% band was discarded as a guess wearing a prediction's clothes). What
stayed frozen was the comparator being measured and the two decoder paths to
measure it on — enough that Task 3 could compute the spec's own
pre-implementation numbers from a description alone, reproduce them exactly
post-implementation, and treat any divergence as evidence the built
comparator was not the specced one. A measurement without a predicted value
is not a measurement without a contract.

**"Confirmed by regenerating to scratch" is a claim about one run, not a
property of the code — and needed a second empirical check to resolve,
not a shrug.** The Task 3 brief (built from Task 2's own "M8" note)
expected `history-seed-42.md` and `vestige-seed-42.png`/`.md` to move on
`make rebaseline`; none of the three did. Rather than accept the diff (or,
worse, its absence) at face value, two throwaway probes traced each
non-move to a specific, checkable cause: the showcase site's hardcoded cell
happened to sit among the *unaffected* 280 sites, not the reordered 19; and
the vestige PNG's `most_dread` picks by a `dread` value with a
first-occurrence tie-break, which never fired across any of the 19 reordered
sites in this particular world. Both are genuine properties of *this* world
at *this* comparator, not bugs — but "the spec's verification section said
it would move" and "it moved" are different claims, and only the second was
actually checked before this campaign. Lesson: a predicted artifact move is
itself a claim that wants the same verify-before-assert discipline
(`[[PROC-20]]`) as any other generated-output claim, even when the
prediction comes with a traced code path attached — tracing the path proves
the *dependency exists*, not that it *fires* for this data.

**A throwaway probe is cheaper than it looks, and cheaper than trusting
inference.** The spec's own M1 caveat named the risk directly: the almanac's
`layers_at` fraction "should match" the worldgen path's "but that is an
inference, not a measurement." Two small test files (built, run with
`--nocapture`, then deleted before commit) turned that inference into six
confirmed numbers and, separately, into the exact reason three artifacts
didn't move. Neither probe touched anything that survives the commit —
`layers_at`/`Layer` were made `pub` only for the probe's duration and
reverted with `git checkout --` before staging, confirmed by a clean `git
status` immediately after. The pattern is reusable: when a private decoder
needs measuring from outside its crate, widen its visibility for the
lifetime of one probe rather than either skipping the measurement or
permanently exporting internals the campaign doesn't otherwise need public.

**The type split's hardest defect was found by review, not by the gate.**
Task 2's fix round shows the shape: four of six review findings landed on
`layer_key` itself (a `to_bits()` precondition violated by any negative or
NaN day nothing in the type system rules out; an overclaimed "total order"
that only holds because of an invariant one layer up, in `windows/worldgen`,
that `domains/history` was silently leaning on; a "material facts only"
doc claim that undersold the `founded_from` tie-break's real role). None of
these failed a test before review — `make gate` was green throughout. The
review discipline of *reading* the diff rather than trusting a green suite
(the same discipline `[[PROC-20]]` names) is what caught them, and each fix
tightened a doc claim to match what the code actually guarantees rather than
changing behavior.

**Scope notes for the record.** No `open-questions.md` Confidence-Gradient
bet moved — grepping identity/entity/stratigraphy/mint-order/palimpsest
found only incidental prose, never a tracked bet. Two followups promoted to
the idea registry rather than left in `.superpowers/sdd/` (git-ignored, dies
with the worktree): the `AgentId`→`EntityId` handle-confusion cousin at
`windows/vessel/src/session.rs:693`, and the type-audit day-tagging
convention gap (`bare-ok(count)` used where decision 0028 specifies
`pending`) surfaced during Task 1 review. A third deferred finding — three
bare-`f64` `assert_eq!`s in `domains/history/tests/record.rs`, against the
Global Constraints' float-comparison ban but exempted here because the
values are exact small integers with no transcendental in the path and the
file's pre-existing asserts already do the same — was judged not worth its
own registry row; fixing three in isolation would leave the file internally
inconsistent, and the honest fix is a file-wide sweep some future campaign
may or may not find worthwhile.

**Nothing is left pending for a census or the heavy tier.** An earlier draft
of this retrospective said `book/src/laboratory/generated/the-history/summary.md`
awaited `scripts/census-run.sh`. Wrong on the mechanism — that artifact is
written by the heavy tier (`cli/tests/history_battery.rs`), so the refresh
would have been `make heavy-remote` — and wrong on the merits, since every
value in it is an order-independent aggregation off the ledger and cannot
have moved. The lesson is the same one this campaign learned about artifact
predictions: **which mechanism authors an artifact is a one-grep fact, and
"it lives under `book/src/laboratory/generated/`" is not evidence about it.**

One genuine census-adjacent note, kept because it is the opposite case —
order-sensitive rather than order-independent. Both census studies request
`metrics: "all"`, which includes `mean-warning-legibility`
(`windows/lab/src/metrics.rs`), and that metric sums a float per vestige *in
stack order*. Reordering a stack changes float summation order. The 8-digit
quantization at `render_csv` will almost certainly absorb it, but "almost
certainly" is the state of the claim, not a measurement, and the check is
`make lab-diff STUDY=the-census` whenever the next census runs.

## Handoff — what the next campaign must not rediscover

Promoted out of the campaign's `.superpowers/sdd/` ledger before the worktree
was torn down, per the scratch-dies-with-the-worktree rule. Everything below
existed only in scratch until this section was written.

**`campaign/the-particular` will not compile against this `main`, and one break
is semantic rather than mechanical.** That branch is parked at 19 commits
awaiting exactly the identity work The Scaffold began.

- `windows/worldgen/src/person_promote.rs:81` reads `r.community` off a
  *reconstructed* `OccupationRecord` and feeds it to `PersonSeed.community`.
  The Scaffold deletes that field, because a reconstructed record genuinely
  does not know its community — the old field held the occupation's **own**
  identity under a misleading name. **The correct resolution is
  `r.community` → `r.id`**, which is numerically identical because
  `reconstruct_occupation` set `community = entity`. It is **not**
  `founded_from`, and it is **not** a re-derivation. Both of those compile
  cleanly and silently change which entity the person's facts key on.
- `domains/history/src/flesh.rs::founder_handle` reads
  `occ.people` / `site` / `founded` / `ended` / `peak_population` flat and needs
  `.core.` inserted.
- Both branches edit `flesh.rs` and `tests/flesh.rs`; expect a textual conflict
  there on top of the two fixes above.

**The three-campaign sequence this campaign opened.** The Particular was parked
because promoting 90 persons shifted a vessel session's NPC from entity 1865 to
1955 and reddened a save-format-class fixture with no content change —
`EntityId` being a positional identifier doing the job of a stable identity.
Rather than rebaseline the symptom, the cause gets fixed in three steps:
**The Scaffold** (done) split the bake's private handles from the ledger's and
removed mint order from the stratigraphy comparator; **The Salt** decouples
derived prose from ids; **The Signet** changes the derivation itself, plausibly
to an ancestry hash. Doing the prerequisites first is what lets The Signet's
artifact diff contain *only* id changes — otherwise a reviewer cannot
distinguish "an id moved" from "prose changed because an id moved."

**Two invariants are now load-bearing and documented only in prose.**
`layer_key`'s totality rests on the bake opening at most one `Genesis`
occupation per site (`windows/worldgen/src/history_bake.rs`), and the three
decoders agree on a key tie only because `sort_by_key` is *stable* over the same
ledger iteration order. Both are stated in `layer_key`'s doc comment; neither is
enforced by a test. A re-genesis path, or a switch to `sort_unstable_by_key`,
breaks one of them silently.

**One correction worth carrying, because the reasoning was wrong in an
instructive way.** During the final review this campaign flagged
`windows/vessel/src/brief.rs`'s `.find(|o| o.core.ended.is_none())` as depending
on a one-alive-occupation-per-cell world property, and measured that property to
hold (0 of 399 cells on seed 42). The measurement was right and the conclusion
was wrong: `founded` leads **both** the old and the new comparator, so that call
returns the earliest-founded alive record either way and the campaign could not
have changed it. The error was checking the property that would make the code
safe without first checking whether the code was already invariant. No registry
row was minted.
