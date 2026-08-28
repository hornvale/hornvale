# Retrospective: The Latch (arc IV.b of The Bridle)

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-latch.md); the decisions are
0366–0369.

## The headline: twelve defects, all mine, and the shape changed

Twelve defects originated in controller prose — spec, plan, or dispatch brief.
**Zero survived in implementer code.** That is the same distribution The Offer
reported, and the fourth campaign running to report it, so the count is no
longer the interesting number. The *shape* is.

**Early defects were wrong identifiers a grep catches.** A test helper
(`crate::common::seed_42_world()`) that does not exist. `WorldTime::from_std_days`
used bare in three places when it returns a `Result`. An import list naming four
symbols the code in the same block never uses — which, under
`clippy --all-targets -D warnings`, could not have passed its own commit gate.
All were caught by pre-dispatch verification, cheaply, before an implementer
ever read them.

**Late defects were internal contradictions no grep can find.** The clearest:
Task 5's brief put the acceptance test in `windows/vessel/tests/suite/passage.rs`
*and* instructed the implementer to reach `delve_at` through its existing test
seam. Both halves are locally reasonable. Together they are unsatisfiable —
`delve_at` and `clear_passage_at` are private to `session.rs`'s module and the
crate exposes no public seam to steer a session to a hand-picked vertex, so an
integration-test crate cannot call them at all. Nothing mechanical sees that.
Re-reading does not either: re-reading checks a claim against the model that
produced it, and the model is what is wrong. It died when an implementer tried
to build it and reported the conflict rather than silently picking a half.

**The lesson for the next controller is about verification targets.** Grepping
identifiers is necessary and it is the cheap half. The expensive half is asking
of each brief: *do these two sentences describe the same buildable object?* No
tool does that, and the campaign's own pre-flight scan — which did find two
defects by exactly this question, before any dispatch — is the only instrument
that has ever caught one.

## The largest error was not on that list, and outranks all twelve

The spec's §3.1 asserted as settled fact that **nothing a possession session
commits is ever persisted** and that **no world-writing path exists after
genesis anywhere in the CLI.** It is false. `possess` takes a documented
`--out <PATH>`; `Session::into_played_world` folds the evolved ledger *and* the
per-session registry into a new `World`; decision 0171 already ruled on how that
save filters (it does not). A prior campaign, The First Mark, built this
deliberately.

The claim shaped an acceptance criterion, `passage.rs`'s module doc, an
idea-registry row, and a draft decision record. It survived four tasks and two
clean reviews. It died at the Definition-of-Done sweep to a three-line script:

```
possess --seed 42 --script 'go n; go n' --out walked.json
  -> played world written, 2 agent-at facts in the saved ledger
  -> agent-at present in the saved registry
possess --world walked.json          -> rc=0
```

**How it was made is the reusable half.** The evidence was a doc comment on the
`ledger` field saying it is "never written back." That comment is *true*, and it
answers its author's question: does a session mutate the world it borrowed? No —
`--world` is read-only, and `cli/src/main.rs` says so. The spec read it as
answering a different question — can these facts ever be saved at all? — and the
two questions have opposite answers.

**A doc comment answers its author's question, not the one a later reader brings
to it.** A constraint read off one is a hypothesis. This one was never framed as
a hypothesis, so nothing ever tested it, and it hardened into four documents
before anybody ran a command.

Worth noting what could *not* have caught it: every gate was green throughout,
`make rebaseline` was clean, and the drift check was empty. The code was correct
the whole time. Only a false sentence in a spec was wrong, and no instrument in
this project reads specs.

## Two gates green for reasons unrelated to correctness

Second theme, and both instances are the same shape: **a gate's scope and a
defect's location disagreed, and the gate could not tell anyone.**

1. **`gate-commit` was RED on the branch tip before Task 1 began**, from a
   docs-only plan commit that overran a 600-character idea-registry budget. The
   pre-commit hook printed *"no Rust-relevant paths staged — skipping"* on both
   docs-only commits. But `docs_consistency` is **a Rust test that guards
   docs**. The hook's heuristic is about which files *changed*, not which tests
   *guard them*, and for this test the two disagree.
2. **The campaign's own three-outcome tripwire is not in
   `docs/timings/subfloor-roster.tsv`**, so `gate-commit` compiles it and never
   runs it. Task 4 deliberately made a barred outcome possible — exactly what
   that test's assertion exists to catch — which means the gate would have
   reported green while that test was red. An implementer following ordinary
   commit discipline would have shipped on it. The dispatch was amended to
   require the full vessel suite in the foreground before any commit, and to
   frame the red as the *expected* signal.

Neither was found by a tool. Both were found by a human reading the roster
during pre-dispatch verification. The roster still does not carry the renamed
test — that is not a miss, it is how the roster works now: a green chamber run
writes it, and this branch has not had one.

3. **A third, found only by the final whole-branch review, and it is the
   sharpest of the three.** `clear` was added to the dispatcher and to neither
   verb roster, so the body-state gate never stood in front of it: a sleeping
   body could clear a passage and commit a fact. `every_bare_verb_help_lists_
   is_classified` exists to catch exactly this and was **green the whole time,
   working correctly** — it asserts that the two lists agree in both
   directions, and a verb missing from both agrees. That was confirmed by
   running it against the defective state, not inferred. **A two-way agreement
   check between two copies has a blind zone at zero copies**, and the only
   instrument that sees into it is a test that drives the behaviour: the
   preceding campaign had written precisely that (`warm_is_refused_while_
   asleep`, naming its own mutation) and the pattern was there to copy. Seven
   task reviews, all green, did not surface it either — each saw a diff that
   added a verb, and none asked what a whole-branch view asked: which lists is
   this verb in?

## What went right, and is worth repeating

- **A task dispatched as one that might correctly produce nothing did.** Task 6
  asked whether restricted passage could fire The Offer's knowledge gate. The
  dispatch handed over three facts bearing on it and **deliberately withheld the
  conclusion they suggested** — because leading an implementer to the
  controller's inference manufactures agreement instead of testing it. The
  implementer traced the paths independently, produced its own counts, and
  returned a well-evidenced BLOCKED. That is a first-class result and the
  framing is the transferable part.
- **The null was recorded, not dropped.** Acceptance criterion 5 stands in the
  spec struck through, with its reason. A criterion quietly deleted teaches a
  successor nothing.
- **An implementer overrode a controller instruction and was right to.** Adding
  the barrier gate broke five pre-existing tests through a shared helper
  (`find_open_cave_vertex`) whose premise the gate invalidated — seed 42's first
  cave-bearing vertex is `Warded`. The brief said "do not restructure them"; that
  instruction was written to prevent gratuitous refactoring and did not foresee
  a change the new gate *makes necessary*. The implementer confirmed the
  breakage empirically before changing anything.
- **A wrong decision citation was caught and not replaced by a guess.** A
  comment said `delve`'s outcome-1 branch read the flagship's starting vertex
  "until decision 0131." 0131 is *"refuted is a seventh registry status"*; 0134
  is *"a partition statistic refuted by its own mechanism is retired"*. Neither
  concerns a terrain epoch. **The controller did not invent the number — it
  copied it in good faith from the committed code, where it had been wrong all
  along.** Task 7 traced the real event to a verifiable commit (`14aa6fbab`, The
  Glasshouse, 2026-08-14) and cited that instead, with a note saying no decision
  covers it. A wrong number propagates; a dropped one is recoverable.

## Environment traps paid for again

- **`git commit -m "$(cat <<'EOF' … EOF)"` breaks on apostrophes** inside the
  heredoc even with a quoted delimiter. Two agents lost time to it in one
  session. The robust form is a message file and `git commit -F <file>`, which
  also removes the constraint on prose that working around it imposes.
- **Line numbers in a plan are claims with a shelf life.** Task 2's registration
  shifted every line below it; Task 4's brief cited four stale ones. Not a logic
  defect, but a plan that names a line is asserting something three tasks of
  edits can expire.

## Deferred, with reasons

- Task 1's probe builds to `BuildDepth::Settlements` while reading only
  `geosphere()`/`is_ocean`/`cave_at`; `Terrain` is the narrowest sufficient
  rung. One-off test, correctness unaffected.
- `passage.rs` writes `ledger: &hornvale_kernel::Ledger` fully qualified rather
  than importing `Ledger`. Purely stylistic.
- A vertex both barred *and* chamber-unrealized reports only the barrier
  refusal. Defensible — you meet the barrier before learning what is beyond it —
  and currently moot: chamber-unrealized is empirically impossible, 0 of 48,316.

Two minors carried into Task 7 were **fixed rather than deferred**, both being a
few lines: `cave_entrance_states` now calls the shared `cave_entrance_addr`
constructor instead of building its own `ChamberAddr` literal (no behavioural
divergence today, but a test helper deriving the address independently of
production is exactly how a later field change goes unnoticed), and the `Warded`
arm of `clear_response` gained a test asserting its refusal prose, mutation-
checked red by merging it into the `Sealed` arm.

**That mutation's pasted red corrected its own prediction.** The doc comment
written before the run said the *second* assertion would fire; the first one
did. The comment now records what happened rather than what was expected, which
is the whole point of pasting a red instead of describing one.
