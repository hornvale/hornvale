# The Reticence — retrospective

*A possessed host's cooperation becomes a variable: refuse, lie, or answer
truthfully because the truth is what costs the rider. Eight tasks, three
decision records (0306–0308). Process lessons only; the campaign's own
account is [the chronicle](../../book/src/chronicle/the-reticence.md).*

## Five plan defects, all in the controller's plan text, none in implementer code

The pattern The Confidant and several campaigns before it already named held
again, exactly: every substantive defect in this campaign originated in the
plan the controller wrote, and every one was caught before it could ship —
three before any implementer ever touched a file, two by implementers doing
exactly what the process asks of them.

**Caught by a written pre-flight scan, before dispatch:**

1. A fabricated kernel accessor (`Ledger::facts()`) that does not exist. The
   real surface is `find`/`iter`/`facts_about`/`facts_of`; the plan's code
   sample was checked against `kernel/src/ledger.rs` directly rather than
   trusted, and rewritten before Task 1 was dispatched.
2. A fabricated `Value` accessor (`as_text()`). `Value` has no such method;
   the plan was rewritten to match on `Value::Text` or use `Ledger::text_of`.
3. **The one that mattered.** The plan joined a species directly to
   `held-by`'s object, but `held-by` targets a *settlement* entity, not a
   people — the real chain is species → `occ-people` → site → `held-by` →
   belief → `cult-form`. Run as written, this would have matched nothing for
   every people and fallen through a legal `_ =>` arm to a single value, for
   every one of fifteen peoples, with no test in the plan positioned to catch
   it — a plausible, uniform, entirely wrong result. Caught by tracing the
   real fact chain in the kernel rather than trusting the plan's shape.
   Consequence: the campaign's headline "15 peoples, 9/6" counts had been
   computed correctly by accident (species and site are 1:1 at this seed) but
   *reasoned* about incorrectly (grouped per settlement, reported as per
   people) — the numbers survived, the reasoning did not, and the code and
   its test were rewritten to key on the real denominator before dispatch.

**Caught by implementers, during execution:**

4. A test that could not discriminate. The brief's own mutation-proof step
   used two waits to distinguish "first appearance" from "increment from
   zero" in an accumulator that starts empty — which a constant-write
   mutation passes just as happily as a correct implementation, because both
   readings look identical from an empty start. The implementer added a
   third wait and an assertion that a twice-suppressed drive's count exceeds
   one, which is the reading only a real accumulator can produce.
5. A roster grep that spilled past its own boundary. The brief listed two
   drive-kind variants that turned out to belong to a *different* enum one
   range-grep away — the pattern matched past the first enum's closing brace
   into the next one. The implementer caught the mismatch against the real
   six-variant enum and corrected the roster before using it.

**The transferable form, restated because it keeps being true:** a plan's
code samples are claims about the codebase, not the codebase itself, and the
cheapest way to falsify a false one is to run the actual command against the
real source before anyone is dispatched to build on it. Three of five
defects this campaign never reached an implementer at all because that check
ran first; the two that did were both caught exactly where the process says
they should be — a test proving it can fail, and a roster checked against
its source.

## A vacuous test and an overstated null, both self-corrected mid-campaign

Two review findings are worth carrying separately from the defect count
above, because neither was a wrong fact — each was a claim that read as
stronger evidence than the artifact underneath it actually was.

**The vacuous guard.** A no-lie test asserted that a falsehood never names
the true state, looping over three truths and comparing each against the
claimed state — except the claimed state was always the same fixed constant,
which was never one of the three truths being looped over, so the assertion
could never fire regardless of what the code under test did. It read as a
protection and protected nothing; the report's own mutation run proved this
directly, staying green under a mutation that should have reddened it. Fixed
in the same round as the finding that surfaced it, and the re-review ran the
mutation itself against the rewritten test to confirm it reddens for the
right reason now.

**The overstated null.** H4's first framing reported "0 of 70" as though it
were seventy independent chances for the doctrine prior to move testimony
and none had. It was not: every one of those seventy points landed on the
one case — zero accumulated overrides on the asked-about drive — where every
possible prior agrees by construction, before any simulation runs at all.
The undiscriminating framing was caught by a reviewer instrumenting the test
itself rather than trusting the printed ratio, and the fix added a positive
control (proving the same comparison the sweep uses *can* detect a
divergence when fed one that should exist) and split the reported number
into its structural half and its empirical half, so the number that ships
says what it actually measured.

**Both were the plan author's text, not an implementer's**, and both
survived one round of drafting before being caught — which is the same
lesson decision-adjacent retrospectives before this one have already
recorded: a correction pass on your own claims is not optional ceremony,
because the first draft of a guard or a finding is exactly where the
confident-but-wrong sentence tends to live.

## The backtick trap fired again, inside this campaign's own tooling

A commit message using backticked identifiers in shell-interpolated text
triggered command substitution mid-commit, corrupting the message (a missing
word, trailing artifact text) while leaving the actual file content clean.
Not amended, per the project's standing never-amend convention — the record
stays as it landed, imperfect message and all. This is at least the third
time this exact shape has bitten a session in this project's history. The
standing rule — never put a backtick or a bare heredoc terminator inside
text that will be shell-interpolated — is well documented and still gets
missed under normal writing pressure, which is the argument for a mechanical
guard over a fourth written reminder: a rule that has already needed
restating three times is not being caught by restating it a fourth.

## A malformed tool invocation, corrected the same night it recurred

The plan directed two separate mutation steps to invoke `scripts/mutate.py`
with a `--to` flag in a form the tool does not accept for the case at hand.
`--to <dest> <file> <old> <new>` writes the mutated text to a *different*
file and leaves the original untouched — a shape built for something
executed from a path (a hook, a shell script invoked by name), not for a
module reached through Rust's import system, where the running build never
sees whatever `--to` wrote. Two separate implementers hit the same
argument-check failure independently before the plan was corrected to the
plain three-argument form for both remaining steps. The standing guidance
this session had been carrying into the campaign recommended `--to` as the
generally-preferred way to mutate a file also under active edit — which is
exactly backwards for this shape of call, and is what put the malformed
invocation in the plan in the first place. Corrected in the standing
guidance, not only in the plan, so the next campaign does not rediscover the
same argument-check failure a third time.

## Deferred, with homes

- **`PLAY-doctrine-colours-the-improvised-name`** — a four-arm improvised
  name keyed on which phenomenon a people's religion already mythologises
  (eclipse, tide, wandering star) was considered and explicitly held in
  reserve in favor of the two-arm `cult-form` split this campaign ships,
  because four arms multiply the testimony surface with no stated need for
  it yet. Home: `book/src/frontier/idea-registry.md`, row
  `PLAY-doctrine-colours-the-improvised-name`, now pointed at this
  campaign's chronicle alongside the spec.
- **A rider concept is not registered.** The doctrine arm of the willingness
  model — a people that names you correctly and knows what to do about it —
  is structurally unreachable until a rider/possession concept is added to
  the concept registry, which is a save-format-reaching act this campaign's
  spec deliberately left to the owner rather than taking autopilot. Home:
  spec §10.4; the registry addition itself is undone and unscheduled.
  `PLAY-host-names-you` in the idea registry carries the split explicitly:
  the improvising half shipped, the doctrine half waits on this.
- **Two levers make the doctrine prior live, not one — and this file named
  only one until the final-fix wave.** The prior remains genuinely sensitive
  to a people's doctrine at reachable override counts; the drive a player can
  currently ask about is observed never to be a drive with override history.
  The levers are:
  1. **Widening what a host can be asked** — a query against a specific
     suppressed drive, or a report of the whole override record rather than
     only the pursued drive's topic.
  2. **Arbitration stickiness.** A drive holds the topic only during an
     opening stretch before it has lost anything, and once it starts losing
     it is never observed to win the topic back. Anything that lets a drive
     regain the topic after a spell of losing makes the mechanism live
     *without touching `ask()` at all*.

  Lever 2 was invisible while the artifacts said the two quantities were
  **disjoint by construction**, which asserted the state was impossible. It
  is not: `driven_overrides` is never reset, so the per-tick disjointness
  that claim rested on says nothing about the accumulated count `ask()`
  actually reads. A follow-up campaign that believed the "impossible"
  framing would have ruled out lever 2 on the strength of a wrong mechanism.
  Home: `windows/lab/tests/suite/reticence_calibration.rs`'s own H4 doc
  comment, which now states the stickiness measurement and the reachability
  verdict as two separate claims.
- **The speech budget stays out of scope.** A host volunteering testimony
  unprompted, rather than only answering when asked, depends on a delivery
  layer the spec names as still unmeasured and deliberately keeps off this
  campaign's critical path. Home: idea registry row `PLAY-host-speech-budget`,
  unchanged by this campaign.
