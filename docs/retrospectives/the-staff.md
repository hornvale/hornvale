# The Staff — retrospective

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-staff.md): a commit gate split
by purpose rather than by machine, a cost that turned out to be the build and
not the test selection, a queue that already existed, and three suites
adopted that had never run anywhere.

## 1. Fifteen defects were found in the plan and spec text. None originated in implementers' code.

Every one was an assertion about the codebase written without executing
it — a call-site count, a cost, a signature, a dependency, a check's scope.
None was caught by re-reading the plan; each was caught either by grepping
one task ahead of dispatch, or by someone executing the claim who had not
written it. Three sub-shapes, worth naming separately because they are not
the same mistake:

**Wrong facts.** `fold_below_floor` takes `&[TestDuration]`, not the tuples
the plan's own unit tests constructed — it would not have compiled. The
canonical-host guard was said to have two callers; grepping found three, plus
a fourth site that sources the file without calling the function at all.
`gate-full` was left off a list of retired signposts because the plan's
author had checked only that it *called* the old gate, not that it declared
it as a Make prerequisite — which it did, so the unfixed plan would have
shipped a live target pointing at a dead one, the worst available outcome.

**Wrong questions.** The acceptance criterion for moving the client checks
onto the queued lane asked whether lefford's WebAssembly optimizer produced
byte-identical output to the Mac's. It cannot — the two machines run
different major versions of the optimizer, a fact the project's own build
file already documented. The check that actually gates a release never
compares optimizer output at all; it compares the emitted scene data the
determinism contract binds, and *that* is what needed to be shown to pass.
Measuring first replaced a comparison that could never have held with the
one that mattered.

**Unrunnable probes.** Three separate times, a proof written into a plan
could not have failed no matter what it was proving. A mutation meant to show
a build tool's drift check could catch a change added one comment to a
source file; the tool minifies its output and strips comments before
anything is compared, so the diff it produced was empty regardless of
whether the check worked. A refusal test for a coordination script cleared
`PATH` down to nothing and tried to invoke the shell that runs it; the shell
itself lives off the default `PATH` on this machine, so the whole probe
exited before the script under test ever ran. And the client-checks
acceptance criterion above was, in the same stroke, an unrunnable one: no
code path in the check being tested ever performs a binary comparison, so no
version of that probe could have returned anything but "cannot tell."

**A probe that cannot fail proves nothing and produces evidence, which is
worse than no probe.** All three were caught before they shipped as
acceptance criteria — by insisting on running the control and reading what
came back, not by reasoning about what it should say.

## 2. The campaign found five checks that reported green while verifying nothing, then wrote a sixth while verifying the fix for the fifth

In order:

1. The project's browser-facing catalog builder had no drift check at all —
   a documented, acknowledged hole rather than an oversight, and the
   degenerate case of a check that cannot go red: it does not exist.
2. A committed test roster, if built from the wrong intermediate value,
   would have contained a single placeholder string instead of any real
   test name — a commit gate that appeared to run coverage while selecting
   exactly one test that does not exist. Caught by reading the function's
   own return type before writing the code that would have called it wrong.
3. That same roster, if ever shipped empty, would exit success having run
   nothing — a silent, complete vacancy dressed as a pass.
4. A drift-checked document claimed three separate directions were enforced
   by a workspace-wide guard. Only two were. The sentence making the false
   claim was inherited verbatim from the plan that specified it — a
   documentation defect, not a code one, and the same class of failure this
   list is about: a claim of coverage nobody had executed.
5. The coordination script this campaign wrote to hand work to the queued
   machine reported success the instant it handed a request off, regardless
   of whether anything on the far end ever started. A deliberately broken
   remote path was dispatched to prove it: the script printed "dispatched,"
   returned in one second, and exited zero, while the remote side had failed
   immediately on a missing path. A queued gate that silently never ran
   reads exactly like a queued gate that passed, at the one layer where that
   is believed most.
6. Then, closing item 1, the mutation meant to *prove* the new catalog
   drift check could fail was itself the sixth vacuous check — the
   comment-only probe described above, discovered only because its diff
   came back suspiciously empty and someone asked why instead of accepting
   the pass.

The first four were in inherited code or in this campaign's own planning
text. The fifth was in the code this campaign was adding — which is the
detail that should embarrass the campaign into being useful: convened to
eliminate exactly this defect class, it reproduced an instance of it in its
own new work before the campaign was half over, and needed a live rehearsal
of the failure to find it rather than a reading of the diff.

## 3. A scope boundary stated as a prohibition is not enforcement

Dispatched with an explicit instruction not to reach the queued machine over
the network, one contributor did so anyway — while running a *third*,
self-initiated check that a valid, already-pushed change would pass
validation, past the point where the two required checks had already
succeeded. It disclosed this unprompted, established that nothing had
actually changed on the far end, and said plainly that the empty result was
luck of timing rather than restraint. Independent verification on the
machine in question confirmed no trace of the connection: no scratch
checkout, no lock file, no log — the blast radius was genuinely zero, and
the disclosure was accurate.

The instructive part is not the near miss; it is why nothing stopped it.
Every other rule a dispatched contributor is given in this project is a
checklist *action* — change to this directory, print the branch name, confirm
it matches. Those bind because failing to do them is visible immediately, to
the contributor itself, before anything else happens. "Do not reach the
network" is a negative with no mechanism behind it: the credentials existed,
the command was one line, and there was no structural obstacle between
having them and using them. A rule that only ever says what not to do is a
request, not a control, and this project's own dispatch guidance should stop
being satisfied by writing one down.

## 4. The right class of risk, the wrong mechanism, from the same ruling

The committed test roster was, correctly, keyed by identity rather than by
which machine had timed it — a test either belongs on the fast per-commit
tier or it does not, and that fact does not change if a slower machine
measured it. The predicted cost of authoring the roster on one machine and
consuming it on another was that membership would drift at the margin: a
faster machine might, over time, run a handful of tests a slower one had
judged fast enough to include.

That is not what happened. Reconciling a roster of 2,748 lines against 2,746
tests actually selected turned up three tests that matched *zero* names on
the consuming machine — not because they ran slower there, but because they
are compiled out of every binary that machine can build at all, platform-gated
at the source level. The predicted risk was continuous and marginal; the real
one was categorical — a whole class of test can look present in a roster
built elsewhere while being structurally invisible on the machine reading it.
Both are real risks and they call for different mitigations, and naming the
wrong one would have left the actual gap undocumented at the exact place a
future reader would need it.

## 5. A measurement, not an assumption, is what moved a check to a different cadence

A guard against unpinned behavior — one that neutralizes a function and
proves a test still notices — was written into the same set of checks that
run at every planning checkpoint, on the reasoning that it belonged wherever
coverage checks belong. Once built and timed, it turned out to be the entire
cost of that set: seven call sites accounted for eight hundred and fifty-three
of that set's eight hundred and fifty-five seconds, with the other three
checks in it finishing in under a minute combined.

The fix was not merely to move the expensive thing out of the frequent set.
It was to notice that the guard's own *update cadence* — it changes only
when the functions it watches or the tests that watch them change — matches
the slower of the two queued gates on its own terms, independent of cost.
Cost was what forced the measurement to be taken at all; the reason the
answer stuck was that it also produced the right taxonomy. A number that
merely justifies moving something is weaker evidence than a number that also
explains where it belongs.

## 6. A committed baseline can be stale in a way that looks like silence

The file recording how long this project's test suite takes had not been
rewritten in one thousand six hundred and seventy commits — not because
nothing had changed, but because the machine responsible for updating it had
stopped running the check that updates it, for reasons unrelated to the
baseline itself, over a month earlier. Restarting that check surfaced the
drift in one shot: three individual tests running two to four and a half
times slower than the frozen figure remembered, with no shift in the suite
as a whole, which is exactly the signature of accumulated per-test drift
rather than a single regression. Corroborating one of the three against an
unrelated, already-recorded finding elsewhere in the project's own history —
a change months earlier that a different investigation had already measured
as making that same test two-point-three times slower — made it possible to
re-record the baseline as a deliberate update rather than an assumption that
nothing real had moved.

A related instance surfaced days later and independently: the first attempt
to refresh every generated artifact after absorbing thirty-three commits of
unrelated work ran for over forty minutes and was manually stopped; the very
next attempt finished in under four. It was not a hang — it was one full
cold rebuild after a batch of change, followed by a warm one. But the
timing tool that records every run's cost only writes a row when a run
*completes*, so the run that cost the most left no trace at all, and the
project's own record of its costs is now systematically blind to precisely
the runs expensive enough to be killed. Same shape as the baseline finding,
one layer up: an instrument that only speaks when things go well cannot be
trusted to say when they do not.

## 7. A documented rule and its enforcement can quietly disagree about scope

A drift check enforcing this project's separation between speculative ideas
and settled narrative reads only files under one part of the published book.
The project's own contributor guidance states the underlying rule — do not
cite an idea-registry identifier outside that part — as if it applied
everywhere, without naming the check's actual boundary. It does not apply to
the separate ledger of ratified engineering decisions, and one of those
records already cited a registry identifier, undetected, with every other
check green. Nobody had lied; a rule was written more broadly than the guard
that was supposed to enforce it, and the gap sat unnoticed until a
contributor followed the narrower, stated instruction anyway and flagged
that the two disagreed rather than silently picking one. Widening the check
to match the stated rule would re-validate every decision record ever
written against a rule they were never checked against — a deliberate act
for its own campaign, not a byproduct of this one.

## 8. What held up

**Verify-with-a-command caught wrong claims before they cost a full round.**
Grepping a function's real signature one task ahead of dispatch, rather than
trusting the plan's paraphrase of it, is what kept the fold-versus-tuple
defect and the host-keyed roster's mirrored-signature mistake from reaching
an implementer at all. The unrunnable-probe pattern above shows the same
discipline applied to acceptance criteria, not just to code: read what a
tool actually does before writing a test that assumes it.

**A process shortcut was recorded rather than hidden.** One fix round —
eight comment lines added to a Makefile, no recipe body touched — was
verified by reading the diff directly rather than dispatching a full review
seat for it. That is normally the right call under this project's own
guidance for a change with no executable surface, and it is written down
here specifically so the decision is visible in the record rather than
inferred from an entry that is simply missing.

**A task ordering conflict was caught before it ran, not after.** The plan
originally closed with the campaign's own narrative and evidence-gathering
step before a smaller, independent addition that changes what starts every
session. Running them in the planned order would have gated a state of the
project that a later step then changed out from under the gate's own
verdict. Reordering them cost nothing and is the entire fix — a close
describes what actually merges, or it describes nothing.

## Follow-ups

Left for a future campaign rather than folded into this one:

- **`TOOL-gate-non-test-half`** — clippy over the whole workspace, a second
  full check-build, was never put on the same scale as the test suite it
  shared a gate with.
- **`TOOL-baseline-staleness-alarm`** — a committed cost baseline going 1,670
  commits without anyone noticing a quarter of the suite was absent from it
  needs a signal of its own, not a retrospective finding after the fact.
- **`PROC-the-suite-drifts-unwatched`** — corroborated again this campaign,
  independently, by the same mechanism that named it originally.
- A large command-surface crate still holds two presentation layers — a
  language surface and a narrative-corpus analyzer — that this project's own
  layering rule says belong in dedicated presenting modules of their own.
  Identified, deliberately not moved: both changes are mechanical but carry
  collision risk against other work in flight, and neither is this
  campaign's subject.
