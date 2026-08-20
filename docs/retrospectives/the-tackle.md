# The Tackle — retrospective

**Merged:** 2026-08-19

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-tackle.md): Arc I.a of The Bridle
— the action layer extracted from a 14,768-line file, `Drive::affordance`
renamed to `Drive::proposal` to free Gibson's word for Arc IV, and one shared
derivation of a body's mass — all of it byte-identical against every committed
artifact.

## 1. Every defect was in the plan text, not in implementer code — again

Five of the campaign's process findings (§2–§6 below) share one shape: the
planning session wrote a **claim about the repository** and did not check it
against the repository. Not one originated in an implementer's work. This is
the fifth or sixth campaign in a row to land on the same sentence, and
repeating it is no longer useful on its own, so this retrospective tries to be
specific about *which kind* of unchecked claim each one was:

| Finding | The claim | What it actually was |
|---|---|---|
| §2 (#17) | this constant reaches an artifact | documented three lines away as unable to |
| §3 (#11) | there are five drives | a grep pattern that skipped every generic impl |
| §4 (#12) | a possession never persists | true of the input file, false of `--out` |
| §5 (#15) | the time-charging rule is undefined | fully implemented in `clock.rs` |
| §6 (#16) | the test selector is `--test <name>` | consolidated to `--test suite -- <name>` |

Four of the five were checkable in under a minute by the session that wrote
them. Three of the five (#12, #15, #16) reached Nathan, a spec, or a plan
before anyone caught them.

## 2. The positive control was aimed at documented-inert code — and the fix is not "be careful"

This is the campaign's headline process lesson.

Task 1's job was to prove the byte-identity acceptance criterion was not
vacuous. The plan specified the probe: change `REMEMBERED_PENALTY` from 5 to 6
and watch the artifact drift check redden. A full `make rebaseline` moved
**nothing**.

The cause was three lines above the constant, in the doc comment on
`move_cost`, the function that consumes it:

> For an EMPTY `avoid` set every edge stays `1` — the byte-identity property
> both planners share.

The plan had picked the one constant in the file that the source explicitly
documents as unable to move an artifact. The refutation was free, adjacent to
the named line, and in plain English.

The immediate lesson is the obvious one — read the doc comment on the consuming
function before running a probe a plan hands you. But that lesson is a request
for more care, and requests for more care do not survive contact with the next
campaign. **The generalizable fix is structural: do not prescribe a mutation
from outside the code.** A plan should name the *property the probe must
demonstrate* — "a change in vessel source must be observable in a committed
artifact" — and leave the choice of mutation to a search against the actual
source at execution time. The prescribed guess was inert; a search over the same
region found a working discriminator (`clock.rs`'s `REFERENCE_MASS_KG`,
70.0 → 71.0) on its **first** candidate.

This is "imperative mood hides assertions" in its purest observed form.
"Change `REMEMBERED_PENALTY` from 5 to 6" reads as an instruction to perform,
and a step you perform does not invite scrutiny — but the sentence silently
asserts that the constant reaches an artifact, and that assertion was false and
cheaply falsifiable. Writing the step as a decision rule ("find a constant whose
perturbation reddens the drift check") would have exposed the assertion as
something to establish rather than something to assume.

## 3. What the surviving control proved is narrower than "the acceptance test works"

The replacement probe worked, and the temptation was to write "the byte-identity
acceptance test is not vacuous" and move on. The number does not support that
sentence.

The probe moves **one file** (`possession-over-time-seed-42.md`), **two lines**,
at the **fifth decimal place** (day 5.00153 → 5.00152). The day-zero possession
transcript did not move at all.

So the demonstrated fact is precise and small: a live path exists from vessel
source, through `tempo` and `cost_ticks`, into an emitted day timestamp in a
committed artifact. The *undemonstrated* claim — the one the confident sentence
would have smuggled in — is that the check is sensitive to an arbitrary accident
anywhere the campaign's four tasks touch. It is not, and nothing run here shows
that it is.

The consequence was adopted into the plan rather than merely noted:
**`drift-exit=0` is necessary, not sufficient.** The behavioural weight rests on
the vessel crate's own suite; byte-identity is a tripwire for one class of
accident, the kind that changes an emitted number. The plan as written implied
byte-identity alone was the whole acceptance criterion, and Task 1's rewrite
says otherwise in as many words.

This is the "a reviewer's sentence can overstate its data" failure caught before
it was written rather than after. The number is the finding.

## 4. Count the method, not the impl header

`grep 'impl Drive for'` returned **3**. The truth is **6**. Three drives are
written `impl<'a> Drive for Thermal<'a>`, and a pattern requiring a space after
`impl` skips every generic implementation in silence.

The spec had already been written saying "five drives" — a guess that happened
to land *between* the wrong count and the right one, which is exactly why it read
as plausible and survived a draft.

The rule: to count trait implementors, count the **method**, not the impl
header. `fn proposal` returns 7 = 1 declaration + 6 implementations. A header
pattern is defeated by generics, `where` clauses, and line breaks; the method
signature is defeated by none of them. Same family as the `git grep -E`
word-boundary trap — a wrong-but-smaller number is indistinguishable from a
right one, and both look like an answer.

Caught by the spec self-review re-running the count instead of trusting the
draft.

## 5. A doc comment answers its author's question, not yours

`Session::ledger`'s doc comment says it is "a clone of the frozen world's
ledger … Never written back". On the strength of that sentence, the planning
session told Nathan — and wrote into the metaplan — that nothing a possession
writes is persisted, and therefore that The Bridle was not save-format-adjacent.

False for the question being asked. The comment describes the **input**
`--world` file. `Session::into_played_world` moves `self.ledger` whole into a
fresh `World`, and `possess --out` saves it: "the played world outlives the
session," shipped by The First Mark. Player facts do reach saved world files,
and the program is save-format-adjacent after all.

The comment was not wrong. It answered the question its author had. The claim
had already survived being stated to Nathan once and written into a spec before
anyone read the code path instead of the prose above it.

The rule: when a claim is load-bearing, read the **code path**, not the comment.
A doc comment is evidence about its author's intent at the time of writing, not
a specification of current behaviour.

## 6. Before calling something undefined, grep for the thing that would implement it

The G3 approval package asked Nathan to rule on "the time-charging rule",
describing the model as undefined. `windows/vessel/src/clock.rs` already has a
complete one: per-action base ticks, scaled by body mass and by a terrain
factor, converted against a rotation-derived tick rate, behind an exhaustive
match so that a new action cannot silently become free.

Worse than a wasted question. The signature is

```rust
pub fn cost_ticks(action: &Action, mass_kg: f64, terrain_factor: f64) -> Ticks
```

— **no driver parameter**. The architecture had already agreed with The
Bridle's keystone (the body pays the tariff; whatever is driving it cannot
reach the cost function, which is why domination will be undetectable as a
physics anomaly) before anyone stated the keystone, and the session asked
Nathan to decide something the code had settled. The question was withdrawn.

"Undefined" is a claim about the repo, and claims about the repo are checkable.
Same family as §5. Escalating a settled question spends the one resource an
approval gate exists to conserve.

A related instance the same week (#16): the plan was drafted against the
`CLAUDE.md` snapshot loaded at session start, which still showed the old
`--test <binary>` selector form. The tree had consolidated every crate's
integration tests into one `suite` binary. **Session-start context is a snapshot
with a timestamp, like any committed baseline** — confirm a convention against
the tree the plan will run in, not against the document describing it.

## 7. Renaming a `#[test]` function is a commit-gate change

Task 3's rename swept eight **test function names** carrying the word
`affordance`.

`docs/timings/subfloor-roster.tsv` selects the commit gate's test tier by
**exact test name**, under an exclude-unknown rule. Renaming those eight tests
without rewriting the roster in the same commit would have silently dropped
them from every `gate-commit` — while the gate kept printing green. This is
precisely the failure `CLAUDE.md` documents at length for `hornvale-hearsay`:
compiled, never run, green and meaningless.

The sharp edge is that **a gate running fewer tests goes green faster**, so the
only visible symptom is an improvement. Same family as "an allow-list gate
cannot see its list go short."

Caught by the implementer, **not by the plan** — Task 3's Files list named
`liveness.rs` and `docs/audits/` only, and the plan had no idea test names were
a gate input. The positive control was run independently by the controller
rather than taken on report: `bash scripts/subfloor-roster.sh | tr '|' '\n'`
yields 8 `proposal` ids and 0 `affordance` ids, and the eight functions exist
under the new names.

The rule: an identifier rename that touches `#[test]` function names must
rewrite `docs/timings/subfloor-roster.tsv` in the same commit, and the proof is
**the selector's own output**, never the gate going green.

## 8. The type-audit report is an aggregate, which inverts the plan's rule

The plan told Task 3 to expect `docs/audits/type-audit-report.md` to drift,
"keyed on public-boundary signatures". It is not. It is 66 lines of counts — by
verdict class, by crate, by pending wave — with **zero per-symbol rows**.

The evidence arrived a task early: Task 2 moved a dozen `pub` items into a
brand-new module and the report did not move one byte.

So the rule inverts, and becomes more useful in the process. For a pure rename
or a pure intra-crate move the report **must not** move; if it does, a
`type-audit:` tag was dropped or duplicated during the edit. It is a
**tag-conservation check**, not a signature check, and its silence is the
signal.

Confirmed in both directions across the campaign. Task 3: file-wide tag count 8
before, 8 after, report unmoved, nothing staged from `docs/audits/`. Task 4, the
one commit that introduced genuinely new public-boundary primitives: the report
moved by exactly **+2 in the vessel crate**, matching the two new primitives (a
`&str` species and an `f64` return) exactly. A check that is silent for three
commits and moves by the predicted amount on the fourth is a working
instrument; one that had drifted on the rename would have been reporting an
error.

## 9. `make board-post NOTE=` command-substitutes backticks

A note written with a markdown code span containing `` `make rebaseline` ``
**executed it**. The Makefile recipe wraps `NOTE` in double quotes when building
its `cargo run` command line, and backticks still command-substitute inside
double quotes. The phrase ran a real rebaseline (wall 101.9 s), spliced its
stdout into the posted note, and left a stray `docs/timings.md` row behind.

The post still looked plausible afterwards, which is the hazard: the damage
landed somewhere other than the visible output. A visibly broken post would have
been safer.

Also true of `$(...)` and `$VAR`. This is a real defect in the recipe, not
merely a usage trap — carried to follow-ups rather than fixed here, because the
board's CAS/append path is off the fast lane and this campaign was
byte-identity-locked on `windows/vessel`.

## What held up well

**Splitting Arc I was the right call, and the reason generalizes.** The two
halves carried opposite acceptance criteria — "no artifact moves" and "artifacts
move, readably" — and run together each would have destroyed the other's
legibility. The carve was made on *kind of risk*, not size, and that is the
axis worth reaching for next time a stage looks too big.

**Every task widened its own file list and said so in the commit message.** The
extraction touched `nav_bench.rs`, `clock.rs` doc references, and three
intra-doc links the plan had not named; the rename touched roster ids the plan
had not named; the mass extraction found a *second* byte-identical inline
derivation and unified both rather than leaving one behind to drift. In each
case the widening was reported as a deviation with a reason, not absorbed
silently.

**The campaign's own byte-identity claim was tested before it was trusted**, and
when the prescribed test failed to fire, the response was to find a working one
and rewrite the plan — not to declare the empty diff a pass. An empty diff needs
a positive control, and this campaign is the case where the control did its job
by failing first.

**A stale `docs/timings.md` row was not manufactured.** A `make rebaseline`
stalled in `dyld` for 81 minutes at close (0.08 s of CPU consumed, blocked
before `main`) and was killed; because `timed.sh` writes its row only on
completion, the ledger carries the clean 38 s re-run and no record of the stall.
The truncated artifact it left behind was restored from git before the re-run
rather than regenerated over.

## Follow-ups

Promoted out of the campaign's git-ignored scratch, which dies with the
worktree.

**F-1 — REGISTRY candidate: the origin-of-intent taxonomy.** Branching an act by
*where the intent came from* shows the system occupying exactly one leaf of six:
in-character / self-originated / **deliberated** (GOAP plan or player command)
is built; *habitual*, *reflexive*, *coerced by a mind*, *coerced by the body*,
and *coerced by the world* are all empty. The load-bearing observation is that
**player command and GOAP plan are two values of one leaf** — siblings, not
opposites — which is why the player/creature split has always felt arbitrary: it
is a distinction drawn *inside* a leaf while five sibling branches sit empty.
Arc III fills one of the five; the other four would each reuse the same
controller machinery. Held as a registry-row candidate rather than minted,
pending Nathan's approval of the program.

**F-2 — REGISTRY candidate: durations and interruption.** `Action::Rest` already
jumps `st.day` forward to the next waking — a multi-day act modelled as one tick
with a teleport. The codebase therefore has one long action and no *concept* of
one, and consequently **no act can be interrupted**. Mind control forces the
question (what happens to an act in flight when a body is seized?), as would
sleep interrupted by a predator. Relatedly, the suite is entirely human-scale:
no sub-tick reflexes, no multi-year acts (build, migrate, raise a child), while
the domains already model world-scale change. The action system is one band of a
multiscalar spectrum not yet described as one.

**F-3 — measure GOAP budget headroom before Arc II.** The planner runs in
Dijkstra mode (`heuristic() == 0`) on a 1,000-node budget, and its failure mode
is a *frozen creature* rather than an error. A penalty of 20 once froze roughly
900 seed-42 fauna and had to be cut to 5. Arc II multiplies the planner's
callers; measure headroom **before** it does, not after a silent freeze. An
admissible geometric heuristic is already reserved in `REMEMBERED_PENALTY`'s own
doc comment.

**F-4 — the stale doc comment on `Session::ledger`.** Beyond the "never written
back" error in §5, its claim that the ledger is "mutated only by `wait`'s tick
(NPC `agent-at` facts)" is also untrue: `act_on_disposition` commits
`disposition-shift` and the hostility loop commits `turned-hostile`. Small, but
it is the exact comment a reader consults to answer "does the player write to
the ledger?", and it answers wrongly — twice now.

**F-5 — undo versus the depth economy (a tension, not a defect).** The daybook
makes undo/redo/retry straightforward: truncate and replay. That retroactively
justifies the materialised play-ledger as a **checkpoint** — replay cost is
O(distance to nearest checkpoint), so checkpoints want to be periodic rather
than terminal, and undo latency sets their spacing. Checked against the play
rows, there is no save-scumming collision: `PLAY-determinism-is-anti-scum` and
`PLAY-no-reroll` both endorse it. **But** `PLAY-eviction-costs-depth` closes
death-farming "against the conserved quantity rather than with a new rule" — a
voluntary step *spends* depth, an eviction *loses* it — and free undo lets a
player rewind an eviction and recover that depth, reopening the exploit that row
closes. Neither depth nor undo is built; whichever lands second must resolve
this. Recorded now because the connection will not be obvious later.

**F-6 — `make board-post` command-substitutes backticks in `NOTE`.** §9 above.
The recipe should single-quote or escape. Cheap and self-contained; not done
here because this campaign was byte-identity-locked on `windows/vessel` and must
not carry unrelated tree changes.
