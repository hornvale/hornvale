# The Deed — retrospective

*Arc I.b of The Bridle. Shipped one action suite in two moods:
bare verbs in-character (gated, charged, committing), `!`-verbs
out-of-character (ungated, free, committing only when stamped). Eight tasks,
seven decision records, one honest limit recorded rather than papered over.*

## The headline: every substantive defect originated in plan text

Not one was introduced by an implementer and caught by a reviewer. Across
eight tasks the count is **fourteen defects in plan or dispatch text, zero in
implementer code that survived to review**. The earlier arcs saw the same
shape; this arc's contribution is the sharper version of it.

**Four of the fourteen were introduced by *corrections* — text written
specifically to fix an earlier defect.** That is the finding worth keeping,
because it is not explained by carelessness. Writing a correction feels like
the careful act, which is exactly when the correction goes unaudited:

| the correction | what it got wrong |
|---|---|
| a per-verb table replacing a wrong uniform rule | omitted `back`, the tenth verb — an *absence*, invisible to a diff review |
| the same table's "charge time" column | gave descriptions (`band change`) where the reader needed yes/no |
| "adding `day()` moves the type-audit report by one" | false — `day()` returns `WorldTime`, a typed quantity, so no primitive crosses the boundary |
| Step 4b's remedy, strengthening a weak guard | applied literally it produces a test that **cannot fail**, because `Ledger::commit` dedups an identical envelope |
| "delete the vacuous test; *this other one* carries its coverage" | the named replacement asserts `!text.contains("no verb")`, **which an empty string also passes** — it covers *is recognised*, not *answers*, so deleting on that basis would have left the exact hole it was closing |

The fifth has its own lesson — **"X is redundant, Y covers it" requires checking Y** — and it is the cheapest of the five to repeat.

The fourth is the sharpest: a correction whose whole purpose was to close a
vacuity hole opened a different vacuity hole. The implementer measured it — a
mutant committing on `out` left the "fixed" guard green — and repaired it by
moving the baseline inside a scout loop.

**What caught them was always something executable or independent**: a
compiler, a mutation, a re-derived grep, or a reader who had not written the
claim. Never re-reading.

## Two practices that did the work, and one new one

1. **Verify the brief against the code immediately before dispatching — one
   task ahead, never in a batch.** This found `!wait` orphaned between two
   tasks (Task 6 deferred it to Task 7; Task 7 never mentioned it) and the
   0069 violation in Task 7's Step 4, both before any code was written.
2. **Tell implementers to re-derive lists rather than trust them**, including
   lists in the text making the claim. Every implementer override in this arc
   came from that instruction.
3. **New: record a deliberate omission with its reason, not as a
   conclusion.** Task 6 declined to ship `!look`/`!knows` *because neither
   renderer had a gate to relax*. Task 7 gave those verbs a gate — which
   falsified the reason — and the omission was re-opened and reversed. Had the
   omission been recorded as "these two are out of scope," nothing would have
   detected the change. **A recorded reason is a tripwire; a recorded
   conclusion is a wall.**

## Asking the wrong question, and being redirected into a better one

The reviewer found a possessed body's committed provenance strings 100%
separable from a creature's, and framed it as: is the trail *indistinguishable*?
I brought that to Nathan as a three-way choice.

**It was the wrong question, and one of the three options could not be built.**
Nathan's concern was substitutability — Liskov — that keyboard input and a
planner be interchangeable drivers of one creature. Checking the *types*
answered it immediately: a creature is an `Npc`, a possessed body is an
`Agent`, no conversion exists anywhere in the tree, and 21 functions in the
creature layer take `&Npc`. My middle option — have the player's provenance
borrow the body's dominant drive — is unimplementable, because an `Agent` has
no drives to borrow.

Two lessons, and the second is the one that generalises:

- **The provenance difference was a *symptom*.** Fixing the string would have
  hidden a structural gap and narrowed it not at all.
- **A question phrased as a menu can be wrong in a way no option reveals.**
  Three plausible options all shared a false premise. What exposed it was
  Nathan restating the *goal* in different vocabulary, not choosing among the
  options — so when an answer feels like a choice among near-equals, the
  framing is the thing to re-derive.

Recorded as decision 0167 and `PLAY-driver-substitutability`; it promotes The
Tackle's origin-of-intent finding, which had the same shape and had been
sitting unminted in a scratch register.

## The merge driver: a ratified safeguard, inverted

Mid-campaign an artifact merged cleanly and wrongly for the third time in a
day. The cause was PROC-12's `merge=hv-regenerate` driver, which **fires, exits
0, and writes a confidently wrong file**: it regenerates over the working tree,
which mid-merge is not the merge product. Git invokes a driver only when both
sides changed a path — exactly when the two sources differ — so **every
invocation was wrong**, and the merges that came out right were the ones where
it never ran.

Two independent sessions merged the *same two commits* in opposite directions
and got opposite clean results, one dropping six primitives and the other an
entire crate's row from a default-deny audit. A competing fix (`929620343`) had
landed 30 hours earlier keying on conflicts; a scratch-repo reproduction of its
own logic showed a **clean** merge, zero conflicts, still dropping one side.
Retired in decision 0166.

**Three independent sessions found it, and the registry had a `raw` row for it
the whole time.** The Stylus minted
`PROC-merge-driver-regenerates-from-unmerged-tree` after hitting it during its
own absorption; The Chroma shipped a fail-loudly fix keying on conflicts; The
Deed reproduced it in both merge directions and retired it. None of the three
knew about the others until the queue's mouth bounced a merge. **The row was
`raw`, which is the registry working exactly as intended and nobody reading
it** — a `raw` row is a capture, not an alarm, and there is no mechanism that
raises one when a third session trips the same wire. Flipped to
`ratified (0166)` at this close.

Three process points fall out:

- **The driver's own test suite had no runner.** Nothing invoked it; the
  `outboard` set names its eight scripts explicitly and this was not among
  them. A month of silence followed.
- **It would not have caught this anyway.** A test that builds its own merge
  scenario *constructs* the working tree, closing by construction the exact gap
  the defect lives in.
- **The first merge conducted after the attribute came out conflicted** on the
  same file — stopping for a human where the driver had been emitting clean
  wrong answers. A decision whose evidence includes the first merge run under
  it is unusually well founded, and it happened by accident.

## Two smaller process findings

**A decision number must come from the allocator, not from main's top.** I took
`0160`, which sat inside another campaign's reserved block. I had re-derived it
against `origin/main` *after main moved twice* — the careful-looking version of
the exact fallback `decision-block-request.sh` forbids. **Main's top says what
landed; it cannot say what is reserved.** Renumbering also surfaced two
citation forms invisible to a slug grep: the record's own H1 carries the number
without the slug, and a Rust line continuation split the path mid-token.

**A searched test fixture can become the straggler that sets a crate's wall
time.** One discriminator cost 136.5 s and finished last of 582 tests, because
its predicate first holds at seed 28 and each world build is ~4 s. Pinning the
seed was wrong — that discipline exists because a pin rotted before. **Ordering
the search rather than shortening it** (try the last known hit, sweep the full
range behind it) took it to 6.1 s while conceding nothing: a rotted hint costs
one wasted build. The control matters, because a faster green is also what a
*broken* search looks like — deliberately mis-hinting a fixture still passed,
just slower.

## The close's own last defect, which proved a decision written an hour earlier

The final stage gate went **red** on two byte-identity tests — after a green
`make rebaseline` and a green `gate-commit`. Both are worth naming as
insufficient here: the fixtures live outside the drift check (F-6), so the
artifacts phase had nothing to regenerate and reported success over stale
files.

The merge queue attributed it to a `chamber/v3` epoch that had landed twenty
minutes before — plausible, correctly timed, and wrong. The diff was **two
lines**, and it was this campaign's own `packs.rs` gloss fix, an hour old:
`provoke` and `soothe` going from bare to sigilled.

**Which made it a live proof of decision 0172's deadline argument.** That
record asserts a concept's doc string is a save-format contract, because
`ConceptDef.doc` is a serialized field of `World.registry`. Here was the
committed seed-42 world moving by exactly those two strings and nothing else.
The fix was free only because the sole artifact carrying those concepts is a
test fixture; after merge, a real saved world would have carried them and the
same two-line change would have cost an epoch.

The queue's own diagnosis of its error is the transferable part, and it is
sharper than the incident: *"I verified WHETHER (a control proving both tests
pass on main alone) and then asserted WHY without verifying that half at all.
A mechanism that fits the timing is more convincing than either half alone,
which is exactly when it needs evidence, not less."*

## The author of a lesson repeating it, two hours later

The merge queue's best contribution tonight was naming its own error:
*"I verified WHETHER (a control) and then asserted WHY without verifying that
half at all."* I agreed, called it the keeper, wrote it into memory, and told
it the shape generalises past merge queues.

**Then I did exactly the same thing, in a commit message, within two hours.**

Fixing the clients-phase red I mutated `back`'s time charge, ran
`make vessel-check`, and saw green. I had *already* correctly explained that
green — the anti-vacuity assertion I had just written was itself vacuous,
because `go n` alone moves the clock off zero. That was the whole story. But I
then invented a *second* mechanism for it — "`make vessel-check` drives the
COMMITTED `book/src/gallery/vessel.wasm`, so the mutation never reached the
binary" — and committed it as fact.

Both halves are false, and both were one command away:

    git check-ignore -v book/src/gallery/vessel.wasm   ->  .gitignore:52
    grep 'vessel-check-run' Makefile                   ->  vessel-check-run: wasm-vessel

The wasm is **gitignored, not committed**, and `wasm-vessel` is a `.PHONY`
prerequisite that rebuilds and re-copies on every run. The mutation reached the
binary every time. I even recruited timing as corroboration — 29 s versus the
first run's 43 s, read as "it skipped the build" — when a warm rebuild measures
~14 s and 29 s is exactly what rebuilding looks like.

**The lesson survives the mechanism being wrong**, which is why it is still in
the drive's comment: a mutation that produces green needs you to prove the
mutant was under test before reading the green as evidence. But the mechanism I
named was fabricated, and a claim about which artifact a gate drives is exactly
the kind that gets believed for months. Caught only because I passed it to the
queue as a finding *for its side of the fence*, and it checked.

Two things follow that the earlier sections do not already say:

- **An explanation that arrives after the observation is already explained is
  the suspicious kind.** I did not need a second mechanism. Inventing one cost
  nothing at the time and would have cost someone a hunt later.
- **Handing a finding to someone else is a verification step.** It was not
  offered as one — I meant it as a courtesy — and it caught a defect three
  reviews and my own re-reading had not.

## Committing to `main` at the merge submission, and the guard that is not a gap

At the last step of the campaign — the one irreversible one — the shell's
working directory had silently reset to the **primary checkout**, and my next
commands carried no `cd`. So `make gate-commit`, the `Sluice-Headline` commit
and a timings commit all landed on **`main`** instead of the campaign branch.

**What stopped it was the push refusing.** `git push origin
HEAD:refs/heads/campaign/the-deed` from `main` was rejected as a
non-fast-forward. Nothing reached a remote; `main` was reset to `origin/main`
and verified at zero divergence.

**That is luck wearing the costume of diligence**, and the queue named the
reason precisely: the refusal depends entirely on the *relative position* of
the two refs. Had main's tip been a **descendant** of the campaign branch, the
same push is a fast-forward and `pre-push` allows it — main's tip would have
gone onto the campaign branch and been submitted as a merge candidate.

**`pre-commit` is NOT the gap here, and writing it up as one would do harm.**
Its worktree guard is deliberately scoped — the hook says so itself:

> Campaigns run in worktrees under `.claude/worktrees/`, and a linked worktree
> sitting on `main` is essentially always a wrong-branch mistake … **The
> PRIMARY checkout on `main` is legitimate (merges, infra, docs)**, so the
> guard keys on linked-vs-primary (`git-dir != git-common-dir`), not on the
> branch name alone.

The primary checkout committing to `main` is the sanctioned path — it is how
merges and infra land. The hook did not fail to stop me; it was never the
instrument. Filing it as a hole risks someone "fixing" it into refusing
legitimate work, which is a worse outcome than the mistake it would prevent.
I had it framed as a gap until the queue corrected me.

**The narrow, true statement of the hole:** nothing prevents pushing *main's
tip* onto a campaign branch, and `pre-push` catches it only in the
non-fast-forward case. Its tail is now closed by an unrelated change that
landed the same night — the chamber consults the mouth before taking the
flock, so such a submission is refused (`already an ancestor of base`) without
consuming the box. Before that it would have merged to "Already up to date"
and burned a run.

**The remedy I adopted, which is cheap and would have caught it:** a branch
assertion in front of every commit and push, rather than trusting the
directory —

    git branch --show-current | grep -qx 'campaign/the-deed' || { echo ABORT; exit 1; }

Two memories about re-anchoring the working directory already existed and did
not save me, because the reset is silent and arrives between commands rather
than inside one. An assertion at the point of use beats a habit.

## What this arc did not do

**Seven of the fourteen concepts it minted are inert.** `chart`, `know`,
`look`, `read`, `sense`, `wait` and `write` render `Gap | Gap` for every
species in every world, because nothing grants them `Steeped` or `KnowsOf` —
no culture can speak the acts a possessed body performs. Task 2 flagged this
and handed it to "whichever task attaches `Action` variants (5-7)". **Tasks 5,
6 and 7 each shipped without deciding**, which is precisely the failure this
retrospective's own practice #1 exists to catch: `!wait`, deferred by Task 6
to a Task 7 that never mentioned it. A finding deferred to a plural, unnamed
set of future tasks is deferred to nobody. It may not be a defect — a culture
that has never charted may rightly lack the word — but it is an unmade
decision that shipped. `LANG-in-character-acts-are-unspeakable`.

At the body level a possessed body is still not a creature: no drives, no
affect, no occupancy presence, two committed predicates against a creature's
five. Decision 0167 records it as a must-fix deferred to its own campaign.
`KNOW-commit-read-same-instant` records a second: `Ledger::commit` quantizes a
fact's day upward, so a fact is invisible to a read at the instant it was
committed — pre-existing, narrow, and made far more reachable by this arc,
since every charged act now leaves a fractional day.

## Follow-ups

- `PLAY-driver-substitutability` — the must-fix above (decision 0167).
- `KNOW-commit-read-same-instant` — the quantize hazard (F-5).
- **F-6, RESOLVED THE OTHER WAY — and the recommendation this retrospective
  originally carried was wrong.** It said `cli/tests/fixtures/`,
  `windows/vessel/tests/fixtures/` and `windows/worldgen/tests/fixtures/`
  should be declared in `docs/generated-paths.txt`. **They should not**, and
  the reason is a distinction this campaign missed: those three are
  **assertions**, not generated inputs. `regenerate-artifacts.sh` mentions
  them **zero** times; `kernel/src/golden.rs` compares them and requires a
  deliberate `REBASELINE=1` to accept drift. By contrast
  `clients/game/core/tests/fixtures/`, which *is* declared, is written by
  `regenerate-artifacts.sh` — a cached input.

  Declaring the three would be **inert today** (regeneration never writes
  them, so the diff is permanently empty) and **harmful the moment anything
  did**: the artifacts phase *commits* the drift it finds, which would
  silently rebaseline a determinism golden and land it green. **The gate
  failing on a stale golden is not the gap — it is the design**, and it is
  exactly what caught this campaign's first red.

  Sharper still, and worse for the original recommendation:
  `cli/tests/fixtures/` holds the frozen `pre-<campaign>` historical pins,
  which `golden.rs`'s own header says are **not goldens at all** — *"their
  bytes must never track current code, so they are compared directly and have
  no accept path."* Declaring that directory is not merely inert, it is a
  category error.

  My stated wrinkle was wrong too, in the same direction: I wrote that the
  drift check "would go red and `make rebaseline` would not fix it." It would
  not go red. It would go **permanently, silently green**.

  **The half that stands, and it is the one worth keeping:** nothing prompts
  `make rebaseline-goldens`, and the golden tests are the only thing standing
  there. That is what the first red actually demonstrated.

  Fixed on main by the merge queue as an executable criterion rather than a
  comment — every declared path must appear in `regenerate-artifacts.sh`,
  derived rather than hardcoded, mutation-checked by declaring one of the
  goldens and watching it fail.

- **F-3** (inherited): `purview_scene`'s ungated NPC marks — reaching it from
  inside a chamber discloses a creature the chamber band withheld. This arc
  built the gate table where the structural fix would live but did not close it.
- **F-4**: per-session predicate doc strings are de-facto save-format contracts
  the moment a played world is saved. True today, undocumented today, and this
  arc widened the set.
- **The composite universe rule is hand-tracked at two sites.**
  `!is_unnameable(..) && !is_extradiegetic(..)` appears in `cli/src/proto.rs`
  and in a worldgen golden test. `is_extradiegetic` was hoisted into
  `domains/language`; `is_unnameable` **cannot** be — it is `&World`-keyed and
  cannot move below `cli` in the layering. So a third exclusion added to
  `proto_root_universe`'s filter needs manual addition at both sites, silently.
- **Adding N concepts moves the trope audits by exactly N**, in both the token
  count and the unrequired-token count, because tropes resolve against the
  concept registry. This campaign's drift table missed it and three trope
  artifacts moved unannounced. Stated here so the next concept-adding campaign
  can predict it instead of discovering it.
- **The published concept manifest's backlog now lists permanently unclosable
  entries.** The seven operator instruments sit in its "Unnamed (lexeme gap)"
  list, but an extradiegetic gap can never close (decision 0172) — so a page
  that reads as a to-do list makes a promise it cannot keep.
- **The Bridle's seam-guard request is answered by silence, which is the wrong
  answer.** Board post 174 proposed `plan_to_water`/`plan_to_room` in
  `windows/vessel/src/action.rs` as seam registrations and named The Deed as
  owner, on the reasoning that the arc changing behaviour there should arm the
  tripwire. No `seam-guard:` tag exists under `windows/vessel/src/` and no
  vessel row exists in the roster. Declined by omission rather than on the
  merits; an inherited request answered by silence will simply be re-asked.
- The CLI hint path for retired bare forms — a bare group-A verb typed from
  habit gets an ordinary unknown-verb refusal rather than "did you mean `!why`?"
- The world REPL (`cli/src/repl.rs`) keeps bare `help`/`why`. A different
  surface, outside the 26 verbs, so not a defect — but the two surfaces now
  spell the same word differently.
