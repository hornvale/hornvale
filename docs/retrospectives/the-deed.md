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
- **F-6**: **three fixture directories** move with the sim and are not
  drift-checked — `cli/tests/fixtures/`, `windows/vessel/tests/fixtures/` and
  `windows/worldgen/tests/fixtures/`. None is in `docs/generated-paths.txt`,
  and `regenerate-artifacts.sh` never writes them; only
  `make rebaseline-goldens` does, and nothing prompts you to run it. The
  structural remedy is to declare them, with a wrinkle worth inheriting rather
  than rediscovering: the drift check would then go red and `make rebaseline`
  would **not** fix it, because the writer is a different command. A red with a
  known remedy still beats silence, but it is a change to shared machinery and
  should not arrive on a campaign close.
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
