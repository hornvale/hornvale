# Decision ledger — The Wicket (2026-09-01)

Campaign: `campaign/the-wicket` · Registry row: `MAP-one-kind-model` ·
Spec: `docs/superpowers/specs/2026-09-01-the-wicket-design.md`

Autopilot is engaged (`campaign-autopilot`). G1/G2/G4/G5 auto-resolve and
land here; G3 and G6 are hard stops.

---

#1 [Q] — **What is the campaign named?** · Decision: **The Wicket** (a small
gate standing in front of open ground) · Why: `the-orange`, the obvious name,
is already the name of a `repertory/` scene (`repertory/the-orange.scene.json`,
founded by The Repertory, currently UNWITNESSED), and a campaign sharing a
corpus item's slug would make every future grep ambiguous — precedent is the
standing "grep the campaign name before adopting it" rule, applied here and
confirmed by `grep -ril` over `docs/`, `book/`, `repertory/` ·
Alternatives discarded: The Orangery, The Grafting, The Rootstock, The Nursery,
The Hasp (all free; none names the thing being retired as exactly) ·
ideonomy passes / overturns: 0 / 0 — naming is not a design decision and no
pass was run for it · Capture: none needed.

#2 [G1] — **Which of MAP-one-kind-model's three additions does this campaign
ship?** · Decision: **the first only** — retire the closed `AnchorKind` gate
so an object kind is a row rather than a variant; plus the totality mechanism
that replaces the compiler, plus one placed kind the enum could not have
named · Why: the frontier essay's own "order of least regret" states the
sequence (closedness → edges → per-instance derivation) and argues explicitly
against landing all three at once — *"three working mechanisms would be put at
risk to serve two missing ones, and the two are additive on the three."* That
is precedent in an `elaborated`/high registry row and its linked essay ·
Alternatives discarded: (B) bundle kind-to-kind edges into the same campaign —
the ideonomy pass found a real coupling argument for it (see below) but 15
kinds do not yet make hand-authoring expensive, so the coupling is a reason to
sequence edges NEXT, not to bundle them; (C) widen the enum or add an escape
hatch — leaves two kind models, which is the defect itself ·
ideonomy passes / overturns: 1 / 1 (framing overturned — see #3) ·
Capture: the edges/inheritance coupling is recorded in the spec's §8 and will
become a registry row amendment at close.

#3 [G1] — **What is the campaign's deliverable, stated as one thing?** ·
Decision: **registry totality replaces compile-time totality**; deleting the
enum is the consequence, not the headline · Why: the ideonomy `purpose` prompt
separated `AnchorKind`'s two inherited purposes — it is the pattern grammar's
vocabulary AND the guarantee that every placeable kind has a noun, a detail and
a thing-kind. Only the second is load-bearing, and closedness is merely a means
to it. The immunology re-instantiation (germline-encoded receptors vs. V(D)J
recombination, which is survivable only because of negative selection) says the
same thing structurally: an open vocabulary is safe exactly to the degree it has
a default-deny gate. This decides the whole test strategy, so it is the
headline · Alternatives discarded: framing the campaign as a refactor
("s/AnchorKind/KindId/"), which would have shipped the re-key with no
replacement for the three exhaustive matches' guarantee ·
ideonomy passes / overturns: 1 / 1 · Capture: spec §5 is written from this
framing.

#4 [Q] — **Does the campaign have to PLACE a new kind, or is an open
vocabulary enough?** · Decision: **it must place one** ·
Why: decision 0398 (*a capability nothing can reach is not a capability*) is
directly on point and was minted three days ago against this exact shape — The
Chattel shipped a container, a lock, a key and an `open`/`close` pair that no
session in any world could stand in front of · Alternatives discarded: prove
openness with a test-only kind (cheap, moves no artifact, and is precisely the
"capability nothing reaches" 0398 refuses) ·
ideonomy passes / overturns: 1 / 0 (the faceted-classification
re-instantiation independently reached the same place: a faceted catalogue can
compose infinitely and still has to shelve the book) ·
Capture: spec §6; the artifact cost is a G3 flagged item.

#5 [G2] — **Do the frontier essay's measured numbers survive checking?** ·
Decision: **no — they are wrong and the campaign corrects them** ·
Why: verified by command, not by reasoning. `AnchorKind` has **15** variants,
not thirty-four, and had 15 at `208efe4f1`, the essay's own commit; there are
**3** exhaustive match sites (`chamber_prose::noun`, `chamber_prose::detail`,
`affordance::thing_kind_of`), not thirty-six; "seventeen files" is 18 source
files / 22 including tests. The fence is ~2.3x smaller than both the essay and
the `MAP-one-kind-model` row advertise · Alternatives discarded: quietly using
the corrected numbers without amending the book — a published essay stating a
false measured fact produces wrong cost estimates from readers acting in good
faith, which is the failure mode CLAUDE.md's own loud-correction paragraph
exists for · ideonomy passes / overturns: 0 / 0 — a measurement, not a design
decision; no pass was run for it · Capture: the essay and the registry row are
both amended as part of the DoD sweep (spec §11).

#6 [Q] — **Is `Sleep` actually gated on `SupportsRest`?** · Decision: **no —
and three doc comments say it is** · Why: read the code rather than the
comments. `Session::sleep` (`session.rs:2536`) refuses a non-empty argument,
charges the clock, commits `rested` and sets `wake_at`; there is no bed check
and no home check. `Session::warm` (`session.rs:2624`) DOES gate, on
`offered_to_observer`, which is what a gated verb looks like here. The creature
layer already agrees with the ungated reading and says so in its own doc
(`liveness.rs:2293`, *"sleeps where it is — its proposal is always `Rest`"*),
and it already reserves a hook for a rest-QUALITY refinement. So
`SupportsRest` reaches only the advertisement layer · Alternatives discarded:
trusting `OfferedVerb::Sleep`'s doc ("gates on `SupportsRest`") and
`Action::Rest`'s doc ("precondition: at home"), which is what an earlier draft
of the spec did — it justified the `kneeler` proof kind out of them and asserted
"a shrine offers nowhere to rest", which is false ·
ideonomy passes / overturns: 0 / 0 — a measurement · Capture: the three
corrections are Task 4's, spec §6a.

#7 [Q] — **Nathan's ruling: sleep is never gated; the place GRADES it.**
Raised by Nathan directly, 2026-09-01, so this is his call and not an
auto-resolved gate. Decision: **the campaign leaves the space and does not
build the mechanism** · Why: the space is free — after this campaign a kind is
a `KindId` with open tables behind it, so a `ComponentStore<KindId, RestQuality>`
is a new table and nothing else — while building the grade would pull in a
`(species, thing)` relation, the drive's site-selection, and a census readout,
which is a campaign · Alternatives discarded: (i) build the grade now (three
subsystems, and it would put the re-key at risk for a mechanism that is additive
on it — the essay's own argument); (ii) note it and change nothing, rejected
because the three false comments actively mislead, as demonstrated by this
campaign's own first draft · ideonomy passes / overturns: 0 / 0 — Nathan's
ruling is the input, and the design question it left (what "leaving space" means
concretely) was answered from the campaign's own thesis rather than by
expansion · Capture: `PSY-rest-quality-is-a-grade-not-a-gate` and
`PSY-rest-site-is-a-tuning-indicator` written to the idea registry; spec §6a;
a decision record at close.

#8 [G1] — **Which kind proves the vocabulary is open?** · Decision: **a
`brazier` carrying `RadiatesHeat`, replacing the `kneeler`** · Why: #6 removed
the kneeler's justification — `SupportsRest` advertises and does not grant, so a
kneeler would have added a listing rather than a capability. `warm` IS enforced
(`session.rs:2624`), `hearth` is its only carrier, and a hearth is confined to a
hearthroom by `the-fire` requires `Alcove` requires `roles: &[Role::Hearthroom]`
— so no shrine, hall, smithy or storeroom can be warmed at. `Session::warm`'s
own doc names the anticipated future carrier, *"a cauldron of coals on
`AnchorKind::Vessel`, say"*, and says such a carrier would have needed a
dispatcher edit before the fix that landed with it — so the brazier arrives to
find the edit already unnecessary, which is the campaign's claim demonstrated
rather than asserted · Alternatives discarded: the kneeler (#6); a wilderness
carrier, which closes a bigger gap but redraws every outdoor interior in every
world (`MAP-wilderness-affords-no-rest`) · ideonomy passes / overturns: 1 / 1 —
the pass that overturned the kneeler was the one that asked which verbs are
enforced rather than which are advertised · Capture: spec §6.

#9 [Q] — **Nathan's ruling: wait, rest and sleep are three acts and must stop
being conflated; absorb it into this campaign.** Raised and scoped by Nathan
directly, 2026-09-01, so the scope call is his and not an auto-resolved gate.
Decision: **absorbed, as stage 5, with a stage gate on the sluice at both the
stage-4 and stage-5 boundaries** · Why: he ruled the golden and census refresh a
normal pre-alpha cost rather than a constraint, and asked for the queue to flush
out unexpected effects. The two halves are near-independent — the act split
needs a graded recovery stock, not the re-key — so absorbing costs sequencing
risk, not design coupling, and the stage gate is what prices that risk ·
Alternatives discarded: (i) pivot to the acts and queue the kind model behind
it, which was my recommendation on the grounds that the conflation produces
wrong behaviour today while the re-key pays off later; (ii) finish The Wicket
and take the acts next, which would have authored one more universal constant
that stage 5 then converts to a row · ideonomy passes / overturns: 0 / 0 — the
scope call is Nathan's and the options were laid out for him rather than
expanded · Capture: spec §6b, stages 4-6.

#10 [Q] — **Nathan's generalisation: essentially everything should vary by
species, and often by individual; watch for constants that should be
components.** Decision: **recorded as `TOOL-authored-scalar-should-be-a-
component`, and it re-frames the campaign's own thesis** · Why: the measured
instance is `FATIGUE_RISE = 0.3` — one sleep-debt rate for every species in
every world, multiplied by `.as_std_days()` when the quantity is per planetary
day. It is the same defect as `AnchorKind`: a world-fact fixed at compile time
that should be a row. So the campaign header is rewritten to say that outright
rather than treating stage 5 as an annexe. The row also records the ladder,
which is `MAP-one-kind-model`'s three additions seen from the numeric side:
constant → per-species row (addition 1) → kind-to-kind edge (addition 2) →
`Lineage`-derived individual value (addition 3). Sleep therefore exercises all
three, which the orange also does, but sleep is nearer to shipping ·
Alternatives discarded: treating it as a passing remark and not recording it,
against the capture invariant; and building the sweep in this campaign, which
would be a third subsystem in an already-widened scope ·
ideonomy passes / overturns: 0 / 0 — Nathan's ruling is the input; the ladder
observation came from mapping it onto the registry row already open ·
Capture: the registry row; spec header and §6b.

#11 [G4] — **The compiler does not enumerate the re-key's full worklist.**
Found by the pre-dispatch brief verification for Task 2 (the
`dispatching-hornvale-subagents` step that greps a brief's claims against the
code), which is exactly the class of defect that step exists to catch, one task
ahead rather than five. Decision: **Task 2 gains a Step 4b, a grep sweep with a
zero end-state** · Why: the plan's Step 4 said "follow the compiler", and the
compiler is blind to two whole categories here. Measured: `snapshot.rs` (3
mentions), `light.rs` (1) and `thing.rs` (2) reference `AnchorKind` **only in
prose**, so they compile clean after the deletion; and at least ten mentions
across `chamber_prose.rs`, `session.rs`, `affordance.rs` and
`tests/suite/affordance.rs` are intra-doc links, which are a rustdoc lint and
therefore invisible to `clippy --all-targets -- -D warnings`, the only lint the
gate runs · Alternatives discarded: trusting `-D warnings` to catch broken
intra-doc links (it does not — different tool); adding `cargo doc` to the gate,
which prices a whole new build into every commit to catch a class of defect a
grep catches for free · ideonomy passes / overturns: 0 / 0 — a measurement ·
Capture: plan Task 2 Step 4b, with the rule that a load-bearing explanation
moves to `domains/thing` beside the roster ratchet rather than being deleted.

Ruling: a campaign whose stated purpose includes correcting stale comments must
not emit ten new ones. Cost if wrong: the sweep costs a few minutes and could in
principle churn prose the reviewer then has to read; that is a far cheaper error
than shipping doc links pointing at a type the campaign deleted.

#12 [G4] — **Plan defect: the Task 1 code block omitted a type-audit tag.**
Found by accident — I ran `make gate-commit` over the tree while Task 1's
implementer was mid-edit, and it reported `thing:135: untagged primitive at
EVERY_HANDLE`. Decision: **the tag is added to the plan's code block and the
rule is promoted into Global Constraints**, and the resolution was sent to the
live implementer rather than left for it to rediscover · Why: `tools/type-audit`
is default-deny at `pub` boundaries and is the commit gate's third step, so any
new `pub const` holding a primitive fails the gate until tagged. The plan's
Global Constraints named `missing_docs` but not the type audit, which is the
stricter of the two and the one that actually reddens · Alternatives discarded:
letting the implementer discover it (it would have, at its own gate run — but
the cost of telling it is one message and the cost of not telling it is a fix
round) · ideonomy passes / overturns: 0 / 0 — a measurement ·
Capture: plan Global Constraints and Task 1's code block.

Ruling: the controller must not run `git add -A` in a worktree a subagent is
working in — it staged the implementer's half-finished file into a docs-only
commit and then ran the gate against it. Unstaged with `git restore --staged`,
which leaves the working tree untouched, so the implementer lost nothing.
Commit with an explicit pathspec while any child is live. Cost if wrong: a
controller commit contains someone else's unfinished work and a red gate blocks
it — recoverable, but it also risks committing a half-applied edit if the hook
had passed. Saved to memory as `never-git-add-all-while-a-subagent-works`.

#13 [G5] — **I landed a red on the branch and Task 1's implementer found it.**
The four idea-registry rows written at #7 and #10 blew
`docs_consistency::registry_idea_cells_are_within_budget`'s 600-char cap on the
Idea cell (1337 / 972 / 763 / 1355). Decision: **compact all four to ≤600, do
not waive** · Why: the waiver fixture is append-never by construction — its own
doc says "a new row over the cap is a failure, not a fixture edit — that ratchet
is the whole mechanism" — so waiving would have been the cheapest repair that
deletes the check, the exact anti-pattern the cap exists to prevent. The cap's
rationale also settles WHERE the cut prose goes: "a row is a shelf-mark: what
the idea is, and a pointer to where it is argued", and the argument for all four
already lives in the spec the Where cell links · Alternatives discarded: adding
four waiver entries; moving the rows to `frontier.md` sections and flipping
`raw` → `elaborated`, which the test itself offers as an option but which would
claim a maturity these ideas have not earned in one day ·
ideonomy passes / overturns: 0 / 0 — a repair · Verified: the four cells now
measure 571 / 554 / 586 / 568, and the full `docs_consistency` module is 28
passed / 0 failed.

Ruling: **this campaign runs `cargo test -p hornvale --test suite -- docs_consistency`
after any edit to `book/src/frontier/idea-registry.md` or `docs/`.** The
pre-commit hook skips `make gate-commit` when no Rust-relevant path is staged,
so every docs-only commit on this branch has been ungated — while
`docs_consistency` and `generated_paths` READ exactly those files. That is a
known trap I had in memory and did not apply, and the cost fell on an
implementer who had to prove the red was not its own. Cost if wrong: a few
seconds per docs commit.

#14 [G5] — **Ruling: `chamber_prose::noun` takes `&str`, not `KindId`, and the
plan was wrong.** Task 2's implementer reported the contradiction rather than
silently following the plan, which is the right call and is why the plan is
being corrected instead of the code · Why: `KindId(pub &'static str)` requires a
`'static` label, and three of `noun`'s callers read the ledger, where
`Ledger::kind_of` returns `Option<&str>` borrowed from a `String` in the fact
store (`kernel/src/ledger.rs:541`). A runtime slice cannot be wrapped in a
`KindId` at all — which is exactly why the retired `noun_for_label` existed, and
my plan deleted the function without noticing it was solving a lifetime problem
rather than a naming one · Alternatives discarded: (i) unify both accessors on
`&str`, which loses `detail`'s typo-safety at every interior call site for the
sake of surface symmetry; (ii) relax `KindId` to a non-`'static` lifetime, a
kernel change with workspace-wide blast radius, refused outright ·
ideonomy passes / overturns: 0 / 0 — a type constraint, not a design choice ·
Capture: plan Task 4's Produces block now states the asymmetry and requires it
be documented at the table, so a later reader does not "tidy" the two accessors
into one. Cost if wrong: none — the alternative does not compile.

#15 [G5] — **Ruling: the frozen verb table sweeps `THING_KINDS`, not
`EVERY_HANDLE`.** Task 2 flagged, in the code's own doc, that
`every_named_kind()` is "one step weaker than it was" because `AnchorKind::ALL`
was generated and `EVERY_HANDLE` is hand-written · Why: Task 1's
`every_named_handle_is_a_roster_row` enforces only *named ⊆ rostered*, so a
handle dropped from `EVERY_HANDLE` silently narrows every sweep measured against
it — a size ratchet that any compensating edit passes. `THING_KINDS` is frozen
as an ORDERED SET and cannot go short, so sweeping it is strictly stronger and
introduces no new mechanism. The handles exist for *code that names a kind*; a
sweep is not that · Alternatives discarded: adding a converse
*rostered ⊆ named* check, which would contradict Task 1's deliberate design
that a kind needs no handle; asserting `EVERY_HANDLE.len()`, which is a count,
and a count is not a membership · ideonomy passes / overturns: 0 / 0 ·
Capture: plan Task 3 Step 1d, including the instruction that if the swap
reddens the table, the disagreement is the finding and the table must not be
edited to match. Cost if wrong: a sweep runs over 16 kinds instead of 16 — the
sets are identical today, so the change is a guarantee, not a behaviour change.

#16 [G5] — **Ruling: the artifact branch table is a RULE, not an enumeration.**
Task 2 hit a generated file no branch listed — `book/src/reference/layering-generated.md`,
which gained vessel's new `hornvale-thing` dependency row — and accepted it with
`make rebaseline-goldens`, correctly · Why: my branch tables enumerated three
outcomes and the world had a fourth, which is the failure mode a compressed
branch table always has. The question that actually decides it is *world-derived
or source-derived*: a source-derived page moving for a statable reason is
expected and regenerates; a world-derived artifact moving is STOP whether or not
it is listed. The layering page is authored by the layering enforcer from the
manifest, so a dependency edit moving it is the mechanism working ·
Alternatives discarded: extending the enumeration with a fourth row, which
would leave the fifth case just as unhandled ·
ideonomy passes / overturns: 0 / 0 · Capture: plan Task 5's branch table gains
the rule; Task 7's inherits it by reference. Cost if wrong: an implementer
regenerates a source-derived file that should have stopped the task — bounded,
because the STOP half is stated by category rather than by list.

#17 [G5] — **Task 2 found two guards that could never fire.**
`no_verb_by_object_table_exists` and `no_hardcoded_anchor_kind_gates_warm` both
searched source text for a literal (`AnchorKind::`) that the re-key makes
impossible anywhere, so both would have read green in every possible tree
forever. Repointed at `kinds::`. Recorded here rather than only in the task
report because it is the campaign's second instance of the same shape — the
first was `SupportsRest` gating a verb that never consults it — and two
instances in one campaign is a pattern worth carrying into the retrospective:
**a guard written against a spelling outlives the spelling.**

#18 [G5] — **Ruling: six Minor findings ride the fix round rather than being
deferred.** The SDD default is that minors never enter the fix loop and go to
the final review's triage · Why the exception: the loop is already open for the
Important finding, so folding in six one-line corrections extends nothing — and
one of them is a **knowingly false doc comment** (`tableau.rs:38`, "the
vocabulary of things is currently a closed enum", now definitively untrue). This
campaign's own §6a deliverable is correcting three comments that describe a gate
nothing enforces; leaving a fourth false comment in place, having been told
about it, would make the campaign incoherent with itself. The remaining five are
a dead rebinding, an off-by-one in a header count, a stray trailing comma, a
bare literal where a handle exists, and a citation whose command does not do
what it claims · Alternatives discarded: strict deferral, which would carry a
false statement through to a final review that may or may not triage it back ·
ideonomy passes / overturns: 0 / 0 · Capture: the deferred one — a test whose
name no longer describes it — stays deferred, because renaming a test edits
`docs/timings/subfloor-roster.tsv`, which selects by exact name.

#19 [G5] — **The doc sweep missed a false claim because the claim never says
`AnchorKind`.** `tableau.rs:38` reads "the vocabulary of things is currently a
closed enum"; the implementer edited line 39 of the same hunk and the grep did
not fire, because the sentence names the concept without naming the identifier.
This is the campaign's **third** instance of one shape — a check written against
a spelling rather than against a meaning. The first: `SupportsRest` advertising
a gate no dispatcher consults. The second: two guards scanning for a literal the
re-key makes impossible. Now a grep sweep that cannot see a false sentence about
the very thing it is sweeping for. Carry all three into the retrospective as one
finding, not three. Cost of the miss here: one stale sentence, caught by review.

#20 [G5] — **An incidental measurement in Task 2's fix round may invalidate
Task 5's proof kind.** Choosing an isolating mutation, the implementer found
that `Role::Shrine` occurs **zero times in any flagship a possession starts
at** — misspelling `the-altar`'s kind reddens one test where misspelling
`the-water-jar`'s reddens four, because no behavioural test renders a shrine ·
Decision: **Task 5's Step 1 now measures existence AND reachability separately,
with a three-way branch** · Why: the brazier was chosen (#8) precisely because
`warm` is enforced and no shrine can be warmed at. If no session can reach a
shrine, the brazier is decision 0398's failure in its original form — a
capability nothing can stand in front of — and this campaign would ship the
exact defect it cites as its justification. The signal is not yet the answer:
"flagship a possession starts at" is a narrower population than "every chamber
in every world" · Alternatives discarded: pre-emptively switching the proof kind
to a reachable role now, which would discard a measurement the campaign has
already half-made and would pick the replacement blind ·
ideonomy passes / overturns: 0 / 0 — a measurement, and the branch table is
the response to it · Capture: plan Task 5 Step 1, including the instruction
that a null is a publishable result — "shrines are unreachable at the census
seeds" is worth more than a brazier nobody meets, and it would retire a role
the grammar currently pays for.

#21 [G5] — **Ruling: the brazier moves from `Role::Shrine` to `Role::Loomroom`,
and the spec is corrected mid-campaign.** Task 2's re-reviewer, checking an
out-of-scope claim I asked about, found the corroborating measurement already
committed in the file this campaign edits: `interior/pattern.rs` records a
48-seed sweep run through `possess --seed N --script` finding the role at
chamber index 2 is `Role::Loomroom` in **24 of 24** structures that have an
index 2, and `Role::Smithy`, `Role::Hall` and `Role::Shrine` at **zero** ·
Why this is a correction and not a preference: that same comment states the
consequence in the general form — *"a gate whose predicate is false everywhere
is not a gate, it is a deletion"* — and cites decision 0398 while doing it. A
brazier in a shrine is a brazier in no world. The campaign that cites 0398 as
the reason it must place a kind at all would have shipped 0398's exact defect ·
Alternatives discarded: (i) leaving the shrine and letting Task 5's Step 1
discover it, which wastes a task and risks the measurement being skipped
precisely because it was expected to pass; (ii) `Role::Smithy`, thematically
the best home for a coal pan and measured at zero, the same trap one step over ·
ideonomy passes / overturns: 0 / 0 — a measurement already in the repo ·
Capture: spec §6 carries a loud correction quoting the sweep; plan Task 5
re-pointed at `the-loom` with `requires: Some(kinds::LOOM)`, and its Step 1
kept — reframed as CONFIRM rather than discover, with the note that a
measurement you expect to pass is the one you skip.

**The failure this records is mine and it is a repeat.** The project's standing
rule is to grep before proposing, and the evidence was not in a decision record
or a retrospective but in a comment beside the `INVENTORY` table the campaign
was already editing — read by two implementers and a reviewer before anyone
connected it to the brazier. Cost if the ruling is wrong: the loomroom is the
only role the sweep found, so the alternative is no placed kind at all, which
would forfeit the campaign's 0398 obligation.

#22 [G5] — **Ruling: Task 3 declines the bare-`KindId("` production guard;
the census the brief predicted does not match what is actually there.**
`git grep -n 'KindId("' -- windows/vessel/src domains/thing/src | grep -v
'kinds::' | wc -l` returns **58**, by file:

```
domains/thing/src/lib.rs          35
windows/vessel/src/affordance.rs  19
windows/vessel/src/underground.rs  2
windows/vessel/src/session.rs      1
windows/vessel/src/chamber_prose.rs 1
```

Why declined, and why the brief's own two branches ("a handful, all in
tests" / "many, spread through production") both mis-describe the shape:
**zero of the 58 sit at a production CONSUMER site.** Every occurrence is one
of three things — a kind's own AUTHORING table (`thing_registry` and the
`kinds` handles module in `domains/thing/src/lib.rs`, 35; `object_registry`
in `windows/vessel/src/affordance.rs`, 19), a test module (`underground.rs`,
`session.rs`, 3), or a doc comment (`chamber_prose.rs`, 1). A guard on the
model of `thing_kind_of_has_no_wildcard_arm` — scanning production source for
a forbidden spelling — would need an exemption covering `thing_registry`
(which necessarily spells its own literals; a handle keyed off the row that
defines it is circular) and `object_registry` (Task 2 left this table
spelling bare literals rather than `kinds::X`, and re-keying it is out of
this task's scope) before it could pass today. That is a table-shaped
allow-list of exactly the kind CLAUDE.md warns nobody maintains, and it is a
different design from the "no bare `KindId(` outside the handles module"
rule the brief describes — the brief's model does not fit the census it
asked for.

Decision: **no guard this task.** The residual risk the brief worried about
— a typo'd literal at a call site — is not what the census found; both gates
in this file (G-a, G-e) already close the two AUTHORING tables' error mode
directly (a bad literal in `INVENTORY` or `object_registry` is caught by
running the two new tests, not by a source-text scan), and the guard's real
job — keeping `object_registry` and `thing_registry`'s own literals honest —
is exactly what G-a/G-e already do, from the other direction (checking the
literal against the roster, not checking that no literal was written).
Converting `object_registry` to `kinds::X` handles, if wanted, is a follow-up
with its own task, not a guard that ships pre-loaded with a two-table
exemption list · Alternatives discarded: writing the guard with the two-table
allow-list anyway, which launders an unmaintainable list into the campaign on
day one · ideonomy passes / overturns: 0 / 0 — a census, not a design choice ·
Capture: none needed; this entry is the record. Cost if wrong: a bare literal
typo'd at a genuine future consumer site compiles silently, same residual
risk the brief named — bounded, because G-a and G-e already cover every
literal in the two tables that currently hold one.

#23 [G5] — **A vacuity guard that is itself vacuous, and a comment that
overclaims — the fourth instance of one shape.** Task 3's review found
`assert!(checked >= INVENTORY.len(), "... the loop is not running")` is
satisfied by `0 >= 0` if `INVENTORY` were empty, while its message asserts the
opposite; and `assert!(!reg.is_empty())` passes with one row where there are
nineteen. Both are **this plan's own literal code**, not an implementer
deviation · Decision: routed to Task 4 Step 1a as plan work rather than opened
as a fix round — Task 4 appends to the same file, and SDD keeps minors out of
the loop · Why absolute counts: the retired correspondence test used an
absolute floor and Task 2 set the better precedent inside this campaign
(`assert_eq!(checked, 38)`). An exact count is a ratchet whose cost is a
deliberate edit when Task 5 appends a pattern — the same cost `FROZEN` already
carries · Alternatives discarded: leaving it to the final review, which would
carry a knowingly-false comment through the sluice stage gate ·
ideonomy passes / overturns: 0 / 0 · Capture: plan Task 4 Step 1a.

**The pattern is now four deep and it is the campaign's real product.** Same
shape every time — *a check whose stated guarantee exceeds what it can
actually detect*: (1) `SupportsRest` advertising a gate `Session::sleep` never
consults; (2) two source-scanning guards hunting a literal the re-key made
impossible; (3) a doc sweep that could not see a false sentence because the
sentence named the concept without naming the identifier; (4) a vacuity guard
whose message claims it cannot pass vacuously. Two of the four are mine, in
plan text. Carry this into the retrospective as ONE finding with four
instances, not four findings — the instances are cheap and the shape is what
generalises.

#24 [G5] — **Ruling: the gallery transcript moving is PROCEED, not STOP, and my
branch table was wrong.** Task 5's implementer stopped, correctly, because
`book/src/gallery/possession-carry-seed-1.md` moved:

```text
-A small room, holding a doorway, a water jar, a loom and a key.
+A small room, holding a doorway, a water jar, a loom, a key and a brazier.
```

Why proceed: that line **is** the campaign's deliverable. Decision 0398 says a
capability nothing can reach is not a capability; this is a body walking into a
loomroom in a real world and finding a brazier it can warm itself at, produced
by a diff of data rows and no dispatcher edit. Stopping on it would have been
stopping on success · The defect is mine: the STOP-on-gallery rule was written
for Task 2, whose premise is that a behaviour-preserving refactor changes
nothing, and I carried it into a task whose entire premise is that exactly one
thing changes. **A branch table is not portable between tasks with opposite
premises** · Alternatives discarded: treating the move as an epoch, which would
forfeit the campaign's 0398 obligation to place something reachable ·
ideonomy passes / overturns: 0 / 0 · Capture: plan Task 5's branch table now
distinguishes a RENDERED ARTIFACT (regenerated from a live walk; may show
anything the world contains) from a LEDGER FOLD (a census column, which must
not move because a room gained furniture — decision 0069 keeps `Interior`
unserialized).

#25 [G5] — **`INVENTORY`'s epoch rule was already stale before this campaign,
and the implementer found it.** Its doc states that appending a pattern with
`at_locale: false` is LATENT because "the only other consumer is
`selection_for`, whose output is read by the chamber renderer and by nothing
that commits", becoming an epoch "on the day something that commits reads a
chamber". Verified: `book/src/gallery/possession-carry-seed-1.md` has rendered
a delved chamber's composed contents since **2026-08-30** (`b8fc0cd02`,
`26ebaf7e4` — The Chattel and The Custodian), so a chamber's contents have
reached a committed, drift-checked artifact for two days before this campaign
began · The doc is not wrong so much as **imprecise in the load-bearing word**:
it conflates *commits* (enters the ledger) with *appears in a committed
artifact*. The first is still false and is what decision 0069 guarantees; the
second has been true since August 30 · Decision: correct the doc as part of
Task 5, stating both halves — no world file moves, and a rendered artifact may
· ideonomy passes / overturns: 0 / 0 · Capture: Task 5 amends
`INVENTORY`'s doc; the retrospective carries it as a finding about a doc whose
invariant decayed silently because nothing tested the sentence.

#26 [G5] — **A seventh ratchet my enumeration missed, found only by running.**
`domains/thing::concept_doc()` ends in `other => unreachable!`, so a roster kind
with no doc arm panics **every world genesis**. My six-ratchet list was built by
grep over count assertions and could not see a panic reachable only at runtime.
The implementer found it, added the arm, and flagged it as unlisted. Recorded
because it is the counter-example to the enumeration I was pleased with: a list
of ratchets derived from static reading is a floor, never a total. Cost if it
had been missed: every world fails to build, which is loud — the dangerous
version of this defect is the one that does not panic.

#27 [G5] — **An eighth ratchet, and the campaign's cost claim is corrected in
the spec.** Task 5 found, by running, that registering a kind as a CONCEPT
obligates an accession slot: `hornvale_language::EPOCH_COHORTS` needed a new
epoch-18 cohort, and without one every genesis defaults the kind to epoch 0 and
reopens the proto-root churn The Accession closed. That forced three byte-golden
refreshes including the keystone `cli/tests/fixtures/world-seed-42.json` —
**verified purely additive: 6 insertions, 0 deletions** · Decision: correct spec
§6's cost claim rather than let the headline stand · Why: §6 said "no enum edit,
no match arm, no macro, no dispatcher edit". The first half held and the second
was too strong — `domains/thing::concept_doc` IS an exhaustive match and it
needed an arm. The claim that survives is narrower and still worth making: the
room grammar's kind vocabulary is open, and adding a kind touches no dispatcher,
enum or match arm **in that path** · Alternatives discarded: keeping the
stronger sentence and treating the two arms as incidental, which is precisely
the shape of an overclaiming guarantee this campaign has now caught four times ·
ideonomy passes / overturns: 0 / 0 — a measurement ·
Capture: spec §6 carries the measured table; new registry row
`MAP-the-concept-side-is-still-closed`.

**The finding is better than the headline it dents.** `MAP-one-kind-model`'s
fence came down in `windows/vessel` and an equivalent one is still standing in
`domains/thing` and `domains/language` — an exhaustive `concept_doc` match whose
missing arm panics world genesis, and a hand-maintained accession cohort. Same
shape, one layer over, two other crates, now named and located instead of
suspected. That is what a forcing function is supposed to produce.

#28 [G5] — **CORRECTION to #25: right date, wrong mechanism — and my correction
carried the error into the ledger.** Entry #25 said a chamber's contents reached
a committed artifact on 2026-08-30 via `b8fc0cd02`/`26ebaf7e4` "landing
`the-loom` and `the-key-by-the-loom`". Task 5's reviewer disputed it and I
verified the dispute myself:

```text
  git log -S'"the-loom"' -- windows/vessel/src/interior/pattern.rs
    f2cfb0974  2026-07-28  (The Blocking)      <- the-loom landed HERE, a month earlier
  git show --stat b8fc0cd02 -- book/src/gallery/possession-carry-seed-1.md
    1 file changed, 77 insertions(+)           <- the transcript was CREATED here
```

`b8fc0cd02` created the transcript, and its **first** version already rendered
`a loom`, `a strongbox` and `a key` — three `at_locale: false` anchors that had
existed for a month. So the gate opened because **a new committed artifact
walked deep enough**, not because a pattern was appended.

**Why this is worth a numbered entry rather than a silent edit.** The
`INVENTORY` doc's job is to tell a successor which act to review. As written,
the correction says appending an `at_locale: false` pattern opened the gate —
which is the very act the clause declares LATENT — so the corrected paragraph
now contradicts itself, and a reader watching for the wrong act watches the
cheap half. The true rule is the more useful one: *adding a committed artifact
that walks a chamber* is the reviewable act, and every pattern already in
`INVENTORY` becomes visible the moment one appears.

Also in that paragraph, and mine: attributing to decision 0069 the claim that a
census "never reads a chamber". 0069 says fine position is never serialized.
That a census or `book/src/domesday/` does not read a chamber is a property of
those readers, not a guarantee 0069 issues. The narrower true statement — 0069
keeps `Interior` unserialized, so no world FILE moves — is the one to keep.

**This is `a-correction-is-unaudited-text` happening in real time**: #25 was
itself a correction, written confidently, and it introduced a false mechanism
while fixing a true staleness. The date was right and I stopped checking.
Cost of the miss: a durable doc comment pointing successors at the wrong act,
caught by review one task later.

#29 [G5] — **Past-tensing a false claim launders it into a false historical
record.** Task 2's doc sweep was told: "a historical note in a test doc → keep
the history, past-tense it." Applied to `affordance.rs:159` it produced *"`key`
and `cave-mouth` **were** the first rows with no anchor-kind variant behind them
at all"* — grammatically past, still factually wrong. `key` got an
`AnchorKind::Key` variant in The Chattel's Task 11, four tasks after the
sentence was written, so it was never true by the time the enum was deleted ·
Decision: Task 6 corrects the fact, not the tense · Why this is worth an entry:
the past-tense rule is a good rule and it has a failure mode I did not state
when I wrote it — **it makes a false sentence read as settled history**, which
is harder to catch than a false present-tense claim, because a reader audits
claims about now and accepts claims about then. Two reviewers read that line
after Task 2 and neither flagged it ·
ideonomy passes / overturns: 0 / 0 · Capture: the retrospective takes this as
the fifth instance of the campaign's signature shape — a statement whose form
outruns what it can support. Add to the rule: past-tense a claim only after
checking it was ever true.

#30 [G5] — **Adjudication: a dirty working tree does not compromise a sluice
stage gate, because the sluice tests a SHA.** Task 6's reviewer raised as
Important that `docs/timings.md` carried an uncommitted row, quoting the brief's
premise — *"this is the last boundary at which the tree is artifact-clean, so
anything the queue reddens is attributable to the re-key alone"* — and
concluding the tree should be reverted before `make sluice-stage` runs ·
Decision: **not a defect and not a fix round.** The observation is right and the
consequence does not follow. `sluice-stage` takes `REF=<full-sha>`; the chamber
checks that commit out on the canonical box and merges main into it there.
Nothing about the submitter's working tree reaches the run. The gate had already
completed green against exactly `747be7000d13`, which is HEAD · Why the reviewer
reached it anyway: it reasoned from the brief's *premise sentence* rather than
from the mechanism, and my sentence was loose — "artifact-clean tree" is about
the COMMITTED artifacts being unmoved by stages 1-4, not about `git status`
being empty · Committed the row as controller tree hygiene ·
ideonomy passes / overturns: 0 / 0 · Cost if wrong: none — the gate is green
either way, and the row is append-only telemetry that would otherwise be lost
or swept into an unrelated commit.

#31 — **FIRST SLUICE STAGE GATE: GREEN.**
`req-747be7000d13-20260901T155501Z` → `reported`, all stage phases rc=0 in
1325 s, **main unchanged at 2d84e1b71**. Stages 1-4 — the enum retired, the
totality gates, prose as a component table, and a new kind placed in a real
world — survive contact with main as a real merge product, not as a branch tip
in isolation. Stage 5 (the acts) begins from here, which is the point of
gating at this boundary: anything the next gate reddens has one candidate cause.

#32 [G5] — **The campaign's signature shape, instances six through eight, all
in Task 7.** (6) Three of the implementer's own property tests were vacuous on
first writing and it caught them itself — the first time the author beat the
reviewer to it. (7) A surviving assertion cannot fail:
`fatigue_with_pending(&ledger, &[], e, t) == fatigue_at(&ledger, e, t)`, where
both sides are one-line calls to the same function; the test's own message
admits it, which makes it honest and still misleading, because it is the
assertion a later reader mistakes for coverage of the thing the test is named
for. (8) A count claim outran its support again: the report says the predicate
doc was changed "at all three registration sites"; there are **seven**, and
three in `windows/lab` still register `RESTED` as *"an agent rested on a day"* —
now documenting a tick count as a bare day-flag, which is the exact misleading
render the implementer fixed in the session path.

**Eight instances, one shape, and it is now unambiguously the campaign's
product.** A statement whose form outruns what it can support: an advertised
gate nothing enforces, a guard hunting an impossible literal, a sweep blind to a
sentence that never names its identifier, a vacuity guard that passes
vacuously, a false claim laundered by past tense, three tests that pass against
their own mutants, an assertion that cannot fail, and a site count off by four.
Two of the eight are mine in plan text; the rest are spread evenly across
implementers and reviewers. The retrospective carries this as ONE finding.

#33 [G5] — **CONTROLLER OBLIGATION, not implementer work: Task 8 must land
before pre-merge close.** Task 7's reviewer established that the nap-fragmentation
deferral is sound *conditionally* — no committed world file carries a `rested`
fact, so nothing durable is baked, and every artifact holding the naps is
regenerable. But census goldens refresh once, at pre-merge close. If Task 8
slipped past that close, what would merge is a creature dozing ~22 times over 39
days with the census encoding it. Task 8 is the next task, so the condition is
easily met — it is recorded because a condition nobody wrote down is a condition
nobody checks. Related: `tick_commit_budget` headroom is now 17% (1.242 against
a 1.5 ceiling) while ~35% of the volume is naps, so **no fact-emitting behaviour
may be added before Task 8 gives that headroom back.**

#34 [G5] — **An honest, specific failure message is not evidence the check
performs what it says.** Task 7's implementer reported that its
quantizer-witness test was vacuous in its first form — it compared
`quantize(n as f64)` to `n as f64`, applying the `i64→f64` rounding to *both*
sides, so the `limit → 10^18` mutation left it green — and that this was the
**fourth** vacuous-first test in that task alone. Its own summary is the
finding: *"three of my four had honest, specific messages describing a check
they were not performing, and none was found by re-reading."*

That sharpens the campaign's eight-instance shape into something operational.
The failing mode is not sloppiness or vague wording — the messages were
precise, and precision is what made them convincing. **Re-reading a check
compares it against the model that produced it, so it cannot see the gap;
only mutation can.** Every one of the eight instances was found by running
something, never by looking harder.

#35 [G5] — **A reviewer's prescribed criterion was wrong, and the implementer
inside the code found the discriminating one.** The review specified the new
`FATIGUE_FALL` guard as "a saturated body drops below `FATIGUE_ACT` in 3-4
cycles". The implementer checked and reported that this does not discriminate:
one half-day night repays 0.5 and takes a saturated body under 0.85 at 0.6, 1.0
**and** 1.6 alike. It substituted "returns to fully rested", which is what
`FATIGUE_FALL`'s doc and the reviewer's own re-derivation are actually authored
against, and which reddens both ways — **0.6 → 7 nights, 1.6 → 2**, with all of
P1-P6 staying green, which is precisely the gap the test exists to close.

This is `campaign-autopilot`'s own rule vindicated from the reviewer's side:
*never prescribe a specific mutation from outside the code; name the property
and let the implementer find one.* I relayed a prescribed criterion instead of
a property, and the person with the code in front of them corrected it. The
rule is not only about plan text — it binds review instructions too, and that
extension is worth carrying into the retrospective.

#36 [G4] — **Task 8 gains the nap-fragmentation fix Task 7 deferred to it.**
Task 7 left a body that lies down while awake taking its span from
`next_awake_day` — one scan step, 7.2 minutes — so an exhausted creature dozes
repeatedly and then sleeps properly at dusk: ~35% of the new fact volume and the
whole 80 → 134 golden growth · Why the deferral was right and why it ends here:
damping it requires a rest's length to be a property of the ACT, and until Task
8 there was one act. `Session::sleep`'s own doc already called the coarseness
"honest but coarse" · The plan now states the properties the fix must achieve
rather than a mechanism, and requires the before/after fact-volume numbers in
the report — `tick_commit_budget` sits at 1.242 against a 1.5 ceiling where it
was 0.96 pre-Task-7 · **Explicit: do not widen `STEADY_STATE_CEILING` or
`NON_GROWTH_MARGIN`.** If passing requires moving a ceiling, that is a finding
about the model, not a calibration chore — and this campaign has already
verified once that no tolerance was widened, which is a check worth keeping
honest · ideonomy passes / overturns: 0 / 0 · Capture: plan Task 8 Step 2b.

#37 [G5] — **Instance eleven, and the sharpest yet: a doc claim falsified by
the fixture it cites as evidence.** `SLEEP_BOUT`'s doc says the hoisted-walk
golden's real nights measure 45,000 and 50,000 ticks "so on the ordinary path
the cycle still decides and **this floor never binds**". Task 8's reviewer
instrumented `act_span` and ran that very golden: of 18 sleeps, **8 have the
floor bind** — cycles of 5,000 / 20,000 / 25,000 / 30,000 all overridden to
40,000 — reconciling exactly with the golden's ten 40,000-spans.

And the binding cases are not the case the floor was written for.
`Fatigue::act()` returns `Sleep` iff `!awake`, so every creature-layer `slept`
is an **in-phase** sleep; the "player types `sleep` at noon" case the floor
exists for is never exercised there. What is exercised eight times is a body
bedding down with its night nearly over and being pushed past dawn — worst case
a 5,000-tick remaining night stretched to 40,000, an **8x overshoot**. The
magnitude claim ("up to a bout") was right; the frequency claim was wrong, and
the fixture named as evidence was the disproof.

#38 [G5] — **`REST_FALL` survives a 90x mutation range: Task 7's Important 1,
recurring for the new constant.** The full vessel suite passes at 0.9, at 0.1
and at 0.01 — 898/898 each time. Only degenerate endpoints redden, so the
effective bracket is the open interval (0, `FATIGUE_FALL`) and 0.5 is held by
nothing. Neither the golden nor `tick_commit_budget` moved at any of the three,
so the fragmentation-return risk from an under-powered rate is witnessed by
nothing either.

The remedy is unusually clean because **the doc already states the derivation
as a checkable inequality**: `REST_BOUT`'s comment says repayment must exceed
`HYSTERESIS_H` (0.1) and that "a quarter-day at `REST_FALL` repays 0.125". At
0.01 that is 0.0025 — 40x below the stated floor, silently. One assertion,
`REST_BOUT.as_std_days() * REST_FALL > HYSTERESIS_H`, turns a sentence into a
guard. **Pattern worth carrying: when a constant's doc states a budget, the
budget is already a test; nobody had written it down as one.**

#39 [G5] — **Fixing the defect made the metric worse, and the metric had been
flattered by the defect.** Task 8's fix round moved `tick_commit_budget` from
1.008333 **up** to 1.058333 and the walk golden from 90 to 108 facts. The
implementer's explanation is the finding: *part of the 90-fact roster's thrift
was the defect itself — an oversleeping body commits nothing* — so 108 is the
honest number and 90 was a number produced partly by the bug. Headroom is 29%
under an untouched 1.5 ceiling, still 15% better than Task 7's 1.242.

Worth carrying: a budget improved by a defect reads exactly like a budget
improved by a fix. Nothing in the instrument distinguishes them, and the only
reason this one was caught is that a reviewer disproved the doc claim the defect
was hiding behind.

#40 [G5] — **A prescribed mechanism was taken, but only after being checked —
and the check refined it.** The review suggested gating `SLEEP_BOUT`'s floor on
`is_awake`. The implementer reproduced the reviewer's measurement first (8 of 18
bind; all 18 `awake=false`), then found the suggestion incomplete: **while
awake, `next_awake_day` does not always answer a scan step** — a body lying down
before dusk scans past the whole night — so the awake branch keeps `max` rather
than collapsing to a bare `SLEEP_BOUT`. Third time this campaign that a
criterion or mechanism supplied from outside the code needed correction by
someone reading it (see #35). The rule holds in both directions: a suggestion is
a hypothesis, and the person with the code tests it.

Two further things done right rather than asserted: the replacement frequency
claim is a **full-day sweep at the wake lattice that asserts its own population
discriminates** (7 of 9 off-phase instants have a cycle shorter than the floor),
not two hand-picked instants; and the `REST_FALL` inequality, having tripped
`clippy::assertions_on_constants`, became a `const _: () = assert!(...)` — so a
bad value now fails to **compile** rather than to test. The residual is stated
plainly rather than glossed: the bracket is (0.4, 1.0), 2.5x rather than
unbounded-below, so 0.5 is still not uniquely determined — *what is now
determined is every claim its own docs make*.

#41 [G5] — **Ruling: "absence means this species does not sleep" is INVERTED to
explicit rows plus a ratchet. My instruction was wrong and the codebase said
so.** I told Task 9's implementer that a species with no row does not sleep, so
Nathan's "(for most species) mandatory" exception would fall out of the table
for free. Task 9's reviewer ruled (a) — explicit — on three grounds, in
ascending force:

1. **This code path already has the opposite convention.** `body_at`
   (`liveness.rs:6558-6612`) resolves seven species traits and every one falls
   back to a documented NEUTRAL default, with its own comment naming that as the
   rule. `fatigue_rise_for` is now the only species lookup in the crate whose
   miss yields a **semantically extreme** value — permanent, total
   sleeplessness — and it states the inverted convention 4,200 lines from where
   the other seven state theirs.
2. **The exception was already free, so the absence buys nothing.** `xorn` is
   the sole absent row, but `view.fatigue`'s only consumer is pushed inside
   `if !ametabolic`, so xorn's missing row changes no behaviour today. The
   exception was already expressed where the arbitration reads it; restating it
   as an absence spends the only signal that could distinguish "authored as
   sleepless" from "nobody authored it".
3. **The trap fired inside the task.** `cold_thermal_npc`'s `species: "test"`
   silently became rate 0.0 and broke a walk test. Nothing reported a bad
   species; a test simply changed meaning.

My objection to an `Option` — that "does not sleep" and "not yet authored" would
be the same `None` — is answered by (a) rather than defeated by it: a row
carrying `0.0` is an authored statement, an absent row is an error. **Two
mechanisms are needed and they catch different traps**: a coverage ratchet
closes the REGISTRY half; a stated neutral fallback in `fatigue_rise_for`
(shaped like `clock::mass_for_species`) closes the STRING half, since a typo in
`Body.species` resolves to nothing whatever the registry contains.

#42 [G5] — **A real physics regression: rise moved to the local clock and fall
did not, so a legally pinnable world can no longer recover.** Task 9 converted
the RISE term to local days and left `FATIGUE_FALL`/`REST_FALL` on standard
days as a stated scope boundary. But a sleep bout is not a standard-day span —
`act_span` runs a sleep to `next_awake_day`, roughly half a **local** day. With
`L` = local day in standard days: accrual per waking phase is `0.3 × 0.5` =
0.15, now invariant in `L`; repayment per night is `FATIGUE_FALL × L/2` = 0.5·L,
still scaling with `L`. **Break-even at `L` = 0.3 std days = 7.2 hours.** Below
that a full night repays less than a waking phase accrues and a creature
ratchets monotonically to 1.0 and stays there.

The unpinned draw is safe (16-40 h), but `RotationPin::PeriodHours` admits
**4-100 h**, so `--day-hours 4` is a legal world where nightly sleep cannot keep
up. Under the pre-task model this was impossible: both terms carried `L/2`, so
the margin was a fixed 3.33x on every world. `FATIGUE_FALL`'s doc also survives
this commit still asserting a recovery span that is true only at `L ≈ 1`.

Decision: **fix it in this campaign** by converting the fall terms too, which
restores `L`-invariance of the cycle while leaving the discriminating test (a
pure ramp) still discriminating — the reviewer checked that. `REST_BOUT`'s fixed
0.25 std days is the residue and becomes a registered follow-up rather than
scope creep. This is a defect, not a fidelity tradeoff: a legal world where
creatures can never recover is broken, not differently calibrated.

#43 [G5] — **Rest quality reads `affordance::offered_to`, not
`offered_to_observer`: the observer's knowledge is deliberately NOT consulted,
and the brief said the opposite.** Task 10's brief instructed the implementer to
read the room through `offered_to_observer` — "the same query every other
surface reads" — citing `Session::warm`'s history, where a hardcoded
`AnchorKind::Hearth` literal was correctly replaced by the offer. The
kind-comparison half of that instruction is taken in full: `room_affords_rest`
asks the offer, never `anchor.kind == kinds::BED`, so a future `SupportsRest`
carrier (a fur, bracken) needs only its `object_registry` row. The *observer*
half is refused, on three grounds in ascending force.

1. **The knowledge gate governs what a body is TOLD, not what happens to it.**
   `offered_to_observer`'s own doc states its job as spec §3.5's: "the offer
   passes through the observer's knowledge before it is **rendered**". Every one
   of its call sites is a rendering surface (`Session::warm`,
   `Session::examine_chamber`). Physical restoration is rendered to nobody. A
   body that sleeps on a bed it does not recognise as a bed still sleeps on a
   bed.
2. **The creature path carries no `Knowledge` at all, so the gate could only
   have been faked.** `Knowledge` is a session structure; `Body` has no field
   for it and `Perceived` carries belief about water and hazard, not the type
   that query wants. Wiring it would have meant either synthesising a
   `Knowledge` for every creature — a fabricated input to a gate — or grading
   the player's bouts and the creature's by two different queries. The
   controller's own standing requirement is that the read and the mover reach
   ONE definition; two queries is the defect that requirement names.
3. **The gate is a seam ahead of its consumer, and would have been permanently
   satisfied here.** `offered_to_observer`'s doc records, measured, that no live
   `Session` can present it with a `known` that fails: `Session::new` absorbs
   the current room before the first turn. Routing restoration through it would
   have added a check that reads as live, can never fire, and sits inside a
   healthy-looking artifact — the shape CLAUDE.md calls worse than an absent
   one.

**What is kept from the middle rung.** `offered_to`, not the narrower
`offered_by` the controller offered as the physical alternative: the
body-relative half of the offer IS physical. `body_can_use(SupportsRest, body)`
is a mass-ratio ceiling — spec §3.4's Gibson point, *a supporter to a sprite is
not one to a giant* — and a bed too small to hold a body does not hold it. So
the query declines exactly one of the three layers, and declines it for a stated
reason rather than for convenience.

**Where the single definition lives.** `liveness::room_affords_rest`, reached
from `rest_timeline` and from nowhere else. Both fatigue entry points
(`fatigue_at`, the read; `fatigue_with_pending`, the mover) go through that one
`rest_timeline`, and both production call sites — `affect_of_memo_occupied` and
`decide_step` — pass the same `RestSites { terrain, body: npc }` built from
inputs each already held. `Session::sleep` needed no edit at all: the site is
DERIVED from the ledger's own `agent-at` timeline rather than carried on the
bout fact, so the player's route and the creature's are graded by the same
function reading the same facts, and there is no second constructor argument for
either to forget.

**The consequence that made the derivation preferable rather than merely
cheaper.** A grade read at the QUERY instant would have been non-monotonic: a
body that slept on a bed and then walked into the road would have had the bed's
repayment retroactively withdrawn. Reading the position the ledger records at
the BOUT makes the grade permanent, and `p7`'s final assertion pins it.

**Reachability, measured rather than assumed (decision 0398's bar).**
`the-fireside-bed` is the locale band's only `SupportsRest` carrier and it needs
a room that is both built and cold, so "a room that affords rest" is not
automatic. Probed over five seeds at 50 settlements each, counting homes whose
derived interior offers `Sleep`: seed 42 **1/50**, seed 13 **26/50**, seed 7
**2/50**, seed 1 **0/50**, seed 100 **0/50**. And it reaches committed history:
a 20-tick, 50-agent walk run with the gain at `1.5` and again at `1.0` differs
on seed 13 (20,020 facts vs 20,050, different ledger digest) and is identical on
the other four — which is what a grade confined to cold, built rooms should look
like.

**Out of scope by Nathan's stopping line, and it never became necessary:** the
people side (a `(species, thing)` edge) and the individual side (a
`Lineage`-derived preference), both still parked at
`PSY-rest-quality-is-a-grade-not-a-gate`.

#44 [G5] — **Task 10 reintroduced, through a parameter, the divergence Task 7
deleted a duplicated formula to abolish.** Task 7's guarantee was STRUCTURAL:
the read and the mover reach one function, so they cannot disagree. Task 10 made
the grade a **per-call-site parameter**, and the reviewer proved the regression
by mutation — setting `affect_of_memo_occupied`'s `sites` to `None` while
`decide_step` keeps `Some` leaves **903 tests passing**. P5 pins the two
FUNCTIONS agreeing with `None, None`; nothing pins the two CALL SITES passing
equal `RestSites`. A structural guarantee downgraded to a convention, silently.

#45 [G5] — **The position-trail read has no witness at all.** Deleting the whole
`position_timeline` merge — `let room = sites.body.home.clone()` — leaves P7
green, all ten `fatigue_stock` tests green, and all 603 vessel lib tests green.
Cause: P7's fixture gives each body a `home` equal to the room it sleeps in, so
home and trail agree at every assertion. So the report's central design claim —
that the site is *derived from the ledger's own `agent-at` timeline*, which is
what justifies the new `O(trail)` cost and why `Session::sleep` needed no edit —
is pinned by nothing. One line in the fixture fixes it: give the bedded body a
`home` that is neither room.

#46 [G5] — **CONSTITUTIONAL LIMIT, and it belongs to Nathan: the rest grade
cannot be finer than the locale.** The reviewer established the player path
works but is **graded too coarsely** — in a built+cold locale the grade fires
everywhere in that locale, so *a player passing out in the street is repaid
exactly as one who found the bed*. That is the real distance from Nathan's
ruling (*prefer a bed... when they can get one*), and it is not an oversight
that can simply be tightened: grading per ANCHOR would require the fold to know
which anchor a body occupied, and **decision 0069 says fine position is never
serialized**. The ledger carries the room, not the spot in it.

So the object-half grade is locale-granular by construction, and making it
anchor-granular is a decision about 0069, not a refinement of this task. Goes in
the G6 package and becomes a registry row; it is NOT fixed in this campaign.
The feared inversion, by contrast, cannot occur: `the-fireside-bed` needs
`built && cold` at both bands, so there is no world where a chamber has a bed
and its locale does not.

#47 [G5] — **The structural guarantee is restored by removing the argument, not
by testing it: `creature_fatigue` is now the one production door, and it closed
a SECOND divergence nobody had named.** #44's repair could have been a test that
watches two call sites agree; it is instead one crate-private function that
builds the whole argument list — species rate, local day, `RestSites` — so
`pending` is the only parameter left, which is exactly what
`fatigue_with_pending`'s own doc has always claimed is "the only thing that
distinguishes" the mover from the read. The reviewer's mutation is no longer
expressible: neither site has a `sites` argument to null out.

The second divergence was already there and unremarked before Task 10. Both call
sites spelled `fatigue_rise_for(&npc.species, Some(&fatigue_rise_registry()))`
by hand, so a future edit hoisting a cached registry at the mover and not the
read would have gone in just as silently as the site grade did — a duplicated
expression, in the same two places, feeding the same fold. One function now
holds it.

`production_reaches_fatigue_through_exactly_one_door` (`liveness.rs`'s own test
module) is the ratchet on top, and it counts THREE things in the production half
of the file rather than one, because a single count would have missed the
reviewer's own mutation in its new form: `RestSites{}` once, `fatigue_at(` once
(its definition — **no production caller**), `fatigue_with_pending(` twice
(definition plus `creature_fatigue`). The first row catches an inlined struct
literal; the second catches the nearest expression of #44's divergence under the
new shape, a call site that goes around `creature_fatigue` and asks for `None`
directly. Both reds were observed (`left: 2, right: 1` on the respective rows).

What it does not prove is stated in its own doc: a text scan cannot see that the
two sites pass the same `terrain` and `npc`. What makes that true is that each
function holds exactly one of each, of distinct types.

#48 [G5] — **#45's fixture fix generalises: a fixture whose FALLBACK and whose
SUBJECT answer the same thing pins neither, and it reads as a pass.** P7 gave
each body a `home` equal to the room it slept in, so `rest_timeline`'s
no-position fallback (`body.home`) and its trail read agreed at every assertion
and the entire `position_timeline` merge could be deleted green. The bodies now
live in a third room that is neither graded room, and the deletion reddens at the
claim (`bed=0.44999999999999996, road=0.44999999999999996`).

The shape is worth naming because it is not the usual vacuous-test failure. The
test DID exercise the code path — the merge ran on every call — and it DID
discriminate the property it was written for. What it could not see was that a
much simpler implementation produced the same answers on this fixture. A
mutation survey that only asks "does the feature's own mutation redden" cannot
find that; the question that does is "what is the SIMPLEST implementation my
fixture cannot tell from the real one".

#49 [G5] — **The mechanism I gave for the empty artifact diff was false, the
conclusion was right, and the true reason is one number.** Task 10's report said
the regenerated artifacts "contain no creature walk at all". They do:
`regenerate-artifacts.sh` runs `possess` twice with scripts containing `wait 90`,
writing two book galleries under a declared path. A future task reading that
sentence would conclude the galleries can never move on a fatigue change; they
can.

Measured instead of asserted: both seed-42 transcripts are byte-identical with
the grade neutralised (`SiteGrade::Afforded => 1.0`), so the null is genuine —
and the reason is the SEED, not the artifact set. Seed 42's flagship, Doaba, is
`built=true cold=false` at **26.16 °C**, so it composes no `the-fireside-bed`
and no bout in those 90 days is graded. Seed 13's flagship is `built=true
cold=true` at **−61.21 °C** and does afford rest, which is why the reviewer's
live `possess --seed 13` moves (947 vs 977 `stirred`) and my own 20-tick walk
digest moves on 13 and on nothing else.

So the honest statement is: *the gallery seed is warm.* Not *the galleries have
no creatures in them.* One of those survives contact with seed 13 and the other
does not.

#50 [G5] — **The instrument I named for the perf concern could not fire, and
naming it was worse than admitting the gap.** Task 10's report and the bench's
own module doc both pointed at `session_length_scaling`'s `fatigue_us` column as
where an `O(trail)` regression would show. `probe_fatigue_us` was passing
`sites: None`, so the trail merge never ran under it. A named mitigation that
cannot fire stops the next reader looking, which is strictly worse than an
admitted absence — the same shape as a check that can never fail.

The probe is graded now, with its own terrain carrying the world's real
`built_rooms` set, kept separate from the sim's so the other five probes' inputs
and the file's recorded findings are untouched. **And the measurement was then
actually taken** (n=1, seed 42, 50 agents, 200 ticks): `fatigue_at` fits
`k = 0.09139 us/call` per additional fact of history, `r^2 = 0.977`, elasticity
**0.34**, final band **64.60 us/call**. That is the cheapest of the six folds by
a factor of 8.8 against the next (`drive_at`, 565.88), and 752x under
`hazard_memory_memo`. The header's older note that fatigue is "the one fold with
no stable elasticity sign" belonged to the pre-Task-10 shape; graded, it has a
clear positive sign well under 1.0, which is what a bounded one-pass merge over a
growing trail should look like.

#51 [G5] — **Reachability re-cited to a committed census column, superseding
#43's throwaway probe.** #43 established decision 0398's bar with a probe written
and deleted inside the task — evidence nobody can re-run. `cold-built-room-share`
(`windows/lab/src/metrics.rs`) has measured the same quantity at **n=1000** since
The Range: the share of a world's built settlement rooms that read `is_cold`, i.e.
"the fraction of the settled world where `interior_of` would compose a hearth" —
and therefore, one grammar link further along, a `the-fireside-bed`.
`book/src/domesday/settlement.md` carries the distribution: **1000 present, 0
absent; min 0, p25 0.0712, median 0.1835, p75 0.3929, max 1, mean 0.2573.**

The probe's per-seed rank order matches (42 low, 13 high, 1 and 100 at the
floor); the magnitudes differ about tenfold because the denominators differ — the
probe counted 50 derived NPC HOMES, the column measures all built settlement
ROOMS. Cite the column. The lesson is small and general: when a task needs to
establish reachability, look for the committed census column before writing a
probe, because a standing measurement at n=1000 outlives the task and a
throwaway at n=5 does not.

#52 — **SECOND STAGE GATE SUBMITTED**, `req-81fe0788700e-20260902T000515Z`,
kind=stage, branch `campaign/the-wicket`, ref `81fe0788700e`. Main has moved
since the first gate — **2d84e1b71 → 18f63ebfa** (three `the-pavement` landings
plus others) — so this gate tests a genuinely different merge product than the
first, which is the whole reason the stage gate merges main in the chamber
rather than gating a branch tip in isolation.

#53 [G4] — **Board intelligence that binds Task 11's DoD, read at submission.**
Three standing hold-off notices matter to a campaign that is about to mint
decision records:

1. **Decision numbers must be reserved, not inferred.** `make decision-block
   NAME=<branch>` hands out a range. Taking max+1 against the main you branched
   from is invisible to every mechanical check until the block's owner mints the
   same number — differing slugs raise no merge conflict and the digest renders
   one line per FILE, so a duplicate reads as a normal entry. Decision 0139's
   own context records two campaigns minting 0134 exactly that way, through
   green gates. A guard now exists (`docs_consistency` asserts no two records
   share a leading number) but it catches the collision only once both are
   committed; the block is the prevention.
2. **Three generated aggregates conflict for EVERY campaign that mints a
   decision while another lands**: `docs/audits/type-audit-report.md`,
   `docs/decisions/README.md`, `docs/digest/decisions-in-force.md`. Resolve by
   REGENERATION in this order — absorb main, regenerate (the `>` redirect is
   what writes the file; running the command bare regenerates nothing and the
   drift check then reads clean), `make rebaseline`, commit. A rebaseline taken
   BEFORE the absorb reverts main's half and bounces again.
3. **A conflict-free merge of a generated file can still be WRONG.**
   `campaign/the-radiation` measured it: absorbing 56 commits produced four
   loud conflicts and **two silent ones** — `concept-registry-generated.md`
   merged cleanly and dropped all four person predicates. Only `make rebaseline`
   found it. So: no-conflict and correctly-merged are unrelated properties for
   generated files. Always regenerate after an absorption; never infer freshness
   from a clean merge.

Also noted, for our own byte-goldens: `windows/vessel/tests/fixtures/` is
deliberately NOT declared generated, and `make rebaseline` does not write it —
only `make rebaseline-goldens` does. This campaign moved those fixtures in Tasks
7-9 and accepted them deliberately, which is the act that separate command
exists to force.

#54 [G5] — **Main absorbed: 63 commits, `the-pavement`'s cube-sphere lattice,
eight conflicts, no silent mis-merge.** Merge `22191f93e`, gate green at 1002
tests. Three things worth keeping:

**The golden literal was COMPOSED and PREDICTED, not pasted.** Both sides had
edited the same fact roster for different reasons — we moved the fact SHAPE
(`rested`/`Flag(true)` → `slept`/`Number(span)`, 80 → 108 rows), main moved every
room ID with the lattice. Neither side was correct for the merge product. The
absorber proved each side moved exactly one column — main's edit byte-identical
after normalising three ids, multiplicities 24/12/12 on both sides; ours moving
predicates only, with agent-at/drank/eaten unchanged at 48/12/6 — then **wrote
the composed 108 rows before running the test**, and got green with zero
adjustments. That ordering is the whole discipline: a third kind of difference
would have reddened that run, whereas resolve-then-paste would have absorbed it
silently.

**Nothing moved beyond the four conflicted files.** The Radiation's failure mode
— generated files merging cleanly and *wrongly*, dropping content with no
conflict — did not occur here, and it was checked for rather than assumed.

**A genuine cross-campaign semantic collision, contained.** Main wrote new code
calling `thing_kind_of`, which this campaign deleted in Task 2. One live line;
now zero. Main's real improvement at that site (`CHAMBERED_SEED` = 14, a seed
that actually has chambers, against our `world_at(1)`) was kept.

#55 [G5] — **A hypothesis in the absorption report, checked and refuted.** The
absorber flagged that our gallery carried `slept … 40000` where the regenerated
file reads `45000`, and suggested "our tip's gallery hadn't been regenerated
after the Task 8 fix round" — i.e. that this campaign had shipped stale
artifacts and its own drift checks had missed them. **Not so.** The gallery's
last pre-merge touch on our branch is `85c8cf5ac` (Task 9's fix round); the only
commits after it are Task 10's, and Task 10 verified zero artifact drift with a
mechanism its own review corrected and confirmed — seed 42's flagship Doaba is
`built=true cold=false` at 26.16 °C, so no rest grade fires there at all. The
span moved because **main** moved it: the cube-sphere lattice relocated every
room, so `next_awake_day` reads a different terrain and returns a different
cycle. Benign, and attributable to the other parent.

Recorded because the hypothesis was reasonable and wrong, and a report's
uncertain aside is exactly the kind of sentence a later reader promotes to fact.

#56 — **SECOND STAGE GATE: GREEN.** `req-71087ce452a6-20260902T010745Z` →
`reported`, all stage phases rc=0 in **1386 s**, main unchanged at `18f63ebfa`.

Both boundaries Nathan asked for are now clean, and the pairing did what it was
designed to do. The first gate (`747be7000d13`, 1325 s against main at
`2d84e1b71`) established that stages 1-4 — the enum retired, the totality gates,
prose as a component table, a new kind placed in a real world — merge and pass
in isolation from the acts work. The second gate tested stage 5 **plus** a
63-commit absorption that included `the-pavement` rewriting every packed
`FacetId` in the world. Had this one reddened, the first gate's green would have
narrowed the candidate causes to the acts and the absorption rather than leaving
ten tasks in scope.

Remaining before merge: Task 11's DoD, one census refresh on lefford (mine to
dispatch, not an implementer's), and Nathan's G6 review.

#57 [G5] — **Task 11 rulings, and one of them contradicts the brief.** The DoD
brief named `docs/decisions/README.md` as one of three "whole-repo aggregates"
to REGENERATE and never hand-edit. It is not generated: it appears in no branch
of `scripts/regenerate-artifacts.sh` and in no row of
`docs/generated-paths.txt`, so its index is hand-maintained and there is
nothing to run. Checked rather than assumed, and the check found a live
consequence — the index goes **0514, 0516**: `0515` (The Pavement's diagonal
ruling) has a record and no index row, omitted when that campaign landed. Added
here with its omission stated, because the index is the retrieval surface and a
missing row is invisible in exactly the way a missing record is not · Decision:
hand-edit the index, regenerate the two that ARE rendered
(`docs/digest/decisions-in-force.md`, `docs/audits/type-audit-report.md`) ·
Verified after `make rebaseline`: `git diff --exit-code` over every declared
generated path moved **one file, three insertions** — the digest's three new
decisions — and the type-audit report did not move at all, which is the right
answer for a sweep that touched only doc comments. Cost if wrong: none; the
regeneration was run anyway and reported an honest empty diff for the other
paths, which is the positive control the brief's own framing would have
skipped.

#58 [G5] — **Two more instances of the campaign's own shape, found in the
freshness sweep, and both are the campaign's own drift.** `SLEPT_PROVENANCE`'s
doc said *"the provenance `sleep` commits its `rested` fact under ... the same
register `liveness.rs` uses for a creature's own Rest"*, and
`sleeping_needs_no_bed`'s doc said the method *"commits `rested` and sets
`wake_at`"*. Task 8 moved that method onto the `slept` predicate and made
`wake_at` conditional on `renders_unconscious`; the constant's NAME and VALUE
were already right, so nothing reddened and the two sentences beside them kept
naming the retired fact. That is instance thirteen and fourteen of *a statement
whose form outran what it could support*, and the sub-shape is worth naming
separately: **a doc comment has no ratchet when the predicate under it
changes**, so the only thing that finds this class is a sweep run against the
vocabulary that exists AFTER the change — the same rule decision 0556's
consequences state for source-text gates. Both corrections say what they used
to say rather than being edited silently, for the reason #29 gives: a quiet fix
teaches nobody why nothing caught it.

#59 — **Confidence Gradient: bet 4 annotated, score unmoved, and the annotation
is a LIMIT rather than a gain.** Decision 0030 requires a re-score of any bet
the campaign moved. Searched the whole chapter: there is no bet about the kind
model, the object vocabulary or composition — `grep` for
`composition over inheritance`, `ComponentStore`, `open vocabulary`,
`closed enum` over `book/src/open-questions.md` returns nothing — so the
product half moves no bet by subject. What it does touch is bet 4's
**traversal** half, in that row's own words (*does an act reach through the
stack and come back changed*): a body can now warm itself in a room that is not
a hearthroom, and a sleep is paid down over its span at a rate that differs by
act and by planet. The annotation leads with the ceiling instead, because that
is the part a reader of a green mechanism would not otherwise see — the grade
is locale-granular, so a player passing out in the street is repaid exactly as
one who found the bed. Score unmoved: one more act with a consequence is not a
market worth standing in, and this row moves only when a human plays and says
it moved.

#60 [G5] — **Instance FIFTEEN, and it is the sharpest: the campaign's own
lesson failed in the same file as its own correction.**
`interior/pattern.rs:104-114` — the `at_locale` **field** doc — still says a
pattern with `at_locale: false` *"cannot move a world: no live read can reach
it."* That is the exact claim ledger #25/#28 corrected on the `INVENTORY` doc
**thirty lines below it**, and it is falsified by this campaign's own proof
kind: `the-brazier` is `at_locale: false` and reached
`book/src/gallery/possession-carry-seed-14.md`. The field doc has been untouched
since The Blocking (`f2cfb0974`, 2026-07-28).

We corrected the sentence we grepped for and left its twin one screen away. The
lesson this campaign wrote down — *grep the claim, not the identifier* — is
exactly what would have caught it, and we did not apply it to our own
correction. Of every instance, this is the one to lead the retrospective with.

#61 [G5] — **Two decision records carry a stale census, and append-only means
now or never.** `0556` and `0557` both state "58 occurrences" of bare
`KindId("`, and `0556` adds that a guard "would have shipped pre-loaded with a
**two-table** allow-list". Re-measured at HEAD: **78**, across **three**
authoring tables — `chamber_prose_registry()` went from 1 occurrence to 18 when
Task 4 turned prose into a component table. The substantive ruling is unaffected
(still zero at a production consumer site), so the decision is right and only
its evidence is wrong. Neither record names the census's scope either; a reader
running the bare grep workspace-wide gets **671**.

The mechanism is plain: a Task-3-era measurement was transcribed into a
closing document without re-measuring, in a campaign whose entire finding is
that transcribed claims outrun their support. Fixable before the merge and not
afterwards.

#62 [G5] — **Compressing a chain produced a false code citation.**
`MAP-wilderness-affords-no-rest`'s rewrite says the heat carriers are confined
by "`the-fire` requires `Alcove`, `roles: [Hearthroom]`". `the-fire` is
`roles: EVERY_ROLE` (`pattern.rs:256`), under a comment written to prevent
exactly this reading — *"NO ROLE WITHHOLDS THE FIRE… it is confined to the
hearthroom by the GRAMMAR rather than by a rule anyone wrote"*. The role gate is
on `the-alcove`. **The previous version of the row stated the three-link chain
correctly**; the DoD compressed it to two links and made it false. Same shape as
the known hazard that compressing a branch table deletes a branch.

#63 [G5] — **CENSUS REFRESH: rc=0 in 865 s, and it moved NOTHING. Our own
prediction was wrong, and the reason is a misattribution that survived three
tasks and two reviews.**

`req-a73d8ce3c9b4`, branch `census/a73d8ce3c9b4-20260902T022056Z`. Diff against
the SHA it ran on: **one line, `docs/timings.md`**. No golden moved.

This campaign predicted the opposite, repeatedly. Task 7's review said to
"budget for it at pre-merge close rather than being surprised by it"; Task 9
wrote that "a refresh should be expected to move health/affect columns"; Task 10
measured seed 13 at 20,020 vs 20,050 facts with a different ledger digest and
warned *"do not read the empty rebaseline diff as a null"*.

**An empty diff needs a positive control, so I checked whether the census could
have moved rather than accepting that it did not:**

```text
  studies/the-census.study.json   "metrics": "all", seeds 0-999
  columns produced                230
  columns naming creature/agent/walk/tick    0
  `health::` in windows/lab/src/metrics.rs   0 hits — health is NOT registered
  the one regex hit, `unrest-coverage`       social unrest, not creature rest
```

`health.rs` runs the vessel drive-simulation forward and reads affect per tick,
but it is a **battery**, not a census metric — it never reaches
`metrics::registry()`, which is what `"all"` expands. The census measures world
GENERATION over 1,000 worlds; creature behaviour is not in it.

So the empty result is correct and expected, and the behaviour surface we did
move was covered where it actually lives — `affect-trace-seed-42.txt` and the
snapshot goldens, which moved during Tasks 7-10 and were refreshed there.

**The finding is the prediction, not the null.** "The census will move
health/affect columns" is a claim whose form outran its support: nobody checked
whether health metrics are census-registered, and it passed through three task
reports and two reviews unchallenged because it sounded like a cost estimate
rather than an assertion. It is the campaign's shape once more — and consistent
with the refined lesson, it was settled by RUNNING the census, not by reading
more carefully.

Consequence for the merge: **there are no census goldens to land.** The census
branch carries only its own timings row.

#64 [G5] — **The final whole-branch review found instances 19-23, and a
MECHANISM the previous eighteen did not contain.**

Four of the five sit in doc blocks this campaign edited in the same pass. But
19 and 20 share a cause the catalogue has no entry for: **a fix round changed
the code, updated one paragraph of a doc block, and left the block's HEADLINE
stating the design it had just superseded.**

Concretely: the campaign deleted `const FATIGUE_RISE` and left its 16-line doc
comment in place with no separator, so that comment is now the head of
**`FATIGUE_FALL`'s** doc. `FATIGUE_FALL`'s rustdoc summary therefore documents
the *rise* rate; `:2228` states the `0.0`-on-miss convention that ledger #41
**inverted**; and `:2236` states the fall terms are on the **standard** day,
which fix round 1 (#42) converted — contradicted twelve lines later inside the
same block.

That is not a claim that decayed, nor one that was never true. It is **an
amendment leaving its own preamble executing the superseded design**, and it
happened in the campaign's most-reviewed file. It is the highest-consequence
finding here: `FATIGUE_FALL` is the constant a future tuner opens, and the
actionable misreading is a double conversion.

The other three: `RadiatesHeat` "has exactly one mechanically-supported
carrier", falsified 45 lines below by the row this campaign added; "seven of the
**fourteen** kinds a room's grammar can place", a stale count carried through a
deliberate rewrite of that very sentence (instance 16's exact twin, same file,
missed by the sweep that caught the other); and a verb-dispatch comment saying
"no new predicate" two lines above the arm Task 8 repointed, where `SLEPT` is a
new predicate.

#65 [G5] — **`REST_BOUT`'s stated weakness is bounded in the WRONG DIRECTION.**
The ledger registered it as a fixed 0.25 std days awaiting local conversion.
`REST_BOUT`'s own doc asserts the calibration *"a quarter-day at `REST_FALL`
repays 0.125"*, which must exceed `HYSTERESIS_H`. Since fix round 1 the
repayment is `REST_FALL * 0.25/L` local days, so it falls below the band at any
**L > 1.25 std days** — a slow-rotating world — reintroducing the measured
nap-fragmentation pathology Task 8 exists to remove. `fatigue_from_rests`'s note
frames the open question as *"a fast-rotating world"*, the opposite direction,
and `REST_BOUT`'s doc — where the calibration claim actually lives — does not
mention `L` at all.

#66 — **Operational, and nothing in the campaign's documents says it: none of
this campaign's own gates runs in `make gate-commit` today.**
`docs/timings/subfloor-roster.tsv` carries **zero** `kind_totality::*`, **zero**
`fatigue_stock::*` and 2 of 5 `action_module::*`. That is by design — a test
with no recorded baseline duration is excluded — and it self-heals when the
chamber's `gate` phase rewrites the roster on the merge run. But between now and
that run, G-a..G-f and the 916-line fatigue-stock suite are invisible to the
gate a developer actually types.

#67 [G5] — **Fix round 2 (the final wave): what was changed, and the one item
that was declined.** Every fix below is prose or comment inside a block this
campaign had already touched; the drift check over `docs/generated-paths.txt`
is clean, so no artifact moved.

- **Instances 19-20 (`liveness.rs`).** `FATIGUE_FALL`'s doc block rewritten
  from its summary line down: it now documents the FALL rate, on the LOCAL
  day, and the orphaned `FATIGUE_RISE` preamble is replaced by a paragraph
  naming the deletion, pointing at `fatigue_rise_for` /
  `DEFAULT_FATIGUE_RISE` for the per-species rate, and stating the mechanism
  at the site. `REST_FALL`'s summary gains the same LOCAL-day qualifier, which
  it lacked entirely.
- **Instance 21 (`affordance.rs`, `book/src/chronicle/the-wicket.md`).** The
  "exactly one mechanically-supported carrier" bullet is replaced. Heat has
  **two dispatchers, not one**, and only one of them was abolished: the `warm`
  VERB reads `object_registry` (so `brazier` works), while `warmth_at`
  (`interior/field.rs:71`) is still a literal `kind != kinds::HEARTH`. The
  brazier does not expose the disagreement because it is `at_locale: false`
  and every production `warmth_at` caller derives its `Interior` through
  `interior_of`, whose `selection` admits `at_locale: true` only — verified by
  reading all three call sites (`liveness.rs:1913`, `:2069`, `:2169`) back to
  `interior_of`. Promote the pattern one band up and the offer and the field
  disagree. The chronicle's "arrived to find the edit already unnecessary" is
  kept and bounded: true of `Session::warm`, false of `warmth_at`.
- **Instance 22 (`affordance.rs`).** The DENOMINATOR was wrong, not the
  numerator: "seven of the **fourteen** kinds a room's grammar can place" —
  fourteen was the deleted enum's variant count, and neither candidate
  quantity is fourteen (the roster is 17; `INVENTORY` places 15, since
  `cave-mouth` and `log` are placed by nothing). Seven is true only against
  the roster, so the roster is what it names now, with the seven listed by
  name. The duplicated article two lines above it is fixed in the same edit,
  as is the one 129-char doc line `cargo fmt` will not rewrap.
- **Instance 23 (`session.rs`).** The `"sleep"` arm's comment. Two of its four
  claims were undone two lines below it; the other two were re-verified rather
  than assumed — "no new concept" holds, and "no new cost dial" holds because
  `clock::cost_of` prices `Action::Rest | Action::Sleep` in one 150-tick arm.
- **#65 (`liveness.rs`).** The `L`-dependence is now stated at `REST_BOUT`,
  where the calibration claim lives, with the derivation
  (`REST_FALL * 0.25/L`, below `HYSTERESIS_H` for `L > 1.25` std days,
  `--day-hours` above 30, `PeriodHours` admitting 100 h) and the note that
  `a_rest_bout_repays_more_than_the_hysteresis_band_it_must_clear` asserts the
  `L = 1` arithmetic and nothing sweeps `L`. `fatigue_from_rests`'s registered
  follow-up now states the SLOW direction and says it read the other way.
- **Task 2's deferred minor, taken.** `the_dispatch_scan_catches_an_anchor_
  kind_keyed_table` → `the_dispatch_scan_walks_a_multi_line_signature`, which
  is what its own doc says it proves. The stated blocker (hand-editing
  `subfloor-roster.tsv`) is not one: that file is an ordinary chamber artifact
  and a missing name only means `gate-commit` skips one test. The roster row
  is updated anyway. `docs/decisions/0397` still cites the old identifier and
  is left alone — decision records are append-only.
- **Minor 7, and its blast radius.** The assertion message claimed "a handle
  added to `hornvale_thing::kinds` lengthens `every_named_kind()`". It does
  not: the helper reads `THING_KINDS` (decision 0556's own ruling). Grepping
  the CLAIM rather than the identifier found the same false statement in the
  paragraph 120 lines above it, which still described the `EVERY_HANDLE`
  arrangement Task 2 shipped and Task 3 replaced. Both corrected, and the
  helper renamed `every_named_kind` → `every_rostered_kind`: a roster is not a
  set of names, which is exactly the distinction 0556/0557 make
  constitutional.

**DECLINED: Task 6's em-dash minor (item 10 of the brief).** The finding was
that `affordance.rs`'s third Task-6 correction "lost the em-dashes its
neighbours use". Read against the file, the paragraph in question
(`object_registry`'s `key`/`cave-mouth` correction) carries four em-dashes
already, so either the finding names a different passage or it no longer
applies. Recorded rather than guessed at: inventing a target for a
cosmetic finding, in the campaign about statements outrunning their support,
would be the twenty-fourth instance. The brief permitted skipping it.
