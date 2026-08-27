# The Coercion — retrospective

*Arc III of The Bridle. An imposed controller, opened and closed by an
out-of-character seam, and a gate row that refuses a held body's own acts
while permitting everything out-of-character. Six tasks, three decision
records (0336-0338). Process lessons only; the campaign's own account is
[the chronicle](../../book/src/chronicle/the-coercion.md).*

## A third unreachable criterion, caught the same way twice in one campaign

Two campaigns before this one, The Reticence shipped an acceptance test
naming a doctrine that turned out to name nothing, and the retrospective
tradition already had a name for the shape: *an acceptance test naming a
noun is asserting that noun exists.* This campaign hit the same trap **twice
before writing a line of code**, both times by grepping the noun before
trusting the spec that named it.

1. **The aboleth.** The metaplan's own acceptance test for this arc reads "an
   aboleth dominates the player." `domains/species` holds no aboleth, and the
   metaplan says so itself, deliberately: the biology of who can possess whom
   is a species-domain question this arc explicitly defers. The acceptance
   test had to be restated against what actually exists — an imposed
   controller opened through an out-of-character seam, with no creature
   deciding to use it — before any task could be dispatched against it.
2. **The death terminator.** Possession was specified to end at release or at
   the possessed creature's death. `Body` carries no life state anywhere in
   the tree a live session touches; the only death on record belongs to
   historical founders baked into deep history. So the `"died"` arm of
   `possession-ended` ships **written and asserted unreachable** rather than
   silently wrong — the same discipline The Reticence's doctrine arm used,
   applied on purpose this time rather than discovered after the fact.

Both were caught before dispatch, at zero cost beyond the grep that found
them. The pattern has now recurred across three unreachable criteria in two
campaigns, which is enough occurrences to promote from "worth remembering" to
"worth checking mechanically": any acceptance test whose language names a
concrete thing — a creature, a doctrine, a record — is a claim that the thing
exists in the tree, and the claim is falsifiable in one command, before any
plan is written around it.

## The metaplan sweep: the count I trusted was smaller than the count that existed

The task brief for this record explicitly overrode an earlier claim of mine:
that `docs/superpowers/specs/2026-08-19-the-bridle-metaplan.md` carried the
retired word `dominated`/`domination` in exactly one place. Run for real,
`grep -n "dominated\|domination"` found it in **seven**, not one — the gate
table row, the arc table's own acceptance test, two illustrative asides in
prose, a non-goals bullet, and a decisions-to-record bullet, plus the
keystone's own example sentence.

The gap is not a rounding error; it is the same shape a great deal of this
project's process history already carries a name for — a claim stated with
confidence, checked against memory rather than against the tool that could
verify it, and wrong by more than one instance. What is different here is
that the claim was mine, made about my own prior planning text, and the
correction arrived as an explicit instruction to re-run the check rather than
trust the earlier count — which is exactly the discipline that caught it.
Nothing about this defect reached an implementer; it was resolved during this
task, before any of the seven sites was edited, by running the grep the
instruction asked for instead of reusing the number already on hand.

## Two plan defects, both mine, both in Task 1's dispatch

- **A private-module import that could not compile.** The dispatch asked for
  the fold's coverage from an integration test importing
  `hornvale_vessel::session::{...}`; `mod session` is private, so no
  integration test can reach it. Resolved by moving the coverage in-module,
  which a separate ruling (F2, made for an unrelated reason) had already
  pointed toward — two independent paths arriving at the same fix is a weak
  but real corroboration that it was the right one.
- **A dedup footgun neither the spec nor I knew about.** `Ledger::commit`
  deduplicates on the *whole* fact envelope, including provenance — so
  committing an identical fact twice, subject through provenance, is a silent
  no-op. The brief's own reopen test would have silently committed two facts
  instead of three, and — the sharper problem — a broken latest-value
  derivation would have passed the same assertion by coincidence. Caught by
  the implementer, not by review; carried forward into every later task whose
  verbs commit these predicates, and is the direct cause of `possess`'s
  provenance string carrying a turn count rather than a static label.

## A vacuous guard closed by a test built for an unrelated reason

`BodyState::all()` is a hand-written roster beside an exhaustive match. The
match protects against a variant nobody classified; nothing protects against
a variant the roster itself forgot to list, and mutation-proving this
directly — dropping the new row from `all()` while leaving `verdict` alone —
left the pre-existing state × mood sweep green, because a shorter roster
simply iterates fewer states and reports success on all of them.

The fix chosen was neither "patch it now" nor "defer it," which were the only
two options on the table until a third was noticed: H1 already counts
refusals over `all() x Mood` and asserts an exact total, so a dropped variant
changes the count and reddens H1 for free, inside a test this campaign
already needed for an unrelated reason. The gap closes at zero added scope —
but only because that property was made explicit rather than left implicit,
since a later "simplification" collapsing H1 into three direct per-variant
assertions would remove the check while reading as a harmless cleanup. Recorded
generally in the idea registry, since the pattern — a hand-written roster with
no compiler link to the enum it enumerates — is not specific to `BodyState`.

## The campaign's best finding was not preregistered, and its wrong prediction is why it was found

The two preregistered tests comparing an imposed controller's committed facts
against a default controller's (H3, H4) held exactly as predicted — and,
found only after a review pass, they hold **trivially**: `ImposedController`
is a stateless pass-through to `DefaultController`, so the byte-identity and
the fact-shape equality are both deducible from the controller's own fifteen
lines before any fixture runs. A correction fixed the *label*, not the
assertion — preregistration forbids changing what a test checks after seeing
its result, and nothing about either test's logic was touched, only the doc
comments claiming they were measuring something the source had already
settled.

The finding that actually moved the chapter came from a wrong prediction
along the way. Wiring the imposed controller into the driven body's own walk
was expected to be "semantically correct but ledger-inert" — a swap that
changes who is recorded as choosing without changing what happens. The
ledger-inert half held. The rest of the prediction was wrong, and checking it
anyway (rather than treating a plausible-sounding argument as settled) is
what surfaced the campaign's real result: a free body holds through most of a
wait, because its controller returns nothing queued unless a verb queued
something; a possessed body does not hold, because its controller hands the
tick straight to the same arbitration every other creature runs. The
committed ledger cannot see the difference — that walk's facts are discarded
either way — but the body's own felt state can, and does, at the reference
seed. A wrong prediction that gets checked instead of assumed is worth more
than a right one that does not, and this is the clearest instance of it in
the campaign's own record.

## Deferred, with homes

- **Inverse-power possession duration.** A hold whose maximum length falls as
  the possessed creature's own strength rises, considered and set aside in
  favor of the two event-based terminators this campaign ships (release,
  death). No measurement motivates a specific curve. Home:
  `book/src/frontier/idea-registry.md`, row
  `PLAY-possession-duration-scales-with-power`.
- **Which creatures can possess another, and the biology behind it.** Named
  out of scope by the metaplan itself (§5) before this campaign began, and
  still out of scope after it — the seam this campaign ships is
  out-of-character precisely because nothing in `domains/species` can open it
  on its own yet. Home: `docs/superpowers/specs/2026-08-19-the-bridle-
  metaplan.md` §5.
- **The `rider`/`ridden` register split.** Already present in shipped prose
  as the narrative agent-noun for a possessor, a register split from
  `possess` rather than a rival mechanism word — flagged for the owner at the
  spec's hard stop and left untouched by this campaign's own decision on
  vocabulary (0336). Home: the spec's own §9, flag 3 (unresolved by design).
- **H3 and H4 need re-measurement once `ImposedController` gains real
  intent.** Both hold trivially today because the type they compare against
  is a stateless pass-through; the day a possessing creature's own
  arbitration replaces that pass-through, both tests stop being tautologies
  and start being measurements. Home: `windows/vessel/src/liveness.rs`'s own
  H3/H4 doc comments, which already state this in place rather than leaving
  it to be rediscovered.
- **Command and charm, the two mechanisms decision 0337 names and neither
  builds.** The gate table (command) and the drive-valuation layer (charm)
  are both unbuilt; the decision record states where each belongs so a later
  campaign does not reach for the controller stack when one of these two is
  what the case actually needs. Home: decision 0337 itself.
