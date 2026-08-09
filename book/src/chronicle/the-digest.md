# The Digest

A legal *code* and a shelf of *reports* are not the same kind of document. The
reports are vast, archival, and never authoritative for what the law is; the
code is small, current, consolidated, and authoritative precisely because
anything it no longer contains is no longer law. Hornvale had the reports —
2,072,357 words of them across 112 decisions, 208 retrospectives, 226 specs,
211 plans, 218 chronicle entries and 13 `CLAUDE.md` files, counted at
`64e8c667`. It had no code. This campaign built the first one.

## The position that was missing

Ask how much of a system's self-description is *derived* rather than authored
and you get an axis:

    0%    hand-authored prose, unchecked
   10%    authored + drift-checked examples
   25%    authored governing text + generated indexes    <-- Hornvale was here
   50%    codification                                   <-- this campaign
   75%    generated governing text
   90%    the system queries its own capability schema to plan
  100%    the system writes and explores its own frontier

The project's index of speculative directions already held the 100% position —
the self-describing program — and had held it as an unelaborated stub since
2026-07-17. It had been picked up and put down repeatedly, and always for the
same reason: the only move on offer was the whole north star, which is not
buildable in a campaign. Nothing named an intermediate rung. Codification is
that rung, and naming it is most of what made the work finite.

Codification is emphatically **not** generate-everything. The unmix doctrine —
already written down, and re-derived from scratch by this campaign's own
brainstorm before anyone found it — warns against exactly that, and its rule
governs the whole design: *store only geological-rate facts; derive on read
anything faster.* Decisions,
their supersession and their scope are stored. Crate names, layers, the
dependency allowlist, the ratified-decision count are scanned from source
every time the artifact regenerates, and are never written down anywhere a
reader could find them stale.

## Two live instances, both found before a line was written

The campaign's brainstorm produced its own motivating evidence, which is a
better provenance than a hypothetical.

`docs/timings/test-baseline-MacBookPro.tsv` — the committed per-test duration
baseline the suite's duration alarm compares against — was **1,055 commits
stale**. It was last written on 2026-07-30; `windows/book/src/lib.rs` alone had
moved +1,082/−455 lines since. The file is specific to the millisecond and
carries an internal commit stamp that nothing ever compares to `HEAD`.
Reasoning from it produced a 3.8× cost misattribution — `hornvale-book`
measured at 9.4% of suite cost against the 36% the baseline implied — and sent
a session's planning in the wrong direction until it was re-measured. A
fast-drifting fact that was *stored*: the precise failure the unmix doctrine
predicts.

`make doctor` — the repo self-map, the thing a fresh session is told to run
first — was drifted at that same moment. `scripts/doctor.sh` hard-coded
`external deps allowlist: serde, serde_json`. The enforced value in
`cli/tests/architecture.rs` had read `["libm", "serde", "serde_json"]` since
decision 0041 admitted `libm` for portable transcendentals. The map had never
learned. Two-thirds of that script was authored prose asserting facts that
live in source, one `echo` line at a time.

Neither is a lapse in anyone's discipline. Both are the predicted output of a
process whose skills all say *capture* and none says *retire*.

## What the collection is

A `Fact { subject, predicate, object, place, day, provenance }` — the kernel's
own shape — in a ledger that is not a `World`, at `tools/digest/`, outside the
cargo workspace on the `tools/type-audit/` precedent (decisions 0027/0028). A
repo tool is neither a domain (it models no slice of the world) nor a window
(it presents no domain), and it must never become a workspace dependency.

Two properties of the collection carry the design.

**It holds no time.** `place` and `day` are always `None`; the store contains
no timestamp, no date, and no commit SHA. Project time is git's. This is not a
concession — it is what dissolves the self-reflective ledger's own
constitutional exclusion. That idea scoped itself out of the development
process on no-wall-clock grounds, and a ledger with no time field cannot
violate that rule in letter or in spirit. Everything temporal is answered by
`git log`, which decision 0088 had already ruled is the review loop's proper
instrument.

**It compacts.** Asserting a fact whose *functional* predicate already has an
object **replaces** it, and the superseded fact leaves the committed artifact
entirely, surviving only in git. No new mechanism was needed:
`PredicateDef { functional: bool }` already existed in the kernel registry.
The single difference from the world ledger is the commit policy — the world
ledger *rejects* a contradiction, the project ledger *replaces* on one. Same
shape, same registry.

Compaction is strictly stronger than answering "what is in force?" with a
query over an append-only log, and the reason is the failure mode above: a
stale fact that remains in the file **will be found** by a grepping reader,
human or agent, and will read as current. Absence beats deprioritisation.

That strength is bought with an obligation. Decision 0088's corollary — *"a
baseline whose rows churn on noise is not archaeology"* — binds here, because
if compaction rewrites the artifact noisily then `git log -p` stops being
readable and the entire justification for compaction dies. So the serialized
form is JSONL, one fact per line, stable-ordered by `(subject, predicate)`,
and the campaign *measured* rather than assumed that replacing one fact
produces a one-line diff. For the same reason a predicate rename carries no
epoch suffix — there is no save to corrupt — but **must be its own commit,
touching nothing else**, because it rewrites every line carrying that
predicate.

## Intent and reality are held apart on purpose

Generated documentation cannot disagree with the code. That sounds like the
whole point until you notice it is also a way to *hide* drift: a document
claiming "we intend X" while the code does Y is evidence of something nobody
noticed, and collapsing the two erases the evidence instead of resolving it.

So the collection stores **intent** as asserted facts, derives **reality** by
scanning source, and a third view reports the delta between them. This is the
line between codification and mere generation, and it is why the `libm` gap
above becomes a reported finding rather than a silently-corrected line.

## Three artifacts, and what each proved

**The self-map.** `make doctor` now prints a block generated by
`digest render doctor`. The layering and the dependency allowlist are derived
from `cli/tests/architecture.rs` — the enforcer itself — so the allowlist line
reads `libm, serde, serde_json` and nothing in the generator may name a member
literally. The remaining three sections (determinism contracts, committed
artifacts, documentation map) are 13 authored lines held as `self-map-line`
facts, one per line, read at render time rather than restated in the shell
script. `scripts/doctor.sh` went from 20 fact-asserting `echo` lines to zero.
The one number that stays derived is the ratified-decision count, because a
count is a fast-drifting fact and the unmix doctrine forbids storing it.

**The in-force decision index.** `docs/digest/decisions-in-force.md` lists 109
of the 112 committed decision records. Three — 0006, 0046, 0082 — are wholly
superseded and are simply *absent*; nobody edited `docs/decisions/` to make
them absent, and `git log -p` recovers every one of them. A fourth, 0026, is
*partly* superseded, and appears with its surviving provisions named. That
scope text is the campaign's clearest illustration of the stored-versus-
scanned line: the plan originally had the scanner infer it from the
parenthetical after "Superseded by", and the corpus makes that impossible —
the same slot carries a rationale in 0063, a genuine scope in 0043, and a bare
date in 0099. Left as designed, the index would have rendered "still in force:
2026-08-04". Scope is asserted, never parsed.

**The delta report.** `docs/digest/intent-vs-reality.md` currently carries one
row, and it is a real finding rather than a demonstration. Decision 0026 ruled
*slugs, not numbers, forward-only* for idea-registry identifiers on
2026-07-10, when the registry held 171 numbered identifiers. It now holds 403.
**232 numbered identifiers were minted after the rule forbade them** — a 136%
increase over the pre-decision set — and the remedy, arriving 17 days later,
was to freeze all 403 in `cli/tests/fixtures/registry-numbered-ids.txt` as
grandfathered rather than convert any of them. The guard is green. It has been
green throughout. It pins the registry against *new* violations while
enshrining 232 existing ones, which is the drift-check failure mode this
project has a name for: a pin against change, not against being wrong.

The delta view can only see that gap by reading git history, because in
current state there is nothing to see — every identifier is on the frozen
list and every check passes. That is the campaign's own design turned back on
itself: project time lives in git, so a view that reports on time must read
git.

## What did not ship

The spec put an MCP tool in v1 scope, and argued for it: if asserting a fact
is onerous, facts stop being asserted, and the result is a beautiful empty
ledger beside a resurgent pile of prose. What shipped is `handle_assert` and
`handle_query` as tested library functions over the JSONL store, with **no
callable surface** — no subcommand, no server, no transport. They are
reachable only from the crate's own tests. Anyone wanting to assert a fact
today edits `docs/digest/facts.jsonl` in an editor.

Nathan's ruling was to ship as-is and record the gap, so it is recorded here
and the spec's risk table now carries that row as **open** rather than
mitigated. Calling a mitigation delivered when no user can reach it would be
precisely the intent-versus-reality collapse the campaign exists to prevent,
performed on the campaign's own paperwork.

The store being plain committed JSONL is what makes this survivable rather
than fatal. It is readable with an editor and reviewable with `git diff`; the
tool is ergonomics, never substrate. A self-model whose only reader is a
running server is more fragile than the prose it replaced.

## The falsification clause, and its answer

The spec froze a stop condition before any code existed: if the generated
`doctor` output cannot reproduce the hand-written one's usefulness — if a
fresh session orients *worse* from the generated map — that is the campaign's
headline, and it stops at v1 rather than proceeding to `CLAUDE.md`.

It was evaluated by diffing the two outputs directly. **Not falsified.** The
generated map is a superset of the hand-written one's information: every line
of orientation prose survives verbatim, one asserted fact is now correct that
was previously wrong, and the section order changed so that the four short
conceptual sections print as one contiguous block instead of being split by a
forty-line dump of `make help`. The only regression the diff found was a
missing blank line between the banner and the first heading, which is fixed in
this campaign's closing commit. A fresh session orients at least as well, from
a map that can no longer say `serde, serde_json` while the enforcer says
otherwise.

That clears the way for a follow-on to attempt the root `CLAUDE.md`. It does
not oblige one. `CLAUDE.md` is a much harder target — judgment prose,
arithmetic, warnings written for a reader who is about to make a mistake —
and the honest read of v1 is that it proved the pipeline, not that it proved
the pipeline scales to the hardest document in the repository.

## The first thing the code found about its own reports

Decision 0006 made seed-derivation labels permanent save-format contracts.
Decision 0099 superseded it wholly on 2026-08-04, on the grounds that worlds
are version-locked to the code that generated them, so the permanence
rationale evaporated. The in-force index therefore does not list 0006, and it
is right not to.

Both the root `CLAUDE.md`'s Determinism section and one of the self-map's own
asserted lines still cite 0006 as live authority for the epoch-suffix rule.
Neither was touched by this campaign — a close is the wrong place to relitigate
a determinism doctrine — but the finding is exactly the shape the delta view
exists to report, and it arrived within minutes of the index first rendering.
Two governing documents citing a wholly-superseded record is not a typo; it is
the reports outliving the code, which is the condition this campaign was built
to make visible.
