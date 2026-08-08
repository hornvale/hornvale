# Retrospective — The Digest (codifying project self-knowledge, 2026-08-08)

Process lessons only; the product story is
[the chronicle](../../book/src/chronicle/the-digest.md). Decision 0020 governs
the form.

## The headline: nine defects, all authored by the plan

**Every defect this campaign hit originated in the implementation plan's own
text. Zero originated in implementer code.** Nine of them, across ten tasks:

1. **Module declarations ahead of their files.** `scan/mod.rs` declared
   `pub mod capability;` (Task 6's file) and `render/mod.rs` declared
   `pub mod doctor;` / `pub mod delta;` (Tasks 6 and 8's), while `main.rs`
   referenced `render::delta` before Task 8 created it. Three compile failures
   waiting in the plan. Caught by the controller's own pre-flight scan, before
   Task 1 was dispatched.
2. **A `[[bin]]` with no `main.rs`.** Task 1's Files list omitted
   `src/main.rs` while the `Cargo.toml` it specified declared a binary target.
   Found by the implementer, resolved with a stub that Task 7 later replaced.
3. **A real E0716 in the plan's test text** — a temporary `String` borrowed
   through a `Vec<&str>`. The implementer fixed it with a local binding and
   kept the assertions byte-identical.
4. **The scope-inference design error, the most serious of the nine.** The
   plan had the decision scanner infer supersession *scope* from the
   parenthetical after "Superseded by". The live corpus puts three different
   things in that slot — a rationale (0063), a genuine scope (0043), and a
   bare date (0099) — plus a form with no parenthetical at all and the id on
   the next line (0006). The distinction is semantic, not syntactic, so no
   parser can make it. Left in, the index would have rendered "still in force:
   2026-08-04" and kept a wholly-superseded decision in force, breaking S4.
   The spec had already placed supersession in the *asserted* column; the plan
   contradicted the spec, and the spec governs.
5. **A bracket-match that hit a type annotation instead of a literal** in the
   `allowed_external` scanner text.
6. **The S6 coverage gap.** The plan's own coverage table promised S6 from
   Task 6, but Task 6's text removes only the layering block; 13 fact-asserting
   `echo` lines remained and no later task touched them. S6 would have landed
   as a partial success labelled done. Caught by the Task 6 reviewer.
7. **A one-argument call to a two-argument function** — Task 7's `main.rs`
   snippet called `index(&all_decisions())` after Task 5 shipped a two-arg
   signature.
8. **A phantom `digest check` subcommand** named in Task 7's Interfaces line
   and specified by no step anywhere in the campaign. Correctly omitted.
9. **`git show --name-only` conflating *touched* with *added*.** Task 8's
   `decision_effective_commit` used an unfiltered `git show` to find when a
   decision took effect. 0043's renumbering commit also *modified*
   `docs/decisions/0026-*` and, being newer, shadowed the true add commit —
   yielding 265/403/138 instead of 171/403/232. Fixed with
   `--diff-filter=A`.

Every one was caught: by TDD, by a task reviewer, or by an implementer
declining to paper over it. None reached the branch tip.

**The lesson is not "plans have bugs."** It is that in a campaign whose entire
subject is documents drifting away from the things they describe, the
controlling document was the least reliable artifact in the room — and it was
the least reliable *because* it was prose asserting facts about code that did
not exist yet, checked by nothing. It is the same class of object as
`scripts/doctor.sh`'s twenty `echo` lines, which is what the campaign was
built to eliminate. The plan is written under exactly the conditions
`PROC-11` names as pathological: fast-drifting facts, stored, hand-maintained,
drift-checked by nobody.

Defect 9 is the one to keep, because it is the one the mechanism caught rather
than a reader. The fix made the code **agree** with a hand measurement taken
earlier, rather than diverge from it — the hand-measured ground truth worked
as an oracle. That is the argument for measuring by hand *first* even when you
intend to automate it: the automation has no other way to be checked.

## The second finding, of equal weight: trusting an enforcement artifact

**Twice in one session the campaign lead reached a confident, wrong conclusion
by trusting an enforcement artifact without asking what it enforced.**

**First, the timing baseline.** `docs/timings/test-baseline-MacBookPro.tsv`
is committed, per-host, specific to the millisecond, and consulted by an
alarm. It reads as authoritative. It was 1,055 commits stale, and reasoning
from it produced a 3.8× cost misattribution and a wrong campaign direction.
That one at least announced itself under re-measurement, and it became the
campaign's motivating example.

**Second, and worse, `docs_consistency`.** Task 8's premise — that the delta
view would find a real intent-versus-reality gap on live repo state — was
declared **falsified** and the task halted. The evidence looked airtight:
`cli/tests/fixtures/registry-numbered-ids.txt` freezes exactly 403 numbered
identifiers, the registry contains exactly 403, `comm` shows zero live
identifiers outside the frozen list, and `cli/tests/docs_consistency.rs`
enforces decision 0026's "no new numbered id" rule and passes 17/17. The
conclusion drawn was that 0026 is enforced and honoured, therefore no gap
exists.

Nathan disagreed, and was right. Git archaeology settled it: the registry held
**171** numbered identifiers immediately before 0026 ratified, and **403**
when the fixture froze 17 days later. **232 were minted after the rule
forbade them.** The freeze grandfathered every one.

The green check was never evidence that the rule had been honoured. It was
evidence that the rule *is being honoured now, against a baseline that
absorbed every prior violation.* The distinction is the whole finding, and it
generalises past this campaign:

> **A passing guard tells you the current state matches its fixture. It tells
> you nothing about whether the fixture was ever right.** Before citing a
> green check as proof a rule was kept, ask when its baseline was written and
> what was already true when it was.

Two corollaries earned here:

- **The 1,402-versus-403 confusion made the wrong conclusion easy.** The
  figure "1,402 numeric identifiers" carried into the spec counts *references*
  to 403 grandfathered identifiers, not violations. A count taken by a grep
  that does not distinguish a row-leading id from a prose mention is not the
  count you think it is. The shipped test now pins row-leading-only counting
  precisely because its absence produced the retracted falsification.
- **The gap was invisible in current state and visible only in history.**
  That is not an accident of this instance: a freeze-the-violations remedy is
  *designed* to make the violation invisible going forward. Any check of the
  form "does the corpus match a frozen list" needs its list dated, and the
  delta view now reads git for exactly this reason.

Both instances share the shape with defect 9 above — an artifact was read for
a claim it does not actually encode. This project has recorded that lesson
before, most recently as *an artifact is evidence only of what it actually
encodes*. It recurred here twice in one day, in a campaign about that exact
subject, which is the strongest argument available that stating it is not
enough and it wants a mechanism.

## The campaign's own brainstorm re-derived PROC-11 from scratch

The idea this campaign builds on — the unmix doctrine, `PROC-11` — was
already in the registry, with its drift-rate rule already written down, in the
form the design ultimately used. The brainstorm re-derived it from first
principles before anyone found the row.

That is the corpus problem demonstrating itself: 2.07 million words is not a
searchable object, and the registry-first habit the `docs/CLAUDE.md` mandates
only works if the thing is findable. It cost this campaign an ideonomy pass
and produced no wrong outcome — the re-derivation agreed with the row — but a
re-derivation that had *disagreed* would have shipped as a fresh idea and
minted a duplicate, which is the documented `TOOL-24` failure.

Four registry rows turned out to describe this campaign before it existed:
`UNI-29`, `UNI-21`, `UNI-28`, `PROC-11`. None foreclosed it. They
under-specified it — every one named the 100% position and none named a rung
short of it, which is why the idea kept being picked up and parked. **An idea
that is repeatedly reopened and re-parked is not evidence of indiscipline; it
is evidence that the registry is missing an intermediate row.** That is worth
watching for, because it is a cheap diagnostic on rows already in hand.

## A near-miss the mutation discipline caught: the vacuous drift check

S2 requires the drift check to be **demonstrated red on command**. Task 7's
first attempt reported the mutation as FAILED to produce a red check — and the
reason was not the mutation. `docs/digest/` was a brand-new directory with no
index entry, and **`git diff --exit-code <path>` is silently vacuous against
an untracked path.** The check was not failing to fire; it was incapable of
firing.

The fix was one `git add`. The hazard is structural and remains open: the
next campaign that introduces a new generated directory gets the same free
pass, and `scripts/regenerate-artifacts.sh` has no guard against it. Nothing
in the repository asserts that every path named in the canonical drift-check
command is tracked.

This is the second time this project's require-RED discipline has caught
something that inspection did not, and it is worth naming what specifically
did the work: not "we ran the check", but "we required the check to fail on
demand, and it would not". A green drift check that cannot fail is
indistinguishable from a green drift check that passed.

## Carried forward, none dropped

- **The MCP surface has no callable path.** Spec §4.8 put it in v1 scope; v1
  ships `handle_assert` / `handle_query` as tested library functions reachable
  only from tests — no CLI subcommand, no server, no transport. Nathan ruled
  ship-as-is-and-record-the-gap (2026-08-08). The spec's risk table row
  ("Authoring friction kills adoption") is now marked **open**, not mitigated,
  because the mitigation is not reachable by a user. Follow-on candidate, not
  v1: a thin `digest assert` / `digest query` CLI, or the real MCP transport
  once the vocabulary settles. **The adoption risk this row names is live**:
  today, asserting a fact means hand-editing JSONL.
- **The untracked-path hazard above** — structural, unguarded, and it will
  recur on the next new generated directory.
- **The delta view's two tests are live-git integration tests.** They read
  real history through `git`. A shallow checkout breaks them — loudly, which
  is the safe failure mode, but any CI that runs them needs full history.
  **The RENDERER was the opposite, and that is the half that mattered**: the
  tests are not what `regenerate-artifacts.sh` runs. Under `git clone
  --depth 1` — `actions/checkout@v4`'s default — the graft boundary makes
  every file look newly added, so the archaeology named the graft root as the
  commit a rule took effect at, and `numbered_ids_at` returned `0` for the
  unresolvable `<effective>^` exactly as it would for a genuine zero. The
  renderer emitted a confident, plausible, wholly wrong sentence — wrong
  commit, wrong baseline, wrong count — in the one artifact whose entire
  purpose is to not say false things about the project. Fixed in the review's
  fix wave: the counter returns `Option`, a shallow checkout is detected up
  front, and the row says it cannot determine the gap. The general lesson is
  the campaign's own thesis turned inward — **a failure mode is only "loud"
  in the code paths you actually checked**, and a tool that reasons about
  history must distinguish "I looked and found none" from "I could not look."
- **The root `CLAUDE.md` drift-check command omitted `docs/digest/`**, so the
  documented verification would have missed digest drift entirely. Fixed in
  this campaign's closing commit; recorded here because the omission is the
  default outcome — a new generated directory is invisible to a hand-written
  command until someone remembers to add it, and nothing checks that the
  command lists every path `regenerate-artifacts.sh` writes.

## Controller process errors, disclosed

- **A dispatch-prompt ruling was never ledgered.** Task 9's resolution — that
  plain functions over a path are the testable core and a full stdio JSON-RPC
  loop is not required by the task — was the controller's own, made inside a
  dispatch prompt and recorded nowhere. The implementer cited it correctly;
  the reviewer could not find it and rightly flagged the claim as
  unverifiable. **A decision made in a dispatch prompt is invisible to
  everyone downstream of that prompt.** Ledger it when you make it, not when a
  reviewer catches that you did not.
- **A task report over-credited a success criterion.** Task 6's report claimed
  S6 for the whole of `scripts/doctor.sh` when the task's diff cleared one
  section of it. The reviewer caught the underlying gap; the credit itself was
  corrected in the ledger so the close reader would not be misled. This is the
  same over-claim shape as the plan defects — a claim about work, written
  beside the work, checked by nothing.

## What worked

- **Nine plan defects, nine catches, zero reaching the tip.** TDD caught the
  compile-shaped ones, reviewers caught the semantic ones, and implementers
  escalated rather than papering over. Defect 4 in particular was escalated to
  Nathan *before* review, on the correct grounds that the plan contradicted
  the spec.
- **A hand measurement used as an oracle for its own automation** (defect 9).
  Without the hand-derived 171/403/232 there would have been nothing for
  265/403/138 to look wrong against.
- **Independent re-derivation at review.** Task 8's reviewer re-derived
  171/403/232 with a fresh Python reimplementation, independent of the Rust
  under review, and reproduced the shadowing commit directly from git history.
  A reviewer who re-implements rather than re-reads is checking the claim, not
  the code's self-description of the claim.
- **A positive control on a fix round.** Task 9's implementer reverted both
  fixes, confirmed both new tests fail against the unfixed code, then restored
  byte-identically — and the re-reviewer noted honestly that one targeted
  branch (unreadable-but-writable) is covered by reading the match arm rather
  than by the repro. Naming what the proof does *not* cover is what makes the
  rest of it credible.
- **Nathan's dissent on the false falsification.** The retraction is the
  campaign's most valuable single event: it converted a "no gap found, ship
  the null" outcome into the delta view's only real row and into the design
  consequence that the view must read git.

## Follow-ups promoted before teardown

Promoted from the git-ignored campaign scratch, which dies with the worktree.

- **A `digest assert` / `digest query` CLI, or the real MCP transport.** The
  named remedy for the open adoption risk.
- **Importing the idea registry into the digest** — explicitly deferred by
  Nathan; not v1.
- **A tracked-path guard for the drift check**: assert that every path named
  in the canonical drift-check command has an index entry, so a new generated
  directory cannot ship with a vacuous check.
- **A staleness assertion on `docs/timings/test-baseline-<host>.tsv` itself.**
  `make ci` has recorded no green run on this Mac since 2026-07-30 — 1,055
  commits — and the file's last output still reads as authoritative. The
  baseline carries an internal commit stamp that nothing compares to `HEAD`.
- **The host name flapped during this very close.** Every timings row from
  earlier the same day reads `MacBookPro`; this campaign's `rebaseline` row
  reads **`Greyjoy`**. `CLAUDE.md` already documents that the `make ci`
  baseline is keyed on `hostname -s` and that renaming the box forks it
  silently — the first run under a new name finds no file, records, and
  cannot alarm. That is not hypothetical any more: the stale baseline this
  campaign was built around is keyed to a name this machine is not currently
  using. The two facts compound, and neither is visible from inside the check.
- **Registry rows have become prose dumps.** `PSY-11`'s single row exceeds
  the length of most design documents. Whatever the digest's eventual row
  format is, it should make that shape impossible rather than merely
  discouraged. (The 600-character cap in `docs_consistency` is grandfathered
  by a waiver list — the same freeze-the-violations shape as the numbered ids.)
- **`render_volume` is `hornvale-book`'s cost centre** (measured 2026-08-08 at
  `64e8c667`): 32/32 book tests calling it cost ≥29 s; 7/7 not calling it cost
  ≤6.7 s. World construction is not the cost (~1.9 s). Visible suspect:
  per-people derivation running once in `render_volume_from`'s tongue-lines
  loop and again per voice in `chorus_sections_from`. **Not profiled** — a
  reading, not a measurement.
- **The `opt-level` hypothesis, untested.** `[profile.dev.package.*]` sets
  `opt-level = 2` for kernel, language, terrain, climate and worldgen, but not
  for `hornvale-book` or `hornvale-vessel` — the two most expensive crates
  (9.4% and 31.6% of suite cost). Cheap A/B: add the entries, re-time.
- **The real cost centre was never discussed:** `hornvale-vessel` (31.6%) plus
  `hornvale-worldgen` (29.6%) is 61% of suite cost.
- **`claim_shape.rs` has a coverage gap** (inherited, re-observed): it is
  default-deny on *seed-looping* tests only, so a test building a single world
  with no loop needs no `claim:` tag — 26 of 39 measured book tests carried
  none for that reason.
- **Two governing documents still cite decision 0006 as live authority** —
  the root `CLAUDE.md` Determinism section and one of the self-map's own
  asserted `self-map-line` facts — after 0099 superseded 0006 wholly on
  2026-08-04. Deliberately not fixed at the close: retiring the epoch-suffix
  rule from the determinism doctrine is a substantive call, not a docs sweep.
  It is the campaign's own first harvest and belongs to whoever makes it.

## Deferred minors, carried so they are not lost

Each was reviewed, judged non-blocking, and left in place with its reason.

- The one-line-diff test would pass against an insertion-order-preserving
  implementation with in-place overwrite; the ordering property holds only
  *jointly* with the stable-ordering test. **A "diff stays small" test needs an
  out-of-order companion, not just a monotonic one.**
- Tie order for two facts sharing a `(subject, predicate)` key — possible only
  for non-functional predicates — is unspecified and untested. The self-map
  render works around it by giving every line its own subject id.
- The vocabulary test does not assert the predicate set is *closed*; an eighth
  unrequested predicate would pass silently. (It has since legitimately grown
  to eight with `self-map-line`.)
- `every_committed_decision_parses` asserts only a non-empty title and a count
  ≥ 112; it would pass with `superseded_by` wrong for every file. Correctness
  there rests on four synthetic unit tests plus a reviewer's hand-trace of all
  four real forms in the corpus.
- 0082's real form — a bare "Superseded by" with the bracket id on the *next*
  line — is covered by no unit test containing a newline in that position;
  hand-verified only.
- A `supersession-scope` entry for a non-superseded decision is silently
  discarded by the render's match arms. Unreachable with v1 data.
- The no-hard-coding guard on the allowlist forbids only the literal
  `serde_json"`, not a partial hard-code of `libm` or `serde` alone. Adequate
  for the historical regression shape, narrower than its name.
- No unit test covers `self_map_lines`' subject-range grouping, the
  `{decision_count}` substitution, or `decision_scopes` — verified by the
  manual `make doctor` comparison and the mutation proof.
- `numbered_ids_at`'s path fallback takes the first non-zero result with no
  signal if two registry paths were simultaneously non-zero at some untested
  revision.
- The destructive MCP test's permission restore is not panic-safe (no RAII
  guard). Inert against the committed code, which returns early via `?`.

## Residuals carried past the close (recorded, not fixed)

Nathan ruled "close 'er up" with these known and open. They are here because
the campaign's scratch ledger dies with its worktree.

- **`render::delta`'s "No gaps found" reassurance is still reachable when
  `decision_effective_commit()` returns `None`** — a bare `continue` that does
  not increment the emitted-rows counter. Reproduced by moving `.git` aside:
  the report prints "No gaps found. Verify the view still works before
  believing this." That is the same could-not-look-reported-as-looked-and-
  found-nothing collapse the pre-merge Critical fixed, one call site over.
  Pre-existing, off CI's path (a checkout always has git), one line to fix.
- **The `--depth 50` justification in `delta.rs`'s source comment is stronger
  than what reproduces on this history.** `Option<usize>` alone would have
  caught every shallow depth tested; `repo_is_shallow()` remains correct and
  cheap but is not demonstrably load-bearing here.
- **The MCP surface ships unreachable** — no CLI subcommand, no transport.
  Asserting a fact today means hand-editing `docs/digest/facts.jsonl`. The
  spec's risk row is marked OPEN rather than mitigated.
- **`hostname -s` on the Mac now returns `Greyjoy`.** Every timings row from
  earlier the same day says `MacBookPro`, so the per-host baseline has FORKED —
  CLAUDE.md's documented `make ci` blind spot #2 landing on top of #1. The
  stale baseline that opened this session is keyed to a name the machine no
  longer uses. Not this campaign's to fix; recorded so the next reader of a
  timings row knows to check which host it claims.
- **Root `CLAUDE.md` and one asserted self-map fact still cite decision 0006
  as live authority** after 0099 wholly superseded it. The generated in-force
  index correctly omits 0006 — this was the tool's first harvest, minutes after
  it first rendered. Left deliberately: retiring the epoch-suffix rule from the
  determinism doctrine is a substantive call, not a docs sweep.
