# Retrospective — The Presiding (SKY-25)

One page, process only. Product is in the chronicle
(`book/src/chronicle/the-presiding.md`) and the spec
(`docs/superpowers/specs/2026-07-17-the-presiding-design.md`).

## Headline lesson

**The registry-first / placed-first inference was found in three more
documents this campaign — all authored independently, all stating bugbear
commits first, none having generated a world to check.**

SKY-25's registry row, the terminator acceptance battery that discovered it,
and the `a_frozen_sky_never_heads_a_cyclic_pantheon` calibration all said the
same thing: `registry()`'s alphabetical order plus the founder floor make
**bugbear** commit its pantheon first on every seed. The 1000-seed census
reads `belief-kind-bugbear` as Absent on **1000 of 1000** seeds. Bugbear
places nowhere.

The Named's retrospective already named this exact inference and its exact
counter ("check placement, not sort order"), one week earlier, about the same
four-species registry. It was made three more times anyway. The failure is
not ignorance of the lesson — the lesson was written down. The failure is
that the inference *feels* like deduction: its premise (the alphabetical
sort) is true and verifiable in the source, so the conclusion inherits that
felt certainty, and the one empirical step — *does that species actually
place?* — is invisible precisely because it's the only part that isn't in the
code you're reading.

**Concrete change to try:** any claim of the form "species X is
first/dominant/absent/present" is a row in a census, not a line of reasoning.
The cheap discipline is a standing rule: if a comment or spec asserts a
species' population behaviour and there is no pasted `almanac`/census output
next to it, it is unverified — treat it as a `TODO(measure)`, not a fact. Three
documents would have been correct on first writing if any of them had run
`hornvale new` on one seed.

## The near-miss that defines the campaign

The regen raced an epoch and I nearly shipped dead numbers.

The Reckoning — a total epoch, every world changed — landed on main while
this campaign's authorized census regen was running. The regen measures the
tree it was launched from, so its 1000 rows described pre-Reckoning worlds.
After absorbing main, the re-pinned calibration **passed**: it reads the
fixture it just wrote, and the bytes matched. Green, and meaningless. This is
this campaign's own chronicle turned on its author — a drift check pins output
against change, not against being right — and it would have shipped a stale
pin under a passing gate if the absorption diff (1001 of 1001 census rows
changed) hadn't forced the question.

Two things saved it: noticing the epoch in the merge, and Nathan authorizing a
second regen against the merged physics. The second regen also cleared The
Reckoning's own accepted census debt (FU6, 633/1000 stale, "main red on the
census tests") — one post-epoch regen paid two campaigns' debts.

**Concrete change to try (now in memory `regen-races-the-epoch-that-lands`):**
absorb main and re-run `make preflight` *immediately before* launching a
regen, not just before the merge — the launch-to-finish window is 30+ minutes,
long enough for another session's epoch to land. And when a regen and a
pending epoch are both in flight, land the epoch first and regen once.

## What the process caught

**Self-review caught two plan defects before dispatch, and the implementer
caught two more.** The writing-plans self-review found an invented test helper
(`full_view(42)`, real idiom `FullView::build(Seed(42), &SkyPins::default())`)
and a missed consumer. The exhaustive class-grep then found three *more*
consumers my eyeballed grep had missed — two of them substantive physics
invariants (`locked ⇒ eternal`, `spinning ⇒ cyclic`) that a careless delete
would have dropped silently. The implementer found a fourth (`registry_metric_
count_is_pinned`). My "exhaustive" enumeration was wrong four times; the grep
was right. The lesson is not "grep harder" — it is that a plan's list of
call-sites is a hypothesis, and the implementer should be told to re-derive it
from the tool, which this plan did.

**The golden-pins.sql tripwire finally fired at the right time.** It had missed
every re-pin since its creation (nobody ran `census-check` at re-pin time).
This campaign ran it, and it caught the retired `belief-kind` column reference
before commit — its designed purpose, working for the first time. Both re-pin
paths (Rust `MetricValue` and raw-CSV DuckDB) now agree at 112/0/1.

**Two blast-radius facts the plan got wrong, both instructive.** (1) I claimed
Task 1 would stay green because only `calibration.rs` reads the fixture. False:
`load_rows` validates the *whole* header against the live registry, so any
metric add/remove reddens every census-reading test at once. There is no
"small" metric change. (2) The Reckoning epoch turned out *not* to move the
religion pins at all (astronomy facts don't change which phenomena head a
pantheon) — so my alarm was right to raise but wrong in fact, and the honest
record is that the re-pin was three *language* means, not the religion counts.

## Follow-ups

Carried from `.superpowers/sdd/followups.md`:

1. **The type-audit report freshness gap** — `make gate` now runs the tag
   `check` (shipped this session, `0481311`), but the committed
   `docs/audits/type-audit-report.md` can still go stale silently on the
   CI-only drift path (CI is manual, decision 0042).
2. **`pantheon-size` and `cult-form` are the same legacy singular** —
   both documented "the goblin flagship's", both duplicating their `-goblin`
   twin. The Presiding cured `belief-kind`; these await the next census-touching
   campaign (deferred to avoid extra column churn on this regen).
3. **The per-species metrics stop at goblin+kobold** — `pantheon-size-*`,
   `cult-form-*` predate The Branches; hobgoblin and bugbear have no coverage.
4. **Goblin is population 1 on every seed measured** (22/22 spinning, 9/9
   locked). The founder floor guarantees a flagship; nothing guarantees it
   grows. Whether a permanently-one-soul people is correct is a demography
   question — and it is the mechanism that let a single soul speak for a world.
5. **`git checkout <file>` is not a mutation-revert on an uncommitted file** —
   it reverts to HEAD, wiping in-progress work; the Task 1 implementer lost
   edits this way. Commit or stash before mutating. (And never `mv file.bak
   file` — mtime defeats cargo's rebuild.) Both belong in the dispatch preamble.
6. **`load_rows` validates the whole fixture header** — recorded so the next
   metric change is scoped as "reddens every census test", not "one file".
