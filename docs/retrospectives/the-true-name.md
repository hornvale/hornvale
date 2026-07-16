# Retrospective — The True Name (ECS program, campaign 2)

Process lessons only; the product story is the chronicle. Campaign 2 of the
entity-component program: give a kind a typed, stable identity (`KindId`),
byte-identical. Four TDD tasks, subagent-driven.

## What worked

- **A byte-identical campaign is the cheapest kind to get right.** Because the
  label was already the de-facto serialized identity, the entire campaign was a
  type-safety-and-documentation refactor with no behavioral change — and the
  existing artifact drift-check certifies that for free. Every task's success
  criterion reduced to "the byte-identity suite is still green." When a
  campaign can be framed this way, frame it this way.
- **Review effort proportioned to diff risk.** The substantive task (re-keying
  the registry and migrating consumers, 11 files) got a full reviewer subagent,
  which independently re-ran the byte-identity suite and traced the one API
  widening to ground. The two trivial tasks (a kernel newtype with complete
  plan code; a doc comment plus one test) got a lightweight controller
  verification of the committed diff instead of a full reviewer dispatch. The
  SDD guidance ("review scaled to the diff's size, complexity, and risk")
  earns its keep — a mechanical newtype does not need a reviewer's whole turn.
- **Keeping campaign 2 separate from campaign 3 was right.** The metaplan left
  the merge open; splitting kept this a clean, low-risk, independently
  verifiable foundation (`KindId` + re-key) rather than bundling it with the
  large `SpeciesDef`-dissolve refactor. Small-and-shippable held.

## What surfaced

- **A `&'static str` identity propagates its `'static` requirement.** Wrapping
  the label as `KindId(&'static str)` forced one helper's `family` parameter to
  widen from `&str` to `&'static str` (to construct a `KindId` for a lookup).
  It is benign now — every family value is authored and `'static`, and no
  ledger-derived runtime string can reach it — but it is a real consequence of
  the build-state label choice worth remembering: the moment a genuinely
  runtime-constructed kind reference needs to become a `KindId`, the answer is
  the free-text resolution pattern (resolve the string against the registry),
  not widening callers to `'static`. The migration already uses that pattern
  correctly for the two ledger-derived species strings; the family case simply
  did not need it.
- **Task-number reuse across campaigns collides on scratch filenames.** The
  `task-N-brief.md` / `task-N-report.md` scratch files are keyed by task number,
  so campaign 2's Task 1 overwrote campaign 1's stale Task 1 report. Harmless
  (scratch, gitignored, the campaign is closed), but a reminder that per-campaign
  scratch is not namespaced — read a report's heading, not just its filename.

## Carried forward

- Nothing deferred from this campaign's own scope — it shipped complete.
- **Next:** campaign 3 (dissolve `SpeciesDef` into domain-owned component
  registries keyed by `KindId`) — the meatier refactor this foundation exists
  for. The `phantom `Id<T>` unification and label interning (`Symbol(u32)`)
  remain optional/later (campaign 4), unneeded so far.
