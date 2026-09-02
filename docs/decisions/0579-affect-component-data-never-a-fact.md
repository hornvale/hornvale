# 0579. Affect — component data, never a fact

**Status:** Accepted (2026-09-02) · **Decider:** Nathan (autopilot) · **Campaign:** The Avowal

## Context

`windows/sentiment::snap_judgment(judger, target)` computes a `Judgment {
warmth, competence, emotion }` between two peoples from their authored
species/demography/language attribute vectors alone — the module's own doc:
"**No world is built**: every distance here is a function of two peoples'
authored attribute vectors alone, computable before any seed exists."
`grep -rn 'Fact\|predicate\|commit' windows/sentiment/src/*.rs` returns
nothing: the capability exists and commits zero facts, so
`bundle:felt-affect` scored 0/3 for a reason with nothing to do with the
capability being absent (spec §2.1). Committing `snap_judgment`'s output as
ledger facts would store ~840 values (15 peoples x 14 targets x 4 quadrant/
magnitude facts, roughly) identical in every world ever generated — the
second source of truth decisions 0346 and 0366 already rule against.

Decision 0576 built `Provision`, the table `tropes::resolve` consults
instead of a bare registry scan, with the component and session homes
present in the type but unreachable by construction (`Home::Component`/
`Home::Session` both carried the uninhabited `Unwired`). This decision wires
the component home.

**The grain, ruled by Nathan at brainstorm** (ledger entry #2, restated here
because it is this decision's load-bearing constraint): `snap_judgment` is
**people-to-people** (`KindId x KindId`) — the corpus's `feels-toward` is
**person-to-person**. Committing people-scale data under the corpus's own
`feels-toward` token would move the trope headline on a grain the corpus
never asked for, which is exactly the quiet mis-scoring decision 0136 exists
to prevent.

## The decision

1. **`predicate:affect-kind` and `predicate:affect-intensity` are declared
   `Present(Home::Component(sentiment_affect_holds))`** —
   `Home::Component` now carries a real `ComponentResolver` (`fn() -> bool`)
   instead of `Unwired`. The resolver calls `hornvale_sentiment::catalog()`
   and `snap_judgment` end to end on the catalog's first two peoples
   (deterministic `PeopleId` order) rather than merely checking the catalog
   is non-empty, so a "served" answer actually exercises the judgment
   pipeline it claims to cover. Neither token is ever registered with
   `ConceptRegistry::register_predicate`, and no `Fact` is ever committed
   under either name — the component home bypasses the ledger entirely,
   which is the architectural statement that this is build-state, not
   saved state.

2. **`predicate:feels-toward` is declared explicitly `Absent`, not merely
   left undeclared** — `Correspondent::Absent(Unserved::NotServed(...))`
   naming the grain ruling verbatim, so a reader of the row (not only the
   outcome) sees *why*: no person-scale producer ships this campaign.
   `bundle:felt-affect` therefore reads **2/3 and stays blocked** — this is
   the campaign's honesty guarantee, pinned by
   `cli/tests/suite/provision.rs::feels_toward_does_not_resolve` and
   `::bundle_felt_affect_reads_two_of_three_and_stays_blocked`. If a later
   change ever completes `felt-affect` without a real person-scale
   producer, those two tests are what must catch it.

3. **Owner of the two token constants: `domains/species`, not
   `windows/sentiment`.** A window may not declare vocabulary a domain must
   own — `windows/sentiment` sits above `domains/species` in the `kernel ->
   domains/* -> windows/* -> cli/` layering (`cli/tests/suite/
   architecture.rs`), and a window's job is to *present* a domain, not to
   mint the domain's naming surface. `hornvale-species` already owns the
   `psyche_registry`/`society_registry` component data the judgment is
   computed from (it is kernel-only, per the one-rule-that-must-never-bend
   in `domains/CLAUDE.md`), and `windows/sentiment` already depends on it
   for exactly that data — so declaring `AFFECT_KIND`/`AFFECT_INTENSITY`
   there keeps `windows/sentiment` purely presentational and follows the
   same subject-type precedent decision 0578 used for `parent-of`/`kin-of`.
   Both constants are plain `pub const &str`, never passed to
   `register_predicate` — there is no ConceptRegistry row for either, by
   design, since neither is ever fact-worthy.

4. **`cli/src/provision.rs::Provision::build`** composes `from_registry`'s
   ledger rows with the two new component rows and the explicit
   `feels-toward` absence, and is what `tropes::resolve`,
   `tropes::render` and `tropes::render_matrix` now all call —
   `from_registry` alone stays available (and stays ledger-only) for
   callers that want that narrower scope (`trope_witness.rs`'s fixture,
   `Provision`'s own unit tests).

5. **The `registry_tokens`-vs-`serves` divergence is reconciled by deleting
   `registry_tokens` outright and reading `Provision::served_tokens` (a new
   method: every declared token the table currently serves, home-blind)
   at both call sites** (`tropes.rs`'s Leverage fan-in `held` set, and the
   token count in the Columns section's preamble). Before this decision,
   both computed "what's served" by re-scanning the registry directly,
   which agreed with `Provision::serves` only because every served token
   happened to be a registry token — a coincidence this decision breaks the
   moment a component-home row exists. Left unreconciled, a witness-blocked
   situation whose bundle includes a component-served token would have
   passed `resolve`'s `missing.is_empty()` check while still failing
   `held.contains(token)` in `render`, reaching the fan-in/`closest`
   computation and falsifying both the "by construction" comment at
   `tropes.rs`'s fan-in loop and the witness-blocked exclusion rationale
   beside it, together. `served_tokens` is the single computation both the
   Leverage table and `resolve` itself now share, so the two cannot
   disagree again without one caller stopping to call it. (No witness
   exists yet for any situation touching `felt-affect`, so this reconciles
   an invariant rather than fixing an observed wrong count today — but the
   invariant is exactly what a future witness would need to hold.)

## Consequences

- **No fact is committed, verified on a real world.**
  `cli/tests/suite/provision.rs::no_fact_is_committed_serving_affect_tokens`
  serializes a real world's ledger before and after every way this task
  exercises the component home (`Provision::build`, `serves`, and a full
  `resolve` run requiring both affect tokens) and asserts byte-identity.
  `resolve` only ever takes `world: &World` (a shared reference), so the
  borrow checker already forbids a mutation through this path — the test is
  the executable record of that property, not a probe that could plausibly
  catch a violation the type system missed.
- `bundle:felt-affect` moves **0/3 -> 2/3**, still blocked — exactly spec
  §5's preregistered null. `polti-1895` stays **0 of 36** and
  `tvtropes-2012` stays **0 of 409**: every situation `felt-affect` used to
  block is still blocked by `feels-toward` (if `felt-affect` is the only
  missing bundle) or by another bundle entirely.
- `cli/Cargo.toml` gains `hornvale-sentiment` as a dependency — architecturally
  sound (`cli` sits above every window in the layering) but new, so
  `book/src/reference/layering-generated.md` moves and is regenerated in the
  same commit.
- `Home` drops its `PartialEq`/`Eq` derive. A derived `PartialEq` would
  compare `Component`'s `fn() -> bool` payload by address, which `rustc`
  correctly warns is not a meaningful comparison under
  `unpredictable_function_pointer_comparisons` (`-D warnings` in the commit
  gate); nothing in the codebase ever compares `Home` values by equality
  (every caller constructs or pattern-matches), so the fix removes the
  unused derive rather than suppressing the warning.
- **A future `feels-toward` registration would not silently reopen the
  grain question, and the direction this fails in is the safe one.**
  `Provision::declare` overwrites any existing row for the same token, and
  `Provision::build` runs `from_registry`'s ledger scan first and the
  explicit `predicate:feels-toward` `Absent` declaration last — so if a
  later campaign registers `feels-toward` as an ordinary
  `register_predicate` row (giving it a real `Home::Ledger` row via
  `from_registry`), `Provision::build`'s hardcoded `Absent` declaration
  still overwrites it and wins. `bundle:felt-affect` would stay 2/3 and
  both honesty tests (`feels_toward_does_not_resolve`,
  `bundle_felt_affect_reads_two_of_three_and_stays_blocked`) would stay
  green even though a real producer now exists — a tripwire that fires by
  BLOCKING a legitimate landing rather than admitting an illegitimate one,
  with the reason discoverable at the row
  (`Unserved::NotServed`'s string) for whoever debugs why a newly
  registered predicate is not moving the trope score. Completing
  `felt-affect` at the person grain therefore requires deleting this
  explicit `Absent` declaration from `Provision::build`, not merely
  registering the predicate elsewhere.

## Alternatives discarded

- **Registering `affect-kind`/`affect-intensity` as ordinary
  `register_predicate` rows**, letting `Provision::from_registry`'s existing
  scan pick them up for free. Rejected: it would make both tokens appear in
  `hornvale concepts` and the registry-generated reference page as if a fact
  could carry them, which is false, and would require the resolver to
  refuse committing on every call site rather than the capability simply
  having no commit path at all.
- **Owning the two constants in `windows/sentiment`.** Rejected outright by
  the layering rule — a window may not declare vocabulary a domain must own.
- **An enum naming each known component producer** (`ComponentResolver::
  Sentiment`, …) instead of a bare `fn() -> bool`. Rejected as premature
  abstraction: there is exactly one producer today, and a function pointer
  keeps a second one a `declare` call away rather than a match arm added
  here later.
- **Leaving `predicate:feels-toward` simply undeclared** rather than an
  explicit `Absent` row. Rejected: an undeclared token and a deliberately
  refused one resolve identically (`Blocked`), but only the explicit row
  lets a future reader of `Provision::build` see *why* without reading this
  decision or the ledger first — and `Unserved::NotServed` already requires
  a mandatory reason, so there is no extra cost to stating it.

## See also

Spec §4.4, §2.1 (`docs/superpowers/specs/2026-09-01-the-avowal-design.md`);
decision 0576 (the provision table and the ledger home); decision 0578 (the
subject-type ownership precedent this decision follows for `domains/
species`); `docs/superpowers/ledgers/2026-09-01-the-avowal.md` entries #2
(the grain ruling) and the Task 6 entry; `cli/src/provision.rs`;
`domains/species/src/lib.rs` (`AFFECT_KIND`, `AFFECT_INTENSITY`);
`cli/tests/suite/provision.rs`.
