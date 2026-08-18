# The Parley Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the myth-transmission model a clock, a second stance geometry,
and a cross-people contact edge, so an account can leave the people that
witnessed it and arrive damaged — then report a preregistered readout over all
of it without nominating any arm.

**Architecture:** Three separable layers of one graph walk, added in dependency
order because edge *cost* reads node *labels* and labels are defined over
*topology*. A `Transmission` policy value carries all three choices; its
`AS_SHIPPED` constant reproduces today's behaviour exactly, which is what makes
every later arm measurable against a real baseline rather than a remembered one.

**Tech Stack:** Rust 2024, `windows/hearsay` (a read-only window over a
committed ledger), `cargo nextest`, hand-built test ledgers from
`windows/hearsay/tests/common/mod.rs`.

**Spec:** `docs/superpowers/specs/2026-08-17-the-parley-design.md` — read §3
(substrate), §5 (the derivation) and §6 (the preregistration) before Task 1.
The plan argues from the spec; both travel together.

## Global Constraints

- **Dependencies are frozen.** `serde`, `serde_json`, `libm` only, workspace-wide
  (`ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`). This campaign adds none.
  `windows/hearsay`'s runtime deps stay `hornvale-kernel`, `hornvale-history`,
  `hornvale-astronomy`; `hornvale-worldgen`, `hornvale-terrain` and
  `hornvale-species` stay **dev-only**.
- **No `HashMap`/`HashSet`.** `BTreeMap`/`BTreeSet`/`Vec` only. Float ordering
  uses `total_cmp`, or `f64::to_bits` where a non-negative finite float is used
  as a `BTreeMap` key (the existing `variants_about_accumulating` pattern).
- **No wall-clock time.** Time is `WorldTime`/`f64` std days from the ledger.
- **`#![warn(missing_docs)]`** — every public item, field and variant gets a
  one-line doc comment. New `pub` primitives at a boundary need a
  `type-audit:` tag (`bare-ok(<class>)` / `waiver(<reason>)`); `make gate-commit`
  runs the audit as a lint and will refuse an untagged one.
- **`cargo fmt` is the last step before every commit.** Fmt-gate skips are this
  project's most common review finding.
- **`make gate-commit` before every commit.** It is an allow-list
  (`docs/timings/subfloor-roster.tsv`), not the workspace — a brand-new test
  is excluded from it *by design* until a green stage gate records a duration,
  so **never** infer from a green `gate-commit` that your new test ran. Run it
  by name as well.
- **Nothing in this campaign may edit** `windows/hearsay/src/amplitude.rs`'s
  `gen_span` return, `accumulate.rs`'s `step`/`precision_at`, or
  `derive.rs`'s `claims_about`. The first two are The Palimpsest's frozen unit
  erratum (spec §2); the third is campaign 1's pinned no-decay baseline.
- **No arm is ever nominated.** `Perpetration` and `Contact` get **no `Default`
  impl**, deliberately, exactly as `Accumulation` has none. A `Default` would
  silently pick a winner.
- **You may override this plan.** If a step's stated fact is wrong, or a better
  test exists than the one written here, do the better thing and **say so in
  your report**. Three implementers did that on the previous campaign and each
  was right. What you may not do is quietly weaken a check.

---

### Task 1: `Perpetration` — the stance geometry, as two arms

**Files:**
- Modify: `windows/hearsay/src/stance.rs`
- Modify: `windows/hearsay/tests/stance.rs`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces:
  - `pub enum Perpetration { Singleton, Inherited }` — `Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord`, with `pub const ALL: [Perpetration; 2]` and `pub fn label(self) -> &'static str` returning `"singleton"` / `"inherited"`.
  - `pub fn stance_of(ledger: &Ledger, lineage: &Lineage, perpetration: Perpetration, subject: EntityId, who: EntityId) -> Stance`
  - `pub fn is_lossy(ledger: &Ledger, lineage: &Lineage, perpetration: Perpetration, subject: EntityId, teller: EntityId, hearer: EntityId) -> bool`

Note for later tasks: **`variants_about_accumulating` does not read stance at
all** — `grep -n 'stance::' windows/hearsay/src/derive.rs` returns exactly two
lines, one a doc comment and one inside `variants_about`. So `Perpetration`
moves `variants_about` only. That is itself worth reporting: campaign 3's
accumulator dropped the stance filter entirely.

- [ ] **Step 1: Write the failing test**

Add to `windows/hearsay/tests/stance.rs`:

```rust
/// Under `Inherited`, a raider's own child is still a `Perpetrator`; under
/// `Singleton` it is not. This is spec §5.2's entire behavioural difference.
#[test]
fn inherited_perpetration_closes_the_attacker_under_descent() {
    // 1 is the victim; 5 is the attacker; 6 is the attacker's child.
    let mut led = ledger_with(&[(1, None), (5, None), (6, Some(5))]);
    put(&mut led, 1, hornvale_history::OCC_ENDED, Value::Number(10.0));
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(5)),
    );
    let lin = lineage_of(&led);

    // The attacker itself is a Perpetrator under BOTH arms.
    for arm in Perpetration::ALL {
        assert_eq!(
            stance_of(&led, &lin, arm, eid(1), eid(5)),
            Stance::Perpetrator,
            "the named attacker is a Perpetrator under {}",
            arm.label()
        );
    }

    // Its child is where the arms part company.
    assert_eq!(
        stance_of(&led, &lin, Perpetration::Singleton, eid(1), eid(6)),
        Stance::Bystander,
        "Singleton: the raider's child is not implicated"
    );
    assert_eq!(
        stance_of(&led, &lin, Perpetration::Inherited, eid(1), eid(6)),
        Stance::Perpetrator,
        "Inherited: the deed stays the line's own"
    );

    // And therefore the raider's first retelling step is lossy under
    // Singleton and free under Inherited -- the asymmetry spec §3.6 measured.
    assert!(
        is_lossy(&led, &lin, Perpetration::Singleton, eid(1), eid(5), eid(6)),
        "Singleton: attacker -> own child crosses a stance boundary"
    );
    assert!(
        !is_lossy(&led, &lin, Perpetration::Inherited, eid(1), eid(5), eid(6)),
        "Inherited: attacker -> own child does not"
    );
}

/// `Singleton` must reproduce the shipped behaviour exactly, including the
/// precedence rule that a victim's own child which IS the named attacker
/// reads `Perpetrator` rather than `VictimLine`. Spec §3.6 measured 124 such
/// foundings, and they are the whole of the victim line's 4.06%.
#[test]
fn the_attacker_label_still_beats_the_victim_line_label() {
    // 2 is founded FROM 1 and is also what destroyed 1.
    let mut led = ledger_with(&[(1, None), (2, Some(1))]);
    put(&mut led, 1, hornvale_history::OCC_ENDED, Value::Number(10.0));
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(2)),
    );
    let lin = lineage_of(&led);
    for arm in Perpetration::ALL {
        assert_eq!(
            stance_of(&led, &lin, arm, eid(1), eid(2)),
            Stance::Perpetrator,
            "attacker precedence holds under {}",
            arm.label()
        );
    }
}
```

Add whatever imports the file needs (`Perpetration`, `stance_of`, `is_lossy`,
`Stance`, `lineage_of`, `eid`, `put`, `ledger_with`, `Value`); the file already
declares `mod common;`.

- [ ] **Step 2: Run the test to verify it fails**

```
cargo test -p hornvale-hearsay --test stance 2>&1 | tail -20
```

Expected: a COMPILE failure naming `Perpetration` as unresolved. **This is the
one case where a compile-error red proves nothing about the assertion**, so do
not treat it as a satisfied red — Step 4 is where the behavioural red matters.

- [ ] **Step 3: Implement**

In `windows/hearsay/src/stance.rs`, add above `stance_of`:

```rust
/// Whether being the perpetrator of an event is inherited by descent.
///
/// `VictimLine` has always been closed under descent — the subject or any
/// descendant — while `Perpetrator` named exactly one entity. Spec §3.6
/// measured what that asymmetry costs: the raider's first retelling step is a
/// stance crossing 3,694 times out of 3,694, against the victim line's 124 of
/// 3,056. Nobody chose it; it fell out of one label being a singleton and the
/// other a closed set.
///
/// **Deliberately no `Default` impl**, exactly as [`crate::accumulate::Accumulation`]
/// has none: a default would silently nominate one reading of whether guilt
/// inherits, which is a dated decision this campaign is not entitled to make
/// (spec §5.2).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Perpetration {
    /// Ships today: only the entity named by `occ-ended-by`.
    Singleton,
    /// `VictimLine`'s mirror: the named attacker or any of its descendants.
    Inherited,
}

impl Perpetration {
    /// Every arm, in a fixed order so a readout's columns are stable.
    pub const ALL: [Perpetration; 2] = [Perpetration::Singleton, Perpetration::Inherited];

    /// This arm's short name, used as a readout column suffix.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn label(self) -> &'static str {
        match self {
            Perpetration::Singleton => "singleton",
            Perpetration::Inherited => "inherited",
        }
    }
}
```

Then change `stance_of`'s attacker branch to consult the arm, keeping the
`Perpetrator`-beats-`VictimLine` precedence exactly where it is:

```rust
pub fn stance_of(
    ledger: &Ledger,
    lineage: &Lineage,
    perpetration: Perpetration,
    subject: EntityId,
    who: EntityId,
) -> Stance {
    if let Some(Value::Entity(attacker)) = ledger.value_of(subject, hornvale_history::OCC_ENDED_BY)
    {
        let implicated = match perpetration {
            Perpetration::Singleton => *attacker == who,
            Perpetration::Inherited => *attacker == who || lineage.is_ancestor(*attacker, who),
        };
        if implicated {
            return Stance::Perpetrator;
        }
    }
    if who == subject || lineage.is_ancestor(subject, who) {
        Stance::VictimLine
    } else {
        Stance::Bystander
    }
}
```

and thread `perpetration` through `is_lossy` to both `stance_of` calls.

- [ ] **Step 4: Fix every call site, then run the whole crate**

`stance_of`/`is_lossy` have 18 call sites (`grep -rn 'stance_of(\|is_lossy(' --include='*.rs' | grep -v 'pub fn'`).
Every existing one is asserting today's behaviour, so every one takes
`Perpetration::Singleton`.

```
cargo test -p hornvale-hearsay 2>&1 | tail -20
```

**Decision table** — do not "fix" a red by weakening it:

| what you see | what it means | what to do |
|---|---|---|
| all green, new tests pass | `Singleton` is behaviour-preserving | go to Step 5 |
| a pre-existing test fails with `Singleton` | `Singleton` is NOT behaviour-preserving — a real regression in your edit | fix the implementation, never the old test |
| a pre-existing test fails only with `Inherited` | expected; you passed the wrong arm at that call site | pass `Singleton` |
| your new test passes before Step 3 | impossible unless it asserts nothing | rewrite the test |

- [ ] **Step 5: Prove the new test can fail**

The plan will not tell you which mutation to make — a plan author does not know
which lines are load-bearing, and the outside guess has been strictly worse than
the inside search every time this was tried. **Find a mutation of your own that
demonstrates this property: with `Inherited` collapsed to `Singleton`'s
behaviour, `inherited_perpetration_closes_the_attacker_under_descent` fails.**
Assert the text you are replacing actually exists before you replace it, revert
after, and put the mutation and its output in your report.

- [ ] **Step 6: Format, gate, commit**

```
cargo fmt
make gate-commit 2>&1 | tail -3
cargo test -p hornvale-hearsay --test stance 2>&1 | tail -5
git add windows/hearsay/src/stance.rs windows/hearsay/tests/stance.rs
git commit -m "feat(hearsay): stance geometry gains an Inherited arm"
```

(The commit body should say: spec §5.2; `Singleton` reproduces today's
behaviour at all 18 call sites; `Inherited` closes `Perpetrator` under descent
as `VictimLine` already is; no `Default` impl, because adopting one is a dated
decision this campaign may not make.)

---

### Task 2: `Clock` — a dead community cannot hold a later claim

**Files:**
- Create: `windows/hearsay/src/clock.rs`
- Modify: `windows/hearsay/src/lib.rs` (add `pub mod clock;`)
- Test: `windows/hearsay/tests/clock.rs` (create)

**Interfaces:**
- Consumes: nothing from Task 1.
- Produces:
  - `pub enum Clock { Off, Alive }` — `Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord`, `pub const ALL: [Clock; 2]`, `pub fn label(self) -> &'static str` → `"no-clock"` / `"clock"`.
  - `pub fn admits(clock: Clock, ledger: &Ledger, occ: EntityId, event_day: f64) -> bool`

- [ ] **Step 1: Write the failing test**

Create `windows/hearsay/tests/clock.rs`:

```rust
//! Spec §5.1: a community that had already ended cannot hold a claim about a
//! later event. Spec §3.5 measured 1,959 of 164,822 holders (1.19%) that do,
//! confirmed on named instances up to ~500 years dead.

mod common;

use common::{eid, ledger_with, put};
use hornvale_hearsay::clock::{Clock, admits};
use hornvale_kernel::ledger::Value;

#[test]
fn a_community_that_ended_before_the_event_is_refused() {
    let mut led = ledger_with(&[(1, None)]);
    put(&mut led, 1, hornvale_history::OCC_ENDED, Value::Number(100.0));

    assert!(
        !admits(Clock::Alive, &led, eid(1), 200.0),
        "ended day 100, event day 200: refused"
    );
    assert!(
        admits(Clock::Alive, &led, eid(1), 50.0),
        "ended day 100, event day 50: admitted"
    );
    assert!(
        admits(Clock::Off, &led, eid(1), 200.0),
        "Clock::Off admits everything -- it is today's behaviour"
    );
}

#[test]
fn a_community_that_never_ended_is_always_admitted() {
    let led = ledger_with(&[(1, None)]);
    assert!(
        admits(Clock::Alive, &led, eid(1), 1.0e9),
        "no occ-ended fact: still standing, admitted at any day"
    );
}

/// The boundary is exact, not a band. A community ending on precisely the
/// event's day IS admitted: it was there. Spec §5.1 inherits the exact-equality
/// discipline `witnesses_of` already uses for survivor refoundings, where 477
/// of 562 pairs sit at a gap of exactly 0.0 -- so there is no threshold to
/// tune and no near-miss band to argue about.
#[test]
fn ending_on_the_event_day_is_admitted() {
    let mut led = ledger_with(&[(1, None)]);
    put(&mut led, 1, hornvale_history::OCC_ENDED, Value::Number(100.0));
    assert!(
        admits(Clock::Alive, &led, eid(1), 100.0),
        "the subject of an ending is a witness to its own ending"
    );
}
```

- [ ] **Step 2: Run to verify it fails**

```
cargo test -p hornvale-hearsay --test clock 2>&1 | tail -20
```

Expected: compile failure, `hornvale_hearsay::clock` unresolved.

- [ ] **Step 3: Implement**

Create `windows/hearsay/src/clock.rs`:

```rust
//! Whether a community was still standing when an event it holds took place.
//!
//! Spec §5.1. `variants_about` and its accumulating sibling walk the founding
//! tree as pure structure, with no notion of when anybody existed, so a
//! community that ceased 500 years before an event still inherits a claim
//! about it: 1,959 of 164,822 holders (1.19%), measured in
//! `tests/probe_contact_substrate.rs`.
//!
//! This is also the second world-time clock campaign 2's §7 said did not
//! exist ("`Claim` carries no time, so `hops` is the only clock"). The ledger
//! carries founding and ending days, and this reads them.

use hornvale_kernel::ledger::{EntityId, Ledger, Value};

/// Whether transmission respects world time.
///
/// **Deliberately no `Default` impl** — see [`crate::stance::Perpetration`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Clock {
    /// Ships today: transmission is pure structure and ignores world time.
    Off,
    /// A holder must not have ended before the event it holds.
    Alive,
}

impl Clock {
    /// Every arm, in a fixed order so a readout's columns are stable.
    pub const ALL: [Clock; 2] = [Clock::Off, Clock::Alive];

    /// This arm's short name, used as a readout column suffix.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn label(self) -> &'static str {
        match self {
            Clock::Off => "no-clock",
            Clock::Alive => "clock",
        }
    }
}

/// Whether `occ` may hold a claim about an event on `event_day`.
///
/// `Clock::Off` admits everything, reproducing today's behaviour exactly.
/// Under `Clock::Alive` an occupation with no `occ-ended` fact is still
/// standing and is always admitted; one that ended is admitted iff it ended
/// on or after the event day. **On or after, not strictly after**: the subject
/// of an ending ends on precisely that day and must remain a witness to it.
///
/// Total by construction — a non-`Number` `occ-ended` admits, the same posture
/// [`crate::amplitude::gen_span`] takes toward unreadable durations.
/// type-audit: bare-ok(count: event_day), bare-ok(flag: return)
pub fn admits(clock: Clock, ledger: &Ledger, occ: EntityId, event_day: f64) -> bool {
    match clock {
        Clock::Off => true,
        Clock::Alive => match ledger.value_of(occ, hornvale_history::OCC_ENDED) {
            Some(Value::Number(ended)) => *ended >= event_day,
            _ => true,
        },
    }
}
```

Add `pub mod clock;` to `windows/hearsay/src/lib.rs`, in alphabetical order
(after `accumulate`/`amplitude`, before `derive`).

- [ ] **Step 4: Run to verify it passes**

```
cargo test -p hornvale-hearsay --test clock 2>&1 | tail -10
```

Expected: `3 passed`.

- [ ] **Step 5: Prove the boundary test can fail**

**Demonstrate this property: `ending_on_the_event_day_is_admitted` fails if the
comparison is strict (`>`) rather than inclusive (`>=`).** That boundary is the
one thing in this module a reader could get wrong in either direction, and a
strict comparison would silently drop every ending's own subject from its own
witness set. Assert the text exists before replacing it, revert, and report the
output.

- [ ] **Step 6: Format, gate, commit**

```
cargo fmt
make gate-commit 2>&1 | tail -3
cargo test -p hornvale-hearsay --test clock 2>&1 | tail -5
git add windows/hearsay/src/clock.rs windows/hearsay/src/lib.rs windows/hearsay/tests/clock.rs
git commit -m "feat(hearsay): a clock, so the dead stop holding later news"
```

---

### Task 3: `ContactGraph` — the raid seam as an edge set

**Files:**
- Create: `windows/hearsay/src/contact.rs`
- Modify: `windows/hearsay/src/lib.rs` (add `pub mod contact;`)
- Test: `windows/hearsay/tests/contact.rs` (create)

**Interfaces:**
- Consumes: nothing from Tasks 1–2.
- Produces:
  - `pub enum Contact { Descent, WithRaidSeam }` — same derives, `ALL`, `label()` → `"descent"` / `"contact"`.
  - `pub struct ContactGraph` with `pub fn peers_of(&self, occ: EntityId) -> &[(EntityId, f64)]` (ascending) and `pub fn edges(&self) -> usize`.
  - `pub fn contact_of(ledger: &Ledger) -> ContactGraph`

- [ ] **Step 1: Write the failing test**

Create `windows/hearsay/tests/contact.rs`:

```rust
//! Spec §5.3: the raid seam, read out of `occ-ended-by` as an undirected
//! edge set stamped with each raid's day.

mod common;

use common::{eid, ledger_with, put};
use hornvale_hearsay::contact::contact_of;
use hornvale_kernel::ledger::Value;

fn raid(led: &mut hornvale_kernel::ledger::Ledger, victim: u64, attacker: u64, day: f64) {
    put(led, victim, hornvale_history::OCC_ENDED, Value::Number(day));
    put(
        led,
        victim,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(attacker)),
    );
}

#[test]
fn a_raid_makes_one_undirected_edge_stamped_with_its_day() {
    let mut led = ledger_with(&[(1, None), (5, None)]);
    raid(&mut led, 1, 5, 400.0);
    let g = contact_of(&led);

    assert_eq!(g.peers_of(eid(1)), &[(eid(5), 400.0)], "victim sees raider");
    assert_eq!(g.peers_of(eid(5)), &[(eid(1), 400.0)], "raider sees victim");
    assert_eq!(g.edges(), 1, "one raid, one edge");
}

#[test]
fn an_ending_with_no_named_attacker_makes_no_edge() {
    let mut led = ledger_with(&[(1, None)]);
    put(&mut led, 1, hornvale_history::OCC_ENDED, Value::Number(400.0));
    let g = contact_of(&led);
    assert_eq!(g.edges(), 0, "no occ-ended-by, no contact");
    assert!(g.peers_of(eid(1)).is_empty());
}

#[test]
fn an_occupation_in_several_raids_carries_several_peers_ascending() {
    let mut led = ledger_with(&[(1, None), (2, None), (5, None)]);
    raid(&mut led, 2, 5, 700.0);
    raid(&mut led, 1, 5, 400.0);
    let g = contact_of(&led);

    let peers = g.peers_of(eid(5));
    assert_eq!(peers.len(), 2, "the raider was in two raids");
    assert!(
        peers.windows(2).all(|w| w[0] <= w[1]),
        "peers must be ascending and therefore deterministic, got {peers:?}"
    );
    assert_eq!(g.edges(), 2);
}

/// A peers list that is not deduplicated would let one raid be walked twice
/// and inflate every reach number the readout reports.
#[test]
fn a_repeated_raid_pair_is_recorded_once_per_ending() {
    let mut led = ledger_with(&[(1, None), (2, None), (5, None)]);
    raid(&mut led, 1, 5, 400.0);
    raid(&mut led, 2, 5, 400.0);
    let g = contact_of(&led);
    let peers = g.peers_of(eid(5));
    assert_eq!(peers.len(), 2, "two distinct victims, two edges: {peers:?}");
}
```

- [ ] **Step 2: Run to verify it fails**

```
cargo test -p hornvale-hearsay --test contact 2>&1 | tail -20
```

Expected: compile failure, `hornvale_hearsay::contact` unresolved.

- [ ] **Step 3: Implement**

Create `windows/hearsay/src/contact.rs`:

```rust
//! The raid seam, as a horizontal edge in the transmission graph.
//!
//! Spec §5.3. Every model before this one walked parent->child down the
//! founding tree and nothing else, so 97.67% of accounts reached exactly one
//! people and none ever reached three. `occ-ended-by` records one event both
//! parties attended; this reads it as an undirected edge.
//!
//! **Undirected is a freeze, not a discovery** (spec §5.3). Asserting that
//! news flows only one way across a raid would be authoring, which decision
//! 0021 forbids for exactly this kind of asymmetry. It is also the ceiling, so
//! a directed variant (`KNOW-directed-contact`) is a restriction measurable
//! against this campaign's numbers.

use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::BTreeMap;

/// Whether transmission may leave the founding tree.
///
/// **Deliberately no `Default` impl** — see [`crate::stance::Perpetration`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Contact {
    /// Ships today: parent->child only.
    Descent,
    /// Descent plus an undirected edge between a victim and its named attacker.
    WithRaidSeam,
}

impl Contact {
    /// Every arm, in a fixed order so a readout's columns are stable.
    pub const ALL: [Contact; 2] = [Contact::Descent, Contact::WithRaidSeam];

    /// This arm's short name, used as a readout column suffix.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn label(self) -> &'static str {
        match self {
            Contact::Descent => "descent",
            Contact::WithRaidSeam => "contact",
        }
    }
}

/// Who met whom, and on what day.
#[derive(Clone, Debug, Default)]
pub struct ContactGraph {
    /// occupation -> (peer, day of the raid that put them in contact),
    /// ascending. Undirected: every raid writes both directions.
    peers: BTreeMap<EntityId, Vec<(EntityId, f64)>>,
}

impl ContactGraph {
    /// Everyone `occ` has been in contact with, ascending, each paired with
    /// the day of the raid that established it. Empty when `occ` never met
    /// anybody.
    /// type-audit: bare-ok(count: return)
    pub fn peers_of(&self, occ: EntityId) -> &[(EntityId, f64)] {
        self.peers.get(&occ).map_or(&[], Vec::as_slice)
    }

    /// How many undirected edges this graph holds — one per raid that named
    /// an `Entity`-valued attacker.
    /// type-audit: bare-ok(count: return)
    pub fn edges(&self) -> usize {
        self.peers.values().map(Vec::len).sum::<usize>() / 2
    }
}

/// Read the raid seam out of a ledger.
///
/// One undirected edge per ending carrying an `Entity`-valued `occ-ended-by`
/// and a `Number`-valued `occ-ended`. An ending missing either contributes
/// nothing: a raid with no day cannot be time-gated, and admitting it ungated
/// would smuggle spec §5.3's clock condition out through the back door.
pub fn contact_of(ledger: &Ledger) -> ContactGraph {
    let mut out = ContactGraph::default();
    for fact in ledger.find(hornvale_history::OCC_ENDED) {
        let victim = fact.subject;
        let Some(Value::Number(day)) = ledger.value_of(victim, hornvale_history::OCC_ENDED) else {
            continue;
        };
        let Some(Value::Entity(attacker)) =
            ledger.value_of(victim, hornvale_history::OCC_ENDED_BY)
        else {
            continue;
        };
        let (day, attacker) = (*day, *attacker);
        if attacker == victim {
            continue; // a community cannot meet itself
        }
        out.peers.entry(victim).or_default().push((attacker, day));
        out.peers.entry(attacker).or_default().push((victim, day));
    }
    // Ascending and deduplicated, so the walk is deterministic and one raid
    // is never traversed twice.
    for peers in out.peers.values_mut() {
        peers.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.total_cmp(&b.1)));
        peers.dedup();
    }
    out
}
```

Add `pub mod contact;` to `windows/hearsay/src/lib.rs` in alphabetical order.

- [ ] **Step 4: Run to verify it passes**

```
cargo test -p hornvale-hearsay --test contact 2>&1 | tail -10
```

Expected: `4 passed`.

- [ ] **Step 5: Check the graph against the real substrate**

The unit tests pin hand-built shapes. Confirm the reader agrees with the probe
that measured the substrate, on a real world:

```
cargo test -p hornvale-hearsay --test probe_contact_substrate the_raid_seam -- --ignored --nocapture --test-threads=1 2>&1 | grep 'named attacker'
```

| what you see | what it means | what to do |
|---|---|---|
| `2,804 (47.4%)` for 12 seeds | matches spec §3.1 | note it and go to Step 6 |
| a different number | main has moved since the spec froze | record BOTH in your report; do not edit the spec |

Then add a temporary `println!` of `contact_of(&world.ledger).edges()` for one
seed and check it is within a few of that seed's named-attacker count (they
differ only by endings missing a day or naming themselves). Remove the
`println!` before committing and report both numbers.

- [ ] **Step 6: Format, gate, commit**

```
cargo fmt
make gate-commit 2>&1 | tail -3
cargo test -p hornvale-hearsay --test contact 2>&1 | tail -5
git add windows/hearsay/src/contact.rs windows/hearsay/src/lib.rs windows/hearsay/tests/contact.rs
git commit -m "feat(hearsay): read the raid seam as an undirected contact graph"
```

---

### Task 4: `Transmission` and `Walk` — the policy, with `AS_SHIPPED` proven

**Files:**
- Create: `windows/hearsay/src/transmission.rs`
- Modify: `windows/hearsay/src/lib.rs`, `windows/hearsay/src/derive.rs`
- Modify: every call site (see Step 4)
- Test: `windows/hearsay/tests/transmission.rs` (create)

**Interfaces:**
- Consumes: `Perpetration` (Task 1), `Clock` (Task 2), `Contact`/`ContactGraph` (Task 3).
- Produces:
  - `pub struct Transmission { pub clock: Clock, pub perpetration: Perpetration, pub contact: Contact }` — `Clone, Copy, Debug, PartialEq, Eq`, with `pub const AS_SHIPPED: Transmission` and `pub fn label(self) -> String` joining the three arm labels with `/`.
  - `pub struct Walk<'a> { pub ledger: &'a Ledger, pub lineage: &'a Lineage, pub contact: &'a ContactGraph, pub policy: Transmission }`
  - `derive::variants_about(walk: &Walk, ladder: &PrecisionLadder, subject: EntityId, predicate: &str) -> Vec<Claim>`
  - `derive::variants_about_accumulating(walk: &Walk, ladders: &PeopleLadders, durations: &PeopleDurations, rule: Accumulation, subject: EntityId, predicate: &str) -> Vec<Claim>`

**Why a context struct rather than more arguments:** `variants_about_accumulating`
already takes 7. Adding a `ContactGraph` and a `Transmission` would make 9 and
trip clippy's `too_many_arguments` under `-D warnings`. `Walk` bundles exactly
the read-side context (the three things every walk needs regardless of arm).

**Blast radius, measured, so you are not surprised:** `variants_about` has 8
call sites, `variants_about_accumulating` 7, across
`tests/derive.rs`, `tests/palimpsest_readout.rs`,
`tests/palimpsest_readout_units.rs`, `tests/retelling_readout_seed42.rs`,
`tests/probe_teller_relations.rs`, `tests/probe_contact_substrate.rs`. **All are
asserting today's behaviour and all take `Transmission::AS_SHIPPED`.**

- [ ] **Step 1: Write the failing test**

Create `windows/hearsay/tests/transmission.rs`:

```rust
//! Spec §5: `Transmission::AS_SHIPPED` must reproduce today's behaviour
//! exactly. Every arm this campaign adds is measured as a difference from it,
//! so if this baseline is not exact, nothing downstream means anything.

mod common;

use common::{chain_with_foundings, eid};
use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::Accumulation;
use hornvale_hearsay::clock::Clock;
use hornvale_hearsay::contact::{Contact, contact_of};
use hornvale_hearsay::derive::variants_about_accumulating;
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::PeopleLadders;
use hornvale_hearsay::lineage::lineage_of;
use hornvale_hearsay::stance::Perpetration;
use hornvale_hearsay::transmission::{Transmission, Walk};

#[test]
fn as_shipped_is_exactly_todays_three_arms() {
    assert_eq!(Transmission::AS_SHIPPED.clock, Clock::Off);
    assert_eq!(
        Transmission::AS_SHIPPED.perpetration,
        Perpetration::Singleton
    );
    assert_eq!(Transmission::AS_SHIPPED.contact, Contact::Descent);
    assert_eq!(
        Transmission::AS_SHIPPED.label(),
        "no-clock/singleton/descent"
    );
}

/// The accumulating walk under `AS_SHIPPED` must return exactly what campaign
/// 3 committed for this fixture: same holders, same hops, same ordering.
#[test]
fn as_shipped_reproduces_the_committed_accumulating_derivation() {
    let led = chain_with_foundings();
    let lin = lineage_of(&led);
    let graph = contact_of(&led);
    let mut durations = PeopleDurations::default();
    durations.insert(
        "human",
        Some(StdDays::new(50.0).expect("positive")),
        Some(StdDays::new(150.0).expect("positive")),
    );
    let ladders = PeopleLadders::of(&led, &durations);
    let walk = Walk {
        ledger: &led,
        lineage: &lin,
        contact: &graph,
        policy: Transmission::AS_SHIPPED,
    };

    let held = variants_about_accumulating(
        &walk,
        &ladders,
        &durations,
        Accumulation::Additive,
        eid(1),
        hornvale_history::OCC_ENDED,
    );

    assert!(!held.is_empty(), "control: the fixture must hold claims");
    assert_eq!(held.len(), 6, "six occupations in the chain, six holders");
    assert!(
        held.windows(2).all(|w| w[0].holder <= w[1].holder),
        "results stay ascending by holder"
    );
    assert_eq!(held[0].hops, 0, "the witness holds at hop 0");
}
```

**If the `held.len()` or `hops` values here disagree with what the code
actually produces, the plan's numbers are wrong and the CODE is right** —
correct the test to the observed values, and say so in your report. These were
written from the fixture's doc comment, not from a run.

- [ ] **Step 2: Run to verify it fails**

```
cargo test -p hornvale-hearsay --test transmission 2>&1 | tail -20
```

Expected: compile failure, `hornvale_hearsay::transmission` unresolved.

- [ ] **Step 3: Implement the policy**

Create `windows/hearsay/src/transmission.rs`:

```rust
//! The three choices a transmission walk makes, as one value.
//!
//! Spec §5. The model is three separable layers — the graph's TOPOLOGY
//! ([`crate::contact::Contact`]), the node LABELLING the cost function reads
//! ([`crate::stance::Perpetration`]), and whether world time constrains either
//! ([`crate::clock::Clock`]). Campaigns 2 and 3 both varied edge COST and left
//! the other two untouched; this bundles all three so a readout can vary one
//! at a time.

use crate::clock::Clock;
use crate::contact::{Contact, ContactGraph};
use crate::lineage::Lineage;
use crate::stance::Perpetration;
use hornvale_kernel::ledger::Ledger;

/// One point in the space of transmission models.
///
/// **Deliberately no `Default` impl** — see [`crate::stance::Perpetration`].
/// [`Transmission::AS_SHIPPED`] is not a default: it is a named baseline, and
/// naming it is the point.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Transmission {
    /// Whether world time constrains who may hold a claim.
    pub clock: Clock,
    /// Whether being a perpetrator is inherited by descent.
    pub perpetration: Perpetration,
    /// Whether transmission may leave the founding tree.
    pub contact: Contact,
}

impl Transmission {
    /// Exactly what shipped before this campaign: no clock, a singleton
    /// perpetrator, descent-only transmission. Every arm is measured as a
    /// difference from this, so `tests/transmission.rs` pins that it
    /// reproduces the committed derivation.
    pub const AS_SHIPPED: Transmission = Transmission {
        clock: Clock::Off,
        perpetration: Perpetration::Singleton,
        contact: Contact::Descent,
    };

    /// This policy's readout column name, `clock/perpetration/contact`.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn label(self) -> String {
        format!(
            "{}/{}/{}",
            self.clock.label(),
            self.perpetration.label(),
            self.contact.label()
        )
    }
}

/// The read-side context every transmission walk needs, whatever its policy.
///
/// Bundled rather than passed as three more arguments because
/// [`crate::derive::variants_about_accumulating`] already takes seven and
/// clippy's `too_many_arguments` fires at eight under `-D warnings`.
pub struct Walk<'a> {
    /// The committed ledger. A window reads this and nothing else.
    pub ledger: &'a Ledger,
    /// The founding tree, from [`crate::lineage::lineage_of`].
    pub lineage: &'a Lineage,
    /// The raid seam, from [`crate::contact::contact_of`]. Built even under
    /// [`Contact::Descent`], where it is simply never consulted — building it
    /// is a cheap ledger scan and a conditional would only add a branch.
    pub contact: &'a ContactGraph,
    /// Which transmission model to walk.
    pub policy: Transmission,
}
```

Add `pub mod transmission;` to `lib.rs` in alphabetical order.

- [ ] **Step 4: Re-signature both walks and fix every call site**

In `derive.rs`, change `variants_about` and `variants_about_accumulating` to
take `walk: &Walk` in place of their `ledger`/`lineage` parameters, reading
`walk.ledger`, `walk.lineage` internally. **Behaviour must not change yet** —
this step is purely mechanical, and Tasks 5 and 6 are where the arms start
doing anything. `variants_about` passes `walk.policy.perpetration` to
`stance::is_lossy`.

```
cargo build -p hornvale-hearsay --tests 2>&1 | tail -20
cargo test -p hornvale-hearsay 2>&1 | tail -20
```

| what you see | what it means | what to do |
|---|---|---|
| all green | the re-signature is behaviour-preserving | Step 5 |
| a test fails on VALUES (not types) | the re-signature changed behaviour | find it; do not adjust the expected value |
| clippy `too_many_arguments` | a walk still takes too many | move the parameter into `Walk` |

- [ ] **Step 5: Run to verify the new test passes**

```
cargo test -p hornvale-hearsay --test transmission 2>&1 | tail -10
cargo clippy -p hornvale-hearsay --all-targets 2>&1 | tail -5
```

Expected: `2 passed`, clippy clean.

- [ ] **Step 6: Prove `AS_SHIPPED` is load-bearing**

**Demonstrate this property: changing any one field of `AS_SHIPPED` makes
`as_shipped_is_exactly_todays_three_arms` fail.** That constant is the baseline
every downstream number is a difference from; if it can drift silently, the
whole readout is unanchored. Report which field you changed and the output.

- [ ] **Step 7: Format, gate, commit**

```
cargo fmt
make gate-commit 2>&1 | tail -3
cargo test -p hornvale-hearsay 2>&1 | tail -5
git add windows/hearsay/src windows/hearsay/tests
git commit -m "refactor(hearsay): a Transmission policy, with AS_SHIPPED pinned"
```

---

### Task 5: The augmented walk — cycles, a clock, and a round trip

**Files:**
- Modify: `windows/hearsay/src/derive.rs`
- Test: `windows/hearsay/tests/augmented_walk.rs` (create)
- Modify: `windows/hearsay/tests/common/mod.rs` (add two fixtures)

**Interfaces:**
- Consumes: everything from Tasks 1–4.
- Produces: no new public names. `variants_about_accumulating` now honours
  `walk.policy.clock` and `walk.policy.contact`.

**The correctness risk, stated plainly (spec §5.5).** A contact edge makes the
graph **cyclic**. Today `variants_about_accumulating` recovers *the* path by
slicing `ancestry(d)[..=pos]`, unique only because the founding tree is
single-parent. That slice cannot survive a horizontal edge. Replace it with a
**best-first relaxation** over the augmented graph, keyed by the ordering the
function already documents: `(width.to_bits(), hops, witness)`. Termination
rests on `Accumulation::step` being non-decreasing for non-negative spans,
which `tests/accumulate.rs::every_rule_is_non_decreasing` pins — a node is only
re-expanded when reached at a strictly smaller width, and width cannot
decrease, so the frontier drains.

- [ ] **Step 1: Add the fixtures**

Append to `windows/hearsay/tests/common/mod.rs`:

```rust
/// Two peoples joined only by a raid, built so a claim can make a ROUND TRIP:
/// out of the victim's lineage, across the seam, and into a line no tree route
/// connects.
///
/// - 1 (human) founded day 0, ended day 1000, raided by 5.
/// - 2 (human) founded day 500, a child of 1 -- the victim's own line.
/// - 5 (kobold) founded day 100, the raider.
/// - 6 (kobold) founded day 1500, a child of 5.
pub fn two_peoples_joined_by_a_raid() -> Ledger {
    let mut led = ledger_with(&[(1, None), (2, Some(1)), (5, None), (6, Some(5))]);
    for (occ, day) in [(1, 0.0), (2, 500.0), (5, 100.0), (6, 1500.0)] {
        put(&mut led, occ, hornvale_history::OCC_FOUNDED, Value::Number(day));
    }
    for (occ, people) in [(1, "human"), (2, "human"), (5, "kobold"), (6, "kobold")] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_PEOPLE,
            Value::Text(people.to_string()),
        );
    }
    put(&mut led, 1, hornvale_history::OCC_ENDED, Value::Number(1000.0));
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(5)),
    );
    put_on(&mut led, 9, hornvale_astronomy::facts::DAY_LENGTH_STD, Value::Number(1.0));
    put_on(&mut led, 9, hornvale_astronomy::facts::MOON_PERIOD_STD, Value::Number(41.7));
    put_on(&mut led, 9, hornvale_astronomy::facts::YEAR_LENGTH_STD, Value::Number(372.4));
    led
}

/// A dead community: 3 is founded from 1 on day 10 and ends on day 20, long
/// before 1's own ending on day 1000. Under `Clock::Off` it inherits a claim
/// about an event 980 days after it ceased to exist; under `Clock::Alive` it
/// does not. Spec §3.5 measured 1,959 such holders on the real substrate.
pub fn a_holder_that_died_before_the_event() -> Ledger {
    let mut led = ledger_with(&[(1, None), (3, Some(1)), (4, Some(3))]);
    for (occ, day) in [(1, 0.0), (3, 10.0), (4, 15.0)] {
        put(&mut led, occ, hornvale_history::OCC_FOUNDED, Value::Number(day));
    }
    for occ in [1u64, 3, 4] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_PEOPLE,
            Value::Text("human".to_string()),
        );
    }
    put(&mut led, 3, hornvale_history::OCC_ENDED, Value::Number(20.0));
    put(&mut led, 1, hornvale_history::OCC_ENDED, Value::Number(1000.0));
    put_on(&mut led, 9, hornvale_astronomy::facts::DAY_LENGTH_STD, Value::Number(1.0));
    put_on(&mut led, 9, hornvale_astronomy::facts::MOON_PERIOD_STD, Value::Number(41.7));
    put_on(&mut led, 9, hornvale_astronomy::facts::YEAR_LENGTH_STD, Value::Number(372.4));
    led
}
```

- [ ] **Step 2: Write the failing tests**

Create `windows/hearsay/tests/augmented_walk.rs`:

```rust
//! Spec §5.1, §5.3, §5.5: the clock removes holders, the contact edge adds
//! them, and a claim reaches a people no tree route connects.

mod common;

use common::{a_holder_that_died_before_the_event, eid, two_peoples_joined_by_a_raid};
use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::Accumulation;
use hornvale_hearsay::clock::Clock;
use hornvale_hearsay::contact::{Contact, contact_of};
use hornvale_hearsay::derive::variants_about_accumulating;
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::PeopleLadders;
use hornvale_hearsay::lineage::lineage_of;
use hornvale_hearsay::transmission::{Transmission, Walk};
use hornvale_kernel::ledger::{EntityId, Ledger};
use std::collections::BTreeSet;

fn durations_for(peoples: &[&str]) -> PeopleDurations {
    let mut d = PeopleDurations::default();
    for p in peoples {
        d.insert(
            p,
            Some(StdDays::new(50.0).expect("positive")),
            Some(StdDays::new(150.0).expect("positive")),
        );
    }
    d
}

fn holders(led: &Ledger, policy: Transmission, subject: EntityId) -> BTreeSet<EntityId> {
    let lin = lineage_of(led);
    let graph = contact_of(led);
    let durations = durations_for(&["human", "kobold"]);
    let ladders = PeopleLadders::of(led, &durations);
    let walk = Walk {
        ledger: led,
        lineage: &lin,
        contact: &graph,
        policy,
    };
    variants_about_accumulating(
        &walk,
        &ladders,
        &durations,
        Accumulation::Additive,
        subject,
        hornvale_history::OCC_ENDED,
    )
    .into_iter()
    .map(|c| c.holder)
    .collect()
}

fn with(policy: Transmission, f: impl FnOnce(&mut Transmission)) -> Transmission {
    let mut p = policy;
    f(&mut p);
    p
}

#[test]
fn the_clock_removes_a_holder_that_died_before_the_event() {
    let led = a_holder_that_died_before_the_event();
    let off = holders(&led, Transmission::AS_SHIPPED, eid(1));
    let on = holders(
        &led,
        with(Transmission::AS_SHIPPED, |p| p.clock = Clock::Alive),
        eid(1),
    );

    assert!(
        off.contains(&eid(3)),
        "control: today's model DOES give the dead community the claim -- \
         if this fails the fixture is wrong, not the clock"
    );
    assert!(!on.contains(&eid(3)), "the clock refuses it");
    assert!(
        on.is_subset(&off),
        "the clock is strictly removing: {on:?} must be a subset of {off:?}"
    );
}

#[test]
fn the_contact_edge_carries_a_claim_across_a_people_boundary() {
    let led = two_peoples_joined_by_a_raid();
    let descent = holders(&led, Transmission::AS_SHIPPED, eid(1));
    let contact = holders(
        &led,
        with(Transmission::AS_SHIPPED, |p| {
            p.contact = Contact::WithRaidSeam
        }),
        eid(1),
    );

    assert!(
        descent.is_subset(&contact),
        "contact is strictly adding: {descent:?} must be a subset of {contact:?}"
    );
    assert!(
        contact.len() > descent.len(),
        "the raid seam must reach somebody new; descent {} vs contact {}",
        descent.len(),
        contact.len()
    );
}

/// Spec §5.5. 6 descends from 5, and 5 descends from nobody, so no founding
/// route connects 6 to 1. Only the seam can carry the account there.
#[test]
fn a_claim_reaches_a_people_that_no_tree_route_connects() {
    let led = two_peoples_joined_by_a_raid();
    let contact = holders(
        &led,
        with(Transmission::AS_SHIPPED, |p| {
            p.contact = Contact::WithRaidSeam
        }),
        eid(1),
    );
    assert!(
        contact.contains(&eid(6)),
        "the raider's child holds the victim's account: {contact:?}"
    );
    assert!(contact.contains(&eid(2)), "the victim's line still holds it");
}

/// The walk must TERMINATE on a cyclic graph. `1 <-> 5` plus descent gives a
/// cycle the moment contact is on; a walk that re-expands a node at an equal
/// width would not drain. Spec §5.5's termination argument rests on
/// `Accumulation::step` being non-decreasing, pinned by
/// `tests/accumulate.rs::every_rule_is_non_decreasing`.
#[test]
fn the_augmented_walk_terminates_on_a_cycle() {
    let led = two_peoples_joined_by_a_raid();
    for rule in Accumulation::ALL {
        let lin = lineage_of(&led);
        let graph = contact_of(&led);
        let durations = durations_for(&["human", "kobold"]);
        let ladders = PeopleLadders::of(&led, &durations);
        let walk = Walk {
            ledger: &led,
            lineage: &lin,
            contact: &graph,
            policy: with(Transmission::AS_SHIPPED, |p| {
                p.contact = Contact::WithRaidSeam
            }),
        };
        let held = variants_about_accumulating(
            &walk,
            &ladders,
            &durations,
            rule,
            eid(1),
            hornvale_history::OCC_ENDED,
        );
        assert!(!held.is_empty(), "{rule:?}: the walk produced nothing");
        assert!(
            held.windows(2).all(|w| w[0].holder <= w[1].holder),
            "{rule:?}: results must stay ascending by holder"
        );
    }
}
```

- [ ] **Step 3: Run to verify they fail**

```
cargo test -p hornvale-hearsay --test augmented_walk 2>&1 | tail -30
```

Expected: the `control:` assertion PASSES (it describes today's behaviour) and
the arm assertions FAIL. **If a `control:` assertion fails, stop** — the fixture
does not have the shape the plan claims, and every later number would be
measured against a broken baseline. Report it rather than adjusting the arm
assertions.

- [ ] **Step 4: Implement the augmented walk**

Rewrite `variants_about_accumulating`'s body as a best-first relaxation. The
shape, with the parts you must not change called out:

- Seed `best` from `witnesses_of` at `((0, 0, w), claim)` **as today**, and keep
  the rule that a witness is never demoted to an inheritor.
- Apply `clock::admits(walk.policy.clock, walk.ledger, holder, event_day)` when
  admitting ANY holder, witnesses included. Read `event_day` from the subject's
  own `predicate` value when it is `Value::Number`; when it is not, the clock
  cannot apply and admits everything.
- Expand a node's neighbours as: `walk.lineage.children_of(node)`, plus — only
  under `Contact::WithRaidSeam` — `walk.contact.peers_of(node)` filtered to
  peers whose contact day is `>= event_day` (spec §5.3 condition 1: a meeting
  cannot carry news of something that has not happened).
- Relax with the existing key `(width.to_bits(), hops, witness)`; re-expand a
  node only when the new key is **strictly** smaller than the stored one.
- Keep the per-path ladder fixed to the **originating witness's** people
  (spec §5.4) — do not re-read it at each step, even though a cross-people path
  now makes that a real choice. Changing it is `KNOW-teller-ladder-at-emit`,
  carried forward.
- Keep `gen_span(teller, hearer)` and the accumulation rule exactly as they are
  (spec §2 — the unit erratum stays frozen).

Also thread the clock into `variants_about` the same way, so both walks honour
`walk.policy`.

- [ ] **Step 5: Run to verify they pass**

```
cargo test -p hornvale-hearsay 2>&1 | tail -20
```

| what you see | what it means | what to do |
|---|---|---|
| all green including the whole crate | the rewrite preserved `AS_SHIPPED` | Step 6 |
| `tests/transmission.rs` fails | the rewrite changed the baseline | fix the walk; that test is the anchor |
| `augmented_walk` hangs | the cycle is not draining | you are re-expanding at an EQUAL key; require strictly smaller |
| a campaign 2/3 test moves | `AS_SHIPPED` is no longer shipped behaviour | fix the walk, never the old expectation |

- [ ] **Step 6: Prove the contact-day filter is load-bearing**

**Demonstrate this property: whether removing the contact-day condition
(`>= event_day`) makes any test fail.** If nothing fails, say so plainly and add
a test that pins it, because an unpinned filter is exactly the seam this project
calls a survivor. If something does fail, report which. Either result is
publishable; a silently unguarded filter is not.

- [ ] **Step 7: Format, gate, commit**

```
cargo fmt
make gate-commit 2>&1 | tail -3
cargo test -p hornvale-hearsay 2>&1 | tail -5
git add windows/hearsay/src/derive.rs windows/hearsay/tests
git commit -m "feat(hearsay): a claim can leave its lineage and arrive damaged"
```

---

### Task 6: The preregistered readout

**Files:**
- Create: `windows/hearsay/tests/parley_readout.rs`

**Interfaces:**
- Consumes: everything from Tasks 1–5.
- Produces: nothing public. A heavy battery.

Model it on `windows/hearsay/tests/palimpsest_readout.rs` — same 40-seed panel
(census seeds 0–39), same `read_world` shape, same `#[ignore = "heavy: …"]`
reason string, same `claim:` tag convention. **Read that file first**; do not
invent a second harness.

- [ ] **Step 1: Write the battery**

Report, per arm, over the panel:

| § | quantity | how |
|---|---|---|
| §6.2 H1 | holders removed by the clock | holders under `AS_SHIPPED` vs `clock = Alive`, as a share |
| §6.3 H2 | median retained precision rung, raider-people vs victim-people holders | per `Perpetration` arm, via `variants_about` (the accumulating walk does not read stance) |
| §6.4 H3 | share of endings whose account reaches 3+ peoples | per `Contact` arm |
| §6.4 | **seed 2 specifically**: cross-people accounts under each `Contact` arm | its own printed line |
| §6.5 H4 | mutually-exclusive cross-people day sets | each side holds a day the other holds nowhere |
| §6.6 | saturated fraction, descent vs contact, per `Accumulation` rule | the null detector |

**Assert only substrate controls** (spec §6.7): the panel built; held claims
exist; no claim reports a rung its own ladder lacks; the `descent` arm
reproduces §3.3's shape (2+ peoples on ~2.3% of endings, **never 3**); the
clock never adds a holder; contact never removes one. **Every hypothesis is
REPORTED, never asserted.** Print each against its §6 decision table so a
reader sees confirmed/falsified without re-deriving it.

- [ ] **Step 2: Run a 3-seed pilot and measure the cost**

Temporarily set the panel to `[0, 1, 2]`:

```
time cargo test -p hornvale-hearsay --test parley_readout -- --ignored --nocapture --test-threads=1 2>&1 | tail -40
```

| measured per-seed cost | what to do |
|---|---|
| ≤ 5 s/seed | keep 40 seeds; record the pilot cost in the battery's own output |
| > 5 s/seed | report the number and the projected 40-seed cost; propose a panel size; do not silently shrink it |

Restore the 40-seed panel before Step 3.

- [ ] **Step 3: Run the full readout and capture it**

Never inline `| tail` on an expensive run — capture, then grep freely:

```
cargo test -p hornvale-hearsay --test parley_readout -- --ignored --nocapture --test-threads=1 > /tmp/parley-readout.txt 2>&1
echo "rc=$?"
tail -5 /tmp/parley-readout.txt
```

- [ ] **Step 4: Report each hypothesis against its decision table**

For each of H1–H4 and the §6.6 null, state confirmed / falsified / null **with
the number**. A falsified prediction is a finding. **Do not edit `derive.rs`,
`accumulate.rs` or the spec to rescue one** — if you are tempted, that is the
finding, and it goes in the report.

- [ ] **Step 5: Format, gate, commit**

```
cargo fmt
make gate-commit 2>&1 | tail -3
git add windows/hearsay/tests/parley_readout.rs
git commit -m "test(hearsay): The Parley's preregistered readout"
```

---

### Task 7: Close the campaign

**Files:**
- Create: `book/src/chronicle/the-parley.md`
- Create: `docs/retrospectives/the-parley.md`
- Modify: `docs/retrospectives/README.md`, `book/src/SUMMARY.md`,
  `book/src/frontier/idea-registry.md`
- Possibly modify: `book/src/open-questions.md`

- [ ] **Step 1: Chronicle**

Write `book/src/chronicle/the-parley.md` at the project's deliberate altitude —
technical, comprehensible without the code. It must carry the readout's real
numbers **including anything falsified**, and must state §3.6's asymmetry and
the §3.4 correction (19, not 111). Add its `SUMMARY.md` entry.

**Book titles are code-generated in places** — after editing run the
`docs_consistency` and book tests and fix what reddens.

- [ ] **Step 2: Retrospective**

`docs/retrospectives/the-parley.md` — process, not product. It must carry:
- **The freeze leaked** (followup F2): the freeze named spec sections, the
  payload also sat in an unmarked chronicle table, and it reached the
  controller via a heading grep run *before* reading. Future freezes put frozen
  content in a separate file that is not opened.
- **The controller's spec stated a wrong mechanism** (`Bystander` forced;
  576 of 3,694 land on `VictimLine`), caught by running, not reading.
- **A weak instrument overstated its data 5.8×** (111 → 19).
- Followup F1: the readout could be a committed artifact; authoring path
  unverified.

Add the one-line entry to `docs/retrospectives/README.md`.

- [ ] **Step 3: Registry**

Flip `KNOW-perpetration-inheritance` and `KNOW-contact-is-an-edge` from
`spec'd` to `shipped`, repointing **Where** at the chronicle. **Repointing
REPLACES a row's prose — never append.** Amend `KNOW-mismatch-needs-contact`
with the corrected 19-of-138 baseline. Leave `KNOW-directed-contact` and
`KNOW-teller-ladder-at-emit` `raw`.

If the readout moved a Confidence Gradient bet, re-score that chapter
(decision 0030).

- [ ] **Step 4: Regenerate artifacts and check drift**

```
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

| what moved | what it means | what to do |
|---|---|---|
| `docs/audits/` only | the type-audit report drifted on new `pub` items | commit it in the same commit |
| `docs/digest/` too | a decision or registry row moved | expected; commit it |
| `book/src/gallery/` or `book/src/domesday/` | a rendered world or census changed | **STOP** — this campaign touches no domain and no census; investigate before committing |
| nothing at all | suspicious, given new `pub` items | verify `make rebaseline` actually ran; an empty diff needs a positive control |

- [ ] **Step 5: Gate, commit, and hand back**

```
cargo fmt
make gate-commit 2>&1 | tail -3
git add -A
git commit -m "docs(parley): chronicle, retrospective and registry for The Parley"
```

Then **stop and report**. The merge is a G6 hard stop: the controller presents
the ledger digest to Nathan and runs `closing-a-campaign`. Do not run
`make sluice` yourself.

---

## Notes for whoever executes this

- **`make gate-commit` is an allow-list, not the workspace.** Every new test
  file in this plan is invisible to it until a green stage gate records a
  duration. Always run your new test by name as well.
- **Absorb main at each task boundary** with
  `make sluice-stage BRANCH=campaign/the-parley REF=$(git rev-parse HEAD)`.
  It never pushes. A conflict is refused at the mouth in milliseconds.
- **Never absorb mid-measurement.** Once Task 6 Step 3 is running, finish the
  readout before absorbing — a preregistered study's baseline and readout must
  see the same physics.
- **`.superpowers/sdd/` is git-ignored and per-worktree.** Promote findings to
  the retrospective before teardown or they are gone.
