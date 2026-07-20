# The Slumber — implementation plan

Spec: `docs/superpowers/specs/2026-07-20-the-slumber-design.md` (G3 approved).
Direct TDD in `windows/vessel/src/liveness.rs`; tune constants empirically via
the harness (as #5 did).

## Stage 1: The fatigue drive
**Goal:** `is_awake(activity, day)`; a `Fatigue` stock drive (urgency = time
since last `rested`, ceiling below survival) whose affordance is `Rest` at home
(else plan home); new `Action::Rest`, `DriveKind::Fatigue`, `Perceived.fatigue`,
`rested` session predicate. Gated by metabolism (Ametabolic → no fatigue, via the
existing empty-drive-set gate).
**Success:** a tired metabolizer plans home and rests; an ametabolic creature
gets no fatigue drive; `Rest` resets fatigue.
**Tests:** `is_awake` per cycle; fatigue rises since last rest and resets on Rest;
the fatigue affordance is Rest-at-home / step-home; ametabolic → no fatigue.
**Status:** IN PROGRESS — fatigue drive wired end-to-end (`Fatigue` drive,
`Action::Rest`, `DriveKind::Fatigue`, `Perceived.fatigue`, `RESTED` predicate +
registration, `fatigue_at`/`is_awake`, tick Rest handling, health by-cause,
felt_phrase weary wording). 52 liveness + 86/87 vessel tests green. One felt-
state test (`needs_reports…content…across the drive cycle`) needs re-pinning:
fatigue raises baseline arousal (0.42 by day 0.5) past `RESTLESS_AROUSAL`, so a
mildly-weary NPC reads "grows restless" earlier — a real interaction to settle
alongside Stage 2's wake-gate and empirical rhythm tuning.

## Stage 2: The wake-gate
**Goal:** thirst & thermal are active only while awake; a survival override keeps
critical thirst active; fatigue is not gated. `arbitrate` gains `awake: bool`.
The tick commits `rested`; the rhythm emerges (forage by day, sleep at home by
night).
**Success:** a sleeping creature reads Idle/Content (not distress); a dying
creature wakes to drink; a multi-day sim shows a day/night rhythm.
**Tests:** wake-gate suppresses thirst/thermal while asleep; survival override;
3-drive arbitration; harness rhythm scenario.
**Status:** MECHANISM DONE (2a) — `Drive::seek_while_asleep`/`survival_override`,
`arbitrate.awake` gate, fatigue retuned to sleep-debt (0.3), wake-gate unit test,
felt-state re-pinned. 56 liveness lib green; clippy + type-audit clean.
REMAINING (2b): wire real solar `is_awake` (Terrain solar hook →
`calendar.is_daylight`/`daylight_fraction_at` + `RoomAddr::coord().latitude`,
`None`→locked→fatigue-only) into affect_of/tick; that bounds fatigue (nightly
sleep) and resolves the 3 red health-calibration tests (fatigue-live-without-gate
distresses real NPCs); then re-pin the harness + thirst-walk tests to the rhythm.

## Stage 2b: wire the live solar wake-gate (bounds fatigue; fixes the reds)
**Status:** Not Started

## Stage 3: Calibration, gallery rebaseline, close
**Goal:** health null-control holds (sleep ≠ distress); behavior tests updated;
the possession-over-time gallery regenerated + reviewed; full gate; DoD.
**Success:** `make gate` green; null-control 0; gallery drift reviewed and
accepted; chronicle/retro/registry.
**Tests:** null-control; harness (rhythm; far-from-home-at-dusk driven home);
possession-over-time regenerated.
**Status:** Not Started
