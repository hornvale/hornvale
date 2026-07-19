# The Diachronic Book (C8) — Design

**Date:** 2026-07-19
**Status:** **COMPLETE — shipped (see [the chronicle](../../../book/src/chronicle/the-diachronic-book.md)); the program's committed sequence C1–C8 closes with this campaign.** Was: Approved at G3 (2026-07-19) — floor/ceiling split confirmed; zero-blast-radius posture confirmed; ledger digest due at G6.
**Campaign:** C8 of the self-writing-book program — the final committed
campaign (metaplan: [program metaplan](2026-07-17-the-self-writing-book-program-metaplan-design.md) §3 C8)
**Theory:** registry row LANG-42 (the diachronic Book), floor slice: the
time axis via observation accumulation. The Kuhnian cycle, knowledge
decay, and schism are the row's ceiling and stay deferred with pointers —
every campaign in this program shipped its row's floor first, and the
ceiling mechanisms (memory dynamics, paradigm state, faction forks) do
not exist in the world model yet.

---

## 1. What this is

The Book gains a time axis. Knowledge at time T is what a culture could
have witnessed and kept by T: the sky's dated events (the closed-form
eclipse ledger the astronomy domain already computes) accumulate, and
each institution climbs MAP-18's knowledge ladder as its records grow —
**unknown → counted → numbered → predictive**. Each volume gains one
additive section, **The Reckoning of Years**, rendering a fixed epoch
pair — the world's first day, and its hundredth year:

> *(day 0)* The sky keeps no dates yet.
> *(the hundredth year)* The sky has darkened, now and again. The
> priesthood numbers the darkenings: ⟨n⟩. The next, it teaches, comes on
> day ⟨D⟩.
>
> *In truth, the darkenings of the first hundred years number ⟨true n⟩.*

And the taught prediction is **checkable — and true**: the model is
exact, so doctrine's predictive authority is real. The Galileo cell that
C6 measured structurally empty fills diachronically: an unverifiable
claim becomes verifiable-and-vindicated as T passes the predicted day.
This is the Babylonian diaries rendered — priesthood records begetting
the saros begetting prediction-as-authority — and it is derived from
machinery the program already ships, end to end.

## 2. What exists (verified, main @52f380d)

- `hornvale_astronomy::eclipse_events(system, calendar, from, until) ->
  Vec<EclipseEvent { day, moon, body, .. }>` — the dated, closed-form,
  deterministic event ledger (the cadence metrics already scan a
  century); the Eclipse Seasons recurrence ladder classifies events.
- `windows/historiography::recount` — the UNI-27 window this campaign is
  structurally continuous with (LANG-42's own framing).
- C4–C7: capability (sky ≥ 0.6 gate), doctrine voices (records vs folk
  memory), the conflict-state vocabulary, the corpus law, the Book's
  section machinery.
- `WorldTime { day: f64 }`; no wall clock anywhere.

## 3. Architecture

### 3.1 The observation ledger at T (pure; `windows/worldgen`)

`observations_of(world, species, at: StdDays) -> Observations` — derived,
per culture: the eclipse events in `[0, T]` that the culture witnessed —
**solar events are public** (a darkened day-sky needs no night eyes);
**lunar events are night-gated** (folk sky-capability ≥ 0.6, the C4
threshold reused). The institution's count (records) is the witnessed
count; the folk hold qualitative recency only (no records — the
counted-qualitative rung, timeless phrasing).

### 3.2 The ladder (preregistered thresholds)

Per (culture-institution, at T): `Unknown` (0 witnessed) →
`Counted` (folk, any n ≥ 1: "the sky has darkened, now and again") →
`Numbered` (organized cult only, n ≥ K_COUNT = 3: the count as a
RevealedClaim-style knowledge line) → `Predictive` (organized cult,
n ≥ K_PREDICT = 8 events of one recurrence class: the next event of the
most-observed class, dated). Folk-only cultures never pass `Counted`
(no records — the SOC-1 gate's diachronic consequence).

### 3.3 The prophecy law

Every `Predictive` line's day D is verified against the closed-form
future event set: the predicted event EXISTS at D (within the recurrence
tolerance the Eclipse Seasons machinery defines — verify at plan time).
Doctrine's predictions are never wrong, and the law asserts it — the
authority-accrual mechanic, testable.

### 3.4 The surface (`windows/book`, `cli`)

- Each volume gains **The Reckoning of Years** (additive; every C4–C7
  section byte-identical): the fixed epoch pair {day 0, day 36525},
  rendering per epoch the lines each culture's ladder position earns,
  plus the truth margin (the true count) wherever a culture's knowledge
  falls short. Eventless worlds render the honest empty arm ("the sky
  keeps no dates to number") — asserted, never vacuous.
- `hornvale book --at <day>`: renders the volumes with the diachronic
  section evaluated at the given day (the committed artifact uses the
  fixed pair; `--at` is the explorer's lens, never committed). Absent
  `--at`, output is today's PLUS the new committed section — the only
  delta is additive.
- New Common constructions (the darkening line, the numbering line, the
  prediction line, the empty line) are corpus-law bidirectional.
  Tongues do not enter this section at the floor.

## 4. The laws (standing tests)

1. **The accumulation law:** knowledge at T is monotone in T at the
   floor (more time, never less knowledge), and `observations_of` at
   T=0 is empty for every culture.
2. **The ladder law:** ladder positions match the preregistered
   thresholds exactly, per measured seed, at both epochs; folk-only
   cultures never exceed `Counted`.
3. **The prophecy law (§3.3):** every rendered prediction's day is in
   the future event set — asserted for every `Predictive` culture at the
   measured seeds; if NO culture reaches `Predictive` by the hundredth
   year anywhere in seeds 1..=5, the test PANICS demanding a wider epoch
   or seed sweep (the Book must show the ladder's top somewhere).
4. **The additivity law:** every pre-C8 section of the committed
   artifact is byte-identical; the diachronic section is the only
   addition.
5. **The corpus law:** every new Common sentence round-trips.
6. **The witness law:** solar counts never depend on sky-capability;
   lunar counts do (both directions asserted, synthetic where the roster
   lacks a case).
7. **Census stability:** zero new metrics; the chorus fixture and every
   census artifact byte-identical (verified at the surface task).

## 5. Determinism and blast radius

- **Zero new draws, zero new streams, zero new facts/concepts. No
  epoch.** Everything derives from committed astronomy + the shipped
  stacks; thresholds are authored constants. Genesis byte-identical.
- Artifact: additive only (law 4). Census untouched (law 7). No AWS.
- `--at` is a render parameter, never persisted (the almanac's
  `--world`-file precedent).

## 6. Non-goals (the ceiling, deferred with pointers)

- Knowledge decay / lost arts / margins reopening (needs memory
  dynamics); the Kuhnian anomaly→revolution cycle (needs paradigm
  state); schism forking the chorus (needs factions). All stay on
  LANG-42 with this spec named as the floor.
- Diachronic re-grounding of the chorus accounts (the C4–C7 sections
  stay static at the floor).
- Tongue-rendered diachronic lines; vessel integration (`book --at` at
  the possessed session's day — captured as the game-seam followup).

## 7. Registry and decisions

- LANG-42 → `spec'd` at spec commit; → `shipped (floor: the time axis —
  observation accumulation, the ladder, the prophecy law; decay/Kuhn/
  schism remain the ceiling)` at close.
- The program metaplan's §3 completes at close: C1–C8 all shipped —
  the close's chronicle marks the program's committed sequence done and
  points at the north star (UNI-29) as the next horizon.

## 8. Flagged for G3

1. **Determinism (leads, and it is clean):** zero draws, zero facts,
   zero census impact, additive artifact — the quietest blast radius in
   the program.
2. **The floor/ceiling split:** accumulation ships; decay, Kuhn, and
   schism stay deferred on LANG-42's row. This is the one campaign whose
   row does NOT fully close — confirm the partial flip is acceptable as
   the program's final committed campaign (the alternative is a much
   larger campaign inventing memory/paradigm/faction dynamics).
3. **Preregistered thresholds:** K_COUNT=3, K_PREDICT=8 same-class
   events, epoch pair {0, 36525}. The prophecy-law panic demands the
   ladder's top be visible somewhere in seeds 1..=5.
4. **Surface taste:** the section title ("The Reckoning of Years"), the
   four line shapes, the epoch labels.
