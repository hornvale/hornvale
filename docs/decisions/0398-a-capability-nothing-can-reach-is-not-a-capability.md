# 0398. A capability nothing can reach is not a capability — the strongbox loses its population gate

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (ruled directly on
Task 11's reported measurement; a fidelity call, so not an autopilot gate) ·
**Amends:** [The Blocking](../retrospectives/the-blocking.md)'s choice of
`Pattern::needs_populous` for `the-strongbox` — amends its *application*, not
its mechanism · **Relates:**
[0396](0396-a-passage-is-a-thing-and-openness-is-its-fold.md),
[0397](0397-the-knowledge-gate-denies-a-passage.md) (the two records this
campaign minted before it),
[0353](0353-a-regression-test-is-specified-by-the-mutation-it-must-fail.md)
(how the relaxation is evidenced),
[0016](0016-studies-preregister-hypotheses.md) (why the *number* is the
deliverable) ·
[The Chattel](../superpowers/plans/2026-08-28-the-chattel.md)

In the context of The Chattel having shipped a container, a lock, a key and
an `open`/`close` verb pair that **no session in any world could stand in
front of**, we decided to **drop `needs_populous` from `the-strongbox` and
from `the-key-in-the-strongbox`**, accepting that a strongbox now stands in
an ordinary hamlet's storeroom and that `Pattern::needs_populous` becomes a
mechanism no authored pattern uses.

## Context

The Blocking gave `the-strongbox` a scale gate and wrote down why: *"a hamlet
has nothing worth locking up."* The Chattel's Task 11 mirrored it onto the key
it authored, with a reason of its own — *"a reader who later relaxes the
strongbox's own scale gate should have to see this one too."* Both are good
sentences. Both are about a world that does not exist.

Task 11 measured the world that does. `Brief::is_populous` reads
`peak_population > hornvale_history::flesh::HAMLET_POPULATION_CEILING`, which
is 150:

```text
seed 42: ceiling=150 occupations=1240 alive=389 populous_alive=0 max_alive_peak=84
seed 13: ceiling=150 occupations=1094 alive=305 populous_alive=0 max_alive_peak=87
seed  1: ceiling=150 occupations=1163 alive=310 populous_alive=0 max_alive_peak=85
```

Not one living occupation in three whole worlds clears the ceiling — the
observed maxima are 84–87 against 150, a factor of nearly two short — and a
48-seed sweep of the flagship settlement a possession actually starts at found
`populous = true` **zero times**. The gate was not making the strongbox rare.
It was making it impossible.

**The measurement was reported rather than acted on, and that was correct.**
Whether a world should have towns of 150 is a fidelity question about
demography, not a question an implementer settles mid-task. It was ruled at
the top: relax the gate.

## The rule

**1. A gate whose predicate is false everywhere is not a gate, it is a
deletion — and the two must be told apart before either is shipped.** The
distinction this record turns on is between a gate that is *correct as a
semantic claim* and one that is *correct as a reachability claim*.
`needs_populous` on a strongbox is a defensible semantic claim: a family
holding does not keep a banded chest. It was a false reachability claim, and
nothing in the code could tell the difference — `selection_for(Role::Store,
true, false, true)` returns the strongbox in a unit test, green, forever,
while no `Brief` in any world ever supplies that `true`. **Unit tests over a
selector cannot see this. Only a walk can**, which is why the relaxation ships
with one (clause 3).

**2. Both patterns come off together, and the mirror is what made that
safe.** Task 11's stated reason for putting `needs_populous: true` on the key
was that a future reader relaxing the strongbox should have to see the key
too. That reader arrived, and the mechanism worked exactly as designed:
relaxing `the-strongbox` alone would have left the key gated behind a flag
nothing else set, and the key would have been silently dropped from every
strongbox that composed — an `open` reporting an empty chest, which is the
precise failure spec §3.8 was written to prevent. The key stays confined by
`requires: Some(AnchorKind::Strongbox)`, which is the honest gate: a key is
inside a strongbox or it is nowhere.

**3. The relaxation is a MEASUREMENT, not an assertion, and it is taken with
the player's own instrument.** The same 48-seed sweep, re-run through
`Session::handle` — the commands a player types, not the selector a test can
call:

```text
before: seeds=48 got_indoors=48 with_strongbox=0 with_key=0
after : seeds=48 got_indoors=48 with_strongbox=8 with_key=8
```

`got_indoors=48` is the anti-vacuity half: the zero was a zero about
strongboxes, not about doors. Every hit is the same shape — the deepest
chamber of a four-chamber structure, reading `["a doorway", "a water jar", "a
strongbox", "a key"]`, with `open a strongbox` answering *"It is locked, and
you are carrying nothing that would open it."* So the joined refusal
`LOCKED_WITHOUT_A_KEY_REFUSAL`, which Task 11 recorded as unreachable through
`Session::handle`, is a played refusal now.

**The sweep is not kept as a test and that is a priced decision.** It builds
48 worlds (~195 s), and the only tier that would run it is the heavy one,
which [0426](0426-the-heavy-tier-is-a-phase-of-the-queue-again.md) has just
put back on every merge after The Governor cut it 3.52x; a 195 s addition
there is a ~44% regression in a number a whole campaign was spent on, bought
for a rate this record already carries with its date. What a permanent test
must hold is that the count is **not zero**, and one seed holds that:
`windows/vessel/tests/suite/strongbox_reachability.rs::a_possession_walks_to_a_strongbox_and_finds_it_locked`
walks seed 1 in and asserts the strongbox, the key inside it, and the locked
reply, for ~3.9 s. The mutation it must fail against is 0398 itself put back:
restore `needs_populous: true` on `the-strongbox` and it reddens on the first
assertion.

**4. `needs_populous` stays as a mechanism, and stays EXERCISED.** No
`INVENTORY` pattern sets it now, so `draw`'s `needs_populous && !populous` arm
is unreachable from production. Deleting the arm would delete a real grammar
capability — social scale is a legitimate axis for a pattern language and the
next pattern may want it. Leaving it unexercised would be worse than either:
the field keeps reading as live, and the first author to write
`needs_populous: true` inherits a filter nothing has run since the day it went
idle. So `draw` splits into a public-facing wrapper and a private
`draw_from(inventory, …)` seam, and
`the_populous_gate_still_works_though_no_authored_pattern_uses_it` feeds a
**synthetic** two-pattern inventory through it, asserting both directions.
Proving the mechanism must not cost a pattern in the world. That test also
asserts its own premise — that `INVENTORY` sets the flag nowhere — so an
author who does set it gets a red pointing at the one place the state of this
mechanism is written down.

## Consequences

- **`Brief::is_populous` has no vessel-side consumer that SELECTS on it, and
  its doc said the opposite.** It read "the vessel's use is
  `Pattern::needs_populous`: the strongbox". It is **wired and idle** now:
  `chamber_interior_of` still passes it into `pattern::selection_for` on every
  chamber derivation, and no authored pattern reads the result. The wiring is
  what lets a future population-gated pattern work on the day it is written,
  so it stays; the doc is corrected in place, and `brief.rs`'s module header —
  which counted "FOUR fields are read" — now counts three and says where the
  fourth went.

- **The grown-lattice relaxation ceiling moves 5 → 6, and the scan did not get
  worse.** `anchor_cells.rs`'s `GROWN_RELAXATIONS` is a measured ceiling whose
  own doc says raising it means the placement scan regressed. That reading is
  right in general and wrong here, so the record is explicit: **the corpus got
  harder.** Every built `Store` chamber gains two anchors, and that corpus
  derives real interiors through `chamber_interior_of`, so its Store fixtures
  went from three anchors to five with no line of `anchor_cells.rs` changing.
  A/B on one tree, flipping only the two `needs_populous` literals:

  ```text
  gate ON : unfaithful=5 surplus=3
    [(3,55,1,14,5), (4,9,1,4,5), (4,22,2,4,3), (4,25,1,7,5), (4,34,2,5,3)]
  gate OFF: unfaithful=6 surplus=4
    [(3,55,1,14,5), (4,9,1,4,5), (4,10,2,5,5), (4,22,2,4,5), (4,25,1,7,5), (4,34,2,5,5)]
  ```

  The last field is the anchor count. Two already-unfaithful rows merely carry
  five anchors instead of three; **exactly one case is new**, `(4,10,2,5,5)` —
  a five-cell blob a three-anchor interior fits and a five-anchor one does
  not. Nothing that was unfaithful became faithful, which is the direction
  that would have signalled a scan change.

- **No committed artifact moved, and that is a finding rather than a relief.**
  `make rebaseline` produced an empty diff across every path
  `docs/generated-paths.txt` declares — including `book/src/gallery/`, which
  was expected to move. Measured why: seed 42's flagship structure draws
  **three** chambers, and its index-2 role is `Loomroom` (the occupation is
  Agrarian), so the gallery's own world has no `Store` chamber to put a
  strongbox in — `enter` then three `enter further in`s reads *"A small room,
  holding a doorway, a water jar and a loom"* and then *"This is as far in as
  the place goes."* The positive control that this empty diff means something
  is the 8-of-48 sweep above — the capability is reachable, so an unmoved
  artifact is a fact about that artifact's world rather than about the
  relaxation.

  **The sentence that used to end this bullet is false, and it is the one a
  future reader leans on.** It read: *"the generator would render a strongbox
  if the world it renders had one."* It would not. `scripts/possession-walk.txt`
  — the input the gallery transcript is generated from — contains exactly
  **one** `enter further in`, so the walk never stands deeper than chamber
  index 1, and `interior::pattern::role_for` puts **every** `Role::Store` at
  index 2 or deeper. Give seed 42 a `Store` at index 2 and the transcript
  would still show no strongbox, because the walk stops one room short of it.
  So there are **two independent reasons the gallery could not move** and only
  one of them was written down; the recorded one (seed 42's index-2 role is
  `Loomroom`) is true, and stating it alone implied that fixing the world
  would move the artifact. Moving the artifact needs a longer walk as well.

- **The room's prose names what the closed chest conceals, and this record is
  where that is written down rather than left for a player to find.**
  `chamber_nouns` lists every anchor in the interior regardless of
  containment, so seed 1's storeroom reads *"A small room, holding a doorway,
  a water jar, a strongbox and a key"* while `open a strongbox` answers *"It
  is locked."* This is not new behaviour — a hearth within an alcove has been
  named the same way since The Blocking — but it was **unobservable** until
  now, because `(Strongbox, Key)` is the first `within` pair whose container
  carries `Openable`. `examine_detail` already gates contents on openness; the
  room description does not.

  **The gap is WIDER than "the prose names it", and the wider half is the one
  worth knowing.** `open_or_close` and `examine_chamber` both resolve a typed
  noun over the whole of `interior.ids()`, contained anchors included, so the
  concealed key is not merely *named* — it is directly **addressable** by the
  same surface that concealed it. Measured on seed 1's storeroom, 2026-08-29,
  through `Session::handle` with the chest shut:

  ```text
  > examine a key  ->  A short shank of worked iron, its ward cut in a single stepped notch.
  > open a key     ->  The key does not open.
  ```

  **The deferral is REPRICED, because it was priced against a fix nobody would
  write.** This bullet used to say the fix "would drop the hearth out of every
  hearthroom's prose, move the gallery, and move a committed transcript". That
  is true of a naive `within.is_some()` filter and false of the rule this same
  campaign already shipped one function away: `examine_detail`'s two arms hide
  contents iff `Encloses && Openable && !opened`. Under that rule the alcove is
  untouched (it carries `Encloses` and no `Openable`, so the hearth stays), and
  `strongbox` is the **only** kind in `affordance::object_registry` carrying
  both — so the change can reach no prose that does not name a strongbox. No
  committed artifact names one: not `book/src/gallery/possession-seed-42.md`
  (see the bullet above — the walk stops at chamber index 1, and every `Store`
  is at index 2 or deeper) and not
  `clients/game/core/tests/fixtures/session-seed-42-chamber.json` (chamber
  index 0, *"A small room, holding a doorway and a screen."*).

  Measured rather than argued, 2026-08-29: the two-arm rule was applied to
  `chamber_nouns` on a scratch tree and the gallery regenerated through
  `regenerate-artifacts.sh`'s own recipe — **byte-identical**, `diff` silent.
  The positive control that the empty diff means something is a deep walk on
  two other seeds, where the rule IS live and does exactly what it should:

  ```text
  seed 13 clean : A small room, holding a doorway, an alcove, a hearth and a bed.
  seed 13 ruled : A small room, holding a doorway, an alcove, a hearth and a bed.
  seed 13 clean : A small room, holding a doorway, a water jar, a strongbox and a key.
  seed 13 ruled : A small room, holding a doorway, a water jar and a strongbox.
  ```

  The hearth-in-an-alcove line is untouched and the key-in-a-strongbox line
  loses the key — which is the whole of the old bullet's first objection,
  falsified on the real subject. (Seed 1 reproduces both rows identically.)

  **What the deferral actually costs, then.** `describe_chamber(interior,
  brief)` is a pure function of the interior — it holds no ledger and no day,
  which is what an openness read needs — and it is `pub`, re-exported from
  `hornvale_vessel`, with an out-of-module caller in
  `windows/vessel/tests/suite/the_blocking.rs`. Threading state into it changes
  a published signature; keeping the signature means the openness filter moves
  up into `Session` and `chamber_nouns` stops being the one catalogue
  `describe_chamber` renders from, which is a property its own doc pins. That
  is the real bill, it is a scoped change with its own reasoning, and it is
  still not a rider on this one — so it stays deferred, now **registered where
  a followup survives a worktree**:
  [`PLAY-closed-container-conceals-nothing`](../../book/src/frontier/idea-registry.md).
  This record previously said it was "registered as a followup" while
  registering it nowhere tracked, which made the claim self-referential.

- **Two doc comments that asserted unreachability are corrected loudly, in
  place.** `session.rs`'s `a_lockable_thing_opens_only_with_the_key_in_custody`
  opened with "No `Session` in any world can stand in front of a strongbox
  today, and that is a MEASUREMENT rather than an assumption", and its sibling
  `a_container_opens_closes_and_re_opens` reasoned from the same
  premise. Both tests are unchanged and both keep their reason: a custody FOLD
  and a four-state openness ladder are still things a played walk cannot
  reach, because the verbs that put a key in the body's hands are Task 12's.
  Only the premise moved, and a premise that outlives its subject produces
  wrong answers from readers acting in good faith.
