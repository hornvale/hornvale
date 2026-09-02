# The Roll — retrospective

Process, not product. The chronicle carries what was built.

## Five prescribed mutations proved nothing, and the world is why

The plan's house form writes each test's mutation into the brief: *this is the
change the assertion must fail against*. Five of them, across three tasks,
turned out to establish nothing when run, and the reasons are worth separating
because only one of them is the familiar "written from outside the code".

- **Task 5, the shared filter.** The brief said flip `!predominantly_marine`.
  Once the filter is genuinely shared, both the producer and the test's
  validator call the same mutated function and move together — the check is
  tautological. The implementer rebuilt the test around an independent
  re-statement of the predicate and re-ran the same mutation, which then
  reddened. **Instance three in this project's memory of the
  never-prescribe-a-mutation-from-outside-the-code rule.**
- **Task 7, dormancy, twice.** The brief said feed the unfiltered roster into
  the drive system, which should make a frozen body walk. At seed 42 it did
  not: the flagship condenses onto fresh water, so its residents drink where
  they stand and commit no position **whether they are ticked or not**.
  "Frozen" and "ticked" are observationally identical there. A second attempt
  at seed 2 was also null — those residents walk once, reach water, and are
  dormant on the second wait regardless. The test now runs at a seed whose
  residents never reach water and therefore wander every tick.
- **Task 7, the roster order.** The brief said sort `self.bodies` by
  `on_roll`. At seed 42 every derived body is on the roll, so the mask is
  all-true and a stable sort on a constant key permutes nothing; the test
  would have passed against a `refresh_roll` that really did reorder.
  `reverse()` is the same defect in a form the world can exhibit.
- **Task 9, the presence line.** The planned assertion was
  `others + N_NAMED == sensed`. Widening the take by one leaves that identity
  intact, because the `others` count re-reads the same unwidened constant for
  its own subtraction. The separator count caught it instead.

**The generalisable half is not "the author had not read the code" — it is
that a mutation needs a world that can exhibit the difference.** Three of the
five were sound descriptions of a defect, aimed at a seed incapable of showing
it. A brief can name the mutation; only a run can tell you the fixture has an
opinion. Each null is now recorded in the test's own doc comment beside the
mutation that replaced it, which is the only place a later reader will look.

## Five enforcement surfaces no brief named

Every one was found by an implementer hitting it, none by the plan:

1. **`claim_shape`** (decision 0093) — an `#[ignore]`ed seed-looping test in
   `windows/vessel` now needs a `claim:` tag, and a `for s in …` loop trips
   the seed-shaped-binding heuristic as a documented false positive.
2. **`heavy_tier`'s frozen untokenised-ignore roster** — a new `#[ignore]`
   reason is a set membership, reviewed like any other. Task 14's own M5 probe
   reuses the M1 probe's reason string verbatim for exactly this reason.
3. **The stream-stamp roster** in `cli/src/streams.rs` — a new stream label
   touches **three** places, not two: the crate's `streams.rs`, the generated
   manifest, and this frozen roster.
4. **`lexicon_guard`** — every mention of the frozen predicate value `cell-id`
   needs a same-line `lexicon:` waiver, because the guard counts bare `cell`
   tokens.
5. **`layering-generated.md`** — a new crate dependency edge (`hornvale-person`
   into the vessel) moves a committed golden.

None was worked around; all five were repaired properly. The note for the next
plan is that the enforcement surface of this repository is wider than any
brief has ever enumerated, and the cheapest fix is not a longer brief but the
habit these implementers already had: repair the ratchet, then say in the
report which one fired.

## The pre-roll baseline was never taken quiet, and the A/B is honest anyway

Every perf reading in this campaign is labelled **CONTENDED**. The Repose's
rule asks for all three load averages under 4; the box did not offer that in
thirty-three minutes of foreground polling, across three separate attempts on
two different days. The readings taken are 4.07/4.96/5.77 at the closest
approach.

What saves the conclusion is that the comparison is **contended against
contended** — 157.2 → 83.5 ms per wait for the same instrument at the same
roster on the same box — and that the verdict clears its budget by an order of
magnitude (72.0 ms against 1000). Contention inflates both sides and cannot
manufacture a 1.88× or close a 14× margin. What is *not* available is a clean
absolute number for the pre-roll world, and the ledger says so rather than
quoting the contended figure as a baseline.

## Inserting above a function detached its doc comment. Again.

A 27-line block explaining why `other_bodies` is a free function, and the
handle-renumbering history behind it, was silently adopted by a new function
inserted beneath it. This is The Tableau's headline process lesson, recorded
one campaign ago, reproduced here in the same codebase within days. The lesson
did not transfer because it is not the kind of thing a person remembers at the
moment of typing; the fix is structural or it is nothing.

## The apostrophe trap landed in history

Commit `82ee9f305`'s message carries a leaked heredoc `EOF)`. The trap is
documented — `git commit -m "$(cat <<'EOF')"` breaks on apostrophes, and a
quoted delimiter does not protect it — and it landed anyway. It was left in
place rather than rewriting a pushed commit. **Task 14 wrote every one of its
own commit messages to a file with an editor tool and used `git commit -F`**,
which is the only form that has never done this.

## A review package came out at 857 KB

The repository's own review-package script emitted 857 KB for Task 8 — far
past what a reviewer can be handed usefully — and had to be trimmed by path.
The same trimming was needed at Task 9. The script has no size discipline and
nothing warns; the cost of missing it is a review that reads a diff of
generated fixtures instead of the code.

## The ledger wins, for names and for births

The sharpest single ruling of the campaign, and it came from a reviewer rather
than the design. A resident's birth day was computed as *now minus a drawn
age*. Derive the same settlement twice on two different days — which is what
re-possessing a saved world a week later does — and the second derivation
commits a *different* value against a **functional** predicate, which is a
contradiction and a panic.

The alternative on offer was a caller contract ("always derive at the same
`now`"), which is a rule with no enforcement pointed at a call the CLI already
makes. The ruling instead made the ledger authoritative for a birth exactly as
it already was for a name: a resident's birth is fixed at its first
derivation. **The general form: a functional predicate plus a re-derivation at
a later day is a contradiction waiting for a calendar**, and the fix is always
to read what is committed rather than to constrain who may call.

## Follow-ups

Each with its reason, because a follow-up without one is a wish.

1. **The key and the transfer verb** (spec §7; decisions 0398, 0516). The
   resident now exists and can hold a key — custody takes any entity — but no
   verb transfers a held thing between creatures and no drive sets one down.
   Moving the key onto a person today would make the only lock in the game
   unopenable, which is the defect The Chattel closed from the other side. The
   transfer verb is the prerequisite, not the key's placement.
2. **The slow tier** (decision 0549; `SOC-off-roll-slow-tier`, raw). The
   unnamed middle between a frozen body and a ticked one. Deferred because it
   is a second visible mechanism carrying its own purity obligation under
   0546, and it is not needed to answer the brief. Build it when a returning
   player's frozen village is the finding.
3. **The lab health battery still walks twice per tick.**
   `windows/lab/src/health.rs:122-150` calls `step_with_occupancy` and then
   `tick()` on the same frozen ledger — the exact shape Task 11 removed from
   `Session::wait`, still in place, untouched here because it is an
   instrument's cost and not the walk's. Its own comment documents the double
   read as deliberate; whether it can take Task 11's treatment is unexamined.
4. **Four unimplemented perf levers**, retired by the plan's own preregistered
   rule once M2 was met four levers early. Their shares, as far as the two
   existing instruments can attribute them:
   - the O(A²) `shared_believed_water` fold and the per-decision
     `Body`/`HazardMemory` clones — **small at A ≤ 200 and not separable from
     the table alone**: `agent_scaling`'s fitted log-log slope is 1.10 and the
     per-segment slopes (1.23, 0.72, 1.23) show none of the curvature a
     dominant quadratic term would produce. Bounding it tighter needs an
     isolated timing of the fold, which does not exist.
   - the cold memos in `snapshot`/`needs` — **unmeasured**; no instrument in
     either example isolates them, and `agent_scaling` may not exercise those
     call sites at all since it drives `step_with_occupancy` directly rather
     than `Session::wait`.
   - `agent_position` fanned out four times over the roster per tick —
     **unmeasured**; bundled with every other fixed per-tick cost in the
     `bytes/agent` trend and not isolable from that table.
   The honest statement is two measured-as-small and two unmeasured, not four
   small.
5. **Log bounding at the new roster.** The Penstock's stage 7, out of scope
   here and measured rather than fixed, as spec §9 said it would be: at seed
   42's roll the wait commits **62.25 facts per wait at 68 bodies**, against an
   unbounded session ledger. The commit-rate battery's ceiling and non-growth
   assertions both hold with room; what the campaign did not do is bound the
   log.
6. **A settlement wider than one room** (`SOC-settlement-wider-than-one-room`,
   new, raw). A chamber seats one body per anchor kind, so 67 residents in one
   room are mostly present, examinable and undrawn. Ruled deliberately: the
   alternative ("placed or lit" sensed) would hide bodies that are in the room,
   which The Sighting rejected for the same reason. The presence line names the
   remainder in words instead. The real fix is the room count.
7. **`body_at` re-resolves `species_of` per resident** — an inherited
   signature, now called `population` times per settlement instead of once.
   Never measured; it is the cheapest of the perf follow-ups to check and
   nobody checked it.

## Every deferred minor, and where it landed

Minors were deferred to the final review by eleven of the fourteen tasks.
**None was fixed in Task 14**, and saying so plainly is the point of the list:
they were re-read, classified, and accepted or promoted, not quietly dropped.

One accounting note, because a count that does not reconcile is worth a
sentence rather than a silent rounding: the campaign's tallies sum to 27
(Task 7 contributing "8 from review 1 + 4 from re-review"), while the
enumerated items sum to 28 — Task 7's re-review names **five**, not four. The
table below groups by kind and carries all of them; the discrepancy is in the
tally, not in the list.

| task | minor | outcome |
|---|---|---|
| 1 | `company_at` returns `Option<(bool,bool)>`, positionally destructured at two call sites (brief-mandated) | accepted — two call sites, both in one file |
| 2 | `wild_agents: true` duplicates `PossessOpts::default()` (brief-mandated literal) | accepted — harmless explicitness |
| 3 | `age_days` tagged `bare-ok(count)` where a continuous days quantity elsewhere takes `diagnostic-value` | accepted — a type-audit class question, not a behaviour one |
| 3 | psyche/dispersion registries fetched fresh rather than through `wc` | accepted — the same caveat `disposition.rs` documents about itself |
| 4 | `body_at` re-resolves `species_of` per resident | **promoted** — follow-up 7 above |
| 4 | eager clone of an existing name | accepted |
| 5 | the `headcount == 0` skip is dead under today's invariant | accepted — defensive, and the invariant is not stated in a type |
| 5 | label-decode and biosphere setup written three times | accepted — test setup |
| 6 | no empty-slice or headcount-1 test | accepted — both are covered incidentally by the identity tests |
| 7 | two BFS traversals per tick | **promoted** — folded into follow-up 4's untouched levers |
| 7 | a redundant bare block; label-vs-entity in `the_roster_never_reorders`; `the_roll_orders_home_first` uses the fallback key; a tautological closing assert in `possession_moves` | accepted — test hygiene, none affecting what is asserted |
| 7 | `i as u16` silent truncation on a settlement of >65,535 | accepted — no world produces one; the census maximum population is 42.1 mean, 18,757 world-total |
| 7 | fail-open vs truncating mask readers | accepted — fail-open is the safer half here |
| 7 | `why`'s day-0 narration probe; the twin predicate's old form unrecoverable | accepted |
| 7 | `roll_len == population + 1` is seed-geometry dependent | accepted — stated in the test's own doc |
| 7 | `RollKey.parent` tag class; a `String` clone per body per tick | **partly promoted** — the clone is a perf item under follow-up 4 |
| 7 | `session.rs` grew ~330 lines; the roll's session half is a module candidate | accepted, and worth doing before the next campaign touches this file |
| 8 | the mutation paragraphs' embedded panic line numbers predate the doc comment's own growth | accepted — a stale line number in prose, not a claim |
| 9 | `presence_line` does five things in one body; irregular whitespace in a panic string | accepted |
| 10 | `body_by_needle`'s doc omits the tie-break rule (last of equal-length ties, roster order) | accepted — and it is exactly the kind of omission that produced the bug it fixed |
| 10 | commit `82ee9f305`'s message carries a leaked heredoc `EOF)` | accepted — a pushed commit is not rewritten; recorded above as its own lesson |
| 13 | a tautological `// Task 13` comment | accepted |
