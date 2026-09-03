# The Detent — retrospective

Process, not product. The product is in
[the chronicle](../../book/src/chronicle/the-detent.md); **the rulings — all
eight, with their alternatives and their costs — are in
[the campaign ledger](../superpowers/ledgers/2026-09-02-the-detent.md)**, which
is where a controller's decisions belong and where this document deliberately
does not repeat them; the measurements are in the spec's §11 and §12.

## A mechanism was read from code, and a sixty-second count refuted it

The campaign was chosen by a count that took under a minute to run and
overturned a committed record.

The previous campaign closed with one fold failing three criteria and left a
registry row naming why: per tick, for every visited room and every emitter,
the emitter's affect at that room's latest-visit day is re-evaluated. Its
chronicle called that mechanism "legible from the code rather than merely
suspected." Nathan's own ranking put that mechanism's memo first among the
candidates for this campaign.

Before choosing, the fold was **counted** on the instrument the criterion is
measured on — a terrain wrapper counting every question the fold asks, the
store's own witness read either side of each probe. **22,302 static terrain
samples per call at tick 60; zero affect replays, at every depth, on two
seeds.** The proposed memo would have moved the criterion by nothing. The cost
was static terrain re-sampled per tick, which is a different quarry entirely.

The rule that came out of it is now
[decision 0628](../decisions/0628-a-registry-rows-mechanism-is-a-count-not-a-reading.md):
**before a spec names a mechanism for a measured cost, count it on the shape the
criterion is measured on, and give the count a denominator.** The zero here was
only worth anything because it carried its denominators — 22,302 samples taken,
roster and probe room counts non-zero, and a second seed where every scan
*does* find an emitter and the replay count is still zero. A bare zero and an
unwired instrument produce the same output.

What makes this worth a decision rather than a note is that it is the **second
consecutive campaign** whose named mechanism was wrong until counted, and both
were written by people who had just spent a campaign inside the code. That is a
statement about reading code as evidence for a committed artifact, not about
either author. The row was corrected in place with its original claim kept as
history, because a rejected mechanism is part of what the next reader needs.

## A witness written by the campaign that named this defect still shipped it

Task 4's job was to prove the room memo works. Its assertions were the right
ones — a repeated read takes zero field samples, the whole tick takes zero —
and **every one of them was satisfiable by a memo nobody had wired in.** Drop
the threading and the deltas read zero and the test stays green.

This is the exact defect shape the campaign's own opening finding is about, on
the campaign's own witness, one stage later. It was not caught by the
implementer and not by the controller; it was caught by the reviewer asking
*what if the plumbing were absent*. The repair is the same one that keeps
turning up: absolute floors beside the zero deltas (11,149 misses > 0,
1,934,552 hits > 0, misses equal to the memo's length) plus a printed shape
line, so a memo that is not there cannot pass.

Naming a defect class in your own spec does not inoculate your own artifacts
against it. The question that found it is cheap, mechanical, and applies to
every witness: **construct the world in which the thing under test is absent,
and check the assertion fails.**

## A verification clause named the wrong denominator, and an implementer's stop overturned it

One post-unblinding change was permitted, conditional on verifying the
mechanism by measurement first. The controller wrote the verification clause:
the latest-visit map had to be **at least 50% of the fresh read's cost**.

The measurement came back at 24%. **The implementer stopped and reported rather
than proceeding**, which is exactly right — the clause was written to be
binding.

The clause was wrong, and the stop is what exposed it. The criterion in
question is about a **slope**; the clause tested a **level**. The map is 24% of
a total that the fit had already shown to be intercept-dominated, and it is
**72–76% of the history-driven growth** — the fitted quantity the criterion
actually measures. Correcting *which quantity a verification compares* is not
the same act as lowering a threshold to fit a result, and the difference was
made checkable rather than rhetorical: the comparison under **both** readings
was recorded before the clause was changed, the ruling was written before the
change was made, and no threshold, constant or criterion moved.

Two process points, both worth more than the fix:

- **"Name the denominator" is already in this project's memory, and the
  controller wrote the defect anyway.** Knowing a failure mode is not the same
  as recognising it in a sentence you are drafting.
- **The overturn came from below.** The pass that caught the controller's error
  was an implementer's tension report, and the ledger records it as that rather
  than dressing it up as a controller's insight. An agent that refuses to
  proceed on a clause it was given is functioning correctly; the value of that
  refusal is entirely lost if the response is to wave it through.

## A registry row's remedy could not work, and only writing the code found out

The row for moving the emitter-scan tests out of the fear-path file specified
its own remedy: make the type crate-visible and move the tests to the
integration suite beside their siblings. **That cannot work.** An
integration-test binary is a separate crate and cannot see crate-private items.
The row was written by someone who had the file open, and it is wrong in a way
that only compiling it reveals.

The intent — tests beside their siblings, the file shorter — was met by a
different mechanism: a test-only module declared with a path attribute, so the
tests leave the 19,809-line file (to 19,577) while staying in-crate and nothing
widens. The row is closed with that correction in its own **Where** cell, so
the next reader gets the mechanism and not just the outcome.

A row's *target* survives being wrong about its *mechanism*, in both the cases
this campaign touched. That is the pattern: **take a row's quarry, re-derive its
means.**

## The plan text is where this campaign's defects originated

Five defects in controller-authored plan and brief text were caught and
corrected by implementers, and one was not caught until the readout.

1. **"Leave the `latest` block where it is."** The plan's own Task 6 text, to
   preserve a witness's ordering. It left an O(distinct-rooms) map above an
   early return the design had specified as prefix-bounded, on a probe for
   which distinct rooms *are* history — so the implementation was falsified
   against its own design, in text the controller wrote. **This one was not
   caught by an implementer**; it cost the campaign a whole readout and the one
   post-unblinding change.
2. **"Six `with_fields` sites."** There are five. The implementer ran the grep
   and reported the count rather than trusting the brief, and correctly left a
   sixth candidate (`LocaleTerrain::new`, a different constructor with no
   fields) alone.
3. **"Two `LocaleTerrain` construction sites."** There are three — the brief's
   note explicitly said TWO. The compiler found the third (`with_calendar`)
   with a missing-field error. A struct literal gets its enumeration for free,
   which is why this one could only ever be a slowdown and not a bug.
4. **`health.rs`'s `run_simulation`.** The brief named the wrong twin:
   `run_simulation` takes a caller's terrain and builds none, and
   `run_simulation_with_locale` is the function that owns a locale context and
   rebuilds terrain per tick. The implementer resolved it by reading the file.
5. **`Facet::new(face, &[])`.** The brief's test text used a constructor that
   does not exist; the brief itself said to check first, and the implementer did
   (it is a plain struct with public fields).
6. **A `tests/suite/common` directory in `windows/vessel`.** Named in a spec
   draft and caught at the spec's own self-review; the crate's shared helpers do
   not live in one.

Two more were authored for the close task itself and are recorded here for the
same reason:

7. **"Place the chronicle directly above The Pawl's line (newest first)."** The
   chronicle list in the book's summary is **oldest-first and appended**; the
   commit that placed The Reservoir's line appended it at the end. The brief's
   own escape hatch — *check how The Reservoir's line was placed and follow it*
   — is what resolved it, and is the shape a brief should take whenever it
   asserts a convention.
8. **A board post routed at `tools/placement-audit/`.** The blind spot being
   reported is The Plumb's constant lint, a different tool; the post went to
   `tools/plumb/`.
9. **"Still no cost gate," for the Confidence Gradient re-score.** There is one
   now — The Rack's per-turn counted budget, recorded in the section directly
   above where the fifth look would have gone. The re-score says the true and
   more useful thing instead: the gate exists, it is a **count** over one
   window's turn path, and it is structurally blind to every read this campaign
   made a thousand times cheaper.
10. **"Insert the fifth look directly after the fourth look."** The Rack's
    section sits between them and opens "The section above, written days
    earlier, states…" — inserting there would have broken a live back-reference.
    Appended at the end of the chapter instead.

The generalisable half: **every one of these is a claim about the tree stated in
prose, and every one that was caught was caught by running a command against
the tree.** The one that was not caught (1) is the one that was not a checkable
claim about the tree at all — it was an instruction, and an instruction cannot
be grepped.

## What the merged tree did that nobody predicted

Three campaigns landed during these nine tasks, and the absorptions produced
three findings the gates did not.

- **A clean automatic merge duplicated a registry row.** Both campaigns had
  edited the **Where** cell of the same row; git merged both edits and produced
  two rows with one ID. The document-consistency check caught it. This lesson
  is already in this project's memory and it landed again — a clean auto-merge
  is not evidence a hand-edited table merged correctly, and the aggregate-
  regeneration habit does not fire for a table nobody generates.
- **The Plumb's constant lint refused two test constants**, because it walks
  files and cannot see a test-only attribute carried on a module declared with
  a path attribute in another file. They now carry declarations whose reason
  text is honest and whose declared axis misstates what they are; the roster's
  denominator moved 686 → 688 with test-only exclusions unchanged. The fix is
  the tool's, and it is filed as such (and posted to the board, since it will
  bite the next campaign that moves tests this way).
- **Moving two tests changed the commit gate.** The sub-floor roster selects by
  exact name, and the two moved tests were listed by their old path, so the
  commit gate ran 1,096 instead of 1,098 until the next green chamber run
  rewrote the roster. This was **recorded when it happened rather than
  discovered later**, which is the only difference between a known consequence
  and a mystery.

The two absorptions also worked exactly as designed, and that is worth stating
because it is the boring outcome: both re-recorded every identity witness
main-first, on a checkout carrying none of this campaign's code, before the
merge. The Rack moved the seed-42 walk, so the merged tree was required to
reproduce a number this campaign did not produce — and did.

## The controller slip

**A ledger entry was committed into the worktree while an absorption
implementer was mid-task in the same tree.** The merge had already
auto-committed, so nothing was clobbered, and the outcome was harmless. The
rule broken is nonetheless exactly the one in this project's memory: *no
controller commits while a subagent works in the same tree* — because the
controller's commit takes the whole index, including whatever half-written file
the subagent has on disk at that instant.

It is recorded because the harmlessness was luck, not care: had the merge not
already committed, the ledger commit would have swept the implementer's
in-progress resolution of a hand-resolved conflict in the session file into a
commit describing a ledger entry.

## Deferred minors, and where each landed

Every minor and info note the campaign deferred, with its outcome.

**Task 1.** No per-field docs on the counting structs (a test crate; not
gate-enforced) — **accepted**. `#[allow(dead_code)]` scoped to the whole bench
struct rather than its two not-yet-read fields — **accepted**; the two fields
did acquire readers in later tasks, which is what the scope was betting on.

**Task 5.** The task report silent on the two subtle checks the brief flagged,
both handled correctly in code — **accepted**, no artifact consequence.

**Task 6, four minors.** Hardcoded tick indices in the witness —
**accepted** (the shape is frozen by the criterion). The growth assertion's
message not naming its late tick, and `late` being data-selected —
**accepted with disclosure**: tick 57 is used because tick 60 judges zero
rooms, and both readouts state that it was disclosed rather than picked. The
chaos comparator's reach shrinking to 90 rooms once the index is warm —
**accepted** (the comparator was independently computed and matched exactly at
1,339 when it was introduced). The oracle test comparing the full-roster
emitter-free branch zero times — **accepted**, and it is the one of the four
with a residue: that branch's identity rests on the site-by-site argument in
the ledger and on the emitter oracle, not on a direct comparison.

**Task 8, two info notes.** Two new session accessors with no caller until the
readout — **one was used by the readout and stands; the other,
`resident_emitter_timeline_copied`, was never called by anything and is
deleted at this close** (the readout read the witness directly). That is the
same disposition this campaign gave the dead fear-memory accessor, applied to
its own work. Rule 4's zero carrying its mechanism — **accepted**, and carried
into the chronicle's honest limits as a measurement of the cheapest case only.

**Task 9c, three doc minors plus one info.** The resident file's `latest_visit`
doc enumerating readers that are no longer its callers — **fixed in the close
absorption** (the doc now names its one caller). `latest_visit_and_witness`
returning a reference its sole caller discards, its rationale lapsed —
**fixed in the close absorption**: deleted for `trail_and_witness`. The spec's
§12 opening overstating that every number cites a `readout2` file, when the
step-1 numbers live under `step1-9c` and the test counts cite no file —
**accepted and not fixed**, because §12 is a frozen readout and the correction
belongs here rather than in it. Nothing but prose guarding the map's placement
below the early return — **carried forward as a followup**, and stated in the
chronicle's honest limits.

## The Confidence Gradient

The chapter's cost thread is re-scored with a fifth occasion
([`open-questions.md`](../../book/src/open-questions.md), "The fifth look at
cost found a repetition, not a quadratic"). Its three claims: the fold's cost
was a repetition rather than a quadratic; the per-commit cost gate that now
exists is a **count** over one window's turn path and is structurally blind to
every read this campaign made cheaper, so the base rate of an unwatched
dimension is unchanged; and — the sharper half — this is the second consecutive
campaign whose named mechanism was wrong until counted, which makes it a
statement about reading code as evidence rather than about either instrument.

**No per-bet score changed, and that is a finding rather than an omission.**
The chapter has no scoring table; it is prose tiers plus a running cost thread.
The bet this campaign is nearest to — the kernel substrate at the high-
confidence tier — is scored on *byte-identity and the claim "this changed
nothing"*, and this campaign made exactly that claim and had it scored the same
way (a single hash across eighteen runs and two trees, then across two code
states). It confirms that tier's text; it does not move it. The cost thread is
where the movement is, and it is a running record rather than a score.

## Followups

Promoted from the ledger and the readouts into the idea registry, where the
tooling and process backlog lives:

- **The water belief's path search per known water room.** After this campaign
  the two water reads are 99.08% of the six timed folds — 8.7 ms/call at the
  final band — and their cost is a bounded path search per known water room per
  read, not the 121 terrain samples per call. It is why the whole-tick criterion
  still fails, and it is the next quarry on this axis. *(New row, `raw`,
  high — measured.)*
- **The danger drive's per-step terrain sampling.** 2,268 of a tick's remaining
  3,168 terrain questions (72%) are the walk's own, once per candidate room per
  step. They are memo lookups now, but the shape is unchanged. *(New row, `raw`,
  high — measured.)*
- **A criterion about a slope wants an effect-size floor, not a fit floor.**
  The frozen goodness-of-fit filter admitted 0 of 4 campaign runs *because* the
  criterion succeeded. *(New row, `raw`, med.)*
- **The constant lint's blind spot for path-included test modules.** *(New row,
  `raw`, med, plus a board technique post.)*
- **The cross-tick affect memo stays unbuilt, with its number and its
  trigger.** The decision rule asked for a time share; its committed witness
  counts, because wall-clock measurement is banned in this project's tests, and
  a release-mode probe measured 785 µs per read at 2.62 replays per read — a
  whole-read cost, not a share. The trigger for building it is a **timed**
  replay share of ≥ 10%. *(Recorded on the corrected row.)*
- **The emitter's trail copy needs a second shape.** Rule 4 measured zero
  entries copied at every tick, with the mechanism stated: an emitter's home is
  frightening before it has committed a dated sighting, so the prefix is empty.
  That is a real measurement of the cheapest case; stressing it needs an emitter
  with history, which nobody constructed.
- **The map's placement below the early return is guarded by prose alone.** The
  criterion it protects is measured once per campaign, so a future edit that
  moves it back would be invisible until the next readout.
- **The growth witness's late tick is data-selected.** H6 is taken at tick 57
  because tick 60 judges zero rooms. Disclosed in both readouts and in the
  test's own message; a witness that chose its tick by a stated rule rather than
  by inspection would be stronger.
