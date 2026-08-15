# Retrospective — The Ballast

A four-item cleanup campaign for the reds [The Staff](the-staff.md) surfaced on
`main`, plus two findings that turned up while cleaning. Process lessons only;
the product account is in `book/src/chronicle/the-ballast.md`.

## 1. Every first diagnosis in this campaign was wrong, and each was refuted cheaply

Five times, a confident explanation was overturned by the smallest available
measurement. This is the campaign's headline, and it is not about any one defect.

| red | first diagnosis | what measurement showed |
|---|---|---|
| board sync tests | tests interfere under parallelism | three tests hardcode a hostname; `peer_ages` excludes `current_host()` |
| `disposition_calibration` | a bug needing a fix | already adjudicated by The Radiation; deliberately left red as a marker |
| `session_cost` | ceiling calibrated off-host, needs re-basing | passes alone on the failing host at load 0.16 — it measures contention |
| roster self-healing | works as documented | never ran once; the script contains no `git` write at all |
| lefford's lane install | "no gate can run anywhere" (inherited) | already installed and current; the report was stale |

None of the five explanations was unreasonable. Each was available *before* any
measurement and survived exactly as long as nobody took a reading. The cost of
the reading was, in every case, a small fraction of the work that would have
followed from believing the guess.

**How to apply:** when a diagnosis arrives already-formed — from a prior session,
a board post, or your own narrative — treat it as a hypothesis and name the one
command that would refute it. If that command is cheap, run it before acting.

## 2. My own worst error: attributing a difference to the one variable I had not held fixed

I told an implementer the board tests "interfere under parallelism," having
compared a **serial run on the Mac** against a **parallel run on lefford**. Two
variables moved; I attributed the difference to one of them and never observed
the failure on the Mac at all. That sent the agent through roughly 150 test
executions chasing a phantom.

This is [right-measurement-wrong-attribution] in its purest form, and the tell
was present in my own evidence: I had no serial-on-lefford or parallel-on-Mac
cell. **A two-cell comparison across two changed variables licenses no
attribution to either.**

## 3. A subagent's guard is not verified until you have seen it fail

The `session_cost` fix added a second serialization class and a guard over it.
The report did not claim the new guard had been mutation-tested, so I ran three
mutations: drop a name from the config, remove the source marker, comment out
the setting. All three went red.

The third was the one that mattered — it reddened **only** the new class's guard
while the pre-existing one stayed green, which is what proves the per-class block
scoping actually isolates rather than matching any similar setting anywhere in
the file. That property was the entire design rationale, and nothing else would
have demonstrated it. This project has now found six checks that reported green
while verifying nothing; the cost of one mutation round is trivial against that.

Separately, the guard compares config text to source text, so it is structurally
blind to whether the tool resolves the filter to anything. A regex typo would
satisfy both directions and match zero tests. Verified independently with
`cargo nextest list -E`.

## 4. Existence is not evidence of rewrite

The roster copy-out originally triggered on `[ -f "$roster" ]`. But the roster is
a **committed** file, so it is present after every checkout, for every set — the
condition was true always, and a comment asserting it would be "absent for every
set but `gate`" was simply false. The effect: every green job of any kind would
copy out an unmodified file, and the retrieval command takes the most *recent*
copy, so it would return a no-op diff while the real one sat unconsulted.

Replaced with `git diff --quiet`, which asks the actual question ("did this run
modify it?") and does not hardcode which set rewrites it.

**The general shape:** when guarding on a file, ask whether you mean *exists*,
*changed*, or *was produced by this run*. For anything under version control the
first is nearly always the wrong one, and it fails in the safe-looking direction.

## 5. An allow-list gate cannot see that its own list went short

The Staff's commit gate runs a named roster; absent tests are excluded by design,
with the documented remedy that a test enters on the next green stage gate. That
remedy never worked: the gate rewrote the roster into scratch the next dispatch
erases, and nothing wrote it back. `hornvale-hearsay` had zero entries and
therefore zero coverage in every green commit gate since it merged.

Two properties made it invisible. The gate got *faster*, which reads as a virtue.
And nothing was red, so nothing was examined. **A self-updating artifact written
to scratch is not self-updating** — and this was documented in the same script,
for a different artifact, three dozen lines above the gap.

The guard added here fails when a workspace crate has no entry, three-valued in
the project's usual ratchet shape so a known gap can be declared with a reason
and a declaration that becomes satisfied fails just as loudly as an undeclared
absence.

## 6. Smaller operational lessons

- **A board post's `BY` defaults to the current branch**, so posting from the
  shared main checkout — which is where you naturally end up when comparing two
  branches — attributes a campaign's coordination request to `main`, where it
  reads as repo policy. Pass `BY` explicitly from that checkout.
- **A board notice is a claim with a timestamp.** The hold-off saying no gate
  could run anywhere was accurate when written and false by the time it was read.
  Corrected on the board rather than left to mislead.
- **"Pre-existing drift" is not automatically out of scope.** An implementer left
  the Makefile header advertising three retired targets and naming neither
  replacement; it was judged unrelated to the deletion at hand. It was the file's
  own table of contents for exactly the ladder being changed, and anyone
  orienting off it reached for a target that exits 2.
- **A decision-number collision can reverse direction.** Two campaigns held an
  unmerged 0134. The first board post assumed the other would close first; the
  ordering changed, and the post had to be corrected rather than left standing.
- **One missed same-commit regeneration**, caught by the close's drift check:
  the seam fix added a function to `history_emit.rs`, shifting every line number
  below it, and `docs/audits/seam-guard-roster.md` records line numbers. The
  roster should have been regenerated in the commit that moved it. It is a
  cosmetic drift here — no verdict changed — but it is the same discipline that
  matters for the type-audit report, and the close is the wrong place to notice
  it. The regeneration is committed separately, labelled as the catch-up it is.

## 7. What this campaign deliberately did not do

- Did not touch `docs/timings/subfloor-roster.tsv` — another campaign holds a
  committed harvest of it, and a second edit would collide for no gain.
- Did not raise or re-base `TURN_BUDGET_MS`. The measurement said the ceiling was
  correct on the host that failed; changing it would have deleted a signal.
- Did not extend the wall-clock-budget class to `graph_cost.rs` — same shape, but
  unmeasured here. Registered rather than silently widened.
