# 0846. The errand is the committed unit of intention

**Status:** Accepted (2026-09-06) · **Decider:** Nathan (autopilot) ·
**Campaign:** The Warrant (Penstock stage 7b) · **Relates:**
[0238](0238-stage-7-is-three-stages-and-their-order-is-forced.md),
[0847](0847-the-7b-before-7c-ordering-holds-on-the-why-not-the-per-step-prose.md),
[0010](0010-predicate-schema-value-kind-enforced.md),
[0015](0015-predicatedef-name-duplicates-key.md);
[The Warrant spec](../superpowers/specs/2026-09-05-the-warrant-design.md)
§3, §4, §4.0, §4.1

In the context of a walking creature's reason for setting out existing only as
a live `Mode` inside the tick and as a prose sentence copied onto every step it
takes, we decided that **the errand — a maximal run of constant `Mode` — is the
committed unit of intention**: one fact per errand, its *reason* carried by one
of **eight permanent `errand/*` predicates**, its object carrying the errand's
**origin**, and its `provenance` returned to naming its producer
(`vessel/liveness`) like every other fact in the repository. We accept that the
errand's endpoint is derived rather than stored, and that an errand is opened
but never closed.

## The three placements, and why each is where it is

**The reason lives in the predicate.** `Value` is `{Entity, Text, Number,
Flag}` and cannot hold a pair, so a fact wanting two typed components must
split them. `register_predicate(name, functional, doc)` gives every predicate a
doc string, and that doc is already the only prose `windows/historiography`
renders for a predicate. Putting the reason there moves the reader-facing words
out of the ledger and into the concept registry — which is
`kernel/src/phenomena.rs`'s producer rule ("a producer cannot know who is
looking … a stored string could only ever be culture-neutral or wrong") applied
to facts rather than to phenomena. A per-drive predicate family is established
convention here and not a novelty: `drank` / `eaten` / `rested` / `slept` are
already four predicates for one relation.

The eight keys are `errand/water-known`, `errand/water-blind`,
`errand/forage`, `errand/comfort`, `errand/rest`, `errand/flight`,
`errand/company`, `errand/home`. They are **`Mode`-shaped, not
`DriveKind`-shaped**: `Homing`/`Idle` is not a pursuit at all, and
`Pursuing(Danger)` is repulsion from a threat rather than attraction to a
target, so a `Go(target, reason)` shape under-types two of the eight cases.
The mapping is an exhaustive `match` with no `_` arm, so widening `Mode` is a
compile error rather than a silent fall-through into the wrong errand. Being
predicate spellings, the eight are **permanent on-disk keys**, as permanent as
`agent-at`: a change is an epoch, never a rebaseline.

**The object carries the ORIGIN, not the target.** The spec's first draft said
target. There is no target to put there. The arbitration seam exposes
`Intent::Do(Action)` and nothing else; `Action::MoveTo(n)` names the *next
step*, and `Drive::proposal` is documented as "the next executable step". No
destination is materialized at the commit site, so the field as first specified
had no possible caller. Surfacing a goal through `Drive`/`Resolution` would be
an arbitration-seam change — an epoch that is also a behaviour change — and
`Flag(true)` would discard a fact that is free and correct. `st.pos` at the
instant the errand's first step is charged is always available and never wrong,
and it is what makes an errand a *segment* rather than a point.

**The provenance names the producer.** `agent-at`'s `provenance` becomes
`"vessel/liveness"`, retiring the semantic content that one test keyed on for
as long as the prose stayed meaningful.

*This paragraph first said the flip "restores the truth of `liveness.rs`'s own
claim that '`provenance` is free-form prose no fold may key on'", and that
overstates what happened — corrected here, in this campaign's own unmerged
record, rather than left to read as a settled result.* The literal claim never
stopped being true and still is: `hornvale_historiography::group` joins a step
to its covering errand by comparing provenance, but `group` is a **renderer** —
it computes a presentation over a fact list already collected, holds no state,
and nothing downstream of it re-enters the ledger. No fold keys on provenance.
What the flip did not restore is the *licence* that sentence was being read
for. A renderer now depends on the field, so provenance is no longer free to
vary per call site; the constraint the campaign actually created is narrower
and is the one worth quoting — **within one producer, one spelling** — and it
is why `the_errand_producer_commits_only_agent_at_and_the_eight_errand_keys`
exists.

## Consequence

The recount now says where a creature **got to**, never where it *meant* to go.
For a completed errand those coincide. For an **abandoned** errand they do not,
and asserting a target would have made the ledger claim an intention the code
never formed — so abandonment becomes visible content instead of a gap in the
trail.

Two asymmetries follow and are inherited by stage 7c rather than solved here.
An errand's **endpoint is derived from its steps**, so a compaction that drops
every step must fold the endpoint into the errand fact first, or the segment
loses one of its two ends. And **nothing ever closes an errand** — there is no
end marker, no expiry, and `st.errand` is never set to `None` — so an errand's
committed span measures *elapsed* time, not time spent walking. Seed 11's first
resident holds `walking home (sated) — 5 steps, days 113.04798 to 116.05578`
across five sleeps and a graze. A compaction that drops steps therefore loses
the activity profile inside an errand, which nothing marks.

The one case where a target genuinely exists — `errand/water-known`, where
`st.believed` holds the source — is deliberately **not** special-cased: one
shape for all eight keys, and the believed source is already recoverable from
the belief fold.

## Alternatives rejected

- **A coarser unit (the drive episode).** Merges the outbound and homing legs
  and deletes `walking home (sated)` outright: strictly less content.
- **Keep the prose and add the typed fact beside it.** Two sources for one
  meaning, free to disagree, and it defers the fidelity call into 7c — which is
  the ordering [0238](0238-stage-7-is-three-stages-and-their-order-is-forced.md)
  forbids.
- **One `intends` predicate with a packed object** (`Text("thirst-known@…")`).
  One key instead of eight, but no per-reason registry doc, so the prose returns
  to code and a future language layer has nothing to realize.
- **The reason in `provenance`.** Recreates the gap being closed: every other
  producer in the repository puts a *system* name there.
- **Widen `Value` with a structured variant.** Right in the abstract; a kernel
  save-format change touching every match arm, every serialized surface and the
  clients, for a benefit this campaign does not need. Carried as
  `TOOL-value-structured-variant`.

**See also.** [The Warrant chronicle](../../book/src/chronicle/the-warrant.md);
[ledger](../superpowers/ledgers/2026-09-05-the-warrant.md) #1, #3, #5, #6, #25,
#27; `windows/vessel/src/liveness.rs` (`errand_predicates`, `errand_key`);
`windows/vessel/tests/suite/the_warrant.rs`.
