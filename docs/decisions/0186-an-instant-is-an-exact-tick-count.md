# 0186. An instant is an exact tick count — the lattice is the time domain

**Status:** Accepted (2026-08-23) · **Decider:** Nathan · **Campaign:** The
Escapement · **Supersedes:** nothing · **Amends:**
[0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md)'s
"never in the compute path" clause, for time specifically

In the context of `WorldTime` being emitted through `quantize` like every
other float at 8 *significant* digits — precision proportional to
MAGNITUDE — facing the fact that time is the only unbounded quantity in the
system, so a committed day's resolution decayed with world age (measured at
a full lattice spacing of 86.4 s between adjacent storable instants at
world-year 100 and **24 hours** at world-year 200,000, a horizon
`windows/worldgen/src/hazard.rs` actually constructs), we decided that **an
instant is an exact `i64` tick count** (100,000 ticks per standard day, one
tick = 0.864 s), not a quantized `f64` day, and that **the tick lattice is
the time domain itself, not a quantization of it**.

**What that second clause forecloses, concretely.** An earlier draft of this
campaign's spec promised the compute path would stay untouched — only the
emit boundary would change representation. `hazard.rs` disproved that within
Task 1: an event's day is drawn continuously (`block_start + stream.next_f64()
* BLOCK_DAYS`), filtered continuously against a window (`day >= start && day
< end`), and then the *survivor* is stored as a `WorldTime` — which rounds to
a tick. Read that stored day back out and compare it against the same window
again (`the_window_is_half_open_at_both_ends` does exactly this) and a raw
continuous draw is now compared against a tick-rounded bound; the half-open
property breaks in whichever direction the rounding went. That is
quantization reaching the compute path, which 0033 forbids in as many words.
Declining to fix it would mean either (a) never storing an instant mid-
computation, which no real pipeline can promise, or (b) accepting a class of
boundary bugs disguised as `f64` ULP noise. Neither is acceptable, so this
decision authorizes the lattice to reach the compute path deliberately: **code
that draws a continuous time converts to ticks once, at the draw, and compares
ticks exactly thereafter — one domain per comparison, never a raw draw against
a round-tripped bound.**

**This is a determinism improvement, not merely a cost.** Boundary behaviour
stops depending on `f64` ULP accidents (which platform, which order of
operations) and becomes exactly reproducible: the same seed produces the same
tick, on every host, forever. `hazard.rs` already documented an analogous ULP
boundary hazard it chose to *record rather than guard*; this decision makes
that class of jitter deterministic instead of merely documented.

**Consequences, accepted deliberately:**

1. Which events fall inside a window can change at the tick boundary
   introduced by this decision. Committed artifacts move; a census may move.
   That is this decision's epoch, not a regression to chase.
2. `WorldTime` leaves the quantize contract **entirely**, for time only. An
   integer is exactly representable in JSON and needs no quantization at any
   magnitude — 0033's "proportional to magnitude" cost, which is what made
   time's resolution decay in the first place, cannot recur for this type.
   0033's other three quantized surfaces (`Value::Number` in the ledger, the
   lab CSV, scene/ephemeris floats) are UNCHANGED: this amendment is scoped to
   time's representation, not to quantization as a mechanism.
3. **The migration is staged** (Ruling 8, `docs/superpowers/plans/2026-08-23-
   the-escapement.md`'s "Execution phasing"), because flipping `WorldTime`'s
   field to `i64` changes comparison behaviour workspace-wide the instant a
   value rounds to its nearest tick at construction, and the majority of
   affected comparison sites are in `windows/vessel`, held off by
   `campaign/the-hand`. Phase A (this record's own task) adds the full
   tick-shaped surface — `from_ticks`, `ticks`, `from_std_days`,
   `as_std_days`, `whole_days`, `tick_of_day` — as accessors over the SAME
   `f64` field `WorldTime` already had, so nothing rounds on storage and no
   behaviour changes yet; `new`/`day` remain as migration shims. Phase B
   flips the field itself to `i64`, derives `Ord`/`Eq`/`Hash`, deletes the
   shims, and lands the tick-domain comparison fixes and the vessel port in
   one commit, gated on The Hand.
4. A year stamped into a day-typed slot is what made `person-died`
   uncommittable in every world (The Ell, decision 0126, superseding 0014).
   This decision does not reopen that boundary — `WorldTime`'s field stays
   private and the crossings stay named — it only changes what the private
   field eventually holds.

**The physical magnitude is nothing** — 0.864 s of jitter on events drawn
across 10,000 to 200,000 years. The reason this needed ratification is not
the magnitude; it is that it amends a constitutional rule (0033) and, once
Phase B lands, moves committed bytes.

**See also.** Decision 0033 (quantize at the emit boundary; amended here for
time specifically); 0126 (superseding 0014 — `Fact.day` carries a typed
`WorldTime`); `docs/superpowers/specs/2026-08-23-the-escapement-design.md`
§2.1 (the ratified position and its four consequences, restated here);
`docs/superpowers/plans/2026-08-23-the-escapement.md`'s "Execution phasing"
(the two-phase migration this decision's Phase A/B language refers to).
