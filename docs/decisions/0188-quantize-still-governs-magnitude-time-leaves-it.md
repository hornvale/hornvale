# 0188. Quantize still governs magnitude; time leaves the contract entirely

**Status:** Accepted (2026-08-24) · **Decider:** Nathan · **Campaign:** The
Escapement · **Supersedes:** nothing · **Amends:**
[0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md)'s
scope — which quantities the quantize-at-emit contract covers, not the
"never in the compute path" clause 0186 already amended

In the context of decision 0033 quantizing every high-precision float at
four emit boundaries — the ledger's `Value::Number` and `Fact.day`, the lab
CSV, and the scene/ephemeris JSON — for one reason (last-ULP libm divergence
between platforms reaching a committed golden), and facing The Escapement's
retype of `WorldTime` to an exact `i64` tick count, we decided that
**`Ledger::commit`'s day-quantization block is deleted outright, and time
leaves the quantize contract for good — while every other quantized surface
0033 named is unchanged.**

**This is a distinct amendment from 0186's, though both amend 0033 from the
same campaign block.** 0186 amended 0033's *compute-path* clause ("never
quantize before a comparison completes"), authorizing the tick lattice to
reach comparisons that used to run on raw `f64` draws. This record amends
0033's *scope* clause instead: which quantities get quantized at all, not
where in the pipeline quantization is allowed to happen. They read almost
identically applied to the same code because both bear on the same deleted
block at `kernel/src/ledger.rs`, but the questions they answer are
different, and each is citable on its own without the other.

**Why quantize existed for `Fact.day` in the first place, and why that
reason no longer applies.** `quantize` rounds an `f64` to 8 *significant*
decimal digits specifically to erase last-ULP libm divergence between
platforms — noise proportional to the value's own magnitude, which is why
significant-digit rounding (not fixed-decimal rounding) was the right tool
for it. An exact `i64` has no ULP: two platforms computing the same tick
count from the same seed produce the identical integer, bit for bit, with
no rounding step required to make them agree. Quantizing an already-exact
integer would not improve its cross-platform stability — there is nothing
left to erase — and section 1 of this campaign's spec found that applying
significant-digit rounding to a magnitude that grows without bound
(committed day, world age unbounded) let precision *decay*: adjacent
storable instants a full 86.4 s apart at world-year 100, degrading to 24
hours at 200,000. Deleting the
quantize call did not just stop being necessary; keeping it was actively
harmful, and worse, quantize's rounding went upward as often as down, so a
fact committed at exactly `t` could fail its own `day <= t` filter on
read-back — an independent, present-day defect `campaign/the-hand` hit
without knowing this campaign existed (spec §1, "the second motivation").

**What 0033 still governs, unchanged by this record.** Nothing else about
0033 moves:

- `Value::Number` in every committed `Fact` object — populations, counts,
  elevations, masses, shares — still quantizes at `Ledger::commit`. These
  quantities are all magnitude-bounded, where significant-digit rounding
  buys effectively constant absolute precision; time was the sole unbounded
  exception, which is exactly why it is the one quantity carved out here.
- The lab CSV's `render_csv` still quantizes every emitted metric.
- The scene/ephemeris JSON's `f64` fields — including `scene/eclipses/v1`'s
  `day`, `from_day` and `until_day` — still quantize, **deliberately**, even
  though the sibling `*_ticks: i64` fields this campaign adds beside them
  carry the same instants exactly. The `f64` fields are a cross-repo
  contract the external Orrery consumes from the released catalog, additive-
  or-versioned-only (spec §5); quantizing them was never about time's
  representation being lattice-exact internally, it is about giving an
  external consumer of the *float* wire format the same cross-platform
  stability every other quantized float on that wire gets. Leaving them
  quantized costs nothing and changes no external behaviour; the added
  `*_ticks` fields are where the precision fix actually lives.

**What it no longer governs.** `WorldTime`'s own serialized form (a bare
`i64` tick count, `#[serde(transparent)]`) is never passed through
`quantize` at any boundary, and `Ledger::commit`'s day-canonicalization block
is gone rather than merely made a no-op — there is no longer code at that
site that could regress into re-quantizing a tick.

**Why the carve-out is principled, not an exception carved for convenience.**
0033's discipline was never "quantize every float," it was "erase platform
noise at the boundary where it would otherwise reach a committed artifact."
An exact integer representation removes the noise at its source instead of
downstream — a strictly stronger guarantee delivered a different way, not a
weaker one smuggled in under a new name. The alternative (quantizing the
tick's own `f64`-day *view*, `as_std_days()`, before any onward consumer read
it) was rejected because that view is never itself stored or compared; only
the source-of-truth tick count is, and it already agrees exactly across
platforms by construction.

**Consequence.** `kernel/src/ledger.rs:commit`'s day-quantization block is
deleted, not merely bypassed — see decision 0186's own commit for that
diff. `windows/scene/src/lib.rs`'s `EclipseElem` and `EclipsesScene` gain
`day_ticks`, `from_day_ticks` and `until_day_ticks: i64` fields beside their
existing quantized `f64` siblings, additive at `scene/eclipses/v1` (Task
10). `kernel/src/ledger.rs`'s own test module holds
`committed_numbers_are_quantized_but_days_are_exact`, asserting both halves
of this split against the same committed fact.

**See also.** Decision 0033 (the quantize-at-emit contract this record
amends the scope of); decision 0186 (the sibling amendment of 0033's
compute-path clause, ratifying the same campaign's tick lattice); spec §1
(the measured precision-decay table and the read-back defect); spec §5 (the
cross-repo scene contract this record's `f64`-fields-stay clause restates).
