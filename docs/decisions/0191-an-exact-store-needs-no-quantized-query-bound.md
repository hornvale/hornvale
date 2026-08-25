# 0191. An exact store needs no quantized query bound

**Status:** Accepted (2026-08-24) · **Decider:** Nathan · **Campaign:** The
Escapement · **Supersedes:**
[0230](0230-a-query-bound-is-quantized-to-read-a-quantized-store.md) — whose
premise, that `Ledger::commit` quantizes a fact's day, was **true when 0230
was made and is false now** · **Relates:**
[0186](0186-an-instant-is-an-exact-tick-count.md) (the retyping that removed
the premise) · [0188](0188-quantize-still-governs-magnitude-time-leaves-it.md)
(time leaving the quantize contract) ·
[0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md)
(the rule 0230 narrowed) · [0126](0126-fact-day-is-a-typed-world-time.md)
(what `Fact.day` is)

In the context of decision 0230 licensing `latest_committed_position`
(`windows/vessel/src/liveness.rs`) to quantize its query bound so it could be
compared against an already-quantized stored day, and The Escapement having
since retyped `hornvale_kernel::WorldTime` to an exact `i64` tick count and
deleted `Ledger::commit`'s day-quantization block outright, we record that
**0230's licensed exception no longer has anything to compensate for, the
call it sanctioned has been deleted, and the ruling is withdrawn rather than
narrowed**: a query bound compared against an exactly-stored instant is
compared as-is, and quantizing it would now *introduce* the very mismatch
0230 existed to remove.

## 0230 was right, and this record is not a criticism of it

This needs saying plainly, because a supersession that reads as a rebuke of
its predecessor teaches the wrong lesson about a log that is meant to be
appended to freely.

0230 adjudicated a real, measured, present-day defect on the tree as it stood.
`Ledger::commit` did quantize `fact.day` to 8 significant digits; that
rounding did go upward as often as down
(`quantize(0.011719999738288106) == 0.01172`, strictly greater); a fact
committed at exactly `t` therefore could and did fail its own `d <= t` filter
on read-back, dropping a possessed body to `npc.home` one line after the
commit that placed it. Decision 0229 made that read happen on **every turn**,
which turned a rare wrong answer into a standing one. Aligning the bound was
the correct minimal fix to a comparison between two different precisions, and
0230's four narrowness properties — the quantized value never escapes, the
result is discrete, the Lorenz guard-rail is untouched, no ordering outside
one part in 10^8 can flip — were argued carefully and were all true.

0230's only vulnerability was that it was **a fix to a symptom of a defect
another campaign was removing at the root, concurrently and without either
knowing of the other.** That is a coordination fact about two branches in
flight, not a defect in the reasoning. The Escapement's own spec §1 anticipated
exactly this and wrote down the obligation it creates: *"Whoever merges second
must delete the other's half of this pair — both branches compile and both
suites pass in isolation, so nothing will raise it."* The Hand merged first, so
the deletion fell to this campaign.

## What changed underneath it

`WorldTime` is now `{ ticks: i64 }`, and `Ledger::commit` canonicalizes a
fact's day not at all — there is nothing for quantization to canonicalize,
because an integer is already equal to itself on every platform (decisions
0186 and 0188). The stored day *is* the day handed in, exactly.

So each of 0230's load-bearing sentences inverts:

- **"The store is quantized."** It is not. `Ledger::commit`'s day-rounding
  block is deleted, not skipped, and
  `committed_numbers_are_quantized_but_days_are_exact` (`kernel/src/ledger.rs`)
  pins both halves.
- **"A full-precision bound against that store compares two different
  precisions."** There are no longer two precisions. Both sides are the same
  `i64`, and `1172 == 1172`.
- **"Aligning the bound restores comparability."** Aligning the bound would now
  *destroy* it: `quantize` on a tick-derived day rounds it off the lattice, so a
  quantized bound compared against exactly-stored days reintroduces the original
  mismatch **in the opposite direction** — a fact committed at `t` failing
  `d <= quantize(t)` because the bound moved down. This is the failure mode
  0230 was written to prevent, arriving by the mechanism 0230 chose.

## The ruling

**A query bound is compared against a stored instant as-is.** No quantization
sits in the read path in `windows/vessel`, and none is licensed to. 0230's
carve-out from 0033's quantize-at-emit-only rule is withdrawn in full rather
than kept as a narrowed permission, because the condition that made it
admissible — a store whose values had been rounded at emit — no longer exists
for time. 0033 itself is untouched by this record; 0186 and 0188 are what moved
time out from under it, and every other quantized surface 0033 names is
unchanged.

**A no-op is not a safe residue.** Leaving `quantize(t.day())` in place would
have been harmless *arithmetically* — quantizing a day derived from an exact
tick count usually returns it unchanged at present-day magnitudes — and that is
precisely the argument against keeping it. It would have stood as a live-sounding
rationale for a compensation with nothing left to compensate, citing a record
whose premise had been removed, and it would have started rounding for real at
deep time, where the tick lattice is finer than the 8-significant-digit one.
Both halves of the pair were deleted for the same reason: a workaround that
outlives its cause is worse than no workaround, because it reads as an active
protection.

## What is in the tree now, verified rather than asserted

- `latest_committed_position` filters on `f.day.map(|d| d <= t)` — a bare
  `WorldTime` comparison, `Ord`-derived on the tick count. The Hand's
  `let t = hornvale_kernel::quantize(t.day());` line is gone.
- `place_agent_now` (`windows/vessel/src/session.rs`) — The Escapement's own
  half of the pair, which advanced the session clock to whatever the ledger
  had actually stored — is gone too.
- `hornvale_kernel::quantize` is called nowhere in `windows/vessel`'s read
  paths. The remaining calls in that crate are `clock::tempo` (a mass ratio,
  quantized so its clamp boundary is reproducible) and `snapshot.rs`'s
  `quantize_serde::f64_field` serializers — both emit-boundary uses 0033
  governs unchanged.
- Both deletion sites carry the full argument in their doc comments, which is
  where a future reader meets it. This record carries the *ruling*; those
  comments carry the *mechanism*.

## On the numbering

0191 supersedes 0230, a **higher** number, which is unusual enough to explain
once. Decision blocks are allocated to campaigns before they run, not at the
moment a record is written: The Escapement holds 0186–0195 and The Hand held a
later block, and The Hand — the younger campaign — merged first. A number is an
identifier, not a timestamp; git holds the chronology.

## Consequences

- `KNOW-commit-read-same-instant` in the idea registry is **closed at the root,
  not at a call site.** 0230 closed it for `latest_committed_position` and left
  it open everywhere else, because the underlying rounding remained. The
  rounding is gone, so every reader comparing a raw day against a committed one
  is now comparing exact ticks. The registry row is updated to say so.
- A future request to quantize a query bound in a read path has no precedent to
  cite. 0230 is superseded, not narrowed, and 0033 forbids it on its own terms.
- The obligation spec §1 placed on the second merger is discharged. Nothing in
  the tree still compensates for `Ledger::commit`'s day rounding.
