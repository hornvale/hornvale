# 0230. A query bound is quantized to read a quantized store

**Status:** Superseded by
[0191](0191-an-exact-store-needs-no-quantized-query-bound.md) (2026-08-24;
accepted 2026-08-24 — The Escapement removed the quantized store this record's
premise rests on; the ruling was correct on the tree it was made against) ·
**Decider:** Nathan · **Relates:**
[0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md)
(the rule this narrows) · [0126](0126-fact-day-is-a-typed-world-time.md)
(what `Fact.day` is) · [0069](0069-fine-position-is-never-serialized.md) (why a
position is a ledger read at all) ·
[0229](0229-one-body-type.md) (what made this call site hot)

In the context of `latest_committed_position` selecting facts by `f.day <= t`
against a `Fact.day` that `Ledger::commit` already quantized, we decided that
**a query bound compared against an already-quantized stored field is itself
quantized at the point of comparison** — narrowly, at this one call site — and
that this is a **read of the emit boundary rather than an entry into the compute
path** decision 0033 forbids.

## Context

0033 puts quantization at the serialization boundary **only**, never in the
compute path, and `kernel/CLAUDE.md` states the same rule as
quantize-at-emit-only. The change this record adjudicates is one line inside a
read:

```rust
fn latest_committed_position(ledger: &Ledger, npc: &Body, t: WorldTime) -> Option<RoomAddr> {
    let t = hornvale_kernel::quantize(t.day());
    ledger.facts_of(npc.entity, AGENT_AT)
        .filter(|f| f.day.map(|d| d.day() <= t).unwrap_or(false))
        .last()
        // …
}
```

It is not a cosmetic alignment. `Ledger::commit` quantizes a fact's day to 8
significant digits, and that rounding goes **upward** as often as down —
`quantize(0.011719999738288106) == 0.01172`, strictly greater. So a fact
committed at exactly `t` could fail its own `d <= t` filter, read back as "no
position committed yet", and fall through to `npc.home`, one line away from the
commit that produced it. The bug was pre-existing, known and narrow: it is
`KNOW-commit-read-same-instant` in the idea registry, raised by The Deed, live
only where `wait` commits at `self.day` and `narrate_motion` reads at that same
instant.

The spec's §5 risk 4 named it and gave two admissible answers — *leave it and
cite the row*, or *fix it deliberately and measure what moves*. The Hand took
the second (commit `0f59008a3`, its own commit ahead of the merge) because 0229
removed `Agent.position`, a stored mutable field, and made the possessed body's
position a read through this very function on **every turn**. What had been a
rare wrong answer would have become the player's position reading stale
immediately after every move.

**What was not done is the reason this record exists.** The measurement was
made and the fix was argued in the function's own doc comment, but no decision
stated the 0033 relationship out loud, and a whole-branch review found the
unruled exception rather than the code. A rule with a silent exception in the
tree is worse than either the rule or a stated exception.

## The ruling, and its scope

**This is the read side of 0033's own boundary, not a second boundary.**
Quantization at emit means the ledger's stored days *are* the 8-digit values;
that is what 0033 chose. A query that compares a full-precision bound against
that store is comparing two different precisions, and the mismatch — not the
quantization — is the defect. Aligning the bound restores comparability with
the values 0033 put there.

Four properties keep it narrow, and all four are load-bearing:

1. **The quantized value never escapes.** `t` is a local, consumed by one
   comparison in one closure. It is never committed, stored, returned, printed,
   or used in arithmetic.
2. **The result is discrete.** The function returns `Option<RoomAddr>` — a
   packed `RoomId` (decision 0006). No lossy float propagates forward, so the
   selection can change but no downstream *number* is coarsened.
3. **The Lorenz guard-rail is untouched.** 0033's guard-rail is that a chaotic
   forward-integrator must never be seeded from quantized ledger floats.
   Nothing here integrates `t`; the drive simulation carries its own
   full-precision `WorldTime` and receives from this function only a room
   address.
4. **It cannot flip an ordering that was not already within one part in 10^8.**
   Two days further apart than the quantum keep their order. The band where the
   selection can change is exactly the band where the unaligned comparison was
   answering the wrong question.

**What this does not license.** It is not a general permission to quantize in a
read path. It licenses quantizing a **bound** for comparison against a field
whose stored value was quantized at its own emit, where the result of the
comparison is a **selection** and not a quantity. Quantizing a value that then
participates in arithmetic, feeds a threshold on a computed quantity, or seeds
an integrator remains what 0033 forbids, and the compute path — noise fields,
sculpting, orbital mechanics — stays at full precision as 0041 and 0090 left
it.

## Evidence, and what each piece does and does not show

- **The fix's own commit is the direct evidence.** `0f59008a3` touches exactly
  two files, `windows/vessel/src/liveness.rs` and `docs/timings.md`. It changed
  no committed artifact: the full `hornvale-vessel` suite (604 tests at that
  commit) stayed green and the artifact regeneration run alongside it produced
  no diff. A behavioural change reaching any golden would have shown up there.
- **The arc's later byte-identity check corroborates but is not a control for
  this change.** 0229 records 12,534 facts, 3,205,503 bytes and identical
  sha256 across two detached worktrees. Both of those trees already carried
  this fix — it landed at Task 3, the rename at Task 7 — so that check shows
  the played-world surface is stable *with* the change, not that the change
  moved nothing. The commit above is the before/after evidence; this is not.
- **The narrow band is unexercised by any committed world.** Nothing in the
  tree commits a fact and reads at the identical instant *except* the
  `wait`/`narrate_motion` path the registry row already named, which is why the
  regen came back clean.

## The alternative that was considered and rejected

`KNOW-commit-read-same-instant` names two fixes: quantize the query day, or
quantize the **session clock**. The second was rejected on blast radius — the
session clock is what every printed day-stamp derives from, so quantizing it
would move player-facing output and every artifact that carries a day, to fix a
comparison in one function. The chosen fix is confined to the comparison that
was wrong.

## Consequences

- `latest_committed_position` is the **only** sanctioned site. A second call
  site wanting this treatment is a request to widen this record, not a
  precedent to follow silently; `hornvale_kernel::quantize` is called at exactly
  one place in `windows/vessel/src/liveness.rs`, and this is it.
- The function's doc comment carries the full argument at the site, which is
  where a future reader meets it. This record carries the *ruling*; the doc
  comment carries the *mechanism*. Neither is a substitute for the other.
- `KNOW-commit-read-same-instant` is closed for this call site and open
  everywhere else: any other reader comparing a raw day against a committed one
  has the same latent bug, and nothing scans for it.
