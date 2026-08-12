# 0126. `Fact.day` carries a typed `WorldTime`

**Status:** Accepted (2026-08-11) · **Decider:** Nathan · **Supersedes:** 0014

In the context of a fact envelope whose optional timestamp had been a bare
`Option<f64>` since the ledger existed, facing a campaign that shipped a
registered predicate **no world could ever commit** because one producer
stamped years into a field every consumer read as days, we decided to
**retype `Fact.day` to `Option<WorldTime>`, with `WorldTime`'s inner `f64`
private behind a validating constructor**, accepting that the change reaches
all 445 existing `WorldTime` uses rather than the 94 write sites alone, and
that the type defends *construction* and not *deserialization*.

**What 0014 said, and on what grounds.** 0014 kept `Fact.day` bare because
"the field is simple, its meaning is clear from context, and wrapping it buys
no safety worth the churn." It was recorded as a do-not-relitigate item so the
question would not resurface at each review. Its own framing named the standard
for reopening: new information.

**What falsified it.** The Particular registered `person-died` and committed it
zero times, in every world, on every seed. Promotion computed a birth as
`founded − maturity.days()` — a bake **year** minus a **day** count — and then
filtered a death against a present in years; the gate it produced was
unsatisfiable for every species in the roster by an order of magnitude, the
narrowest margin being 39.4 years against a required 5.5. Nothing caught it,
because every layer agreed it worked: the predicate was registered and
documented; the domain unit test hand-built both branches and passed; the trope
probe counted the capability as vocabulary the world held; and the live-world
test's `if let Some(died)` arm had never once executed while staying green. It
was found by scoring a preregistered prediction *numerically* at a campaign
close — the only step that computed a figure the tests did not already assert.

That is not an argument that types are better than documentation in the
abstract. It is a worked counterexample to 0014's specific claim that the
meaning was clear from context: the meaning was clear to every reader and
different for one writer, and no reader could see the writer.

**The type 0014 declined already existed.** `kernel/src/field.rs` has defined
`WorldTime { day: f64 }` — "fractional days since world genesis" — throughout,
used **445 times** by fields, phenomena and observers. The fact envelope was
the one time-carrying surface in the kernel that opted out. The churn 0014
weighed was never the cost of *introducing* a type; it was the cost of making
one surface agree with the rest of the kernel.

**Two properties the retype had to preserve, and did.**

- **Quantization stays on the inner `f64`.** `Ledger::commit` quantizes days at
  the emit boundary (decision 0033); the retype rebuilds the `WorldTime` around
  the quantized value rather than dropping the step. Dropping it would have
  reddened nothing on one platform and violated the constitution.
- **The wire shape stays a bare JSON number.** `#[serde(transparent)]` is what
  holds that, and it is now asserted directly (`WorldTime` must serialize as
  `12.25`, not `{"day":12.25}`) rather than inferred from a round-trip test
  that cannot tell the two apart.

**The guarantee's boundary, stated because overstating it would repeat the
original defect.** `WorldTime` guarantees that **no non-finite value can be
constructed in Rust**. It does **not** guarantee that no non-finite value can
be **deserialized**: `#[serde(transparent)]`'s derived `Deserialize` never
calls the validating constructor. That path is closed today only because
serde_json is the sole deserializer the dependency allowlist admits (decision
0004) and it rejects out-of-range numbers outright — `1e999` is "number out of
range", and `NaN`/`Infinity` are not JSON literals. Admit any non-JSON format
and `WorldTime` silently accepts infinity, with `Ledger::check`'s non-finite-day
branch — removed as dead code by this retype, correctly, given the constructor
— no longer standing behind it. A type-safety claim actually held by a
dependency's parser is precisely the layered agreement that let the original
defect survive four green layers, and it must not be filed as a property of the
type.

**Consequence.**

- Every `Fact.day` write goes through `WorldTime::new`, and every read through
  `WorldTime::day()`. Validation is **finiteness only**: a day is a point on an
  axis and goes negative legitimately (founders are born before the history
  record begins), so `Years`'s non-negative rule is deliberately not copied.
- Three `type-audit: waiver(decision-0014: …)` tags survive on fields that are
  *not* `Fact.day` — `PersonSeed`'s three day-shaped fields, `Founder.founded`,
  and `SessionSnapshot.day`, the last being the `vessel/session/v2` wire schema.
  They are re-pointed at this record. Each stays bare because it is a
  pre-commit or wire quantity that funnels into a `Fact.day` later, not because
  the envelope's timestamp is untyped.
- Making the type enforce is what forced the *unit* repair beside it: with the
  slot typed, the history bake's year-valued stamps had to cross a named
  boundary, and `person-died` became reachable with no change to its own logic
  (0 → 144 facts on seed 42).

**See also.** Decision 0014 (superseded); 0033 (quantize at the emit boundary);
0004 (the dependency allowlist, which this record's boundary claim depends on);
0008 / 0044 (typed quantities, and where they live); 0028 (the `bare-ok`
rubric, whose waiver examples cite 0014); `book/src/chronicle/the-ell.md`.
