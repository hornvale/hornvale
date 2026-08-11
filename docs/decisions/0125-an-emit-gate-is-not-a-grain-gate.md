# 0125. An emit gate is not a grain gate — a field's doc names the condition that makes it absent

**Status:** Accepted (2026-08-11) · **Decider:** Nathan · **Relates:**
[0013](0013-definition-of-done-includes-the-book.md),
[0022](0022-sim-emits-data-clients-render.md),
[0055](0055-external-clients-consume-a-versioned-wasm-catalog.md),
[0115](0115-a-clients-mirror-may-omit-a-channel.md)

In the context of *The Grain* discovering that a client campaign had been
designed around data the simulation was already computing, facing five doc
comments that described *where a value is emitted* in the vocabulary of *what
resolution the model holds*, we decided that **a field documented as absent must
name the condition that makes it absent**, and specifically that an **emit gate**
(this producer chose not to send it here) is never to be described as a **grain
gate** (the model does not know it at this resolution) — accepting that this
places a real documentation burden on every optional field, on the grounds that
the alternative has been measured and it costs a campaign.

## The concrete cost

`SurroundsCell`'s `regime`, `temperature_c`, `moisture`, `elevation_m` and
`height_asl_m` were each documented **"fine grain, `null` when coarse"**. The
actual gate is `is_here`: `ctx.describe` runs for every cell in the producer's
build loop, computes all five, and discards them on every cell but the
observer's own. The values exist everywhere; they are *unsent*, not *unavailable*.

A client campaign read those comments and concluded the simulation could not
supply a temperature × moisture surface below grid resolution. It spec'd around
that premise, which was the exact opposite of the truth, and the campaign that
inherited the work spent its opening on a diagnosis that a correct sentence in a
doc comment would have made unnecessary. The registry row
`GRAIN-emit-gate-is-not-a-grain-gate` carries the trace.

The failure is not that the comments were vague. It is that they were **specific
about the wrong axis**. "Null when coarse" is a claim about the model's
resolution; the truth was a claim about the emitter's choice. A reader has no way
to tell a description of physics from a description of plumbing when both are
phrased as physics.

## The decision

1. **Every field that can be absent documents the condition, in the condition's
   own vocabulary.** `is_here`-gated means the doc says `is_here`. Resolution-
   limited means the doc says which resolution. Never one word for the other.
2. **Where a value is computed and discarded, the doc says so** — that is
   information a consumer acts on. It is the difference between "ask for this at
   a different zoom" and "this does not exist yet".
3. **Where the absence is structural, say what would change it.** A doc that
   names the resolution should name what a finer one would require, so the next
   reader is not left to infer whether the limit is fundamental. `color`'s own
   comment already does this well — "a finer colour would need a finer lithology,
   not a different builder" — and it is the model for the rest.
4. **A published schema carries the same obligation, one level up.** A wire field
   documented `null`-able owes its consumer the condition. Where the condition is
   a resolution rather than a gate, the *document itself* discloses it — see
   [0126](0126-disclose-a-resolution-rather-than-refine-a-field.md).

## Consequences

- The five comments were corrected, and `SurroundsCell::regime` now carries the
  full explanation with the other four pointing at it. The correction includes
  the history — that the previous wording misled a campaign — because a reader
  who knows why a comment is emphatic is less likely to trim it.
- `scene/surrounds/v2`'s reference page states the same thing for the wire, and
  names the campaign-scale cost. That is deliberate: the schema page is what a
  cross-repo consumer reads, and it is the audience with the least ability to
  check the claim against the code.
- **The rule cuts both ways, which is why it is worth ratifying rather than
  merely fixing.** `micro` is emitted on every cell precisely so that its
  presence carries no ambiguity: it is a pure function of a room's address and
  the world seed, so an `Option` could only have meant "this emitter chose not to
  say" — the ambiguity this record exists to remove. A field that is always
  derivable should not be optional on the wire.
- This does not require prose about absence on fields that are never absent. It
  binds `Option`, `null`, and `skip_serializing_if`.

## See also

`The Grain` spec §4 and its [chronicle](../../book/src/chronicle/the-grain.md);
`SurroundsCell::regime` in `windows/scene/src/surrounds.rs`; the "Epistemic
state, and the grain that follows from it" section of
[`scene-surrounds-v2.md`](../../book/src/reference/scene-surrounds-v2.md).
