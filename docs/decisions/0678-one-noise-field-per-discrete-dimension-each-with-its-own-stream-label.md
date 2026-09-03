# 0678. One noise field per discrete dimension, each with its own stream label

**Status:** Accepted (2026-09-02) · **Decider:** Nathan (stated as a hard
constraint) · **Relates:**
[0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md) (the emit boundary a
position-sampled field still quantizes at),
[0039](0039-epochs-replace-tiers-refine.md) (the epoch suffix such a change
would need), [The Hachure](../../book/src/chronicle/the-hachure.md)

In the context of *The Hachure* designing a coherent detail field below the
terrain mesh's ~110 km floor, we decided that **each discrete dimension of a
generated field gets its own noise field with its own `stream_labels!` entry**
— never one field feeding two axes, and never a neighbouring sample off one
field standing in for another — accepting one seed label per axis where a
single label would have done.

## The decision

- One draw per discrete dimension. Relief, aspect, wetness and openness are
  four fields, four labels.
- FBM octaves *within* one field are structure inside one dimension and are
  fine. The rule is about dimensions, not about frequencies.
- An offset sample off a neighbouring field does **not** satisfy this. An
  offset stays correlated unless it is large relative to feature size — a
  trap that passes a unit test on two sampled points and fails on a map,
  where the correlation is the visible thing.

## Why

The failure a shared field produces is not cosmetic and it is not local:
**every hollow would be damp and every rise dry, everywhere, forever.** That
reads as a law of physics nobody put in the sim — a discovered correlation,
inviting explanation, produced by an implementation shortcut. It is worse
than noise because it is legible.

The inverse case is the goal and is a different thing entirely: **one
dimension, many consumers.** A single relief field read by both the room prose
and the map means "you are standing in a hollow" and the dip drawn on the map
are *one* hollow. Built as two independent inventions they contradict each
other while each is locally right — the map says ridge, the prose says
hollow.

## Consequences

- The rule binds the deferred `MAP-coherent-detail-field` work, which is
  where it will first be built. It is recorded now, ahead of that campaign,
  because it is Nathan's ruling rather than that campaign's choice and would
  otherwise have to be rediscovered or relitigated.
- **The existing `micro_field` violates the spirit of it and is untouched for
  now.** `windows/locale/src/micro.rs` draws its four axes as *sequential*
  draws off one `LOCALE_MICRO` stream. They are independent — a stream's
  successive draws are — but they are **ordered**, so adding a fifth axis
  today shifts the existing four. Separately-labelled fields would fix that
  permanently, which is a real gain beyond coherence.
- Fixing it is an **epoch**: the four-axis draw order off `LOCALE_MICRO` is a
  documented save-format contract (`micro.rs`), `micro.wetness` is emitted to
  four vessel fixtures, two scene fixtures, three game-core fixtures and the
  gallery, and replacing sequential draws with position-sampled fields changes
  every world ever generated. Per [0039](0039-epochs-replace-tiers-refine.md)
  it is an epoch suffix and never a rename: `LOCALE_MICRO` retires into the
  streams module's retired list and four new labels are minted. The Hachure
  did **not** take that epoch — Nathan deferred it with Stage 3 — so the
  contract stands unchanged until the campaign that does.
