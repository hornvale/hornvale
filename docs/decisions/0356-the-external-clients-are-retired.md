# 0356. The external clients are retired, and scene schemas stop being cross-repo contracts

**Status:** Accepted (2026-08-27) · **Decider:** Nathan ·
**Amends:** [0055](0055-external-clients-consume-a-versioned-wasm-catalog.md)
(its mechanism stands; its premise — that there are external clients — no longer
holds) · **Relates:**
[0022](0022-sim-emits-data-clients-render.md),
[0023](0023-in-repo-clients-carry-their-own-toolchains.md),
[0188](0188-quantize-still-governs-magnitude-time-leaves-it.md) (the step this
unblocks finishing)

In the context of a campaign needing to know whether it could delete the `f64`
day fields from `scene/eclipses` rather than merely deprecate them, we decided
**the external clients (Goldengrove, the Orrery) are retired, so a scene schema
is no longer a cross-repo contract** — and accepted that the repo's own records
had been asserting the opposite, in force, for long enough to produce a wrong
review.

**Context.** [0055](0055-external-clients-consume-a-versioned-wasm-catalog.md)
established *how* an external client consumes Hornvale: through a versioned wasm
catalog built and released from this repo, never a re-implementation and never a
server. That mechanism was correct and is not in dispute. What changed is its
subject. With no external consumer, `clients/world-wasm`'s released catalog has
no reader outside this repository, and the discipline that followed from having
one — **additive-or-versioned only**, applied to `scene/system/v1`,
`scene/tiles/v1` and their siblings — is no longer load-bearing. A schema may now
be changed outright where that is the simpler design, versioned where that is
clearer, and neither choice owes anything to a consumer that does not exist.

**What this does not license.** Two things stay exactly as they were, and for
reasons this decision does not touch:

- **The repo boundary is still the determinism boundary** (0055's other half).
  Byte-identical seeded output up to and including the wasm ABI is a guarantee
  Hornvale makes to *itself*, asserted by the golden smoke test that compares
  wasm output against the native CLI for the same seed and pins. That test is
  not about external consumers and does not relax.
- **Seed labels remain permanent contracts.** The root guide pairs schema
  discipline with seed-label discipline in one sentence; only the first half
  moves. A seed label binds every world ever generated, not a client.

**Consequence, and the reason this record exists rather than a quiet edit.** A
decision that outlives its subject does not sit inert — it actively produces
wrong answers from people reading it in good faith. On 2026-08-27 the merge queue
vetted a campaign's `scene/eclipses` v1→v2 bump as *correct* precisely because
`CLAUDE.md` still described scene schemas as cross-repo contracts under an
additive-or-versioned rule; that framing made a version bump look like the
disciplined choice and hid that the change also **removed** the float fields. The
gate caught it, from a test whose doc comment argued the case the record no
longer supported. The vet should have.

**Alternatives considered.** *Edit 0055 in place* — refused; decisions are
append-only and superseding is how a reader learns the premise changed rather
than finding a record that silently disagrees with its own history. *Mark 0055
Superseded outright* — refused; its mechanism is still the right answer for any
future external client, so retiring the whole record would throw away a
conclusion in order to retract a premise. *Leave it and rely on tribal
knowledge* — refused; that is what had been happening, and it cost a review.
