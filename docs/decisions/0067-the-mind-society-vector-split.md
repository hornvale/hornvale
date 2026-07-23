# 0067. The mind/society vector split

**Status:** Accepted (2026-07-23) · **Decider:** Nathan

In the context of [The Cloister](../../book/src/chronicle/the-cloister.md) —
campaign 4 of the Dragons program — we decided to cleave the species crate's
six-dimension `PsychVector` into two closed vectors: the psychology every minded
creature has, and the psychology only a society has.

**The split.**
- `hornvale_species::MindVector { threat_response, deliberation_latency,
  time_horizon }` — the individual-mind vector, carried by **every** minded kind
  (the four settling peoples and the three solitary dragons).
- `hornvale_species::SocietyVector { sociality, status_basis, in_group_radius }`
  — the community-mind vector, carried **only** by `Settled` kinds, with
  `const fn baseline()` returning the goblin-authored society reading
  (`Hierarchic`, `Rank`, 0.5).

The dimension assignment follows the existing consumer seam: the vessel (the
game-liveness / individual-NPC layer) reads *exclusively* the three mind dims,
while the society dims are read only by the chorus (collective belief/authority)
and the language register. A solitary dragon, driven by the vessel, needs only
its mind vector; the split makes the type state what the code already did.

**The mechanism.**
- `psyche_registry()` keeps its name and now returns the `MindVector` store for
  all seven minded kinds; a new `society_registry()` returns the `SocietyVector`
  store for the four peoples. `WorldComponents` gains a `society` store beside
  `psyche` (mirroring `perception`).
- The three dragons' authored society dims (`Hierarchic`/`Rank`/`in_group_radius`
  0.05) are **deleted, not migrated** — a solitary asserts no society shape.
- The **mixed consumers** that read both a mind dim and a society dim
  (`voice_params`, `chorus` stance, the settlement `PsychSummary` projections)
  take both vectors; the ~3 all-minded CLI catalogue sites (`phonology`, `audio`)
  that reach a dragon resolve `society_registry().get(kind).unwrap_or(baseline())`.
  Every worldgen/chorus consumer runs over Settled peoples, so it looks the
  society vector up directly (always present) — the baseline fallback is scoped
  only to the dragon-reachable catalogue path. This is deliberately the same
  fallback shape the coming Perception campaign will use for the goblin-baseline
  exposure stopgap.
- `check_integrity` gains one clause: **`society` key-set == `Settled` key-set**
  — every settling kind carries a society vector, no non-settling kind does. It
  sits orthogonal to The Eremite's nested-capacity lattice (speech ⊆ perception
  ⊆ mind): *to have a society, one must settle.*

**Determinism.** **Byte-identical, not an epoch.** No new seeded draws; no stream
label touched; the society facts reuse the same predicate constants
(`IN_GROUP_RADIUS` / `SOCIALITY_MODE` / `STATUS_BASIS`), only their source moved.
The four peoples keep the same six values split across two homes, so every placed
reading and emitted fact is unchanged; the world psyche-fact emission is already
`Settled`-gated, so dragons dropping their society dims moves zero world bytes.
The reference catalogue is byte-identical too — the only catalogue consumers of a
dragon's psyche (`voice_params`, `morph_options`) read only `status_basis` and
`sociality` among the society dims, and the dragons' authored `Rank`/`Hierarchic`
already equal `baseline()`; the one dim that differs (`in_group_radius` 0.05) is
read by no unplaced-path consumer. **Verified, not asserted:** the reference
pages and the three seed-42 almanacs were regenerated and found identical; the
census was untouched (no regen).

**Scope.** This settles the mind/society separation and the solitary mind's
freedom from a society it does not keep. **Deferred:** dragon perception (next);
placing the menagerie (the landscape-of-fear cascade; BIO-35 Stage 2); a reduced
society vector for Gregarious (pack) minds; per-chromatic dragon differentiation.

**See also.** [The Cloister](../../book/src/chronicle/the-cloister.md) chronicle
and its [spec](../superpowers/specs/2026-07-22-the-cloister-mind-society-split-design.md);
decisions 0064 (potency = CR/30), 0065 (SocialForm + the solitary mind), and 0066
(drift = f(sociality × lifespan)), the program's earlier campaigns;
`domains/species/src/lib.rs` (`MindVector`, `SocietyVector`, `society_registry`);
`windows/worldgen/src/components.rs` (`check_integrity`).
