# 0064. Potency is Challenge Rating over thirty, over the supernatural set

**Status:** Accepted (2026-07-21) · **Decider:** Nathan

In the context of [The Assay](../../book/src/chronicle/the-assay.md) — the
warm-up campaign of the Dragons program — we decided that
`BiosphereTraits.potency` (a creature's magical might, the `b·potency` term in
`kernel::sovereignty_floor`) is **authored as the creature's 5E adult Challenge
Rating divided by thirty (`CR/30`)**, nonzero only for the **supernatural set**
(kinds with innate supernatural might — dragon, plant/fey, or elemental);
mundane beasts, monstrosities, minions, and the four peoples carry `0.0`.

This replaces the freehand values shipped with the biosphere in *The Menagerie*
(red `0.95`, white/black `0.85`, treant `0.6`, xorn `0.5`), which had no source
and mis-ranked the chromatics (white == black, all three near the ceiling).

**The rule.**
- `potency = CR / 30`, `CR` taken from the 5E Monster Manual adult stat block.
  30 is CR's published ceiling (the tarrasque), so the full 5E range maps onto
  `[0, 1]` with headroom above adults for ancient wyrms and deities.
- Shipped values: red-dragon `17/30`, black-dragon `14/30`, white-dragon
  `13/30`, treant `9/30`, xorn `5/30`; every other roster kind `0.0`.
- **CR is an offline authoring input, not runtime state.** No `challenge_rating`
  field is added to the physical model and no CR→potency function runs at
  generation time; the literal is written as a self-documenting `N.0 / 30.0`
  expression with a comment citing the CR ("models author, dice roll" —
  decision 0009). The division is IEEE-exact, so quantize-at-emit (decision
  0033) is unaffected.

**Why CR, and why the supernatural set only.** `mass` already carries a
creature's physical formidability (and is already 5E canon). `potency` is the
*magical* residual on top of the body, so it is sourced from the one scalar 5E
publishes as overall menace (CR) but applied only where that menace is
supernatural — a mundane bruiser's high CR is body, not magic, and must not
double-count into the sovereignty floor. Hit dice were rejected (they track
size/toughness, correlating hard with `mass`); a mass-regressed "magical
residual" was rejected as larger than a warm-up warrants (a possible later
refinement). The one borderline, **otyugh** (aberration with minor telepathy),
is `0.0`: telepathy is a sense, not might.

**Consequence.** No committed artifact changes: the mighty menagerie is unplaced
in every shipped world, and `sovereignty_floor` only bites at placement, so no
census regen was required (verified: `git diff` over the generated pages clean).
The rule is **age-ladder-ready** — a later campaign's wyrmling→ancient
categories slot their own CRs into the same `CR/30` with no re-derivation. The
outcome is pinned by `domains/species/tests/potency_assay.rs` (each value's
CR/30 and the red>black>white>treant>xorn>0 order).

**Scope.** This settles the *sourcing of the potency value* only. The Dragons
program's architectural reframe — dissolving the peopled-cluster tier so mind,
perception, and language compose freely onto solitary creatures (idea-registry
UNI-31), and the sociality-governed language regime (BIO-37) — is deferred to
that program's later campaigns and is not ratified here.

**See also.** [The Assay](../../book/src/chronicle/the-assay.md) chronicle and
its [spec](../superpowers/specs/2026-07-21-the-assay-design.md); decision 0009
(models author, dice roll); decision 0033 (quantize-at-emit); `kernel/src/ecology.rs`
(`sovereignty_floor`); `domains/species/src/lib.rs` (`biosphere_registry`).
