# The Vestige — retrospective

**Completed:** 2026-07-23 (slug-named per decision 0026; spec
`docs/superpowers/specs/2026-07-23-the-vestige-design.md`, plan
`docs/superpowers/plans/2026-07-23-the-vestige.md`, nine tasks: lift
occupation-reconstruction → Vestige types + people-residue → pre-human residue
+ palimpsest → `vestige_dread` field → determinism guard → residue lens →
census metrics → almanac → close). Campaign 3 of the subsurface arc, straight
after The Lode. Ran under campaign-autopilot.

**An integration layer is a different kind of campaign, and it changed where
the work lived.** The Deep and The Lode each *generated* something new (a
column, then features over it). The Vestige generated almost nothing — it
*joined* four existing systems (settlement history, the deep-time archive, The
Lode's provinces, the Dread field) and read residue out of their intersection.
The consequence for the plan: the hard tasks were not "derive X" but "reconstruct
X faithfully from what the ledger already recorded." Most of the risk moved into
Task 1 (lifting occupation reconstruction to a shared worldgen query), and once
that decoder was exactly right the rest followed cheaply. Lesson for the arc's
later integration rungs: budget the effort at the *join*, not at the new type.

**The decoder-must-match-the-encoder lift could not be delegated across the
crate boundary — and that was the right call.** Task 1 needed almanac's private
`record_of`/`layers_at` occupation decoder, but worldgen cannot depend on
almanac (the dependency runs the other way). The resolution was a *verbatim
port* into worldgen with a round-trip test pinning it to the encoder, not a
refactor to share the code. Attempting to break the Cargo cycle mid-campaign
would have been scope creep into the layering; the port plus a contract test is
the honest small move. Lesson: when the clean deduplication would invert an
architectural edge, copy-with-a-pinning-test beats the "right" refactor until a
campaign is actually chartered to move the boundary.

**Two reviews caught defects that would have survived the suite — one
architectural, one at census scale.** Task 3's review found that widening
`sphere_fbm01` from `pub(crate)` to `pub` to reach the pre-human scar gate
leaked a calibration-coupled internal across the crate boundary — green tests,
but a latent API-stability trap. The fix encapsulated the gate as
`GeneratedTerrain::prehuman_scar_at` and re-narrowed the noise function, staying
byte-identical. Task 6's review found that `vestiges_at` rescans the whole
occupation ledger *per cell* — correct, tested, and O(cells × occupations),
which would have turned the all-cell census metric into a quadratic blow-up
exactly where The Local Census had just bought the arc its cheap regen. The fix
(a batched `occupations_by_cell` + `vestiges_field`, one scan, byte-identical to
the per-cell path) was added as its own task. **Lesson: "correct and tested" is
not "cheap at scale" — a reviewer who asks the complexity question, not just the
correctness question, is what keeps an arc's census affordable.**

**Expose-not-wire is the disciplined shape for a hook.** The `vestige_dread`
field is derived and public but wired to no consumer — deliberately, resolving a
G3 flag. The alternative (wire it into the Dread/vessel path now) would have
coupled a reserved-metaphysics concept into active machinery ahead of the gate
that governs it. Building the field proves the derivation works and gives the
future rung a typed seam to consume; leaving it unwired keeps the coupling
decision with the campaign that earns it. Lesson: a hook you can *see compiled*
(a public, tested, unconsumed derivation) is worth more than a hook that is only
described in a spec, and costs nothing to leave dangling.

**The census coupling debt is now an arc-wide standing cost, as predicted.**
The Deep opened it (3 metrics), The Lode added 4, The Vestige adds 4 more — the
fixture-schema calibration tests are red at merge until the census golden is
regenerated out of band (owner-authorized carve-out, done on another machine).
The Lode retro flagged that a multi-campaign arc should plan the regen cadence
rather than accumulate silently; three campaigns in, the honest accounting is
that the arc *chose* to batch all three regens into one out-of-band pass at the
arc's convenience rather than pay per-campaign. That is a defensible cadence, but
it means each campaign merged with a known-red calibration column — recorded here
so the choice stays visible, not silent.

**Scope notes for the record.** No `open-questions.md` Confidence-Gradient bet
moved — grepping underworld/deep/lode/residue/subsurface/underground found only
incidental prose, not a tracked bet; the gradient update due is the registry
flip of MAP-10's campaign-3 line to *shipped*. Live seal-failure, the entity
behind a gate, and the full memory/upkeep valence model all stayed reserved
(the metaphysics gate and the vertical-relationship campaign, per spec §10) —
the derived-neutral-truth stance the arc has held since The Ground.
