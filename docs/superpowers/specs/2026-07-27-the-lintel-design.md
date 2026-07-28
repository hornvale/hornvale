# The Lintel — the chamber band, and the doorway that reaches it

**Campaign:** The Lintel — campaign 1 of The Rose Window **as amended by
Amendment 2** (§1b of the metaplan; read that first, this spec builds only on
the amended program).
**Date:** 2026-07-27 · **Status:** spec, awaiting G3
**Parent:** `2026-07-25-the-rose-window-metaplan-design.md` §1b.3 (the band
notation), §1b.4 (the brief), §1b.6 (band transitions)
**Prior rungs:** The Hearth (the anchor graph), The Threshold (it went live),
The Purview (the situated chart)
**Decisions in force:** 0069 (fine position is never serialized), 0073 (epoch
granularity is declared), 0077 (zoom is path truncation), 0009 (models author,
dice roll), 0016 (studies preregister). **0075 as superseded by §1b.7** — but
this campaign does **not** exercise the supersession: it ships no solve.
**Registry:** `CLIENT-scale-bands`, `CLIENT-brief-is-the-contract`,
`CLIENT-depth-follows-content`
**Ledger:** `.superpowers/sdd/decision-ledger.md` (16 entries, nine ideonomy
passes, three overturns)

---

## 1. What this is

**The chamber band exists and can be entered.** At a built locale, `enter`
reaches a human-scale chamber; `out` returns; movement between a structure's
chambers is by named aperture. The anchors found there derive from the locale's
committed history via the brief.

It ships **no lattice, no metric line of sight, no pathfinding over cells, no
epoch, and no rendering beyond prose.** Those are The Blocking's and The Panes'.

Same shape as the two campaigns this program has already landed: substrate
first, byte-identical, nothing new *made up* — what was simulated at the wrong
scale becomes reachable at the right one.

## 2. Why it is byte-identical — the migration that isn't

Amendment 2's law 4 ("the anchor vocabulary is declared per band") has a reading
that would be expensive: *move* The Hearth's vocabulary to the chamber band. That
would move the warmth-seeking behaviour The Threshold shipped — a cold creature
crossing to a fire — and so would move NPC positions, the health battery, and the
seed-42 galleries.

**v1 refuses that reading.** It declares the admissibility axis and **freezes the
walk-band column at today's answer**: every one of the nine shipped patterns
stays admissible at the walk band. A settlement has a communal fire; a dwelling
has a hearth; both are `Hearth`. Nothing moves, and no NPC behaviour changes.

Two verified consequences:

- Neither `scripts/possession-walk.txt` nor `scripts/possession-over-time-walk.txt`
  contains `enter` or `exit`, so adding the verbs and making the refusal
  directional drifts **no committed gallery**. Extending the walk script to
  exercise descent is a deliberate, separate act (§8's artifact list).
- `new --seed 42` and every committed artifact stay byte-identical, which makes
  the health battery a *check* rather than the gate — the same posture The Hearth
  took, and the reason it could land at all.

The migration of walk-band furnishing to a coarser vocabulary (`Ford`, `Grove`,
`Clearing`) is **deferred, not forgotten**: it moves behaviour, so it earns its
own campaign and its own byte-identity argument.

## 3. The band admissibility table (normative)

```
  anchor kind   walk band   chamber band   note
  -----------   ---------   ------------   -----------------------------------
  Ground        yes         yes            every place has an open middle
  Threshold     yes         yes            the two-level seam -- now the ONLY
                                             thing that changes a band (§1b.6)
  Hearth        yes         yes            a communal fire / a dwelling's fire
  Alcove        yes         yes
  Bed           yes         yes            frozen: forbidding it at the walk
                                             band would move behaviour (§2)
  Vessel        yes         yes
  Screen        yes         yes            still shapes nothing until a solve
  Pool          yes         yes
  Log           yes         yes
```

v1 adds **no new anchor kinds.** The table's job here is to declare the axis and
to make the walk column a stated commitment rather than an accident. Chamber-only
kinds arrive with the vocabulary that needs them (The Precincts for districts,
The Blocking for anything a solve requires).

## 4. The brief, minimally

`Brief` is derived, never stored, and is the **only** input chamber derivation
may read besides the address and the seed (§1b.4). v1 populates the subset it
can use:

```
  field         source                                    role
  -----------   ---------------------------------------   --------------------
  people        the cell's alive occupation (occ-people)  reserved: materials
  function      OccupationRecord.function                 reserved: how many
                                                            chambers
  tech          OccupationRecord.tech                     reserved: whether a
                                                            cellar exists
  notability    OccupationRecord.notability               reserved: whether a
                                                            hall exists
  built         Terrain::is_built at the WALK band        USED: whether a
                                                            structure stands
                                                            here at all
  cold          Terrain::is_cold at the WALK band         USED: furnishing,
                                                            unchanged
```

**As shipped, the brief's only live consumer in chamber derivation is `built`.**
`function`, `tech` and `notability` are carried but never read: `structure_at`
draws its chamber count as `1 + draw % MAX_CHAMBERS`, blind to the brief, and
nothing derives a cellar or a hall. The three rows above are *reserved for* those
readings, not descriptions of v1 behaviour. This compounds v1's identical-prose
limitation: because the count is drawn from one distribution regardless of the
brief, a backwater hamlet and a regional seat draw their chamber counts from the
same distribution, and neither the size nor the prose of a structure yet
distinguishes them. Wiring these three is the first thing a follow-on campaign
should do, and the seam is already in place — the brief is passed to
`structure_at` today.

Fields the amendment names and v1 deliberately leaves unread — `cause`,
`ended_by`, `founded`/`ended`, `tongue`, `deity`, `peak_population`,
`stratigraphy` — are the ruin signature and the district vocabulary, and they
belong to later campaigns. The type carries them from the start so that adding a
consumer never changes the seam.

## 5. Addressing a chamber

A chamber is a **deeper `RoomAddr`**, per §1b.3's third law: an address is
identity, not shape. v1 declares one constant:

```
  CHAMBER_DEPTH_OFFSET = 9      // walk band + 9 refinements ~= 3.3 m
```

`MAX_DEPTH = 29` (`kernel/src/room.rs:19`) against a walk depth of 12 leaves
ample headroom. Which of the 4^9 descendants are chambers is **sparse** (law 1):
a structure's chamber set is a small derived list, and every other deep address
simply is not a place. Existence is a predicate, and the predicate is the brief.

**The verified footgun this must handle.** `LocaleTerrain::is_built` is "built
iff `room` packs to a room id in the injected settlement-territory set"
(`windows/vessel/src/liveness.rs:735-745`) — and that set is keyed at the **walk
band**. A chamber address is not in it, so a naive read returns `false` and a
dwelling's interior would draw *wild* patterns. Therefore: **band-aware terrain
reads truncate the address to the walk band before consulting terrain**, which is
0077's path-truncation move used downward. Asserted by test, not by convention.

## 6. The seam

```
  verb            from            to                     commits
  -------------   -------------   --------------------   -------
  enter           a built locale  the structure's         nothing
                                  threshold chamber
  enter <named>   a chamber       an adjacent chamber     nothing
  out             a chamber       the LOCALE — always,    nothing
                                  from any depth inside
  exit            any             REFUSED (coarse-ward)   nothing
```

**`out` leaves the structure, not one chamber.** An earlier draft of this table
had it step back to "the chamber it came from, or the locale"; as shipped
(`Session::leave`) it always exits to the locale, from however deep inside the
possession had got. A backward step would need a chamber trail the way `back` has
one for the walk band, and nothing yet asks for it. The eventual fix is a **named
backward aperture** (`further out`, the mirror of `further in`) with its own
trail — *not* overloading `out` to mean two different things depending on depth,
which would leave the player with no single word for "get me outside".

**Nothing commits, and that is verified rather than designed.** `Session::go`
mutates `self.agent.position` and pushes to `self.trail` without committing a
fact (`windows/vessel/src/session.rs:627`), and the session's ledger is "written
to only by `wait`'s tick (NPC `agent-at` facts). Never written back."
(`session.rs:121`). The player's position has never been a committed datum, so
descent needs no schema change, no new predicate, and no epoch — and re-entering
a chamber re-derives it identically, which is §3.1's byte-identical-by-
construction property obtained for free.

**The refusal becomes directional.** Today one constant answers both directions —
*"The grain of the world resists; that way lies another scale of things"*
(`session.rs:578-582`). Coarse-ward `exit` keeps it (possessing a settlement or a
culture stays a deferred arc); fine-ward `enter` succeeds where a structure
stands, and fails with a *physical* reason where none does.

**Movement inside a structure is by named aperture, never by compass.** Chambers
have no bearing relative to one another — that would be shape, and an address is
identity — so `enter the hall` is the idiom and `go north` is refused inside a
structure. This is §7's "APERTURES, NOT STAIRS" taken literally.

## 7. Determinism and labels

- **`room/furnishing/v1` already exists** — declared by The Hearth
  (`windows/vessel/src/streams.rs`) and made LIVE by The Threshold. It is **not**
  reused here: chamber *existence* and pattern *selection* have different blast
  radii, and 0073's whole point is to split labels by blast radius before the
  first bump. This campaign declares **`room/chambers/v1`**, which will be the
  first furnishing-family stream anything actually *draws* from — `selection`
  takes no seed today ("The Hearth's revised T4 dropped it, since v1's draw is a
  pure admissibility filter"), so the existing label currently versions a
  deterministic filter rather than a draw.
- **The pattern inventory is FROZEN, and not merely by this campaign's scope.**
  `ROOM_FURNISHING`'s own doc comment states that since The Threshold a creature
  stands at an anchor and its thermal drive reads the warmth there, so "adding or
  reordering a pattern is an EPOCH, not a tweak" — and that `selection` admits a
  pattern requiring another only once that other is present, making inventory
  ORDER load-bearing. §3's "v1 adds no new anchor kinds" is therefore a hard
  constraint, not a tidiness preference.
- The churny `room/layout/vN` label is not declared by this campaign and is not
  needed until The Blocking.
- The chamber set and each chamber's `Interior` are pure functions of
  `(brief, address, seed)`; there is no day term in v1, so a chamber does not yet
  read world-time (Amendment 1 §1a.8's requirement lands with the ruin signature,
  in a later campaign).
- No `f64` enters the chamber derivation; the existing composer is already
  integer-and-enum work, so nothing here touches quantization.

## 8. Success criteria

- **From the seed-42 possession, `enter` at a built locale reaches a chamber, and
  `out` returns** — observed in a transcript, not demonstrated in a unit test.
  This is the campaign's headline and the first time the program's descent exists
  at all.
- **Byte-identity, verified not assumed:** `new --seed 42`, the seed-42
  possession galleries, and every committed artifact are unchanged. The walk
  scripts are extended to exercise descent **in a separate, final commit**, so
  the drift is one reviewable diff whose every line is the new verbs' output.
- **A chamber address reads as built** — the §5 truncation footgun, asserted
  directly.
- **Derivation is pure:** the same `(brief, address, seed)` yields an identical
  chamber set and identical interiors, over a sweep rather than one case.
- **Sparseness holds:** the number of chambers at a structure is bounded and
  small, and a locale that is not built has none — asserted, because the failure
  mode (every deep address is a place) is silent and catastrophic.
- **The refusal is directional**, with the coarse-ward sentence unchanged
  byte-for-byte.
- **The health null-control still holds** (chronicity stays zero; every distress
  run recovers), as a check rather than a gate, since §2 keeps behaviour still.

## 9. Risks

1. **Chamber prose does not exist.** `windows/locale`'s describer is written for
   km-scale places; asking it to describe a 3 m chamber would produce "a
   fern-choked draw" indoors. v1 needs a **minimal chamber describer** reading
   the anchor graph. Whether the locale describer even *runs* at depth 21 is
   unverified and is the plan's first task, not an assumption here.
2. **Scope pressure toward The Precincts.** Districts, city structure, and the
   settlement rung are explicitly out (§10). The tell that this risk is
   materialising: a pattern inventory growing rather than a seam being fixed.
3. **The verb surface.** Parsing `enter the hall` is std-only string work (no
   clap, per the dependency allowlist) and the noun must resolve against the
   chamber's own catalogue, sharing the prose's nouns as The Purview's chart
   already does.
4. **The frozen walk-band column is a deliberate debt.** §2 defers a migration
   that will eventually move behaviour; the spec says so rather than letting a
   later session discover it as a surprise.

## 10. Out of scope — indexed, not lost

- The lattice solve, integer LOS, cell A*, the checker — **The Blocking**.
- Districts, city structure, rung 2 — **The Precincts**.
- The browser drawing any of it — **The Panes**.
- The ruin signature (`cause`, `ended_by`, age and wear) — a later campaign;
  the `Brief` type carries the fields unread so the seam does not change.
- Walk-band vocabulary migration (§2) — its own campaign, because it moves
  behaviour.
- Denser settlement (`SOC-dense-settlement`) — its own history-domain campaign.

## 11. Definition of Done

- `make gate` green; `make gate-full` before merge (a pub-boundary touch).
- Type-audit clean — every new pub-boundary primitive carries a verdict tag.
- Committed artifacts regenerated and drift-checked; the walk-script extension
  isolated in its own commit.
- Chronicle entry `book/src/chronicle/the-lintel.md`.
- Freshness sweep of the chapters this touches: the possession/game chapters, the
  room-mesh chapter (which currently says "room" for both bands), and the
  scene-protocol reference.
- Retrospective in `docs/retrospectives/the-lintel.md`.
- Registry: `CLIENT-scale-bands` and `CLIENT-brief-is-the-contract` flipped to
  `shipped` with **Where** repointed; `CLIENT-depth-follows-content` corrected
  where it still describes automatic band transitions (overturned by ledger #12).
- A decision record for the **locale/chamber/place** terminology, since the
  registry governs nothing (0031) and this is the kind of fix that silently
  regrows.
- Confidence-Gradient re-score if this moves a bet in `open-questions.md`.

## 12. Flagged for G3

1. **`room/chambers/v1` is a save-format-class label declaration**, declared now
   per 0073 rather than at its first bump. It is deliberately *not*
   `room/furnishing/v1` (which already exists and is live): chamber existence and
   pattern selection churn independently, and merging them would put a frequent
   bump inside a label whose blast radius includes every creature's thermal
   drive history.
2. **Freezing the walk-band vocabulary column** (§2) is a deliberate
   non-migration that trades correctness-of-scale for byte-identity. The
   alternative — migrate now — is a bigger campaign that moves NPC behaviour and
   makes the health battery the gate.
3. **Chamber prose is new writing, not reuse** (risk 1), and prose is the
   constitutionally primary surface, so its altitude is an owner-visible choice.
4. **No day term in v1** (§7): a chamber does not yet change across deep time,
   which Amendment 1 §1a.8 will eventually require.

## 13. Provenance

Brainstorm 2026-07-27 under autopilot, nine ideonomy passes, three overturns —
two of which corrected decisions the same session had adopted (automatic band
transitions; promotion-as-framing). Nathan supplied the macro/micro correction
that produced Amendment 2, the choice of the solved glyph lattice, the descent
decision, and the expectation of traditional line-of-sight that forced §1b.7.
Full ledger with alternatives discarded: `.superpowers/sdd/decision-ledger.md`.
Followups: `.superpowers/sdd/followups.md`.
