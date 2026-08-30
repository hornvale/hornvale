# The Company — two creatures in a room, and what one of them holds

**Campaign:** The Company
**Date:** 2026-08-30
**Decision block:** 0496-0505
**Status:** spec, awaiting G3
**Predecessor:** [The Repertory](2026-08-30-the-repertory-design.md)

## 1. What this is for

The Repertory left `the-orange` standing DECLARED and named its beat 2 —
*the goblin holds an orange* — as the next campaign's acceptance criterion.
This campaign takes beats 1 and 2 together, because investigating beat 2
falsified beat 1.

## 2. Beat 1 was passing for the wrong reason

`the-orange`'s beat 1 reads *"two creatures share a room, and the possessing
will can tell"* and asserts `/social/0/label`. That assertion does not test
co-location. `SocialEntry`'s own doc states the channel's contract:

> membership is world truth ... a consumer must filter it ... rendering it
> unfiltered ships a cheat pane

`social` lists **every derived body in the world**. Measured, at 12 seeds x 2
targets:

| | result |
| --- | --- |
| witnesses with non-empty `social` | 24 of 24 |
| witnesses with non-empty `sensed.present` | **0 of 24** |
| after `wait 1`, `wait 5`, `wait 20`, `wait 60` at seed 42 | still 0 |

So beat 1 passes whenever any NPC exists anywhere, which is always. This is
the *narrower question than the claim* shape: a real assertion, correctly
evaluated, answering less than the description attached to it.

**Beat 1 becomes `/sensed/present/0/label`.** `SensedChannel` is documented as
"the presence-gated channel: true only while the agent stands here", and
`present` as "who else is in this room right now".

## 3. Custody belongs on `PresentEntry`

The Chattel put `carrying: Vec<CarriedEntry>` on `SelfChannel`. This campaign
adds the co-located half. It goes on **`PresentEntry`**, not `SocialEntry`,
and the precedent is stated in the code rather than chosen here —
`PresentEntry::felt` is

> a presence-gated read of another creature's interior, **which is why it
> lives here and not in `social`**

Custody is the same kind of fact. Putting it on `social` would broadcast what
every creature in the world is holding — where a key is, what a merchant
carries — to a possession that has never met them, which is a materially more
exploitable disclosure than a mood and would deepen an already-acknowledged
cheat.

`hornvale_vessel::thing::held_by(ledger, holder, day)` is already
holder-agnostic, so no new derivation is needed; the field is additive on
`vessel/session/v2` exactly as `SelfChannel::carrying` was, so **no schema
version moves**.

## 4. A witness is a QUERY, not a PIN

The Repertory recorded a witness as a concrete `(seed, target, day)`. That is
wrong for this campaign's scenes and wrong in general, for three reasons.

**It rots.** The co-located pair this campaign found is selected by
`--creature 9630022852472602626`. Entity ids are lineage-derived (The Signet),
so that number moves the first time derivation changes — silently, since a
moved id resolves to *some other* creature or to a refusal, not to an error
that names the cause.

**It contradicts a rule the corpus already enforces.** Repertory assertions
are structural and never golden strings, precisely because a golden rots under
unrelated change. A pinned entity id is a golden string wearing a witness's
clothes.

**It answers a different question.** A pin asks *does this exact world still
do X* — grounded, a regression check. A query asks *does some world do X* —
existential, a capability check. Repertory scenes are overwhelmingly the
latter.

So a scene records a **selector**. The resolved identity is an observation the
run reports, never the thing that drives it. A pin stays expressible for the
regression case; it is the exception, not the default.

## 5. The selector, and the search behind it

`hornvale scout` is the sibling genus and the naming should follow it: scout
already scans seeds for worlds satisfying **genesis pins**. This generalises
the same move to **world-state predicates**.

The v1 selector vocabulary is minimal and earned:

```
  co-located                       any body with at least one creature present
  co-located(species: X)           ... where the present creature is an X
  co-located(self: X, other: Y)    ... and the driven body is an X
```

**Cost shape, measured.** A world build is ~3.5 s; a roster is small (7
derived bodies at seed 42). So the search builds each world **once** and
checks every roster member **in-process**; doing it out-of-process multiplies
by the roster for no gain. The co-located pair at seed 42 — otyugh
`9630022852472602626` and carrion-crawler `...627`, both in room `633110509`,
each reporting `present=1` — was found by exactly this scan.

## 6. UNWITNESSED — a sixth verdict

A scene whose preconditions no world satisfies is not ABSENT. ABSENT means the
scene ran and a beat failed: a capability is missing. **UNWITNESSED means the
world never assembles the stage**, which is a different finding and points at
different work.

`the-orange` as authored — a drow **and** a goblin in one room — is genuinely
in this state, and nothing in the instrument could say so.

**The variant ships with the search that can produce it, never ahead of it.**
This is the same rule that kept `PARTIAL` out of The Repertory: a verdict no
code can produce is a declaration, which is the defect the family exists to
avoid.

## 7. What `the-orange` becomes

The scene splits, because two different things were bundled in it:

- **`two-in-a-room`** (new, `control: 40`) — the mechanism: two creatures
  share a room and the possessing will can see what one of them carries.
  Satisfiable *today* once §3 lands, at a `co-located` selector.
- **`the-orange`** (unchanged in intent) — keeps the species constraint and
  the persuasion beats. Expected **UNWITNESSED** on the species pair, which is
  a truer statement of its blocker than the current declaration.

That split is the point of the campaign: it moves `the-orange`'s stated
blocker from *"objects have no instances"* — which The Chattel made false —
to *"the world does not put a drow and a goblin in a room"*, which is a claim
about the world rather than about the program.

## 8. Non-goals

- **No verb addresses another creature.** Beats 3-7 are untouched.
- **No change to who is disclosed.** `social` keeps its unfiltered contract;
  this campaign adds nothing to it and narrows nothing in it.
- **No schema version bump.** Both fields are additive on
  `vessel/session/v2`.

## 9. Flagged for review

1. **Two regeneration paths, and the second is incomplete.** Changing emitted
   session JSON moves `windows/vessel/tests/fixtures/*.json`, which
   `make rebaseline` does **not** write — that is `make rebaseline-goldens`.
   And per a board technique from The Glasshouse, `rebaseline-goldens` is
   itself an explicit list of seven scoped invocations, so a golden outside it
   is accepted only by its own scoped command. Sequence: `make rebaseline`,
   then `make rebaseline-goldens`, then check for scoped goldens in neither.
   `clients/game/core/tests/fixtures/` is drift-checked and will move.
2. **`--creature` refused one roster member** (`13226382737635672064`,
   "hobgoblin of Kae") while accepting the other six. Unexplained; the search
   must treat a refusal as "not a witness" rather than as an error, and the
   cause is worth a look but is not this campaign's business.
3. **No determinism-contract change.** Both fields are additive; nothing in
   the compute path moves; no seed label, stream order, or hash constant is
   touched.
4. **Low confidence, no precedent either way:** whether the selector search
   belongs in the resolver (test-side) or as a CLI subcommand. A subcommand is
   reusable and inspectable by hand; a test-side search avoids adding surface.
   Deferred to the plan.
