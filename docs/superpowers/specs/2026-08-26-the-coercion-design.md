# The Coercion — design

**Arc III of The Bridle.** The Hand made the driver a parameter: `Session {
bodies, driven: usize }`, and said in its own spec that `driven` "generalises to
a controller map in Arc III without the body type changing." This campaign is
that generalisation — a body driven by someone other than the player, and a gate
that refuses the player's in-character acts while it lasts.

Autopilot engaged. Branched from main `793308584`.

---

## 1. Scope

Someone else takes your body. You are still there, watching. Your in-character
verbs refuse; your out-of-character verbs work; and the facts the body commits
are indistinguishable from the ones it would have committed on its own.

**In:** `possessed-by` / `possession-ended` as committed facts; a
`BodyState::PossessedByAnother` gate row; an imposed controller; release at the
possessor's option; the vocabulary boundary that separates possession from
charm and command.

**Out:** which creatures can do it (§5 of the metaplan defers the biology to a
species-domain campaign); mortality, so the death terminator ships unreachable
(§6); charm, command, and any general `Control` abstraction (§4); a cost to the
possessor (§7).

---

## 2. Vocabulary, settled first because it shaped everything after it

### 2.1 One word: `possess`

The metaplan's gate table calls this row `dominated`. **Nothing shipped uses
that word in this sense** — every `dominat*` in the tree is unrelated ("dominant
species", "SYS-dominated", "dominates the night sky"). Measured against the
incumbent:

```
  root      shipped code    prose (book + docs)
  possess          747                    1642
  ridden            41                      46    <- doc comments only
  rider             32                      97    <- doc comments only
  usurp              0                       0
```

Player-possession and possession-by-another are **the same mechanism**: both
replace a body's decision procedure with an external agent's, both leave the
body co-present with its own drives and affect (decision 0226), and both make the
acts the body's rather than the driver's (decision 0168). The only difference is
who the external agent is. Introducing a second word would be the
convergent-emergence accretion `cli/tests/suite/lexicon_guard.rs` exists to
prevent, arriving from the other direction.

`usurp` was proposed during the brainstorm and rejected on this evidence. It is
recorded here because the proposal came from the same session that had just
objected to `dominated`, which is the point: the count settled it and the
reasoning did not.

### 2.2 `possess` is reserved for body-control; the have-a-thing sense is `carried`

There is no inventory subsystem today and no `possess` in the have/own sense
anywhere in the tree, so this clash is **latent, not live**. It is settled now
because it costs one sentence now and a rename later: "possessions" is exactly
the word an inventory campaign's authors will each reach for independently,
which is how `cell` reached 447 identifiers and 16,201 occurrences before anyone
decided.

### 2.3 The row name states its relation

`BodyState::PossessedByAnother`, not `Possessed`. The flat name would be wrong:
the gate would refuse *your* in-character acts on a body *you* possess. This
state is **relational** in a way every other row is not — `Awake`, `Asleep`,
`unconscious`, `dead`, `blind` are all true of the body regardless of who asks.
The derivation therefore takes the asker, and the variant name says so rather
than hiding it behind a short label that reads as "has any possessor."

---

## 3. The model

### 3.1 An open/close fact pair

The ledger is append-only; facts are never retracted. Sleep — the gate's only
existing row — sidesteps this by being **derived** (`next_awake_day` vs
`self.day`, `windows/vessel/src/session.rs:1910`, `fn body_state`), so it self-terminates. A
committed possession fact does not, and both of this campaign's terminators are
**events** rather than schedules, so a self-terminating span cannot serve.

Repo precedent exists and this is not an invention: occupations use
`occ-founded` / `occ-ended` / `occ-ended-by`, and `Ledger::latest_value_of`
(`kernel/src/ledger.rs:511`) is the fold primitive.

```
  possessed-by       subject = the body's entity    object = Entity(possessor)
  possession-ended   subject = the body's entity    object = Text(reason)

  possessed-by-another(body, asker)  iff
      the latest `possessed-by` for `body` is not followed by a
      `possession-ended`, AND its object is not `asker`
```

**Both predicates are new registry entries**, which is save-format reach. See §8.

### 3.2 The gate row

`gate.rs` is 82 lines and already carries the tripwire the metaplan promised:
`body_state_variants_must_all_be_rostered` is an exhaustive match with **no
wildcard arm**, so adding a variant fails to compile until `BodyState::all` and
`verdict` are both revisited. Adding the row is therefore compiler-forced, not
remembered.

```
                          IC          OOC
  Awake                   permitted   permitted
  Asleep                  refused     permitted
  PossessedByAnother      refused     permitted    <- this arc
```

The refusal message names the condition without naming the possessor: the body
does not know who holds it (§3.4).

### 3.3 Derived, not stored

`BodyState` is computed, never held on `Session` — `session.rs:1910`'s `fn body_state` derives
`Asleep` today. `PossessedByAnother` derives from the §3.1 fold. No new session
field, nothing to keep in sync, and a reload re-derives the same answer because
the facts are in the ledger.

### 3.4 The ledger cannot tell — which is a consequence, not new work

Metaplan §3.6 says every act stamps its provenance, and the arc's acceptance
test says the ledger cannot tell. These do not conflict: §3.6 distinguishes
**mood** (in- vs out-of-character, a property of the request), while the
acceptance test is about the **driver**. Decision 0168 already settled the
driver half — "an act's facts and time cost are determined by the body
performing it and by nothing else; a driver selects *which* act, it never alters
*what the act does*."

So the `agent-at` trail of a possessed body is byte-identical to one it would
have produced freely. What the ledger records is the **imposition**, not the
acts — exactly as a conservatorship register records the grant and not each
signature.

---

## 4. Similar to, and mechanically distinct from, charm and command

This section exists to stop a later campaign reaching for the wrong subsystem,
which is cheaper to prevent than to unpick.

```
  mechanism           does what to the             subsystem                gate row?
                      decision procedure
  -----------------   -------------------------   ----------------------   ---------
  possession          REPLACES it                 controller stack         yes
  control undead      REPLACES it (nothing to     controller stack         yes
                      modify: no drives at all)
  command             CONSTRAINS its outputs      gate table               yes, partial
  charm, suggestion   MODIFIES its inputs         drive valuations         NO
  persuasion, hire    MODIFIES its inputs,        valuations + a           no
                      with consent                relationship fact
```

**Charm is not a gate concept.** A charmed creature is refused nothing — it
genuinely wants to help, because its valuations moved. Building charm in the
gate table would be the wrong subsystem, and building it in the controller stack
would be worse: a charmed creature must keep running its own arbitration, which
is precisely what the controller stack replaces.

**Control undead validates the taxonomy rather than straining it.** Socially it
resembles charm; mechanically it is possession, because an undead has no drives
to reweight — there is no procedure to modify, only one to supply.

**No general `Control` trait.** The three mechanisms share a description and not
an implementation; unifying them would force charm into the controller stack.
YAGNI, and actively wrong.

**The independent social axis**, from the frontier essay's own enumeration
(command, persuasion, deception, coercion, hire, inheritance, institution):
possession "requires no consent and leaves no relationship behind". Charm is the
interesting cell — no consent, but it leaves a relationship the subject believes
in and the charmer knows is false, which is The Reticence's testimony/truth gap
at social scale. Noted, not built.

---

## 5. The imposition seam is out-of-character

No creature can possess another today, and metaplan §5 defers that deliberately:
"Arc III ships the *mechanism* of an imposed controller. Which creatures possess
which, and the biology behind it, is a species-domain question for a later
campaign." The acceptance test's aboleth does not exist in
`domains/species/src/lib.rs`.

So possession is imposed through an out-of-character verb, which the metaplan
independently argues for: "If a mind flayer takes your body, IC commands refuse;
that is the point. But you are still there, watching. **OOC is what lets you
observe your own domination.**" The observation channel and the imposition seam
are the same channel, for this arc only.

---

## 6. The death terminator ships unreachable, deliberately

Possession ends at the possessed creature's death or at the possessor's option.
**Release is reachable; death is not.** `Body`
(`windows/vessel/src/body.rs:15`) has no life state, nothing in
`windows/vessel/` models mortality, and the only death in the tree is
`death_day` in `windows/worldgen/src/person_promote.rs` — historical founders in
the deep-history bake, not live session bodies.

The `possession-ended` reason is therefore `Text("released")` in every reachable
path. The `"died"` arm is written, unreachable, and asserted to be unreachable
by §7's H2, so the day mortality ships the arm is already correct and the
assertion turns red rather than the behaviour going silently wrong.

This is the same shape The Reticence used for its doctrine arm, and the same
discipline: an acceptance test naming a noun is asserting that noun exists.

**A fixed time limit is explicitly NOT this campaign's model.** An inverse-power
duration — a stronger creature harder to hold — is a good idea and is deferred
to a registry row, not built here.

---

## 7. Preregistered measurement (decision 0016)

Frozen before the code. A falsified prediction is a finding.

**H1 — the gate refuses IC and permits OOC while possessed by another.** Over
the full `BodyState` x `Mood` cross product, `verdict` returns `Refused` for
exactly one new pair and `Permitted` for the other. *Criterion:* the existing
cross-product sweep in `gate_table.rs` extended to the new row, mutation-proved —
flipping the new arm's verdict must redden it.

**H2 — the death terminator is unreachable.** No sequence of currently-shipped
verbs produces a `possession-ended` whose reason is `"died"`. *Criterion:* a
count over the shipped verb roster, asserted zero, with the roster size stated.
**This assertion is designed to turn red when mortality ships**, which is the
point of writing it now.

**H3 — the act trail is indistinguishable.** A body driven by an imposed
controller and the same body driven by its own default controller, over the same
tick span from the same seed, commit facts that differ only in *which* acts were
chosen, never in their shape, cost, or subject. *Criterion:* byte comparison of
the committed fact stream's non-choice fields.

**H4 — the null this campaign is prepared to report.** A possessed body may be
unable to reach any state a free body could not, making the whole condition
observationally empty from the ledger's side. *Criterion:* count the distinct
fact shapes emitted under imposition versus free running, over a stated
denominator. **If the sets are identical, that is the headline** — it would mean
possession is invisible not merely in provenance but in consequence, which is a
stronger claim than §3.4 makes and would want its own decision record.

---

## 8. Decisions to record

- **Possession by another is the same mechanism as possession by the player**,
  and carries the same word. (§2.1.)
- **Possession replaces a decision procedure; command constrains its outputs;
  charm modifies its inputs.** Three mechanisms, three subsystems, and charm is
  not a gate concept. (§4.)
- **A possession state is relational, not intrinsic**, and the gate derivation
  takes the asker. (§2.3.)

---

## 9. Flagged for G3 — owner decisions, not autopilot's

1. **Two new registry predicates** (`possessed-by`, `possession-ended`) — save-format
   reach. Leading the flags because that is where a mistake is permanent.
2. **The arc's name.** Under §4's taxonomy, *coercion* describes the **command**
   row — making someone choose to act — while this arc removes the choosing.
   The metaplan names both the row and the arc; renaming either is one decision
   record. "The Coercion" may be the wrong name for what this ships.
3. **`rider` / `ridden` already exist in shipped doc comments** as the narrative
   agent-noun for a possessor, including in The Reticence's generated artifact.
   That is a register split rather than a rival mechanism word, and this campaign
   does not touch it — but the field is not as clean as §2.1 implies, and the
   owner should decide whether it stays.
4. **H4 could make §3.4 obsolete.** If the null fires, possession is invisible in
   consequence and not merely in provenance, which is a stronger claim than this
   spec makes anywhere.

---

## 10. Definition of done (decisions 0013, 0020)

Chronicle entry, retrospective, book freshness sweep, Confidence Gradient
re-score if a bet moved, registry flips, keystone refreeze, and the merge through
the queue. Plus the metaplan's own §3.7 table updated: it currently renders a
`dominated` row that this campaign does not ship.
