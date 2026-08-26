# The Reticence — design

**`PLAY-host-may-refuse`.** The Confidant gave a possessed host a voice and made
that voice unreliable in two ways it could not help: no word for the state, and
blindness to what arbitration suppressed. Both are *incapacities*. This campaign
adds the third unreliability, and it is the first one that is a **choice**: the
host knows, has the word, and decides what you get.

Autopilot engaged. Branched from main `7576eca00`.

---

## 1. Scope

You ask the body you are wearing how it feels. Whether it answers, and whether
the answer is true, depends on what its people believes you *are* and on what you
have made it do.

**In:** a per-`DriveKind` disposition toward the rider, derived from a cultural
doctrine prior and a fold over the possession's own history; three testimony
outcomes (refusal, falsehood, costly truth) as one mechanism; the instrument that
measures whether disposition actually moves testimony.

**Out:** the host volunteering unprompted (the speech budget — `NARR-delivery`
is still unmeasured and would go on the critical path). Eviction, resistance to
being ridden at all, and inheritance-into-the-soul: §7 explains why these are
named here and deliberately not built. No new `GapReason` variant (§4.4).

---

## 2. Two corrections to the campaign's stated premise

Both were found by running commands against the tree, not by reading forward from
the brainstorm. They are recorded rather than quietly fixed.

### 2.1 The stated foundation does not exist

The Confidant's spec defers this campaign on the grounds that it "needs a
disposition-toward-rider model, and that model should derive from The Cant's
layers 2-3 rather than be authored a second time here."

**The Cant's layers 2-3 are not built.** The Cant's own spec §2 states it
"delivers layer 1 only"; layers 2 (perturbation) and 3 (feedback, held claims,
racecraft) are marked `c9+` and belong to the **Myth thread**, a different
program from The Bridle. The shipped surface is one function:

```rust
// windows/sentiment/src/judgment.rs:112
pub fn snap_judgment(judger: &PeopleTraits, target: &PeopleTraits) -> Judgment
```

People x people, world-invariant. **A rider is not a people**, so even the layer
that exists has the wrong arity. `hornvale-sentiment` additionally has zero
consumers — `grep -rn "hornvale-sentiment" --include=Cargo.toml .` returns only
its own manifest.

The near-miss is worth recording because it is this project's most common defect
shape: the campaign controller first reported "prerequisite met — The Cant is
merged." That was a real command answering a **narrower question** than the claim
attached to it. Merged and layers-2-3-shipped are different questions.

### 2.2 The arc numbering has already collided

`docs/superpowers/specs/2026-08-19-the-bridle-metaplan.md:305` and
`docs/superpowers/specs/2026-08-23-the-hand-design.md:28-29` both assign
**Arc III = The Coercion** (an imposed controller; `dominated` joins the gate
table) and **Arc IV = The Offer** (objects advertise verbs). The Confidant's spec
line 3 claims "Arc III of The Bridle" for content the metaplan's arc table does
not list, and the metaplan has not been amended since 2026-08-19.

**This spec therefore claims no arc number.** It is `PLAY-host-may-refuse`. The
collision is flagged for G3 (§10) as an owner decision; renumbering silently
would invalidate two committed specs.

---

## 3. The model

### 3.1 The pipeline gains one stage

`Session::ask` (`windows/vessel/src/session.rs:4868`) today is four steps:

```
  driven_affect() -> AffectLabel
      -> lexicon_from_in(...)
      -> testify(&lexicon, label)  -> Option<FeltStateWord>
      -> render_testimony(...)     -> (turn, heard_value)
```

The Reticence inserts **willingness** between affect and testify. Nothing else in
the pipeline moves.

```
  driven_affect()      ----+
  driven_suppressed()  ----+--> disposition(host, rider) --> Stance --> testify
        (accumulated)      |         ^
                           |         |
                    doctrine prior ---+
```

### 3.2 The doctrine prior is derived from facts the world already commits

What a people believes a rider *is* is **not authored**. It is read off two things
the world already has, which keeps this campaign inside decision 0259
(conceptual deficiency is derived, not authored) and decision 0021 (prejudice
belongs to characters, not the engine — the question is never absence but
*whose*).

**Lexical coverage** separates settled from unsettled. `windows/worldgen/src/lib.rs:5938`
already steeps a settled people in `god` and `spirit`:

```rust
for concept in ["home", "hearth", "god", "spirit"] {
    if world.registry.concept(concept).is_some() {
        classes.insert(concept.to_string(), ExposureClass::Steeped);
    }
}
```

gated on `!settled.is_empty()`. A people with no settlement has neither concept
in its lexicon, so the lookup returns `LexEntry::Gap { reason }` and the doctrine
has to reach for a word the culture *does* have.

**Exactly what is shared here, stated precisely, because §6 turns on it.**
`Lexicon::entry(&self, concept: &str) -> Option<&LexEntry>`
(`domains/language/src/lexicon.rs:186`) is **concept-generic** — it takes a
`&str`, so `"god"` and `"spirit"` go through the same lookup felt states do, and
the `Root` / `Gap` split comes back unchanged. That is shared, by calling it.

`testify` is **not** shareable and this spec does not claim it: its signature is
`testify(lexicon: &Lexicon, label: AffectLabel)`, and its substitution rule is
circumplex distance over `AffectLabel` variants. There is no circumplex for what
a rider is. The doctrine's arm-selection is the `cult-form` rule below — a
*different rule answering a different question*, not a second copy of the
nearest-neighbour rule.

**`cult-form` chooses the arm among settled peoples**, and it is a clean
per-people property. Measured on seed 42 (`hornvale new --seed 42`):

```
  distinct peoples                     15
  beliefs (is-belief / held-by)       145
  peoples with MIXED cult-forms         0     <- uniform per people
  cult-form = organized                 9 peoples  (87 beliefs)
  cult-form = folk                      6 peoples  (58 beliefs)
```

Both arms are amply reachable; neither is a criterion that code satisfies and a
player never meets.

**`high-god` was considered and rejected as the discriminator**: seed 42 commits
exactly **1** `high-god` fact against 15 peoples, so a ranked-pantheon test would
have produced one live arm and one effectively dead one. This is recorded because
the rejected option was the first one that looked right.

**A negation pass corrected the axis, and the essay is the authority.** The
first draft of this section read `god` and `spirit` as *the doctrine* — an
organized cult "has" the god reading, a folk cult the spirit reading. The
frontier essay draws the line somewhere else entirely
(`book/src/frontier/frontier.md`, the possession section):

> a people with the doctrine names you correctly and knows what to
> do about it, and a people without one explains you with the words it has —
> intrusive thoughts, a haunting, a fever, a god, a wandering ancestor.

`god` is in the list of things a **doctrine-LESS** people reaches for. The real
axis is *has a rider-doctrine* vs *improvises*, and `god`/`spirit` sit entirely on
the improvising side.

**Measured: no people in Hornvale has a rider-doctrine today.** The concept
registry contains **zero** concepts for a rider, possession, haunting or soul, and
the religion domain holds exactly two concepts, `god` and `spirit`
(`hornvale concepts`). So every people in every world is currently on the
improvising side, and this campaign ships that side.

The resulting prior, in full:

```
  ANY people (today)      -> improvises; has no word for what you actually are
    no settlement         -> no god/spirit word either; reaches for a body-state
                             word it does have (gap-carrying, via LexEntry::Gap)
    settled, folk cult    -> "spirit"  (gloss: "a lesser or unseen
                             supernatural presence")
    settled, organized    -> "god"     (gloss: "a deity")

  a people WITH a rider-doctrine -> names you correctly. NOT REACHABLE TODAY;
                             requires registering a rider concept -- see 10.5
```

**The prior's sign, which the first draft omitted entirely.** Naming the arms is
not enough: a prior with no direction cannot move a stance, and the graph of this
model had an unlabelled edge from *arm* to *cooperation*. The essay supplies the
direction and it **inverts the intuitive reading** — a people with the doctrine
"knows what to do about it", so doctrine is the *less* cooperative prior, not the
more. Within the improvising side the same logic orders the arms:

```
  organized cult -> apparatus, precedent, a prescribed response  -> LESS cooperative
  folk cult      -> a word, and no machinery behind it           -> MORE cooperative
  no settlement  -> not even a word; nothing to invoke           -> MOST cooperative
```

The host that cannot name you is the one most willing to talk to you. That is
the opposite of the reading this spec started with, and it is the one the essay
and the registry both support.

### 3.3 The history fold is accumulated suppression

The Confidant already computes, every tick, the drives arbitration found *active
and did not pursue* — `WalkState::suppressed`, surfaced as
`Session::suppressed_drives`. **When the player is driving, that set is precisely
what the rider made this body ignore.**

Today it is a per-decision read, overwritten by every `advance_one` iteration and
explicitly not hysteretic (`windows/vessel/src/liveness.rs:4996-5010`). This campaign
accumulates it per `DriveKind` across the possession.

The axis you override is the axis it goes quiet on. The loop is closed and
derived; nothing about the disposition is authored.

**This makes one of The Confidant's deferred items live**, as its retrospective
predicted: `suppressed` is threaded through the live session path only, not the
stateless affect snapshot. This campaign owes that threading.

### 3.4 Granularity: per-`DriveKind`

A single scalar cannot express the requirement. The frontier essay asks for a
host who "tells the truth in a way calculated to cost you", and *calculated* is
only meaningful if the host chooses **which** truths. Per-fact granularity is
combinatorial and rejected. `DriveKind` is the granularity that is both already
indexed and causally motivated by §3.3.

### 3.5 Prior and fold are kept separate, never pre-summed

The disagreement between them is the readable output. A people whose doctrine
calls you an ancestor-spirit, ridden by someone who has overridden its thirst
eleven times, should still *call you* by the warm word and still refuse to say
where the water is. Summing the two at construction makes that unreachable.

This is **The Confidant's own shape one level up**: that campaign's deliverable
was the gap between what arbitration computed and what the host said; this one's
is the gap between what the culture says a rider is and what this rider has been.
§6 states what this campaign does about that.

---

## 4. The three behaviours are one mechanism

### 4.1 One filter with a sign

| behaviour | mechanism |
|---|---|
| refuse | no testimony on that drive's topic |
| lie | report a label the host selects |
| costly truth | accurate label, suppression **revealed** rather than withheld |

The third is the anti-purposed variant and it is the one that shows the mechanism
is single: a hostile host that answers *accurately, because accuracy is what costs
you* is not a fourth code path. It is truth with the salience ranking inverted,
drawing from the channel The Confidant cut and forbade testimony to touch.

### 4.2 A lie needs no new plumbing

`ask` already writes what the host said into the player's `Knowledge` under
`"{body}::feels"` (`session.rs:4893-4897`), deliberately outside the ground-truth
check, precisely so a told falsehood can land. That seam is built and tested.

### 4.3 A lie MUST NOT reuse `FeltStateWord::Nearest`

A lie and a lexical substitution have the same *shape* — report X when the truth
is Y — and different causes. The Confidant's `misreport_distance` compares concept
ids and **cannot distinguish them**. If a lie reuses `Nearest`, that committed
metric silently begins measuring two phenomena in one number, and its committed
artifact (`docs/audits/the-confidant-report.md`) would keep reading as a
coverage measurement while having become a mixture.

A lie gets its own `FeltStateWord` variant. The metric is updated to exclude it
in the same commit, and a test pins that the two are counted separately.

### 4.4 No new `GapReason` variant

Refusal is not "no word exists" — the word exists and the host declines it. That
is a **filter stage**, matching the precedent The Confidant set when it placed the
cognitive gap before the lexicon rather than inside it.

Consequence, stated so a later campaign does not have to rediscover it: the two
sites that re-derive the proto-root universe rule without calling the shared
helper (`cli/src/proto.rs` `render_proto`, and
`windows/worldgen/tests/suite/proto_goblinoid_golden.rs`) stay **deferred**. They
diverge the moment a `GapReason` variant is added, and this campaign adds none.

---

## 5. Preregistered measurement (decision 0016)

Frozen before the code that would move it. A falsified prediction is a finding.

**H1 — the improvising arms are well-distributed.** Over seed 42's **15**
peoples, the prior resolves to `organized` for **9** and `folk` for **6**, with
**0** peoples resolving to more than one arm, and **0** peoples resolving to the
doctrine arm (no rider concept is registered). *Criterion:* exact counts, not a
ratio. Any people resolving to two arms falsifies the uniformity claim in §3.2;
any people resolving to the doctrine arm means a rider concept was registered
after all, and §10.4 was decided without this line being updated.

**H2 — the fold discriminates per-drive.** After the rider overrides drive *d*
some number of times, the host's stance on *d* moves and its stance on every other
active drive does not. *Criterion:* mutation-proved — substituting a constant for
the accumulated count must redden the test. A test made robust by being made
weaker would pass quietly, which is the failure The Confidant caught in its own
Task 8.

**H3 — refusal is selective, not global.** A host reticent on thirst still answers
on fatigue in the same session. *Criterion:* a count of drives answered vs
refused in one session, > 0 on both sides.

**H4 — the null this campaign is prepared to report.** The doctrine prior may not
move observable testimony at all once the fold is running: the fold may dominate
every reachable case. *Criterion:* count the sessions in which prior and fold
select different stances, with the denominator stated. **If that count is zero,
that is the headline**, and it means the prior is decorative — the same finding
The Cupel reported about people-preference, and the same shape as The Cant's own
measured null (the derivable axes never admire, 0/210).

---

## 6. The duplicated-rulebook question, answered explicitly

Decision 0261 (this campaign's immediate predecessor) states that a rule
duplicated on purpose carries a two-way agreement test, and
`TOOL-duplicated-rulebook-audit` was opened three days ago to count how many
undeclared duplicates exist. §3.5 says this campaign measures a gap that is
structurally The Confidant's gap at a different arity. **That is the exact shape
those two artifacts exist to catch**, so the spec answers it rather than leaving
it to review:

**The honest answer is that one half is shared and the other half is not a
duplicate at all**, and the first draft of this spec got it wrong in the
direction that flatters the campaign — it claimed the prior reuses `testify`,
which cannot be true, because `testify` takes an `AffectLabel`. Corrected:

- **Shared, by calling it:** the lexicon gap lookup, `Lexicon::entry(concept)`.
  Concept-generic, used directly. No second copy exists or is written.
- **Not shared, and not a duplicate:** arm-selection. `testify` substitutes by
  circumplex distance between felt states; the doctrine selects by `cult-form`.
  These are different rules answering different questions, not two copies of one
  rule — which is the distinction 0261 actually draws.

**The live risk is therefore the gap-MEASUREMENT pattern, not the lookup.** Both
campaigns measure a divergence between a derived truth and a held belief. If
implementation finds itself writing a second divergence-measuring helper rather
than generalising The Confidant's, that is a **deliberate duplication** and it
acquires a two-way agreement test under 0261 — not a note; a test that fails from
both directions.

---

## 7. Named, and deliberately not built

Each of these reads the same disposition value and is out of scope. They are
listed so the value is designed as a **named derived quantity** rather than a
private field in `testimony.rs`, which is the difference between this campaign
enabling them and blocking them.

- **Resistance** — how hard the body is to ride at all. The essay's third
  possession gate.
- **Eviction** — a host that hates you enough expels you. Needs a threshold
  nobody has calibrated.
- **Inheritance** — what survives into the soul. The essay says this needs no
  merge semantics precisely *because* it is "what the host said and the soul
  kept", which is this campaign's output read by a later consumer.
- **The Coercion** — the metaplan's real Arc III, a controller imposed by another
  creature. It shares this campaign's axis (a body whose control is contested)
  and is unshipped. §10 flags the sequencing for Nathan.

---

## 8. Deliberately NOT authored

No authored table of what any given people thinks a rider is. No alignment axis.
No hand-tuned hostility constants. Every input is a committed fact or an existing
derived lexicon state; if a value cannot be derived, it does not ship.

---

## 9. Decisions to record

- **A host's cooperation is derived from doctrine and conduct, never authored.**
  (Extends 0259's derived-not-authored discipline from conceptual deficiency to
  social disposition.)
- **A deliberate falsehood is a distinct testimony variant from a lexical
  substitution.** (Protects `misreport_distance` from becoming a mixture; §4.3.)
- **Prior and fold are stored separately; their disagreement is the output.**
  (§3.5.)

---

## 10. Flagged for G3 — owner decisions, not autopilot's

1. **The arc collision (§2.2).** Three routes: amend the metaplan to insert the
   testimony arc and renumber The Offer; leave the metaplan and treat The
   Confidant's "Arc III" as an error to correct in its own spec; or stop
   numbering arcs. Autopilot has no precedent to resolve this — no decision
   record addresses arc renumbering.
2. **The falsified foundation (§2.1).** Recorded here as a fact; the owner may
   still want The Cant's layer 2 sequenced before more consumers are built on
   layer 1's shape.
3. **The 0261 hazard (§6).** The spec commits to sharing rather than copying. If
   the owner would rather this campaign not touch `testify`'s internals at all,
   the answer changes and §3.2 needs rework.
4. **Should this campaign register a rider concept (§3.2)?** Today zero exist, so
   the doctrine arm — a people that "names you correctly and knows what to do
   about it" — is unreachable, and the campaign ships the improvising side only.
   Registering one would make the arm live and is the natural home for
   `PLAY-host-names-you`'s other half, but it is a concept-registry addition with
   save-format reach, which is an owner call rather than autopilot's.
5. **Sequencing against The Coercion (§7).** It shares this campaign's axis and
   is unshipped. Building this one first is defensible and was Nathan's stated
   priority; it is flagged because the duplicated-rulebook risk is real and
   points the other way.

---

## 11. Definition of done (decisions 0013, 0020)

Chronicle entry, retrospective, book freshness sweep, Confidence Gradient
re-score if a bet moved, registry flips (`PLAY-host-may-refuse`,
`PLAY-host-names-you`, `PLAY-host-is-a-narrator`'s remaining two thirds),
keystone refreeze, and the merge through the queue.

**Plus, carried forward:** the flamegraph debt. `docs/timings/census-yellow-log.tsv`
row `2026-08-25T01:10:43Z` rolls the obligation forward for the second time and
asks that it be taken by someone who is not the author of the change under
suspicion. This campaign's close census satisfies that condition. If it goes
yellow, it is flamegraphed here.
