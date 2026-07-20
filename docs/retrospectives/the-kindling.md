# The Kindling — retrospective

One page, process not product. The Kindling wired the species domain's
`MetabolicClass` into the drive layer: a metabolism gate (ametabolic → no
drives) and a temperature-coupled thirst (a per-class path integral), realizing
the water-facing half of the species campaign's deferred CAP-1.

## What worked

- **A scoping question reframed the whole campaign — and it was the right
  altitude.** Started as followup #1, "heat quickens thirst," and the plan was a
  universal coupling curve bolted onto the flat drive. Nathan's "zoomed-out"
  question — *how do non-metabolizing creatures interact, and is there per-drive
  control?* — sent the search into the species domain, which already authored
  `MetabolicClass` (including an ametabolic case) and had *already spec'd* the
  ectotherm temperature coupling as a parked cap. The bolt-on became a grounded
  realization of an existing model. **Lesson: when a feature feels like a hack,
  check whether the substrate already models it; grounding beats inventing, and
  a good scoping question from the owner is worth more than a page of design.**

- **The tick/read consistency solved itself by construction.** A stateful mover
  (the drive tick) and a stateless read (`affect_of`) must compute the same
  path-integral thirst or a creature acts on one value and is narrated with
  another. Rather than maintain an incremental accrual in the tick and align it
  to a re-derived read (the fiddly discipline `believed_water` needed), both
  call ONE shared fold, re-derived over the committed history (`frozen + out`)
  each time — identical inputs, identical result. **Lesson: when a mover and a
  read must agree, make both fold the same committed data; don't keep two
  representations in sync.**

- **Free byte-identity via the degenerate case.** The coupling was designed to
  collapse *exactly* to the old flat rate at a thermoneutral or unreadable
  temperature. Because the tested worlds and settlements sit at temperate cells,
  the entire existing suite (84 vessel tests, the health null-control) stayed
  green with zero re-pinning — the change is inert where it was already correct.
  **Lesson: design a new mechanism to degenerate exactly to the old one at the
  old operating point, and the regression surface shrinks to nothing.**

## What to watch

- **"Followup" is a size estimate, not a contract.** #5/#4/#2 landed as clean
  single commits; #1 grew into a spec'd campaign once its scope expanded. That
  promotion (write the spec, take the G3 stop) was correct, not overhead —
  forcing it to stay "a followup" would have skipped the design conversation
  that found the metabolic grounding.

- **A batch text-edit over-matched.** Inserting `home`/`terrain` locals after
  every `let p = SUSTENANCE;` hit a non-`drive_at` test; clippy caught the unused
  vars. Cheap, but a reminder to scope `perl -0pi` matches to the intended set
  or verify the match count.

## Followups (captured to spec + registry)

- The **allometric CAP-1** (energetic basal-rate coupling in `allometry.rs`) —
  this campaign realized only the water-drive half.
- **Autotroph coupling** (transpiration + a light-hunger "bask" drive).
- **Ametabolic creatures' own drives** — construct integrity, undead
  hunger-for-life, and the *inverted* elemental fed by its element (Nathan's
  fire-elemental question) — the per-kind drive-authoring layer this gate opens.
- Exact segment-start temperature sampling in the Hold-jump (a negligible,
  deterministic approximation today — ledger #6).
- The `arbitrate` `Disposition`/`MindState` struct tidy (the too-many-arguments
  smell, now two campaigns deep).

## Confidence Gradient

Checked `book/src/open-questions.md` for a moved bet: The Kindling sits in the
cognition/drive layer, beneath the world-generation bets that chapter scores,
and moves none of them (same scope note as [The Temperament](../../book/src/chronicle/the-temperament.md)'s
health metric). No re-score.
