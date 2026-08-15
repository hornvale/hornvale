# The Compendium — a corpus family for game-system capability, read backwards

**Status:** spec, awaiting G3 review · **Campaign:** The Compendium ·
**Date:** 2026-08-15 · **Branch:** `campaign/the-compendium`

Hornvale scores itself against an external catalogue of *dramatic situations*
(`tropes/`, decision 0095). This campaign opens a second family that scores it
against external catalogues of *game-system capability* — beginning with
Herbert Wolverson's *Roguelike Tutorial — In Rust*, and built from the first
commit to take NetHack, ADoM and CDDA as further columns.

The instrument's whole design turns on one distinction the trope family never
needed: **an unmet capability is not one fact but three.** It may be
deliberately refused by a ratified decision, planned but unbuilt, or genuinely
nobody's yet. An instrument that cannot tell them apart reports a deficiency
list that is mostly false.

---

## 1. What this campaign produces

1. `systems/wolverson-2021.system.json` — a frozen, provenance-stamped corpus:
   every chapter of the tutorial, each carrying a verdict and an anchor.
2. `cli/src/systems.rs` + `hornvale systems report|check|matrix` — the resolver.
   Corpus is **data**, resolver is **code** (decision 0011).
3. `docs/audits/system-coverage-wolverson-2021.md` and
   `docs/audits/system-matrix.md` — committed, drift-checked artifacts.
4. `cli/tests/system_coverage.rs` — the byte-ratchet, running in nextest and so
   in the gate ladder with no Makefile change — see the note below on *which*
   gate.
5. A chronicle entry (decision 0013) carrying the reading, and idea-registry
   rows for every hole the matrix surfaces.

## 2. Scope

**In.** One corpus, all four numbered sections of the tutorial. The corpus
format and the matrix renderer handle N columns from day one — decision 0095
makes a single column structurally unable to support any comparative claim, so
a format that cannot take a second corpus is the thing this campaign would most
regret.

**Out, deliberately.**

- **Authoring a second corpus.** NetHack/ADoM/CDDA are the point of the
  exercise and are not this campaign. Shipping the format with one honest
  column is the deliverable.
- **`MAP-spell-corpus`.** Its own registry row draws the line: a published
  spell list is an instrument for *parameter calibration* — "how many named
  units a satisfying space carries, and at what composition depth" — and
  explicitly "not coverage audit". Same 0095 discipline, different job,
  different output type. **Do not merge these two families.** One resolver with
  two incompatible reading modes is the failure this sentence exists to
  prevent.
- **Building anything the matrix reveals.** This campaign ships the instrument
  and one reading, not the fixes. In particular it does not build the action
  clock, colour, or combat.
- **Executable per-chapter probes.** See §9.

## 3. Why a new family and not a third trope corpus

The trope resolver scores requirement bundles against the **concept registry** —
predicates, concepts and phenomena, which is the sim's ledger vocabulary. That
substrate cannot carry this corpus.

A substantial share of the tutorial is renderer work: colour, user interface,
particle effects, bloodstains, text layers, a REX Paint menu. None of it has a
concept-registry token and none of it ever will, because **decision 0022 puts
rendering outside the ledger on purpose.** Resolved against the registry, every
one of those chapters would read *blocked by a dangling bundle* — the matrix's
own † mark, meaning "a catalogue asked for something no catalogue declares".
The number produced would be plausible, would be near zero, and would be a
category error.

> **Amended after scoring (Task 6).** The paragraph above originally opened
> "roughly a third of the tutorial is renderer work" — a pre-measurement
> estimate that was never re-derived once verdicts existed. The measured
> figure is **8 of 74 items refused under 0022 (11%)**; 11 items (15%) mention
> 0022 at all. The argument is unaffected — eight chapters that can never
> carry a registry token is decisive on its own — but the fraction was wrong
> and had been copied into a decision record, the chronicle and `CLAUDE.md`
> before anyone re-derived it. Corrected in all four places; recorded here
> rather than silently overwritten, since this is the source the copies came
> from.

The two families also measure different subjects. `tropes/` asks whether a
**world** can represent a situation. `systems/` asks whether a **program**
implements a capability. Sharing a resolver would force one of those questions
to be asked in the other's vocabulary.

**The lineage worth recording**, because it produced the design rather than
decorating it: stripped of proper nouns, this instrument is *a frozen external
checklist resolved against a living internal vocabulary, where each unmet item
is classified by why it is unmet.* The mature instance of that shape in another
domain is a **compliance control matrix** (SOC 2, ISO 27001), and that domain
has already learned the two things this design turns on — that the load-bearing
artifact is never the score but the **justification attached to every "not
applicable"**, and that a hand-maintained control matrix does not go stale so
much as begin to **lie**, which is why mature frameworks separate the frozen
control catalogue from the justified applicability statement from the
machine-collected evidence. §4 and §5 are that separation.

## 4. Five verdicts

```
  present       Hornvale does this           -> cites a MECHANISM anchor
  refused       Hornvale deliberately won't  -> cites a DECISION anchor
  deferred      planned, not built           -> cites a REGISTRY anchor
  absent        genuine hole, nothing claimed-> cites NOTHING (the honest red)
  inapplicable  about bracket-lib or tooling -> cites a REASON (0095's third)
```

Decision 0095 ratifies three verdicts and states the reason plainly: without
`inapplicable(reason)`, "every difference between Hornvale and a
nineteenth-century French dramaturgical taxonomy reads as a deficiency. Georges
Polti is not owed a world." Applied to a tutorial, the same argument demands two
further splits, because the *reasons* an unmet chapter is unmet are themselves
load-bearing.

`refused` is the verdict the trope family never needed and this one cannot work
without. Chapter 2.6, *Dealing Damage*, is not a gap: decision 0070 rules that
"no stored, mutable health value may exist anywhere" and closes by ordering
combat explicitly after it — "combat built first would invent a counter." An
instrument that files that as *missing* has misread a ratified refusal as a
deficiency.

This is not a hypothetical failure. It is The Repertoire's own Critical review
finding: a table that listed seven capabilities the world already had under a
heading reading *missing*. Decision 0095 cites it as the argument for why the
artifact misrepresenting the backlog is a distinct risk from the measurement
being wrong.

## 5. The anchor discipline

Every verdict except `absent` must cite an anchor, and **the resolver verifies
the anchor still resolves.** All three kinds are machine-checkable with
machinery that already exists:

| anchor | form | checked against |
|---|---|---|
| decision | `decision:0070` | membership in `docs/digest/decisions-in-force.md` |
| registry | `registry:CLIENT-action-clock` | a parsed row ID in `book/src/frontier/idea-registry.md` |
| mechanism | `test:<crate>::<name>` (preferred) or `path:<file>` | see below |

A mechanism anchor is resolved **without building**: `path:` is a filesystem
check, and `test:` greps the named crate's sources for a `fn <name>`
definition. That is cheap and keeps the ratchet inside the ordinary gate, at
the cost of not proving the test passes — an honest weakness, and part of why
§7 calls `present` the least-entitled verdict. Resolution must not shell out to
`cargo`; the ratchet test would then build inside a build.

**Two anchor kinds read generated artifacts, and that is a dependency worth
naming.** `decisions-in-force.md` and the idea registry are themselves
drift-checked (`docs/digest/` and the registry's own `docs_consistency`
checks), so a stale digest fails the same gate run that would mis-resolve an
anchor against it. The failure mode is therefore *loud and simultaneous*, not
silent — but a resolver run outside the gate, against an unregenerated digest,
can report a false DANGLING. The report prints the digest's git blob hash so a
confusing red is traceable.

Four RED conditions:

```
  DANGLING        the anchor stopped resolving -- a decision was superseded,
                  a registry row was deleted or renamed
  STALE-DEFERRED  the verdict says `deferred`, but the cited registry row now
                  reads `shipped`. The verdict has become a lie.
  UNJUSTIFIED     a non-`absent` verdict with no anchor. A parse error, not a
                  warning -- the tropes family's reasonless-`inapplicable` rule.
  NOVELTY         the `absent` count rose against the committed artifact.
```

**Why each is borrowed rather than invented.**

- *Superseded decisions are absent from the digest by construction.* Its header
  says so, and it is observably true: 0006 and 0014 do not appear, 0014 having
  been superseded by 0126. So "is this refusal still in force?" needs no new
  machinery.
- *Registry IDs already parse.* `cli/tests/docs_consistency.rs` carries
  `looks_like_registry_id` and `parse_registry`, and already enforces ID
  uniqueness.
- *STALE-DEFERRED is seam-guard's STALE-DECL, exactly.* CLAUDE.md states the
  principle: a one-directional acknowledgement can only ever be satisfied, so it
  rots; the fix is a check that fails the moment someone adds the missing thing.
- *NOVELTY is the ratchet shape* `tropes check`, `type-audit` and the timings
  baseline all use. A gate that failed on the mere existence of an unmet chapter
  would go red on day one and train everyone to ignore it.

**The accepted cost, ratified at G3: the idea registry becomes a gated
interface.** Today a row's `status` cell is prose that humans read. After this
campaign, a row flipping to `shipped` reddens a committed artifact, and
renaming or retiring a row breaks a gate. That is the intended mechanism, not a
side effect — but it is a real constraint on 76 existing `CLIENT-*` rows and
every future one, and it lands on a session that has no reason to expect it.

**So diagnosability is a requirement of this campaign, not a nicety.** A
DANGLING or STALE-DEFERRED failure must name, in the failure text itself: the
anchor that stopped resolving, the corpus item that cited it, what change
would have caused it (a supersession, a rename, a status flip), and the two
legitimate repairs — re-verdict the item, or restore the anchor. A red whose
message is "the report moved" is what the trope ratchet gives up by being a
byte comparison, and it is affordable there because a trope corpus has no
external editors. This one does: anybody editing the registry is editing this
instrument's inputs without knowing it.

**The payoff, stated concretely.** When the action clock ships,
`CLIENT-action-clock` flips to `shipped` and every chapter deferred against it
goes red until a human re-reads it. When 0070 is superseded by a combat
decision, chapter 2.6's refusal goes red the same day. The matrix cannot quietly
become a lie — which is what a hand-authored table does *by default*. The
repo's own standing example is in CLAUDE.md: a committed prose figure for
census cost ("budget 15 minutes") sat beside a ledger already carrying a figure
twice as large, two independent readers anchored on the stale one, and the
resulting extrapolation was wrong by 2.2x. That paragraph was rewritten into a
*pointer* rather than a figure, deliberately. This matrix is the same problem
one level up: its cells are claims with dates, so each one points at something
checkable instead of asserting.

## 6. The surplus read, derived not authored

The trope matrix's most valuable table is its *demand* read — the corpora scored
against themselves, where disagreement is the finding a single column cannot
carry. The analogue here is the **surplus**: what does the corpus never think to
ask for?

> Enumerate `domains/*` and `windows/*`. Any subsystem that **no chapter's
> `present` verdict cites** is surplus — the corpus has no vocabulary for it.

Derived, so it moves on its own as domains land; authored, it would rot exactly
as §5 exists to prevent. It also makes the instrument's bias visible **in its own
output**, which decision 0095 demands rather than merely permits.

**Declared limitation:** subsystem granularity is coarse, and a domain cited by
a single chapter reads as fully covered. That is a real weakness of this read and
the report must print it next to the list.

## 7. The bias this instrument must declare before it prints a number

Decision 0095's first commitment is that "provenance is emitted, not
documented" — the report prints the corpus's bias *before* any figure, so a
reader cannot reach the score without passing the statement that the catalogue
is one instrument with known bias. This corpus carries an additional bias the
trope corpora do not, and it must be printed in the same place.

In `tropes/`, requirements were mapped onto the bundle vocabulary **by a model
reading wiki prose blind**, which is a weak authority but an *independent* one.
Here the verdicts are authored by us, about ourselves. That structure invites
self-flattery, and the anchor requirement mitigates it unevenly:

- `refused` and `deferred` are **strongly** checked. A decision must be in
  force; a registry row must exist and must not read `shipped`.
- `present` is **weakly** checked. A path that exists is not a working feature,
  and a test that exists is not proof that this chapter's capability is met.
  Preferring `test:` over `path:` narrows the gap without closing it.

So `present` is the verdict this instrument is least entitled to, and the report
must say so above its own headline. Accepting that is cheaper than the
alternative (§9), and naming it is what keeps the artifact honest.

## 8. Corpus construction and the freeze

The corpus is frozen **before** any verdict is measured (decision 0016), with
its chapter count asserted in a test, so that changing it is a deliberate act.

**The count must be established, not assumed.** A fetch of the live table of
contents on 2026-08-15 renders **four numbered sections and 73 chapters**
(13 + 7 + 20 + 33), plus two front-matter pages (*Introduction*, *Building for
the Web*) and two back-matter pages. An earlier framing in this campaign's own
brainstorm said "five sections, ~90 chapters" — it was counting mdbook's
top-level numbering, which includes front and back matter. Stage 1 establishes
the exact figure from the source and the corpus asserts it; no downstream stage
may take the count from this paragraph.

Front matter is `inapplicable` by construction (*Building for the Web* is a
bracket-lib toolchain chapter), and saying so in the corpus is better than
silently excluding it — an excluded chapter is invisible, an `inapplicable` one
carries its reason.

Corpus JSON mirrors the trope schema's header so the two families read alike.
**The item unit is generalized from the first commit** (ratified at G3): a
tutorial's items are chapters, but NetHack has no chapters and its items are
features and mechanics. Keying the schema on `chapters` would force a format
migration on the first corpus that is not a tutorial — the exact corpus this
family exists to admit.

```json
{
  "corpus": "wolverson-2021",
  "unit": "chapter",
  "ordered": true,
  "provenance": "Herbert Wolverson, Roguelike Tutorial - In Rust ...
                 An instrument with known bias, not a standard ...",
  "frozen": "before first measurement, The Compendium",
  "items": [
    { "id": "2.6", "kind": "chapter", "title": "Dealing Damage",
      "verdict": "refused", "anchor": "decision:0070",
      "note": "vitality is a fold over committed wounds; no stored HP" }
  ]
}
```

**`ordered` is the field generalizing early actually bought**, and it is not
cosmetic. Wolverson's items form a *pedagogical ladder*: each chapter assumes
the one before it, which is what makes "the first chapter Hornvale cannot
replicate" a meaningful sentence and the single most useful reading this corpus
produces. NetHack's feature list has no such order, and the same sentence about
it would be meaningless. So the resolver may make **ordinal claims — first
unmet item, longest satisfied prefix — only for a corpus that declares
`ordered: true`**, and must refuse them otherwise rather than silently ranking
by `id`. A corpus-level `unit` labels the items; a per-item `kind` lets a mixed
corpus (features *and* mechanics *and* dungeon fixtures) stay honest about what
each row is.

## 9. What this campaign refuses to build, and why

**Executable per-chapter probes.** The strongest conceivable instrument runs
each chapter as a scenario script against `possess --script` and reads the
verdict off a transcript — behaviour, not assertion, which is the standard
memory `measure-dont-narrate-the-mechanism` and `check-the-consumer-actually-
acts-on-it` both push toward. It is refused here on two grounds: it costs far
more than the rest of the campaign combined, and it **still cannot cover the
eight renderer items**, which have no transcript to read. Captured as a
follow-up against the chapters where it would actually discriminate.
(*Amended in Task 6 with §3: this read "the renderer third" on the same
un-re-derived estimate.*)

## 10. Stages

1. **Freeze the corpus.** Fetch the ToC, establish the exact chapter count,
   write the corpus with `verdict: null` throughout, assert the count. No
   verdicts yet — the freeze precedes the measurement.
2. **The resolver and its verdicts.** `systems.rs`: load, resolve, the four RED
   conditions. Anchor resolution against the digest, the registry, and the
   filesystem. Unit-tested against fixtures, including one deliberately
   dangling anchor and one deliberately stale `deferred` — a check must be
   observed going red before it is trusted.
3. **Author the verdicts.** All chapters, each with an anchor. The largest and
   least mechanical stage; expect it to surface registry rows that do not exist
   yet.
4. **Report, matrix, surplus read.** The two rendered artifacts, the ratchet
   test, the redirects in `regenerate-artifacts.sh`.
5. **The reading.** Chronicle entry, idea-registry rows for every hole, book
   freshness sweep, retrospective.

## 10a. Constraints this campaign inherits

No new dependencies (decision 0004): the resolver reads JSON through
`serde_json` and everything else through `std`. Corpus is data, resolver is
code (0011). `docs/audits/` is already declared in `docs/generated-paths.txt`,
so the drift check picks the new artifacts up without an edit there — but the
first commit introducing them **must `git add` them**, because
`git diff --exit-code` against an untracked path is silently vacuous and would
report "no drift" forever.

## 11. Testing

- Fixture corpora exercising each of the four RED conditions. **Each must be
  observed failing before its fix**, per memory `five-vacuous-guards-one-
  campaign` and `an-empty-diff-needs-a-positive-control`.
- The count assertion on the frozen corpus.
- Byte-ratchet on both artifacts (`cli/tests/system_coverage.rs`), modelled on
  `trope_coverage.rs`, which is a whole-file comparison — strictly stronger than
  a per-item rule, at the cost of diagnosis.
- A test that every `present` verdict's mechanism anchor resolves, and that no
  `deferred` row's registry ID reads `shipped`.

## 12. What was verified, not assumed

| claim | how | result |
|---|---|---|
| the tutorial's ToC and chapter count | fetched 2026-08-15, **one fetch** | 4 sections, 73 chapters + front/back matter — Stage 1 re-establishes this from the source; it is not authority for the freeze |
| chapter 2.6 adds HP, melee, A\* pursuit, `BlocksTile` | fetched `chapter_7.html` | confirmed all four |
| chapter 2.5's monsters do not chase | fetched `chapter_6.html` | confirmed — they only print on sighting |
| superseded decisions are absent from the digest | read `decisions-in-force.md` | 0006 and 0014 absent; header states it |
| registry IDs are machine-parseable | read `cli/tests/docs_consistency.rs` | `looks_like_registry_id`, `parse_registry` |
| `docs/audits/` is already drift-checked | read `docs/generated-paths.txt` | present — **no edit needed there** |
| the tropes ratchet runs in the gate | read `cli/tests/trope_coverage.rs` | a nextest byte-comparison test — but see §12a, the ladder moved under this campaign |
| Hornvale has no attack verb / no HP | read `session.rs` HELP + grep workspace | confirmed; wounds exist only in history-baking |
| NPCs move only on `wait` | grep `step_with_occupancy` | exactly one call site, in `fn wait` |
| the game client is monochrome | read `cell.rs` | `enum Ink { Plain }`; colour is on the wire, unread |

## 12a. Which gate the ratchet actually runs in — amended mid-campaign

This spec was written against a single `make gate`. Task 1 absorbed 66 commits
of main carrying the gate-ladder rewrite (decisions 0132/0133), and the claim
above needs correcting rather than quietly inheriting.

There are now three gates: `gate-commit` (local, seconds, every commit),
`gate-stage` and `gate-campaign` (both dispatched to one strictly serial lane
on the canonical box). **`gate-commit` runs only the *sub-floor tier* — tests
with a recorded baseline duration in `docs/timings/subfloor-roster.tsv`.** A
newly written test has no such baseline and is therefore **excluded from
`gate-commit` by design**; it enters the roster on the next green *stage* gate,
which measures it.

So the honest statement of this instrument's enforcement, replacing "it runs in
the gate":

- **From the first stage gate onward**, the ratchet and the anchor audit run
  in `gate-stage` and `gate-campaign` — every plan-stage boundary and every
  merge.
- **In `gate-commit`, they run only after** a green stage gate has measured
  them into the roster.

This weakens nothing the design depends on: the anchor discipline's job is to
redden when a decision is superseded or a registry row ships, and both are
edits that go through a stage or campaign gate before merging. But it does mean
**a local `gate-commit` immediately after writing these tests will not run
them**, which would otherwise read as a passing gate. Do not take a green
`gate-commit` as evidence the ratchet works; take the stage gate.

## 13. Open questions and follow-ups

- Executable per-chapter probes (§9), scoped to chapters where a transcript
  would discriminate.
- The surplus read's coarse granularity (§6).
- `make worktree-take` branches from `origin/main` and does not warn when local
  `main` is ahead; it bit at this campaign's start.

## 14. Decisions this campaign should ratify

- **A capability corpus is a sibling family to a trope corpus, never a member.**
  Different subject (program vs world), different resolution substrate.
- **A coverage verdict must cite a machine-checked anchor, and an unmet item
  must distinguish refused / deferred / absent.** The generalisation of 0095's
  third verdict, and the rule that makes the artifact unable to rot silently.
  Its accepted cost — ratified at G3 — is that the idea registry becomes a
  gated interface: a row's ID and status are load-bearing for a committed
  artifact, and the failure text must be diagnosable by a session that does not
  know this instrument exists.
- **An ordinal reading requires a declared ordering.** A corpus states
  `ordered`, and the resolver refuses first-unmet / longest-prefix claims for a
  corpus that does not — an unordered catalogue ranked by `id` would
  manufacture a ladder its source never had.
