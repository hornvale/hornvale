# 0135. A coverage verdict cites a machine-checked anchor

**Status:** Accepted (2026-08-15) · **Decider:** Nathan · **Relates to:**
[0026](0026-slugs-not-numbers.md),
[0031](0031-the-frontier-is-published-in-the-book.md),
[0095](0095-a-corpus-is-an-instrument-never-a-standard.md),
[0131](0131-refuted-is-a-seventh-registry-status.md),
[0134](0134-a-capability-corpus-is-a-sibling-to-a-trope-corpus.md)

In the context of a coverage matrix that scores this project against an
external catalogue, facing the fact that a hand-maintained control matrix does
not go stale so much as begin to **lie**, we decided that **every verdict
except `absent` must cite an anchor the resolver re-checks on every run, and
that an unmet item must distinguish `refused` from `deferred` from `absent`**,
accepting that the idea registry thereby becomes a **gated interface**.

**An unmet capability is not one fact but three.** It may be deliberately
refused by a ratified decision, planned but unbuilt, or genuinely nobody's yet.
An instrument that cannot tell them apart publishes a deficiency list that is
mostly false. Decision 0095 already ratifies the same argument one step
earlier — without `inapplicable(reason)`, "every difference between Hornvale
and a nineteenth-century French dramaturgical taxonomy reads as a deficiency."
Applied to a program-capability catalogue, the reasons an item is unmet are
themselves load-bearing, so the vocabulary is five:

```
  present       cites a MECHANISM anchor  (test name, or a path)
  refused       cites a DECISION anchor   (in force, not superseded)
  deferred      cites a REGISTRY anchor   (a row that exists, not `shipped`)
  absent        cites NOTHING             (the honest red)
  inapplicable  cites a REASON            (0095's third verdict)
```

**The anchor is the evidence, so the resolver checks it.** A decision anchor is
membership in the generated in-force index — superseded decisions are absent
from it by construction, so "is this refusal still in force?" needs no new
machinery. A registry anchor is a parsed row identifier, using the same parse
and the same status normalization the registry's own drift check uses, never a
second rule. A mechanism anchor is resolved **without building**: a path check,
or a source scan for a test definition that is not `#[ignore]`d. Resolution
must not shell out to `cargo` — the ratchet would then build inside a build.

Four conditions turn the committed artifact red: **DANGLING** (the anchor
stopped resolving), **STALE-DEFERRED** (the cited row now reads `shipped`, so
the verdict has become a lie), **UNJUSTIFIED** (a non-`absent` verdict with no
anchor — a parse error, not a warning), and **NOVELTY** (the `absent` count
rose). None is invented: STALE-DEFERRED is the mutation guard's
stale-declaration verdict, and NOVELTY is the ratchet shape the trope audit,
the type audit and the duration baseline already use. A gate that failed on the
mere existence of an unmet item would go red on day one and train everyone to
ignore it.

**Clause 2: the subject is the whole program, and a spanning item takes its
weakest half.** The corpus scores Hornvale-the-program, `clients/` included —
they are in this repository, so they are in the subject. An item whose
capability has a **sim** half and a **client** half takes **the weaker of the
two**, and its anchor cites the half that decided the verdict (a `path:` anchor
when that half lives in `clients/`, which is outside the cargo workspace and so
beyond a `test:` anchor's reach). Each half is scored against the item's
capability as a whole, not against every element the item happens to add: a
single missing widget inside an otherwise-delivered half is a note, not a
demotion.

This clause exists because **decision 0022 is an assignment of responsibility,
not a refusal.** The sim emits data and clients render; nothing in it says
rendering does not count. A `refused decision:0022` verdict says exactly that,
converting *built elsewhere, or not built* into *deliberately will not* — the
strongest claim this vocabulary has, awarded to items nobody ever decided
against. What 0022 does forbid is narrow: **the sim carrying a picture.** The
tutorial's `Renderable { glyph, fg, bg }` component has no counterpart here and
never will. Pictures existing is not forbidden at all, and three in-repo
renderers draw them.

**Ratified by Nathan at G6, 2026-08-15, on a defect this record's own first
version made possible.** Without the clause, the corpus applied 0022
asymmetrically: an item that was mostly sim had its sim half scored `present`
and its render half waved away as 0022's, while an item that was mostly render
had the *whole* item scored `refused` under the same decision — and which
treatment an item got tracked which produced the more favourable verdict. Five
reviews missed it because **every anchor resolved.** The anchors were real sim
tests; the resolver cannot see that a verdict changed subject halfway through a
corpus, and no check in this record catches an instrument that silently
switches what it is measuring. Re-auditing all 74 items under the clause moved
twelve verdicts in both directions — four demotions, six promotions out of
`refused`, two re-anchorings — and left **no item refused under 0022 at all**.

**The accepted cost: the idea registry becomes a gated interface.** Ratified by
Nathan at this campaign's G3 review, 2026-08-15.

The registry's IDs and statuses were **already** machine-consumed before this
record, and stating otherwise would overstate the change. Three committed
tests read them: `the_waiver_list_only_shrinks` (`docs_consistency.rs:489`),
which fails by design when a waived row is renamed; `no_new_numbered_registry_ids`
(`:536`), enforcing decision 0026 against a frozen list; and
`every_refuted_row_cites_its_evidence` (`:432`), which reads a *normalized
status* and arrives with decision 0131.

**What is new is narrower and stronger: a row's status now reddens a committed
artifact outside the registry's own drift check, authored by an instrument its
editors have no reason to know exists.** Every prior consumer lives in
`docs_consistency.rs` — the same file, the same run, guarding the registry on
the registry's own behalf, so a row's editor meets it immediately and in
context. A `deferred` verdict in `systems/` is a claim made *about* a row by a
document in another directory, on a schedule the row's editor does not
control: flipping a row to `shipped` now reddens `docs/audits/`, and renaming
or retiring one breaks a check whose failure text is the only thing that will
explain why. That is the intended mechanism rather than a side effect — it is
precisely what stops the matrix quietly becoming a lie — but it is a real
constraint on every existing and future row, and it lands on sessions that did
not ask for it. Decision 0026's permanent-ID rule gains a consumer outside the
file that has always enforced it.

**So diagnosability is part of the decision, not a nicety.** A DANGLING or
STALE-DEFERRED failure must name, in the failure text itself: the anchor that
stopped resolving, the corpus item that cited it, what kind of change would
have caused it (a supersession, a rename, a status flip), and the two
legitimate repairs — re-verdict the item, or restore the anchor. A red whose
message is only "the artifact moved" is affordable for the trope ratchet
because a trope corpus has no external editors. This one has many: anybody
editing the registry is editing this instrument's inputs without knowing it.

**Consequence.** `present` remains the verdict the instrument is least entitled
to — a path that exists is not a working feature, and a resolvable test name is
not proof a capability is met — and the report prints that above its own tally
rather than in a footnote. `refused` and `deferred` are strongly checked;
`absent` and `inapplicable` are BOTH unfalsifiable by construction and are the
two verdicts a reader must take on the author's word — `absent` cites nothing,
and `inapplicable`'s `reason:` anchor is confirmed *present* (empty prose is a
parse error, UNJUSTIFIED) but its *content* is never checked against anything,
the same way a registry or decision anchor's content is. `NOVELTY` ratchets
only the `absent` count, not `inapplicable`'s, so this instrument's one
falsification-by-count guard still watches a single number. An item whose real
position is a refusal recorded only in a registry row, not a ratified decision,
must read `absent`: the anchor kinds are not interchangeable, and widening
`refused` to accept a registry anchor would let an unratified opinion
masquerade as a project position.

**See also.** `cli/src/systems.rs` (the resolver); `cli/tests/system_coverage.rs`
(the ratchet and the anchor audit); `docs/audits/system-coverage-wolverson-2021.md`;
[The Compendium chronicle](../../book/src/chronicle/the-compendium.md);
[The Compendium
spec §5](../superpowers/specs/2026-08-15-the-compendium-design.md).
