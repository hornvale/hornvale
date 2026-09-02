# The Hallmark

A domain crate depends on the kernel and never on a sibling. It is one of the
oldest rules in the project and it is the right rule: it is what makes *adding
a domain* a local act rather than a negotiation with nine others. It has also
been sending the project a bill, and until this campaign nobody had read the
total.

`UnitError` was defined three times, byte for byte, in the kernel and in two
domains. `GenesisError` was defined verbatim twice, and the outcome struct
beside it existed as two structurally identical copies differing only in the
name of their payload field. Five variants with word-identical documentation
appeared in a domain and in a window, bridged by a five-arm match whose every
arm mapped a name to itself. A three-valued vocabulary appeared in religion
and again in language, bridged by a conversion function in the composition
root. A five-member rock roster appeared inside an eleven-variant climate
enum, bridged by another. A three-member cave roster appeared inside another
climate enum, bridged by nothing at all — by a *test*, hand-written, whose
job was to notice if the two rosters ever drifted apart.

None of that is a violation. Every one of those duplicates is what obedience
to the layering rule looks like when two domains need the same word. The
missing piece was never the rule; it was the answer to the question the rule
provokes. If a sibling import is forbidden and the word is genuinely shared,
where is the word supposed to live?

## The rule that existed, and how narrow it was

Decision 0044 is the project's only ratified placement test, and it is scoped
to *coherent physical quantities*: an elevation datum, a temperature pair.
Decision 0216 extended it by analogy to exactly one non-unit roster — the
depth `Band` — without generalizing the argument. Four other kernel residents
(`ecology`, `color`, `room`, `provenance`) were placed by campaign-spec
rulings that were never promoted to decision records at all. `kernel/CLAUDE.md`
documents the kernel's *contracts* in detail and says nothing about its
*scope*.

So the project had a placement doctrine covering units, a precedent covering
one roster, and four undocumented placements — and a coming Entity-Component
layer that will ask the question at scale, because a cross-domain query
returning a `terrain::Horizon` to a non-terrain consumer has no legal home
for that type outside the kernel.

Decision 0517 generalizes rather than relaxes. A type belongs in the kernel
when any of three clauses holds: **(a)** more than one domain speaks it today
— 0044's clause, widened from quantities to any type; **(b)** it originates in
a kernel type — 0044's clause, unchanged; **(c)** it appears in the wire
schema of a component registered for cross-domain query.

Clause (a) carries the qualifier that does the real work, taken from 0216: a
*forced* duplicate qualifies, a *deliberate* projection does not. A duplicate
is forced when one side exists only because layering forbids the import; it is
deliberate when deleting one side would remove an independent answer, or when
the two carry different information. That test is semantic, and the campaign's
own baseline shows why nothing weaker will do.

Clause (c) is **dormant** — there is no component catalog yet. It is ratified
now precisely because it is dormant: the alternative is each Entity-Component
campaign relitigating placement per component, which is how one arrives at a
kernel full of types nobody argued for. And the modifier over all three
clauses is a brake rather than an accelerator: a volatile type iterates
domain-side, may reserve its identity kernel-side early, and moves when its
shape settles. Kernel churn is the most expensive rebuild tier in the
workspace, and a promotion is a save-format-adjacent act. Promotion is meant
to be occasional.

## The kernel holds the roster; the domain holds the meaning

This formula is 0216's, restated, and it is not a stylistic preference — it is
forced by the orphan rule. An inherent impl cannot live outside the type's
defining crate, so a promotion cannot carry a domain's derivations with it.
The split falls out cleanly, and where each half landed is the interesting
part:

- `Sentiment`'s *derivation* moved **with** the type, because
  `Sentiment::of(&Phenomenon)` is a pure function of a kernel type — clause
  (b) in one sentence. Religion's ledger spellings (`"eternal"`, `"cyclic"`,
  `"ambient"`) stayed behind as a free function, because those spellings are
  religion's contract with the ledger, not the kernel's business.
- Terrain kept `band_at_depth` and `Cave::from_reach` while `Horizon` and
  `CaveKind` left. Those are the derivations — how a depth becomes a horizon,
  how a reach becomes a cave. The roster is what climate needed; the
  derivations are what terrain does with it.
- `pub use` keeps every call site source-compatible, so a promotion is not a
  rename sweep across the tree.

## Six vocabularies, and the one that did not go to the kernel

`UnitError`'s two duplicate definitions were deleted and both domains adopted
the kernel's. `GenesisError` moved verbatim into a new kernel `genesis`
module, and `GenesisOutcome<T>` replaced two structural copies with one
generic — at the cost of a field rename at roughly fifteen sites, the payload
going from `system`/`globe` to `value`. `Sentiment` moved beside the
`Phenomenon` it is a function of, and the conversion function in the
composition root was deleted. `Horizon` and `CaveKind` moved on exactly the
argument the kernel's own `band.rs` module doc had already made about `Band`:
climate does not *derive* a horizon, it only names one, so climate's mirror is
forced rather than deliberate.

The sixth is the instructive one, because it did not involve the kernel at
all. `locale::Substrate` was deleted in favour of `climate::GroundKind` — the
same five variants, the same documentation — and no type moved anywhere.
Locale is a *window*, and a window may import a domain. Its duplicate was
never forced; it was an ordinary refactor that had been sitting there looking
like a layering cost. The criterion's scope is domains, and the difference
between "the layering rule made me do this" and "nobody got around to it" is
exactly the difference the criterion has to be able to draw.

## A detector that refuses to decide

The forward half of the campaign is `tools/placement-audit`, built on the same
machinery as the type audit: a source walk, no build, outside the workspace.
It looks for **shape twins** — two `pub enum`s whose variant-name sets are
identical, or two `pub struct`s whose field-name sets are identical, in
different crates where at least one is a domain. Name equality is neither
necessary nor sufficient; the match is on member sets.

Each detected twin must carry a verdict in its own doc comment —
`promote(anchor)`, `deliberate(why the duplication buys something)`, or
`deferred(why not yet)` — with a short fingerprint of the sorted member names
embedded in the tag. That produces three verdicts on the seam-guard model:
an untagged twin is **red** (the novelty ratchet: existing debt is tagged once,
new forced duplicates cannot land silently); a tagged twin whose shape no
longer matches its fingerprint is **red** (reconsider-on-touch: editing a type
whose placement is in question reopens the question at the moment it is
cheapest to answer, and only then); tagged-and-matching is green and listed in
the committed roster.

Failing on novelty rather than on existence is the whole design, and the
project has learned it the hard way twice: a gate that goes red on day one and
stays red trains everyone to ignore it, and a report that can never fail is
ignored just as fast. Untouched debt never nags here. A *touched* type has to
answer for itself.

**The tool detects and demands; it never decides**, and the baseline is the
proof that this is a requirement rather than modesty. After the six
consolidations, exactly two shape twins remain in the tree:

| twin | fingerprint | why it stays |
| --- | --- | --- |
| `astronomy::Rotation` / `climate::RotationRegime` | `606e60` | a lossy projection — climate's copy drops spin direction and carries a standard-day length; the worldgen conversion genuinely converts |
| `kernel::Provenance` / `language::Evidential` | `49883f` | an epistemic fact against a grammatical category; the shared variant names are a coincidence of English |

Both are *identical in shape* and neither is a forced duplicate. An
auto-promoter would have promoted both — collapsing a deliberate information
loss in the first case, and in the second merging two concepts that a natural
language happens to spell alike. That is why the verdict is always a human's,
carried in a tag the tool can only read.

Where it runs was decided by measurement rather than by expectation. The rule
was set before the tool existed: within twice the type audit's own warm cost
(about 6.3 s) and it joins the commit gate; over, and it joins the stage gate.
It measures 4.5–5.8 s warm — under the threshold, and under the type audit
itself — so it runs on every commit.

## The defect a survey found and byte-identity could not have

The consolidation survey turned over one field that was not a duplicate at
all. `EraClimate.day` is documented as "absolute standard day of the era", and
it has two producers.

One of them, `paleoclimate_from`, writes exactly that: a negative day count
running back a million years, matching the ice-history samples it is compared
against. The other, `bake_eras`, writes a **bake year** — a number between 0
and 2000, stepped by an epoch length — into the same slot, and the bake reads
it back against a loop variable that is also a year. Both paths are internally
consistent. Neither was broken. They never met at one consumer, so nothing in
the world was wrong, and no test could have been red. What was wrong was that
one field carried two units that differ by a factor of 365.25, and its
documentation named only one of them.

This was diagnosed as a STOP: the campaign had scoped a type migration, and
every available retype made things worse. Converting the bake's constant
through the days hatch would reinterpret year 2000 as day 2000. Converting at
the construction site instead would move the bake's own era boundaries
relative to an unconverted comparand — changing behaviour and committed bytes.
Converting both is a repair of the bake's entire time axis, which is not a type
migration. The diagnosis was recorded, an idea-registry row was opened, and
the field's tags were left exactly as they were.

The decider reversed the deferral at the merge stop: fix the axis now, then
finish the migration it was blocking. What made that a tractable order rather
than a fresh investigation is that the STOP had already produced the consumer
table.

Bake-path `EraClimate.day` has exactly two readers, both inside the bake. One
compares it against a bake year — that reader needs a **unit**. The other is a
`min_by` picking the oldest era — that reader needs an **ordering** and never
looks at the number. No bake-path value reaches the extraction pass, the
committed facts, the ledger, or any artifact. So the fix is not a conversion
at all: the bake's year axis moves into a bake-owned parallel vector beside the
era series, and `EraClimate.day` takes the era's true deep-time day on both
producers.

The smaller diff was available and was refused for a reason worth recording.
Storing days and converting the comparand back would have bought
unit-consistency by writing `bake_year × 365.25` into the field — the bake's
year axis in a day costume. A reader would then find an era stamped *day
91312.5 after genesis* which is physically a glacial state 750,000 years
*before* genesis. Single-axis in unit, still false in referent. Correcting a
unit into a wrong number is not a repair.

**The determinism argument is control-flow identity, not measurement.**
`era_years[e]` holds the identical `f64` the field used to hold, unconverted,
and the loop variable it is compared against is untouched — so every `<=`
outcome is bit-for-bit what it was, exact-equality grid alignments included.
The `min_by` selects the first minimum, and bake days ascend where bake years
ascended, so it returns the same era before and after. That is a stronger
claim than "we measured no change": the same doubles reach the same
comparison. The seed-42 byte comparison is the *check* on that argument, not a
substitute for it, and it came back identical.

With one axis in the slot, the blocked migration landed: `EraClimate.day`,
`PaleoRecord.glacial_maximum_day`, `IceState.day` and the ice integrator's
sample series are all `WorldTime` now — an exact tick count, which needs no
quantization at any magnitude and sorts with `Ord` instead of `total_cmp`.
Four `pending(wave-2)` tags disappeared from the audit.

That migration's own justification needed correcting too, and the correction
is the transferable part. The obvious safe direction — ticks to days, exact
below 247 million years — is *not* the direction the argument leans on. The
load-bearing conversion is days to ticks, which **always rounds**, and which is
a no-op here only because every era day this window constructs lands on an
exact whole-day integer: the ice step is 730,500 days and each era day is
`−365,250,000 + e × 15,218,750`. A future fractional step would need that
argument re-verified rather than assumed to still hold — which is a different
sentence from "it round-trips losslessly", and only the second one was true.

## A namespace is not an ontology

The cave half of `Formation` was adjudicated once and overruled once, and both
readings were defensible.

The execution-time adjudication was to keep climate's three variants —
`KarstCave`, `LavaTube`, `FractureCave` — as a deliberate projection, on
evidence: the promised payoff of an embed was "keep every emitted string
identical through one spelling-table edit", and there *is no spelling table*.
The corpus's strings are hand-authored `&'static str` literals sitting beside
the enum, not derived from it. Absent the payoff, the embed read as churn.

The decider ruled the other way at the merge stop, on grounds the adjudication
had not weighed: the correspondence is worth making **structural** whether or
not a spelling table rides along. So `Formation::Cave(CaveKind)` replaced the
three variants, and four grouped match arms across three crates collapsed to
one wildcard each — the compiler naming every site, and finding no others.

The spellings did not move, and why they do not have to is the part worth
publishing. `CaveKind::name` answers `"karst"`, `"lava-tube"`, `"fracture"` —
its scene-emission legend. The surface corpus's genera answer `"karst-cave"`,
`"lava-tube"`, `"fracture-cave"`, because an underworld community is a
community *of* one of the formations the surface corpus already named. Those
are two **namespaces**, not two ontologies: *karst* is a dissolution process,
*karst-cave* is a habitat name standing among tundra and reef and savanna, and
a name that has to sit in a list of surface formations has to say which kind of
thing it is. One of the three agrees by coincidence, and that coincidence is
exactly why the mismatch survived as long as it did — `lava-tube` matched, and
looked like the rule working.

So the embed made the *variant* join structural and left the *spelling* join
hand-written, which is the correct division: `Formation` carries no name of its
own for a structural join to route through. One hand-written correspondence
remains, and two tests pin it in both directions — every cave kind reaches a
genus that occurs in the corpus, and the mapping is not transposed.

The test that was deleted is the interesting deletion. It asserted that the
three cave kinds map to three *distinct* formations — which the embed makes
true by construction, since two different `CaveKind`s wrapped in the same
variant are unequal by definition. It had stopped being an assertion about the
program and become an assertion about the type system. It never touched the
spelling join at all.

`Formation` itself carries no `placement:` tag, and should not: it is not a
shape twin. It holds eighteen other variants beside `Cave`, so embedding one
enum inside the other leaves the two member sets nothing like each other and
the detector correctly says nothing. What stands in for a tag here is prose and
tests — the variant's documentation cites the kernel type and the ruling that
put it there, and the genus map is pinned by assertions rather than by a
fingerprint.

## What did not move, and the one thing that did

A seed-42 world is byte-identical across the whole campaign: sha256
`e70ca3d0…`, reproduced three times, including independently by a reviewer
across the axis repair and the `WorldTime` migration. Every drift-checked
artifact is unmoved except the two that any pub-boundary change must move —
the type-audit report and the digest's decision index.

One committed file moved that is neither, and it is worth naming because it
marks the edge of a refusal clause. 0517 refuses "any promotion that changes a
committed spelling without an epoch", and every member here kept its spelling:
`Regolith` is still `Regolith`. But a locale test fixture pins `{:?}` output,
and `stratum: Regolith` is now `stratum: Rock(Regolith)` — 810 lines of debug
rendering, rebaselined, and proved line-for-line against the same cells in the
same order. The refusal clause reaches the ledger and the artifacts a world
emits. A structural spelling visible only through `Debug`, in a fixture that no
epoch governs, is a test's own business — but it is not *nothing*, and a
campaign that promises "no committed spelling changed" should be able to say
which committed file changed anyway.
