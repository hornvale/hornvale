# The Manikin — design

A manikin is a body that is nobody: a reference figure a tailor fits cloth
against, precisely because no customer has its proportions. Hornvale's species
model has needed one since the first people was authored, and has been using a
goblin instead.

## 1. Why

Every scalar in the species model is a bare ratio in `[0, 1]`, and the number
`0.5` on each of them means one thing: *goblin*. This is not a convention in
prose. `SocietyVector::baseline` (`domains/species/src/lib.rs:187`) is a
`pub const fn` whose doc reads "Equal to the goblin's authored society dims,"
and `society_baseline_equals_the_goblin_authored_society` (`:2895`) asserts the
equality. `goblin_is_the_baseline_vector` (`:2514`) asserts that every goblin
scalar sits at exactly `0.5`. The identity element of the model and one
inhabitant of the world are the same fact, pinned by test.

That weld was cheap when the roster was one people. It is a frame bug, and the
project has already ratified the decision that names it. **BIO-39 — "a unit is
not a frame"** — was elevation: `ConditionNiche` read the raw isostatic datum, so
an authored optimum of `2600` meant 5200 m above sea level on one world and 5900
on another. The kobold's documented "exclusive highland stronghold" was
unoccupiable on most seeds, its fit ran ~25× below every other people's
everywhere, and it shipped. Its best-fit share went 0.4 % → 26.5 % once the frame
was corrected.

The species baseline is that bug one level up, at psychology rather than
elevation. `0.5` is a datum; the frame it is stated in is a particular people;
and the datum quietly means something different as the roster grows. Nothing has
broken yet because the roster is four goblinoids and a handful of beasts that
carry no vectors at all. A campaign to add humans, elves, and dwarves is what
makes it bite, which is why this campaign runs first and alone.

## 2. What is wrong

**Four distinct things are welded into one word.** "Baseline" is currently doing
all of the following simultaneously:

1. the **fallback value** a mixed consumer resolves for a `Solitary` kind that
   carries no society vector;
2. the **authored anchor** one kind is pinned to;
3. the **test contract** that adding a kind is byte-neutral for existing kinds;
4. the **reader's calibration** — what a person should picture on being told a
   dimension reads `0.5`.

Only (4) has any business naming a people, and the people it names should be one
the reader has intuitions about. The other three are frame properties.

**The identity element is necessarily an invention, and is documented as a
discovery.** There is a tempting alternative in which `0.5` is *descriptive* —
the roster's central tendency, a measured mean. Determinism forbids it outright:
a centroid moves when a kind is added, so every world would shift on every
roster change. The baseline must therefore be a fiat. Stating it as "goblin's
value" disguises a fiat as an observation, which is exactly what let it go
unexamined for the model's whole life.

**The obvious repair is wrong, and the owner caught it.** The intuitive fix is to
re-anchor on humans — humans are fantasy's boring default, and a reader has a
human body to calibrate against. But humans are not strictly average across the
roster this world is going to hold. Human night vision is genuinely poor; it sits
*below* much of the current bestiary. Author humans at `0.5` on that dimension
and the number stops meaning "typical" and starts meaning "weak," silently
re-scaling the kobold's authored `0.9` against a feeble reference. The
re-anchoring would relocate the frame bug rather than remove it. The articulation
dimensions run the other way — the phonology envelope is built on IPA, a
human-calibrated inventory, so a human anchor there is *better* founded than a
goblin one. A single people cannot be the right anchor for all three vectors.

**Therefore the reference must be nobody.** This is a well-worn resolution
outside this project: the CIE 1931 standard observer, ICRP's "standard man," the
anthropometric manikin. SI reached it too, and instructively — the 2019 redefinition
did not replace the platinum kilogram prototype with a *better* cylinder, it
stopped anchoring on an artifact at all.

The full option space, as a scale on *how much a people is privileged by the
identity element*:

| Privilege | Position | What `0.5` means |
|---|---|---|
| 0 % | Reserved midpoint — no kind may sit there | Neutral, uninhabited, unnamed |
| 20 % | De-anchored, exemplar named in prose only | Neutral by fiat |
| 40 % | Per-dimension anchors | Varies by vector |
| **60 %** | **Named reference vector; kinds may coincide with it** | **The manikin** |
| 80 % | Anchor by definition — today's scheme, tenant swapped | "Goblin", or "human" |
| 100 % | Deep anthropocentrism — ranges relative to human too | Human, and so does the range |

Today sits at 80 % with goblin. This campaign moves to 60 %.

## 3. Design decisions

**D1 — The manikin is a named reference vector, never a rostered kind.**
`MindVector::MANIKIN`, `SocietyVector::MANIKIN`, and `PerceptionVector::MANIKIN`
are associated consts on the three vector types. The manikin has no `KindId`, no
entry in any registry, no gloss in `KIND_CONCEPTS`, no `family_of` row, no mass,
no metabolic class, no resource vector, and no condition niche.

The alternative — a never-instantiated `manikin` species in
`biosphere_registry()` — was considered and rejected on verified grounds.
`windows/worldgen/tests/non_void_roster.rs` iterates that registry and asserts
the void list is empty **with no allowlist**; its header calls a never-placed
kind "a ghost" and cites BIO-39 as the bug it refuses. A rostered manikin fails
it by construction, and the only repair is to introduce an allowlist into a
default-deny test whose entire value is that it has none. It would further owe
six meaningless authored values and a special case in every consumer that
iterates the roster — the class of thing The Vacancy established as where silent
breakage lives.

A manikin that is a *value* cannot be void, because voidness is a property of a
kind's carrying capacity and the manikin has no biosphere to have capacity in.

**D2 — An associated const, not a `const fn`.** `SocietyVector::baseline()`
becomes `SocietyVector::MANIKIN`. At the five call sites the reference reads as
what it is — a citation of a standard — rather than as a computation.

**D3 — Two doc conventions are promoted to values.** `MindVector` (`:155`) and
`PerceptionVector` (`:197`) currently declare "0.5 ≡ the goblin baseline" in doc
comments with nothing in code to cite. They gain `MANIKIN` consts. This is new
surface, and it is the point: a convention that exists only in prose cannot be
depended on, tested, or moved deliberately.

**D4 — Every manikin value is today's value.** `Hierarchic`, `Rank`, and `0.5`
throughout. The campaign changes what the midpoint *means* and what cites it, and
changes no number. This is what makes it byte-neutral and therefore cheap; see §4.

**D5 — Goblin's coincidence with the manikin is pinned as a characterization,
not a contract.** `goblin_is_the_baseline_vector` survives, renamed, with a doc
comment stating explicitly that goblin is *authored at* the manikin's values,
that this is authorship rather than definition, and that the test exists so a
future characterization of goblin arrives as a visible diff rather than a silent
shift in numbers. This is the pattern The Vacancy applied in this same registry
to the `Autotroph`/Kleiber divergence.

The two rejected alternatives are both worse in a specific way. Deleting the test
leaves this campaign's byte-neutrality claim unguarded. Keeping it unchanged
re-welds the two meanings the campaign exists to separate.

**D6 — `society_baseline_equals_the_goblin_authored_society` loses its goblin.**
It becomes an assertion that the fallback consumers resolve equals the manikin.
The word "goblin" does not appear in it.

## 4. Preregistered

The campaign's substantive claim is checkable, so it is frozen here before the
code that would move it.

**Prediction: zero artifact drift.** No authored value changes, so no derived
value changes, so no committed artifact changes. After the implementation,
`git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
docs/audits/` returns clean, and `make gate` is green.

**What falsification would mean.** If any artifact moves, the premise that the
baseline is *only* a naming convention is false — something derives from the
identity of the baseline kind rather than from its values, and the campaign is
materially larger than specified. That is a finding worth the campaign on its
own, and it stops work for a re-spec rather than being absorbed by re-pinning the
drifted artifact. Re-pinning a golden to rescue this prediction is forbidden.

**What the null proves.** A clean diff proves the weld was documentary rather
than structural — that no derivation ever read "which kind is the baseline." It
does not prove the weld was harmless, only that removing it is free. The harm
BIO-39 describes is latent and arrives with the roster, which is C2's business.

## 5. Non-goals

- **Authoring humans, elves, or dwarves.** That is C2, and it is an epoch.
- **Characterizing goblin.** Moving goblin off the manikin's values changes
  goblin's language envelope, culture rungs, and demography weights, and so
  changes every world that has goblins — an epoch declaration, a census regen on
  lefford, and golden re-pins. This campaign makes that decision *possible* by
  decoupling it from the baseline question; it does not take it. Filed as an
  idea-registry row.
- **Touching `biosphere_registry()` or the roster.** Nothing about who lives in
  the world changes.
- **Widening any vector.** Each vector type's doc reserves widening to its own
  campaign; that stands.
- **Per-dimension anchors** (the scale's 40 % rung). The observation that human
  is well-founded for articulation and poorly-founded for perception is real, but
  it is an argument about where *humans* are authored, which is C2's decision to
  make against a manikin that already exists.

## 6. Verification

1. `cargo test -p hornvale-species` — the retargeted tests, scoped, during
   iteration.
2. `make gate` — the commit gate. Budget `timeout: 3600000`; measured 22–37 min
   on this Mac (2026-07-30), not the ~4 min decision 0040 budgeted.
3. `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` — folded into
   `make gate`, but the three new consts are the reason to watch it. Tag grammar
   is `bare-ok(<class>)`; the struct fields already carry `bare-ok(ratio)`.
4. `make rebaseline` then the §4 drift diff — the preregistered readout. The
   type-audit report in `docs/audits/` is in the diff set and is the commonly
   missed one.
5. One gating agent at a time on this Mac (0086 / `CLAUDE.md`) — a single
   `make ci` already reports `cpu_ratio` 8.25–8.50 on ten cores.

## 7. Definition of Done

- [ ] Three `MANIKIN` consts; `SocietyVector::baseline()` gone. Five production
      call sites update (`cli/src/phonology.rs` ×3, `cli/src/audio.rs:41`,
      `windows/worldgen/src/lib.rs:11518`); the sixth occurrence is in the test
      D6 rewrites.
- [ ] Module doc (`lib.rs:7`) and the three vector docs (`:155`, `:171`, `:197`)
      no longer define the midpoint in terms of a people.
- [ ] Tests retargeted per D5 and D6.
- [ ] `type-audit:` tags on the new consts; `#![warn(missing_docs)]` satisfied.
- [ ] `book/src/domains/species.md` — "The closed vector, baseline goblin"
      section rewritten; the dimension table's "Goblin (baseline)" column
      renamed. **Chapter H1s in Gallery/Reference are code-generated; this one is
      hand-authored, so it is edited directly.**
- [ ] A short book section introducing the manikin — the reference body that is
      nobody, its lineage (CIE standard observer, ICRP standard man), and the
      rule that the identity element is a fiat forced by determinism.
- [ ] Chronicle entry (`book/src/chronicle/the-manikin.md`). **This spec cites
      registry IDs freely and the book may not**: `docs_consistency`'s
      `the_book_carries_no_registry_ids_or_process_vocabulary` permits them only
      in The Frontier part, and it also bans a set of process words outright. The
      chronicle and the new book section must name the *concept* — "a unit is not
      a frame", "the roster grid" — never `BIO-39` or `BIO-37`.
- [ ] Freshness sweep of chapters naming the goblin baseline —
      `book/src/domains/language.md` and `book/src/introduction.md` both do.
- [ ] Retrospective (`docs/retrospectives/the-manikin.md`), decision 0020.
- [ ] Idea-registry rows for C2, C3, and goblin characterization.
- [ ] §4's preregistered diff run and its result recorded in the chronicle,
      whichever way it came out.

## 8. Flagged for review

**None of this campaign is a save-format, epoch, or determinism-contract
change** — that is its whole design, and §4 is the check that the claim is true
rather than merely intended. The flagged items are therefore about what this
campaign *enables* rather than what it does:

1. **C2 is an epoch and a census regen.** Adding settling peoples changes the
   settlement-genesis roster and so moves every world. Census regen is a
   carve-out requiring explicit authorization (0081, and it runs on lefford per
   0079/0086). Not requested here.
2. **Goblin characterization is deferred, not declined.** §5 states the cost. It
   should ride a campaign already paying for an epoch rather than buy one alone.
3. **The name "manikin" is reversible and mildly at risk.** It is a plausible
   future construct kind in a world that already has `MetabolicClass::Ametabolic`
   and treants. Surfaced to the owner; adopted anyway.
4. **New public surface** — three consts where there was one `const fn`. The two
   new ones formalize conventions that previously existed only as prose, which is
   an argued gain (D3) rather than an incidental addition.
