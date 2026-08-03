# The Vernacular

**August 2026 · outcome: merged (parts 1–2) — the world's text stops being a
channel, and the registry learns to say "no one here can name this"**

A vernacular is the tongue a place actually speaks, as against the learned
language written over it. Hornvale had the learned language everywhere: the
concept registry, the per-culture lexicon, the clause realizer and the register
seam all existed, and nearly every surface that spoke bypassed all four to emit
English written by hand at compile time.

That much was known. What was not known is that the English had become
*load-bearing*.

## The measurement that started it

`windows/worldgen` decided which concept a phenomenon glossed to — and
therefore what a people's deity was named — by grepping the phenomenon's
display sentence:

```rust
if phenomenon.description.contains("moon") { Some("moon") }
else if phenomenon.description.contains("star") { Some("star") }
else { Some("sun") }
```

Rewording one string from `"a {} moon"` to `"a {} lunar disc"` — a change with
no semantic content — moved **73 committed facts** on seed 42, including nine
of forty-eight deity names. No stream label changed. Nothing in the gate would
have explained why.

Prose was a save-format-class channel that nothing declared and nothing
protected.

## Part 1 — the referent contract

`Phenomenon` gained a `Referent`: a registered concept id plus registered
qualifier concepts, machine-facing, and **the only field a consumer may branch
on**. The field is non-optional, so the compiler forced all 27 construction
sites to declare what their phenomenon is *about*.

The gloss consumers then read the referent instead of the prose, behind a
shared `GLOSSING_KINDS` roster — decision 0094's shape, which landed on main
mid-campaign and governs exactly this: a deliberate duplicate shares its
roster, never its derivation. The lab's second opinion was re-grounded on the
concept registry and the lexicon, sources the gloss path never consults.

The codomain was deliberately *not* widened. Eclipses and tides carry perfectly
good referents and would gloss if listed; teaching them to is a world change,
not a refactor, and doing it here would have made the campaign's own prediction
unfalsifiable.

**Measured, as a matched pair on one tree:** the same null reword now moves
**0** facts. The deity counts landed exactly on the frozen 9-of-48 and
7-of-48 — same mechanism, same magnitude, coupling gone.

## Part 2 — the register nobody had used

Nathan read the `star-class` finding and named what the branch taxonomy could
not: **there are objective facts that must be representable and that no
creature in this world could think, let alone say.** A star has a spectral
class whether or not anyone has invented spectroscopy.

The kernel had modelled this since `manifest.rs` was written. Every concept
carries lexeme, percept and cognition edges, each `Present` or `Absent(Void)`,
and `Void::Unnamed` means precisely *no word realizes this*. `cli/src/concepts.rs`
tallies it into the registry report.

**No domain had ever used it.** The column had read zero for the project's
entire life while `star-class` sat in the ledger as the prose string
`"yellow dwarf (G)"` — for exactly that reason.

Nine spectral classes were registered honestly. The language machinery promptly
contradicted them: every registered concept enters the exposures map, so
proto-goblinoid acquired a word for "yellow dwarf" and each daughter recorded
having *forgotten* it. The reconstructed ancestor spoke of the main sequence.

So `GapReason` gained a third provenance. A lexicon can now say *why* a word is
missing — `gap (unnameable): no culture here has encountered the main sequence`,
distinct from `gap (experiential): goblin has no exposure` — and the unnameable
stopped reserving proto-roots. Unnameable does not mean unspeakable: a concept
with no word must be *circumlocuted*, the way `packs.rs` already gives `sea` as
"many water."

## What the reviews found that the work did not

Four task reviews passed a defect that only whole-branch scope could see.
`manifests` is `#[serde(skip)]`, so on a world loaded from a save the
declaration evaporated and the nine reclassified as `Experiential` — and
`book/src/reference/dictionary-generated.md`, a published page, shipped that
wrong claim as live output. Every guard the campaign wrote built its world
in-process. The hole was the shape of the save boundary.

Two other findings were the same species. A guard reported as proof was
vacuous: deleting the exclusion it protected left all 588 language and worldgen
tests green. And a committed golden turned out to snapshot not merely a
superseded path but a *broken* one — 22 duplicate proto forms, including one
shared by `one` and `yellow-dwarf`, the exact homophony `assign_proto_roots`
exists to prevent, under a module doc claiming to mirror a page it no longer
mirrored.

## The reading

`Void::Unnamed` 9, `Void::Imperceptible` 9 — the same nine concepts on both
edges — against 191 registered concepts, at commit `08b70ba8`. Both columns had
read zero since the correspondence ledger was built.

That is the history, not the fraction. Nine is a floor: only astronomy has
audited its concepts this way. A sample of the existing `Void::Gap` uses found
seven of eight correctly `Gap` — cultures obviously name weather, crops, kin and
gods, and our packs genuinely have not got there. The residue sits in two
blanket loops over mixed populations, where `grass-sward` is our gap but
`hadal-trench` is nameable by nobody here.

And even there `Unnamed` is the wrong repair, which is the sharpest thing the
campaign learned about its own thesis. `Void::Unnamed` is a **static** edge: it
says unnameable in *every* world. A spectral class qualifies. A hadal trench
does not — it is unnameable only because this world's peoples happen to be
terrestrial, contingent per seed, and the exposure classifier already says so
correctly. The registry's static edge and the per-world classifier are
different instruments, and the campaign's first framing blurred them.

## What did not move

Zero committed facts, across 44 commits — verified at every task and again at
the whole-branch review as an *ordered* fact list, not merely a set. No epoch is
owed; decision 0084's measurement came back NO-BUMP honestly.

`star-class` committed `Value::Text("yellow dwarf (G)")` through part 2. Part
3a made the ledger commit the registered concept id instead — the author's
ground-truth register (`SPECTRAL_CLASSES` in `domains/astronomy/src/star.rs`)
still renders the Morgan–Keenan display back out from that id, so nothing
downstream lost information. Six facts moved (one `star-class`, five
`neighbor-class`) and nothing else did. No epoch was owed: a spectral class is
a pure function of an unchanged mass draw, so only the ledger's *spelling*
moved, not the draw that produces it — decision 0084's re-spelling case.

## Part 3b: text stops being stored

The audit that opened this campaign found hand-written English scattered across
two dozen producer sites. What it did not say, and what part 3b establishes, is
*why they clustered there*. `Phenomenon.description` was a `String` written at
emit time, and a producer cannot know who is looking: an `ObserverContext`
carries place, time, lens and position, and by constitutional design carries no
species. So a stored description could only ever be culture-neutral or wrong.
The field's **type** guaranteed the leak. Every producer that wrote one was
obeying the interface it was given.

The field is therefore deleted, not relocated. Rendering moves to the windows,
where a speaker is known.

### Two registers, asymmetric in the campaign's own dimension

Hornvale realizes a sentence two ways. A tongue takes a **concept id**, resolves
it through that people's lexicon, and fails loudly when the people has no word —
a real fact about them. Common, the author's out-of-world register, took a
**word the caller had already chosen**.

That asymmetry is the whole explanation for where the leaks lived. The tongue
path has a seam where *"is this concept sayable?"* can be asked; Common had
none, so every leak entered upstream of the clause layer, in a caller assembling
a string. The field's own documentation recorded the confusion without resolving
it: its doc comment called it a lexeme while its type-audit tag called it an
identifier, and **both were accidentally true**, because concept ids had been
named as English words. The identity map worked until an id was hyphenated, and
each break grew its own bespoke helper — one for species labels, one for
spectral classes, and, for `celestial-body`, none at all, which is how a raw
registry key shipped into the gallery unnoticed beside the prose everyone was
reading instead.

Common now has a declared vocabulary, and the measurement is the interesting
part: across 191 registered concepts, **zero** declared exceptions were needed.
Strip a trailing `-kind`, replace hyphens with spaces, and the naming convention
yields good English for every one — `abyssal-plain`, `coral-reef`,
`temperate-grassland`. The one genuine miss belongs to astronomy and is declared
there. So Common is *total*, and totality is a property of the resolver's
signature rather than of a validating constructor: the translation asymmetry
becomes a type-level fact. Concept to Common is infallible. Concept to a
people's tongue returns a gap — and a gap now always means something true about
the world, never an authoring hole.

### The description was load-bearing on world order

Deleting the field moved something no one predicted. The salience ordering's
tie-break ran `salience → kind → description`, so the prose was quietly serving
as a sort key. With it gone, two lunar eclipses tied on the remaining keys
changed places — and because the belief engine joins committed beliefs to a
re-computed phenomena list **by list position**, two deities swapped periods.
Eight pantheon lines and two agent names moved.

The reword-invariance battery existed precisely to prove that rewording a
description changed nothing. It compared the gloss *after* the ordering ran, so
it never looked at order at all. The coupling it was written to disprove
outlived it, one field over, for the entire life of the test. This is the
campaign's strongest result: a relocated description would have carried that
coupling forward intact and invisible. Only deletion exposed it.

The ordering gained `period_days` and `venue` legs in response, and the comments
that had claimed the tie-break was total were corrected to say what is true —
it is deterministic but partial, and the positional join depends on the sort's
stability rather than on totality.

### What the world says now

The seed-42 almanac's salient phenomena, rendered from referents at the moment
of reading, in the words of the people the document is voiced by:

```
*As reckoned among the bugbears.*

- [0.70] Doa
- [0.29] heat
- [0.17] Doqoo
- [0.05] rain
```

`Doa` is the bugbears' word for the sun; `Doqoo` is wind. `heat` and `rain` are
concepts their tongue has no word for, circumlocuted into Common — so the line
where a people's vocabulary runs out is now *visible in the artifact* instead of
hidden behind an author's English. Qualifiers are ordered by the drawn
per-species headedness that already orders every compound, which is why the two
moons read `Goododo Daboa` and `Dodoa Daboa` rather than in an order someone
chose.

The header line is not decoration. One world now has as many almanacs as it has
peoples, and the committed artifact is a projection that picks one; a projection
whose choice is invisible reads as neutral fact.

### What did not move, and what is honestly still there

Zero committed facts, across all of part 3b — pinned by the whole-world
byte-identity fixture at every task, and confirmed on two further seeds when the
ordering changed. No epoch is owed: nothing about a draw moved, only how the
world is described after it.

The claim this part earns is narrower than "text no longer exists in the
simulation," and the whole-branch review was right to insist on the distinction.
The **phenomenon channel** is text-free, and no path lets a concept id escape
into a phenomenon bullet. But `SkyReport` and `ClimateReport` still carry
description strings assembled by an English renderer inside a domain, and the
almanac prints them verbatim a few lines above the code that closed the
phenomenon leak. They are a parallel channel of the same shape, untouched here.
Naming them is part of the result.
