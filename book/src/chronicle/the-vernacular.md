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
