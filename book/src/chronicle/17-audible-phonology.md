# Campaign 17: Audible Phonology

**July 2026 · 10 commits · outcome: complete, merged — explored under an
explicit abandon gate, and the gate said go: the world's names can now be
heard**

## What was attempted

Campaign 16 gave the world's two peoples real phonologies and proved the
difference on the page — a goblin name and a kobold name that could never be
confused for each other, each rendered two ways from one feature bundle: a
plain-script spelling for the almanac and an IPA transcription for anyone
asking how it is actually said. But IPA on the phonology reference page is
correct and silent. A reader who does not already read the alphabet learns
that **Zvetnot** is `/zvetnot/` and is no closer to hearing it than before.
This campaign set out to add a third surface view over the same `Segment` —
one shaped not for the eye but for a speech synthesizer — and to spend it on
small committed audio clips, so that the sample names on the book's phonology
page could be played rather than only parsed. The concern was named up front:
a synthesizer built for English might render invented kobold names badly
enough that the machinery would not be worth its weight, and the campaign
was staged so that everything past the view itself could be abandoned if the
listen test failed.

## What landed

**A third view, sitting beside romanization and IPA.** The founding rule of
the phoneme module — a sound is a feature bundle, never a string, and every
spelling is a view computed on demand — takes a third tenant. `espeak(seg)`
renders a segment as its espeak-ng phoneme mnemonic, the Kirshenbaum-style
notation the synthesizer's direct-input mode reads, exhaustively over the
same curated inventory the other two views cover. Romanization is for the
almanac, IPA is for the eye, and espeak is for the ear: three renderings of
one underlying truth, none of them stored, none of them able to drift from
the others because there is still only ever one fact beneath all three. Five
of the mnemonics differ from the IPA symbol where the synthesizer's notation
demands it — the postalveolar sibilants `ʃ`/`ʒ` become `S`/`Z`, the velar
nasal `ŋ` becomes `N` — and two are documented approximations, discussed
below.

**A word is assembled, not just transcribed.** A view over a single segment
is not yet something a synthesizer can voice; a name is a sequence, and a
sequence needs stress. `espeak_word` concatenates a name's segment mnemonics
and inserts an explicit `'` stress marker before the first vowel, then wraps
the whole in the `[[…]]` brackets that put espeak-ng into direct phoneme
input — bypassing its English letter-to-sound rules entirely, which is the
only way an invented name gets pronounced as written rather than guessed at.
So **Zvetnot** becomes `[[zv'etnot]]`. The explicit stress keeps the
formulation self-contained rather than leaning on the synthesizer's automatic
stress assignment, which has no basis for a word it has never seen. The
formulation joins romanization and IPA as a third rendering every
`GeneratedName` now carries, built in the same render pass.

**A filename that is a checksum of the sound.** Audio clips are named by the
CRC-32 (IEEE, the zlib polynomial) of the espeak formulation string, rendered
as eight lowercase hex digits: `[[zv'etnot]]` hashes to `b98f6a7c.mp3`. The
checksum is the freshness mechanism, and it is a strict one. Any change to
the phonology that alters a name's formulation — a new segment mnemonic, a
different stress rule, a shifted inventory — changes the hash, which changes
the referenced filename, which the phonology page now points at a clip that
does not exist. A single set-equality test reads the committed page for its
`../audio/<crc32>.mp3` references and asserts the audio directory holds
exactly that set: every referenced clip present, no orphan clips lingering.
It never compares a waveform to anything. espeak-ng makes no cross-version
promise that the same input yields the same bytes, and the test does not need
one — content-addressing catches a phonology change through the *name* the
content computes, not through the audio the synthesizer happens to produce.
The CRC-32 is hand-rolled, a table-free bitwise loop of about twenty lines
with a known-answer test pinning `crc32(b"123456789") == 0xcbf43926`, because
the no-new-crates rule holds here as everywhere.

**Authoring offline, the way the world already authors.** The clips are
produced by a new `hornvale voice` subcommand, and it is an authoring tool,
not a runtime one. It walks the same species-by-sample-name set the phonology
page renders, and for each formulation whose clip is missing it runs
espeak-ng (voice `en`, 130 words per minute) to a temporary wav and ffmpeg
(libmp3lame, quality 4) to the committed mp3 — the two settings pinned as
constants, the two tools the only places in the workspace that shell out to
anything external. Existing clips are never regenerated; content-addressing
makes them write-once. This is the "models author, dice roll" pattern
(decision 0009) applied to sound instead of to a name bank: an external tool
runs offline on a human's machine, its output is committed to the repository,
and CI never executes it — the freshness gate reasons only about the
committed page and the committed clips, never about espeak-ng or ffmpeg being
installed. Eight clips, roughly seventy kilobytes, is the whole binary weight
this campaign adds to the book.

**Two approximations, the espeak analogue of the digraphs.** Two segments in
the world's inventories fall outside what voice `en` can voice, and each gets
a documented nearest form rather than silence. The click `ǃ` becomes `tS`,
because voice `en` silently drops a bare `!`; the ejective `kʼ` becomes `k`,
because `'` is the synthesizer's stress marker and cannot appear inside a
phoneme. Both are audible, both are wrong, and both are recorded as wrong in
the module's own docs — the exact trade the romanization already makes when
it spells a click `ts` and an ejective `kx`, a segment outside the target
notation's coverage given a defined nearest neighbor instead of being forced
into a symbol that lies about it. Neither approximation surfaces in seed 42's
sample names — no people in that world claims a click or an ejective — but
the mapping is exhaustive over the inventory, ready the day a species anatomy
calls for one.

## What was learned

- **A gate you might not pass changes how you build up to it.** The campaign
  was exploratory by construction: the spec declared that nothing past the
  view function merges if the listen test fails, and the audio machinery — the
  checksum, the columns, the subcommand — was built only *after* eight clips
  were generated by hand and a human confirmed they sounded worth the weight.
  The order was the point. Committing to the machinery first and testing the
  sound afterward would have made the abandon decision expensive enough to
  bias it toward "keep"; testing the sound first made "go" a real choice
  rather than a foregone one.
- **Validating the tool before the spec is written surfaces its limits early
  enough to design around them.** espeak-ng's direct phoneme input was
  exercised on the command line before a word of the spec existed, and that
  is where the click and ejective limitations turned up — while they were
  still a design input rather than a shipped surprise. The two approximations
  were written into the spec as deliberate, documented choices from the
  start, mapped onto a precedent the romanization had already set, instead of
  being discovered by a reader whose clip played the wrong sound.
- **Content-addressing lets a binary artifact live under a freshness gate
  without a byte-comparison.** The hard problem with committing generated
  audio is that a synthesizer's output is not reproducible across versions,
  so a gate that compared waveforms would fail on a machine that merely had a
  different espeak-ng installed. Naming the clip by the checksum of its
  *input* sidesteps this entirely: the gate reasons about the deterministic
  formulation string, which the sim computes and CI can recompute, and treats
  the audio itself as opaque presentation-layer weight it never inspects.

## Deferred, deliberately

Per-segment audio on the inventory tables, scoped out on purpose — this
campaign made the sample *names* audible and left the individual phonemes
silent. A trill that actually trills: voice `en` renders `r` as an English
approximant, so a kobold name loses its rolled r, and recovering it is a
one-constant change to a different pinned voice that no one has yet chosen and
tuned. Any richer text-to-speech route than a phoneme synthesizer — a
neural voice, a per-species timbre, prosody that carries a myth's formality
the way the render seam already carries it in text — all of which the espeak
view is a substrate for and none of which this campaign attempts. And the
paralinguistic layer Campaign 16 already deferred, the calls and cries that
are a different kind of sound than speech, still waiting on the campaign
willing to author them.

## Artifacts

[The Phonology reference page](../reference/phonology.md) — now carrying an
Espeak column and an Audio column on every sample-name table, the clips
played inline. [The Language chapter](../domains/language.md) — the phoneme
model, now with three views over `Segment` rather than two. `hornvale voice`
— the offline authoring subcommand, run by a human, its clips committed and
CI never executing it. `cli/tests/audio_artifacts.rs` — the set-equality
test that keeps the committed audio directory exactly the phonology page's
referenced set, catching a phonology change through the checksum it renames a
clip by, never through a waveform.
