# Audible Phonology — espeak view and committed audio artifacts

**Date:** 2026-07-08
**Status:** Exploratory — see the abandon gate in §7. Nothing in this spec
merges to main until the listen test passes.

## 1. Motivation

IPA on the phonology reference page is correct but silent: there is no way
to *hear* a generated name. This campaign adds a third surface view over
`Segment` — the espeak-ng phoneme formulation — and uses it to author small
committed audio clips, making the sample names on the book's phonology page
audible.

The espeak formulation, like romanization and IPA, is a **view, never
stored** (the phoneme module's founding rule). Audio is presentation-layer
authoring output, exactly like the model-authored banks: an external tool
runs offline, its output is committed, and the sim core never touches it
(decision 0009, "models author, dice roll").

## 2. The `espeak()` view

`domains/language/src/phoneme.rs` gains a third exhaustive view function
beside `romanize()` and `ipa()`:

```rust
/// Render a segment as its espeak-ng phoneme mnemonic (Kirshenbaum-style),
/// for the offline audio authoring step.
pub fn espeak(seg: &Segment) -> &'static str
```

The mapping over the curated 28-segment inventory (verified against
espeak-ng 1.52.0, voice `en`, via `espeak-ng -x "[[...]]"`):

| Segments | espeak mnemonic |
|---|---|
| p b t d k g q, f v x, s z, m n, l w | same letter as romanization |
| ʃ / ʒ (postalveolar sibilants) | `S` / `Z` |
| ŋ (velar nasal) | `N` |
| r (trill) | `r` (voice `en` renders an approximant; accepted, see §8) |
| j (postalveolar approximant) | `j` |
| ǃ (click) | `tS` — **approximation**; `!` is silently dropped by voice `en` |
| kʼ (ejective) | `k` — **approximation**; `'` is espeak's stress marker and cannot appear in a phoneme |
| i e a o u | same letter |

The click and ejective approximations follow the romanization-digraph
precedent (`ts`, `kx`): a segment outside the target notation's coverage
gets a documented nearest form in the module docs. Segments outside the
curated inventory return `"?"`, matching the other two views.

**Word assembly:** a word's formulation is the concatenation of its
segments' mnemonics with a `'` stress marker inserted before the first
vowel, wrapped for espeak's direct phoneme input: `[[zv'etnot]]`. Explicit
stress keeps the formulation self-contained rather than relying on
espeak's automatic stress assignment. Assembly lives beside the view (a
`espeak_word(&[Segment]) -> String` helper in the same module).

## 3. Content-addressed artifact names

Audio filenames are the CRC-32 (IEEE, the zlib polynomial) of the word's
espeak formulation string (the `[[...]]` form, UTF-8 bytes), rendered as
eight lowercase hex digits:

```
book/src/audio/{crc32:08x}.mp3
```

CRC-32 is hand-rolled (~20 lines, table-free bitwise form, std-only — the
no-new-crates rule holds). It lives in the CLI beside its only consumer,
with a known-answer test (`crc32(b"123456789") == 0xcbf43926`).

Content-addressing is the freshness mechanism: if a phonology change alters
a formulation, the referenced filename changes, and the existence test
(§6) fails until the authoring step is re-run. No waveform is ever
byte-compared.

## 4. The phonology page

The generated `book/src/reference/phonology.md` (via `hornvale phonology`)
changes in the **Sample names** tables only:

- a new **Espeak** column showing the formulation (`[[zv'etnot]]`),
- a new **Audio** column with an inline `<audio controls preload="none"
  src="../audio/<crc32>.mp3"></audio>` element (mdbook passes raw HTML
  through).

The inventory tables are unchanged (no per-segment audio in this campaign
— scope was deliberately pinned to sample names).

## 5. Offline authoring: `hornvale voice`

A new CLI subcommand:

```
hornvale voice --world world.json [--out book/src/audio]
```

walks the same species × sample-name set the phonology page renders, and
for each formulation whose `<crc32>.mp3` is missing from the output
directory:

1. `espeak-ng -v en -s 130 -w <tmp>.wav "[[...]]"`
2. `ffmpeg -y -loglevel error -i <tmp>.wav -codec:a libmp3lame -qscale:a 4 <out>/<crc32>.mp3`

Voice, speed, and encoder settings are pinned as constants in the
subcommand. Existing files are never regenerated (content-addressing makes
them immutable). If `espeak-ng` or `ffmpeg` is missing from `PATH`, the
command fails loudly naming the missing tool. Temp files go to a
process-scoped temp directory.

This is the only place in the workspace that shells out to either tool.
It is an authoring command, run by a human, its output committed — CI
never executes it.

Verified end-to-end on espeak-ng 1.52.0 + ffmpeg (Homebrew): one word ≈
9 KB of mp3; the seed-42 sample set (~12 words) ≈ 100–150 KB committed.

## 6. CI freshness

- The existing artifact drift check already regenerates the phonology page;
  espeak formulations and crc32 names are deterministic, so drift in the
  page is caught today with zero new CI steps.
- One new test (in `cli/tests/`) parses the committed phonology page for
  `src="../audio/([0-9a-f]{8})\.mp3"` references and asserts each file
  exists in `book/src/audio/`. Orphaned audio files (committed but no
  longer referenced) are reported as failures too, keeping the directory
  exactly the referenced set.
- CI does **not** install espeak-ng or ffmpeg and never compares audio
  bytes: espeak-ng makes no cross-version waveform-stability promise, and
  the artifacts are presentation-layer.

## 7. Staging and the abandon gate

The exploration concern is explicit: espeak's rendering may not sound good
enough to justify the machinery, in which case everything past Stage 1 is
abandoned.

1. **Stage 1 — the `espeak()` view.** View function, word assembly, unit
   tests. Valuable standalone (a third notation, and the substrate for any
   future TTS route). Kept even if audio is abandoned.
2. **Stage 2 — listen test (the gate).** Generate audio for the seed-42
   sample names with the Stage-4-shaped pipeline run by hand; the user
   listens. **Go:** proceed. **No-go:** delete the branch beyond Stage 1;
   record the outcome in the idea registry.
3. **Stage 3 — crc32 + page changes.** Hand-rolled crc32 with known-answer
   test; Espeak and Audio columns in the generated page.
4. **Stage 4 — `hornvale voice`.** The authoring subcommand.
5. **Stage 5 — freshness test + book sweep.** Existence/orphan test;
   regenerate committed artifacts; chronicle entry per Definition of Done.

## 8. Known limits (accepted)

- Voice `en` renders `r` as an English approximant, not a trill; kobold
  names lose their rolled r. Acceptable for a first pass; a different
  pinned voice is a one-constant change.
- Click and ejective are pulmonic approximations (§2) — audible, wrong,
  documented. Same trade the romanization already makes.
- Regenerating audio on a machine with a different espeak-ng version
  produces different bytes for the *same* filename. Accepted: files are
  write-once in practice (content-addressed, never regenerated while
  present), and no gate compares bytes.

## 9. Testing summary

- `espeak()` exhaustive-view unit tests extending the existing
  distinct-views test; word-assembly test including stress placement.
- crc32 known-answer test.
- Phonology-page golden output already covered by the artifact drift check.
- Audio existence/orphan test (§6).
- No test executes espeak-ng or ffmpeg.
