# Campaign 17 (Audible Phonology) — retrospective

**Merged:** 2026-07-08

**What the abandon gate bought, and what it cost.** The campaign was staged
around an explicit listen-test gate (spec §7): Stage 1 shipped the `espeak()`
view standalone, and everything past it — checksum naming, the page columns,
the `voice` subcommand — was contingent on a human confirming, after Stage 1,
that hand-generated clips sounded worth the binary weight. The cost was real
and small: eight clips had to be produced by hand before any authoring
machinery existed to produce them, throwaway work in the sense that the
`voice` subcommand later reproduced it, but not wasted, because those exact
clips are what the human listened to at the gate. What it bought was a genuine
abandon option. Had the audio machinery been built first and the sound tested
afterward, the sunk cost of the machinery would have pressed the decision
toward "keep it, it's already written"; testing the sound first made GO a
choice made on the sound's merits rather than on the code's. The lesson worth
carrying: an abandon gate only functions if the expensive, hard-to-discard
work sits *after* it, not before — order the stages so the cheap disposable
prototype is what the gate judges, and the gate stays honest.

**Validating the tool before the spec changed the design, not just the
confidence.** espeak-ng's direct phoneme input (`[[…]]`) was exercised on the
command line before the spec was written, and that ordering is why the click
and ejective limitations are documented approximations in §2 rather than
shipped surprises. Two concrete design decisions trace directly to
pre-spec validation: the click maps to `tS` because the command line showed
voice `en` silently drops a bare `!`, and the ejective maps to `k` because
the command line showed `'` is reserved as the stress marker and cannot sit
inside a phoneme. Neither is a limitation a reading of espeak-ng's docs would
have surfaced as sharply as one command that produced the wrong sound. The
approximations were then written into the spec against the precedent the
romanization digraphs already set (`ts`, `kx`), so the design absorbed the
tool's limits as deliberate choices instead of discovering them at review or,
worse, in a reader's ear. Same pattern as earlier campaigns' "validate the
external assumption before committing to a shape," now applied to an external
*tool* rather than an external formula.

**Committing binary artifacts to the book — content-addressing is the whole
trick.** The standing worry with committed generated audio is that a
synthesizer makes no cross-version byte-stability promise, so any freshness
gate that compared waveforms would fail on a machine that merely had a
different espeak-ng installed — turning a presentation-layer artifact into a
reproducibility burden the "same seed, byte-identical world" contract never
asked for. Naming each clip by the CRC-32 of its espeak *formulation* — the
deterministic string the sim computes, not the audio the synthesizer emits —
resolves this cleanly: the drift check reasons about the committed page and
the committed filenames, both recomputable anywhere, and treats the mp3 bytes
as opaque. The set-equality test (`cli/tests/audio_artifacts.rs`) never opens
a clip; it asserts the audio directory is exactly the set of hashes the page
references, so a phonology change is caught through the *rename* it forces,
and an orphaned clip is caught through the reference it lacks. This is the
reusable result: a binary artifact can live under a strict freshness gate
without a byte-comparison, provided its name is a checksum of a deterministic
input the gate can independently recompute. Weight stayed negligible — eight
clips, ~70 KB — which the scope pin (sample names only, no per-segment audio)
kept true.

**Do differently next time.** Nothing surfaced that demands a process change;
the gate held, the validation-first ordering paid off, and the freshness
mechanism worked as specified. The one thing worth naming for the next
campaign that commits generated binaries: decide the content-addressing
scheme in the spec, not the implementation, because it is the choice that
determines whether the artifact is gate-able at all — here it was §3, and
having it fixed before Stage 3 meant the page columns and the existence test
were designed against a naming contract rather than around an ad-hoc one.
