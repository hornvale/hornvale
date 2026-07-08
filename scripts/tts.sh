#!/usr/bin/env bash
# scripts/tts.sh — ungated offline text-to-speech convenience for hearing
# generated Hornvale names and epithets spoken aloud (spec §10, Campaign
# Y2-3, The Tongues).
#
# This is NEVER part of the CI gate and NEVER produces a committed audio
# artifact — it is a developer/author convenience for sanity-checking the
# `hornvale phonology` page's IPA transcriptions by ear, nothing more.
#
# It wraps espeak-ng (https://github.com/espeak-ng/espeak-ng), driven at the
# PHONEME level rather than the text level: feeding a name's surface spelling
# to espeak-ng's ordinary English-grapheme reader would apply English
# letter-to-sound rules to a language that has none, so this script instead
# takes the IPA transcription `hornvale phonology` already prints for every
# sample name and speaks it directly. espeak-ng has no native Unicode IPA
# input mode, so the IPA string is first translated into Kirshenbaum ASCII
# phonetic notation (the scheme espeak-ng natively parses inside `[[ ... ]]`
# brackets) — the fallback the spec anticipates ("Kirshenbaum notation
# fallback if Unicode IPA input proves awkward").
#
# The Kirshenbaum mapping below covers exactly the curated IPA glyph set
# `hornvale_language::phoneme::ipa` is exhaustive over (see
# domains/language/src/phoneme.rs); it is a best-effort acoustic proxy, not a
# claim of phonetic precision — the ejective (kʼ) and click (ǃ) glyphs have
# no clean Kirshenbaum/espeak-ng equivalent and are approximated as their
# nearest plain consonant.
#
# Usage:
#   scripts/tts.sh [options] <ipa-string>
#   echo "/zvetnot/" | scripts/tts.sh
#
# Options:
#   -k, --kirshenbaum   Treat the input as already-Kirshenbaum ASCII (skip
#                        the IPA translation step).
#   -t, --text           Treat the input as ordinary text (skip phoneme mode
#                        entirely; espeak-ng's default grapheme reader —
#                        useful only for spot-checking espeak-ng itself, not
#                        for hearing a generated name correctly).
#   -v, --voice VOICE    espeak-ng voice to use (default: en; Hornvale has no
#                        per-species voice — this is a generic proxy).
#   -o, --out FILE.wav   Write a .wav instead of playing through speakers.
#   -h, --help           Show this help and exit.
#
# Examples:
#   scripts/tts.sh "zvetnot"                  # plain IPA-ish string
#   scripts/tts.sh "/ŋodsvedsxak/"            # slashes are stripped
#   scripts/tts.sh -k "[[zv'EtnQt]]"          # already Kirshenbaum
#   cargo run -p hornvale -- phonology | \
#     grep -m1 Settlement | awk -F'/' '{print $2}' | scripts/tts.sh
#
# Install espeak-ng (never a workspace dependency — this script alone uses
# it, and only outside CI):
#   brew install espeak-ng        # macOS
#   apt install espeak-ng         # Debian/Ubuntu

set -euo pipefail

usage() {
    sed -n '2,45p' "$0" | sed 's/^# \{0,1\}//'
}

mode="ipa"
voice="en"
outfile=""
input=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        -k|--kirshenbaum)
            mode="kirshenbaum"
            shift
            ;;
        -t|--text)
            mode="text"
            shift
            ;;
        -v|--voice)
            voice="$2"
            shift 2
            ;;
        -o|--out)
            outfile="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        --)
            shift
            break
            ;;
        -*)
            echo "tts.sh: unknown option: $1" >&2
            usage >&2
            exit 2
            ;;
        *)
            input="$1"
            shift
            ;;
    esac
done

if [[ -z "$input" ]]; then
    if [[ -t 0 ]]; then
        echo "tts.sh: no input given (pass an argument or pipe one on stdin)" >&2
        usage >&2
        exit 2
    fi
    input="$(cat)"
fi

if ! command -v espeak-ng >/dev/null 2>&1; then
    cat >&2 <<'EOF'
tts.sh: espeak-ng not found on PATH.
Install it (this script's only dependency; never a workspace crate):
  brew install espeak-ng        # macOS
  apt install espeak-ng         # Debian/Ubuntu
EOF
    exit 1
fi

# Strip IPA slash/bracket delimiters (`hornvale phonology` prints /.../).
input="${input#/}"
input="${input%/}"
input="$(printf '%s' "$input" | tr -d '\n')"

# IPA -> Kirshenbaum, over exactly the curated glyph set
# hornvale_language::phoneme::ipa is exhaustive over. Only four glyphs in
# that set differ from their Kirshenbaum spelling (kʼ, ǃ, ʃ, ʒ, ŋ); the rest
# (p b t d k g q f v x s z m n r l j w i e a o u) are already plain ASCII
# and pass through unchanged, so no substitution is needed for them.
ipa_to_kirshenbaum() {
    local s="$1"
    s="${s//kʼ/k}"   # ejective: no clean Kirshenbaum equivalent, plain stop
    s="${s//ǃ/k}"    # click: no clean Kirshenbaum equivalent, plain stop
    s="${s//ʃ/S}"
    s="${s//ʒ/Z}"
    s="${s//ŋ/N}"
    printf '%s' "$s"
}

espeak_args=(-v "$voice")
if [[ -n "$outfile" ]]; then
    espeak_args+=(-w "$outfile")
fi

case "$mode" in
    text)
        espeak-ng "${espeak_args[@]}" -- "$input"
        ;;
    kirshenbaum)
        espeak-ng "${espeak_args[@]}" -- "[[${input}]]"
        ;;
    ipa)
        kirshenbaum="$(ipa_to_kirshenbaum "$input")"
        espeak-ng "${espeak_args[@]}" -- "[[${kirshenbaum}]]"
        ;;
esac
