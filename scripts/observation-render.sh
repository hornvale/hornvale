#!/usr/bin/env bash
# observation-render.sh — rasterize validated Atlas observation packets locally.
set -euo pipefail

readonly VIEWPORT_WIDTH=1440
readonly VIEWPORT_HEIGHT=900

usage() {
    echo "usage: scripts/observation-render.sh --manifest PATH --frames DIR" >&2
}

die() {
    echo "observation-render: $*" >&2
    exit 2
}

manifest=""
frames=""
while [ "$#" -gt 0 ]; do
    case "$1" in
        --manifest|--frames)
            option="$1"
            [ "$#" -ge 2 ] || { usage; die "$option requires a value"; }
            value="$2"
            case "$option" in
                --manifest) manifest="$value" ;;
                --frames) frames="$value" ;;
            esac
            shift 2
            ;;
        *) usage; die "unexpected argument '$1'" ;;
    esac
done

[ -n "$manifest" ] || { usage; die "--manifest must not be empty"; }
[ -n "$frames" ] || { usage; die "--frames must not be empty"; }
[ -f "$manifest" ] || die "manifest does not exist: $manifest"
[ -d "$frames" ] || die "frame directory does not exist: $frames"

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)"
command -v jq >/dev/null 2>&1 || die "jq is required"
command -v deno >/dev/null 2>&1 || die "Deno is required to compose Atlas preview HTML"

if [ -n "${HV_OBSERVATION_FIREFOX:-}" ]; then
    firefox="$HV_OBSERVATION_FIREFOX"
elif command -v firefox >/dev/null 2>&1; then
    firefox="$(command -v firefox)"
elif [ -x /Applications/Firefox.app/Contents/MacOS/firefox ]; then
    firefox=/Applications/Firefox.app/Contents/MacOS/firefox
else
    die "Firefox headless screenshot backend is unavailable; install Firefox or set HV_OBSERVATION_FIREFOX"
fi
if [[ "$firefox" == */* ]]; then
    [ -x "$firefox" ] || die "Firefox headless screenshot backend is unavailable: $firefox"
elif ! command -v "$firefox" >/dev/null 2>&1; then
    die "Firefox headless screenshot backend is unavailable: $firefox"
fi

manifest_abs="$(cd "$(dirname "$manifest")" && pwd -P)/$(basename "$manifest")"
frames_abs="$(cd "$frames" && pwd -P)"
cwd_abs="$(pwd -P)"
case "$frames_abs" in
    /|"$cwd_abs") die "refusing broad frame target: $frames_abs" ;;
esac

(
    cd "$root"
    cargo run --quiet -p hornvale -- observations validate --manifest "$manifest_abs" >/dev/null
) || die "manifest validation failed: $manifest_abs"

episode_id="$(jq -er '.id | strings | select(length > 0)' "$manifest_abs")" \
    || die "validated manifest has no episode id"
case "$episode_id" in
    ""|.|..|.*|-*|*[!A-Za-z0-9_-]*)
        die "manifest id must be a safe filename component: $episode_id"
        ;;
esac
world_seed="$(jq -er '.seed | tostring' "$manifest_abs")" \
    || die "validated manifest has no seed"
world_revision="$(jq -er '.world_revision | strings | select(length > 0)' "$manifest_abs")" \
    || die "validated manifest has no world revision"
frame_count="$(jq -er '.frame_count | numbers | select(. > 0 and floor == .)' "$manifest_abs")" \
    || die "validated manifest has no positive frame count"

png_dimensions() {
    local png="$1"
    local signature ihdr width_hex height_hex width height
    signature="$(od -An -tx1 -N 8 "$png" | tr -d '[:space:]')"
    ihdr="$(od -An -tx1 -j 12 -N 4 "$png" | tr -d '[:space:]')"
    [ "$signature" = 89504e470d0a1a0a ] && [ "$ihdr" = 49484452 ] \
        || die "Firefox did not produce a PNG: $png"
    width_hex="$(od -An -tx1 -j 16 -N 4 "$png" | tr -d '[:space:]')"
    height_hex="$(od -An -tx1 -j 20 -N 4 "$png" | tr -d '[:space:]')"
    [ "${#width_hex}" -eq 8 ] && [ "${#height_hex}" -eq 8 ] \
        || die "Firefox PNG header is truncated: $png"
    printf -v width '%d' "0x$width_hex"
    printf -v height '%d' "0x$height_hex"
    printf '%s %s\n' "$width" "$height"
}

work_dir="$(mktemp -d "$frames_abs/.observation-render.XXXXXX")"
trap 'rm -rf "$work_dir"' EXIT

shopt -s nullglob
packet_files=("$frames_abs"/frame-*.json)
[ "${#packet_files[@]}" -gt 0 ] || die "no frame packets found in $frames_abs"
[ "${#packet_files[@]}" -eq "$frame_count" ] \
    || die "expected $frame_count contiguous frame packets, found ${#packet_files[@]}"

source_digest=""
for ((index = 0; index < frame_count; index++)); do
    printf -v packet_name 'frame-%03d.json' "$index"
    packet="$frames_abs/$packet_name"
    [ -f "$packet" ] || die "expected contiguous frame packets; missing $packet_name"
    jq -e . "$packet" >/dev/null 2>&1 || die "packet is not valid JSON: $packet_name"

    packet_index="$(jq -er '.frame_index | numbers | select(floor == .)' "$packet")" \
        || die "packet frame_index is missing or invalid: $packet_name"
    [ "$packet_index" -eq "$index" ] \
        || die "packet frame_index mismatch in $packet_name: expected $index, found $packet_index"
    packet_episode="$(jq -er '.episode_id | strings' "$packet")" \
        || die "packet episode_id is missing: $packet_name"
    [ "$packet_episode" = "$episode_id" ] \
        || die "packet episode_id mismatch in $packet_name: expected $episode_id, found $packet_episode"
    packet_seed="$(jq -er '.world_seed | strings' "$packet")" \
        || die "packet world_seed is missing: $packet_name"
    [ "$packet_seed" = "$world_seed" ] \
        || die "packet world_seed mismatch in $packet_name: expected $world_seed, found $packet_seed"
    packet_revision="$(jq -er '.world_revision | strings' "$packet")" \
        || die "packet world_revision is missing: $packet_name"
    [ "$packet_revision" = "$world_revision" ] \
        || die "packet world_revision mismatch in $packet_name: expected $world_revision, found $packet_revision"
    packet_digest="$(jq -er '.source_digest | strings | select(length > 0)' "$packet")" \
        || die "packet source_digest is missing: $packet_name"
    if [ -z "$source_digest" ]; then
        source_digest="$packet_digest"
    elif [ "$packet_digest" != "$source_digest" ]; then
        die "packet source_digest mismatch in $packet_name: expected $source_digest, found $packet_digest"
    fi

    html="$work_dir/frame-$(printf '%03d' "$index").html"
    if ! (
        cd "$root"
        deno eval --quiet '
            import { parseObservationFramePacket, renderObservationFrameHtml } from "./clients/atlas/src/observation.ts";
            const [packetPath, htmlPath] = Deno.args;
            const packet = parseObservationFramePacket(await Deno.readTextFile(packetPath));
            await Deno.writeTextFile(htmlPath, renderObservationFrameHtml(packet, { width: 1440, height: 900 }));
        ' "$packet" "$html"
    ); then
        die "packet is not valid Atlas observation input: $packet_name"
    fi
done

for ((index = 0; index < frame_count; index++)); do
    printf -v frame_base 'frame-%03d' "$index"
    staged_png="$work_dir/$frame_base.png"
    "$firefox" --headless --window-size "$VIEWPORT_WIDTH,$VIEWPORT_HEIGHT" \
        --screenshot "$staged_png" "file://$work_dir/$frame_base.html" >/dev/null 2>&1 \
        || die "Firefox failed to rasterize $frame_base.html"
    [ -s "$staged_png" ] || die "Firefox did not produce $frame_base.png"
    read -r width height < <(png_dimensions "$staged_png")
    [ "$width" -eq "$VIEWPORT_WIDTH" ] && [ "$height" -eq "$VIEWPORT_HEIGHT" ] \
        || die "Firefox produced ${width}x${height}; expected ${VIEWPORT_WIDTH}x${VIEWPORT_HEIGHT}"
done

stale_pngs=("$frames_abs"/frame-*.png)
for stale_png in "${stale_pngs[@]}"; do
    rm -f -- "$stale_png"
done
for ((index = 0; index < frame_count; index++)); do
    printf -v frame_base 'frame-%03d' "$index"
    mv "$work_dir/$frame_base.png" "$frames_abs/$frame_base.png"
done

printf 'observation-render: wrote %s Atlas laptop PNG frames at %sx%s for %s\n' \
    "$frame_count" "$VIEWPORT_WIDTH" "$VIEWPORT_HEIGHT" "$episode_id"
