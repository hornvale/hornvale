#!/usr/bin/env bash
# observation-film.sh — verify authoritative packets and optionally assemble local film.
set -euo pipefail

usage() {
    echo "usage: scripts/observation-film.sh --manifest PATH --frames DIR --out DIR" >&2
}

die() {
    echo "observation-film: $*" >&2
    exit 2
}

manifest=""
frames=""
out=""
while [ "$#" -gt 0 ]; do
    case "$1" in
        --manifest|--frames|--out)
            option="$1"
            [ "$#" -ge 2 ] || { usage; die "$option requires a value"; }
            value="$2"
            case "$option" in
                --manifest) manifest="$value" ;;
                --frames) frames="$value" ;;
                --out) out="$value" ;;
            esac
            shift 2
            ;;
        *) usage; die "unexpected argument '$1'" ;;
    esac
done

[ -n "$manifest" ] || { usage; die "--manifest must not be empty"; }
[ -n "$frames" ] || { usage; die "--frames must not be empty"; }
[ -n "$out" ] || { usage; die "--out must not be empty"; }
[ -f "$manifest" ] || die "manifest does not exist: $manifest"
[ -d "$frames" ] || die "frame directory does not exist: $frames"

command -v jq >/dev/null 2>&1 || die "jq is required"
if command -v sha256sum >/dev/null 2>&1; then
    checksum() { sha256sum "$1" | awk '{print $1}'; }
elif command -v shasum >/dev/null 2>&1; then
    checksum() { shasum -a 256 "$1" | awk '{print $1}'; }
else
    die "sha256sum or shasum is required"
fi

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)"
manifest_abs="$(cd "$(dirname "$manifest")" && pwd -P)/$(basename "$manifest")"
frames_abs="$(cd "$frames" && pwd -P)"
cwd_abs="$(pwd -P)"
case "$frames_abs" in
    /|"$cwd_abs") die "refusing broad frame target: $frames_abs" ;;
esac

out_parent="$(dirname "$out")"
out_leaf="$(basename "$out")"
[ "$out_leaf" != "." ] && [ "$out_leaf" != ".." ] || die "refusing broad output target: $out"
[ -d "$out_parent" ] || die "output parent directory does not exist: $out_parent"
out_parent_abs="$(cd "$out_parent" && pwd -P)"
out_abs="$out_parent_abs/$out_leaf"
if [ -d "$out_abs" ]; then
    out_abs="$(cd "$out_abs" && pwd -P)"
fi
case "$out_abs" in
    /|"$cwd_abs") die "refusing broad output target: $out_abs" ;;
esac
case "$out_abs/" in
    "$frames_abs/"*) die "output directory must not be inside the input frame directory" ;;
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
frame_rate="$(jq -er '.frame_rate | numbers | select(. > 0)' "$manifest_abs")" \
    || die "validated manifest has no positive frame rate"

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
done

mkdir -p "$out_abs"
work_dir="$(mktemp -d "$out_abs/.observation-film.XXXXXX")"
trap 'rm -rf "$work_dir"' EXIT
sidecar_tmp="$work_dir/$episode_id.sha256"

printf '%s  manifest/%s\n' "$(checksum "$manifest_abs")" "$(basename "$manifest_abs")" >"$sidecar_tmp"
for ((index = 0; index < frame_count; index++)); do
    printf -v packet_name 'frame-%03d.json' "$index"
    printf '%s  packets/%s\n' "$(checksum "$frames_abs/$packet_name")" "$packet_name" >>"$sidecar_tmp"
done

png_files=("$frames_abs"/frame-*.png)
for png in "${png_files[@]}"; do
    printf '%s  frames/%s\n' "$(checksum "$png")" "$(basename "$png")" >>"$sidecar_tmp"
done

ffmpeg_command="${HV_OBSERVATION_FFMPEG:-ffmpeg}"
video_tmp="$work_dir/$episode_id.mp4"
if command -v "$ffmpeg_command" >/dev/null 2>&1; then
    [ "${#png_files[@]}" -eq "$frame_count" ] \
        || die "ffmpeg is available but expected $frame_count rendered PNG frames, found ${#png_files[@]}"
    for ((index = 0; index < frame_count; index++)); do
        printf -v png_name 'frame-%03d.png' "$index"
        [ -f "$frames_abs/$png_name" ] || die "expected contiguous rendered PNG frames; missing $png_name"
    done
    "$ffmpeg_command" -v error -y -framerate "$frame_rate" \
        -i "$frames_abs/frame-%03d.png" -c:v libx264 -pix_fmt yuv420p \
        -movflags +faststart "$video_tmp"
    [ -s "$video_tmp" ] || die "ffmpeg did not produce a non-empty film"
    printf '%s  video/%s.mp4\n' "$(checksum "$video_tmp")" "$episode_id" >>"$sidecar_tmp"
    mv "$video_tmp" "$out_abs/$episode_id.mp4"
    printf 'observation-film: wrote derived film %s\n' "$out_abs/$episode_id.mp4"
else
    owned_video="$out_abs/$episode_id.mp4"
    if [ -e "$owned_video" ] || [ -L "$owned_video" ]; then
        die "existing derived video prevents ffmpeg-unavailable rerun: $owned_video"
    fi
    printf 'observation-film: ffmpeg unavailable; verified packets and wrote checksum sidecar only\n'
fi

mv "$sidecar_tmp" "$out_abs/$episode_id.sha256"
printf 'observation-film: verified %s packets for %s; checksums: %s\n' \
    "$frame_count" "$episode_id" "$out_abs/$episode_id.sha256"
