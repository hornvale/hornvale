#!/usr/bin/env bash
# test-observation-film.sh — bounded verification for local observation assembly.
set -uo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
film="$root/scripts/observation-film.sh"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

pass=0
fail=0
ok() { printf '  ok: %s\n' "$1"; pass=$((pass + 1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail + 1)); }

run_film() {
    local name="$1"
    shift
    HV_OBSERVATION_FFMPEG=missing-observation-ffmpeg \
        bash "$film" "$@" >"$tmp/$name.out" 2>"$tmp/$name.err"
}

expect_refusal() {
    local name="$1"
    local message="$2"
    shift 2
    if run_film "$name" "$@"; then
        bad "$name unexpectedly succeeded"
    elif grep -Fq -- "$message" "$tmp/$name.err"; then
        ok "$name"
    else
        bad "$name did not report '$message': $(cat "$tmp/$name.err")"
    fi
}

manifest="$tmp/episode manifest.json"
jq '.id = "HV-TEST" | .frame_count = 3 | .world_revision = "revision-test"' \
    "$root/observations/episodes/HV-001.json" >"$manifest"

write_packet() {
    local dir="$1"
    local file_index="$2"
    local packet_index="${3:-$file_index}"
    local episode="${4:-HV-TEST}"
    local seed="${5:-42}"
    local revision="${6:-revision-test}"
    local digest="${7:-fnv1a64:test-digest}"
    mkdir -p "$dir"
    jq -n \
        --arg episode "$episode" \
        --argjson index "$packet_index" \
        --arg seed "$seed" \
        --arg revision "$revision" \
        --arg digest "$digest" \
        '{episode_id:$episode,frame_index:$index,world_seed:$seed,world_revision:$revision,source_digest:$digest}' \
        >"$dir/frame-$(printf '%03d' "$file_index").json"
}

write_sequence() {
    local dir="$1"
    write_packet "$dir" 0
    write_packet "$dir" 1
    write_packet "$dir" 2
}

printf '== observation-film: argument and path bounds\n'
expect_refusal missing-args "usage:"
expect_refusal missing-frames-dir "frame directory does not exist" \
    --manifest "$manifest" --frames "$tmp/absent" --out "$tmp/out"

invalid_manifest="$tmp/invalid manifest.json"
jq '.frame_count = 0' "$manifest" >"$invalid_manifest"
invalid_frames="$tmp/invalid manifest frames"
write_sequence "$invalid_frames"
expect_refusal invalid-manifest "manifest validation failed" \
    --manifest "$invalid_manifest" --frames "$invalid_frames" --out "$tmp/invalid-out"

empty_frames="$tmp/empty frames"
mkdir -p "$empty_frames"
expect_refusal missing-frames "no frame packets" \
    --manifest "$manifest" --frames "$empty_frames" --out "$tmp/out"

valid_frames="$tmp/valid frames"
write_sequence "$valid_frames"
expect_refusal inside-output "output directory must not be inside the input frame directory" \
    --manifest "$manifest" --frames "$valid_frames" --out "$valid_frames/rendered"
expect_refusal empty-output "--out must not be empty" \
    --manifest "$manifest" --frames "$valid_frames" --out ""
expect_refusal broad-output "refusing broad output target" \
    --manifest "$manifest" --frames "$valid_frames" --out "/"

escaped_manifest="$tmp/escaped manifest.json"
jq '.id = "../escaped"' "$manifest" >"$escaped_manifest"
escaped_parent="$tmp/escaped-parent"
mkdir -p "$escaped_parent"
if run_film escaped-id --manifest "$escaped_manifest" --frames "$valid_frames" --out "$escaped_parent/package"; then
    bad "path-traversing manifest id unexpectedly succeeded"
else
    if grep -Fq -- "manifest id must be a safe filename component" "$tmp/escaped-id.err" \
        && [ ! -e "$escaped_parent/escaped.sha256" ] && [ ! -e "$escaped_parent/escaped.mp4" ]; then
        ok "path-traversing manifest id cannot write outside --out"
    else
        bad "path-traversing manifest id was not clearly refused: $(cat "$tmp/escaped-id.err")"
    fi
fi

printf '== observation-film: packet sequence identity\n'
gap_frames="$tmp/gap frames"
write_packet "$gap_frames" 0
write_packet "$gap_frames" 2 2
expect_refusal non-contiguous "contiguous frame packets" \
    --manifest "$manifest" --frames "$gap_frames" --out "$tmp/gap-out"

index_frames="$tmp/index mismatch"
write_sequence "$index_frames"
write_packet "$index_frames" 1 2
expect_refusal index-mismatch "packet frame_index mismatch" \
    --manifest "$manifest" --frames "$index_frames" --out "$tmp/index-out"

episode_frames="$tmp/episode mismatch"
write_sequence "$episode_frames"
write_packet "$episode_frames" 1 1 HV-WRONG
expect_refusal episode-mismatch "packet episode_id mismatch" \
    --manifest "$manifest" --frames "$episode_frames" --out "$tmp/episode-out"

seed_frames="$tmp/seed mismatch"
write_sequence "$seed_frames"
write_packet "$seed_frames" 1 1 HV-TEST 99
expect_refusal seed-mismatch "packet world_seed mismatch" \
    --manifest "$manifest" --frames "$seed_frames" --out "$tmp/seed-out"

revision_frames="$tmp/revision mismatch"
write_sequence "$revision_frames"
write_packet "$revision_frames" 1 1 HV-TEST 42 revision-wrong
expect_refusal revision-mismatch "packet world_revision mismatch" \
    --manifest "$manifest" --frames "$revision_frames" --out "$tmp/revision-out"

digest_frames="$tmp/digest mismatch"
write_sequence "$digest_frames"
write_packet "$digest_frames" 1 1 HV-TEST 42 revision-test fnv1a64:wrong
expect_refusal digest-mismatch "packet source_digest mismatch" \
    --manifest "$manifest" --frames "$digest_frames" --out "$tmp/digest-out"

printf '== observation-film: verified no-video output\n'
out="$tmp/render output"
if run_film no-video --manifest "$manifest" --frames "$valid_frames" --out "$out"; then
    sidecar="$out/HV-TEST.sha256"
    if grep -Fq "ffmpeg unavailable; verified packets and wrote checksum sidecar only" "$tmp/no-video.out"; then
        ok "ffmpeg absence is reported clearly"
    else
        bad "ffmpeg absence was not reported: $(cat "$tmp/no-video.out")"
    fi
    if [ -f "$sidecar" ] && [ "$(wc -l <"$sidecar" | tr -d ' ')" -eq 4 ]; then
        ok "successful no-video mode writes manifest and packet checksums"
    else
        bad "checksum sidecar missing or incomplete"
    fi
    if [ ! -e "$out/HV-TEST.mp4" ]; then
        ok "no-video mode does not claim a derived film"
    else
        bad "no-video mode wrote an unexpected film"
    fi
    if (cd "$out" && shasum -a 256 -c HV-TEST.sha256) >"$tmp/no-video-check.out" 2>"$tmp/no-video-check.err"; then
        ok "package-directory checksum verification succeeds"
    else
        bad "package-directory checksum verification failed: $(cat "$tmp/no-video-check.err")"
    fi
else
    bad "valid no-video assembly failed: $(cat "$tmp/no-video.err")"
fi

if run_film rerun-no-video-sidecar --manifest "$manifest" --frames "$valid_frames" --out "$out"; then
    if (cd "$out" && shasum -a 256 -c HV-TEST.sha256) >"$tmp/rerun-no-video-check.out" 2>"$tmp/rerun-no-video-check.err"; then
        ok "package-directory checksum verification survives a no-video rerun"
    else
        bad "rerun package-directory checksum verification failed: $(cat "$tmp/rerun-no-video-check.err")"
    fi
else
    bad "no-video rerun failed: $(cat "$tmp/rerun-no-video-sidecar.err")"
fi

printf '== observation-film: derived film checksums\n'
for index in 0 1 2; do
    printf 'png-%s\n' "$index" >"$valid_frames/frame-$(printf '%03d' "$index").png"
done
fake_ffmpeg="$tmp/fake-ffmpeg"
cat >"$fake_ffmpeg" <<'EOF'
#!/usr/bin/env bash
set -eu
for final; do :; done
printf 'derived-film\n' >"$final"
EOF
chmod +x "$fake_ffmpeg"
video_out="$tmp/video output"
if HV_OBSERVATION_FFMPEG="$fake_ffmpeg" bash "$film" \
    --manifest "$manifest" --frames "$valid_frames" --out "$video_out" \
    >"$tmp/video.out" 2>"$tmp/video.err"; then
    if [ -s "$video_out/HV-TEST.mp4" ]; then
        ok "available assembler writes a derived film"
    else
        bad "available assembler did not write a film"
    fi
    if [ "$(wc -l <"$video_out/HV-TEST.sha256" | tr -d ' ')" -eq 8 ] \
        && grep -Fq "  HV-TEST.mp4" "$video_out/HV-TEST.sha256"; then
        ok "sidecar covers manifest, packets, PNG frames, and video"
    else
        bad "video sidecar is incomplete"
    fi
    if (cd "$video_out" && shasum -a 256 -c HV-TEST.sha256) >"$tmp/video-check.out" 2>"$tmp/video-check.err"; then
        ok "package-directory checksum verification covers the derived film"
    else
        bad "video package-directory checksum verification failed: $(cat "$tmp/video-check.err")"
    fi
else
    bad "available assembler path failed: $(cat "$tmp/video.err")"
fi

if run_film rerun-no-ffmpeg --manifest "$manifest" --frames "$valid_frames" --out "$video_out"; then
    if [ ! -e "$video_out/HV-TEST.mp4" ]; then
        ok "ffmpeg-unavailable rerun removes the owned stale video"
    else
        bad "ffmpeg-unavailable rerun left an unchecked stale video"
    fi
else
    if grep -Fq -- "existing derived video" "$tmp/rerun-no-ffmpeg.err"; then
        ok "ffmpeg-unavailable rerun refuses an existing derived video"
    else
        bad "rerun neither reconciled nor clearly refused the stale video: $(cat "$tmp/rerun-no-ffmpeg.err")"
    fi
fi

printf '\ntest-observation-film.sh: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
