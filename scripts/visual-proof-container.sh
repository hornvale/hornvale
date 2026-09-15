#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
image="hornvale/visual-proof:mesa-25.0.7"
mesa_version="25.0.7-2+deb13u1"
libvulkan_version="1.4.309.0-1"
vulkan_tools_version="1.4.304.0+dfsg1-1"
test_name="proof::rendered_proof_reads_distinct_frames_and_patch_readiness"

visual_proof_validate_pins() {
    [ "${1:-}" = "$mesa_version" ] \
        && [ "${2:-}" = "$libvulkan_version" ] \
        && [ "${3:-}" = "$vulkan_tools_version" ]
}

visual_proof_validate_provenance() {
    [[ "${1:-}" =~ ^[[:xdigit:]]{40}$ ]] \
        && { [ "${2:-}" = true ] || [ "${2:-}" = false ]; }
}

visual_proof_build_provenance() {
    local checkout="${1:?checkout path required}"
    local revision status clean
    revision="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$checkout" rev-parse HEAD)"
    status="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$checkout" status --porcelain)"
    if [ -n "$status" ]; then
        clean=false
    else
        clean=true
    fi
    visual_proof_validate_provenance "$revision" "$clean" || {
        echo "visual-proof-container: checkout provenance is invalid" >&2
        return 1
    }
    printf '%s\n%s\n' "$revision" "$clean"
}

visual_proof_container_command() {
    local checkout="${1:?checkout path required}"
    local selector="${2:?test selector required}"
    local revision clean
    revision="$(visual_proof_build_provenance "$checkout" | sed -n '1p')"
    clean="$(visual_proof_build_provenance "$checkout" | sed -n '2p')"
    printf '%q ' docker run --rm --network=host \
        --tmpfs /tmp:exec \
        -e WGPU_BACKEND=vulkan \
        -e VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/lvp_icd.json \
        -e LIBGL_ALWAYS_SOFTWARE=1 \
        -e CARGO_TARGET_DIR=/tmp/hornvale-target \
        -e "PLANETARIUM_BUILD_PROVENANCE_REVISION=$revision" \
        -e "PLANETARIUM_BUILD_PROVENANCE_CLEAN=$clean" \
        -w /workspace \
        -v "$checkout:/workspace:ro" \
        "$image" \
        cargo +1.96.1 test --locked --manifest-path clients/visual/Cargo.toml \
        -p planetarium --test suite "$selector" -- --exact --nocapture --ignored
    printf '\n'
}

build_image() {
    docker build --pull=false \
        --file "$root/clients/visual/Containerfile.llvmpipe" \
        --tag "$image" "$root/clients/visual"
}

run_proof() {
    local revision clean
    revision="$(visual_proof_build_provenance "$root" | sed -n '1p')"
    clean="$(visual_proof_build_provenance "$root" | sed -n '2p')"
    local command=(
        docker run --rm --network=host
        --tmpfs /tmp:exec
        -e WGPU_BACKEND=vulkan
        -e VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/lvp_icd.json
        -e LIBGL_ALWAYS_SOFTWARE=1
        -e CARGO_TARGET_DIR=/tmp/hornvale-target
        -e "PLANETARIUM_BUILD_PROVENANCE_REVISION=$revision"
        -e "PLANETARIUM_BUILD_PROVENANCE_CLEAN=$clean"
        -w /workspace
        -v "$root:/workspace:ro"
        "$image"
        cargo +1.96.1 test --locked --manifest-path clients/visual/Cargo.toml
        -p planetarium --test suite "$test_name" -- --exact --nocapture --ignored
    )
    "${command[@]}"
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
    command -v docker >/dev/null 2>&1 || {
        echo "visual-proof-container: Docker is required; use direct Metal mode explicitly with HV_VISUAL_PROOF_CONTAINER=0" >&2
        exit 2
    }

    if ! docker image inspect "$image" >/dev/null 2>&1; then
        build_image
    fi
    run_proof
fi
