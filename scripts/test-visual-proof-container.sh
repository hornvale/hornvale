#!/usr/bin/env bash
set -uo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$root/scripts/visual-proof-container.sh"

pass=0
fail=0
ok() { echo "  ok: $1"; pass=$((pass + 1)); }
bad() { echo "  FAIL: $1"; fail=$((fail + 1)); }

echo "== visual proof container: command construction"
command_output="$(visual_proof_container_command "$root" proof::rendered_proof_reads_distinct_frames_and_patch_readiness)"
base_image="$(sed -n 's/^FROM \(rust@sha256:[^ ]*\)$/\1/p' "$root/clients/visual/Containerfile.llvmpipe")"
case "$base_image" in
    rust@sha256:1f0dbad1df66647807e6952d1db85d0b2bda7606cb2139d82517e4f009967376) ok "uses the verified immutable Rust/trixie base image";;
    *) bad "missing pinned base image: $command_output";;
esac
case "$command_output" in
    *"hornvale/visual-proof:mesa-25.0.7"*) ok "uses the Mesa proof image";;
    *) bad "missing proof image: $command_output";;
esac
case "$command_output" in
    *"WGPU_BACKEND=vulkan"*) ok "selects Vulkan explicitly";;
    *) bad "missing Vulkan backend: $command_output";;
esac
case "$command_output" in
    *"VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/lvp_icd.json"*) ok "uses the packaged lavapipe ICD path";;
    *) bad "missing packaged lavapipe ICD path: $command_output";;
esac
case "$command_output" in
    *"proof::rendered_proof_reads_distinct_frames_and_patch_readiness"*) ok "runs only the rendered proof";;
    *) bad "missing exact proof selector: $command_output";;
esac
revision="$(git -C "$root" rev-parse HEAD)"
clean="$(git -C "$root" status --porcelain | grep -q . && echo false || echo true)"
case "$command_output" in
    *"PLANETARIUM_BUILD_PROVENANCE_REVISION=$revision"*) ok "passes the host checkout revision";;
    *) bad "missing host checkout revision: $command_output";;
esac
case "$command_output" in
    *"PLANETARIUM_BUILD_PROVENANCE_CLEAN=$clean"*) ok "passes the host checkout clean state";;
    *) bad "missing host checkout clean state: $command_output";;
esac

echo "== visual proof container: provenance contract"
if visual_proof_validate_provenance "$(printf 'a%.0s' {1..40})" true; then
    ok "accepts a complete valid provenance pair"
else
    bad "rejected a complete valid provenance pair"
fi
if ! visual_proof_validate_provenance "not-a-revision" true; then
    ok "rejects an invalid revision"
else
    bad "accepted an invalid revision"
fi
if ! visual_proof_validate_provenance "$(printf 'a%.0s' {1..40})" maybe; then
    ok "rejects an invalid clean state"
else
    bad "accepted an invalid clean state"
fi

echo "== visual proof container: Make dispatch"
make_target="$(sed -n '/^visual-check-run:/,/^game-check:/p' "$root/Makefile")"
default_value="\${HV_VISUAL_PROOF_CONTAINER:-1}"
native_value="\${HV_VISUAL_PROOF_CONTAINER:-1}\" = 0"
case "$make_target" in
    *"$default_value"*) ok "container is the default";;
    *) bad "Makefile does not default to the container";;
esac
case "$make_target" in
    *"$native_value"*) ok "native mode is an explicit opt-out";;
    *) bad "Makefile lacks the explicit native opt-out";;
esac
case "$make_target" in
    *'command -v docker >/dev/null 2>&1'*'falling back to native visual proof'*) ok "Docker-missing fallback is retained";;
    *) bad "Makefile lacks Docker-missing native fallback";;
esac

for package_pin in \
    'mesa-vulkan-drivers=25.0.7-2+deb13u1' \
    'libvulkan1=1.4.309.0-1' \
    'vulkan-tools=1.4.304.0+dfsg1-1'; do
    if grep -Fq "$package_pin" "$root/clients/visual/Containerfile.llvmpipe"; then
        ok "Containerfile pins $package_pin"
    else
        bad "Containerfile is missing $package_pin"
    fi
done
if grep -Fq 'VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/lvp_icd.json' "$root/clients/visual/Containerfile.llvmpipe"; then
    ok "Containerfile exports the packaged lavapipe ICD path"
else
    bad "Containerfile is missing the packaged lavapipe ICD path"
fi

echo "== visual proof container: pin validation"
if visual_proof_validate_pins "25.0.7-2+deb13u1" "1.4.309.0-1" "1.4.304.0+dfsg1-1"; then
    ok "accepts the verified Mesa and Vulkan pins"
else
    bad "rejected verified package pins"
fi
if ! visual_proof_validate_pins "25.0.6-1" "1.4.309.0-1" "1.4.304.0+dfsg1-1"; then
    ok "rejects a stale Mesa pin"
else
    bad "accepted an unverified Mesa pin"
fi

printf '\ntest-visual-proof-container.sh: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
