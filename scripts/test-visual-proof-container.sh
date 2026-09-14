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
    *"proof::rendered_proof_reads_distinct_frames_and_patch_readiness"*) ok "runs only the rendered proof";;
    *) bad "missing exact proof selector: $command_output";;
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
