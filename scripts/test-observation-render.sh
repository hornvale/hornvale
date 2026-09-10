#!/usr/bin/env bash
# test-observation-render.sh — bounded verification for local observation rasterization.
set -uo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)"
renderer="$root/scripts/observation-render.sh"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

pass=0
fail=0
ok() { printf '  ok: %s\n' "$1"; pass=$((pass + 1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail + 1)); }

manifest="$tmp/HV-TEST.json"
frames="$tmp/frames"
jq '.id = "HV-TEST" | .frame_count = 3 | .world_revision = "revision-test"' \
    "$root/observations/episodes/HV-001.json" >"$manifest"
timeout 3600000 cargo run --quiet -p hornvale -- observations export --manifest "$manifest" --out "$frames" \
    >"$tmp/export.out" 2>"$tmp/export.err" || {
    cat "$tmp/export.err" >&2
    exit 1
}

fake_firefox="$tmp/fake-firefox"
cat >"$fake_firefox" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

for argument in "$@"; do
    case "$argument" in
        file://*) html="${argument#file://}" ;;
    esac
done
for ((index = 1; index <= $#; index++)); do
    if [ "${!index}" = "--screenshot" ]; then
        screenshot_index=$((index + 1))
        output="${!screenshot_index}"
        break
    fi
done
[ -n "${html:-}" ]
[ -n "${output:-}" ]
grep -Fq 'data-width="1440" data-height="900"' "$html"

deno eval '
const output = Deno.args[0];
const width = 1440;
const height = 900;
const raw = new Uint8Array((width * 3 + 1) * height);
for (let row = 0; row < height; row++) raw[row * (width * 3 + 1)] = 0;
const compressed = new Uint8Array(await new Response(
  new Blob([raw]).stream().pipeThrough(new CompressionStream("deflate")),
).arrayBuffer());
function crc32(bytes) {
  let crc = 0xffffffff;
  for (const byte of bytes) {
    crc ^= byte;
    for (let bit = 0; bit < 8; bit++) crc = (crc >>> 1) ^ (0xedb88320 & -(crc & 1));
  }
  return (crc ^ 0xffffffff) >>> 0;
}
function chunk(kind, data) {
  const bytes = new Uint8Array(12 + data.length);
  new DataView(bytes.buffer).setUint32(0, data.length);
  bytes.set(new TextEncoder().encode(kind), 4);
  bytes.set(data, 8);
  new DataView(bytes.buffer).setUint32(8 + data.length, crc32(bytes.slice(4, 8 + data.length)));
  return bytes;
}
const header = new Uint8Array(13);
const view = new DataView(header.buffer);
view.setUint32(0, width);
view.setUint32(4, height);
header.set([8, 2, 0, 0, 0], 8);
const pieces = [chunk("IHDR", header), chunk("IDAT", compressed), chunk("IEND", new Uint8Array())];
const image = new Uint8Array(8 + pieces.reduce((size, piece) => size + piece.length, 0));
image.set([137, 80, 78, 71, 13, 10, 26, 10]);
let offset = 8;
for (const piece of pieces) {
  image.set(piece, offset);
  offset += piece.length;
}
await Deno.writeFile(output, image);
' "$output"
EOF
chmod +x "$fake_firefox"

run_renderer() {
    local name="$1"
    shift
    HV_OBSERVATION_FIREFOX="$fake_firefox" \
        timeout 3600000 bash "$renderer" "$@" >"$tmp/$name.out" 2>"$tmp/$name.err"
}

expect_refusal() {
    local name="$1"
    local message="$2"
    shift 2
    if run_renderer "$name" "$@"; then
        bad "$name unexpectedly succeeded"
    elif grep -Fq -- "$message" "$tmp/$name.err"; then
        ok "$name"
    else
        bad "$name did not report '$message': $(cat "$tmp/$name.err")"
    fi
}

png_dimensions() {
    local file="$1"
    local width height
    width="$(od -An -tx1 -j 16 -N 4 "$file" | tr -d '[:space:]')"
    height="$(od -An -tx1 -j 20 -N 4 "$file" | tr -d '[:space:]')"
    printf '%d×%d\n' "0x$width" "0x$height"
}

printf '== observation-render: valid ordered raster sequence\n'
if run_renderer valid --manifest "$manifest" --frames "$frames"; then
    if [ "$(find "$frames" -maxdepth 1 -type f -name 'frame-*.png' | wc -l | tr -d ' ')" -eq 3 ] \
        && [ -f "$frames/frame-000.png" ] && [ -f "$frames/frame-001.png" ] \
        && [ -f "$frames/frame-002.png" ]; then
        ok "one contiguous PNG is written beside every packet"
    else
        bad "renderer did not write exactly the contiguous PNG sequence"
    fi
    if [ "$(png_dimensions "$frames/frame-000.png")" = '1440×900' ]; then
        ok "PNG dimensions match the declared laptop viewport"
    else
        bad "PNG dimensions were $(png_dimensions "$frames/frame-000.png"), not 1440×900"
    fi
else
    bad "valid renderer invocation failed: $(cat "$tmp/valid.err")"
fi

first_checksums="$tmp/first.sha256"
shasum -a 256 "$frames"/frame-*.png >"$first_checksums"
if run_renderer repeat --manifest "$manifest" --frames "$frames" \
    && cmp -s "$first_checksums" <(shasum -a 256 "$frames"/frame-*.png); then
    ok "repeated rasterization is byte-identical with the local backend"
else
    bad "repeated rasterization changed the PNG sequence: $(cat "$tmp/repeat.err")"
fi

printf '== observation-render: refuse malformed packets before replacement\n'
before_bad="$tmp/before-bad.sha256"
shasum -a 256 "$frames"/frame-*.png >"$before_bad"
printf '%s' '{not JSON}' >"$frames/frame-001.json"
expect_refusal malformed-packet "packet is not valid JSON: frame-001.json" \
    --manifest "$manifest" --frames "$frames"
if cmp -s "$before_bad" <(shasum -a 256 "$frames"/frame-*.png); then
    ok "malformed packet leaves the prior PNG sequence intact"
else
    bad "malformed packet replaced an existing PNG"
fi

printf '== observation-render: backend boundary\n'
if HV_OBSERVATION_FIREFOX=missing-observation-firefox \
    timeout 3600000 bash "$renderer" --manifest "$manifest" --frames "$frames" \
    >"$tmp/unavailable.out" 2>"$tmp/unavailable.err"; then
    bad "missing backend unexpectedly succeeded"
elif grep -Fq 'Firefox headless screenshot backend is unavailable' "$tmp/unavailable.err"; then
    ok "missing backend is refused clearly"
else
    bad "missing backend refusal was unclear: $(cat "$tmp/unavailable.err")"
fi

printf '\ntest-observation-render.sh: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
