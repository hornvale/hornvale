// The catalog's golden smoke: wasm scene JSON must be byte-identical to
// the native CLI's (the two-language golden contract at the wasm seam).
// Usage: node drive.mjs <wasm> <native-system.json> <native-tiles.json> \
//                       <tiles-width> <native-pinned-tiles.json> <native-region.json>
import { readFileSync } from "node:fs";

const [wasmPath, sysPath, tilesPath, widthStr, pinnedTilesPath, regionPath] = process.argv.slice(2);
if (!pinnedTilesPath || !regionPath) {
  console.error(
    "usage: node drive.mjs <wasm> <sys.json> <tiles.json> <width> <pinned-tiles.json> <region.json>",
  );
  process.exit(2);
}
const width = Number(widthStr);
const { instance } = await WebAssembly.instantiate(readFileSync(wasmPath), {});
const e = instance.exports;
const out = () =>
  new TextDecoder().decode(new Uint8Array(e.memory.buffer, e.hw_out_ptr(), e.hw_out_len()));
const fail = (what, detail) => {
  console.error(`world-wasm smoke FAILED — ${what}${detail ? `: ${detail}` : ""}`);
  process.exit(1);
};
const expect = (code, want, what) => {
  if (code !== want) fail(what, `status ${code}: ${out()}`);
};
const golden = (got, path, what) => {
  const want = readFileSync(path, "utf8").trim();
  if (got.trim() !== want) fail(what, `wasm and native JSON differ (native: ${path})`);
};

// Default genesis, both scenes, byte-identical to native.
expect(e.hw_new(42n), 0, "hw_new(42)");
expect(e.hw_scene_system(), 0, "hw_scene_system");
golden(out(), sysPath, "scene/system/v1 (seed 42)");
expect(e.hw_scene_tiles(width), 0, "hw_scene_tiles");
golden(out(), tilesPath, "scene/tiles/v1 (seed 42)");
expect(e.hw_scene_tiles_region(0, 3, 4, 4, 16), 0, "hw_scene_tiles_region");
golden(out(), regionPath, "scene/tiles-region/v1 (seed 42, face 0 L3 4,4 s16)");

// Pinned genesis (terrain pin: deterministic force, satisfiable on any seed).
const pins = new TextEncoder().encode(JSON.stringify({ plates: "12" }));
new Uint8Array(e.memory.buffer, e.hw_in_ptr(), pins.length).set(pins);
expect(e.hw_new_pinned(42n, pins.length), 0, "hw_new_pinned(42, plates=12)");
expect(e.hw_scene_tiles(width), 0, "hw_scene_tiles (pinned)");
golden(out(), pinnedTilesPath, "scene/tiles/v1 (seed 42, plates=12)");

// Error paths: unknown pin → -3 with envelope; scene without world intact.
const bad = new TextEncoder().encode(JSON.stringify({ nonsense: "1" }));
new Uint8Array(e.memory.buffer, e.hw_in_ptr(), bad.length).set(bad);
if (e.hw_new_pinned(42n, bad.length) !== -3) fail("unknown pin", "expected -3");
if (!JSON.parse(out()).error) fail("unknown pin", "no error envelope");
// A refused/errored pinned call cleared the world: scenes must refuse too.
if (e.hw_scene_system() !== -3) fail("scene after cleared world", "expected -3");

console.log("world-wasm smoke OK (system + tiles + tiles-region + pinned byte-identical; error envelopes sound)");
