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

// Staleness: the catalog caches ONE SceneContext per world (The Cistern), so
// every hw_new* must drop it with the world. The live context here is the
// PINNED (plates=12) planet's, built two lines up; a plain hw_new must not let
// it survive. Byte-identity against the UNPINNED native golden is what proves
// it did not — a surviving context would serve plates=12 terrain. The mirror
// direction (a default context surviving into a pinned world) is what the
// pinned block above already tests, so between them both genesis paths are
// covered.
expect(e.hw_new(42n), 0, "hw_new(42) after a pinned world");
expect(e.hw_scene_tiles_region(0, 3, 4, 4, 16), 0, "hw_scene_tiles_region (context reset)");
golden(out(), regionPath, "scene/tiles-region/v1 served a stale SceneContext (pinned terrain)");

// Projection (The Winnowing): hw_scene_tiles_selected emits only the named
// per-tile layers. The check that matters is (c) below — a layer that IS
// emitted must be byte-identical to the full document's bytes for it. That is
// the wire-level form of the scene crate's independence property: projecting
// narrows the document and changes nothing inside a layer.
const writeIn = (text) => {
  const bytes = new TextEncoder().encode(text);
  new Uint8Array(e.memory.buffer, e.hw_in_ptr(), bytes.length).set(bytes);
  return bytes.length;
};
// The bytes of one per-tile layer, `"name":[…]` inclusive. Every selectable
// layer is a flat array of numbers, so the first `]` after the opening `[`
// closes it. Fails loudly rather than returning empty: a silent miss would
// make the equality below compare nothing to nothing.
const layerBytes = (doc, name, where) => {
  const key = `"${name}":[`;
  const at = doc.indexOf(key);
  if (at < 0) fail(`layer bytes`, `${where} has no "${name}" array`);
  const end = doc.indexOf("]", at + key.length);
  if (end < 0) fail(`layer bytes`, `${where}'s "${name}" array is unterminated`);
  return doc.slice(at, end + 1);
};

expect(e.hw_scene_tiles(width), 0, "hw_scene_tiles (projection baseline)");
const fullDoc = out();

// The full selection reproduces the full document exactly — the projecting
// serializer and the default one are separate code paths, so this is a real
// comparison and not a thing against itself.
const allNames = [
  "elevation_m", "ocean", "biome", "plate", "unrest", "t_mean_c", "t_swing_c",
  "t_diurnal_amp_c", "current_east", "current_north", "moisture", "precip_mm_yr",
  "snow_fraction", "precip_regime", "cloud_fraction", "weather_propensity",
  "cloud_type", "water", "drainage",
];
expect(
  e.hw_scene_tiles_selected(width, writeIn(JSON.stringify(allNames))),
  0,
  "hw_scene_tiles_selected (all nineteen)",
);
// This list is a hand-maintained copy of `TileFields::ALL_NAMES`, so a
// mismatch here is ambiguous by construction: it may mean the projecting
// serializer diverged from the derive, or it may simply mean THIS list drifted
// from `windows/scene/src/lib.rs`'s `ALL_NAMES` (a layer added there and not
// here). Check the list before the serializer — the duplication is
// self-detecting, which is why it is tolerated, but it detects itself here.
if (out() !== fullDoc) {
  fail(
    "full selection",
    "differs from the unprojected document — either the projecting serializer " +
      "diverged from scene_json, or drive.mjs's `allNames` has drifted from " +
      "TileFields::ALL_NAMES (windows/scene/src/lib.rs). Compare the lists first.",
  );
}

// A subset: three layers requested, sixteen withheld.
const wanted = ["elevation_m", "ocean", "biome"];
expect(
  e.hw_scene_tiles_selected(width, writeIn(JSON.stringify(wanted))),
  0,
  "hw_scene_tiles_selected (subset)",
);
const projected = out();
// (a) it parses.
let doc;
try {
  doc = JSON.parse(projected);
} catch (err) {
  fail("projected document", `does not parse: ${err.message}`);
}
// (b) EVERY unrequested layer is genuinely absent — no key, not a null or an
// empty array. All sixteen, not a representative one: an over-emitting
// projection that happened to spare the one sampled name would otherwise pass.
// Checked on the raw text too, so a key nested anywhere cannot hide from the
// parsed-object test.
for (const withheld of allNames.filter((n) => !wanted.includes(n))) {
  if (withheld in doc) fail("projected document", `"${withheld}" is present but was not requested`);
  if (projected.includes(`"${withheld}":`)) {
    fail("projected document", `raw text still carries a "${withheld}" key`);
  }
}
// Metadata is never selectable and must survive the projection.
for (const meta of ["schema", "seed", "width", "height", "biome_legend", "features"]) {
  if (!(meta in doc)) fail("projected document", `metadata "${meta}" was dropped`);
}
if (doc.schema !== "scene/tiles/v1") fail("projected document", `schema is ${doc.schema}`);
if (projected.length >= fullDoc.length) fail("projected document", "no smaller than the full one");
// (c) every requested layer's bytes match the full document's, exactly.
for (const name of wanted) {
  if (layerBytes(projected, name, "the projection") !== layerBytes(fullDoc, name, "the full document")) {
    fail("projected layer", `"${name}" bytes differ from the full document's`);
  }
}

// Field-list refusals, each distinguishable by return code: a typo'd layer
// name (-4) is a different client bug from a payload that is not an array
// of names (-5), and both differ from "no world live" (-3).
if (e.hw_scene_tiles_selected(width, writeIn(JSON.stringify(["elevation_m", "elevatoin_m"]))) !== -4) {
  fail("unknown tile field", "expected -4");
}
if (!JSON.parse(out()).error?.includes("elevatoin_m")) {
  fail("unknown tile field", "envelope does not name the offending field");
}
if (e.hw_scene_tiles_selected(width, writeIn(JSON.stringify({ elevation_m: true }))) !== -5) {
  fail("malformed tile fields", "expected -5");
}
if (!JSON.parse(out()).error) fail("malformed tile fields", "no error envelope");
// Longer than the 4096-byte input buffer: refused before a single byte is read.
if (e.hw_scene_tiles_selected(width, 5000) !== -1) fail("oversized field list", "expected -1");
// Not UTF-8: a lone continuation byte.
new Uint8Array(e.memory.buffer, e.hw_in_ptr(), 2).set([0xff, 0xfe]);
if (e.hw_scene_tiles_selected(width, 2) !== -2) fail("non-UTF-8 field list", "expected -2");
// A refused field list leaves the world alone.
expect(e.hw_scene_tiles(width), 0, "hw_scene_tiles after refused field lists");
if (out() !== fullDoc) fail("world after refused field lists", "document changed");

// Error paths: unknown pin → -3 with envelope; scene without world intact.
const bad = new TextEncoder().encode(JSON.stringify({ nonsense: "1" }));
new Uint8Array(e.memory.buffer, e.hw_in_ptr(), bad.length).set(bad);
if (e.hw_new_pinned(42n, bad.length) !== -3) fail("unknown pin", "expected -3");
if (!JSON.parse(out()).error) fail("unknown pin", "no error envelope");
// A refused/errored pinned call cleared the world: scenes must refuse too.
if (e.hw_scene_system() !== -3) fail("scene after cleared world", "expected -3");
// Including the projecting one — and -3 there means "no world", never "bad
// field list", even though the field list here is perfectly valid.
if (e.hw_scene_tiles_selected(width, writeIn(JSON.stringify(["elevation_m"]))) !== -3) {
  fail("projected scene after cleared world", "expected -3");
}

console.log("world-wasm smoke OK (system + tiles + tiles-region + pinned byte-identical; projection omits what was not asked for and preserves what was, byte for byte; error envelopes sound; scene context reset)");
