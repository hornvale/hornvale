// The exhibit's own wasm smoke: a CHEAPER SECOND WITNESS than the catalog's
// golden (`clients/world-wasm/drive.mjs`), and deliberately a different
// question.
//
// `world-check` asks whether the wasm's `lot/life/v1` bytes are IDENTICAL to
// the native CLI's, which is the determinism guarantee and needs a world
// file, six `cargo run`s and several minutes. This asks the exhibit's own
// question instead — "does the wasm the page will load actually answer the
// four calls the page makes, with payloads the page's parsers accept" —
// which needs the wasm and nothing else. A page that ships against a wasm
// missing an export is dark, and nothing in `lot-check`'s deno half can see
// that: the bundle typechecks against an interface, not against a binary.
//
// Usage: node drive.mjs <wasm>
import { readFileSync, writeFileSync } from "node:fs";

const [wasmPath] = process.argv.slice(2);
if (!wasmPath) {
  console.error("usage: node drive.mjs <wasm>");
  process.exit(2);
}

const { instance } = await WebAssembly.instantiate(readFileSync(wasmPath), {});
const e = instance.exports;
const out = () =>
  new TextDecoder().decode(new Uint8Array(e.memory.buffer, e.hw_out_ptr(), e.hw_out_len()));
const fail = (what, detail) => {
  console.error(`lot smoke FAILED — ${what}${detail ? `: ${detail}` : ""}`);
  process.exit(1);
};
const expect = (code, want, what) => {
  if (code !== want) fail(what, `status ${code}: ${out()}`);
};

for (const name of ["hw_new", "hw_lot", "hw_lot_pinned", "hw_lot_curve", "hw_lot_places"]) {
  if (typeof e[name] !== "function") fail(`${name} is not exported by this wasm`);
}

expect(e.hw_new(42n), 0, "hw_new(42)");

// lot/life/v1 — what the Life and Story stages read.
expect(e.hw_lot(0n), 0, "hw_lot(0)");
const lifeText = out();
const life = JSON.parse(lifeText);
if (life.schema !== "lot/life/v1") fail("hw_lot(0)", `schema is ${life.schema}`);
if (!Array.isArray(life.slots) || life.slots.length === 0) fail("hw_lot(0)", "no slots");
if (typeof life.occupation !== "string") {
  fail("hw_lot(0)", "occupation is not decimal text — a full-width id would round in a browser");
}

// lot/curve/v1 — what the hero line and the When graph read.
expect(e.hw_lot_curve(), 0, "hw_lot_curve");
const curveText = out();
const curve = JSON.parse(curveText);
if (curve.schema !== "lot/curve/v1") fail("hw_lot_curve", `schema is ${curve.schema}`);
if (!(curve.souls_ever > 0)) fail("hw_lot_curve", `souls_ever is ${curve.souls_ever}`);
if (!Array.isArray(curve.births_by_epoch) || curve.births_by_epoch.length === 0) {
  fail("hw_lot_curve", "births_by_epoch is empty");
}

// lot/places/v1 — what the Where map reads, at a year inside the span.
const midYear = (curve.start_year + curve.present_year) / 2;
expect(e.hw_lot_places(midYear), 0, `hw_lot_places(${midYear})`);
const placesText = out();
const places = JSON.parse(placesText);
if (places.schema !== "lot/places/v1") fail("hw_lot_places", `schema is ${places.schema}`);
if (!Array.isArray(places.places) || places.places.length === 0) {
  fail("hw_lot_places", "no community was alive at the span's midpoint");
}

// The pinned form, both with and without a site — the two shapes the "select
// a year" and "select a location" modes send. 4294967295 is the ABI's
// "no site pin"; vertex 0 is a legal site, so a zero would pin a real place.
expect(e.hw_lot_pinned(1n, midYear, 4294967295), 0, "hw_lot_pinned (year only)");
const pinned = JSON.parse(out());
if (pinned.pick.year === null) fail("hw_lot_pinned", "the payload did not echo the year pin");
expect(
  e.hw_lot_pinned(1n, midYear, places.places[0].site),
  0,
  "hw_lot_pinned (year and site)",
);
const bothPinned = JSON.parse(out());
if (bothPinned.site !== places.places[0].site) {
  fail("hw_lot_pinned", `asked for site ${places.places[0].site}, got ${bothPinned.site}`);
}

// Leave the three payloads where `deno task test` can read them, so the
// hand-reduced fixtures are checked against the real shapes on any run of
// the gate that has just built the wasm.
writeFileSync("/tmp/hv-lot-life-0.json", lifeText);
writeFileSync("/tmp/hv-lot-curve.json", curveText);
writeFileSync("/tmp/hv-lot-places.json", placesText);

console.log(
  `lot smoke OK — lot/life/v1 (${life.slots.length} slots, ${life.silences.filled} filled), ` +
    `lot/curve/v1 (${curve.souls_ever} souls ever), ` +
    `lot/places/v1 (${places.places.length} places at year ${Math.round(midYear)})`,
);
