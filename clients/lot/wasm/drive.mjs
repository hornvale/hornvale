// The Lot's golden smoke: wasm lot JSON must be byte-identical to the
// native CLI's (the two-language golden contract at the wasm seam), moved
// out of the world catalog's own drive.mjs (Task 10b, ledger #17) along with
// the four exports it drives.
// Usage: node drive.mjs <wasm> <native-lot0.json> <native-lot3y1500.json>
import { readFileSync } from "node:fs";

const [wasmPath, lotPath, lotPinnedPath] = process.argv.slice(2);
if (!wasmPath || !lotPath || !lotPinnedPath) {
  console.error("usage: node drive.mjs <wasm> <lot0.json> <lot3y1500.json>");
  process.exit(2);
}
const { instance } = await WebAssembly.instantiate(readFileSync(wasmPath), {});
const e = instance.exports;
const out = () =>
  new TextDecoder().decode(new Uint8Array(e.memory.buffer, e.hl_out_ptr(), e.hl_out_len()));
const fail = (what, detail) => {
  console.error(`lot-wasm smoke FAILED — ${what}${detail ? `: ${detail}` : ""}`);
  process.exit(1);
};
const expect = (code, want, what) => {
  if (code !== want) fail(what, `status ${code}: ${out()}`);
};
const golden = (got, path, what) => {
  const want = readFileSync(path, "utf8").trim();
  if (got.trim() !== want) fail(what, `wasm and native JSON differ (native: ${path})`);
};

// Default genesis, byte-identical to `hornvale lot --index 0 --json`.
expect(e.hl_new(42n), 0, "hl_new(42)");
expect(e.hl_lot(0n), 0, "hl_lot(0)");
golden(out(), lotPath, "lot/life/v1 (seed 42, index 0)");
const seed42Lot0 = out();

// lot/curve/v1 — schema only (no native golden shipped for this shape).
expect(e.hl_lot_curve(), 0, "hl_lot_curve");
const curve = JSON.parse(out());
if (curve.schema !== "lot/curve/v1") fail("hl_lot_curve", `schema is ${curve.schema}`);

// lot/places/v1 — schema only, at a year inside the world's span.
expect(e.hl_lot_places(1500), 0, "hl_lot_places(1500)");
const places = JSON.parse(out());
if (places.schema !== "lot/places/v1") fail("hl_lot_places", `schema is ${places.schema}`);

// Pinned draw, byte-identical to `hornvale lot --index 3 --year 1500 --json`.
// 4294967295 (u32::MAX) is the ABI's "no site pin" sentinel.
expect(e.hl_lot_pinned(3n, 1500, 4294967295), 0, "hl_lot_pinned(3, 1500, no site)");
golden(out(), lotPinnedPath, "lot/life/v1 (seed 42, index 3, year 1500)");

// A year outside the world's span is refused, not silently clamped.
expect(e.hl_lot_pinned(3n, 9000, 4294967295), 2, "hl_lot_pinned refuses a year outside the span");

// Staleness: a fresh hl_new must drop the prior world's lot context. Since
// this crate has no pinned genesis (unlike the catalog), the witness is a
// SECOND SEED rather than a terrain pin — the two worlds' lot 0 must differ.
expect(e.hl_new(7n), 0, "hl_new(7)");
expect(e.hl_lot(0n), 0, "hl_lot(0) (seed 7)");
if (out() === seed42Lot0) {
  fail("hl_lot(0) (seed 7)", "matches seed 42's lot-0 bytes — LOT_CTX looks stale");
}

console.log(
  "lot-wasm smoke OK (lot/life/v1 + lot/curve/v1 + lot/places/v1 byte-identical/well-formed; pinned golden; out-of-span refusal; context reset across hl_new)",
);
