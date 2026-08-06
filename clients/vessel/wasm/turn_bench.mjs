// The Sighting's ABI turn-cost bench: what one turn costs through the
// WebAssembly boundary, measured directly rather than extrapolated from the
// native `windows/vessel/examples/turn_cost.rs` figures by an assumed
// 3.6-3.8x ratio (`CLIENT-four-clocks`). This is that re-measurement.
//
// This is the drive.mjs loop (same ABI, same instantiate/read/write
// pattern) with timing wrapped around it — no second way of driving the
// ABI is invented here.
//
// Usage: node turn_bench.mjs [path/to/vessel.wasm]
// Informative only: nothing here asserts or exits non-zero on a "bad" number.
import { readFile } from "node:fs/promises";

const wasmPath = process.argv[2] ??
  new URL(
    "./target/wasm32-unknown-unknown/release/hornvale_vessel_wasm.wasm",
    import.meta.url,
  );

// Copied verbatim from `windows/vessel/examples/turn_cost.rs`'s `SEQUENCE`
// constant, so the native and ABI readings run the same verbs.
const SEQUENCE = [
  "look",
  "map",
  "examine me",
  "wait 1",
  "look",
  "enter",
  "map",
  "look",
  "out",
  "look",
];

// Same verb classification `turn_cost.rs` uses.
function verbClass(line) {
  const head = line.split(/\s+/)[0] ?? "";
  if (head === "wait") return "day-advancing";
  if (["enter", "out", "go", "back", "dive", "surface"].includes(head)) {
    return "moving";
  }
  return "neither";
}

// Same median discipline as `turn_cost.rs`'s `median()`: sort in place, take
// the middle element (odd n=5 here, so no averaging ambiguity).
function median(xs) {
  const sorted = [...xs].sort((a, b) => a - b);
  return sorted[Math.floor(sorted.length / 2)];
}

const RUNS = 5;

const bytes = await readFile(wasmPath);
const { instance } = await WebAssembly.instantiate(bytes, {});
const {
  hv_start,
  hv_in_ptr,
  hv_handle,
  hv_out_ptr,
  hv_out_len,
  hv_snapshot_ptr,
  hv_snapshot_len,
  memory,
} = instance.exports;

const decoder = new TextDecoder();
const encoder = new TextEncoder();

const send = (line) => {
  const enc = encoder.encode(line);
  new Uint8Array(memory.buffer, hv_in_ptr(), enc.length).set(enc);
  return hv_handle(enc.length);
};
const readOut = () => decoder.decode(new Uint8Array(memory.buffer, hv_out_ptr(), hv_out_len()));

const readSnapshotJson = () =>
  decoder.decode(
    new Uint8Array(memory.buffer, hv_snapshot_ptr(), hv_snapshot_len()),
  );

const starts = [];
const turns = [];
const snaps = [];
const turnsByClass = { moving: [], "day-advancing": [], neither: [] };
const snapsByClass = { moving: [], "day-advancing": [], neither: [] };

for (let run = 0; run < RUNS; run++) {
  const t0 = performance.now();
  const rc = hv_start(42n);
  const startMs = performance.now() - t0;
  starts.push(startMs);
  if (rc !== 0) {
    throw new Error(`hv_start(42) returned ${rc}, expected 0`);
  }
  // Consume the opening output the way drive.mjs does (not timed — this is
  // genesis's already-produced prose, not part of a handle() call).
  readOut();

  for (const line of SEQUENCE) {
    const cls = verbClass(line);

    const t1 = performance.now();
    send(line);
    const turnMs = performance.now() - t1;
    turns.push(turnMs);
    turnsByClass[cls].push(turnMs);

    const t2 = performance.now();
    readSnapshotJson();
    const snapMs = performance.now() - t2;
    snaps.push(snapMs);
    snapsByClass[cls].push(snapMs);
  }
}

// The byte figure, mirroring `turn_cost.rs` exactly: a FRESH session,
// snapshot immediately after start (walk band), then `enter`, then snapshot
// again (chamber band) — not pulled from the SEQUENCE loop above, whose
// `enter`/`out` pair would otherwise need band-tracking of its own.
if (hv_start(42n) !== 0) {
  throw new Error("hv_start(42) returned non-zero on the byte-count run");
}
readOut();
const walkBytes = readSnapshotJson().length;
send("enter");
const chamberBytes = readSnapshotJson().length;

console.log(`Session::start   median ${median(starts).toFixed(3).padStart(8)} ms`);
console.log(`hv_handle(verb)  median ${median(turns).toFixed(3).padStart(8)} ms`);
console.log(`snapshot+decode  median ${median(snaps).toFixed(3).padStart(8)} ms`);

for (const cls of ["moving", "day-advancing", "neither"]) {
  const t = turnsByClass[cls];
  const s = snapsByClass[cls];
  console.log(
    `  ${cls.padEnd(13)} n=${String(t.length).padEnd(3)} handle median ${
      median(t).toFixed(3).padStart(8)
    } ms   snapshot+decode median ${median(s).toFixed(3).padStart(8)} ms`,
  );
}

console.log(`snapshot bytes   walk ${walkBytes}, chamber ${chamberBytes}`);
