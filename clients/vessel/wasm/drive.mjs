// The Casement's smoke driver: possess the wasm build and assert its
// seed-42 opening is byte-identical to the committed native transcript.
// Usage: node drive.mjs [path/to/vessel.wasm]
// CI and `make vessel-check` run this; it exits non-zero on any mismatch.
import { readFile } from "node:fs/promises";
import assert from "node:assert/strict";

const wasmPath = process.argv[2] ??
  new URL("./target/wasm32-unknown-unknown/release/hornvale_vessel_wasm.wasm", import.meta.url);
const transcriptPath = new URL(
  "../../../book/src/gallery/possession-seed-42.md",
  import.meta.url,
);

// The transcript's opening: everything between the ```text fence and the
// first prompt line. Reading the committed artifact (rather than a golden
// string here) means `make rebaseline` keeps this check honest after any
// worldgen change.
function openingFromTranscript(md) {
  const fence = "```text\n";
  const start = md.indexOf(fence);
  assert.notEqual(start, -1, "transcript has a ```text fence");
  const body = md.slice(start + fence.length);
  const end = body.indexOf("\n> ");
  assert.notEqual(end, -1, "transcript has a prompt line");
  return body.slice(0, end);
}

const golden = openingFromTranscript(await readFile(transcriptPath, "utf8"));
const bytes = await readFile(wasmPath);

// Empty imports object: the module may import nothing (spec guarantee).
const { instance } = await WebAssembly.instantiate(bytes, {});
const { hv_start, hv_in_ptr, hv_handle, hv_out_ptr, hv_out_len, memory } = instance.exports;

const readOut = () =>
  new TextDecoder().decode(
    new Uint8Array(memory.buffer, hv_out_ptr(), hv_out_len()),
  );
const send = (line) => {
  const enc = new TextEncoder().encode(line);
  new Uint8Array(memory.buffer, hv_in_ptr(), enc.length).set(enc);
  return hv_handle(enc.length);
};

// 1. Genesis + byte-identity with the native transcript.
const t0 = performance.now();
assert.equal(hv_start(42n), 0, "seed-42 genesis succeeds");
const genesisMs = performance.now() - t0;
assert.equal(readOut(), golden, "wasm opening === native transcript opening");

// 2. Walking works and returns different prose. The direction comes from the
// opening's own "Ways on:" line, not a hardcoded compass point — a worldgen
// epoch may reshape the seed-42 opening room's exits, and this smoke asserts
// "walking works", not any particular geography.
const ways = golden.match(/^Ways on: (.+)\.$/m);
assert.notEqual(ways, null, "opening lists its ways on");
const dir = ways[1].split(", ")[0].toLowerCase();
assert.equal(send(`go ${dir}`), 0);
const stepped = readOut();
assert.notEqual(stepped, golden, "moving changed the room");
assert.match(stepped, /^\[room /, "room header present");

// 3. Retrace.
assert.equal(send("back"), 0);
assert.equal(readOut(), golden, "back retraces to the opening room");

// 4. Unknown verbs answer politely, in-session.
assert.equal(send("dance"), 0);
assert.match(readOut(), /No verb 'dance'/);

// 5. Release ends the possession.
assert.equal(send("release"), 1, "release returns Turn::Released");
assert.equal(readOut(), "You let go.");

// 6. Re-possession with a different seed (exercises the teardown path).
assert.equal(hv_start(43n), 0, "seed-43 genesis succeeds after teardown");
assert.notEqual(readOut(), golden, "a different seed is a different world");

// 7. And back to 42: same world again (determinism across restarts).
assert.equal(hv_start(42n), 0);
assert.equal(readOut(), golden, "seed 42 re-derives byte-identically");

const kib = (bytes.length / 1024).toFixed(0);
console.log(`casement smoke OK — ${kib} KiB wasm, seed-42 genesis ${genesisMs.toFixed(0)} ms`);
