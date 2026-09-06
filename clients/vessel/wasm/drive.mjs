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

// 2. Walking works and returns different prose. This used to parse a direction
// out of the opening's own "Ways on:" line rather than hardcode one, because a
// worldgen epoch could reshape the seed-42 opening room's exits and the smoke
// asserts "walking works", not any particular geography.
//
// That reason is gone. Since decision 0141 a compass heading is an OVERLAY on
// the three-edge adjacency rather than a lookup among three labelled exits, so
// all eight points resolve from every walk-band cell regardless of which way
// the triangle underfoot happens to point. A fixed direction is therefore the
// stronger check now, not the weaker one: if any of the eight ever stops
// resolving, this fails, whereas a parse would quietly follow the prose
// wherever it went. The exits sentence is pinned separately, just below.
//
// The Ken (spec §4.3): openness is the default and a wall is news, so the
// once-unconditional "No direction here is closed; the nearest ground lies
// ..." line is gone from ordinary ground — and seed 42's flagship start is
// ordinary ground (nothing refused, every bearing carrying ground), so the
// opening prints no exits clause at all now. The positive pin the comment
// above promises ("just below") is therefore this absence, not a match.
assert.doesNotMatch(
  golden,
  /No direction here is closed/,
  "the vacuous exits clause must not survive on ordinary ground",
);
assert.equal(send("go n"), 0);
const stepped = readOut();
assert.notEqual(stepped, golden, "moving changed the room");
assert.match(stepped, /^\[room /, "room header present");

// 3. Retrace.
//
// An in-character move now CHARGES TIME (The Deed, decision 0168). The room
// header USED to carry that day, so a full-text equality against the day-0
// opening needed the day elided out first. The Ken's Task 3 removed the day
// (and the room id) from the header entirely — a controller ruling, since
// no shared derivation with the sky line existed to honour the spec's
// literal ask — so the header is now identical on every visit to the same
// room regardless of the clock, and `back`'s promise ("you return to the
// same room, rendered the same way") is a plain byte-for-byte equality
// against the opening, no elision required.
//
// Verified before weakening it, because "the assertion moved" and "the
// feature regressed" look identical from here: driving seed 42 through
// `go n` + `back` natively returns to the room it opened in, and the two
// blocks are BYTE-IDENTICAL.
//
// The day itself still has to be checked SOMEWHERE, or `back`'s own charge
// could regress silently while this passes — the exact anti-vacuity concern
// this file's history already records once. `!whoami` is where the day
// lives now (it always did; the header was a second, redundant clock). It
// is the out-of-character instrument (spec §3.2 group A), so reading it
// costs no time of its own, and comparing the day it reports right after
// the step against the day right after `back` is what actually pins
// `back`'s own charge — comparing against the OPENING's day would be
// vacuous, since `go n` alone already moves the clock off zero.
const dayOf = (s) => {
  const m = s.match(/, day ([0-9.]+), room \d+\.$/);
  assert.ok(m, `!whoami reports a day: ${s}`);
  return Number(m[1]);
};
assert.equal(send("!whoami"), 0);
const dayAfterStep = dayOf(readOut());
assert.equal(send("back"), 0);
const retraced = readOut();
assert.equal(
  retraced,
  golden,
  "back retraces to the opening room, byte for byte (the header carries no day to elide since The Ken)",
);
assert.equal(send("!whoami"), 0);
const dayAfterBack = dayOf(readOut());
assert.ok(
  dayAfterBack > dayAfterStep,
  `back charges time of its own: day ${dayAfterStep} -> ${dayAfterBack}`,
);

// 4. Unknown verbs answer politely, in-session.
assert.equal(send("dance"), 0);
assert.match(readOut(), /No verb 'dance'/);

// 5. Release ends the possession.
assert.equal(send("release"), 1, "release returns Turn::Released");
assert.equal(readOut(), "You let go.");

// 6. Re-possession with a DIFFERENT possessable seed (exercises teardown).
// Scouted, not hardcoded: many seeds generate no settlement at all, so
// `hv_start` returns 2 (possession refused) for them — that is a valid
// world, not a bug, and hardcoding one made this check fail whenever
// worldgen moved. 43 and 45 are both settlement-free today.
let other = null;
for (let seed = 43n; seed < 60n; seed++) {
  if (hv_start(seed) === 0) {
    other = seed;
    break;
  }
}
assert.notEqual(other, null, "some seed in 43..60 is possessable");
assert.notEqual(readOut(), golden, "a different seed is a different world");

// 7. And back to 42: same world again (determinism across restarts).
assert.equal(hv_start(42n), 0);
assert.equal(readOut(), golden, "seed 42 re-derives byte-identically");

// 8. The snapshot rides alongside the prose, and its narration IS the prose.
const snapshotJson = () =>
  new TextDecoder().decode(
    new Uint8Array(memory.buffer, hv_snapshot_ptr(), hv_snapshot_len()),
  );
assert.ok(hv_snapshot_len() > 0, "a live possession carries a snapshot");
const snap = JSON.parse(snapshotJson());
assert.equal(snap.schema, "vessel/session/v2");
for (const key of ["self", "sensed", "known", "social", "narration"]) {
  assert.ok(key in snap, `snapshot carries the ${key} channel`);
}
assert.equal(
  snap.narration.prose.trimEnd(),
  golden.trimEnd(),
  "narration.prose === the transcript opening the prose ABI already returns",
);

const kib = (bytes.length / 1024).toFixed(0);
console.log(`casement smoke OK — ${kib} KiB wasm, seed-42 genesis ${genesisMs.toFixed(0)} ms`);
