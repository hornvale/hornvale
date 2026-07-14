# The Casement — a playable possession in the project book

**Date:** 2026-07-13
**Status:** SHIPPED 2026-07-14 (merged to main; chronicle: book/src/chronicle/the-casement.md)
**Campaign:** The Casement

## What this is

A gallery exhibit in the project book: the possess/vessel loop, compiled to
WebAssembly, embedded in a chapter as a live terminal. The reader types a
seed; the world derives **in their browser** — the same crates, the same
bytes of prose the native binary produces — and they walk it.

In the project's terms this is a *casement*: a pane in a document through
which the document's subject runs live. It is the sibling of the Atlas and
the Live Orrery, not the first brick of the game client. The Vessel arc
stays native-first; if a browser client comes later it inherits whatever
here proved durable, but nothing in this campaign is designed as that
client's foundation.

Feasibility is already proven by spike: the full `worldgen → vessel`
dependency chain compiles to `wasm32-unknown-unknown` with **zero source
changes** (the constitutional constraints — no wall-clock, no HashMap, no
threads, libm transcendentals — turn out to be exactly the WASM-portability
constraints). Measured at opt-level=z: 474 KB raw / 193 KB gzipped; seed-42
genesis ~4 s in-module; the seed-42 opening text is byte-identical to the
native binary's (decision 0041 extends to wasm unmodified).

## Decisions already made (owner-ratified in brainstorm)

1. **Identity:** gallery exhibit, one small campaign.
2. **World source:** live genesis from any u64 seed, seed 42 prefilled.
   No committed world.json.
3. **Deployment:** the .wasm is **built at deploy time** in `book.yml`,
   never committed. This deliberately diverges from the committed-bundle
   pattern (atlas.js / orrery.js) and gets its own decision-log entry.
4. **Crate home:** the Rust cdylib lives **outside the workspace**
   (type-audit precedent, decisions 0027/0028 lineage).
5. **Visual treatment:** book-native transcript (mockup B) — the prose in
   the book's serif voice, monospace confined to the `Ways on:` line and
   the input row — under an explicit demo heading so the seamlessness
   doesn't make it disappear.

## Components

```
clients/vessel/            Deno project, mirroring clients/atlas + clients/orrery
  deno.json                fmt / lint / check / test / build tasks
  src/                     terminal UI + worker protocol (TypeScript)
  -> deno task build emits book/src/gallery/vessel.js and
     book/src/gallery/vessel-worker.js   (committed, drift-checked in CI)

clients/vessel/wasm/       Rust cdylib crate, excluded from the workspace
  Cargo.toml               path-deps on kernel/worldgen/vessel (+ astronomy,
                           terrain for pin types); serde-allowlist unchanged;
                           own [profile.release]: LTO, panic=abort, strip
  src/lib.rs               the extern "C" surface (below)
  drive.mjs                Node smoke driver (promoted from the spike)
  -> builds to vessel.wasm (NOT committed; gitignored at
     book/src/gallery/vessel.wasm)

book/src/gallery/possession-live.md   the chapter ("A Possession, Live"),
                           listed in SUMMARY.md after the committed
                           transcript possession-seed-42.md

.github/workflows/book.yml gains a build step: pinned toolchain (1.96.1) +
                           wasm32 target, build vessel.wasm into
                           book/src/gallery/ BEFORE `mdbook build` (~2-3 min)

.github/workflows/ci.yml   gains a `vessel` job (below)

Makefile                   gains `wasm-vessel`: build + copy vessel.wasm into
                           book/src/gallery/ for local `mdbook serve`
```

## The wasm export surface

Raw `extern "C"` — no wasm-bindgen, no new crates anywhere. Five exports:

- `hv_start(seed: u64) -> i32` — build the world (default pins,
  `SkyChoice::Generated`, day 0.0), mint the flagship possession, place the
  opening text in the output buffer. Non-zero on failure, and the **error
  text (the `BuildError` display) goes to the output buffer** so the page
  can show why a seed refused, in the sim's own voice.
- `hv_in_ptr() -> *mut u8` — the input buffer JS writes UTF-8 commands to.
- `hv_handle(len: usize) -> i32` — one verb, one response; 0 continue,
  1 released, negative on protocol error.
- `hv_out_ptr() -> *const u8` / `hv_out_len() -> usize` — the current
  output text.

Constraints on the implementation:

- **Re-possession must not leak.** `hv_start` on a live session drops the
  old session first, then reclaims and drops the old world (it was leaked
  to give the session a `'static` borrow), then builds the new one.
- **The imports object is empty.** The module may import nothing — no
  clock, no network, no DOM. Five exports, memory in, prose out. This is
  the exhibit's one-line sandbox audit and the chapter says so.
- **Optimization level is a measured choice, not a default.** The plan
  measures opt-level=z vs opt-level=3 (size and genesis wall time) and
  picks; ceiling 1 MB raw. The spike's z numbers (474 KB / ~4 s) are the
  fallback if 3 buys little.

## The JS client

- **Genesis runs in a Web Worker.** A synchronous multi-second call on the
  main thread can't even repaint a status line. The worker owns the wasm
  instance; the page owns the DOM.
- **Protocol (worker messages):** `start{seed}` -> `started{text}` |
  `error{text}`; `command{line}` -> `out{text, released}`.
- **Nothing in the page script may assume a singleton.** One casement per
  page is what ships, but the terminal is constructed per container element
  with its own worker, so a future page can hold two (the diptych). The
  *wasm* keeps its internal single-session static — one world per worker
  instance is the isolation boundary.
- **Waypost:** `?seed=N` in the page URL prefills and auto-possesses, so a
  link is a world. Invalid `?seed` falls back to 42, silently.
- **No staged genesis progress.** The status line is indeterminate ("the
  genesis of seed N…"); threading a JS progress callback into
  `build_world`'s stage hooks is worldgen surgery an exhibit doesn't
  justify.

## The chapter

`book/src/gallery/possession-live.md`:

- Prose intro in the book's voice: every word below derives in the
  reader's browser from nothing but the seed; identical bytes to the
  native binary (and why that's true — decision 0041).
- An explicit `## The Demo` heading; the live region framed with the gold
  left border so the book-native styling doesn't vanish into the page.
- Controls row: seed input (u64, prefilled 42) + a *possess* button; both
  disabled during genesis. After `release`/`quit` the transcript ends with
  "You let go." and the controls re-enable.
- Input line: Enter submits; up/down recalls history; `help` works as in
  the CLI. Output appends as a growing transcript, auto-scrolled inside a
  max-height region.
- **Docent line** under the demo: one sentence on what this is and what it
  costs ("genesis takes a few seconds; everything derives from the seed
  above"), plus a **view-source link** to `clients/vessel` — the
  reproducible-notebook courtesy.
- Theme-aware via mdBook CSS variables (rust light / coal dark).
- If `vessel.wasm` is absent (local build without `make wasm-vessel`), the
  demo region shows a visible "not built — run `make wasm-vessel`" line
  instead of a broken page.

## Error handling

- Non-numeric / out-of-range seed: caught client-side, message in the
  transcript, no worker round-trip.
- Genesis failure: the sim's own error text, inline in the transcript.
- Worker/wasm load failure: the docent line degrades to the failure reason.
- Unknown verbs are already the session's job ("No verb '…'").

## CI and testing

New `ci.yml` job `vessel`, mirroring the atlas/orrery jobs plus a Rust leg:

1. **Deno:** `fmt --check`, `lint`, `check`, `test`, then `deno task
   build` + `git diff --exit-code` on the two committed bundles.
2. **Rust:** `cargo fmt --check` and `cargo clippy -- -D warnings` on the
   wasm crate via `--manifest-path` (it is outside the workspace gate),
   then a release build for `wasm32-unknown-unknown`.
3. **Byte-identity smoke:** the Node driver possesses seed 42 in the
   freshly built wasm and asserts the opening text equals the native
   golden (the same opening the committed transcript records). This is the
   determinism guarantee extended to wasm, on every commit — identity is
   guaranteed by this check, not by freezing the .wasm bytes (which are
   re-derived at every deploy).

Deno tests cover the worker protocol and transcript rendering; the Node
smoke covers the wasm end-to-end. The Rust wrapper is thin enough that the
smoke is its test.

`book.yml` ordering: rust step builds `vessel.wasm` into `book/src/gallery/`
before `mdbook build`, so mdBook copies it like any other gallery asset.

## Explicitly out of scope (indexed, not lost)

Idea-registry rows (status `raw`, next free number in their category), so
the anti-relitigation index knows them:

- **The vigil** — a casement whose day advances in wall-clock time while
  the page sits open (client-side only; determinism waived per 0022/0023,
  Live Orrery precedent).
- **The diptych** — two casements side by side, different seeds, same
  verbs; the comparative form.
- **Chapter-pinned casements** — pedagogy: the climate chapter opens a
  casement in a monsoon belt; each chapter's phenomenon, walkable.
  (Depends on possess-at-position, which the vessel does not expose yet.)
- **Attract mode** — the walker battery as autoplaying screensaver.

Also out of scope: staged genesis progress, day selection, saving
knowledge across page loads, mobile-keyboard niceties beyond what a plain
input gives, and any wasm-bindgen/tooling adoption.

## Definition of Done (per process, decision 0020/0030)

- The chapter, SUMMARY entry, and working exhibit on the published book.
- CI `vessel` job green; `book.yml` deploy step proven on main.
- Decision-log entry: deploy-built wasm, not committed (the divergence
  from the committed-bundle pattern, and why: half-MB binary per worldgen
  change vs a 2-3 min deploy step; identity held by the CI smoke).
- Idea-registry rows above; frontier essay stub only if one earns it.
- Chronicle entry + freshness sweep (the Atlas and Live Orrery chapters
  gain a sibling cross-reference; the gallery index mentions the live
  possession next to the frozen transcript).
- Campaign retrospective in `docs/retrospectives/`.
