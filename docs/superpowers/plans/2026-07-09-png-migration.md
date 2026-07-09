# PNG Migration (Pay the 0018 Debt) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement decision 0018 — a hand-rolled, pure-std PNG encoder in the kernel; the three domain raster renders switch from P6 PPM to PNG; the committed gallery artifacts migrate off PPM/BMP; CI's artifact command list updates.

**Architecture:** One new kernel module (`kernel/src/png.rs`) encodes 8-bit RGB pixel buffers as PNG using stored (uncompressed) deflate blocks, a compile-time CRC32 table, and Adler-32 — deterministic bytes, no dependencies. Each domain `render.rs` splits its pixel loop from its encoding: a raw-RGB pixel function plus a `*_png` public function that calls the kernel encoder. The old `*_ppm` functions are deleted (their only caller is the CLI). Settlement's overlay, which today parses a PPM header, instead stamps raw RGB pixels and encodes. The First Light example swaps its hand-rolled BMP writer for the same encoder.

**Tech Stack:** Rust edition 2024, std only (decision 0004). Workspace crates: `hornvale-kernel`, `hornvale-terrain`, `hornvale-climate`, `hornvale-settlement`, `hornvale` (CLI).

**Governing documents:** decision 0018 (`docs/decisions/0018-gallery-images-are-hand-rolled-png.md`); rendering strategy spec (`docs/superpowers/specs/2026-07-09-rendering-strategy-design.md`), §2 Ring 1 and §5 item 1.

## Global Constraints

- **No new dependencies** — `serde` + `serde_json` only, workspace-wide (decision 0004; enforced by `cli/tests/architecture.rs`). The encoder is std-only.
- **Determinism** — same seed + same pins → byte-identical artifacts. No wall-clock, no `HashMap`/`HashSet` (`clippy.toml` enforces).
- **Layering** — domains depend on `hornvale-kernel` and nothing else; never on another domain (decision 0002).
- **`#![warn(missing_docs)]`** — every public item, field, and constant gets a one-line doc comment. All crates already set this.
- **The full gate before every commit:** `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`. Run `cargo fmt` (no `--check`) as the final step before each commit.
- **Never `--no-verify`.** Commit hooks run the gate; if they fail, fix the code.
- Workspace root: `/Users/nathan/Projects/hornvale/hornvale`. All commands run from there.

---

### Task 1: Kernel PNG encoder

**Files:**
- Create: `kernel/src/png.rs`
- Modify: `kernel/src/lib.rs` (add `pub mod png;` to the module list)

**Interfaces:**
- Consumes: nothing (std only).
- Produces: `hornvale_kernel::png::encode_rgb(width: u32, height: u32, rgb: &[u8]) -> Vec<u8>` — `rgb` is row-major 8-bit RGB, top row first, length exactly `width * height * 3`; panics otherwise (fail fast — a caller bug). Returns complete PNG file bytes. Tasks 2–5 all call this.

- [ ] **Step 1: Write the module with a stubbed `encode_rgb`**

Create `kernel/src/png.rs` exactly as below, **except** with `encode_rgb`'s body stubbed:

```rust
pub fn encode_rgb(width: u32, height: u32, rgb: &[u8]) -> Vec<u8> {
    let _ = (width, height, rgb);
    Vec::new()
}
```

(keep the real doc comment from the listing). Everything else — helpers and tests — goes in as written:

```rust
//! Hand-rolled PNG encoding (decision 0018): stored-deflate zlib and a
//! compile-time CRC32 table — pure std, no dependencies. Output is
//! deterministic bytes (fixed chunk layout, no compression heuristics), so
//! the gallery drift check holds. Encodes 8-bit RGB only — the one pixel
//! format the gallery uses. Larger than a compressing encoder would emit;
//! at gallery resolutions the penalty is irrelevant (0018).

/// The 8-byte PNG file signature.
const SIGNATURE: [u8; 8] = [0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A];

/// Largest payload of one stored deflate block.
const STORED_BLOCK_MAX: usize = 0xFFFF;

/// CRC32 (IEEE, reflected) lookup table, built at compile time.
const CRC_TABLE: [u32; 256] = {
    let mut table = [0u32; 256];
    let mut n = 0;
    while n < 256 {
        let mut c = n as u32;
        let mut k = 0;
        while k < 8 {
            c = if c & 1 != 0 {
                0xEDB8_8320 ^ (c >> 1)
            } else {
                c >> 1
            };
            k += 1;
        }
        table[n] = c;
        n += 1;
    }
    table
};

/// CRC32 of `data` — the PNG chunk checksum.
fn crc32(data: &[u8]) -> u32 {
    let mut c = 0xFFFF_FFFF_u32;
    for &b in data {
        c = CRC_TABLE[((c ^ u32::from(b)) & 0xFF) as usize] ^ (c >> 8);
    }
    c ^ 0xFFFF_FFFF
}

/// Adler-32 of `data` — the zlib stream checksum.
fn adler32(data: &[u8]) -> u32 {
    const MOD: u32 = 65_521;
    let (mut a, mut b) = (1_u32, 0_u32);
    for &byte in data {
        a = (a + u32::from(byte)) % MOD;
        b = (b + a) % MOD;
    }
    (b << 16) | a
}

/// Append one chunk: big-endian length, type, payload, CRC over type +
/// payload.
fn push_chunk(out: &mut Vec<u8>, kind: &[u8; 4], payload: &[u8]) {
    out.extend_from_slice(&(payload.len() as u32).to_be_bytes());
    out.extend_from_slice(kind);
    out.extend_from_slice(payload);
    let mut crc_input = Vec::with_capacity(4 + payload.len());
    crc_input.extend_from_slice(kind);
    crc_input.extend_from_slice(payload);
    out.extend_from_slice(&crc32(&crc_input).to_be_bytes());
}

/// A zlib stream holding `raw` in stored (uncompressed) deflate blocks.
fn zlib_stored(raw: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(raw.len() + raw.len() / STORED_BLOCK_MAX * 5 + 16);
    out.extend_from_slice(&[0x78, 0x01]); // CMF/FLG: 32K window, check bits valid
    let mut blocks = raw.chunks(STORED_BLOCK_MAX).peekable();
    while let Some(block) = blocks.next() {
        out.push(u8::from(blocks.peek().is_none())); // BFINAL; BTYPE=00 (stored)
        let len = block.len() as u16;
        out.extend_from_slice(&len.to_le_bytes());
        out.extend_from_slice(&(!len).to_le_bytes());
        out.extend_from_slice(block);
    }
    out.extend_from_slice(&adler32(raw).to_be_bytes());
    out
}

/// Encode `width × height` 8-bit RGB pixels (row-major, top row first) as a
/// complete PNG file. Deterministic: fixed chunk layout (IHDR, one IDAT,
/// IEND), stored deflate, filter 0 on every row. Panics unless
/// `rgb.len() == width * height * 3` and both dimensions are nonzero — a
/// caller bug, not an input condition.
pub fn encode_rgb(width: u32, height: u32, rgb: &[u8]) -> Vec<u8> {
    assert!(width > 0 && height > 0, "encode_rgb: zero dimension");
    assert_eq!(
        rgb.len(),
        width as usize * height as usize * 3,
        "encode_rgb: pixel buffer must be width * height * 3 bytes"
    );
    // Raw scanline data: each row prefixed with filter byte 0 (None).
    let row = width as usize * 3;
    let mut raw = Vec::with_capacity((row + 1) * height as usize);
    for scanline in rgb.chunks(row) {
        raw.push(0);
        raw.extend_from_slice(scanline);
    }
    let mut ihdr = Vec::with_capacity(13);
    ihdr.extend_from_slice(&width.to_be_bytes());
    ihdr.extend_from_slice(&height.to_be_bytes());
    ihdr.extend_from_slice(&[8, 2, 0, 0, 0]); // 8-bit, RGB, deflate, filter 0, no interlace
    let mut out = Vec::new();
    out.extend_from_slice(&SIGNATURE);
    push_chunk(&mut out, b"IHDR", &ihdr);
    push_chunk(&mut out, b"IDAT", &zlib_stored(&raw));
    push_chunk(&mut out, b"IEND", &[]);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The standard CRC32 check value.
    #[test]
    fn crc32_matches_the_standard_check_value() {
        assert_eq!(crc32(b"123456789"), 0xCBF4_3926);
    }

    /// The RFC 1950 example checksum.
    #[test]
    fn adler32_matches_the_known_vector() {
        assert_eq!(adler32(b"Wikipedia"), 0x11E6_0398);
    }

    #[test]
    fn encode_is_byte_deterministic() {
        let rgb = vec![7_u8; 4 * 2 * 3];
        assert_eq!(encode_rgb(4, 2, &rgb), encode_rgb(4, 2, &rgb));
    }

    #[test]
    fn signature_and_ihdr_are_well_formed() {
        let png = encode_rgb(4, 2, &vec![0_u8; 4 * 2 * 3]);
        assert!(png.starts_with(&SIGNATURE));
        // IHDR: length 13 at offset 8, type at 12, width at 16, height at 20.
        assert_eq!(&png[8..12], &13_u32.to_be_bytes());
        assert_eq!(&png[12..16], b"IHDR");
        assert_eq!(&png[16..20], &4_u32.to_be_bytes());
        assert_eq!(&png[20..24], &2_u32.to_be_bytes());
        // Bit depth 8, color type 2 (RGB).
        assert_eq!(&png[24..26], &[8, 2]);
        assert!(png.ends_with(&[b'I', b'E', b'N', b'D', 0xAE, 0x42, 0x60, 0x82]));
    }

    /// Full structural round trip without a decoder dependency: walk the
    /// chunks, verify every CRC, reassemble the stored deflate blocks, and
    /// check the payload is exactly the filter-prefixed scanlines with a
    /// valid Adler-32. 300 × 250 pixels → 225,250 raw bytes → four stored
    /// blocks, so the multi-block path is exercised.
    #[test]
    fn stored_blocks_reassemble_to_the_scanlines() {
        let (w, h) = (300_u32, 250_u32);
        let rgb: Vec<u8> = (0..w as usize * h as usize * 3)
            .map(|i| (i % 251) as u8)
            .collect();
        let png = encode_rgb(w, h, &rgb);

        // Expected raw stream: filter byte 0 before each row.
        let mut expected = Vec::new();
        for row in rgb.chunks(w as usize * 3) {
            expected.push(0);
            expected.extend_from_slice(row);
        }

        // Walk chunks, collecting IDAT and checking CRCs.
        let mut idat = Vec::new();
        let mut pos = 8;
        while pos < png.len() {
            let len = u32::from_be_bytes(png[pos..pos + 4].try_into().unwrap()) as usize;
            let kind = &png[pos + 4..pos + 8];
            let payload = &png[pos + 8..pos + 8 + len];
            let crc = u32::from_be_bytes(png[pos + 8 + len..pos + 12 + len].try_into().unwrap());
            let mut crc_input = kind.to_vec();
            crc_input.extend_from_slice(payload);
            assert_eq!(crc, crc32(&crc_input), "bad CRC on {kind:?}");
            if kind == b"IDAT" {
                idat.extend_from_slice(payload);
            }
            pos += 12 + len;
        }

        // Parse the zlib stream: 2-byte header, stored blocks, Adler-32.
        assert_eq!(&idat[..2], &[0x78, 0x01]);
        let mut raw = Vec::new();
        let mut p = 2;
        loop {
            let bfinal = idat[p];
            let len = u16::from_le_bytes([idat[p + 1], idat[p + 2]]) as usize;
            let nlen = u16::from_le_bytes([idat[p + 3], idat[p + 4]]);
            assert_eq!(nlen, !(len as u16), "stored block NLEN mismatch");
            raw.extend_from_slice(&idat[p + 5..p + 5 + len]);
            p += 5 + len;
            if bfinal == 1 {
                break;
            }
        }
        assert_eq!(raw, expected);
        assert_eq!(
            u32::from_be_bytes(idat[p..p + 4].try_into().unwrap()),
            adler32(&raw)
        );
    }

    #[test]
    #[should_panic(expected = "width * height * 3")]
    fn wrong_buffer_length_panics() {
        encode_rgb(4, 2, &[0_u8; 5]);
    }
}
```

In `kernel/src/lib.rs`, add to the module list (alphabetical, between `phenomena` and `refine`):

```rust
pub mod png;
```

No root re-export: `png::encode_rgb` reads better qualified than a bare `encode_rgb` at the crate root.

- [ ] **Step 2: Run the tests, expect red**

Run: `cargo test -p hornvale-kernel png`
Expected: FAIL — `signature_and_ihdr_are_well_formed` and `stored_blocks_reassemble_to_the_scanlines` panic on out-of-bounds indexing into the empty vec, and `wrong_buffer_length_panics` fails because the stub never panics. (`crc32_matches_the_standard_check_value`, `adler32_matches_the_known_vector`, and `encode_is_byte_deterministic` pass — their subjects are already implemented.)

- [ ] **Step 3: Replace the stub with the real body from the listing, run green**

Run: `cargo test -p hornvale-kernel png`
Expected: `test result: ok. 6 passed`

- [ ] **Step 4: Full gate**

Run: `cargo test --workspace && cargo fmt && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all green. (Watch for `missing_docs` warnings — every `pub` item above has a doc comment; keep them.)

- [ ] **Step 5: Commit**

```bash
git add kernel/src/png.rs kernel/src/lib.rs
git commit -m "feat(kernel): hand-rolled pure-std PNG encoder (decision 0018)

Stored-deflate zlib, compile-time CRC32 table, Adler-32 — deterministic
bytes, no dependencies. 8-bit RGB only, the gallery's one pixel format."
```

---

### Task 2: Terrain — `elevation_png` replaces `elevation_ppm`

**Files:**
- Modify: `domains/terrain/src/render.rs`
- Modify: `cli/src/main.rs` (`cmd_map`, usage text)
- Modify: `.github/workflows/ci.yml` (the `map` line of "Artifacts are current")
- Delete: `book/src/gallery/elevation-seed-42.ppm` (regenerated as `.png`)

**Interfaces:**
- Consumes: `hornvale_kernel::png::encode_rgb(u32, u32, &[u8]) -> Vec<u8>` (Task 1).
- Produces: `hornvale_terrain::render::elevation_png(geo: &Geosphere, globe: &TectonicGlobe) -> Vec<u8>` (PNG file bytes). `elevation_ppm` is deleted; its only caller was `cmd_map`.

- [ ] **Step 1: Replace the PPM test with a PNG test (red)**

In `domains/terrain/src/render.rs` tests, replace `ppm_is_well_formed_and_byte_deterministic` with:

```rust
#[test]
fn png_is_well_formed_and_byte_deterministic() {
    let geo = Geosphere::new(4);
    let globe = generate(Seed(42), &geo, &TerrainPins::default())
        .unwrap()
        .globe;
    let a = elevation_png(&geo, &globe);
    assert_eq!(a, elevation_png(&geo, &globe));
    assert!(a.starts_with(&[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]));
    // IHDR width and height, big-endian, at offsets 16 and 20.
    assert_eq!(&a[16..20], &MAP_WIDTH.to_be_bytes());
    assert_eq!(&a[20..24], &(MAP_WIDTH / 2).to_be_bytes());
}
```

Run: `cargo test -p hornvale-terrain render`
Expected: FAIL — `elevation_png` not found.

- [ ] **Step 2: Split pixels from encoding; delete PPM**

In `domains/terrain/src/render.rs`, replace `elevation_ppm` with:

```rust
/// Raw RGB pixels of the equirectangular elevation map (row-major, top row
/// first): longitude −180 → 180 across, latitude 90 → −90 down, pixel
/// centers sampled.
fn elevation_pixels(geo: &Geosphere, globe: &TectonicGlobe) -> Vec<u8> {
    let (width, height) = (MAP_WIDTH, MAP_WIDTH / 2);
    let index = LatBandIndex::new(geo);
    let mut out = Vec::with_capacity((width * height * 3) as usize);
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..width {
            let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
            let cell = index.nearest(geo, latitude, longitude);
            out.extend_from_slice(&color(*globe.elevation.get(cell), globe.sea_level));
        }
    }
    out
}

/// Render the globe as an equirectangular PNG (decision 0018). Same globe,
/// same bytes.
pub fn elevation_png(geo: &Geosphere, globe: &TectonicGlobe) -> Vec<u8> {
    hornvale_kernel::png::encode_rgb(MAP_WIDTH, MAP_WIDTH / 2, &elevation_pixels(geo, globe))
}
```

Update the stale docs in the same file: the module doc's "an equirectangular P6 PPM elevation map" becomes "an equirectangular PNG elevation map (decision 0018)", and `MAP_WIDTH`'s doc "PPM image width in pixels" becomes "Raster image width in pixels".

Run: `cargo test -p hornvale-terrain render` — expected: FAIL only in `cli` build if run workspace-wide (cmd_map still calls `elevation_ppm`); the terrain crate's own tests PASS.

- [ ] **Step 3: Update the CLI**

In `cli/src/main.rs` `cmd_map`, replace the `--out` block's first two lines:

```rust
        let png = hornvale_terrain::render::elevation_png(terrain.geosphere(), terrain.globe());
        std::fs::write(out, png).map_err(|e| format!("writing {out}: {e}"))?;
```

and replace the link line so the image renders inline in browsers (the point of 0018):

```rust
        doc.push_str(&format!("![Full-color render](./{name})\n\n"));
```

Update `cmd_map`'s doc comment ("the PPM image to disk" → "the PNG image to disk") and the usage line 34:

```text
  hornvale map [--world <PATH>] [--out <PNG>] render the elevation map (markdown to stdout)
```

Run: `cargo test --workspace`
Expected: PASS.

- [ ] **Step 4: Regenerate the gallery artifact**

```bash
git rm book/src/gallery/elevation-seed-42.ppm
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-png-sky.json
cargo run -p hornvale -- map --world /tmp/hv-png-sky.json --out book/src/gallery/elevation-seed-42.png > book/src/gallery/elevation-seed-42.md
file book/src/gallery/elevation-seed-42.png
```

Expected `file` output: `PNG image data, 256 x 128, 8-bit/color RGB, non-interlaced` — a system decoder accepting the bytes is the end-to-end check.

- [ ] **Step 5: Update CI**

In `.github/workflows/ci.yml`, the `map` line of "Artifacts are current" changes its `--out`:

```yaml
          cargo run -p hornvale -- map --world /tmp/hv-ci-sky.json --out book/src/gallery/elevation-seed-42.png > book/src/gallery/elevation-seed-42.md
```

- [ ] **Step 6: Full gate, then commit**

Run: `cargo test --workspace && cargo fmt && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`

```bash
git add domains/terrain/src/render.rs cli/src/main.rs .github/workflows/ci.yml book/src/gallery/elevation-seed-42.png book/src/gallery/elevation-seed-42.md
git commit -m "feat(terrain): elevation map renders as PNG (decision 0018)

elevation_ppm becomes private elevation_pixels + public elevation_png;
the gallery artifact migrates and the page embeds it inline."
```

---

### Task 3: Climate — `biome_pixels` + `biome_png` replace `biome_ppm`

**Files:**
- Modify: `domains/climate/src/render.rs`
- Modify: `cli/src/main.rs` (`cmd_biome_map`, usage text)
- Modify: `.github/workflows/ci.yml` (both `biome-map` lines)
- Delete: `book/src/gallery/biome-seed-42.ppm`, `book/src/gallery/biome-seed-42-locked.ppm` (regenerated as `.png`)

**Interfaces:**
- Consumes: `hornvale_kernel::png::encode_rgb` (Task 1).
- Produces: `hornvale_climate::render::biome_pixels(geo: &Geosphere, biomes: &CellMap<Biome>) -> Vec<u8>` — **public** raw RGB, row-major, top row first, `MAP_WIDTH × MAP_WIDTH/2` (256×128); Task 4's overlay stamps this buffer at the composition root. And `hornvale_climate::render::biome_png(geo, biomes) -> Vec<u8>` — PNG file bytes. `biome_ppm` is deleted.

- [ ] **Step 1: Replace the PPM test with PNG + pixels tests (red)**

In `domains/climate/src/render.rs` tests, replace `ppm_is_well_formed_and_byte_deterministic` (keep its `checker` helper and any other tests) with:

```rust
#[test]
fn png_is_well_formed_and_byte_deterministic() {
    let geo = Geosphere::new(4);
    let biomes = checker(&geo);
    let a = biome_png(&geo, &biomes);
    assert_eq!(a, biome_png(&geo, &biomes));
    assert!(a.starts_with(&[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]));
    assert_eq!(&a[16..20], &MAP_WIDTH.to_be_bytes());
    assert_eq!(&a[20..24], &(MAP_WIDTH / 2).to_be_bytes());
}

#[test]
fn pixel_buffer_is_exactly_the_raster() {
    let geo = Geosphere::new(4);
    let biomes = checker(&geo);
    let pixels = biome_pixels(&geo, &biomes);
    assert_eq!(pixels.len(), (MAP_WIDTH * (MAP_WIDTH / 2) * 3) as usize);
    assert_eq!(pixels, biome_pixels(&geo, &biomes));
}
```

Run: `cargo test -p hornvale-climate render`
Expected: FAIL — `biome_png`, `biome_pixels` not found.

- [ ] **Step 2: Implement; delete PPM**

In `domains/climate/src/render.rs`, replace `biome_ppm` with:

```rust
/// Raw RGB pixels of the equirectangular biome map (row-major, top row
/// first) — also the base image settlement's overlay stamps at the
/// composition root.
pub fn biome_pixels(geo: &Geosphere, biomes: &CellMap<Biome>) -> Vec<u8> {
    let (width, height) = (MAP_WIDTH, MAP_WIDTH / 2);
    let index = LatBandIndex::new(geo);
    let mut out = Vec::with_capacity((width * height * 3) as usize);
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..width {
            let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
            let cell = index.nearest(geo, latitude, longitude);
            out.extend_from_slice(&biomes.get(cell).color());
        }
    }
    out
}

/// Render the biome field as an equirectangular PNG (decision 0018). Same
/// field, same bytes.
pub fn biome_png(geo: &Geosphere, biomes: &CellMap<Biome>) -> Vec<u8> {
    hornvale_kernel::png::encode_rgb(MAP_WIDTH, MAP_WIDTH / 2, &biome_pixels(geo, biomes))
}
```

Update the stale docs: module doc "an equirectangular P6 PPM" → "an equirectangular PNG (decision 0018)"; `MAP_WIDTH` doc "PPM image width in pixels" → "Raster image width in pixels".

Run: `cargo test -p hornvale-climate render`
Expected: PASS (the `cli` crate still fails to build workspace-wide until Step 3 — `cmd_biome_map` and `cmd_settlement_map` call `biome_ppm`).

- [ ] **Step 3: Update the CLI (both call sites)**

In `cli/src/main.rs` `cmd_biome_map`, replace the `--out` block's first two lines:

```rust
        let png = hornvale_climate::render::biome_png(climate.geosphere(), &climate.biome_map());
        std::fs::write(out, png).map_err(|e| format!("writing {out}: {e}"))?;
```

and the link line:

```rust
        doc.push_str(&format!("![Full-color render](./{name})\n\n"));
```

`cmd_settlement_map` also calls the deleted `biome_ppm` (it feeds settlement's `overlay_ppm`, which Task 4 replaces). To keep this task's commit green without reaching into settlement, bridge it with a three-line shim that rebuilds the old PPM base from the new pixel API — Task 4 deletes it:

```rust
        let pixels =
            hornvale_climate::render::biome_pixels(climate.geosphere(), &climate.biome_map());
        let mut base = b"P6\n256 128\n255\n".to_vec();
        base.extend_from_slice(&pixels);
        let ppm = hornvale_settlement::render::overlay_ppm(&base, &sites, flagship);
        std::fs::write(out, ppm).map_err(|e| format!("writing {out}: {e}"))?;
```

(The settlement gallery artifact stays PPM until Task 4 regenerates it.)

Update `cmd_biome_map`'s doc comment ("the PPM image" → "the PNG image") and usage line 35:

```text
  hornvale biome-map [--world <PATH>] [--out <PNG>] render the biome map (markdown to stdout)
```

Run: `cargo test --workspace`
Expected: PASS.

- [ ] **Step 4: Regenerate the two biome gallery artifacts**

```bash
git rm book/src/gallery/biome-seed-42.ppm book/src/gallery/biome-seed-42-locked.ppm
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-png-sky.json
cargo run -p hornvale -- new --seed 42 --rotation locked --out /tmp/hv-png-locked.json
cargo run -p hornvale -- biome-map --world /tmp/hv-png-sky.json --out book/src/gallery/biome-seed-42.png > book/src/gallery/biome-seed-42.md
cargo run -p hornvale -- biome-map --world /tmp/hv-png-locked.json --out book/src/gallery/biome-seed-42-locked.png > book/src/gallery/biome-seed-42-locked.md
file book/src/gallery/biome-seed-42.png book/src/gallery/biome-seed-42-locked.png
```

Expected: both report `PNG image data, 256 x 128, 8-bit/color RGB, non-interlaced`.

- [ ] **Step 5: Update CI (both biome-map lines)**

```yaml
          cargo run -p hornvale -- biome-map --world /tmp/hv-ci-sky.json --out book/src/gallery/biome-seed-42.png > book/src/gallery/biome-seed-42.md
          cargo run -p hornvale -- biome-map --world /tmp/hv-ci-locked.json --out book/src/gallery/biome-seed-42-locked.png > book/src/gallery/biome-seed-42-locked.md
```

- [ ] **Step 6: Full gate, then commit**

Run: `cargo test --workspace && cargo fmt && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`

```bash
git add domains/climate/src/render.rs cli/src/main.rs .github/workflows/ci.yml book/src/gallery/biome-seed-42.png book/src/gallery/biome-seed-42.md book/src/gallery/biome-seed-42-locked.png book/src/gallery/biome-seed-42-locked.md
git commit -m "feat(climate): biome map renders as PNG (decision 0018)

biome_ppm becomes public biome_pixels + biome_png; both gallery biome
artifacts migrate. cmd_settlement_map bridges via a 3-line PPM shim
that the settlement task deletes."
```

---

### Task 4: Settlement — `overlay_png` replaces `overlay_ppm`

**Files:**
- Modify: `domains/settlement/src/render.rs`
- Modify: `cli/src/main.rs` (`cmd_settlement_map`, usage text — deletes Task 3's shim)
- Modify: `.github/workflows/ci.yml` (both `settlement-map` lines)
- Delete: `book/src/gallery/settlement-seed-42.ppm`, `book/src/gallery/settlement-seed-42-locked.ppm` (regenerated as `.png`)

**Interfaces:**
- Consumes: `hornvale_kernel::png::encode_rgb` (Task 1); `hornvale_climate::render::biome_pixels` (Task 3) at the CLI composition root only — settlement itself stays kernel-only.
- Produces: `hornvale_settlement::render::overlay_png(base: &[u8], sites: &[(f64, f64)], flagship: Option<(f64, f64)>) -> Vec<u8>` — `base` is raw 256×128 RGB (`MAP_WIDTH × MAP_HEIGHT`), row-major, top row first; returns PNG file bytes. Constants `PPM_WIDTH`/`PPM_HEIGHT` are renamed `MAP_WIDTH`/`MAP_HEIGHT` (`usize`, 256/128). `overlay_ppm` and the private `header_len` are deleted.

- [ ] **Step 1: Rewrite the overlay tests for raw pixels (red)**

In `domains/settlement/src/render.rs` tests, replace `base_ppm` and the two overlay tests (keep the ASCII tests). The existing tests assert stamped colors at computed offsets past the PPM header; the new ones assert on the raw buffer via a new private `overlay_pixels`, plus a structural check on `overlay_png`:

```rust
    fn base_pixels() -> Vec<u8> {
        vec![0_u8; MAP_WIDTH * MAP_HEIGHT * 3]
    }

    /// The raw-buffer offset of the pixel for (lat, lon).
    fn pixel_offset(lat: f64, lon: f64) -> usize {
        let (px, py) = pixel_for(lat, lon, MAP_WIDTH, MAP_HEIGHT);
        (py * MAP_WIDTH + px) * 3
    }

    #[test]
    fn overlay_stamps_sites_red_and_flagship_yellow() {
        let out = overlay_pixels(&base_pixels(), &[(30.0, -60.0)], Some((10.0, 20.0)));
        let site = pixel_offset(30.0, -60.0);
        assert_eq!(&out[site..site + 3], &[255, 0, 0]);
        let flag = pixel_offset(10.0, 20.0);
        assert_eq!(&out[flag..flag + 3], &[255, 255, 0]);
    }

    #[test]
    fn flagship_wins_overlap() {
        let out = overlay_pixels(&base_pixels(), &[(10.0, 20.0)], Some((10.0, 20.0)));
        let at = pixel_offset(10.0, 20.0);
        assert_eq!(&out[at..at + 3], &[255, 255, 0]);
    }

    #[test]
    fn overlay_png_is_well_formed_and_deterministic() {
        let a = overlay_png(&base_pixels(), &[(30.0, -60.0)], Some((10.0, 20.0)));
        assert_eq!(
            a,
            overlay_png(&base_pixels(), &[(30.0, -60.0)], Some((10.0, 20.0)))
        );
        assert!(a.starts_with(&[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]));
        assert_eq!(&a[16..20], &(MAP_WIDTH as u32).to_be_bytes());
        assert_eq!(&a[20..24], &(MAP_HEIGHT as u32).to_be_bytes());
    }
```

Run: `cargo test -p hornvale-settlement render`
Expected: FAIL — `overlay_pixels`, `overlay_png`, `MAP_WIDTH` not found.

- [ ] **Step 2: Implement; delete `overlay_ppm` and `header_len`**

In `domains/settlement/src/render.rs`: rename the constants and replace `header_len` + `overlay_ppm` with:

```rust
/// Raster overlay width in pixels; matches `hornvale_climate::render::MAP_WIDTH`.
pub const MAP_WIDTH: usize = 256;
/// Raster overlay height in pixels; `MAP_WIDTH / 2`.
pub const MAP_HEIGHT: usize = 128;
```

```rust
/// Copy `base` (raw 256×128 RGB pixels, row-major, top row first) and stamp
/// settlement marks: red (`[255, 0, 0]`) for each site, yellow
/// (`[255, 255, 0]`) for the flagship — stamped last, so it wins overlap.
fn overlay_pixels(base: &[u8], sites: &[(f64, f64)], flagship: Option<(f64, f64)>) -> Vec<u8> {
    let mut out = base.to_vec();
    let mut stamp = |lat: f64, lon: f64, color: [u8; 3]| {
        let (px, py) = pixel_for(lat, lon, MAP_WIDTH, MAP_HEIGHT);
        let start = (py * MAP_WIDTH + px) * 3;
        if start + 3 <= out.len() {
            out[start..start + 3].copy_from_slice(&color);
        }
    };
    for &(lat, lon) in sites {
        stamp(lat, lon, [255, 0, 0]);
    }
    if let Some((lat, lon)) = flagship {
        stamp(lat, lon, [255, 255, 0]);
    }
    out
}

/// Stamp settlement marks onto `base` (raw 256×128 RGB, e.g.
/// `hornvale_climate::render::biome_pixels`) and encode as a PNG
/// (decision 0018).
pub fn overlay_png(base: &[u8], sites: &[(f64, f64)], flagship: Option<(f64, f64)>) -> Vec<u8> {
    hornvale_kernel::png::encode_rgb(
        MAP_WIDTH as u32,
        MAP_HEIGHT as u32,
        &overlay_pixels(base, sites, flagship),
    )
}
```

Update the module doc: "a P6 PPM overlay onto a caller-supplied base image" → "a PNG overlay onto a caller-supplied raw-RGB base image (decision 0018)"; the module now knows nothing of image *containers*, only bare pixels — note that. Check `domains/settlement/Cargo.toml` already depends on `hornvale-kernel` (it does — all domains do).

Run: `cargo test -p hornvale-settlement render`
Expected: PASS.

- [ ] **Step 3: Update the CLI — delete Task 3's shim**

In `cli/src/main.rs` `cmd_settlement_map`, replace the shimmed `--out` block body with:

```rust
        let pixels =
            hornvale_climate::render::biome_pixels(climate.geosphere(), &climate.biome_map());
        let png = hornvale_settlement::render::overlay_png(&pixels, &sites, flagship);
        std::fs::write(out, png).map_err(|e| format!("writing {out}: {e}"))?;
```

and the link line:

```rust
        doc.push_str(&format!("![Full-color render](./{name})\n\n"));
```

Update `cmd_settlement_map`'s doc comment ("the biome PPM overlaid" → "the biome raster overlaid, as PNG") and usage line 36:

```text
  hornvale settlement-map [--world <PATH>] [--out <PNG>] render the settlement map (markdown to stdout)
```

Run: `cargo test --workspace`
Expected: PASS.

- [ ] **Step 4: Regenerate the two settlement gallery artifacts**

```bash
git rm book/src/gallery/settlement-seed-42.ppm book/src/gallery/settlement-seed-42-locked.ppm
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-png-sky.json
cargo run -p hornvale -- new --seed 42 --rotation locked --out /tmp/hv-png-locked.json
cargo run -p hornvale -- settlement-map --world /tmp/hv-png-sky.json --out book/src/gallery/settlement-seed-42.png > book/src/gallery/settlement-seed-42.md
cargo run -p hornvale -- settlement-map --world /tmp/hv-png-locked.json --out book/src/gallery/settlement-seed-42-locked.png > book/src/gallery/settlement-seed-42-locked.md
file book/src/gallery/settlement-seed-42.png book/src/gallery/settlement-seed-42-locked.png
```

Expected: both report `PNG image data, 256 x 128, 8-bit/color RGB, non-interlaced`.

- [ ] **Step 5: Update CI (both settlement-map lines)**

```yaml
          cargo run -p hornvale -- settlement-map --world /tmp/hv-ci-sky.json --out book/src/gallery/settlement-seed-42.png > book/src/gallery/settlement-seed-42.md
          cargo run -p hornvale -- settlement-map --world /tmp/hv-ci-locked.json --out book/src/gallery/settlement-seed-42-locked.png > book/src/gallery/settlement-seed-42-locked.md
```

- [ ] **Step 6: Full gate, then commit**

Run: `cargo test --workspace && cargo fmt && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`

```bash
git add domains/settlement/src/render.rs cli/src/main.rs .github/workflows/ci.yml book/src/gallery/settlement-seed-42.png book/src/gallery/settlement-seed-42.md book/src/gallery/settlement-seed-42-locked.png book/src/gallery/settlement-seed-42-locked.md
git commit -m "feat(settlement): overlay renders as PNG on raw pixels (decision 0018)

overlay_ppm + header parsing become overlay_pixels + overlay_png; the
CLI shim from the biome task is deleted; both settlement gallery
artifacts migrate."
```

---

### Task 5: First Light — BMP becomes PNG

**Files:**
- Modify: `kernel/examples/first_light.rs`
- Modify: `book/src/gallery/first-light.md` (the image reference)
- Delete: `book/src/gallery/first-light-seed-42.bmp` (regenerated as `.png`)

**Interfaces:**
- Consumes: `hornvale_kernel::png::encode_rgb` (Task 1; the example links its own crate, so the path is `hornvale_kernel::png::encode_rgb`).
- Produces: `book/src/gallery/first-light-seed-42.png`, 512×512.

- [ ] **Step 1: Replace the BMP writer**

In `kernel/examples/first_light.rs`, replace `write_noise_bmp` with:

```rust
/// Render fbm over seed 42's terrain stream as a PNG (decision 0018),
/// mapping the unit interval onto an ink-to-parchment ramp.
fn write_noise_png(path: &Path) -> std::io::Result<()> {
    let size = IMAGE_SIZE;
    let terrain = SEED.derive("terrain");
    let mut rgb = Vec::with_capacity((size * size * 3) as usize);
    for row in 0..size {
        // PNG rows run top-down; the BMP predecessor stored bottom-up with
        // y = size - 1 - row, so top-down y = row keeps the image identical.
        let y = f64::from(row);
        for col in 0..size {
            let x = f64::from(col);
            let v = fbm_2d(terrain, x / 64.0, y / 64.0, 5);
            let (r, g, b) = ramp(v);
            rgb.extend_from_slice(&[r, g, b]);
        }
    }
    let mut file = std::fs::File::create(path)?;
    file.write_all(&hornvale_kernel::png::encode_rgb(size, size, &rgb))
}
```

In `main()`, change the call:

```rust
    write_noise_png(&out.join("first-light-seed-42.png"))?;
```

Update the module doc: "(BMP, dependency-free)" → "(PNG via the kernel's hand-rolled encoder, decision 0018)".

**Sanity note for the implementer:** the old BMP stored rows bottom-up (first stored row = bottom scanline) with `y = size - 1 - row`, i.e. the bottom scanline sampled `y = size - 1` and the top scanline sampled `y = 0`. PNG stores top-down, so the top scanline (row 0) must sample `y = 0` — hence `let y = f64::from(row)`. The rendered image is visually identical; only the container changed.

- [ ] **Step 2: Regenerate and verify**

```bash
git rm book/src/gallery/first-light-seed-42.bmp
cargo run -p hornvale-kernel --example first_light
file book/src/gallery/first-light-seed-42.png
```

Expected: `first light artifacts written to book/src/gallery` then `PNG image data, 512 x 512, 8-bit/color RGB, non-interlaced`. Also confirm `book/src/gallery/world-seed-42.md` is byte-unchanged (`git status` shows no diff on it).

Open the image and eyeball it against the old BMP (both are in git history):

```bash
open book/src/gallery/first-light-seed-42.png
```

Expected: the ink-to-parchment noise field, not vertically flipped (compare `git show HEAD:book/src/gallery/first-light-seed-42.bmp > /tmp/old.bmp && open /tmp/old.bmp` if unsure).

- [ ] **Step 3: Update the book page**

In `book/src/gallery/first-light.md` line 18:

```markdown
![Seed 42's fractal noise field, rendered ink-to-parchment](first-light-seed-42.png)
```

- [ ] **Step 4: Full gate, then commit**

Run: `cargo test --workspace && cargo fmt && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`

```bash
git add kernel/examples/first_light.rs book/src/gallery/first-light-seed-42.png book/src/gallery/first-light.md
git commit -m "feat(kernel): First Light artifact migrates BMP to PNG (decision 0018)"
```

---

### Task 6: Verification sweep and book freshness

**Files:**
- Possibly modify: any file the greps below surface.

**Interfaces:**
- Consumes: everything above.
- Produces: a workspace where the full CI artifact list regenerates byte-identically and no living document references PPM/BMP.

- [ ] **Step 1: Run the full CI artifact list locally**

Run every command from `.github/workflows/ci.yml`'s "Artifacts are current" step exactly as written (post-edit), then:

```bash
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
git status --porcelain book/src/gallery/
```

Expected: no diff, no untracked files (a stray `.ppm` here means a missed `git rm`).

- [ ] **Step 2: Sweep for stale references**

```bash
grep -rn -i "ppm\|\.bmp" --include="*.rs" --include="*.yml" --include="*.toml" . | grep -v target
grep -rn -i "ppm\|\.bmp" book/src docs --include="*.md" | grep -v "book/src/chronicle/" | grep -v "docs/decisions/" | grep -v "docs/retrospectives/" | grep -v "docs/superpowers/"
```

Expected: the first grep returns nothing. The second returns nothing — chronicles, decisions, retrospectives, and specs are historical records and keep their PPM mentions (decision 0018 itself and this plan mention PPM deliberately). If either grep surfaces a living page (e.g. a domain chapter in `book/src/domains/` describing "a hand-rolled PPM"), update that sentence to say PNG and cite decision 0018.

- [ ] **Step 3: Build the book**

```bash
mdbook build book
```

Expected: clean build. Spot-check `book/book/gallery/elevation-seed-42.html` in a browser — the map should now render inline on the page (the payoff 0018 was ratified for).

- [ ] **Step 4: Full gate one last time**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all green.

- [ ] **Step 5: Commit (only if Steps 2–3 changed anything)**

```bash
git add -A book/src docs
git commit -m "docs(book): freshness sweep after the PNG migration (decision 0018)"
```

---

## Definition of Done

- [ ] `hornvale_kernel::png::encode_rgb` exists, documented, with the six unit tests passing.
- [ ] `elevation_png` / `biome_pixels` + `biome_png` / `overlay_png` replace the three `*_ppm` functions; no `_ppm` function remains in the workspace.
- [ ] The gallery holds `.png` artifacts only — five maps (256×128) plus First Light (512×512); every `.ppm`/`.bmp` is `git rm`'d.
- [ ] Generated gallery pages embed images inline (`![Full-color render](…)`).
- [ ] CI's "Artifacts are current" list writes `.png` names and the drift check passes locally.
- [ ] `file` identifies every committed PNG; the mdbook-rendered pages display them inline.
- [ ] Full gate green on every commit.
