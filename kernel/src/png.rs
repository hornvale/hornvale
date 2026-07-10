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
/// type-audit: bare-ok(render-internal: width), bare-ok(render-internal: height), bare-ok(render-internal: rgb), bare-ok(artifact: return)
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
        let rgb = vec![0_u8; 4 * 2 * 3];
        let png = encode_rgb(4, 2, &rgb);
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

    /// A raw stream that is an exact multiple of the stored-block size:
    /// 28 × 771 RGB → (28 × 3 + 1) × 771 = 65,535 filtered bytes — one
    /// full stored block that is also the final block (LEN = 0xFFFF,
    /// BFINAL = 1).
    #[test]
    fn exact_block_multiple_ends_on_a_full_final_block() {
        let (w, h) = (28_u32, 771_u32);
        let rgb: Vec<u8> = (0..w as usize * h as usize * 3)
            .map(|i| (i % 253) as u8)
            .collect();
        let png = encode_rgb(w, h, &rgb);

        // Locate the IDAT payload.
        let mut idat = Vec::new();
        let mut pos = 8;
        while pos < png.len() {
            let len = u32::from_be_bytes(png[pos..pos + 4].try_into().unwrap()) as usize;
            if &png[pos + 4..pos + 8] == b"IDAT" {
                idat.extend_from_slice(&png[pos + 8..pos + 8 + len]);
            }
            pos += 12 + len;
        }

        // One stored block: BFINAL = 1, LEN = 0xFFFF, then exactly the
        // 65,535 payload bytes and the 4-byte Adler-32 — nothing else.
        assert_eq!(idat[2], 1, "single full block must be final");
        assert_eq!(u16::from_le_bytes([idat[3], idat[4]]), 0xFFFF);
        assert_eq!(idat.len(), 2 + 5 + 65_535 + 4);
    }

    #[test]
    #[should_panic(expected = "width * height * 3")]
    fn wrong_buffer_length_panics() {
        encode_rgb(4, 2, &[0_u8; 5]);
    }
}
