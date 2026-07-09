//! Content-addressed audio artifacts for the book's phonology page: a
//! hand-rolled CRC-32 names each clip after its espeak formulation, and
//! `hornvale voice` authors any missing clips offline via espeak-ng +
//! ffmpeg (the only place in the workspace that shells out to either).

/// CRC-32/IEEE (the zlib polynomial, reflected, bitwise — no table), the
/// checksum content-addressing the book's audio clips. Hand-rolled to keep
/// the serde-only dependency allowlist intact.
#[allow(dead_code)]
pub(crate) fn crc32(bytes: &[u8]) -> u32 {
    let mut crc: u32 = 0xffff_ffff;
    for &byte in bytes {
        crc ^= u32::from(byte);
        for _ in 0..8 {
            let mask = (crc & 1).wrapping_neg();
            crc = (crc >> 1) ^ (0xedb8_8320 & mask);
        }
    }
    !crc
}

/// The committed filename for a formulation's clip: eight lowercase hex
/// digits of its CRC-32, `.mp3`. Content-addressing is the freshness
/// mechanism — a phonology change alters the formulation, hence the
/// filename, and the artifact-set test fails until `hornvale voice` runs.
#[allow(dead_code)]
pub(crate) fn audio_filename(formulation: &str) -> String {
    format!("{:08x}.mp3", crc32(formulation.as_bytes()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn crc32_matches_the_ieee_known_answer() {
        // The standard check value for CRC-32/IEEE.
        assert_eq!(crc32(b"123456789"), 0xcbf4_3926);
        assert_eq!(crc32(b""), 0);
    }

    #[test]
    fn audio_filenames_are_eight_lowercase_hex_digits_dot_mp3() {
        let name = audio_filename("[[zv'etnot]]");
        assert_eq!(name.len(), 12);
        assert!(name.ends_with(".mp3"));
        assert!(
            name[..8]
                .chars()
                .all(|c| c.is_ascii_hexdigit() && !c.is_ascii_uppercase())
        );
        // Content-addressed: distinct formulations, distinct names.
        assert_ne!(name, audio_filename("[[q'aq]]"));
    }
}
