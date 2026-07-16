//! Content-addressed audio artifacts for the book's phonology page: a
//! hand-rolled CRC-32 names each clip after its espeak formulation, and
//! `hornvale voice` authors any missing clips offline via espeak-ng +
//! ffmpeg (the only place in the workspace that shells out to either).

use hornvale_kernel::{Seed, World};
use std::fs;
use std::path::Path;

/// The pinned espeak-ng voice. Changing it only affects newly authored
/// clips — existing files are content-addressed and never regenerated.
const ESPEAK_VOICE: &str = "en";
/// The pinned espeak-ng speaking rate, words per minute.
const ESPEAK_SPEED: &str = "130";

/// `hornvale voice [--out DIR]`: author any missing audio clips for the
/// phonology page's sample names (default out: `book/src/audio`). The
/// offline authoring step — espeak-ng and ffmpeg run here and nowhere
/// else; output is committed and CI only ever checks the file *set*.
pub(crate) fn cmd_voice(args: &[String]) -> Result<(), String> {
    let out_dir = Path::new(crate::flag_value(args, "--out").unwrap_or("book/src/audio"));
    fs::create_dir_all(out_dir).map_err(|e| format!("creating {}: {e}", out_dir.display()))?;
    let world = World::new(Seed(crate::phonology::REFERENCE_SEED));
    let (mut written, mut kept) = (0u32, 0u32);
    // peopled-only: fauna never speak, so `sample_names_for` (which reaches
    // `peopled(def)`) is undefined for them (Task 4 widened `registry()` to
    // include biosphere-only kinds).
    for (species, def) in hornvale_species::registry() {
        if def.peopled.is_none() {
            continue;
        }
        for (_, name) in crate::phonology::sample_names_for(&world, species, &def) {
            let path = out_dir.join(audio_filename(&name.espeak));
            if path.exists() {
                kept += 1;
                continue;
            }
            record(&name.espeak, &path)?;
            println!(
                "wrote {}  ({species} {} {})",
                path.display(),
                name.roman,
                name.espeak
            );
            written += 1;
        }
    }
    println!("{written} clip(s) written, {kept} already present");
    Ok(())
}

/// Author one clip: espeak-ng renders the formulation to a temporary wav,
/// ffmpeg encodes it to mp3 at `out`. Fails loudly naming whichever tool
/// is missing or unhappy.
fn record(formulation: &str, out: &Path) -> Result<(), String> {
    let wav = std::env::temp_dir().join(format!("hornvale-voice-{}.wav", std::process::id()));
    let wav_str = wav.to_str().ok_or("temp dir path is not UTF-8")?;
    let out_str = out.to_str().ok_or("output path is not UTF-8")?;
    run(
        "espeak-ng",
        &[
            "-v",
            ESPEAK_VOICE,
            "-s",
            ESPEAK_SPEED,
            "-w",
            wav_str,
            formulation,
        ],
    )?;
    let result = run(
        "ffmpeg",
        &[
            "-y",
            "-loglevel",
            "error",
            "-i",
            wav_str,
            "-codec:a",
            "libmp3lame",
            "-qscale:a",
            "4",
            out_str,
        ],
    );
    let _ = fs::remove_file(&wav);
    result
}

/// Run one external authoring tool to completion, inheriting stderr so its
/// diagnostics reach the operator.
fn run(tool: &str, args: &[&str]) -> Result<(), String> {
    let status = std::process::Command::new(tool)
        .args(args)
        .status()
        .map_err(|e| format!("{tool}: {e} (is it installed and on PATH?)"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("{tool} exited with {status}"))
    }
}

/// CRC-32/IEEE (the zlib polynomial, reflected, bitwise — no table), the
/// checksum content-addressing the book's audio clips. Hand-rolled to keep
/// the serde-only dependency allowlist intact.
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
