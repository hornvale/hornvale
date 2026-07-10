//! Deterministic asciinema-v2 cast encoding (The Orrery spec): a render
//! evaluated over a time range becomes one self-contained animation,
//! embeddable in the Book and convertible to mp4 by external tools
//! (`agg`/ffmpeg). Pure `serde_json`; **synthetic** frame timing (no
//! wall-clock), so the output is byte-deterministic and drift-checkable like
//! every gallery artifact.
#![warn(missing_docs)]

use serde::Serialize;

/// One asciinema output event, serialized as the array `[time, "o", data]`:
/// terminal output `data` shown at relative `time` seconds.
#[derive(Serialize)]
struct Event(f64, &'static str, String);

/// Encode a flip-book of terminal frames as an asciinema-v2 document: a JSON
/// header line, then one output event per frame at synthetic time `k * dt`.
/// Each frame string is emitted verbatim as terminal output — callers include
/// any screen-clear so playback redraws frame by frame. Deterministic: no
/// wall-clock, fixed header key order, `serde_json` escaping.
pub fn asciinema_v2(width: u16, height: u16, dt: f64, frames: &[String]) -> String {
    // Header with fixed key order (no wall-clock `timestamp`, for determinism).
    let mut out = format!(r#"{{"version": 2, "width": {width}, "height": {height}}}"#);
    out.push('\n');
    for (k, frame) in frames.iter().enumerate() {
        let event = Event(k as f64 * dt, "o", frame.clone());
        out.push_str(&serde_json::to_string(&event).expect("event serializes"));
        out.push('\n');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_and_events_are_well_formed() {
        let frames = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        let cast = asciinema_v2(80, 24, 0.5, &frames);
        let mut lines = cast.lines();
        let header: serde_json::Value = serde_json::from_str(lines.next().unwrap()).unwrap();
        assert_eq!(header["version"], 2);
        assert_eq!(header["width"], 80);
        assert_eq!(header["height"], 24);
        let events: Vec<serde_json::Value> =
            lines.map(|l| serde_json::from_str(l).unwrap()).collect();
        assert_eq!(events.len(), 3);
        for (k, ev) in events.iter().enumerate() {
            assert_eq!(ev[0].as_f64().unwrap(), k as f64 * 0.5);
            assert_eq!(ev[1], "o");
        }
        assert_eq!(events[2][2], "c");
    }

    #[test]
    fn encoding_is_byte_deterministic() {
        let frames = vec!["x\ny".to_string(), "\u{1b}[2Jz".to_string()];
        assert_eq!(
            asciinema_v2(40, 10, 0.1, &frames),
            asciinema_v2(40, 10, 0.1, &frames)
        );
    }

    #[test]
    fn ansi_escapes_and_newlines_survive_the_round_trip() {
        let frame = "\u{1b}[38;5;39m*\u{1b}[0m\nline2\n".to_string();
        let cast = asciinema_v2(10, 2, 1.0, std::slice::from_ref(&frame));
        let ev: serde_json::Value = serde_json::from_str(cast.lines().nth(1).unwrap()).unwrap();
        assert_eq!(ev[2].as_str().unwrap(), frame);
    }
}
