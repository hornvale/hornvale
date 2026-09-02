//! The shape fingerprint: FNV-1a 64 over the sorted member names joined
//! with '\n', rendered as the first 6 lowercase hex digits. Hand-rolled
//! because the fingerprint is committed inside `placement:` tags — std's
//! DefaultHasher is documented as unstable across releases and cannot back
//! a committed spelling.

/// Fingerprint a sorted member-name list.
pub fn of(members: &[String]) -> String {
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    let mut feed = |byte: u8| {
        h ^= u64::from(byte);
        h = h.wrapping_mul(0x0000_0100_0000_01b3);
    };
    for (i, m) in members.iter().enumerate() {
        if i > 0 {
            feed(b'\n');
        }
        for b in m.bytes() {
            feed(b);
        }
    }
    format!("{h:016x}")[..6].to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn owned(names: &[&str]) -> Vec<String> {
        names.iter().map(|n| n.to_string()).collect()
    }

    #[test]
    fn the_same_member_list_fingerprints_the_same_way_twice() {
        let a = owned(&["Bright", "Dim", "Level"]);
        let b = owned(&["Bright", "Dim", "Level"]);
        assert_eq!(of(&a), of(&b));
    }

    #[test]
    fn a_different_member_list_fingerprints_differently() {
        let a = owned(&["Bright", "Dim", "Level"]);
        let c = owned(&["Bright", "Dim", "Askew"]);
        assert_ne!(of(&a), of(&c));
    }

    #[test]
    fn the_fingerprint_is_six_lowercase_hex_digits() {
        let fp = of(&owned(&["x", "y"]));
        assert_eq!(fp.len(), 6);
        assert!(
            fp.chars()
                .all(|c| c.is_ascii_hexdigit() && !c.is_ascii_uppercase())
        );
    }

    #[test]
    fn pins_the_verdicts_fixtures_hand_computed_fingerprints() {
        // The `tests/fixtures/verdicts/` fixture (Task 10, Step 5) embeds
        // these two hex values literally as its `shape(...)` tags. They were
        // computed by hand (an independent FNV-1a implementation, not this
        // one) before being written into the fixture text, per the
        // reasonless-tag-is-an-error discipline: the fixture's tag must be
        // provably right, never guessed. This test is the tripwire — if the
        // hash ever changes, both this assertion and the fixture go stale
        // together instead of the fixture silently drifting alone.
        assert_eq!(of(&owned(&["Alpha", "Beta", "Gamma"])), "6e79e6");
        assert_eq!(of(&owned(&["One", "Two"])), "e23a3d");
    }

    #[test]
    fn order_insensitivity_is_the_callers_responsibility_not_the_functions() {
        // of() hashes the slice in the order given — it does not sort. Two
        // callers passing the same set in different orders get different
        // fingerprints unless they both sort first, which is what every
        // caller in this crate does (extract.rs sorts members on push).
        let sorted = owned(&["Bright", "Dim", "Level"]);
        let unsorted = owned(&["Level", "Bright", "Dim"]);
        assert_ne!(of(&sorted), of(&unsorted));
    }
}
