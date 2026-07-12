//! Hierarchical seeding and deterministic random streams — the root of the
//! byte-identical guarantee (decision 0001).

use serde::{Deserialize, Serialize};

/// A deterministic seed. The world seed is the root; everything else is
/// derived from it by labeled paths, so adding a new consumer never
/// perturbs existing streams.
/// type-audit: bare-ok(constructor-edge)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Seed(pub u64);

const FNV_OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
const FNV_PRIME: u64 = 0x0000_0100_0000_01b3;

fn splitmix64(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9e37_79b9_7f4a_7c15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    z ^ (z >> 31)
}

impl Seed {
    /// Derive a child seed for `label`. FNV-1a over the label, mixed with
    /// the parent, then scrambled. Stable forever: changing this breaks
    /// every saved world.
    /// type-audit: bare-ok(identifier-text)
    pub fn derive(&self, label: &str) -> Seed {
        let mut h = FNV_OFFSET ^ self.0;
        for byte in label.as_bytes() {
            h ^= u64::from(*byte);
            h = h.wrapping_mul(FNV_PRIME);
        }
        Seed(splitmix64(&mut h))
    }

    /// A fresh deterministic random stream rooted at this seed.
    pub fn stream(&self) -> Stream {
        Stream { state: self.0 }
    }
}

/// A splitmix64 random stream. Not serialized; always re-derived from a Seed.
#[derive(Clone, Debug)]
pub struct Stream {
    state: u64,
}

impl Stream {
    /// Next raw 64-bit value from the stream.
    /// type-audit: bare-ok(constructor-edge)
    pub fn next_u64(&mut self) -> u64 {
        splitmix64(&mut self.state)
    }

    /// Uniform in [0, 1), using the top 53 bits.
    /// type-audit: bare-ok(ratio)
    pub fn next_f64(&mut self) -> f64 {
        (self.next_u64() >> 11) as f64 / (1u64 << 53) as f64
    }

    /// Uniform in [lo, hi], inclusive. Panics if lo > hi.
    /// type-audit: bare-ok(count)
    pub fn range_u32(&mut self, lo: u32, hi: u32) -> u32 {
        assert!(lo <= hi, "range_u32: lo > hi");
        let span = u64::from(hi - lo) + 1;
        lo + (self.next_u64() % span) as u32
    }

    /// Deterministically pick one element; None if empty.
    pub fn pick<'a, T>(&mut self, items: &'a [T]) -> Option<&'a T> {
        if items.is_empty() {
            return None;
        }
        let i = (self.next_u64() % items.len() as u64) as usize;
        Some(&items[i])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn derive_is_deterministic() {
        assert_eq!(Seed(42).derive("astronomy"), Seed(42).derive("astronomy"));
    }

    #[test]
    fn derive_differs_by_label() {
        assert_ne!(Seed(42).derive("astronomy"), Seed(42).derive("climate"));
    }

    #[test]
    fn derive_differs_by_parent() {
        assert_ne!(Seed(42).derive("astronomy"), Seed(43).derive("astronomy"));
    }

    #[test]
    fn derive_chains_compose() {
        let a = Seed(7).derive("settlement").derive("name");
        let b = Seed(7).derive("settlement").derive("name");
        assert_eq!(a, b);
    }

    #[test]
    fn stream_is_deterministic() {
        let mut a = Seed(9).stream();
        let mut b = Seed(9).stream();
        assert_eq!(
            [a.next_u64(), a.next_u64(), a.next_u64()],
            [b.next_u64(), b.next_u64(), b.next_u64()]
        );
    }

    #[test]
    fn next_f64_is_in_unit_interval() {
        let mut s = Seed(1).stream();
        for _ in 0..1000 {
            let x = s.next_f64();
            assert!((0.0..1.0).contains(&x));
        }
    }

    #[test]
    fn range_u32_is_inclusive_and_bounded() {
        let mut s = Seed(2).stream();
        let mut saw_lo = false;
        let mut saw_hi = false;
        for _ in 0..2000 {
            let x = s.range_u32(3, 5);
            assert!((3..=5).contains(&x));
            saw_lo |= x == 3;
            saw_hi |= x == 5;
        }
        assert!(saw_lo && saw_hi);
    }

    #[test]
    fn pick_returns_none_on_empty() {
        let mut s = Seed(3).stream();
        let empty: [u8; 0] = [];
        assert_eq!(s.pick(&empty), None);
    }

    #[test]
    fn pick_returns_an_element() {
        let mut s = Seed(3).stream();
        let items = ["a", "b", "c"];
        assert!(items.contains(s.pick(&items).unwrap()));
    }

    #[test]
    fn seed_serializes_roundtrip() {
        let s = Seed(42).derive("x");
        let json = serde_json::to_string(&s).unwrap();
        assert_eq!(serde_json::from_str::<Seed>(&json).unwrap(), s);
    }
}
