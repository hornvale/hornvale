//! Hierarchical seeding and deterministic random streams — the root of the
//! byte-identical guarantee (decision 0001).

use serde::{Deserialize, Serialize};

/// A deterministic seed. The world seed is the root; everything else is
/// derived from it by labeled paths, so adding a new consumer never
/// perturbs existing streams.
/// type-audit: bare-ok(constructor-edge)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Seed(pub u64);

/// A seed-derivation leg — the only way to call [`Seed::derive`].
/// Wraps a string so a bare `&str`, and therefore a typo'd inline
/// literal, cannot silently compile where a leg was expected. Zero-cost:
/// borrows, never allocates.
/// type-audit: bare-ok(identifier-text: return)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StreamLabel<'a>(&'a str);

impl StreamLabel<'static> {
    /// A permanent, save-format-contract leg. Called ONLY inside a
    /// crate's own `streams.rs` — never at an ad hoc use site (a
    /// `tools/type-audit` check enforces this — see PROC-17's own plan,
    /// Task 11).
    /// type-audit: bare-ok(identifier-text: s)
    pub const fn from_static(s: &'static str) -> Self {
        StreamLabel(s)
    }
}

impl<'a> StreamLabel<'a> {
    /// A runtime-computed leg — a species name, a settlement's cell id, a
    /// salt. Legitimately dynamic, never centralizable.
    /// type-audit: bare-ok(identifier-text: s)
    pub fn dynamic(s: &'a str) -> Self {
        StreamLabel(s)
    }

    /// The wrapped string. `pub` because `stream_labels()` in every OTHER
    /// crate needs it to build the `Vec<(&'static str, &'static str)>`
    /// the manifest reads — that return type does not change; only how
    /// each crate builds it does.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn as_str(&self) -> &'a str {
        self.0
    }
}

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
    /// A fresh deterministic random stream rooted at this seed.
    pub fn stream(&self) -> Stream {
        Stream { state: self.0 }
    }

    /// Derive a child seed for a typed `label`. FNV-1a over the label's
    /// bytes, mixed with the parent, then scrambled. Stable forever:
    /// changing this breaks every saved world.
    pub fn derive(&self, label: StreamLabel<'_>) -> Seed {
        let bytes = label.as_str();
        let mut h = FNV_OFFSET ^ self.0;
        for byte in bytes.as_bytes() {
            h ^= u64::from(*byte);
            h = h.wrapping_mul(FNV_PRIME);
        }
        Seed(splitmix64(&mut h))
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

    /// Deterministically pick an index with probability proportional to its
    /// weight. `None` if the slice is empty or all weights are ≤ 0. Weights
    /// are consumed in slice order; the threshold is `next_f64() * total`,
    /// and the first cumulative bucket strictly exceeding it wins. This
    /// draw-semantics is a frozen save-format contract.
    /// type-audit: bare-ok(index)
    pub fn weighted_index(&mut self, weights: &[f64]) -> Option<usize> {
        let total: f64 = weights.iter().filter(|w| **w > 0.0).sum();
        if total <= 0.0 {
            return None;
        }
        let threshold = self.next_f64() * total;
        let mut cumulative = 0.0;
        for (i, &w) in weights.iter().enumerate() {
            if w <= 0.0 {
                continue;
            }
            cumulative += w;
            if cumulative > threshold {
                return Some(i);
            }
        }
        // Floating-point tail: return the last positive-weight index.
        weights.iter().rposition(|&w| w > 0.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn derive_is_deterministic() {
        assert_eq!(
            Seed(42).derive(StreamLabel::dynamic("astronomy")),
            Seed(42).derive(StreamLabel::dynamic("astronomy"))
        );
    }

    #[test]
    fn derive_differs_by_label() {
        assert_ne!(
            Seed(42).derive(StreamLabel::dynamic("astronomy")),
            Seed(42).derive(StreamLabel::dynamic("climate"))
        );
    }

    #[test]
    fn derive_differs_by_parent() {
        assert_ne!(
            Seed(42).derive(StreamLabel::dynamic("astronomy")),
            Seed(43).derive(StreamLabel::dynamic("astronomy"))
        );
    }

    #[test]
    fn derive_chains_compose() {
        let a = Seed(7)
            .derive(StreamLabel::dynamic("settlement"))
            .derive(StreamLabel::dynamic("name"));
        let b = Seed(7)
            .derive(StreamLabel::dynamic("settlement"))
            .derive(StreamLabel::dynamic("name"));
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
    fn weighted_index_is_deterministic() {
        let w = [1.0, 2.0, 3.0];
        let a = Seed(7).stream().weighted_index(&w);
        let b = Seed(7).stream().weighted_index(&w);
        assert_eq!(a, b);
        assert!(a.is_some());
    }

    #[test]
    fn weighted_index_empty_and_zero_are_none() {
        assert_eq!(Seed(1).stream().weighted_index(&[]), None);
        assert_eq!(Seed(1).stream().weighted_index(&[0.0, 0.0]), None);
    }

    #[test]
    fn weighted_index_respects_weights() {
        // With weights [0, 1], index 0 (weight 0) can never be chosen.
        for s in 0..64 {
            assert_eq!(Seed(s).stream().weighted_index(&[0.0, 1.0]), Some(1));
        }
    }

    #[test]
    fn weighted_index_uniform_matches_pick_distribution() {
        // Equal weights → all indices reachable across seeds.
        let seen: std::collections::BTreeSet<usize> = (0..200)
            .filter_map(|s| Seed(s).stream().weighted_index(&[1.0, 1.0, 1.0, 1.0]))
            .collect();
        assert_eq!(seen, [0, 1, 2, 3].into_iter().collect());
    }

    #[test]
    fn seed_serializes_roundtrip() {
        let s = Seed(42).derive(StreamLabel::dynamic("x"));
        let json = serde_json::to_string(&s).unwrap();
        assert_eq!(serde_json::from_str::<Seed>(&json).unwrap(), s);
    }

    #[test]
    fn derive_is_deterministic_for_a_typed_label() {
        let seed = Seed(42);
        assert_eq!(
            seed.derive(StreamLabel::from_static("astronomy")),
            seed.derive(StreamLabel::from_static("astronomy"))
        );
    }

    #[test]
    fn derive_differs_by_typed_label() {
        let seed = Seed(42);
        assert_ne!(
            seed.derive(StreamLabel::from_static("astronomy")),
            seed.derive(StreamLabel::from_static("climate"))
        );
    }

    #[test]
    fn derive_dynamic_and_static_agree_on_the_same_bytes() {
        let seed = Seed(7);
        let owned = String::from("goblin");
        assert_eq!(
            seed.derive(StreamLabel::dynamic(&owned)),
            seed.derive(StreamLabel::dynamic("goblin"))
        );
    }

    #[test]
    fn stream_label_as_str_round_trips() {
        let label = StreamLabel::from_static("terrain");
        assert_eq!(label.as_str(), "terrain");
    }
}
