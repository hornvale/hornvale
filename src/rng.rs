//! Deterministic random number generation for seeded world generation.
//!
//! This module provides [`SeededRng`], a deterministic pseudo-random number generator
//! that produces the same sequence of values given the same seed. This is critical
//! for Hornvale's "same seed → same world" guarantee.
//!
//! # Algorithm
//!
//! Uses xoshiro256** (xoshiro256starstar), a fast, high-quality PRNG with:
//! - 256 bits of state
//! - Period of 2^256 - 1
//! - Excellent statistical properties
//! - Simple, auditable implementation
//!
//! # Example
//!
//! ```
//! use hornvale::rng::SeededRng;
//!
//! let mut rng = SeededRng::new(12345);
//! let value = rng.next_u64();
//! let in_range = rng.range(1, 100);
//! let choice = rng.choice(&["apple", "banana", "cherry"]);
//! ```

/// A deterministic pseudo-random number generator.
///
/// `SeededRng` wraps the xoshiro256** algorithm, providing a simple interface
/// for generating random values from a seed. The same seed always produces
/// the same sequence of values.
#[derive(Debug, Clone)]
pub struct SeededRng {
    state: [u64; 4],
}

impl SeededRng {
    /// Creates a new RNG from a seed.
    ///
    /// The seed is expanded into the full 256-bit state using SplitMix64,
    /// ensuring good distribution even from simple seeds like `0` or `1`.
    #[must_use]
    pub fn new(seed: u64) -> Self {
        // Use SplitMix64 to expand the seed into full state
        let mut sm = SplitMix64(seed);
        Self {
            state: [sm.next(), sm.next(), sm.next(), sm.next()],
        }
    }

    /// Returns the next random `u64`.
    pub fn next_u64(&mut self) -> u64 {
        let result = self.state[1].wrapping_mul(5).rotate_left(7).wrapping_mul(9);

        let t = self.state[1] << 17;

        self.state[2] ^= self.state[0];
        self.state[3] ^= self.state[1];
        self.state[1] ^= self.state[2];
        self.state[0] ^= self.state[3];

        self.state[2] ^= t;
        self.state[3] = self.state[3].rotate_left(45);

        result
    }

    /// Returns a random `f64` in the range [0.0, 1.0).
    pub fn next_f64(&mut self) -> f64 {
        // Use the upper 53 bits for the mantissa (IEEE 754 double precision)
        let bits = self.next_u64() >> 11;
        bits as f64 * (1.0 / (1u64 << 53) as f64)
    }

    /// Returns a random integer in the range [min, max] (inclusive).
    ///
    /// # Panics
    ///
    /// Panics if `min > max`.
    pub fn range(&mut self, min: i64, max: i64) -> i64 {
        assert!(min <= max, "range: min ({min}) must be <= max ({max})");

        if min == max {
            return min;
        }

        // Handle potential overflow: compute range as u64 carefully
        // (max - min + 1) could overflow when the range spans all of i64
        let min_u = (min as u64).wrapping_sub(i64::MIN as u64);
        let max_u = (max as u64).wrapping_sub(i64::MIN as u64);
        let diff = max_u - min_u;

        // Check if adding 1 would overflow (meaning we want the full u64 range)
        let range = match diff.checked_add(1) {
            Some(r) => r,
            None => {
                // Full u64 range: just return the raw u64 reinterpreted as i64
                return self.next_u64() as i64;
            }
        };

        let threshold = range.wrapping_neg() % range;

        // Rejection sampling to avoid modulo bias
        loop {
            let r = self.next_u64();
            if r >= threshold {
                return min.wrapping_add((r % range) as i64);
            }
        }
    }

    /// Returns a random `f64` in the range [min, max).
    ///
    /// # Panics
    ///
    /// Panics if `min >= max`.
    pub fn range_f64(&mut self, min: f64, max: f64) -> f64 {
        assert!(min < max, "range_f64: min ({min}) must be < max ({max})");
        min + self.next_f64() * (max - min)
    }

    /// Returns a random element from a slice.
    ///
    /// # Panics
    ///
    /// Panics if the slice is empty.
    pub fn choice<'a, T>(&mut self, items: &'a [T]) -> &'a T {
        assert!(!items.is_empty(), "choice: slice must not be empty");
        let index = self.range(0, items.len() as i64 - 1) as usize;
        &items[index]
    }

    /// Shuffles a slice in place using Fisher-Yates algorithm.
    pub fn shuffle<T>(&mut self, items: &mut [T]) {
        let len = items.len();
        if len <= 1 {
            return;
        }

        for i in (1..len).rev() {
            let j = self.range(0, i as i64) as usize;
            items.swap(i, j);
        }
    }

    /// Returns a random boolean with the given probability of being `true`.
    ///
    /// # Panics
    ///
    /// Panics if `probability` is not in the range [0.0, 1.0].
    pub fn chance(&mut self, probability: f64) -> bool {
        assert!(
            (0.0..=1.0).contains(&probability),
            "chance: probability ({probability}) must be in [0.0, 1.0]"
        );
        self.next_f64() < probability
    }

    /// Creates a new RNG derived from this one.
    ///
    /// The child RNG has a different sequence but is deterministically
    /// derived from the parent's current state. This is useful for:
    /// - Generating independent sub-sequences
    /// - Creating child contexts in generation hierarchies
    ///
    /// After calling `fork()`, both the parent and child can be used
    /// independently without affecting each other's sequences.
    #[must_use]
    pub fn fork(&mut self) -> Self {
        // Use current state to generate a seed for the child
        let child_seed = self.next_u64();
        Self::new(child_seed)
    }

    /// Creates a new RNG derived from this one with an additional key.
    ///
    /// This is useful for generating deterministic sub-sequences based on
    /// a context identifier (like an entity ID or location hash).
    ///
    /// ```
    /// use hornvale::rng::SeededRng;
    ///
    /// let mut rng = SeededRng::new(12345);
    /// let mut location_rng = rng.fork_with_key(42); // e.g., location ID
    /// ```
    #[must_use]
    pub fn fork_with_key(&mut self, key: u64) -> Self {
        // Mix the key with current state
        let base_seed = self.next_u64();
        let mixed_seed = base_seed ^ key.wrapping_mul(0x9e3779b97f4a7c15);
        Self::new(mixed_seed)
    }

    /// Returns the current internal state (for serialization/debugging).
    #[must_use]
    pub fn state(&self) -> [u64; 4] {
        self.state
    }

    /// Creates an RNG from a specific state (for deserialization).
    ///
    /// # Panics
    ///
    /// Panics if the state is all zeros (invalid for xoshiro).
    #[must_use]
    pub fn from_state(state: [u64; 4]) -> Self {
        assert!(
            state.iter().any(|&x| x != 0),
            "from_state: state must not be all zeros"
        );
        Self { state }
    }
}

/// SplitMix64 generator for seed expansion.
///
/// Used internally to expand a single u64 seed into the full 256-bit state.
struct SplitMix64(u64);

impl SplitMix64 {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9e3779b97f4a7c15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
        z ^ (z >> 31)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_determinism() {
        // Same seed produces same sequence
        let mut rng1 = SeededRng::new(12345);
        let mut rng2 = SeededRng::new(12345);

        for _ in 0..100 {
            assert_eq!(rng1.next_u64(), rng2.next_u64());
        }
    }

    #[test]
    fn test_different_seeds_different_sequences() {
        let mut rng1 = SeededRng::new(12345);
        let mut rng2 = SeededRng::new(54321);

        // Very unlikely to match even once in 100 iterations
        let mut any_different = false;
        for _ in 0..100 {
            if rng1.next_u64() != rng2.next_u64() {
                any_different = true;
                break;
            }
        }
        assert!(any_different);
    }

    #[test]
    fn test_next_f64_range() {
        let mut rng = SeededRng::new(42);

        for _ in 0..1000 {
            let f = rng.next_f64();
            assert!((0.0..1.0).contains(&f), "f64 out of range: {f}");
        }
    }

    #[test]
    fn test_range_inclusive() {
        let mut rng = SeededRng::new(42);

        for _ in 0..1000 {
            let v = rng.range(5, 10);
            assert!((5..=10).contains(&v), "range out of bounds: {v}");
        }
    }

    #[test]
    fn test_range_single_value() {
        let mut rng = SeededRng::new(42);
        assert_eq!(rng.range(7, 7), 7);
    }

    #[test]
    #[should_panic(expected = "min")]
    fn test_range_invalid() {
        let mut rng = SeededRng::new(42);
        rng.range(10, 5);
    }

    #[test]
    fn test_range_f64() {
        let mut rng = SeededRng::new(42);

        for _ in 0..1000 {
            let f = rng.range_f64(1.0, 5.0);
            assert!((1.0..5.0).contains(&f), "range_f64 out of bounds: {f}");
        }
    }

    #[test]
    fn test_choice() {
        let items = ["apple", "banana", "cherry"];
        let mut rng = SeededRng::new(42);

        for _ in 0..100 {
            let c = rng.choice(&items);
            assert!(items.contains(c));
        }
    }

    #[test]
    fn test_choice_determinism() {
        let items = [1, 2, 3, 4, 5];
        let mut rng1 = SeededRng::new(999);
        let mut rng2 = SeededRng::new(999);

        for _ in 0..50 {
            assert_eq!(rng1.choice(&items), rng2.choice(&items));
        }
    }

    #[test]
    #[should_panic(expected = "empty")]
    fn test_choice_empty() {
        let items: [i32; 0] = [];
        let mut rng = SeededRng::new(42);
        rng.choice(&items);
    }

    #[test]
    fn test_shuffle() {
        let mut items = [1, 2, 3, 4, 5];
        let original = items;
        let mut rng = SeededRng::new(42);

        rng.shuffle(&mut items);

        // Should still contain same elements
        let mut sorted = items;
        sorted.sort();
        assert_eq!(sorted, [1, 2, 3, 4, 5]);

        // Very unlikely to be in same order (1/120 chance)
        // Run multiple times to virtually guarantee difference
        let mut ever_different = items != original;
        for _ in 0..10 {
            items = [1, 2, 3, 4, 5];
            rng.shuffle(&mut items);
            if items != original {
                ever_different = true;
            }
        }
        assert!(ever_different);
    }

    #[test]
    fn test_shuffle_determinism() {
        let mut items1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let mut items2 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

        let mut rng1 = SeededRng::new(777);
        let mut rng2 = SeededRng::new(777);

        rng1.shuffle(&mut items1);
        rng2.shuffle(&mut items2);

        assert_eq!(items1, items2);
    }

    #[test]
    fn test_shuffle_empty_and_single() {
        let mut empty: [i32; 0] = [];
        let mut single = [42];
        let mut rng = SeededRng::new(42);

        rng.shuffle(&mut empty);
        rng.shuffle(&mut single);
        assert_eq!(single, [42]);
    }

    #[test]
    fn test_chance() {
        let mut rng = SeededRng::new(42);

        // Test extremes
        assert!(!rng.chance(0.0));
        assert!(rng.chance(1.0));

        // Test that 50% gives roughly half true
        let mut true_count = 0;
        for _ in 0..1000 {
            if rng.chance(0.5) {
                true_count += 1;
            }
        }
        // Should be around 500, allow wide margin
        assert!(
            (400..600).contains(&true_count),
            "Expected ~500 true, got {true_count}"
        );
    }

    #[test]
    fn test_fork() {
        let mut parent = SeededRng::new(12345);

        // Advance parent a bit
        for _ in 0..10 {
            parent.next_u64();
        }

        let child1 = parent.fork();
        let child2 = parent.fork();

        // Children should have different sequences from each other
        let mut c1 = child1.clone();
        let mut c2 = child2.clone();
        assert_ne!(c1.next_u64(), c2.next_u64());
    }

    #[test]
    fn test_fork_determinism() {
        let mut parent1 = SeededRng::new(12345);
        let mut parent2 = SeededRng::new(12345);

        // Same operations produce same children
        for _ in 0..5 {
            parent1.next_u64();
            parent2.next_u64();
        }

        let mut child1 = parent1.fork();
        let mut child2 = parent2.fork();

        for _ in 0..50 {
            assert_eq!(child1.next_u64(), child2.next_u64());
        }
    }

    #[test]
    fn test_fork_with_key() {
        let mut rng = SeededRng::new(12345);

        let mut fork_a = rng.fork_with_key(100);
        let mut fork_b = rng.fork_with_key(200);

        // Different keys produce different sequences
        assert_ne!(fork_a.next_u64(), fork_b.next_u64());
    }

    #[test]
    fn test_fork_with_key_determinism() {
        let mut rng1 = SeededRng::new(12345);
        let mut rng2 = SeededRng::new(12345);

        let mut fork1 = rng1.fork_with_key(42);
        let mut fork2 = rng2.fork_with_key(42);

        for _ in 0..50 {
            assert_eq!(fork1.next_u64(), fork2.next_u64());
        }
    }

    #[test]
    fn test_state_roundtrip() {
        let mut rng = SeededRng::new(12345);

        // Advance a bit
        for _ in 0..100 {
            rng.next_u64();
        }

        let state = rng.state();
        let mut restored = SeededRng::from_state(state);

        // Should produce same sequence from here
        for _ in 0..50 {
            assert_eq!(rng.next_u64(), restored.next_u64());
        }
    }

    #[test]
    #[should_panic(expected = "all zeros")]
    fn test_from_state_zero_invalid() {
        let _ = SeededRng::from_state([0, 0, 0, 0]);
    }

    #[test]
    fn test_distribution_uniformity() {
        // Basic chi-squared test for uniformity
        let mut rng = SeededRng::new(42);
        let mut buckets = [0u32; 10];

        for _ in 0..10000 {
            let v = rng.range(0, 9) as usize;
            buckets[v] += 1;
        }

        // Each bucket should have ~1000 items
        // Allow 200 deviation (very loose, but catches obvious bias)
        for (i, &count) in buckets.iter().enumerate() {
            assert!(
                (800..1200).contains(&count),
                "Bucket {i} has {count} items (expected ~1000)"
            );
        }
    }

    #[test]
    fn test_seed_zero_works() {
        // Seed 0 should still produce valid output
        let mut rng = SeededRng::new(0);
        let v1 = rng.next_u64();
        let v2 = rng.next_u64();
        assert_ne!(v1, v2);
        assert_ne!(v1, 0); // Very unlikely to be 0
    }

    #[test]
    fn test_large_range() {
        let mut rng = SeededRng::new(42);

        // Test full i64 range works
        let v = rng.range(i64::MIN, i64::MAX);
        // Just verify it doesn't panic and returns something
        let _ = v;

        // Test negative ranges
        for _ in 0..100 {
            let v = rng.range(-100, -50);
            assert!((-100..=-50).contains(&v));
        }
    }
}
