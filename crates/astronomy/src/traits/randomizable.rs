use rand::prelude::*;

/// The `Randomizable` trait, allowing for randomization.
pub trait Randomizable {
  /// Get a random instance of the implementing type.
  fn get_random<R: Rng + ?Sized>(rng: &mut R) -> Self;
  /// Get a random habitable instance of the implementing type.
  fn get_random_habitable<R: Rng + ?Sized>(rng: &mut R) -> Self;
  /// Get a random exotic instance of the implementing type.
  fn get_random_exotic<R: Rng + ?Sized>(rng: &mut R) -> Self;
  /// Get a random exotic and habitable instance of the implementing type.
  fn get_random_exotic_habitable<R: Rng + ?Sized>(rng: &mut R) -> Self;
  /// Get a random earthlike (and habitable) instance of the implementing type.
  fn get_random_earthlike<R: Rng + ?Sized>(rng: &mut R) -> Self;
}
