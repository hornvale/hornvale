/// A trait for methods that determine if something is habitable.
pub mod maybe_habitable;
/// A trait for methods that count the stars.
pub mod stellar_countable;
/// A trait for methods that sum the masses of the stars.
pub mod stellar_massable;

/// The prelude.
pub mod prelude {
  pub use super::maybe_habitable::MaybeHabitable;
  pub use super::stellar_countable::StellarCountable;
  pub use super::stellar_massable::StellarMassable;
}
