use rand_seeder::SipRng;

/// The `Random` resource.
///
/// This makes a random number generator available to processes that need one.
///
/// This should only be used for processes that need a random number generator
/// that is not tied to a specific system. For example, if a system needs a
/// random number generator, it should create its own, using the seed string
/// from the `SeedString` resource. This is to ensure that the behavior of the
/// system is consistent, regardless of the order in which systems are run.
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct Random(pub SipRng);
