use crate::system_data::AllData;

/// The `EffectContextData` struct, which represents the data in the context
/// of an effect application.
pub trait EffectContextData {}

impl<'data> EffectContextData for AllData<'data> {}
