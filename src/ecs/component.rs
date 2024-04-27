use std::any::Any;

/// A component trait.
///
/// This is taken more-or-less verbatim from Catherine West's delightful talk
/// at RustConf 2018 and her article here:
/// @see https://kyren.github.io/2018/09/14/rustconf-talk.html
pub trait Component: Any + 'static {}
impl<T: Any + 'static> Component for T {}
