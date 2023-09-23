#[macro_export]
macro_rules! get_effect_event_channel {
  ($data: expr) => {{
    &mut $data.effect_event_channel
  }};
}
