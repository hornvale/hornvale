#[macro_export]
macro_rules! get_input_event_channel {
  ($data: expr) => {{
    &mut $data.input_event_channel
  }};
}
