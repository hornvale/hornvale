#[macro_export]
macro_rules! get_output_event_channel {
  ($data: expr) => {{
    &mut $data.output_event_channel
  }};
}
