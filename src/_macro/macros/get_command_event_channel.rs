#[macro_export]
macro_rules! get_command_event_channel {
  ($data: expr) => {{
    &mut $data.command_event_channel
  }};
}
