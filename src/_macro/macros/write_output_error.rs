#[macro_export]
macro_rules! write_output_error {
  ($data: expr, $error: expr) => {{
    #[allow(unused_imports)]
    use $crate::ecs::event::OutputEvent;
    get_output_event_channel!($data).single_write(OutputEvent {
      string: format!("{}", $error),
    });
  }};
}
