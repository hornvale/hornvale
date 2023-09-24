#[macro_export]
macro_rules! write_output_event {
  ($data: expr, $string: expr) => {{
    #[allow(unused_imports)]
    use $crate::event::OutputEvent;
    #[allow(unused_imports)]
    use $crate::system_data::WriteEventTrait;
    $data.write_event(OutputEvent {
      output: $string.to_string(),
    });
  }};
}
