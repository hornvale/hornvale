#[macro_export]
macro_rules! clone_output {
  ($data: expr) => {{
    #[allow(unused_imports)]
    use std::io::Write as _;
    let output_resource = $data.output_resource.0.as_ref().unwrap();
    output_resource.clone()
  }};
}

#[macro_export]
macro_rules! get_output_event_channel {
  ($data: expr) => {{
    &mut $data.output_event_channel
  }};
}

#[macro_export]
macro_rules! write_output_event {
  ($data: expr, $string: expr) => {{
    #[allow(unused_imports)]
    use $crate::ecs::event::OutputEvent;
    get_output_event_channel!($data).single_write(OutputEvent { string: $string.into() });
  }};
}

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
