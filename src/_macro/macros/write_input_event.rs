#[macro_export]
macro_rules! write_input_event {
  ($data: expr, $string: expr) => {{
    #[allow(unused_imports)]
    use $crate::ecs::event::InputEvent;
    get_input_event_channel!($data).single_write(InputEvent { input: $string.into() });
  }};
}
