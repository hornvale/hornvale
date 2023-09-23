#[macro_export]
macro_rules! write_input_event {
  ($data: expr, $string: expr) => {{
    #[allow(unused_imports)]
    use $crate::ecs::event::InputEvent;
    #[allow(unused_imports)]
    use $crate::ecs::WriteInputEventTrait;
    $data.write_input_event($string.to_string());
  }};
}
