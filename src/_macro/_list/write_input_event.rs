#[macro_export]
macro_rules! write_input_event {
  ($data: expr, $string: expr) => {{
    #[allow(unused_imports)]
    use $crate::event::InputEvent;
    #[allow(unused_imports)]
    use $crate::system::WriteEventTrait;
    $data.write_event(InputEvent::new($string.to_string()));
  }};
}
