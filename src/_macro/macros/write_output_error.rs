#[macro_export]
macro_rules! write_output_error {
  ($data: expr, $error: expr) => {{
    #[allow(unused_imports)]
    use $crate::ecs::event::OutputEvent;
    #[allow(unused_imports)]
    use $crate::ecs::WriteOutputEventTrait;
    $data.write_output_event(format!("<red>{}<reset>", $error).to_string());
  }};
}
