#[macro_export]
macro_rules! write_command_event {
  ($data: expr, $command: expr) => {{
    #[allow(unused_imports)]
    use $crate::ecs::WriteCommandEventTrait;
    #[allow(unused_imports)]
    use $crate::event::CommandEvent;
    $data.write_command_event($command);
  }};
}
