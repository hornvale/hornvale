#[macro_export]
macro_rules! write_command_event {
  ($data: expr, $command: expr) => {{
    #[allow(unused_imports)]
    use $crate::ecs::event::CommandEvent;
    #[allow(unused_imports)]
    use $crate::ecs::WriteCommandEventTrait;
    $data.write_command_event($command);
  }};
}
