#[macro_export]
macro_rules! write_command_event {
  ($data: expr, $command: expr) => {{
    #[allow(unused_imports)]
    use $crate::ecs::event::CommandEvent;
    get_command_event_channel!($data).single_write(CommandEvent {
      command: $command.into(),
    });
  }};
}
