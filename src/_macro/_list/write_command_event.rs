#[macro_export]
macro_rules! write_command_event {
  ($data: expr, $command: expr) => {{
    #[allow(unused_imports)]
    use $crate::event::CommandEvent;
    #[allow(unused_imports)]
    use $crate::system_data::WriteEventTrait;
    $data.write_event(CommandEvent { command: $command });
  }};
}
