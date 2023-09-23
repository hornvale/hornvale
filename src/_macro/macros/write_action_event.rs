#[macro_export]
macro_rules! write_action_event {
  ($data: expr, $action: expr) => {{
    #[allow(unused_imports)]
    use $crate::ecs::event::ActionEvent;
    #[allow(unused_imports)]
    use $crate::ecs::WriteActionEventTrait;
    $data.write_action_event($action);
  }};
}
