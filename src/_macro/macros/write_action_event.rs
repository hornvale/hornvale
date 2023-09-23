#[macro_export]
macro_rules! write_action_event {
  ($data: expr, $action: expr) => {{
    use std::sync::Arc;
    #[allow(unused_imports)]
    use $crate::ecs::event::ActionEvent;
    #[allow(unused_imports)]
    use $crate::ecs::WriteActionEventTrait;
    $data.write_action_event(Arc::new($action));
  }};
}
