#[macro_export]
macro_rules! write_action_event {
  ($data: expr, $action: expr) => {{
    #[allow(unused_imports)]
    use $crate::event::ActionEvent;
    #[allow(unused_imports)]
    use $crate::system_data::WriteEventTrait;
    $data.write_event(ActionEvent { action: $action });
  }};
}
