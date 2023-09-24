#[macro_export]
macro_rules! write_effect_event {
  ($data: expr, $effect: expr) => {{
    #[allow(unused_imports)]
    use $crate::event::EffectEvent;
    #[allow(unused_imports)]
    use $crate::system_data::WriteEffectEventTrait;
    $data.write_effect_event($effect);
  }};
}
