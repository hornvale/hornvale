#[macro_export]
macro_rules! write_effect_event {
  ($data: expr, $effect: expr) => {{
    #[allow(unused_imports)]
    use $crate::ecs::WriteEffectEventTrait;
    #[allow(unused_imports)]
    use $crate::event::EffectEvent;
    $data.write_effect_event($effect);
  }};
}
