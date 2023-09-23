#[macro_export]
macro_rules! write_effect_event {
  ($data: expr, $effect: expr) => {{
    #[allow(unused_imports)]
    use $crate::ecs::event::EffectEvent;
    $data.write_effect_event($effect);
  }};
}
