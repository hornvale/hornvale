#[macro_export]
macro_rules! write_effect_event {
  ($data: expr, $effect: expr) => {{
    #[allow(unused_imports)]
    use $crate::ecs::event::EffectEvent;
    get_effect_event_channel!($data).single_write(EffectEvent { effect: $effect });
  }};
}
