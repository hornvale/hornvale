#[macro_export]
macro_rules! write_effect_event {
  ($data: expr, $effect: expr) => {{
    #[allow(unused_imports)]
    use $crate::event::EffectEvent;
    #[allow(unused_imports)]
    use $crate::system_data::WriteEventTrait;
    $data.write_event(EffectEvent { effect: $effect });
  }};
}
