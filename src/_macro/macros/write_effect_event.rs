#[macro_export]
macro_rules! write_effect_event {
  ($data: expr, $effect: expr) => {{
    use std::sync::Arc;
    #[allow(unused_imports)]
    use $crate::ecs::event::EffectEvent;
    #[allow(unused_imports)]
    use $crate::ecs::WriteEffectEventTrait;
    $data.write_effect_event(Arc::new($effect));
  }};
}
