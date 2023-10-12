#[macro_export]
macro_rules! impl_base_uuid_wrapper {
  ($type:ty) => {
    impl $crate::entity_uuid::BaseUuidWrapperTrait for $type {
      fn new(uuid: String) -> Self {
        Self($crate::entity_uuid::BaseUuid::new(uuid))
      }
    }
  };
}
