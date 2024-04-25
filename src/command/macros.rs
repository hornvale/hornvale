/// Macro to define a command variant with minimal repetition.
#[macro_export]
macro_rules! define_command_variant {
  ($variant:ident, $original:ty, $dom:expr, $iom:expr) => {
    pub struct $variant;

    impl hornvale_command::prelude::Command for $variant {
      const NAME: &'static str = <$original as Command>::NAME;
      const SYNONYMS: &'static [&'static str] = <$original as Command>::SYNONYMS;
      const BRIEF: &'static str = <$original as Command>::BRIEF;
      const DESCRIPTION: &'static str = <$original as Command>::DESCRIPTION;
      const ARITY: CommandArity = <$original as Command>::ARITY;
      const DIRECT_OBJECT_MODIFIER: Option<CommandModifier> = $dom;
      const INDIRECT_OBJECT_MODIFIER: Option<CommandModifier> = $iom;

      fn execute(
        world: &mut World,
        actor: Entity,
        direct_object: Option<Entity>,
        indirect_object: Option<Entity>,
      ) -> Result<(), AnyError> {
        <$original as Command>::execute(world, actor, direct_object, indirect_object)
      }
    }
  };
}
