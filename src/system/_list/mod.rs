// This file is generated (see build.rs). Please do not edit manually.
pub mod action_processor;
pub use action_processor::ActionProcessor as ActionProcessorSystem;
pub mod command_processor;
pub use command_processor::CommandProcessor as CommandProcessorSystem;
pub mod effect_processor;
pub use effect_processor::EffectProcessor as EffectProcessorSystem;
pub mod input_processor;
pub use input_processor::InputProcessor as InputProcessorSystem;
pub mod output_processor;
pub use output_processor::OutputProcessor as OutputProcessorSystem;
