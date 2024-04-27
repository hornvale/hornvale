use std::{any::Any, cell::RefCell, rc::Rc};

/// The actual component type.
pub type Component = Rc<RefCell<dyn Any>>;
