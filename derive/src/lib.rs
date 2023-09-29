extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(StackEvent)]
pub fn derive_stack_event(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);
  let name = &input.ident;
  let expanded = quote! {
    use crate::event::StackEventTrait;
    use crate::event::EventStackEntry;
    impl StackEventTrait for #name {
      /// Get the stacktrace for this event.
      fn get_stacktrace(&self) -> Vec<String> {
        self.stack.iter().rev().map(|e| e.to_string()).collect()
      }
      /// Get the stack for this event.
      fn get_stack(&self) -> &crate::event::EventStack {
        &self.stack
      }
      /// Set the stack for this event.
      fn set_stack(&mut self, stack: crate::event::EventStack) {
        self.stack = stack;
      }
      /// Add an entry to the stack for this event.
      fn add_stack_entry(&mut self, entry: EventStackEntry) {
        let mut stack = self.get_stack().clone();
        stack.push(entry);
        self.set_stack(stack);
      }
      /// Remove an entry from the stack for this event.
      fn remove_stack_entry(&mut self, entry: EventStackEntry) {
        let mut stack = self.get_stack().clone();
        stack.retain(|e| *e != entry);
        self.set_stack(stack);
      }
    }
  };
  TokenStream::from(expanded)
}

#[proc_macro_derive(TaggedEvent)]
pub fn derive_tagged_event(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);
  let name = &input.ident;
  let expanded = quote! {
    use crate::event::EventTag;
    use crate::event::TaggedEventTrait;
    impl TaggedEventTrait for #name {
      /// Get tags for this event.
      fn get_tags(&self) -> Vec<EventTag> {
        self.tags.clone()
      }
      /// Set tags for this event.
      fn set_tags(&mut self, tags: Vec<EventTag>) {
        self.tags = tags;
      }
      /// Add a tag to this event.
      fn add_tag(&mut self, tag: EventTag) {
        let mut tags = self.get_tags();
        tags.push(tag);
        self.set_tags(tags);
      }
      /// Remove a tag from this event.
      fn remove_tag(&mut self, tag: EventTag) {
        let mut tags = self.get_tags();
        tags.retain(|t| *t != tag);
        self.set_tags(tags);
      }
    }
  };
  TokenStream::from(expanded)
}

#[proc_macro_derive(UuidEvent)]
pub fn derive_uuid_event(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);
  let name = &input.ident;
  let expanded = quote! {
    use crate::event::UuidEventTrait;
    impl UuidEventTrait for #name {
      /// Get the UUID for this event.
      fn get_uuid(&self) -> uuid::Uuid {
        self.uuid
      }
    }
  };
  TokenStream::from(expanded)
}
