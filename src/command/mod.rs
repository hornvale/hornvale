/// The `Command` struct.
///
/// This should provide (in a safe, serializable form), the specific command
/// and its parameters.
///
/// A command translates into either an in-character (IC) `Action` or an out-
/// of-character (OOC)... IDK what to call it.  These might be disparate enough
/// that there's not much point in trying to create a term that describes all
/// of them.
///
/// These objects are passed in `CommandEvent`s, through `EventChannel`s, from
/// the `ProcessInputSystem` to the `ProcessCommandSystem`, where they will be
/// converted into an `Action` or the appropriate code executed directly.
///
/// Commands have zero duration; they should be requested and transformed into
/// an action (if appropriate) in the same tick.  But it might be worth taking
/// control away from the player until a noninterruptible action completes, or
/// something like that.  E.g. moving from room to room may take a long time
/// in the wilderness, and we can simulate that by delaying the return of the
/// input prompt.
#[derive(Clone, Debug)]
pub struct Command;
