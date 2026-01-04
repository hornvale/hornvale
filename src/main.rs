//! Hornvale main application.

use hornvale::rules::RuleSet;
use hornvale::{StdIO, World};

fn main() {
    let mut world = World::new();
    let mut rules = RuleSet::new();
    let mut io = StdIO::new();

    hornvale::repl::run_repl(&mut world, &mut rules, &mut io);
}
