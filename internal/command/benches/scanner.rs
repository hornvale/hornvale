//! Benchmarking the scanner module.
#![allow(missing_docs)]

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use hornvale_command::prelude::*;

fn scanner(criterion: &mut Criterion) {
  let mut group = criterion.benchmark_group("Scanner");
  let input_data = vec![
    ("take sword",),
    ("kill troll with sword",),
    ("take the red cube, green cylinder, and yellow prism",),
    ("look at the red-eyed goblin's club",),
    ("read the print on the underside of the kettle on the stove",),
    ("remember the red-eyed, shining-haired goblin as franklin",),
    ("steal gold from the elf's moneybag",),
    ("sneak poisoned coin into the elf's moneybag",),
    ("give the elf a poisoned coin",),
    ("ask the elf about the red-eyed goblin",),
    ("tell the elf about the red-eyed goblin",),
    ("show the elf the red-eyed goblin's club",),
    ("tell the farmer \"I know who you are and I know what you've done.\"",),
    ("we also support numbers like 2421432910 and 230492 and 20920252.",),
    ("!@#$!@#$!$#@!$#@!$#@!$#@!$#@!$#@!$#@!$#@!$#@$#@!$@!$@!$#@!#@$!$@!$!",),
    ("a an the a an the a an the a an the a an the a an the a an the a an the",),
    ("1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th 12th 13th 14th 15th 16th 17th 18th 19th 20th",),
    ("north; east; west; south; north; east; west; south",),
    ("open trap. d. n. kill troll with sword. g. g. drop sword. s. e. w. n.",),
    ("enter closet then look at mrs crabapple then look at the baby",),
    ("use the red-eyed goblin's club to kill the elf",),
    ("use the red-eyed goblin's club to kill the elf with the poisoned coin",),
    ("use the red-eyed goblin's club to kill the elf with the poisoned coin in the kitchen",),
    ("use the red-eyed goblin's club to kill the elf with the poisoned coin in the kitchen at midnight",),
    ("use the red-eyed goblin's club to kill the elf with the poisoned coin in the kitchen at midnight on the full moon",),
    ("use the red-eyed goblin's club to kill the elf with the poisoned coin in the kitchen at midnight on the full moon in the year 2022",),
    ("use the red-eyed goblin's club to kill the elf with the poisoned coin in the kitchen at midnight on the full moon in the year 2022 in the elven kingdom of elvendom",),
    ("use the red-eyed goblin's club to kill the elf with the poisoned coin in the kitchen at midnight on the full moon in the year 2022 in the elven kingdom of elvendom in the elven city of elvenville",),
    ("use the red-eyed goblin's club to kill the elf with the poisoned coin in the kitchen at midnight on the full moon in the year 2022 in the elven kingdom of elvendom in the elven city of elvenville in the elven tavern of elveninn",),
    ("use the red-eyed goblin's club to kill the elf with the poisoned coin in the kitchen at midnight on the full moon in the year 2022 in the elven kingdom of elvendom in the elven city of elvenville in the elven tavern of elveninn with the elven barmaid while the elven bard sings a song of elven love",),
  ];
  for i in input_data.iter() {
    let input_name = if i.0.len() > 80 {
      format!("{}... ({})", i.0[0..80].to_string(), i.0.len())
    } else {
      format!("{} ({})", i.0, i.0.len())
    };
    group.bench_with_input(BenchmarkId::new("Instantiation and scanning", input_name), i, |b, i| {
      b.iter(|| {
        let mut scanner = Scanner::new(i.0);
        scanner.scan_tokens().unwrap();
        scanner.reset();
      })
    });
    group.bench_with_input(BenchmarkId::new("Scanning", i.0), i, |b, i| {
      let mut scanner = Scanner::new(i.0);
      b.iter(|| {
        scanner.scan_tokens().unwrap();
        scanner.reset();
      })
    });
  }
  group.finish();
}

criterion_group!(benches, scanner);
criterion_main!(benches);
