import { assert, assertEquals } from "@std/assert";
import { eventsOf, eventX } from "./timeline.ts";
import { life } from "./fixtures.ts";

Deno.test("the common life is born, matures undated, and ends", () => {
  const events = eventsOf(life());
  assertEquals(events.map((e) => e.kind), ["birth", "ending", "maturity"]);
  assertEquals(events[0].age, 0);
  assertEquals(events[0].label, "Born in year 1752.");
  assertEquals(events[1].label, "Died at 32.");
});

Deno.test("dated events ascend by age and undated ones come last", () => {
  const moved = life();
  moved.moved_to = 12;
  moved.moved_year = 1770;
  const events = eventsOf(moved);
  assertEquals(events.map((e) => e.kind), ["birth", "move", "ending", "maturity"]);
  const dated = events.filter((e) => e.age !== null).map((e) => e.age as number);
  for (let at = 1; at < dated.length; at += 1) {
    assert(dated[at] >= dated[at - 1], `ages fell at ${at}`);
  }
  assertEquals(events[1].label, "Moved on at 18.");
  // Every undated event sits after every dated one.
  const firstUndated = events.findIndex((e) => e.age === null);
  assert(events.slice(firstUndated).every((e) => e.age === null));
});

Deno.test("maturity is emitted only when the payload says it happened", () => {
  const child = life();
  child.matured = false;
  child.age_at_death = 2.5;
  const events = eventsOf(child);
  assertEquals(events.map((e) => e.kind), ["birth", "ending"]);
  assertEquals(events[1].label, "Died at 3, before maturity.");
});

Deno.test("a community's fate names its committed cause", () => {
  const burned = life();
  burned.ending = { kind: "community-fate", cause: "burned" };
  assertEquals(eventsOf(burned)[1].label, "Died at 32, when the community was burned.");
});

Deno.test("an outbreak ending names its pathogen", () => {
  const struck = life();
  struck.ending = { kind: "outbreak", cause: "the pest" };
  assertEquals(eventsOf(struck)[1].label, "Died at 32 in an outbreak of the pest.");
});

Deno.test("a life still running says so rather than dying", () => {
  const alive = life();
  alive.ending = { kind: "alive", cause: null };
  assertEquals(eventsOf(alive)[1].label, "Still alive at 32, when the record ends.");
});

Deno.test("eventX places the birth at 0, the ending at the far end, and undated nowhere", () => {
  const subject = life();
  const events = eventsOf(subject);
  assertEquals(eventX(events[0], subject, 600), 0);
  assertEquals(eventX(events[1], subject, 600), 600);
  assertEquals(eventX(events[2], subject, 600), null);
});

Deno.test("a life of zero length does not divide by zero", () => {
  const stillborn = life();
  stillborn.matured = false;
  stillborn.age_at_death = 0;
  const events = eventsOf(stillborn);
  assertEquals(eventX(events[1], stillborn, 600), 0);
});
