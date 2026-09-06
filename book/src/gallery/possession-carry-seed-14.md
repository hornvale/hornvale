# A Possession of Seed 14 — a thing carried

*(This transcript is frozen. It is the only gallery page that types the
custody verbs — `take`, `drop`, `put`, `open`, `close`, `carrying` — and it
is the campaign's thesis end to end: a key is picked up beside a loom two
chambers into a village dwelling, carried one room further, refused by a shut
lid, and then used to open that chest. Two refusals are the evidence, not
the noise. `take a key` in the very first room answers "You see no a key
here." — that room composes none, since The Custodian moved the key pattern
off `Role::Threshold`, the one role every built structure has — which is
what makes the closing beat, where a key set down in that same room is
picked up again, a measurement rather than a reply. And `take a key` at the
shut chest answers "The key is shut away in something closed.": the lid
means something, and until The Chattel's fix round it did not.*

*A key on a floor is a stand-in for a PERSON, and reading it as a difficulty
setting is the mistake this page invites. The household that lives here
would hold its own key or stash it somewhere only a resident knows; the
custody mechanism for exactly that already exists and is body-agnostic. What
is missing is the resident, so which rooms furnish a key is a
prop-management knob for as long as nobody is home to carry one
(`PLAY-key-placement-stands-in-for-a-resident`).*

*Read the room descriptions as the GRAMMAR's catalogue and not as an
inventory, because that is what they are — and this dwelling has TWO keys,
which is what makes the difference visible. Chamber prose renders the
pattern the room was composed from, never the ledger, so it moves for
nobody: the storeroom lists "a key" while the chest is shut on it
(`PLAY-closed-container-conceals-nothing`), and the front room says "a
doorway and a screen" on the last entry, silent about the key a player has
just set down on its floor (`PLAY-room-prose-omits-what-the-ledger-holds`).
Both are the same absent read, and both are deferred with a priced bill
rather than unnoticed.*

*One more thing not to mistake for a bug: `close` does not re-lock. A lid
and a lock are separate states, so the second `open` needs no key
(decision 0399).*

*Seed 14, and it was seed 1 until The Pavement. A dwelling's chambers are
drawn from its room's seed, and that campaign moved every room address, so
which seeds draw a strongbox is not preserved across an epoch — seed 1's
flagship dwelling now draws three chambers and no chest, and this page
regenerated onto it with every beat below the loomroom answering "You see no
a strongbox here." The seed moved to the one
`windows/vessel/tests/suite/strongbox_reachability.rs` had already moved to
for the same reason, so the test that guards this shape and the page that
publishes it now name the same world.)*

```text
[room 4189198211, day 0]
Temperate forest — old-growth timber, sun-warmed, in a hollow — in the lands of Vapatkapak. The sky above: Twilight. The horizon shines pale blue-white. The small, distant moon shows its last-quarter face. The sky is fair, with scattered cumulus. You can enter the settlement of Vapatkapak. Something ended here: migration. A dense thicket presses close around you. Underfoot, pale limestone; the ground slopes.
Here: Mekgshak, Zloppzekmok, Moopsh'wotzh'lap and Zweekdzettshat, and 54 others.
No direction here is closed; the nearest ground lies N, NE, E, SE, S, SW, W, NW.
> enter
[chamber 1098173182543299, day 0.00985]
A small room in Vapatkapak, holding a doorway, a bench, a water jar and a stone ledge.
Here: Mekgshak, Zloppzekmok, Moopsh'wotzh'lap and Zweekdzettshat, and 54 others.
Ways on: out, the hearth, the loomroom, the store.
> carrying
You are carrying nothing.
> take a key
You see no a key here.
> enter the loomroom
[chamber 1098173182260419, day 0.0197]
A small room in Vapatkapak, holding a doorway, a water jar, a loom, a key, a brazier and a stone ledge.
Here: Mekgshak, Zloppzekmok, Moopsh'wotzh'lap and Zweekdzettshat, and 54 others.
Ways on: out.
> take a key
You take the key.
> carrying
You are carrying a key.
> enter the threshold
[chamber 1098173182543299, day 0.0394]
A small room in Vapatkapak, holding a doorway, a bench, a water jar and a stone ledge.
Here: Mekgshak, Zloppzekmok, Moopsh'wotzh'lap and Zweekdzettshat, and 54 others.
Ways on: out, the hearth, the loomroom, the store.
> enter the store
[chamber 1098173180704195, day 0.04925]
A small room in Vapatkapak, holding a doorway, a water jar, a strongbox, a key and a stone ledge.
Here: Mekgshak, Zloppzekmok, Moopsh'wotzh'lap and Zweekdzettshat, and 54 others.
Ways on: out.
> examine a strongbox
A banded chest, low and heavier than it looks, its lid seated flush.
> take a key
The key is shut away in something closed.
> open a strongbox
You open the strongbox. Within it: a key.
> put a key in a strongbox
You put the key in the strongbox.
> carrying
You are carrying nothing.
> take a key
You take the key.
> carrying
You are carrying a key.
> close a strongbox
You close the strongbox.
> open a strongbox
You open the strongbox. Within it: a key.
> out
[room 4189198211, day 0.10835]
Temperate forest — old-growth timber, sun-warmed, in a hollow — in the lands of Vapatkapak. The sky above: Night. The small, distant moon shows its last-quarter face. The sky is fair, with scattered cumulus. You can enter the settlement of Vapatkapak. Something ended here: migration. A dense thicket presses close around you. Underfoot, pale limestone; the ground slopes.
Here: Mekgshak, Zloppzekmok, Moopsh'wotzh'lap and Zweekdzettshat, and 54 others.
No direction here is closed; the nearest ground lies N, NE, E, SE, S, SW, W, NW.
> enter
[chamber 1098173182543299, day 0.1182]
A small room in Vapatkapak, holding a doorway, a bench, a water jar and a stone ledge.
Here: Mekgshak, Zloppzekmok, Moopsh'wotzh'lap and Zweekdzettshat, and 54 others.
Ways on: out, the hearth, the loomroom, the store.
> carrying
You are carrying a key.
> drop a key
You set the key down.
> carrying
You are carrying nothing.
> take a key
You take the key.
> carrying
You are carrying a key.
```
