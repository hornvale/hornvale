# Architecture

The book [📖](https://hornvale.github.io/hornvale/) ahould go into deeper detail, but here's a quick overview of the architecture of the game. This is intended to be concise and crisp so that I can update it continually and refer to
it as a reference.

## Game Loop

The game loop is, basically, a REPL. It's a loop that reads a command from the player, executes it and its consequences, and then repeats once the proximal consequences have been resolved. More about these "consequences" and what "execution" means later.

_Hornvale_ adopts some elements of an Entity-Component-System architecture, so the game loop is largely based around systems dispatched to perform modifications on the game world. This is a single-threaded, deterministic loop, for the sake of simplicity; this is, after all, a text adventure.

This is not by itself sufficient to render the game world and rules in the detail we want, so the vast majority of the game engine is implemented using _events_, which are far more flexible and extensible.

### Input and Parsing

The player provides _input_ as a raw string, something familiar to anyone who's played a MUD, Interactive Fiction, or LARPed as an adult on Slack during normal business hours. This input is parsed; the parser may eventually use a context-free grammar, or multiple strategies, but for now it's just a simple tokenizer that splits the input into a series of tokens. These tokens are then used to create a _command_ object, which is enqueued for evaluation.

### Commands

Commands are merely a way of mapping human input into programmatic action. The execution of the command creates an _action_ object, the same as would be created by an AI, a game rule, or any other mechanism within the program. Thus, we bring the player into the game engine as a first-class citizen, and allow them to interact with the game world in a way that is consistent with the rest of the game.

## Actions

Actions are expressed intent to modify game state by any entity in the game world, whether it be the player character, a goblin, or a door. These normally contain some logic to check whether an action is _possible_ under universal game rules. For instance, attempting to walk northwest in a room without a northwest passage will fail immediately with output presented to the player. An action being attempted normally results in an _event_ being created and enqueued for processing.

### Events

Events are the building blocks of the game. They are managed in a priority queue and are dispatched accordingly. They are created with rich metadata that allows event subscribers to filter, modify, anticipate, and respond to them in a variety of ways. An event may implement logic in its own `process` method, or it may simply be a notification that something has happened. In the case of complex events, like combat, a complex system may be used to manage the event and its consequences.

### Output

As events are processed, they may generate output. This output is collected and presented to the player as scrolling text, familiar to anyone who's played a MUD or IF.

## Effects

Effects are direct modifications to the game state: a door is opened, a goblin is slain, a trap is sprung, the intent to exit the game loop is signaled. These always originate from an event.
