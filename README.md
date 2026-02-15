# Nat20
_**natural 20** (noun)_ \
_The result of rolling a twenty-sided die and obtaining a value of 20, before applying any modifiers, often representing an exceptional outcome._ \
_- Oxford Dictionary (probably)_

## Overview

**Nat20** is a **Dungeons & Dragons 5th Edition combat engine**  written in Rust. The engine is based on the [System Reference Document v5.2.1](#legal-srd). By _combat engine_, it means nat20 can execute the rules involved in throwing a Fireball at a Goblin, and it'll make him do a Dexterity Saving Throw before rolling 8d6 and (probably) burning the him to a crisp, but it is **not** a full _game engine_ and it is **not** a complete game (no actual "gameplay", UI, or tooling beyond a developer/debug view). Think of it like the code version of a D&D rulebook!

This workspace contains two crates:

- **`core`** (`nat20_core`): the core, data-driven rules/logic backend. This is where the magic happens!
- **`gui`** (`nat20_gui`): an [ImGui](`https://crates.io/crates/imgui`)-based developer/debug UI for inspecting and exercising the engine. It is intentionally a fancy debugging tool rather than a consumer-facing game UI. If you want to poke around and see what the engine can do (so far :wink:), do feel free to take it for a spin!

## Usage

The engine is currently very much a work-in-progress, so there is no packaged release or anything like that yet. If you want to try it out, the easiest way is to clone the repository and run the GUI crate: 
```bash
cargo run -p nat20_gui <optional-log-level>
```
In the GUI, you can spawn some creatures and run through a combat encounter to see how the engine handles turns, actions, movement, and spellcasting.

In the future, the engine is intended to be usable as a library that can be integrated into other projects, such as a full-fledged game or a virtual tabletop application.

Here's an example of the Fireball scenario described earlier :fire::

![nat20_demo](https://github.com/user-attachments/assets/950921bc-09e3-4ea7-bc01-bd29ce95628a)

## Engine architecture

### ECS core
The core of the engine is built around an **ECS (Entity-Component-System)** architecture using the [`hecs`](https://crates.io/crates/hecs) crate. This allows for flexible and efficient management of game entities and their behaviors.

  - `core/src/entities` defines entity types like characters/creatures.
  - `core/src/components` defines the data components (abilities, items, effects, resources, etc.).
  - `core/src/systems` implements rules and mechanics over the ECS world.

### Data-driven design, moddable by default

The engine is intentionally **data-driven**, meaing pretty much all “game content” comes from JSON definitions under `assets/registries` and can be extended or replaced without recompiling the engine:

- Classes, subclasses, species, backgrounds, feats, items, spells, resources, and actions are defined in JSON. For example, here is the JSON definition for the *Fire Bolt* cantrip:
```JSON
{
    "id": "nat20_core::spell.fire_bolt",
    "description": "You hurl a mote of fire at a creature or an object within range. Make a ranged spell attack against the target. On a hit, the target takes 1d10 Fire damage. A flammable object hit by this spell starts burning if it isn’t being worn or carried",
    "base_level": 0,
    "school": "evocation",
    "flags": [
        "verbal",
        "somatic"
    ],
    "kind": {
        "standard": {
            "condition": {
                "attack_roll": "spell_attack_roll"
            },
            "payload": {
                "damage": "(1 + (character_level + 1) / 6)d10;fire"
            }
        }
    },
    "resource_cost": {
        "nat20_core::resource.action": 1
    },
    "targeting": {
        "kind": "single",
        "range": "120 feet",
        "require_line_of_sight": true,
        "allowed_targets": ["not_dead"]
    }
}
```

- Scripted behaviors for spells and effects that have some unique behavior (e.g. Counterspell) are implemented in Rhai and used in combination with the JSON definitions. For example, here is the Rhai script that defines when *Counterspell* can be triggered:
```rhai
// Script for checking if Counterspell should be triggered by an event
fn reaction_trigger(context) {
    let event = context.event;

    // Only care about "action-like" events (ActionRequested / ReactionRequested)
    if !event.is_action_requested() {
        return false;
    }

    let action = event.as_action_requested();

    // Cannot counterspell yourself
    if action.actor == context.reactor {
        return false;
    }

    // Only react to spells (for now any spell; you can refine later)
    if !action.action_context.is_spell() {
        return false;
    }

    true
}
```

This makes the system inherently moddable: new spells or items can be added by dropping a JSON file (and optional script) into the registries folder.

### Event-based architecture

The engine uses an **event-based architecture** where everything is represented as an event, from rolling a D20 to performing an action. This allows for very transparent combat-logging, so you can see exactly what happened at every step of combat, and to allow reactions that can be triggered by whatever you want (`core/src/engine/event`).

<img width="550" height="304" alt="event_log_hellish_rebuke" src="https://github.com/user-attachments/assets/b1ba17ee-5e05-4213-8e7f-0986f3287a32" />

The payload of each event tracks every dice roll and bonus modifier that went into it, so you can see exactly how a final result was computed.

<img width="714" height="161" alt="event_log_dice_breakdown" src="https://github.com/user-attachments/assets/9ed466d9-3823-43d6-b00e-aa9748ef66fb" />

### Game state/encounter orchestration
The `core/src/engine` module containts the overall **game state** (`core/src/engine/game_state`), which is the judge, jury, and executioner of the engine. The game state includes the ECS world, combat encounters (`core/src/engine/encounter`), and event/interaction state. Whenever a creature wants to do basically anything, a request is sent to the game state, which will then validate, e.g. that it's the creatures turn if they're in combat or that they have enough resources to perform an action.

### Auxiliary systems
- **Geometry & movement**: collision, line-of-sight, and navigation/pathing use [`parry3d`](https://crates.io/crates/parry3d), [`rerecast`](https://crates.io/crates/rerecast), and [`polyanya`](https://crates.io/crates/polyanya) (`engine/src/engine/geometry` and `engine/src/systems/geometry`/`movement`).
- **Scripting**: rules and effects can be extended via [**Rhai**](https://rhai.rs/) scripts (`engine/src/scripts`), used by data-defined registry entries.
- **Units and math**: [`uom`](https://crates.io/crates/uom) and [`glam`](https://crates.io/crates/glam) help keep movement, ranges, and values consistent.

## What currently works

Coverage is intentionally incomplete, but the following fundamentals are implemented and exercised by tests and data definitions:

- **Character fundamentals**: ability scores, skill checks, saving throws, proficiency, modifiers, and d20 roll logic.
- **Equipment and combat stats**: weapons, armor, loadouts, attack/damage rolls, weapon properties (finesse/versatile/two-handed), and equipment-derived modifiers.
- **Class progression & features**: levels, class features, feats, and prompts for choices (e.g., fighting styles and equipment packages).
- **Resources and actions**: action economy resources (action/bonus/reaction), basic action definitions like weapon attack and dash.
- **Species/backgrounds**: initial character origin data with selectable options.
- **Spells & effects (partial)**: registry-backed spells and effects with scripted hooks. Examples include `fireball`, `magic_missile`, `eldritch_blast`, and `hex` variants.
- **Combat state & movement**: turn-based encounters, movement/pathing, and line-of-sight checks.

If you want to poke around and see it in action fire up the **GUI crate**, which exists to visualize and debug the underlying systems (`cargo run -p nat20_gui`).

## License

The project source code is currently licensed under the **MIT License** (see `LICENSE`). Licensing may evolve as the project matures.

### Legal (SRD)

This work includes material taken from the System Reference Document 5.2.1 (“SRD 5.2.1”) by Wizards of the Coast LLC and available at https://dnd.wizards.com/resources/systems-reference-document. The SRD 5.2.1 is licensed under the Creative Commons Attribution 4.0 International License available at https://creativecommons.org/licenses/by/4.0/legalcode.
