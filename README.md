# Nat20
_**natural 20** (noun)_ \
_The result of rolling a twenty-sided die and obtaining a value of 20, before applying any modifiers, commonly representing an exceptional outcome._ \
_- Oxford Dictionary (probably)_

***

**Nat20** is a **Dungeons & Dragons 5th Edition rule engine** written in Rust based on the [System Reference Document v5.2.1](#legal-srd). By _rule engine_, it means Nat20 can throw a Fireball at a Goblin, and it'll make him do a Dexterity Saving Throw before rolling 8d6 Fire damage and (probably) burning him to a crisp, but it is **not** a full game engine and it is **not** a complete game - think of it like the code version of a D&D rulebook!

The workspace contains two crates:

- [**`nat20_core`**](core): the core, data-driven rules backend. This is where the magic happens!
- [**`nat20_gui`**](gui): a [Dear ImGui](https://crates.io/crates/imgui)-based developer/debug UI for inspecting and manually testing the engine. It is intentionally a fancy debugging tool rather than a consumer-facing game UI. If you want to poke around and see the engine in action, check out the [How do I use it?](#how-do-i-use-it) section.

Here's an example of the Fireball scenario :fire::

![nat20_demo](https://github.com/user-attachments/assets/950921bc-09e3-4ea7-bc01-bd29ce95628a)

## What can it do?

While it's still a work in progress, Nat20 can already handle a variety of core D&D mechanics, including:

- **Character fundamentals**: species, backgrounds, ability scores, skill checks, saving throws, proficiency, modifiers, and rolling D20s.
- **Classes and progression**: class levels, class features, feats, and prompts for choices (e.g., fighting styles and equipment packages).
- **Actions and spells**: action economy resources (action/bonus/reaction), common actions like weapon attacks and dash, and spells like Fireball, Magic Missile and Hex.
- **Reactions**: reactions like Counterspell, Shield and Opportunity Attacks, which are triggered by specific events in the game.
- **Equipment**: weapons and armor, and weapon properties (finesse/versatile/two-handed).
- **Combat**: turn-based combat encounters with initiative order.
- **Effects and conditions**: status effects and conditions like poisoned and paralyzed.
- **Movement and positioning**: pathfinding, movement speed, and line-of-sight.
- **Rest**: short and long rests, including hit point recovery and resource replenishment.
- And more!

For a comprehensive overview of the current progress implementing the SRD v5.2.1, check out [`PROGRESS.md`](docs/PROGRESS.md).

## How do I use it?

There's currently no packaged release, however in the future the engine is intended to be a library which can be integrated into other projects, such as a full-fledged game or a virtual tabletop application. For now though you can clone the repo and take it for a spin locally:

```bash
cargo run -p nat20_gui <optional-log-level (e.g., trace, debug, info, warn, error)>
```

The GUI is (hopefully) self-explanatory, but here are some quick tips:
- **Mouse wheel scroll**: zoom in and out.
- **Middle-click + drag**: rotate the camera.
- **Middle-click + drag + shift**: translate the camera.
- **"Spawn Creature" button**: choose "New Character" to make one from scratch, choose "Predefined Creature" to spawn a predefined one and place it somewhere in the world.
- **Action bar**: left-click on a (player controlled) creature to see the action bar. Left-click on the ground to move.
- **Right-click**: open a debug menu when relevant. Try it on a creature!
- **Esc**: open the settings menu, mostly for enabling debug windows.

The engine also contains a comprehensive test suite (see [`core/tests`](core/tests)), which can be run using:

```bash
cargo test -p nat20_core
```

## How does it work?

### ECS core
The core of the engine is built around an **ECS (Entity-Component-System)** architecture using [hecs](https://crates.io/crates/hecs):

  - [`core/src/entities`](core/src/entities) defines entity types like characters/creatures.
  - [`core/src/components`](core/src/components) defines the data components (abilities, items, effects, resources, etc.).
  - [`core/src/systems`](core/src/systems) defines systems to handle stuff like taking damage, checking line-of-sight and performing actions.

### Data driven & moddable

The engine is entirely **data driven**, meaning that all items, classes, spells etc. come from JSON files in [`assets/registries`](assets/registries), where each of the subdirectories corresponds to a different type of data. If you want to add e.g. a new species all you need to do is create a new JSON file in the appropriate subdirectory with the relevant data - no recompilation required! The easiest way to get started is to look at the existing JSON files, but there are also auto-generated schemas (see [`assets/registries/.schemas`](assets/registries/.schemas)), which give you type-hints if you're using VS Code (see [`.vscode/settings.json`](.vscode/settings.json)).

Here's an example of the Fire Bolt cantrip:
```json
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
            "phases": [
                {
                    "condition": {
                        "attack_roll": "spell_attack_roll"
                    },
                    "payload": {
                        "components": [
                            {
                                "damage": "(1 + (character_level + 1) / 6)d10;fire"
                            }
                        ],
                        "delivery": {
                            "projectile": {
                                "trajectory": "ray",
                                "velocity": "40 m/s"
                            }
                        }
                    }
                }
            ]
        }
    },
    "resource_cost": {
        "nat20_core::resource.action": 1
    },
    "targeting": {
        "kind": "single",
        "range": "120 feet",
        "line_of_sight": {
            "trajectory": "ray"
        },
        "allowed_targets": [
            {
                "life_states": [
                    "dead"
                ],
                "invert": true
            }
        ]
    },
    "timeline": {
        "total_duration": 1.0,
        "perform_time": 0.5
    }
}
```

As shown above, you've got quite a lot of freedom when defining stuff like the damage of Fire Bolt: `(1 + (character_level + 1) / 6)d10;fire` or Fireball: `(8 + spell_level - 3)d6;fire` to account for things like character level scaling and upcasting.

Some rules require unique logic to determine how they work, which can be implemented using [Lua](https://www.lua.org/) scripts. Similar to the JSON schemas, there are also type stubs for Lua (see [`assets/registries/.luastubs/nat20.lua`](assets/registries/.luastubs/nat20.lua)). Here's an example of the script which checks if Counterspell can be used as a reaction:
```lua
---@type ReactionTriggerFn
local function reaction_trigger(engine_state, reactor, event)
    local action = event:as_action_requested()
    if not action then
        return false
    end

    -- Cannot counterspell yourself.
    if action.actor == reactor then
        return false
    end

    -- Only react to spells.
    return action.action_context:is_spell()
end

return {
    reaction_trigger = reaction_trigger,
}
```

### Event-based architecture

The engine uses an **event-based architecture** where everything is represented as an [`Event`](core/src/engine/event.rs), from rolling a D20 to performing an action. This allows for very transparent combat-logging, so you can see exactly what happened at every step of combat, and lets reactions be triggered by whatever you want.

<img width="550" height="304" alt="event_log_hellish_rebuke" src="https://github.com/user-attachments/assets/b1ba17ee-5e05-4213-8e7f-0986f3287a32" />

The payload of each event tracks every dice roll and bonus modifier that went into it, so you can see exactly how a final result was computed.

<img width="714" height="161" alt="event_log_dice_breakdown" src="https://github.com/user-attachments/assets/9ed466d9-3823-43d6-b00e-aa9748ef66fb" />

### Engine orchestration
The [`EngineState`](core/src/engine/engine_state.rs) is the judge, jury, and executioner of the engine. The engine state includes the ECS world, combat [`Encounter`](core/src/engine/encounter.rs)s, and event/prompt state. Whenever a creature wants to do basically anything, a request is sent to the engine state, which will then validate, e.g. that it's the creature's turn if they're in combat or that they have enough resources to perform an action.

### Auxiliary systems
- **Geometry & movement**: collision, line-of-sight, and navigation/pathing use [`parry3d`](https://crates.io/crates/parry3d), [`rerecast`](https://crates.io/crates/rerecast), and [`polyanya`](https://crates.io/crates/polyanya).
- **Scripting**: the Lua scripts are run using [`mlua`](https://crates.io/crates/mlua).
- **Units and math**: [`uom`](https://crates.io/crates/uom) for converting Imperial SRD units to internal metric, and [`glam`](https://crates.io/crates/glam) for math stuff.

## License

The project source code is currently licensed under the **MIT License** (see [`LICENSE`](LICENSE)). Licensing may evolve as the project matures.

### Legal (SRD)

This work includes material taken from the System Reference Document 5.2.1 ("SRD 5.2.1") by Wizards of the Coast LLC and available at https://dnd.wizards.com/resources/systems-reference-document. The SRD 5.2.1 is licensed under the Creative Commons Attribution 4.0 International License available at https://creativecommons.org/licenses/by/4.0/legalcode.
