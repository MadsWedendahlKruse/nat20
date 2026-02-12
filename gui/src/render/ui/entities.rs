use hecs::{Entity, World};
use nat20_core::{
    components::{
        ability::AbilityScoreMap,
        damage::DamageResistances,
        effects::effect::{EffectLifetime, EffectsMap},
        health::{hit_points::HitPoints, life_state::LifeState},
        id::{FeatId, Name, SpeciesId, SubspeciesId},
        level::{ChallengeRating, CharacterLevels},
        resource::ResourceMap,
        skill::SkillSet,
        species::{CreatureSize, CreatureType},
        speed::Speed,
        spells::spellbook::Spellbook,
        time::EntityClock,
    },
    engine::game_state::GameState,
    systems,
};
use strum::{Display, EnumIter};

use crate::{
    render::ui::{
        components::render_effect,
        inventory::{render_loadout, render_loadout_inventory},
        utils::{ImguiRenderable, ImguiRenderableMutWithContext, ImguiRenderableWithContext},
    },
    table_with_columns,
};

#[derive(Clone, EnumIter, Display)]
pub enum CreatureRenderMode {
    Full,
    Inspect,
    Compact,
}

impl From<usize> for CreatureRenderMode {
    fn from(value: usize) -> Self {
        match value {
            0 => CreatureRenderMode::Full,
            1 => CreatureRenderMode::Inspect,
            2 => CreatureRenderMode::Compact,
            _ => CreatureRenderMode::Full,
        }
    }
}

impl ImguiRenderableWithContext<(&World, &CreatureRenderMode)> for Entity {
    fn render_with_context(&self, ui: &imgui::Ui, (world, mode): (&World, &CreatureRenderMode)) {
        match mode {
            CreatureRenderMode::Full => {
                let entity = *self;
                ui.text(format!("ID: {:?}", entity));

                if let Some(tab_bar) = ui.tab_bar(format!("CharacterTabs{:?}", entity)) {
                    if let Some(tab) = ui.tab_item("Overview") {
                        render_overview(ui, world, entity, mode);
                        tab.end();
                    }

                    if let Some(tab) = ui.tab_item("Effects") {
                        render_effects(ui, world, entity);
                        render_if_present::<Vec<FeatId>>(ui, world, entity);
                        tab.end();
                    }

                    if let Some(tab) = ui.tab_item("Skills") {
                        systems::helpers::get_component::<SkillSet>(world, entity)
                            .render_with_context(ui, (world, entity));
                        tab.end();
                    }

                    if let Some(tab) = ui.tab_item("Inventory") {
                        render_loadout(ui, world, entity);
                        tab.end();
                    }

                    if let Some(tab) = ui.tab_item("Spellbook") {
                        systems::helpers::get_component::<Spellbook>(world, entity)
                            .render_with_context(
                                ui,
                                &systems::helpers::get_component::<ResourceMap>(world, entity),
                            );
                        tab.end();
                    }

                    if let Some(tab) = ui.tab_item("Resources") {
                        render_if_present::<ResourceMap>(ui, world, entity);
                        tab.end();
                    }

                    tab_bar.end();
                }
            }

            CreatureRenderMode::Inspect => {
                let entity = *self;
                render_if_present::<Name>(ui, world, *self);

                if let Some(tab_bar) = ui.tab_bar(format!("CharacterTabs{:?}", entity)) {
                    if let Some(tab) = ui.tab_item("Overview") {
                        render_overview(ui, world, entity, mode);
                        tab.end();
                    }

                    if let Some(tab) = ui.tab_item("Effects") {
                        render_effects(ui, world, entity);
                        render_if_present::<Vec<FeatId>>(ui, world, entity);
                        tab.end();
                    }

                    tab_bar.end();
                }
            }

            CreatureRenderMode::Compact => {
                render_if_present::<Name>(ui, world, *self);
                render_if_present::<CharacterLevels>(ui, world, *self);
                render_if_present::<ChallengeRating>(ui, world, *self);
                render_if_present::<LifeState>(ui, world, *self);
                render_if_present::<HitPoints>(ui, world, *self);
                render_effects_compact(ui, world, *self);
            }
        }
    }
}

pub fn render_if_present<T>(ui: &imgui::Ui, world: &World, entity: Entity)
where
    T: hecs::Component + 'static + ImguiRenderable,
{
    if let Ok(component) = world.get::<&T>(entity) {
        component.render(ui);
    }
}

pub fn render_species_if_present(ui: &imgui::Ui, world: &World, entity: Entity) {
    let mut query = world
        .query_one::<(&SpeciesId, &Option<SubspeciesId>)>(entity)
        .unwrap();
    if let Some((species, subspecies)) = query.get() {
        (species.clone(), subspecies.clone()).render(ui);
    }
}

fn render_overview(ui: &imgui::Ui, world: &World, entity: Entity, mode: &CreatureRenderMode) {
    match mode {
        CreatureRenderMode::Full | CreatureRenderMode::Inspect => {
            render_species_if_present(ui, world, entity);

            render_if_present::<CreatureSize>(ui, world, entity);
            ui.same_line();
            render_if_present::<CreatureType>(ui, world, entity);

            render_if_present::<CharacterLevels>(ui, world, entity);
            render_if_present::<ChallengeRating>(ui, world, entity);
            render_if_present::<LifeState>(ui, world, entity);
            render_if_present::<HitPoints>(ui, world, entity);

            render_if_present::<Speed>(ui, world, entity);

            ui.separator_with_text("Armor Class");
            systems::loadout::armor_class(world, entity).render(ui);
            systems::helpers::get_component::<AbilityScoreMap>(world, entity)
                .render_with_context(ui, (world, entity));
            render_if_present::<DamageResistances>(ui, world, entity);
        }
        _ => {}
    }
}

fn render_effects(ui: &imgui::Ui, world: &World, entity: Entity) {
    let time_mode = systems::helpers::get_component::<EntityClock>(world, entity).mode();
    if let Ok(effects) = world.get::<&EffectsMap>(entity) {
        effects.render_with_context(ui, &time_mode);
    }
}

fn render_effects_compact(ui: &imgui::Ui, world: &World, entity: Entity) {
    let time_mode = systems::helpers::get_component::<EntityClock>(world, entity).mode();
    let effects = systems::helpers::get_component::<EffectsMap>(world, entity);
    let conditions = effects
        .values()
        .filter(|e| !matches!(e.lifetime, EffectLifetime::Permanent))
        .collect::<Vec<_>>();
    ui.separator_with_text("Conditions");
    if !conditions.is_empty() {
        if let Some(table) = table_with_columns!(ui, "Conditions", "Condition", "Duration") {
            for effect in conditions {
                // TODO: Duplicated logic with EffectsMap#render_with_context
                if effect.parent.is_some() {
                    continue;
                }
                ui.table_next_column();
                render_effect(ui, effect, &effects);
                ui.table_next_column();
                effect.lifetime.render_with_context(ui, &time_mode);
            }
            table.end();
        }
    } else {
        ui.text("None");
    }
}

impl ImguiRenderableMutWithContext<&mut GameState> for Entity {
    fn render_mut_with_context(&mut self, ui: &imgui::Ui, game_state: &mut GameState) {
        let entity = *self;
        ui.text(format!("ID: {:?}", entity));

        if let Some(tab_bar) = ui.tab_bar(format!("CharacterTabs{:?}", entity)) {
            if let Some(tab) = ui.tab_item("Overview") {
                render_overview(ui, &game_state.world, entity, &CreatureRenderMode::Full);
                tab.end();
            }

            if let Some(tab) = ui.tab_item("Effects") {
                render_effects(ui, &game_state.world, entity);
                render_if_present::<Vec<FeatId>>(ui, &game_state.world, entity);
                tab.end();
            }

            if let Some(tab) = ui.tab_item("Skills") {
                systems::helpers::get_component::<SkillSet>(&game_state.world, entity)
                    .render_with_context(ui, (&game_state.world, entity));
                tab.end();
            }

            if let Some(tab) = ui.tab_item("Inventory") {
                render_loadout_inventory(ui, game_state, entity);
                tab.end();
            }

            if let Some(tab) = ui.tab_item("Spellbook") {
                if let Ok((spellbook, resources)) = game_state
                    .world
                    .query_one_mut::<(&mut Spellbook, &mut ResourceMap)>(entity)
                {
                    spellbook.render_mut_with_context(ui, resources);
                }
                tab.end();
            }

            if let Some(tab) = ui.tab_item("Resources") {
                render_if_present::<ResourceMap>(ui, &game_state.world, entity);
                tab.end();
            }

            tab_bar.end();
        }
    }
}
