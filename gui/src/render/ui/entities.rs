use hecs::{Entity, World};
use nat20_core::{
    components::{
        ability::AbilityScoreMap,
        damage::DamageResistances,
        effects::{effect::EffectLifetime, effect_manager::EffectManager},
        health::{hit_points::HitPoints, life_state::LifeState},
        id::{FeatId, Name, SpeciesId, SubspeciesId},
        level::{ChallengeRating, CharacterLevels},
        resource::ResourceMap,
        skill::SkillSet,
        species::{CreatureSize, CreatureType},
        spells::spellbook::Spellbook,
        time::EntityClock,
    },
    engine::engine_state::EngineState,
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

impl ImguiRenderableWithContext<(&EngineState, &CreatureRenderMode)> for Entity {
    fn render_with_context(
        &self,
        ui: &imgui::Ui,
        (engine_state, mode): (&EngineState, &CreatureRenderMode),
    ) {
        match mode {
            CreatureRenderMode::Full => {
                let entity = *self;
                ui.text(format!("ID: {:?}", entity));

                if let Some(tab_bar) = ui.tab_bar(format!("CharacterTabs{:?}", entity)) {
                    if let Some(tab) = ui.tab_item("Overview") {
                        render_overview(ui, engine_state, entity, mode);
                        tab.end();
                    }

                    if let Some(tab) = ui.tab_item("Effects") {
                        render_effects(ui, &engine_state.world, entity);
                        render_if_present::<Vec<FeatId>>(ui, &engine_state.world, entity);
                        tab.end();
                    }

                    if let Some(tab) = ui.tab_item("Skills") {
                        systems::helpers::get_component::<SkillSet>(&engine_state.world, entity)
                            .render_with_context(ui, (engine_state, entity));
                        tab.end();
                    }

                    if let Some(tab) = ui.tab_item("Inventory") {
                        render_loadout(ui, &engine_state.world, entity);
                        tab.end();
                    }

                    if let Some(tab) = ui.tab_item("Spellbook") {
                        systems::helpers::get_component::<Spellbook>(&engine_state.world, entity)
                            .render_with_context(
                                ui,
                                &systems::helpers::get_component::<ResourceMap>(
                                    &engine_state.world,
                                    entity,
                                ),
                            );
                        tab.end();
                    }

                    if let Some(tab) = ui.tab_item("Resources") {
                        render_if_present::<ResourceMap>(ui, &engine_state.world, entity);
                        tab.end();
                    }

                    tab_bar.end();
                }
            }

            CreatureRenderMode::Inspect => {
                let entity = *self;
                render_if_present::<Name>(ui, &engine_state.world, *self);

                if let Some(tab_bar) = ui.tab_bar(format!("CharacterTabs{:?}", entity)) {
                    if let Some(tab) = ui.tab_item("Overview") {
                        render_overview(ui, engine_state, entity, mode);
                        tab.end();
                    }

                    if let Some(tab) = ui.tab_item("Effects") {
                        render_effects(ui, &engine_state.world, entity);
                        render_if_present::<Vec<FeatId>>(ui, &engine_state.world, entity);
                        tab.end();
                    }

                    tab_bar.end();
                }
            }

            CreatureRenderMode::Compact => {
                render_if_present::<Name>(ui, &engine_state.world, *self);
                render_if_present::<CharacterLevels>(ui, &engine_state.world, *self);
                render_if_present::<ChallengeRating>(ui, &engine_state.world, *self);
                render_if_present::<LifeState>(ui, &engine_state.world, *self);
                render_if_present::<HitPoints>(ui, &engine_state.world, *self);
                render_effects_compact(ui, &engine_state.world, *self);
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

fn render_overview(
    ui: &imgui::Ui,
    engine_state: &EngineState,
    entity: Entity,
    mode: &CreatureRenderMode,
) {
    match mode {
        CreatureRenderMode::Full | CreatureRenderMode::Inspect => {
            let world = &engine_state.world;
            render_species_if_present(ui, world, entity);

            render_if_present::<CreatureSize>(ui, world, entity);
            ui.same_line();
            render_if_present::<CreatureType>(ui, world, entity);

            render_if_present::<CharacterLevels>(ui, world, entity);
            render_if_present::<ChallengeRating>(ui, world, entity);
            render_if_present::<LifeState>(ui, world, entity);
            render_if_present::<HitPoints>(ui, world, entity);

            systems::movement::speed(engine_state, entity).render(ui);

            ui.separator_with_text("Armor Class");
            systems::loadout::armor_class(engine_state, entity).render(ui);
            systems::helpers::get_component::<AbilityScoreMap>(world, entity)
                .render_with_context(ui, (engine_state, entity));
            render_if_present::<DamageResistances>(ui, world, entity);
        }
        _ => {}
    }
}

fn render_effects(ui: &imgui::Ui, world: &World, entity: Entity) {
    let time_mode = systems::helpers::get_component::<EntityClock>(world, entity).mode();
    if let Ok(effects) = world.get::<&EffectManager>(entity) {
        effects.effects.render_with_context(ui, &time_mode);
    }
}

fn render_effects_compact(ui: &imgui::Ui, world: &World, entity: Entity) {
    let time_mode = systems::helpers::get_component::<EntityClock>(world, entity).mode();
    let effects = &systems::helpers::get_component::<EffectManager>(world, entity).effects;
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
                render_effect(ui, effect, effects);
                ui.table_next_column();
                effect.lifetime.render_with_context(ui, &time_mode);
            }
            table.end();
        }
    } else {
        ui.text("None");
    }
}

impl ImguiRenderableMutWithContext<&mut EngineState> for Entity {
    fn render_mut_with_context(&mut self, ui: &imgui::Ui, engine_state: &mut EngineState) {
        let entity = *self;
        ui.text(format!("ID: {:?}", entity));

        if let Some(tab_bar) = ui.tab_bar(format!("CharacterTabs{:?}", entity)) {
            if let Some(tab) = ui.tab_item("Overview") {
                render_overview(ui, engine_state, entity, &CreatureRenderMode::Full);
                tab.end();
            }

            if let Some(tab) = ui.tab_item("Effects") {
                render_effects(ui, &engine_state.world, entity);
                render_if_present::<Vec<FeatId>>(ui, &engine_state.world, entity);
                tab.end();
            }

            if let Some(tab) = ui.tab_item("Skills") {
                systems::helpers::get_component::<SkillSet>(&engine_state.world, entity)
                    .render_with_context(ui, (engine_state, entity));
                tab.end();
            }

            if let Some(tab) = ui.tab_item("Inventory") {
                render_loadout_inventory(ui, engine_state, entity);
                tab.end();
            }

            if let Some(tab) = ui.tab_item("Spellbook") {
                if let Ok((spellbook, resources)) = engine_state
                    .world
                    .query_one_mut::<(&mut Spellbook, &mut ResourceMap)>(entity)
                {
                    spellbook.render_mut_with_context(ui, resources);
                }
                tab.end();
            }

            if let Some(tab) = ui.tab_item("Resources") {
                render_if_present::<ResourceMap>(ui, &engine_state.world, entity);
                tab.end();
            }

            tab_bar.end();
        }
    }
}
