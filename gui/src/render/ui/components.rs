use std::{ops::Deref, vec};

use hecs::{Entity, World};
use imgui::{TreeNodeFlags};
use nat20_core::{
    components::{
        ability::{Ability, AbilityScore, AbilityScoreMap},
        actions::{
            action::{
                ActionCondition, ActionContext, ActionKind, ActionKindResult, ActionResult,
                ActionConditionResolution, ReactionResult,
            },
            targeting::{AreaShape, TargetInstance, TargetingKind, TargetingRange},
        },
        d20::{D20CheckDC, D20CheckOutcome, D20CheckResult, RollMode},
        damage::{
            AttackRollResult, DamageComponentMitigation, DamageComponentResult,
            DamageMitigationEffect, DamageMitigationResult, DamageResistances, DamageRoll,
            DamageRollResult, MitigationOperation,
        },
        effects::effect::{EffectInstance, EffectLifetime, EffectsMap},
        health::{hit_points::HitPoints, life_state::LifeState},
        id::{ActionId, FeatId, Name, ResourceId, SpeciesId, SpellId, SubspeciesId},
        items::{
            equipment::{
                armor::{Armor, ArmorClass, ArmorDexterityBonus, ArmorType},
                loadout::Loadout,
                weapon::{MELEE_RANGE_DEFAULT, Weapon},
            },
            item::{Item, ItemRarity},
            money::MonetaryValue,
        },
        level::{ChallengeRating, CharacterLevels, Level},
        modifier::{Modifiable, ModifierSet},
        proficiency::{Proficiency, ProficiencyLevel},
        resource::{ResourceAmount, ResourceAmountMap, ResourceBudgetKind, ResourceMap},
        saving_throw::{SavingThrowKind, SavingThrowSet},
        skill::{Skill, SkillSet, skill_ability},
        species::{CreatureSize, CreatureType},
        speed::Speed,
        spells::spellbook::Spellbook, time::{TimeDuration, TimeMode},
    },
    registry::registry::{FeatsRegistry, SpellsRegistry},
    systems::{
        self,
        d20::{D20CheckDCKind, D20ResultKind},
    },
};
use std::collections::HashSet;
use strum::IntoEnumIterator;
use uom::si::{angle::degree, length::meter, mass::kilogram};

use crate::{
    render::ui::{
        engine::render_event_description,
        text::{TextKind, TextSegment, TextSegments, indent_text, item_rarity_color},
        utils::{
            ImguiRenderable, ImguiRenderableMutWithContext, ImguiRenderableWithContext,
            ProgressBarColor, render_progress_bar,
            roman_numeral,
        },
    },
    table_with_columns,
};

pub enum ModifierSetRenderMode {
    Line,
    List(u8),
    Hoverable,
}

fn sign(value: i32) -> &'static str {
    if value >= 0 { "+" } else { "-" }
}

impl ImguiRenderableWithContext<ModifierSetRenderMode> for ModifierSet {
    fn render_with_context(&self, ui: &imgui::Ui, mode: ModifierSetRenderMode) {
        match mode {
            ModifierSetRenderMode::Line => {
                if self.is_empty() {
                    return;
                }
                let mut segments = Vec::new();
                for (source, value) in self.iter() {
                    if value == &0 {
                        continue;
                    }
                    segments.push((
                        format!("{} {}", sign(*value), value.abs()),
                        TextKind::Normal,
                    ));
                    segments.push((format!("({})", source), TextKind::Details));
                }
                TextSegments::new(segments).render(ui);
            }

            ModifierSetRenderMode::List(indent_level) => {
                for (source, value) in self.iter() {
                    if value == &0 {
                        continue;
                    }
                    TextSegments::new(vec![
                        (format!("{}{}", sign(*value), value.abs()), TextKind::Normal),
                        (source.to_string(), TextKind::Details),
                    ])
                    .with_indent(indent_level)
                    .render(ui);
                }
            }

            ModifierSetRenderMode::Hoverable => {
                let total = format!("{}{}", sign(self.total()), self.total().abs());
                ui.text(total);
                if self.is_empty() {
                    return;
                }
                if ui.is_item_hovered() {
                    ui.tooltip(|| {
                        ui.text(format!("Total: {}", self.total()));
                        self.render_with_context(ui, ModifierSetRenderMode::List(1));
                    });
                }
            }
        }
    }
}

// TODO: Replace with Name (or similar struct)?
impl ImguiRenderable for Name {
    fn render(&self, ui: &imgui::Ui) {
        ui.text(self.as_str());
    }
}

impl ImguiRenderable for CharacterLevels {
    fn render(&self, ui: &imgui::Ui) {
        let mut class_strings = Vec::new();
        for (class_id, level_progression) in self.all_classes() {
            let level = level_progression.level();
            let class_str = if let Some(subclass_id) = level_progression.subclass() {
                format!("Level {} {}", level, subclass_id)
            } else {
                format!("Level {} {}", level, class_id)
            };
            class_strings.push(class_str);
        }
        let all_classes = class_strings.join(", ");
        ui.text(all_classes);
    }
}

impl ImguiRenderable for ChallengeRating {
    fn render(&self, ui: &imgui::Ui) {
        // TODO: Not sure if we should write level or challenge rating
        ui.text(format!("Level {}", self.total_level()));
    }
}

// TODO: Store all colors in one place?
pub static FULL_HEALTH_COLOR: [f32; 4] = [0.0, 0.7, 0.0, 1.0];
pub static FULL_HEALTH_BG_COLOR: [f32; 4] = [0.0, 0.2, 0.0, 1.0];
pub static LOW_HEALTH_COLOR: [f32; 4] = [0.7, 0.0, 0.0, 1.0];
pub static LOW_HEALTH_BG_COLOR: [f32; 4] = [0.2, 0.0, 0.0, 1.0];

impl ImguiRenderable for HitPoints {
    fn render(&self, ui: &imgui::Ui) {
        render_progress_bar(
            ui,
            self.current(),
            self.max(),
            self.temp().map(|temp| temp.amount()),
            self.current() as f32 / self.max() as f32,
            150.0,
            "HP",
            None,
            Some(ProgressBarColor {
                color_full: FULL_HEALTH_COLOR,
                color_empty: LOW_HEALTH_COLOR,
                color_full_bg: FULL_HEALTH_BG_COLOR,
                color_empty_bg: LOW_HEALTH_BG_COLOR,
            }),
        );
    }
}

impl ImguiRenderable for LifeState {
    fn render(&self, ui: &imgui::Ui) {
        match self {
            LifeState::Normal => {}
            LifeState::Unconscious(death_saving_throws) => {
                // Render something like [++|-] where + is a success and - is a failure
                let mut segments = Vec::new();
                segments.push(("Unconscious: ".to_string(), TextKind::Details));
                segments.push(("[".to_string(), TextKind::Details));
                segments.push((
                    format!("{}", "+".repeat(death_saving_throws.successes() as usize)),
                    TextKind::Green,
                ));
                segments.push(("|".to_string(), TextKind::Details));
                segments.push((
                    format!("{}", "-".repeat(death_saving_throws.failures() as usize)),
                    TextKind::Red,
                ));
                segments.push(("]".to_string(), TextKind::Details));
                TextSegments::new(segments).render(ui);
            }
            _ => {
                TextSegment::new(format!("{:?}", self), TextKind::Details).render(ui);
            }
        }
    }
}

fn proficiency_icon(proficiency: &ProficiencyLevel) -> &'static str {
    match proficiency {
        ProficiencyLevel::None => "",
        ProficiencyLevel::Half => "½",
        ProficiencyLevel::Proficient => "*",
        ProficiencyLevel::Expertise => "**",
    }
}

impl ImguiRenderableWithContext<&str> for Proficiency {
    fn render_with_context(&self, ui: &imgui::Ui, context: &str) {
        let level = self.level();
        if level != &ProficiencyLevel::None {
            ui.text(proficiency_icon(level));
            if ui.is_item_hovered() {
                ui.tooltip(|| {
                    let mut segments = vec![
                        (format!("{}", level), TextKind::Normal),
                        (format!("({})", self.source()), TextKind::Details),
                    ];
                    if !context.is_empty() {
                        segments.push((context.to_string(), TextKind::Details));
                    }
                    TextSegments::new(segments).render(ui);
                });
            }
        }
    }
}

impl ImguiRenderable for Proficiency {
    fn render(&self, ui: &imgui::Ui) {
        self.render_with_context(ui, &"");
    }
}

impl ImguiRenderable for AbilityScore {
    fn render(&self, ui: &imgui::Ui) {
        ui.separator_with_text(self.ability.to_string());

        let modifier = self.ability_modifier().total();
        TextSegments::new(vec![
            (format!("Total: {}", self.total()), TextKind::Normal),
            (
                format!("(Modifier: {}{})", sign(modifier), modifier),
                TextKind::Details,
            ),
        ])
        .render(ui);

        TextSegments::new(vec![
            (self.base.to_string(), TextKind::Normal),
            ("Base".to_string(), TextKind::Details),
        ])
        .with_indent(1)
        .render(ui);

        self.modifiers
            .render_with_context(ui, ModifierSetRenderMode::List(1));
    }
}

impl ImguiRenderableWithContext<(&World, Entity)> for AbilityScoreMap {
    fn render_with_context(&self, ui: &imgui::Ui, (world, entity): (&World, Entity)) {
        ui.separator_with_text("Abilities");

        let saving_throws = systems::helpers::get_component::<SavingThrowSet>(world, entity);

        let style = ui.push_style_var(imgui::StyleVar::ButtonTextAlign([0.5, 0.5]));
        for (i, ability) in Ability::iter().enumerate() {
            let ability_score = self.get(&ability);
            let saving_throw_kind = SavingThrowKind::Ability(ability);
            let saving_throw_proficiency = saving_throws.get(&saving_throw_kind).proficiency();

            if i > 0 {
                ui.same_line();
            }
            ui.button_with_size(
                format!("{}\n{}", ability.acronym(), ability_score.total()),
                [30.0, 30.0],
            );
            if ui.is_item_hovered() {
                ui.tooltip(|| {
                    ability_score.render(ui);

                    ui.separator_with_text("Saving Throw");

                    if saving_throw_proficiency.level() != &ProficiencyLevel::None {
                        TextSegments::new(vec![
                            (
                                format!("{}", saving_throw_proficiency.level()),
                                TextKind::Normal,
                            ),
                            (
                                format!("({})", saving_throw_proficiency.source()),
                                TextKind::Details,
                            ),
                        ])
                        .render(ui);
                    }
                    let result = saving_throws.check(&saving_throw_kind, world, entity);
                    let modifiers = &result.modifier_breakdown;
                    let total = modifiers.total();
                    ui.text(format!("Bonus: {}{}", sign(total), total.abs()));
                    modifiers.render_with_context(ui, ModifierSetRenderMode::List(1));
                });
            }
        }
        style.pop();
    }
}

impl ImguiRenderableWithContext<(&World, Entity)> for SkillSet {
    fn render_with_context(&self, ui: &imgui::Ui, (world, entity): (&World, Entity)) {
        // Empty column is for proficiency
        if let Some(table) = table_with_columns!(ui, "Skills", "", "Skill", "Bonus") {
            // Skills are ordered by ability, so if the ability changes, we can
            // render a separator. Since the first skill is Athletics, we just
            // have to start with anything other than Strength.
            let mut prev_ability = Ability::Charisma;

            for skill in Skill::iter() {
                let ability = skill_ability(&skill).unwrap();

                // If the ability has changed, render a separator
                if ability != prev_ability {
                    ui.table_next_row_with_flags(imgui::TableRowFlags::empty());
                    ui.table_next_column();
                    ui.table_next_column();

                    let label = format!("{}", ability);
                    ui.text_colored([0.7, 0.7, 0.7, 1.0], &label);
                    prev_ability = ability;

                    ui.table_next_column();
                }

                // Proficiency column
                ui.table_next_column();
                let proficiency = self.get(&skill).proficiency();
                proficiency.render(ui);
                // Skill name
                ui.table_next_column();
                ui.text(skill.to_string());
                // Bonus column
                ui.table_next_column();
                // TODO: Avoid doing an actual skill check here every time
                let result = self.check(&skill, world, entity);
                result
                    .modifier_breakdown
                    .render_with_context(ui, ModifierSetRenderMode::Hoverable);
            }

            table.end();
        }
    }
}

static EMPTY_RESOURCE_ICON: &str = "X"; // Placeholder for empty resource icon
static FILLED_RESOURCE_ICON: &str = "O"; // Placeholder for filled resource icon

impl ImguiRenderable for ResourceMap {
    fn render(&self, ui: &imgui::Ui) {
        // Split resources into flat and tiered
        let flat_resources: Vec<(&ResourceId, &ResourceBudgetKind)> = self
            .iter()
            .filter(|(_, r)| matches!(r, ResourceBudgetKind::Flat(_)))
            .collect();
        let tiered_resources: Vec<(&ResourceId, &ResourceBudgetKind)> = self
            .iter()
            .filter(|(_, r)| matches!(r, ResourceBudgetKind::Tiered { .. }))
            .collect();

        if let Some(table) = table_with_columns!(ui, "Resources", "Resource", "Count", "Recharge") {
            for (resource_id, resource) in flat_resources.iter() {
                // Resource ID column
                ui.table_next_column();
                if let Some(source) = self.is_resource_disabled(*resource_id) {
                    TextSegment::new(
                        resource_id.to_string(),
                        TextKind::Red,
                    )
                    .render(ui);

                    if ui.is_item_hovered() {
                        ui.tooltip(|| {
                            TextSegments::new(vec![
                                ("Disabled by:".to_string(), TextKind::Normal),
                                (source.to_string(), TextKind::Details)
                            ]).render(ui);
                        });
                    }

                } else {
                    ui.text(resource_id.to_string());
                }

                // Resource count column
                ui.table_next_column();
                match resource {
                    ResourceBudgetKind::Flat(budget) => {
                        ui.text(format!("{}/{}", budget.current_uses, budget.max_uses));
                    }
                    _ => {
                        ui.text("Expected ResourceKind::Flat");
                    }
                }
                // let mut text = String::new();
                // for i in (0..max).rev() {
                //     if i < current {
                //         text.push_str(FILLED_RESOURCE_ICON);
                //     } else {
                //         text.push_str(EMPTY_RESOURCE_ICON);
                //     }
                // }
                // ui.text(text);
                // Recharge column
                ui.table_next_column();
                ui.text(format!(
                    "{}",
                    systems::resources::recharge_rule(resource_id)
                ));
            }
            table.end();
        }

        for (resource_id, resource) in tiered_resources {
            ui.separator_with_text(resource_id.to_string());
            if let Some(table) = table_with_columns!(ui, resource_id.to_string(), "Level", "Slots")
            {
                match resource {
                    ResourceBudgetKind::Tiered(budgets) => {
                        for (tier, budget) in budgets {
                            // Level column
                            ui.table_next_column();
                            ui.text(roman_numeral(*tier));
                            // Current uses column
                            ui.table_next_column();
                            ui.text(format!("{}/{}", budget.current_uses, budget.max_uses));
                        }
                    }
                    _ => {
                        ui.text("Expected ResourceKind::Tiered");
                    }
                }
                table.end();
            }
        }
    }
}

#[derive(Debug)]
enum SpellbookUiAction {
    Prepare(SpellId),
    Unprepare(SpellId),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum RenderMode {
    ReadOnly,
    Editable,
}

fn render_spellbook_ui(
    ui: &imgui::Ui,
    spellbook: &Spellbook,
    resources: &ResourceMap,
    mode: RenderMode,
) -> Vec<SpellbookUiAction> {
    let mut actions = Vec::new();

    ui.text(format!("{:#?}", spellbook));

    // if spellbook.is_empty() {
    //     ui.text("No spells known.");
    //     return actions;
    // }

    // // --- Cantrips ---
    // ui.separator_with_text("Cantrips");
    // for spell_id in spellbook.all_spells() {
    //     let spell = SpellsRegistry::get(spell_id).unwrap();
    //     if spell.is_cantrip() {
    //         let _disabled = match mode {
    //             RenderMode::ReadOnly => Some(ui.begin_disabled(true)),
    //             RenderMode::Editable => None,
    //         };
    //         if ui.button(spell_id.to_string()) {
    //             // (e.g. open inspector) -> if you later add an inspect action, push it here.
    //         }
    //     }
    // }

    // // --- Prepared Spells ---
    // ui.separator_with_text("Prepared Spells");
    // let prepared_spells: HashSet<SpellId> = spellbook.prepared_spells().clone();
    // let mut rendered = 0;
    // for spell_id in &prepared_spells {
    //     let spell = SpellsRegistry::get(spell_id).unwrap();
    //     let label = format!("{} ({})", spell_id, roman_numeral(spell.base_level()));

    //     let _disabled = match mode {
    //         RenderMode::ReadOnly => Some(ui.begin_disabled(true)),
    //         RenderMode::Editable => None,
    //     };
    //     if ui.button(label) {
    //         if matches!(mode, RenderMode::Editable) {
    //             actions.push(SpellbookUiAction::Unprepare(spell_id.clone()));
    //         }
    //     }
    //     rendered += 1;
    // }
    // for i in rendered..spellbook.max_prepared_spells() {
    //     render_empty_button(ui, &format!("Empty##{}", i));
    // }

    // // --- All Spells ---
    // ui.separator_with_text("All Spells");

    // if let Some(table) = table_with_columns!(ui, "Spells", "Level", "Spells", "Slots") {
    //     // group by level
    //     let mut spells_by_level: HashMap<u8, Vec<&SpellId>> = HashMap::new();
    //     let all_spells = spellbook.all_spells().clone();
    //     for spell_id in &all_spells {
    //         let spell = SpellsRegistry::get(spell_id).unwrap();
    //         spells_by_level
    //             .entry(spell.base_level())
    //             .or_default()
    //             .push(spell_id);
    //     }
    //     let max_level = spells_by_level.keys().max().cloned().unwrap_or(0);

    //     let slots = resources
    //         .get(&ResourceId::new("nat20_core", "resource.spell_slot"))
    //         .and_then(|r| match r {
    //             ResourceBudgetKind::Tiered(budgets) => Some(budgets),
    //             _ => panic!("Expected ResourceKind::Tiered for SPELL_SLOT"),
    //         })
    //         .unwrap();

    //     for level in 1..=max_level {
    //         // Level
    //         ui.table_next_column();
    //         ui.text(roman_numeral(level));

    //         // Spells
    //         ui.table_next_column();
    //         if let Some(spells) = spells_by_level.get(&level) {
    //             for spell_id in spells {
    //                 let label = spell_id.to_string();
    //                 let is_prepared = spellbook.is_spell_prepared(spell_id);

    //                 let prepared_style = is_prepared.then(|| {
    //                     ui.push_style_color(imgui::StyleColor::Button, SELECTED_BUTTON_COLOR)
    //                 });
    //                 let _disabled = match mode {
    //                     RenderMode::ReadOnly => Some(ui.begin_disabled(true)),
    //                     RenderMode::Editable => None,
    //                 };

    //                 if ui.button(label) {
    //                     if matches!(mode, RenderMode::Editable) {
    //                         actions.push(SpellbookUiAction::Prepare((*spell_id).clone()));
    //                     }
    //                 }

    //                 if let Some(s) = prepared_style {
    //                     s.pop();
    //                 }
    //                 ui.same_line();
    //             }
    //         }

    //         // Slots
    //         ui.table_next_column();
    //         if let Some(budget) = slots.get(&level) {
    //             ui.text(format!("{}/{}", budget.current_uses, budget.max_uses));
    //         } else {
    //             ui.text("0/0");
    //         }
    //     }
    //     table.end();
    // }

    actions
}

impl ImguiRenderableWithContext<&ResourceMap> for Spellbook {
    fn render_with_context(&self, ui: &imgui::Ui, resources: &ResourceMap) {
        // Read-only: render and ignore any clicks (they’re disabled anyway)
        let _ = render_spellbook_ui(ui, self, resources, RenderMode::ReadOnly);
    }
}

impl ImguiRenderableMutWithContext<&ResourceMap> for Spellbook {
    fn render_mut_with_context(&mut self, ui: &imgui::Ui, resources: &ResourceMap) {
        // Mutable: render, then apply the collected intents
        let actions = render_spellbook_ui(ui, self, resources, RenderMode::Editable);
        // for a in actions {
        //     match a {
        //         SpellbookUiAction::Prepare(id) => self.prepare_spell(&id),
        //         SpellbookUiAction::Unprepare(id) => self.unprepare_spell(&id),
        //     };
        // }
    }
}

pub fn render_effect(ui: &imgui::Ui, effect: &EffectInstance, all_effects: &EffectsMap) {
    
    let _root_id = ui.push_id(effect.effect_id.to_string());
    
    if effect.children.is_empty() {
        ui.text(effect.effect_id.to_string());
        return;
    }

    ui.tree_node_config(effect.effect_id.to_string()).flags(TreeNodeFlags::empty()).build(|| {
        for effect_id in &effect.children {
            if let Some(child_effect) = all_effects.get(effect_id) {
                render_effect(ui, child_effect, all_effects);
            }
        }
    });
}

impl ImguiRenderableWithContext<&TimeMode> for EffectsMap {
    fn render_with_context(&self, ui: &imgui::Ui, time_mode: &TimeMode) {
        let (permanent_effects, temporary_effects): (Vec<&EffectInstance>, Vec<&EffectInstance>) =
            self.values()
                .partition(|e| matches!(e.lifetime, EffectLifetime::Permanent));

        ui.separator_with_text("Conditions");
        if let Some(table) = table_with_columns!(ui, "Conditions", "Effect", "Source", "Duration") {
            for effect in &temporary_effects {
                if effect.parent.is_some() {
                    // Skip child effects; they will be rendered under their parent
                    continue;
                }

                // Effect ID column
                ui.table_next_column();
                render_effect(ui, effect, self);
                // Source column
                ui.table_next_column();
                ui.text(effect.source.to_string());
                // Duration column
                ui.table_next_column();
                effect.lifetime.render_with_context(ui, time_mode);
            }
            table.end();
        }

        ui.separator_with_text("Permanent Effects");
        if let Some(table) = table_with_columns!(ui, "Permanent Effects", "Effect", "Source") {
            for effect in &permanent_effects {
                // Effect ID column
                ui.table_next_column();
                render_effect(ui, effect, self);
                // Source column
                ui.table_next_column();
                ui.text(effect.source.to_string());
            }
            table.end();
        }
    }
}

impl ImguiRenderable for Vec<FeatId> {
    fn render(&self, ui: &imgui::Ui) {
        if let Some(table) = table_with_columns!(ui, "Feats", "Feat") {
            for feat in self {
                ui.table_next_column();
                ui.text(feat.to_string());
                if ui.is_item_hovered() {
                    ui.tooltip(|| {
                        let feat = FeatsRegistry::get(feat).unwrap();
                        ui.text(feat.description());
                    });
                }
            }
            table.end();
        }
    }
}

impl ImguiRenderable for MonetaryValue {
    fn render(&self, ui: &imgui::Ui) {
        ui.text(self.to_string());
    }
}

fn render_item_misc(ui: &imgui::Ui, item: &Item) {
    ui.text_colored([0.7, 0.7, 0.7, 1.0], &item.description);
    // Fake right-aligned text for weight and value
    let text = format!("{} kg, {}", item.weight.get::<kilogram>(), item.value);
    let text_width = ui.calc_text_size(&text)[0];
    let available_width = ui.content_region_avail()[0];
    ui.set_cursor_pos([available_width - text_width, ui.cursor_pos()[1] + 10.0]);
    ui.text(text);
}

fn render_item_name(ui: &imgui::Ui, item: &Item) {
    let token = ui.push_style_color(imgui::StyleColor::Text, item_rarity_color(&item.rarity));
    ui.separator_with_text(&item.name);
    token.pop();
}

impl ImguiRenderableWithContext<(&World, Entity)> for Weapon {
    fn render_with_context(&self, ui: &imgui::Ui, context: (&World, Entity)) {
        let (world, entity) = context;
        render_item_name(ui, self.item());
        self.item().rarity.render(ui);
        let damage_roll = self.damage_roll(
            systems::helpers::get_component::<AbilityScoreMap>(world, entity).deref(),
            systems::helpers::get_component::<Loadout>(world, entity)
                .is_wielding_weapon_with_both_hands(self.kind()),
        );
        damage_roll.render(ui);
        ui.separator();
        ui.text(format!("{}", self.category()));
        for property in self.properties() {
            ui.text(format!("{}", property));
        }
        ui.separator();
        render_item_misc(ui, &self.item());
    }
}

impl ImguiRenderableWithContext<(&World, Entity)> for Armor {
    fn render_with_context(&self, ui: &imgui::Ui, context: (&World, Entity)) {
        let (world, entity) = context;
        render_item_name(ui, &self.item);
        self.item.rarity.render(ui);
        self.armor_type.render(ui);
        let armor_class = self
            .armor_class(systems::helpers::get_component::<AbilityScoreMap>(world, entity).deref());
        armor_class.render(ui);
        ui.same_line();
        ui.text("Armor Class");
        ui.separator();
        armor_class.dexterity_bonus.render(ui);
        if armor_class.dexterity_bonus != ArmorDexterityBonus::Unlimited {
            ui.same_line();
            TextSegment::new(format!("({} Armor)", self.armor_type), TextKind::Details).render(ui);
            ui.separator();
        }
        render_item_misc(ui, &self.item);
    }
}

impl ImguiRenderable for ArmorDexterityBonus {
    fn render(&self, ui: &imgui::Ui) {
        if self != &ArmorDexterityBonus::Unlimited {
            indent_text(ui, 1);
            let max_dexterity_bonus = self.max_bonus();
            if max_dexterity_bonus == 0 {
                TextSegments::new(vec![
                    ("No Armor Class bonus from", TextKind::Details),
                    ("Dexterity", TextKind::Ability),
                ])
                .render(ui);
            } else {
                TextSegments::new(vec![
                    ("Armor Class bonus from", TextKind::Details),
                    ("Dexterity", TextKind::Ability),
                    (
                        format!("is limited to {}", max_dexterity_bonus).as_str(),
                        TextKind::Details,
                    ),
                ])
                .render(ui);
            }
        }
    }
}

impl ImguiRenderable for ArmorType {
    fn render(&self, ui: &imgui::Ui) {
        let mut segments = vec![(self.to_string(), TextKind::Details)];
        if self != &ArmorType::Clothing {
            segments.push(("Armor".to_string(), TextKind::Details));
        }
        TextSegments::new(segments).render(ui);
    }
}

impl ImguiRenderable for ItemRarity {
    fn render(&self, ui: &imgui::Ui) {
        // TextSegment::new(self.to_string(), TextKind::Item(self.clone())).render(ui);
        TextSegment::new(self.to_string(), TextKind::Details).render(ui);
    }
}

impl ImguiRenderable for ArmorClass {
    fn render(&self, ui: &imgui::Ui) {
        ui.text(format!("{}", self.total()));
        if ui.is_item_hovered() {
            ui.tooltip(|| {
                ui.text(format!("Total AC: {}", self.total()));
                TextSegments::new(vec![
                    (format!("{}", self.base.0), TextKind::Normal),
                    (format!("({})", self.base.1), TextKind::Details),
                ])
                .render(ui);
                self.dexterity_bonus.render(ui);
                if !self.modifiers.is_empty() {
                    self.modifiers
                        .render_with_context(ui, ModifierSetRenderMode::List(1));
                }
            });
        }
    }
}

impl ImguiRenderable for DamageRoll {
    fn render(&self, ui: &imgui::Ui) {
        let min_max_rolls = self.min_max_rolls();
        let min_damage = min_max_rolls
            .iter()
            .map(|(min_roll, _, _)| min_roll)
            .sum::<i32>();
        let max_damage = min_max_rolls
            .iter()
            .map(|(_, max_roll, _)| max_roll)
            .sum::<i32>();
        ui.text(format!("{}-{} Damage", min_damage, max_damage));

        let mut damage_components = vec![self.primary.clone()];
        damage_components.extend(self.bonus.clone());

        for component in damage_components {
            indent_text(ui, 1);
            TextSegment::new(
                &component.to_string(),
                TextKind::Damage(component.damage_type),
            )
            .render(ui);
        }
    }
}

impl ImguiRenderable for DamageComponentResult {
    fn render(&self, ui: &imgui::Ui) {
        TextSegments::new(vec![
            (
                &format!("{} {}", self.result.subtotal, self.damage_type),
                TextKind::Damage(self.damage_type),
            ),
            (
                &format!(
                    "({} ({}d{})",
                    self.result.rolls.iter().sum::<u32>(),
                    self.result.rolls.len(),
                    self.result.die_size as u32,
                ),
                TextKind::Details,
            ),
        ])
        .render(ui);
        if !self.result.modifiers.is_empty() {
            ui.same_line();
            self.result
                .modifiers
                .render_with_context(ui, ModifierSetRenderMode::Line);
        }
        ui.same_line();
        TextSegment::new(")", TextKind::Details).render(ui);
    }
}

impl ImguiRenderable for DamageRollResult {
    fn render(&self, ui: &imgui::Ui) {
        for (i, component) in self.components.iter().enumerate() {
            if i > 0 {
                ui.same_line();
                ui.text("+");
                ui.same_line();
            }
            component.render(ui);
        }
        ui.same_line();
        ui.text(format!("= {}", self.total));
    }
}

impl ImguiRenderable for D20CheckResult {
    fn render(&self, ui: &imgui::Ui) {
        let mut segments = vec![
            (self.selected_roll.to_string(), TextKind::Normal),
            ("(1d20)".to_string(), TextKind::Details),
        ];
        if self.advantage_tracker.roll_mode() != RollMode::Normal {
            segments.push((
                format!(
                    " ({}, {}, {:?})",
                    self.rolls[0],
                    self.rolls[1],
                    self.advantage_tracker.roll_mode()
                ),
                TextKind::Details,
            ));
        }
        match self.outcome {
            Some(D20CheckOutcome::CriticalSuccess) => {
                segments.push(("(Critical Success!)".to_string(), TextKind::Normal));
            },
            Some(D20CheckOutcome::CriticalFailure) => {
                segments.push(("(Critical Failure!)".to_string(), TextKind::Normal));
            }
            _ => { /* No outcome to render */ }
        }
        TextSegments::new(segments).render(ui);
        if !self.modifier_breakdown.is_empty() {
            ui.same_line();
            self.modifier_breakdown
                .render_with_context(ui, ModifierSetRenderMode::Line);
        }
        ui.same_line();
        ui.text(format!("= {}", self.total()));
    }
}

impl ImguiRenderable for AttackRollResult {
    fn render(&self, ui: &imgui::Ui) {
        self.roll_result.render(ui);
    }
}

impl ImguiRenderable for DamageComponentMitigation {
    fn render(&self, ui: &imgui::Ui) {
        let text_kind = TextKind::Damage(self.damage_type);

        if self.original.subtotal == self.after_mods {
            // No mitigation applied
            TextSegment::new(
                &format!("{} {}", self.original.subtotal, self.damage_type),
                text_kind,
            )
            .render(ui);
            return;
        }

        let mut amount = self.original.subtotal.to_string();
        for modifier in &self.modifiers {
            let explanation = match modifier.operation {
                MitigationOperation::FlatReduction(_) => format!("{}", modifier.source),
                _ => format!("{:?}", modifier.operation),
            };
            amount = format!("({} {} ({}))", amount, modifier.operation, explanation);
        }
        TextSegments::new(vec![
            (
                format!("{} {}", self.after_mods, self.damage_type),
                text_kind,
            ),
            (amount, TextKind::Details),
        ])
        .render(ui);
    }
}

impl ImguiRenderableWithContext<(&World, u8)> for ActionResult {
    fn render_with_context(&self, ui: &imgui::Ui, (world, indent_level): (&World, u8)) {
        let target_name = match &self.target {
            TargetInstance::Entity(entity) => {
                let character_name = systems::helpers::get_component::<Name>(world, *entity);
                character_name.as_str().to_string()
            }
            TargetInstance::Point(point) => todo!(),
        };

        match &self.kind {
            ActionKindResult::Standard(action_outcome) => {
                if action_outcome.is_empty() {
                    ui.same_line();
                    TextSegment::new(
                        "with no effect",
                        TextKind::Normal,
                    ).render(ui);
                    return;
                }

                if let Some(damage) = &action_outcome.damage {
                    let no_damage_text = match damage.resolution {
                        ActionConditionResolution::Unconditional => "took no damage",
                        ActionConditionResolution::AttackRoll { .. } => "was not hit",
                        ActionConditionResolution::SavingThrow { .. } => "took no damage",
                    };

                    ui.group(|| {
                        damage.damage_taken.render_with_context(
                            ui,
                            (&target_name, indent_level + 1, no_damage_text, None),
                        );
                        damage.new_life_state.render_with_context(
                            ui,
                            (
                                &target_name,
                                Some(self.performer.name().as_str()),
                                indent_level + 1,
                            ),
                        );
                    });

                    if ui.is_item_hovered() {
                        ui.tooltip(|| {
                            match damage.resolution {
                                ActionConditionResolution::Unconditional => {
                                    (&damage.damage_roll, &damage.damage_taken)
                                        .render(ui);
                                }

                                ActionConditionResolution::AttackRoll {
                                    ref attack_roll,
                                    ref armor_class,
                                } => {
                                    TextSegment::new(
                                        format!("{}'s", target_name),
                                        TextKind::Target,
                                    )
                                    .render(ui);
                                    ui.same_line();
                                    ui.text("Armor Class:");
                                    ui.same_line();
                                    armor_class.render(ui);

                                    ui.text("");
                                    ui.text("Attack Roll:");
                                    ui.same_line();
                                    attack_roll.render(ui);

                                    if let Some(damage_taken) = &damage.damage_taken {
                                        ui.text("");
                                        ui.text("Damage Roll:");
                                        ui.same_line();
                                        damage.damage_roll.as_ref().unwrap().render(ui);

                                        ui.text("");
                                        ui.text("Damage Taken:");
                                        ui.same_line();
                                        damage_taken.render(ui);
                                    } else {
                                        ui.text(format!("Attack did not hit. Attack roll ({}) was less than Armor Class ({})", 
                                            attack_roll.roll_result.total(), armor_class.total()));
                                    }
                                }

                                ActionConditionResolution::SavingThrow {
                                    ref saving_throw_dc,
                                    ref saving_throw_result,
                                } => {
                                    ui.text("Saving Throw DC:");
                                    ui.same_line();
                                    saving_throw_dc.render(ui);

                                    ui.text("");
                                    ui.text("Saving Throw:");
                                    ui.same_line();
                                    saving_throw_result.render(ui);

                                    ui.same_line();
                                    let label = if saving_throw_result.is_success(saving_throw_dc) {
                                        "(Success)" } else {
                                            "(Failure)"
                                        };
                                    TextSegment::new(label, TextKind::Details)
                                        .render(ui);
                                    
                                    ui.text("");
                                    (&damage.damage_roll, &damage.damage_taken)
                                        .render(ui);
                                }
                            }
                        });
                    }
                }

                if let Some(healing) = &action_outcome.healing {
                    ui.group(|| {
                        TextSegments::new(vec![
                            (target_name.as_str(), TextKind::Target),
                            ("was healed for", TextKind::Normal),
                            (&format!("{} HP", healing.healing.subtotal), TextKind::Healing),
                        ])
                        .with_indent(indent_level + 1)
                        .render(ui);
                        healing.new_life_state.render_with_context(
                            ui,
                            (
                                &target_name,
                                Some(self.performer.name().as_str()),
                                indent_level + 1,
                            ),
                        );
                    });

                    if ui.is_item_hovered() {
                        ui.tooltip(|| {
                            ui.text("Healing:");
                            ui.same_line();
                            TextSegment::new(
                                &format!("{} HP", healing.healing),
                                TextKind::Healing,
                            ).render(ui);
                        });
                    }
                }

                if let Some(effect) = &action_outcome.effect {
                    if !effect.applied {
                        ui.same_line();
                        TextSegments::new(vec![
                            (target_name.as_str(), TextKind::Target),
                            ("was unaffected by", TextKind::Normal),
                            (&effect.effect.to_string(), TextKind::Effect),
                        ]).with_indent(indent_level + 1).render(ui);
                        return;
                    }

                    TextSegments::new(vec![
                        (target_name.as_str(), TextKind::Target),
                        ("gained effect", TextKind::Normal),
                        (&effect.effect.to_string(), TextKind::Effect),
                    ])
                    .with_indent(indent_level + 1)
                    .render(ui);
                }
            }

            ActionKindResult::Utility => todo!(),

            ActionKindResult::Composite { actions } => todo!(),

            ActionKindResult::Custom {} => todo!(),

            ActionKindResult::Reaction { result } => match result {
                ReactionResult::ModifyEvent { modification } => {
                    // TODO: No idea how to render this yet
                    return;
                }

                ReactionResult::CancelEvent {
                    event,
                    resources_refunded,
                } => {
                    // ui.same_line();
                    TextSegment::new("\tcancelling", TextKind::Normal).render(ui);
                    ui.same_line();
                    render_event_description(ui, event, world);
                }

                ReactionResult::NoEffect => {
                    ui.same_line();
                    TextSegment::new("with no effect", TextKind::Normal).render(ui)
                }
            },
        }
    }
}

impl ImguiRenderableWithContext<(&str, u8, &Option<AttackRollResult>)>
    for DamageComponentMitigation
{
    fn render_with_context(&self, ui: &imgui::Ui, context: (&str, u8, &Option<AttackRollResult>)) {
        let (target_name, indent_level, attack_roll) = context;

        let mut segments = vec![
            (target_name.to_string(), TextKind::Target),
            ("was hit for".to_string(), TextKind::Normal),
            (
                format!("{} {} damage", self.after_mods, self.damage_type),
                TextKind::Damage(self.damage_type),
            ),
        ];
        if let Some(attack_roll) = attack_roll {
            if matches!(attack_roll.roll_result.outcome, Some(D20CheckOutcome::CriticalSuccess)) {
                segments.push(("(Critical Hit!)".to_string(), TextKind::Details));
            }
        }
        TextSegments::new(segments)
            .with_indent(indent_level)
            .render(ui);
    }
}

impl ImguiRenderableWithContext<(&str, u8, &str, Option<AttackRollResult>)>
    for Option<DamageMitigationResult>
{
    fn render_with_context(
        &self,
        ui: &imgui::Ui,
        context: (&str, u8, &str, Option<AttackRollResult>),
    ) {
        let (target_name, indent_level, no_damage_text, attack_roll) = context;
        ui.group(|| match self {
            // Some damage was taken
            Some(result) => {
                for component in &result.components {
                    component.render_with_context(ui, (target_name, indent_level, &attack_roll));
                }
            }
            // No damage was taken
            None => {
                let mut segments = vec![
                    (target_name.to_string(), TextKind::Target),
                    (no_damage_text.to_string(), TextKind::Normal),
                ];
                if let Some(attack_roll) = attack_roll {
                    if matches!(attack_roll.roll_result.outcome, Some(D20CheckOutcome::CriticalFailure)) {
                        segments.push(("(Critical Miss!)".to_string(), TextKind::Details));
                    }
                }
                TextSegments::new(segments)
                    .with_indent(indent_level)
                    .render(ui);
            }
        });
    }
}

impl ImguiRenderable for DamageMitigationResult {
    fn render(&self, ui: &imgui::Ui) {
        for (i, component) in self.components.iter().enumerate() {
            if i > 0 {
                ui.same_line();
                ui.text("+");
                ui.same_line();
            }
            component.render(ui);
        }
        ui.same_line();
        ui.text(format!("= {}", self.total));
    }
}

pub fn new_life_state_text(
    entity: &str,
    new_state: &LifeState,
    actor: Option<&str>,
) -> Vec<(String, TextKind)> {
    let entity_component = (entity.to_string(), TextKind::Target);
    let actor_component = actor.map(|a| (a.to_string(), TextKind::Actor));

    match new_state {
        LifeState::Normal => {
            if let Some(actor_component) = actor_component {
                return vec![
                    entity_component,
                    ("was revived by".to_string(), TextKind::Normal),
                    actor_component,
                ];
            } else {
                return vec![
                    entity_component,
                    ("was revived".to_string(), TextKind::Normal),
                ];
            }
        }

        LifeState::Unconscious(_) => {
            if let Some(actor_component) = actor_component {
                return vec![
                    entity_component,
                    ("was knocked unconscious by".to_string(), TextKind::Normal),
                    actor_component,
                ];
            } else {
                return vec![
                    entity_component,
                    ("fell unconscious".to_string(), TextKind::Normal),
                ];
            }
        }

        LifeState::Stable => {
            if let Some(actor_component) = actor_component {
                return vec![
                    entity_component,
                    ("was stabilized by".to_string(), TextKind::Normal),
                    actor_component,
                ];
            } else {
                return vec![
                    entity_component,
                    ("was stabilized".to_string(), TextKind::Normal),
                ];
            }
        }

        LifeState::Dead => {
            if let Some(actor_component) = actor_component {
                return vec![
                    entity_component,
                    ("was killed by".to_string(), TextKind::Normal),
                    actor_component,
                ];
            } else {
                return vec![entity_component, ("died".to_string(), TextKind::Normal)];
            }
        }
    }
}

impl ImguiRenderableWithContext<(&str, Option<&str>, u8)> for Option<LifeState> {
    // This is used to render a LifeState which is being transitioned to
    fn render_with_context(&self, ui: &imgui::Ui, context: (&str, Option<&str>, u8)) {
        let (entity, actor, indent_level) = context;
        if let Some(life_state) = self {
            TextSegments::new(new_life_state_text(entity, life_state, actor))
                .with_indent(indent_level)
                .render(ui);
        }
    }
}

impl ImguiRenderable for (&Option<DamageRollResult>, &Option<DamageMitigationResult>) {
    fn render(&self, ui: &imgui::Ui) {
        let (damage_roll, damage_taken) = self;

        let Some(damage_roll) = damage_roll else {
            ui.text("No damage roll.");
            return;
        };

        ui.text("Damage Roll:");
        ui.same_line();
        damage_roll.render(ui);

        if let Some(damage_taken) = damage_taken {
            ui.text("");
            ui.text("Damage Taken:");
            ui.same_line();
            damage_taken.render(ui);
        } else {
            ui.text("No damage taken.");
        }
    }
}

impl ImguiRenderable for D20CheckDC<SavingThrowKind> {
    fn render(&self, ui: &imgui::Ui) {
        self.dc.render_with_context(ui, ModifierSetRenderMode::Line);
        ui.same_line();
        TextSegments::new(vec![
            (format!("({})", self.key), TextKind::Ability),
            (format!("= {}", self.dc.total()), TextKind::Normal),
        ])
        .render(ui);
    }
}

impl ImguiRenderable for D20CheckDC<Skill> {
    fn render(&self, ui: &imgui::Ui) {
        self.dc.render_with_context(ui, ModifierSetRenderMode::Line);
        ui.same_line();
        TextSegments::new(vec![
            (format!("({})", self.key), TextKind::Skill),
            (format!("= {}", self.dc.total()), TextKind::Normal),
        ])
        .render(ui);
    }
}

impl ImguiRenderable for D20CheckDCKind {
    fn render(&self, ui: &imgui::Ui) {
        match self {
            D20CheckDCKind::SavingThrow(dc) => dc.render(ui),
            D20CheckDCKind::Skill(dc) => dc.render(ui),
            D20CheckDCKind::AttackRoll(target, armor_class) => {
                armor_class.render(ui);
            }
        }
    }
}

impl ImguiRenderable for D20ResultKind {
    fn render(&self, ui: &imgui::Ui) {
        match self {
            // D20ResultKind::SavingThrow { kind, result } => {
            //     TextSegments::new(vec![
            //         ("Saving Throw:".to_string(), TextKind::Normal),
            //         (kind.to_string(), TextKind::Ability),
            //     ])
            //     .render(ui);
            //     indent_text(ui, 1);
            //     result.render(ui);
            // }
            // D20ResultKind::Skill { skill, result } => {
            //     TextSegments::new(vec![
            //         ("Skill Check:".to_string(), TextKind::Normal),
            //         (skill.to_string(), TextKind::Skill),
            //     ])
            //     .render(ui);
            //     indent_text(ui, 1);
            //     result.render(ui);
            // }
            D20ResultKind::SavingThrow { result, .. } | D20ResultKind::Skill { result, .. } => {
                result.render(ui);
            }
            D20ResultKind::AttackRoll { result } => {
                TextSegment::new("Attack Roll:", TextKind::Normal).render(ui);
                indent_text(ui, 1);
                result.render(ui);
            }
        }
    }
}

impl ImguiRenderable for (SpeciesId, Option<SubspeciesId>) {
    fn render(&self, ui: &imgui::Ui) {
        let (species, subspecies) = self;
        let text = if subspecies.is_some() {
            subspecies.as_ref().unwrap().to_string()
        } else {
            species.to_string()
        };
        TextSegment::new(text, TextKind::Details).render(ui);
    }
}

impl ImguiRenderable for DamageResistances {
    fn render(&self, ui: &imgui::Ui) {
        ui.separator_with_text("Restistances");

        if self.is_empty() {
            ui.text("None");
            return;
        }

        if let Some(table) = table_with_columns!(ui, "Resistances", "Type", "Effect") {
            for (damage_type, resistances) in self.effects.iter() {
                if resistances.is_empty() {
                    continue; // Skip empty resistances
                }

                // TODO: Multiple resistances for the same damage type?
                let effective_resistance = self.effective_resistance(*damage_type).unwrap();

                ui.table_next_column();
                TextSegment::new(damage_type.to_string(), TextKind::Damage(*damage_type))
                    .render(ui);

                ui.table_next_column();

                effective_resistance.render(ui);
            }
            table.end();
        }
    }
}

impl ImguiRenderable for DamageMitigationEffect {
    fn render(&self, ui: &imgui::Ui) {
        ui.text(format!("{:?}", self.operation));
        if ui.is_item_hovered() {
            ui.tooltip(|| {
                TextSegment::new(format!("{}", self.source), TextKind::Details).render(ui);
            });
        }
    }
}

impl ImguiRenderableWithContext<&TimeMode> for EffectLifetime {
    fn render_with_context(&self, ui: &imgui::Ui, time_mode: &TimeMode) {
        match self {
            EffectLifetime::TurnBoundary { remaining, duration, .. } => {
                remaining.render_with_context(ui, time_mode);
                ui.same_line();
                TextSegment::new("/", TextKind::Details).render(ui);
                ui.same_line();
                duration.render_with_context(ui, time_mode);
            }
            // TODO: Does it make sense to render the other durations?
            _ => {}
        }
    }
}

impl ImguiRenderableWithContext<&TimeMode> for TimeDuration {
    fn render_with_context(&self, ui: &imgui::Ui, time_mode: &TimeMode) {
        match time_mode {
            TimeMode::RealTime => {
                let seconds = self.as_seconds();
                render_seconds(ui, &seconds);
            }

            TimeMode::TurnBased { .. } => {
                let turns = self.as_turns();
                render_turns(ui, &turns);
            }
            
            TimeMode::Paused => {
                let turns = self.as_turns();
                render_turns(ui, &turns);
            }
        }
    }
}

fn render_seconds(ui: &imgui::Ui, seconds: &f32) {
    let seconds = *seconds as u32;
    let minutes = seconds / 60;
    let remaining_seconds = seconds % 60;
    let mut segments = Vec::new();
    if minutes > 0 {
        segments.push((
            format!("{} minutes", minutes),
            TextKind::Details,
        ))
    } 
    if seconds > 0 {
        segments.push((
            format!("{} seconds", remaining_seconds),
            TextKind::Details,
        ))
    }
    TextSegments::new(segments).render(ui);
}

fn render_turns(ui: &imgui::Ui, turns: &u32) {
    TextSegment::new(
        format!("{} turns", turns),
        TextKind::Details,
    )
    .render(ui);
}


impl ImguiRenderable for CreatureSize {
    fn render(&self, ui: &imgui::Ui) {
        TextSegment::new(self.to_string(), TextKind::Details).render(ui);
    }
}

impl ImguiRenderable for CreatureType {
    fn render(&self, ui: &imgui::Ui) {
        TextSegment::new(self.to_string(), TextKind::Details).render(ui);
    }
}

impl ImguiRenderableWithContext<&World> for Vec<Entity> {
    fn render_with_context(&self, ui: &imgui::Ui, world: &World) {
        if self.len() == 1 {
            ui.same_line();
            TextSegment::new(
                systems::helpers::get_component::<Name>(world, self[0]).as_str(),
                TextKind::Target,
            )
            .render(ui);
        } else if self.len() > 1 {
            // Hashset has random order, so we can't just convert the vec to a set
            // since this will cause the order to change on each render
            let mut rendered_targets = HashSet::new();
            for action_target in self.iter() {
                if rendered_targets.contains(action_target) {
                    continue;
                }
                indent_text(ui, 1);
                TextSegment::new(
                    systems::helpers::get_component_clone::<Name>(world, *action_target)
                        .to_string(),
                    TextKind::Target,
                )
                .render(ui);
                rendered_targets.insert(action_target);
            }
        }
    }
}

impl ImguiRenderableWithContext<(&World, Entity, &ActionContext)> for ActionKind {
    fn render_with_context(&self, ui: &imgui::Ui, context: (&World, Entity, &ActionContext)) {
        let (world, entity, action_context) = context;
        match self {
            ActionKind::Standard { payload, .. } => {
                if let Some(damage) = &payload.damage() {
                    damage(world, entity, action_context).render(ui);
                }

                if let Some(healing) = &payload.healing() {
                    // TODO: More info? Modifiers?
                    let healing = healing(world, entity, action_context);
                    TextSegment::new(
                        format!("{}-{} Healing", healing.min_roll(), healing.max_roll()),
                        TextKind::Healing,
                    )
                    .render(ui);
                }

                if let Some(effect) = &payload.effect() {
                    TextSegment::new(format!("{}", effect.effect_id), TextKind::Effect).render(ui);
                }
            }

            ActionKind::Composite { actions } => {
                for action in actions {
                    action.render_with_context(ui, (world, entity, action_context));
                }
            }

            _ => {}
        }
    }
}

impl ImguiRenderable for ResourceAmountMap {
    fn render(&self, ui: &imgui::Ui) {
        if self.is_empty() {
            ui.text("No cost");
            return;
        }

        for (resource, amount) in self.iter() {
            let amount_text = match amount {
                ResourceAmount::Flat(amount) => amount.to_string(),
                ResourceAmount::Tiered { tier, amount } => format!("{} Level {}", amount, tier),
            };
            ui.text(format!("{} {}", amount_text, resource));
        }
    }
}

// TODO: Pretty janky 'type' here
impl ImguiRenderableWithContext<(&World, Entity)>
    for (&ActionId, &ActionContext, &ResourceAmountMap)
{
    fn render_with_context(&self, ui: &imgui::Ui, (world, entity): (&World, Entity)) {
        let (action_id, context, cost) = self;
        let action = systems::actions::get_action(action_id).unwrap();

        ui.child_window("Action Tooltip")
            .size([400.0, 0.0])
            .child_flags(imgui::ChildFlags::ALWAYS_AUTO_RESIZE | imgui::ChildFlags::AUTO_RESIZE_Y)
            .build(|| {
                ui.separator_with_text(&action_id.to_string());

                let spell_id: SpellId = action.id.clone().into();
                let spell = SpellsRegistry::get(&spell_id);
                if let Some(spell) = spell {
                    if spell.base_level() > 0 {
                        TextSegment::new(format!("Level {} {} Spell", spell.base_level(), spell.school()), TextKind::Details).render(ui);
                    } else {
                        TextSegment::new(format!("{} Cantrip", spell.school()), TextKind::Details).render(ui);
                    }
                }

                action
                    .kind
                    .render_with_context(ui, (world, entity, context));
                
                ui.separator();

                let targeting = (action.targeting)(world, entity, context);
                targeting.range.render(ui);
                targeting.kind.render(ui);

                match action.kind() {
                    ActionKind::Standard { condition, .. } => match condition {
                        ActionCondition::AttackRoll { .. } => {
                            TextSegment::new("Attack Roll", TextKind::Details).render(ui);
                        }
                        ActionCondition::SavingThrow { saving_throw, .. } => {
                            let saving_throw = saving_throw(world, entity, &context);
                            let saving_throw_ability = match saving_throw.key {
                                SavingThrowKind::Ability(ability) => ability,
                                _ => todo!()
                            };
                            TextSegments::new(vec![
                                (saving_throw_ability.to_string(), TextKind::Ability),
                                ("Saving Throw".to_string(), TextKind::Details),
                            ])
                            .render(ui);
                        }
                        _ => {}
                    },
                    _ => {}
                }

                if let Some(spell) = spell {
                    ui.separator();
                    for flag in spell.flags() {
                        TextSegment::new(format!("{:?}", flag), TextKind::Details).render(ui);
                    }
                }

                ui.separator();

                cost.render(ui);

                ui.separator();

                TextSegment::new(action.description.as_str(), TextKind::Details)
                    .wrap_text(true)
                    .render(ui);
            });
    }
}

pub static SPEED_COLOR: [f32; 4] = [0.3, 0.7, 0.8, 1.0];
pub static SPEED_COLOR_BG: [f32; 4] = [0.15, 0.35, 0.4, 1.0];

impl ImguiRenderable for Speed {
    fn render(&self, ui: &imgui::Ui) {
        let total_speed = self.total_speed();
        let text = if self.moved_this_turn().value == 0.0 {
            format!("Speed: {} meters", total_speed.value)
        } else {
            format!(
                "Speed: {} / {} meters",
                total_speed.value - self.moved_this_turn().value,
                total_speed.value
            )
        };
        TextSegment::new(text, TextKind::Details).render(ui);
    }
}

impl ImguiRenderable for TargetingRange {
    fn render(&self, ui: &imgui::Ui) {
        let range_text = if self.max().get::<meter>() == 0.0 {
            return;
        } else if *self == *MELEE_RANGE_DEFAULT {
            "Melee".to_string()
        } else if self.max() == self.normal() {
            format!("{:.1} meters", self.max().get::<meter>())
        } else {
            format!(
                "{:.1} ({:.1}) meters",
                self.normal().get::<meter>(),
                self.max().get::<meter>()
            )
        };
        TextSegments::new(vec![
            ("Range:".to_string(), TextKind::Details),
            (range_text, TextKind::Details),
        ])
        .render(ui);
    }
}

impl ImguiRenderable for TargetingKind {
    fn render(&self, ui: &imgui::Ui) {
        let text = match self {
            TargetingKind::SelfTarget => "Self Target".to_string(),
            TargetingKind::Single => "Single Target".to_string(),
            TargetingKind::Multiple { max_targets, allow_duplicates } => if *allow_duplicates {
                format!("{} Targets", max_targets)
            } else {
                format!("{} Targets (Unique)", max_targets)
            },
            TargetingKind::Area { shape, .. } => match shape {
                AreaShape::Arc { angle, length } => {
                    format!(
                        "AoE: Arc\n\tAngle: {:.0}°, Length: {:.1} meters",
                        angle.get::<degree>(),
                        length.get::<meter>()
                    )
                }
                AreaShape::Sphere { radius } => {
                    format!("AoE: Sphere\n\tRadius: {:.1} meters", radius.get::<meter>())
                }
                AreaShape::Cube { side_length } => {
                    format!(
                        "AoE: Cube\n\tSide Length: {:.1} meters",
                        side_length.get::<meter>()
                    )
                }
                AreaShape::Cylinder { radius, height } => {
                    format!(
                        "AoE: Cylinder\n\tRadius: {:.1} meters, Height: {:.1} meters",
                        radius.get::<meter>(),
                        height.get::<meter>()
                    )
                }
                AreaShape::Line { length, width } => {
                    format!(
                        "AoE: Line\n\tLength: {:.1} meters, Width: {:.1} meters",
                        length.get::<meter>(),
                        width.get::<meter>()
                    )
                }
            },
        };
        TextSegment::new(text, TextKind::Details).render(ui);
    }
}
