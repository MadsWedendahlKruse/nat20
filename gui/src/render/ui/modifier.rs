use std::{collections::BTreeMap, fmt};

use nat20_core::components::modifier::{
    FlatModifierMap, ModifierKind, ModifierKindResult, ModifierMap, ModifierResult, ModifierSource,
    ModifierValue,
};

use crate::render::ui::{
    text::{TextKind, TextSegments},
    utils::{ImguiRenderable, ImguiRenderableWithContext, signed_value},
};

pub enum ModifierRenderMode {
    Line,
    List(bool), // bool is whether to indent the list or not
    Hoverable,
}

pub fn modifiers_to_string<T>(map: &BTreeMap<ModifierSource, T>) -> String
where
    T: fmt::Display,
{
    let mut string = String::new();
    for (i, (source, modifier)) in map.iter().enumerate() {
        let modifier_string = modifier.to_string();

        if i > 0 {
            string.push(' ');
            if !modifier_string.starts_with('-') {
                string.push_str("+ ");
            }
        }

        let source_string = match source {
            ModifierSource::Base => "".to_string(),
            other => format!(" ({})", other),
        };

        string.push_str(&format!("{}{}", modifier, source_string));
    }
    string
}

pub fn modifier_to_string(modifier: &ModifierKind) -> String {
    let mut string = String::new();
    match modifier {
        ModifierKind::Flat(flat) => string.push_str(&signed_value(flat)),
        ModifierKind::Dice(dice_set) => string.push_str(&format!("+{}", dice_set)),
        ModifierKind::Composite(modifiers) => {
            string.push_str(&composite_to_string(modifiers, |modifier| {
                modifier_to_string(modifier)
            }));
        }
    };
    string
}

pub fn modifier_result_to_string(modifier_result: &ModifierKindResult) -> String {
    let mut string = String::new();
    match modifier_result {
        ModifierKindResult::Flat(flat) => string.push_str(&signed_value(flat)),
        ModifierKindResult::Dice(dice_set_result) => {
            string.push_str(&format!("+{}", dice_set_result))
        }
        ModifierKindResult::Composite(modifier_kind_results) => {
            string.push_str(&composite_to_string(modifier_kind_results, |modifier| {
                modifier_result_to_string(modifier)
            }));
        }
    };
    string
}

fn composite_to_string<T>(modifiers: &Vec<T>, mapper: fn(&T) -> String) -> String {
    modifiers
        .iter()
        .map(mapper)
        .collect::<Vec<String>>()
        .join(" ")
}

impl ImguiRenderableWithContext<ModifierRenderMode> for ModifierMap {
    fn render_with_context(&self, ui: &imgui::Ui, mode: ModifierRenderMode) {
        render_map(
            ui,
            &self,
            mode,
            |modifier| modifier_to_string(modifier),
            self.range().to_string(),
        );
    }
}

impl ImguiRenderableWithContext<ModifierRenderMode> for FlatModifierMap {
    fn render_with_context(&self, ui: &imgui::Ui, mode: ModifierRenderMode) {
        render_map(
            ui,
            &self,
            mode,
            |modifier| signed_value(modifier),
            signed_value(&self.total()),
        );
    }
}

impl ImguiRenderableWithContext<ModifierRenderMode> for ModifierResult {
    fn render_with_context(&self, ui: &imgui::Ui, mode: ModifierRenderMode) {
        render_map(
            ui,
            &self,
            mode,
            |modifier| modifier_result_to_string(modifier),
            self.total().to_string(),
        );
    }
}

fn render_map<T>(
    ui: &imgui::Ui,
    map: &BTreeMap<ModifierSource, T>,
    mode: ModifierRenderMode,
    value_mapper: fn(&T) -> String,
    total: String,
) where
    T: fmt::Display + ModifierValue,
{
    if map.is_empty() {
        return;
    }

    match mode {
        ModifierRenderMode::Line => {
            let mut segments = Vec::new();
            for (source, value) in map.iter() {
                if value.is_zero() {
                    continue;
                }
                segments.push((value_mapper(value), TextKind::Normal));
                segments.push((format!("({})", source), TextKind::Details));
            }
            TextSegments::new(segments).render(ui);
        }

        ModifierRenderMode::List(indent) => {
            for (source, value) in map.iter() {
                if value.is_zero() {
                    continue;
                }

                if indent {
                    ui.indent();
                }

                TextSegments::new(vec![
                    (value_mapper(value), TextKind::Normal),
                    (source.to_string(), TextKind::Details),
                ])
                .render(ui);

                if indent {
                    ui.unindent();
                }
            }
        }

        ModifierRenderMode::Hoverable => {
            ui.text(&total);

            if ui.is_item_hovered() {
                ui.tooltip(|| {
                    ui.text(format!("Total: {}", total));
                    render_map(ui, map, ModifierRenderMode::List(true), value_mapper, total);
                });
            }
        }
    }
}
