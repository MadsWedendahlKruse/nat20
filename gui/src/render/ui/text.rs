use std::{borrow::Cow, fmt::Display};

use nat20_core::components::{damage::DamageType, items::item::ItemRarity};

use crate::render::ui::utils::ImguiRenderable;

pub fn damage_type_color(damage_type: &DamageType) -> [f32; 4] {
    match damage_type {
        DamageType::Bludgeoning | DamageType::Piercing | DamageType::Slashing => {
            [0.8, 0.8, 0.8, 1.0]
        }
        DamageType::Fire => [1.0, 0.5, 0.0, 1.0],
        DamageType::Cold => [0.0, 1.0, 1.0, 1.0],
        DamageType::Lightning => [0.25, 0.25, 1.0, 1.0],
        DamageType::Acid => [0.0, 1.0, 0.0, 1.0],
        DamageType::Poison => [0.5, 0.9, 0.0, 1.0],
        DamageType::Force => [0.9, 0.0, 0.0, 1.0],
        DamageType::Necrotic => [0.25, 0.7, 0.25, 1.0],
        DamageType::Psychic => [1.0, 0.5, 1.0, 1.0],
        DamageType::Radiant => [1.0, 0.9, 0.0, 1.0],
        DamageType::Thunder => [0.5, 0.0, 1.0, 1.0],
    }
}

pub fn item_rarity_color(rarity: &ItemRarity) -> [f32; 4] {
    match rarity {
        ItemRarity::Common => [1.0, 1.0, 1.0, 1.0],
        ItemRarity::Uncommon => [0.12, 1.0, 0.0, 1.0],
        ItemRarity::Rare => [0.2, 0.4, 1.0, 1.0],
        ItemRarity::VeryRare => [0.64, 0.21, 0.93, 1.0],
        ItemRarity::Legendary => [1.0, 0.5, 0.0, 1.0],
    }
}

pub fn indent_text(ui: &imgui::Ui, indent_level: u8) {
    for _ in 0..indent_level {
        ui.text("\t");
        ui.same_line();
    }
}

#[derive(Debug, Clone)]
pub enum TextKind {
    Actor,
    Target,
    Action,
    Normal,
    Damage(DamageType),
    Healing,
    Effect,
    Details,
    Ability,
    Skill,
    Item(ItemRarity),
    // General purpose text colors
    Green,
    Red,
}

impl TextKind {
    pub fn color(&self) -> [f32; 4] {
        match self {
            TextKind::Actor => [0.8, 1.0, 0.8, 1.0],
            TextKind::Target => [1.0, 0.8, 0.8, 1.0],
            TextKind::Action => [1.0, 1.0, 0.8, 1.0],
            TextKind::Normal => [1.0, 1.0, 1.0, 1.0],
            TextKind::Damage(damage_type) => damage_type_color(damage_type),
            TextKind::Healing => [0.5, 1.0, 0.5, 1.0],
            TextKind::Effect => [1.0, 0.8, 0.5, 1.0],
            TextKind::Details => [0.75, 0.75, 0.75, 1.0],
            TextKind::Ability => [0.75, 0.5, 1.0, 1.0],
            TextKind::Skill => [0.5, 0.75, 1.0, 1.0],
            TextKind::Item(item_rarity) => item_rarity_color(item_rarity),
            TextKind::Green => [0.0, 1.0, 0.0, 1.0],
            TextKind::Red => [1.0, 0.0, 0.0, 1.0],
        }
    }
}

pub struct TextSegment<'a> {
    pub text: Cow<'a, str>,
    pub kind: TextKind,
    pub wrap_text: bool,
}

impl<'a> TextSegment<'a> {
    pub fn new<T: Into<Cow<'a, str>>>(text: T, kind: TextKind) -> Self {
        Self {
            text: text.into(),
            kind,
            wrap_text: false,
        }
    }

    pub fn color(&self) -> [f32; 4] {
        self.kind.color()
    }

    pub fn wrap_text(mut self, wrap: bool) -> Self {
        self.wrap_text = wrap;
        self
    }
}

impl ImguiRenderable for TextSegment<'_> {
    fn render(&self, ui: &imgui::Ui) {
        if self.wrap_text {
            let color_token = ui.push_style_color(imgui::StyleColor::Text, self.color());
            ui.text_wrapped(&self.text);
            color_token.end();
        } else {
            ui.text_colored(self.color(), &self.text);
        }
    }
}

pub struct TextSegments<'a> {
    segments: Vec<TextSegment<'a>>,
    indent_level: u8,
}

impl<'a> TextSegments<'a> {
    pub fn new<T, I>(segments: I) -> Self
    where
        T: Display,
        I: IntoIterator<Item = (T, TextKind)>,
    {
        Self {
            segments: segments
                .into_iter()
                .map(|(text, kind)| TextSegment::new(text.to_string(), kind))
                .collect(),
            indent_level: 0,
        }
    }

    pub fn with_indent(mut self, indent_level: u8) -> Self {
        self.indent_level = indent_level;
        self
    }
}

impl ImguiRenderable for TextSegments<'_> {
    fn render(&self, ui: &imgui::Ui) {
        if self.segments.is_empty() {
            return;
        }
        ui.group(|| {
            indent_text(ui, self.indent_level);
            for (i, segment) in self.segments.iter().enumerate() {
                if i > 0 {
                    ui.same_line();
                }
                segment.render(ui);
            }
        });
    }
}
