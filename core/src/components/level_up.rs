use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    sync::LazyLock,
};

use hecs::{Entity, World};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    components::{
        ability::Ability,
        class::ClassAndSubclass,
        id::{
            ActionId, BackgroundId, ClassId, EffectId, FeatId, ItemId, SpeciesId, SpellId,
            SubclassId, SubspeciesId,
        },
        modifier::ModifierSource,
        resource::ResourceMap,
        skill::Skill,
        spells::spellbook::{SpellSource, Spellbook},
    },
    registry::registry::{
        BackgroundsRegistry, ClassesRegistry, FeatsRegistry, SpeciesRegistry, SpellsRegistry,
    },
    systems::{self},
};

static ABILITY_SCORE_POINT_COST: LazyLock<HashMap<u8, u8>> = LazyLock::new(|| {
    HashMap::from([
        (8, 0),
        (9, 1),
        (10, 2),
        (11, 3),
        (12, 4),
        (13, 5),
        (14, 7),
        (15, 9),
    ])
});

static ABILITY_SCORE_POINTS: u8 = 27;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ChoiceItem {
    Action(ActionId),
    Spell(SpellId, SpellSource),
    Background(BackgroundId),
    Class(ClassId),
    Subclass(SubclassId),
    Effect(EffectId),
    Feat(FeatId),
    Species(SpeciesId),
    Subspecies(SubspeciesId),
    Equipment {
        items: Vec<(u8, ItemId)>,
        money: String, // e.g., "10 GP"
    },
}

impl ChoiceItem {
    pub fn id(&self) -> &'static str {
        match self {
            ChoiceItem::Action(_) => "choice.action",
            ChoiceItem::Spell(_, _) => "choice.spell",
            ChoiceItem::Background(_) => "choice.background",
            ChoiceItem::Class(_) => "choice.class",
            ChoiceItem::Subclass(_) => "choice.subclass",
            ChoiceItem::Effect(_) => "choice.effect",
            ChoiceItem::Feat(_) => "choice.feat",
            ChoiceItem::Species(_) => "choice.species",
            ChoiceItem::Subspecies(_) => "choice.subspecies",
            ChoiceItem::Equipment { .. } => "choice.equipment",
        }
    }

    /// Primarily used for visualization purposes.
    pub fn priority(&self) -> u8 {
        match self {
            ChoiceItem::Species(_) => 0,
            ChoiceItem::Subspecies(_) => 1,
            ChoiceItem::Background(_) => 2,
            ChoiceItem::Class(_) => 3,
            ChoiceItem::Subclass(_) => 4,
            ChoiceItem::Equipment { .. } => 5,
            ChoiceItem::Action(_) => 6,
            ChoiceItem::Spell(_, _) => 6,
            ChoiceItem::Effect(_) => 7,
            ChoiceItem::Feat(_) => 8,
        }
    }
}

impl std::fmt::Display for ChoiceItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ChoiceItem::Effect(id) => write!(f, "{}", id),
            ChoiceItem::Feat(id) => write!(f, "{}", id),
            ChoiceItem::Action(id) => write!(f, "{}", id),
            ChoiceItem::Spell(id, _) => write!(f, "{}", id),
            ChoiceItem::Background(id) => write!(f, "{}", id),
            ChoiceItem::Class(id) => write!(f, "{}", id),
            ChoiceItem::Subclass(id) => write!(f, "{}", id),
            ChoiceItem::Species(id) => write!(f, "{}", id),
            ChoiceItem::Subspecies(id) => write!(f, "{}", id),
            ChoiceItem::Equipment { items, money } => {
                let mut lines: Vec<String> = items
                    .iter()
                    .map(|(count, id)| format!("{} x {}", count, id.to_string()))
                    .collect();
                if !money.is_empty() {
                    lines.push(money.to_string());
                }
                write!(f, "{}", lines.join("\n"))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema)]
pub struct ChoiceSpec {
    pub id: String,
    pub label: String,
    pub options: Vec<ChoiceItem>,
    pub picks: u8,
    pub allow_duplicates: bool,
}

impl ChoiceSpec {
    pub fn single(label: impl Into<String>, options: Vec<ChoiceItem>) -> Self {
        if options.is_empty() {
            panic!("ChoiceSpec must have at least one option");
        }

        Self {
            // Assuming all the options have the same type we can just infer the
            // id from the first option.
            id: options.first().map(|item| item.id().to_string()).unwrap(),
            label: label.into(),
            options,
            picks: 1,
            allow_duplicates: false,
        }
    }

    pub fn with_id(&mut self, id: impl Into<String>) -> &mut Self {
        self.id = id.into();
        self
    }

    pub fn priority(&self) -> u8 {
        self.options
            .iter()
            .map(ChoiceItem::priority)
            .max()
            .unwrap_or(0)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum LevelUpPrompt {
    Choice(ChoiceSpec),
    AbilityScores(HashMap<u8, u8>, u8),
    AbilityScoreImprovement {
        feat: FeatId,
        budget: u8,
        abilities: HashSet<Ability>,
        max_score: u8,
    },
    SkillProficiency(HashSet<Skill>, u8, ModifierSource),
    ReplaceSpells {
        spells: Vec<SpellId>,
        source: SpellSource,
        replacements: u8,
    },
}

impl LevelUpPrompt {
    pub fn priority(&self) -> u8 {
        match self {
            LevelUpPrompt::Choice(spec) => spec.priority(),
            LevelUpPrompt::AbilityScores(_, _) => 4,
            LevelUpPrompt::SkillProficiency(_, _, _) => 5,
            LevelUpPrompt::ReplaceSpells { .. } => 7,
            LevelUpPrompt::AbilityScoreImprovement { .. } => 8,
        }
    }

    pub fn ability_scores() -> Self {
        LevelUpPrompt::AbilityScores(ABILITY_SCORE_POINT_COST.clone(), ABILITY_SCORE_POINTS)
    }

    pub fn background() -> Self {
        LevelUpPrompt::Choice(ChoiceSpec::single(
            "Background",
            BackgroundsRegistry::keys()
                .cloned()
                .map(ChoiceItem::Background)
                .collect(),
        ))
    }

    pub fn class() -> Self {
        LevelUpPrompt::Choice(ChoiceSpec::single(
            "Class",
            ClassesRegistry::keys()
                .cloned()
                .map(ChoiceItem::Class)
                .collect(),
        ))
    }

    pub fn feats(world: &World, entity: Entity) -> Self {
        // Feats need special handling since they can have prerequisites and
        // can (or can't) be repeatable.
        LevelUpPrompt::Choice(ChoiceSpec::single(
            "Feat",
            FeatsRegistry::keys()
                .filter_map(|feat_id| {
                    systems::feats::can_acquire_feat(world, entity, feat_id).ok()?;
                    // TODO: Bit of a dirty hack to remove fighting styles from the list of feats.
                    if feat_id.id().starts_with("feat.fighting_style.") {
                        return None;
                    }
                    Some(ChoiceItem::Feat(feat_id.clone()))
                })
                .collect(),
        ))
    }

    pub fn species() -> Self {
        LevelUpPrompt::Choice(ChoiceSpec::single(
            "Species",
            SpeciesRegistry::keys()
                .cloned()
                .map(ChoiceItem::Species)
                .collect(),
        ))
    }

    pub fn subspecies(species: &SpeciesId) -> Self {
        let subspecies =
            SpeciesRegistry::get(species).map_or_else(HashSet::new, |r| r.subspecies.clone());
        LevelUpPrompt::Choice(ChoiceSpec::single(
            "Subspecies",
            subspecies.into_iter().map(ChoiceItem::Subspecies).collect(),
        ))
    }

    pub fn spells(
        world: &World,
        entity: Entity,
        class_and_subclass: &ClassAndSubclass,
        cantrips: bool,
        number_of_spells: u8,
        max_spell_level: u8,
    ) -> Self {
        let source = SpellSource::Class(class_and_subclass.clone());
        if let Some(class) = ClassesRegistry::get(&class_and_subclass.class)
            && let Some(spellcasting_rules) = class.spellcasting_rules(&class_and_subclass.subclass)
        {
            let spellbook = systems::helpers::get_component::<Spellbook>(world, entity);
            let resources = systems::helpers::get_component::<ResourceMap>(world, entity);
            let known_spells = spellbook
                .known_spells_for_class(class_and_subclass, &resources)
                .unwrap();

            let options = spellcasting_rules
                .spell_list
                .iter()
                .filter_map(|spell_id| {
                    if known_spells.contains(spell_id) {
                        return None;
                    }
                    let spell = SpellsRegistry::get(spell_id)?;
                    if spell.base_level() > max_spell_level {
                        return None;
                    } else if cantrips && spell.is_cantrip() {
                        Some(ChoiceItem::Spell(spell_id.clone(), source.clone()))
                    } else if !cantrips && !spell.is_cantrip() {
                        Some(ChoiceItem::Spell(spell_id.clone(), source.clone()))
                    } else {
                        None
                    }
                })
                .collect();
            let (id, label) = if cantrips {
                (
                    "choice.cantrips".to_string(),
                    "Cantrip Selection".to_string(),
                )
            } else {
                ("choice.spells".to_string(), "Spell Selection".to_string())
            };

            LevelUpPrompt::Choice(ChoiceSpec {
                id,
                label,
                options,
                picks: number_of_spells,
                allow_duplicates: false,
            })
        } else {
            panic!(
                "Class {:?} does not have spellcasting capabilities",
                class_and_subclass
            );
        }
    }

    pub fn spell_replacement(
        world: &World,
        entity: Entity,
        class_and_subclass: &ClassAndSubclass,
        replacements: u8,
    ) -> Self {
        let spellbook = systems::helpers::get_component::<Spellbook>(world, entity);
        let resources = systems::helpers::get_component::<ResourceMap>(world, entity);
        let known_spells = spellbook
            .known_spells_for_class(class_and_subclass, &resources)
            .unwrap();

        LevelUpPrompt::ReplaceSpells {
            spells: known_spells.into_iter().collect(),
            source: SpellSource::Class(class_and_subclass.clone()),
            replacements,
        }
    }
}

impl Display for LevelUpPrompt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LevelUpPrompt::Choice(spec) => write!(f, "{}", spec.label),
            LevelUpPrompt::AbilityScores(_, _) => write!(f, "Ability Scores"),
            LevelUpPrompt::AbilityScoreImprovement { .. } => {
                write!(f, "Ability Score Improvement")
            }
            LevelUpPrompt::SkillProficiency(_, _, _) => {
                write!(f, "Skill Proficiency")
            }
            LevelUpPrompt::ReplaceSpells { .. } => {
                write!(f, "Replace Spells")
            }
        }
    }
}
