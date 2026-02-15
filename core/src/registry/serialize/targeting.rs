use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    str::FromStr,
    sync::{Arc, LazyLock},
};

use hecs::{Entity, World};
use serde::{Deserialize, Serialize};
use uom::si::length::meter;

use crate::{
    components::{
        actions::{
            action::{ActionContext, TargetingFunction},
            targeting::{AreaShape, EntityFilter, TargetingContext, TargetingKind, TargetingRange},
        },
        health::life_state::LifeState,
        items::equipment::loadout::Loadout,
        species::CreatureType,
    },
    registry::serialize::{
        parser::{Evaluable, EvaluationError, IntExpression, Parser},
        quantity::LengthExpressionDefinition,
        variables::{PARSER_VARIABLES, VariableMap},
    },
    systems,
};

static TARGETING_DEFAULTS: LazyLock<HashMap<String, Arc<TargetingFunction>>> =
    LazyLock::new(|| {
        HashMap::from([
            (
                "weapon_targeting".to_string(),
                Arc::new(
                    |world: &World, entity: Entity, action_context: &ActionContext| {
                        if let ActionContext::Weapon { slot } = action_context {
                            TargetingContext {
                                kind: TargetingKind::Single,
                                range: systems::helpers::get_component::<Loadout>(world, entity)
                                    .weapon_in_hand(slot)
                                    .unwrap()
                                    .range()
                                    .clone(),
                                require_line_of_sight: true,
                                allowed_targets: vec![EntityFilter::not_dead()],
                            }
                        } else {
                            panic!("Action context must be Weapon");
                        }
                    },
                ) as Arc<TargetingFunction>,
            ),
            (
                "self".to_string(),
                Arc::new(|_: &World, _: Entity, _: &ActionContext| TargetingContext::self_target())
                    as Arc<TargetingFunction>,
            ),
        ])
    });

// TODO: Should this live somewhere else?
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct IntExpressionDefinition {
    pub raw: String,
    pub expression: IntExpression,
}

impl FromStr for IntExpressionDefinition {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut parser = Parser::new(input);
        let expression = parser.parse_int_expression()?;
        Ok(IntExpressionDefinition {
            raw: input.to_string(),
            expression,
        })
    }
}

impl TryFrom<String> for IntExpressionDefinition {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<IntExpressionDefinition> for String {
    fn from(spec: IntExpressionDefinition) -> Self {
        spec.raw
    }
}

impl Display for IntExpressionDefinition {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}", self.raw)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AreaShapeDefinition {
    Sphere {
        radius: LengthExpressionDefinition,
    },
    // Arc {
    //     angle: IntExpressionDefinition,
    //     length: LengthExpressionDefinition,
    // },
    Cube {
        side: LengthExpressionDefinition,
    },
    Cylinder {
        radius: LengthExpressionDefinition,
        height: LengthExpressionDefinition,
    },
    Line {
        length: LengthExpressionDefinition,
        width: LengthExpressionDefinition,
    },
}

impl Evaluable for AreaShapeDefinition {
    type Output = AreaShape;

    fn evaluate(
        &self,
        world: &World,
        entity: Entity,
        context: &ActionContext,
        variables: &VariableMap,
    ) -> Result<AreaShape, EvaluationError> {
        match self {
            // AreaShapeDefinition::Arc {
            //     angle,
            //     length: radius,
            // } => Ok(AreaShape::Arc {
            //     angle: angle.evaluate(world, entity, context, variables)?,
            //     length: radius.evaluate(world, entity, context, variables)?,
            // }),
            AreaShapeDefinition::Sphere { radius } => Ok(AreaShape::Sphere {
                radius: radius.evaluate(world, entity, context, variables)?,
            }),
            AreaShapeDefinition::Cube { side } => Ok(AreaShape::Cube {
                side_length: side.evaluate(world, entity, context, variables)?,
            }),
            AreaShapeDefinition::Cylinder { radius, height } => Ok(AreaShape::Cylinder {
                radius: radius.evaluate(world, entity, context, variables)?,
                height: height.evaluate(world, entity, context, variables)?,
            }),
            AreaShapeDefinition::Line { length, width } => Ok(AreaShape::Line {
                length: length.evaluate(world, entity, context, variables)?,
                width: width.evaluate(world, entity, context, variables)?,
            }),
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TargetingKindDefinition {
    SelfTarget,
    Single,
    Multiple {
        max_targets: IntExpressionDefinition,
        allow_duplicates: bool,
    },
    Area {
        shape: AreaShapeDefinition,
        fixed_on_actor: bool,
    },
}

impl Evaluable for TargetingKindDefinition {
    type Output = TargetingKind;

    fn evaluate(
        &self,
        world: &World,
        entity: Entity,
        context: &ActionContext,
        variables: &VariableMap,
    ) -> Result<TargetingKind, EvaluationError> {
        match self {
            TargetingKindDefinition::SelfTarget => Ok(TargetingKind::SelfTarget),
            TargetingKindDefinition::Single => Ok(TargetingKind::Single),
            TargetingKindDefinition::Multiple {
                max_targets,
                allow_duplicates,
            } => {
                let value = max_targets
                    .expression
                    .evaluate(world, entity, context, variables)?;

                // Clamp to a sensible range and cast to u8
                let max_targets_i32 = value.max(0).min(u8::MAX as i32);
                Ok(TargetingKind::Multiple {
                    max_targets: max_targets_i32 as u8,
                    allow_duplicates: *allow_duplicates,
                })
            }
            TargetingKindDefinition::Area {
                shape,
                fixed_on_actor,
            } => Ok(TargetingKind::Area {
                shape: shape.evaluate(world, entity, context, variables)?,
                fixed_on_actor: *fixed_on_actor,
            }),
        }
    }
}

// TODO: This looks pretty weird in JSON format, see "Hold Person"
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum EntityFilterDefinition {
    Tags {
        tag: EntityFilterTag,
    },
    LifeState {
        life_states: HashSet<LifeState>,
        #[serde(default)]
        invert: bool,
    },
    CreatureType {
        creature_types: HashSet<CreatureType>,
        #[serde(default)]
        invert: bool,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EntityFilterTag {
    All,
    Characters,
    Monsters,
}

impl EntityFilterDefinition {
    pub fn evaluate(&self) -> EntityFilter {
        match self {
            EntityFilterDefinition::Tags { tag } => match tag {
                EntityFilterTag::All => EntityFilter::All,
                EntityFilterTag::Characters => EntityFilter::Characters,
                EntityFilterTag::Monsters => EntityFilter::Monsters,
            },
            EntityFilterDefinition::LifeState {
                life_states,
                invert,
            } => {
                if *invert {
                    EntityFilter::NotLifeStates(life_states.clone())
                } else {
                    EntityFilter::LifeStates(life_states.clone())
                }
            }
            EntityFilterDefinition::CreatureType {
                creature_types,
                invert,
            } => {
                if *invert {
                    EntityFilter::NotCreatureTypes(creature_types.clone())
                } else {
                    EntityFilter::CreatureTypes(creature_types.clone())
                }
            }
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct TargetingContextDefinition {
    pub kind: TargetingKindDefinition,
    pub range: LengthExpressionDefinition,
    pub require_line_of_sight: bool,
    pub allowed_targets: Vec<EntityFilterDefinition>,
}

impl TargetingContextDefinition {
    pub fn function(&self) -> Arc<TargetingFunction> {
        Arc::new({
            let definition = self.clone();
            move |world: &World, entity: Entity, action_context: &ActionContext| {
                let range = definition
                    .range
                    .evaluate(world, entity, action_context, &PARSER_VARIABLES)
                    .unwrap();
                let range = TargetingRange::new::<meter>(range.get::<meter>());

                let kind = definition
                    .kind
                    .evaluate(world, entity, action_context, &PARSER_VARIABLES)
                    .unwrap();

                TargetingContext {
                    kind,
                    range,
                    require_line_of_sight: definition.require_line_of_sight,
                    allowed_targets: definition
                        .allowed_targets
                        .iter()
                        .map(|entity_filter| entity_filter.evaluate())
                        .collect(),
                }
            }
        })
    }
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum TargetingDefinition {
    Default(String),
    Custom(TargetingContextDefinition),
}

impl TargetingDefinition {
    pub fn function(&self) -> Arc<TargetingFunction> {
        match self {
            TargetingDefinition::Default(name) => {
                if let Some(function) = TARGETING_DEFAULTS.get(name) {
                    function.clone()
                } else {
                    panic!("Unknown TargetingDefinition default: {}", name);
                }
            }
            TargetingDefinition::Custom(definition) => definition.function(),
        }
    }
}
