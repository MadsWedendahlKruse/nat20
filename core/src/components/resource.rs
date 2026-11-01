use std::{
    collections::{BTreeMap, HashMap, HashSet, btree_map, hash_map::Entry},
    fmt::{Debug, Display},
    hash::Hash,
    ops::{Add, AddAssign, Deref, Sub, SubAssign},
    str::FromStr,
};

use schemars::JsonSchema;
use serde::{Deserialize, Deserializer, Serialize};
use serde_with::{DisplayFromStr, serde_as};
use strum::EnumDiscriminants;

use crate::{
    components::{
        id::{IdProvider, ResourceId},
        modifier::ModifierSource,
    },
    registry::{
        registry::rules,
        serialize::schema::{impl_schema_via, impl_string_schema},
    },
    systems::time::RestKind,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub enum RechargeRule {
    Turn,
    Rest(RestKind),
    Daily,
    Never,
}

impl FromStr for RechargeRule {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "turn" => Ok(RechargeRule::Turn),
            "short_rest" => Ok(RechargeRule::Rest(RestKind::Short)),
            "long_rest" => Ok(RechargeRule::Rest(RestKind::Long)),
            "daily" => Ok(RechargeRule::Daily),
            "never" => Ok(RechargeRule::Never),
            _ => Err(format!("Invalid RechargeRule: {}", s)),
        }
    }
}

impl TryFrom<String> for RechargeRule {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<RechargeRule> for String {
    fn from(val: RechargeRule) -> Self {
        match val {
            RechargeRule::Turn => "turn".to_string(),
            RechargeRule::Rest(RestKind::Short) => "short_rest".to_string(),
            RechargeRule::Rest(RestKind::Long) => "long_rest".to_string(),
            RechargeRule::Daily => "daily".to_string(),
            RechargeRule::Never => "never".to_string(),
        }
    }
}

impl_string_schema!(
    RechargeRule,
    "RechargeRule",
    "description": "When the resource recharges.",
    "enum": ["turn", "short_rest", "long_rest", "daily", "never"]
);

impl RechargeRule {
    /// Checks if this recharge rule is recharged by another rule.
    /// For example, `ShortRest` is also recharged by `LongRest`.
    ///
    /// `Never` is (as the name suggests) never recharged.
    pub fn is_recharged_by(&self, other: &RechargeRule) -> bool {
        if self == &RechargeRule::Never {
            return false;
        }
        if let (RechargeRule::Rest(self_rest), RechargeRule::Rest(other_rest)) = (self, other) {
            return other_rest >= self_rest;
        }
        *other >= *self
    }
}

impl Display for RechargeRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize, JsonSchema)]
pub struct ResourceBudget {
    current_uses: u8,
    max_uses: BTreeMap<ModifierSource, u8>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResourceBudgetError {
    InsufficientResources { needed: u8, available: u8 },
    CurrentUsesAboveMax { current_uses: u8, max_uses: u8 },
}

impl ResourceBudget {
    pub fn new(
        current_uses: u8,
        max_uses: u8,
        source: ModifierSource,
    ) -> Result<Self, ResourceBudgetError> {
        if current_uses > max_uses {
            return Err(ResourceBudgetError::CurrentUsesAboveMax {
                current_uses,
                max_uses,
            });
        }
        Ok(Self {
            current_uses,
            max_uses: BTreeMap::from_iter([(source, max_uses)]),
        })
    }

    pub fn with_max_uses(
        max_uses: u8,
        source: ModifierSource,
    ) -> Result<Self, ResourceBudgetError> {
        Self::new(max_uses, max_uses, source)
    }

    pub fn can_afford(&self, amount: u8) -> bool {
        self.current_uses >= amount
    }

    pub fn current_uses(&self) -> u8 {
        self.current_uses
    }

    pub fn max_uses_total(&self) -> u8 {
        self.max_uses.values().sum::<u8>()
    }

    pub fn max_uses(&self) -> &BTreeMap<ModifierSource, u8> {
        &self.max_uses
    }

    pub fn spend(&mut self, amount: u8) -> Result<(), ResourceBudgetError> {
        if self.current_uses < amount {
            return Err(ResourceBudgetError::InsufficientResources {
                needed: amount,
                available: self.current_uses,
            });
        }
        self.current_uses -= amount;
        Ok(())
    }

    pub fn is_empty(&self) -> bool {
        self.max_uses_total() == 0
    }

    pub fn remove_uses(&mut self, source: &ModifierSource) {
        self.max_uses.remove(source);
        if self.current_uses > self.max_uses_total() {
            self.current_uses = self.max_uses_total();
        }
    }

    pub fn merge(&mut self, other: &ResourceBudget, policy: &ResourceMergePolicy) {
        for (source, max_uses) in &other.max_uses {
            match self.max_uses.entry(source.clone()) {
                btree_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(*max_uses);
                }
                btree_map::Entry::Occupied(mut occupied_entry) => match policy.max_uses {
                    ResourceMergeOperation::Add => {
                        *occupied_entry.get_mut() += *max_uses;
                    }
                    ResourceMergeOperation::Replace => {
                        *occupied_entry.get_mut() = *max_uses;
                    }
                    ResourceMergeOperation::Skip => {}
                },
            };
        }

        match policy.current_uses {
            ResourceMergeOperation::Add => {
                self.current_uses += other.current_uses;
            }
            ResourceMergeOperation::Replace => {
                self.current_uses = other.current_uses;
            }
            ResourceMergeOperation::Skip => {}
        }

        if self.current_uses > self.max_uses_total() {
            self.current_uses = self.max_uses_total();
        }
    }

    pub fn recharge_full(&mut self) {
        self.current_uses = self.max_uses_total();
    }

    pub fn restore(&mut self, amount: u8) {
        self.current_uses += amount;
        if self.current_uses > self.max_uses_total() {
            self.current_uses = self.max_uses_total();
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ResourceMergeOperation {
    Add,
    Replace,
    Skip,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ResourceMergePolicy {
    pub current_uses: ResourceMergeOperation,
    pub max_uses: ResourceMergeOperation,
    /// Techinically only relevant for tiered resources, but it's easier to have
    /// everything in one struct
    pub tier: ResourceMergeOperation,
}

impl ResourceMergePolicy {
    pub fn add_all() -> Self {
        Self {
            current_uses: ResourceMergeOperation::Add,
            max_uses: ResourceMergeOperation::Add,
            tier: ResourceMergeOperation::Add,
        }
    }
}

#[serde_as]
#[derive(Debug, Clone, PartialEq, EnumDiscriminants, Serialize, Deserialize, JsonSchema)]
#[strum_discriminants(
    name(ResourceKind),
    derive(Serialize, Deserialize, JsonSchema),
    serde(rename_all = "snake_case")
)]
#[serde(untagged)]
pub enum ResourceBudgetKind {
    Flat(ResourceBudget),
    // An untagged enum is buffered before a variant is picked, and the buffer
    // keeps the map keys as strings, so the tier has to be parsed explicitly
    Tiered(
        #[serde_as(as = "BTreeMap<DisplayFromStr, _>")]
        #[schemars(with = "BTreeMap<u8, ResourceBudget>")]
        BTreeMap<u8, ResourceBudget>,
    ),
}

impl ResourceBudgetKind {
    pub fn kind(&self) -> ResourceKind {
        match self {
            ResourceBudgetKind::Flat(_) => ResourceKind::Flat,
            ResourceBudgetKind::Tiered(_) => ResourceKind::Tiered,
        }
    }

    pub fn recharge_full(&mut self) {
        match self {
            ResourceBudgetKind::Flat(budget) => budget.recharge_full(),
            ResourceBudgetKind::Tiered(budgets) => {
                for budget in budgets.values_mut() {
                    budget.recharge_full();
                }
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            ResourceBudgetKind::Flat(budget) => budget.is_empty(),
            ResourceBudgetKind::Tiered(budgets) => budgets.values().all(|b| b.is_empty()),
        }
    }

    pub fn can_afford(&self, cost: &ResourceAmount) -> bool {
        match (self, cost) {
            (ResourceBudgetKind::Flat(budget), ResourceAmount::Flat(amt)) => {
                budget.can_afford(*amt)
            }
            (ResourceBudgetKind::Tiered(budgets), ResourceAmount::Tiered { tier, amount }) => {
                if let Some(budget) = budgets.get(tier) {
                    budget.can_afford(*amount)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn spend(&mut self, cost: &ResourceAmount) -> Result<(), ResourceError> {
        match (&mut *self, cost) {
            (ResourceBudgetKind::Flat(budget), ResourceAmount::Flat(amt)) => {
                Ok(budget.spend(*amt)?)
            }
            (ResourceBudgetKind::Tiered(budgets), ResourceAmount::Tiered { tier, amount }) => {
                if let Some(budget) = budgets.get_mut(tier) {
                    Ok(budget.spend(*amount)?)
                } else {
                    Err(ResourceError::InvalidTier { tier: *tier })
                }
            }
            _ => Err(ResourceError::MistmatchKind {
                other: cost.kind(),
                own: self.kind(),
            }),
        }
    }

    pub fn restore(&mut self, cost: &ResourceAmount) -> Result<(), ResourceError> {
        match (&mut *self, cost) {
            (ResourceBudgetKind::Flat(budget), ResourceAmount::Flat(amt)) => {
                Ok(budget.restore(*amt))
            }
            (ResourceBudgetKind::Tiered(budgets), ResourceAmount::Tiered { tier, amount }) => {
                if let Some(budget) = budgets.get_mut(tier) {
                    Ok(budget.restore(*amount))
                } else {
                    Err(ResourceError::InvalidTier { tier: *tier })
                }
            }
            _ => Err(ResourceError::MistmatchKind {
                other: cost.kind(),
                own: self.kind(),
            }),
        }
    }

    pub fn remove_uses(&mut self, source: &ModifierSource) {
        match (&mut *self, source) {
            (ResourceBudgetKind::Flat(budget), source) => budget.remove_uses(source),
            (ResourceBudgetKind::Tiered(budgets), source) => {
                for (_, budget) in budgets.iter_mut() {
                    budget.remove_uses(source);
                }
            }
        }
    }

    pub fn merge(
        &mut self,
        other: ResourceBudgetKind,
        policy: &ResourceMergePolicy,
    ) -> Result<(), ResourceError> {
        match (&mut *self, &other) {
            (ResourceBudgetKind::Flat(budget), ResourceBudgetKind::Flat(other_budget)) => {
                Ok(budget.merge(&other_budget, policy))
            }
            (ResourceBudgetKind::Tiered(budgets), ResourceBudgetKind::Tiered(other_budgets)) => {
                for (tier, other_budget) in other_budgets {
                    match budgets.entry(*tier) {
                        btree_map::Entry::Occupied(mut occupied_entry) => {
                            match policy.tier {
                                ResourceMergeOperation::Add => {
                                    occupied_entry.get_mut().merge(&other_budget, policy);
                                }
                                ResourceMergeOperation::Replace => {
                                    occupied_entry.insert(other_budget.clone());
                                }
                                ResourceMergeOperation::Skip => {}
                            }
                            if occupied_entry.get().is_empty() {
                                occupied_entry.remove();
                            }
                        }
                        btree_map::Entry::Vacant(vacant_entry) => {
                            vacant_entry.insert(other_budget.clone());
                        }
                    };
                }
                Ok(())
            }
            _ => Err(ResourceError::MistmatchKind {
                other: other.kind(),
                own: self.kind(),
            }),
        }
    }

    // TODO: Only used for testing purposes
    pub fn max_uses(&self) -> Vec<ResourceAmount> {
        match self {
            ResourceBudgetKind::Flat(budget) => vec![ResourceAmount::Flat(budget.max_uses_total())],
            ResourceBudgetKind::Tiered(budgets) => budgets
                .iter()
                .map(|(tier, budget)| ResourceAmount::Tiered {
                    tier: *tier,
                    amount: budget.max_uses_total(),
                })
                .collect(),
        }
    }

    pub fn current_uses(&self) -> Vec<ResourceAmount> {
        match self {
            ResourceBudgetKind::Flat(budget) => vec![ResourceAmount::Flat(budget.current_uses)],
            ResourceBudgetKind::Tiered(budgets) => budgets
                .iter()
                .map(|(tier, budget)| ResourceAmount::Tiered {
                    tier: *tier,
                    amount: budget.current_uses,
                })
                .collect(),
        }
    }

    pub fn from_amount(amount: &ResourceAmount, source: ModifierSource) -> Self {
        match amount {
            ResourceAmount::Flat(max_uses) => {
                ResourceBudgetKind::Flat(ResourceBudget::with_max_uses(*max_uses, source).unwrap())
            }
            ResourceAmount::Tiered { tier, amount } => {
                let mut map = BTreeMap::new();
                map.insert(
                    *tier,
                    ResourceBudget::with_max_uses(*amount, source).unwrap(),
                );
                ResourceBudgetKind::Tiered(map)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResourceError {
    BudgetError(ResourceBudgetError),
    MistmatchKind {
        other: ResourceKind,
        own: ResourceKind,
    },
    InvalidResourceKind(String),
    InvalidTier {
        tier: u8,
    },
    MissingResource {
        id: ResourceId,
    },
    InsufficientResource {
        id: ResourceId,
        needed: ResourceAmount,
        available: ResourceAmount,
    },
    SpendDisabled {
        id: ResourceId,
    },
}

impl From<ResourceBudgetError> for ResourceError {
    fn from(err: ResourceBudgetError) -> Self {
        ResourceError::BudgetError(err)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct Resource {
    pub id: ResourceId,
    pub kind: ResourceKind,
    pub recharge: RechargeRule,
}

impl IdProvider for Resource {
    type Id = ResourceId;

    fn id(&self) -> &Self::Id {
        &self.id
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(into = "String")]
pub enum ResourceAmount {
    Flat(u8),
    Tiered { tier: u8, amount: u8 },
}

impl ResourceAmount {
    pub fn is_empty(&self) -> bool {
        match self {
            ResourceAmount::Flat(amt) => *amt == 0,
            ResourceAmount::Tiered { amount, .. } => *amount == 0,
        }
    }

    pub fn kind(&self) -> ResourceKind {
        match self {
            ResourceAmount::Flat(_) => ResourceKind::Flat,
            ResourceAmount::Tiered { .. } => ResourceKind::Tiered,
        }
    }
}

impl Add for ResourceAmount {
    type Output = ResourceAmount;

    fn add(self, other: ResourceAmount) -> ResourceAmount {
        match (self, other) {
            (ResourceAmount::Flat(a), ResourceAmount::Flat(b)) => ResourceAmount::Flat(a + b),
            (
                ResourceAmount::Tiered {
                    tier: t1,
                    amount: a,
                },
                ResourceAmount::Tiered {
                    tier: t2,
                    amount: b,
                },
            ) if t1 == t2 => ResourceAmount::Tiered {
                tier: t1,
                amount: a + b,
            },
            _ => panic!("Cannot add ResourceAmounts of different kinds or tiers"),
        }
    }
}

impl Sub for ResourceAmount {
    type Output = ResourceAmount;

    fn sub(self, other: ResourceAmount) -> ResourceAmount {
        match (self, other) {
            (ResourceAmount::Flat(a), ResourceAmount::Flat(b)) => ResourceAmount::Flat(a - b),
            (
                ResourceAmount::Tiered {
                    tier: t1,
                    amount: a,
                },
                ResourceAmount::Tiered {
                    tier: t2,
                    amount: b,
                },
            ) if t1 == t2 => ResourceAmount::Tiered {
                tier: t1,
                amount: a - b,
            },
            _ => panic!("Cannot subtract ResourceAmounts of different kinds or tiers"),
        }
    }
}

impl AddAssign for ResourceAmount {
    fn add_assign(&mut self, other: ResourceAmount) {
        *self = self.clone() + other;
    }
}

impl SubAssign for ResourceAmount {
    fn sub_assign(&mut self, other: ResourceAmount) {
        *self = self.clone() - other;
    }
}

impl PartialOrd for ResourceAmount {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (ResourceAmount::Flat(a), ResourceAmount::Flat(b)) => a.partial_cmp(b),
            (
                ResourceAmount::Tiered {
                    tier: t1,
                    amount: a,
                },
                ResourceAmount::Tiered {
                    tier: t2,
                    amount: b,
                },
            ) if t1 == t2 => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl FromStr for ResourceAmount {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split(':').collect();
        if parts.len() == 1 {
            let amount = parts[0]
                .parse::<u8>()
                .map_err(|_| format!("Invalid ResourceAmount format: {}", s))?;
            Ok(ResourceAmount::Flat(amount))
        } else if parts.len() == 2 {
            let tier = parts[0]
                .parse::<u8>()
                .map_err(|_| format!("Invalid ResourceAmount format: {}", s))?;
            let amount = parts[1]
                .parse::<u8>()
                .map_err(|_| format!("Invalid ResourceAmount format: {}", s))?;
            Ok(ResourceAmount::Tiered { tier, amount })
        } else {
            Err(format!("Invalid ResourceAmount format: {}", s))
        }
    }
}

// Manual deserialization to support both integer and string formats
impl<'de> Deserialize<'de> for ResourceAmount {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize, JsonSchema)]
        #[serde(untagged)]
        enum Helper {
            Int(u8),
            Str(String),
        }

        match Helper::deserialize(deserializer)? {
            Helper::Int(value) => Ok(ResourceAmount::Flat(value)),
            Helper::Str(s) => s.parse().map_err(serde::de::Error::custom),
        }
    }
}

impl From<ResourceAmount> for String {
    fn from(amount: ResourceAmount) -> Self {
        match amount {
            ResourceAmount::Flat(amt) => amt.to_string(),
            ResourceAmount::Tiered { tier, amount } => format!("{}:{}", tier, amount),
        }
    }
}

impl schemars::JsonSchema for ResourceAmount {
    fn schema_name() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Borrowed("ResourceAmount")
    }

    fn inline_schema() -> bool {
        true
    }

    fn json_schema(_: &mut schemars::SchemaGenerator) -> schemars::Schema {
        schemars::json_schema!({
            "description": "A flat amount (`1`) or a tiered amount `<tier>:<amount>` \
                 (e.g. `2:1` for one level-2 spell slot).",
            "anyOf": [
                { "type": "integer", "minimum": 0 },
                { "type": "string", "pattern": "^[0-9]+:[0-9]+$" }
            ]
        })
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ResourceAmountMap {
    map: HashMap<ResourceId, ResourceAmount>,
}

impl Default for ResourceAmountMap {
    fn default() -> Self {
        Self::new()
    }
}

impl ResourceAmountMap {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn insert(&mut self, resource: ResourceId, amount: ResourceAmount) {
        self.map.insert(resource, amount);
    }

    pub fn remove(&mut self, resource: &ResourceId) -> Option<ResourceAmount> {
        self.map.remove(resource)
    }

    pub fn replace_resource(
        &mut self,
        from: &ResourceId,
        to: &ResourceId,
        amount: &ResourceAmount,
    ) {
        if let Some(from_amount) = self.map.get(from) {
            if amount >= from_amount {
                self.map.remove(from);
            } else {
                self.map
                    .entry(from.clone())
                    .and_modify(|e| *e -= amount.clone());
            }
            self.map
                .entry(to.clone())
                .and_modify(|e| *e += amount.clone())
                .or_insert(amount.clone());
        }
    }
}

impl Deref for ResourceAmountMap {
    type Target = HashMap<ResourceId, ResourceAmount>;

    fn deref(&self) -> &Self::Target {
        &self.map
    }
}

impl FromIterator<(ResourceId, ResourceAmount)> for ResourceAmountMap {
    fn from_iter<T: IntoIterator<Item = (ResourceId, ResourceAmount)>>(iter: T) -> Self {
        let map = iter.into_iter().collect();
        Self { map }
    }
}

impl_schema_via!(ResourceAmountMap, HashMap<ResourceId, ResourceAmount>);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceMap {
    resources: HashMap<ResourceId, ResourceBudgetKind>,
    /// Some effects might prevent certain resources from being used
    disabled_resources: HashMap<ResourceId, HashSet<ModifierSource>>,
}

impl ResourceMap {
    pub fn new() -> Self {
        Self {
            resources: HashMap::new(),
            disabled_resources: HashMap::new(),
        }
    }

    // TODO: Can't this also be used to reduce max uses? Probbaly not intended
    pub fn add(
        &mut self,
        resource: ResourceId,
        budget: ResourceBudgetKind,
        policy: &ResourceMergePolicy,
    ) -> Result<(), ResourceError> {
        match self.resources.entry(resource) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().merge(budget, policy)?;
                if entry.get().is_empty() {
                    entry.remove();
                }
            }
            Entry::Vacant(entry) => {
                if !budget.is_empty() {
                    entry.insert(budget);
                }
            }
        }
        Ok(())
    }

    pub fn remove_uses(
        &mut self,
        resource: &ResourceId,
        source: &ModifierSource,
    ) -> Result<(), ResourceError> {
        if let Some(budget) = self.resources.get_mut(resource) {
            Ok(budget.remove_uses(source))
        } else {
            Err(ResourceError::InvalidResourceKind(format!(
                "Resource with id {} not found",
                resource
            )))
        }
    }

    pub fn remove(&mut self, id: &ResourceId) {
        self.resources.remove(id);
    }

    pub fn get(&self, id: &ResourceId) -> Option<&ResourceBudgetKind> {
        self.resources.get(id)
    }

    pub fn get_mut(&mut self, id: &ResourceId) -> Option<&mut ResourceBudgetKind> {
        self.resources.get_mut(id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&ResourceId, &ResourceBudgetKind)> {
        self.resources.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&ResourceId, &mut ResourceBudgetKind)> {
        self.resources.iter_mut()
    }

    pub fn can_afford(&self, id: &ResourceId, cost: &ResourceAmount) -> bool {
        if let Some(resource) = self.resources.get(id)
            && !self.disabled_resources.contains_key(id)
        {
            resource.can_afford(cost)
        } else {
            false
        }
    }

    pub fn can_afford_all(&self, cost: &ResourceAmountMap) -> Result<(), Vec<ResourceId>> {
        let mut lacking_resources = Vec::new();
        for (res_id, res_cost) in &cost.map {
            if !self.can_afford(res_id, res_cost) {
                lacking_resources.push(res_id.clone());
            }
        }

        if lacking_resources.is_empty() {
            Ok(())
        } else {
            Err(lacking_resources)
        }
    }

    pub fn spend(&mut self, id: &ResourceId, cost: &ResourceAmount) -> Result<(), ResourceError> {
        let Some(resource) = self.resources.get_mut(id) else {
            return Err(ResourceError::MissingResource { id: id.clone() });
        };

        if self.disabled_resources.contains_key(id) {
            return Err(ResourceError::SpendDisabled { id: id.clone() });
        }

        resource.spend(cost)
    }

    pub fn spend_all(&mut self, cost: &ResourceAmountMap) -> Result<(), Vec<ResourceError>> {
        match self.can_afford_all(cost) {
            Ok(()) => {
                let mut spend_errors = Vec::new();
                for (id, res_cost) in &cost.map {
                    if let Err(e) = self.spend(id, res_cost) {
                        spend_errors.push(e);
                    }
                }
                if spend_errors.is_empty() {
                    Ok(())
                } else {
                    Err(spend_errors)
                }
            }
            Err(lacking_resources) => {
                let errors = lacking_resources
                    .into_iter()
                    .map(|id| ResourceError::InsufficientResource {
                        id: id.clone(),
                        needed: cost.map.get(&id).unwrap().clone(),
                        available: self
                            .get(&id)
                            .unwrap()
                            .current_uses()
                            .first()
                            .unwrap()
                            .clone(),
                    })
                    .collect();
                Err(errors)
            }
        }
    }

    pub fn restore(
        &mut self,
        id: &ResourceId,
        restoration: &ResourceAmount,
    ) -> Result<(), ResourceError> {
        let Some(resource) = self.resources.get_mut(id) else {
            return Err(ResourceError::MissingResource { id: id.clone() });
        };

        resource.restore(restoration)
    }

    pub fn restore_all(&mut self, restoration: &ResourceAmountMap) -> Result<(), ResourceError> {
        for (id, res_restoration) in &restoration.map {
            self.restore(id, res_restoration)?;
        }

        Ok(())
    }

    pub fn is_resource_disabled(&self, id: &ResourceId) -> Option<&HashSet<ModifierSource>> {
        self.disabled_resources.get(id)
    }

    pub fn disable_resource(&mut self, id: &ResourceId, source: ModifierSource) {
        self.disabled_resources
            .entry(id.clone())
            .or_insert_with(HashSet::new)
            .insert(source);
    }

    pub fn enable_resource(&mut self, id: &ResourceId, source: &ModifierSource) {
        if let Some(sources) = self.disabled_resources.get_mut(id) {
            sources.remove(source);
            if sources.is_empty() {
                self.disabled_resources.remove(id);
            }
        }
    }
}

impl Default for ResourceMap {
    fn default() -> Self {
        let mut map = ResourceMap::new();
        map.resources = rules()
            .default_resources
            .map
            .iter()
            .map(|(id, amount)| {
                (
                    id.clone(),
                    ResourceBudgetKind::from_amount(amount, ModifierSource::Base),
                )
            })
            .collect();
        map
    }
}

#[cfg(test)]
mod tests {
    use rstest::rstest;

    use super::*;

    #[test]
    fn budget_new() {
        assert!(ResourceBudget::new(2, 2, ModifierSource::Base).is_ok());
        assert_eq!(
            ResourceBudget::new(2, 1, ModifierSource::Base),
            Err(ResourceBudgetError::CurrentUsesAboveMax {
                current_uses: 2,
                max_uses: 1
            })
        );
    }

    #[test]
    fn budget_remove_uses() {
        let mut budget = ResourceBudget::new(3, 3, ModifierSource::Base).unwrap();
        budget.remove_uses(&ModifierSource::Base);
        assert_eq!(budget.current_uses, 0);
        assert_eq!(budget.max_uses_total(), 0);
    }

    #[rstest]
    #[case(ResourceMergeOperation::Add, ResourceMergeOperation::Add, 3, 5)]
    #[case(ResourceMergeOperation::Add, ResourceMergeOperation::Skip, 2, 2)]
    #[case(ResourceMergeOperation::Skip, ResourceMergeOperation::Add, 2, 5)]
    #[case(ResourceMergeOperation::Skip, ResourceMergeOperation::Skip, 2, 2)]
    #[case(ResourceMergeOperation::Replace, ResourceMergeOperation::Replace, 1, 3)]
    #[case(ResourceMergeOperation::Replace, ResourceMergeOperation::Add, 1, 5)]
    #[case(ResourceMergeOperation::Add, ResourceMergeOperation::Replace, 3, 3)]
    fn budge_merge(
        #[case] current_policy: ResourceMergeOperation,
        #[case] max_policy: ResourceMergeOperation,
        #[case] current_uses: u8,
        #[case] max_uses: u8,
    ) {
        let policy = ResourceMergePolicy {
            current_uses: current_policy,
            max_uses: max_policy,
            tier: ResourceMergeOperation::Skip,
        };
        let mut budget1 = ResourceBudget::new(2, 2, ModifierSource::Base).unwrap();
        let mut budget2 = ResourceBudget::new(1, 3, ModifierSource::Base).unwrap();
        budget1.merge(&mut budget2, &policy);
        assert_eq!(budget1.current_uses, current_uses);
        assert_eq!(budget1.max_uses_total(), max_uses);
    }

    fn flat_resource(current: u8, max: u8) -> ResourceBudgetKind {
        ResourceBudgetKind::Flat(ResourceBudget::new(current, max, ModifierSource::Base).unwrap())
    }

    fn tiered_resource(tiers: &[(u8, u8, u8)]) -> ResourceBudgetKind {
        let mut map = BTreeMap::new();
        for (tier, current, max) in tiers {
            map.insert(
                *tier,
                ResourceBudget::new(*current, *max, ModifierSource::Base).unwrap(),
            );
        }
        ResourceBudgetKind::Tiered(map)
    }

    #[test]
    fn budget_kind_json_round_trip() {
        for res in [
            flat_resource(1, 3),
            tiered_resource(&[(1, 4, 4), (2, 0, 3), (10, 1, 1)]),
        ] {
            let json = serde_json::to_string(&res).unwrap();
            assert_eq!(
                serde_json::from_str::<ResourceBudgetKind>(&json).unwrap(),
                res
            );
        }
    }

    #[test]
    fn flat_spend_success() {
        let mut res = flat_resource(3, 3);
        assert!(res.spend(&ResourceAmount::Flat(2)).is_ok());
        assert_eq!(res.current_uses()[0], ResourceAmount::Flat(1));
    }

    #[test]
    fn flat_spend_insufficient() {
        let mut res = flat_resource(1, 2);
        let err = res.spend(&ResourceAmount::Flat(2)).unwrap_err();
        match err {
            ResourceError::BudgetError(ResourceBudgetError::InsufficientResources {
                needed,
                available,
            }) => {
                assert_eq!(needed, 2);
                assert_eq!(available, 1);
            }
            _ => panic!("Unexpected error variant"),
        }
    }

    #[test]
    fn flat_recharge() {
        let mut res = flat_resource(2, 5);
        res.recharge_full();
        assert_eq!(res.current_uses()[0], ResourceAmount::Flat(5));
    }

    #[test]
    fn flat_is_empty() {
        let res = flat_resource(0, 0);
        assert!(res.is_empty());
    }

    #[test]
    fn flat_remove_uses_success() {
        let mut res = flat_resource(4, 4);
        res.remove_uses(&ModifierSource::Base);
        assert_eq!(res.max_uses()[0], ResourceAmount::Flat(0));
        assert_eq!(res.current_uses()[0], ResourceAmount::Flat(0));
    }

    #[test]
    fn flat_recharge_rule_order() {
        assert!(RechargeRule::Rest(RestKind::Short) > RechargeRule::Turn);
        assert!(RechargeRule::Rest(RestKind::Long) > RechargeRule::Rest(RestKind::Short));
        assert!(RechargeRule::Daily > RechargeRule::Rest(RestKind::Long));
        assert!(RechargeRule::Never > RechargeRule::Daily);
    }

    #[test]
    fn tiered_spend_success() {
        let mut res = tiered_resource(&[(1, 2, 2), (2, 1, 1)]);
        assert!(
            res.spend(&ResourceAmount::Tiered { tier: 1, amount: 1 })
                .is_ok()
        );
        let uses = res.current_uses();
        assert_eq!(
            uses,
            vec![
                ResourceAmount::Tiered { tier: 1, amount: 1 },
                ResourceAmount::Tiered { tier: 2, amount: 1 }
            ]
        );
    }

    #[test]
    fn tiered_spend_invalid_tier() {
        let mut res = tiered_resource(&[(1, 2, 2)]);
        let err = res
            .spend(&ResourceAmount::Tiered { tier: 2, amount: 1 })
            .unwrap_err();
        match err {
            ResourceError::InvalidTier { tier } => assert_eq!(tier, 2),
            _ => panic!("Unexpected error variant"),
        }
    }

    #[test]
    fn tiered_remove_uses_success() {
        let mut res = tiered_resource(&[(1, 2, 2), (2, 1, 1)]);
        res.remove_uses(&ModifierSource::Base);
        let uses = res.max_uses();
        assert_eq!(
            uses,
            vec![
                ResourceAmount::Tiered { tier: 1, amount: 0 },
                ResourceAmount::Tiered { tier: 2, amount: 0 }
            ]
        );
    }

    #[test]
    fn tiered_recharge() {
        let mut res = tiered_resource(&[(1, 0, 2), (2, 0, 1)]);
        res.recharge_full();
        let uses = res.current_uses();
        assert_eq!(
            uses,
            vec![
                ResourceAmount::Tiered { tier: 1, amount: 2 },
                ResourceAmount::Tiered { tier: 2, amount: 1 }
            ]
        );
    }

    #[test]
    fn tiered_is_empty() {
        let res = tiered_resource(&[(1, 0, 0), (2, 0, 0)]);
        assert!(res.is_empty());
    }

    #[test]
    fn resource_map_add_and_get() {
        let mut map = ResourceMap::new();
        let res = flat_resource(1, 1);
        map.add(
            ResourceId::new("nat20_core", "Action"),
            res.clone(),
            &ResourceMergePolicy::add_all(),
        );
        let got = map.get(&ResourceId::new("nat20_core", "Action")).unwrap();
        assert_eq!(got.max_uses(), vec![ResourceAmount::Flat(1)]);
    }

    #[test]
    fn resource_map_iter() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Action"),
            flat_resource(1, 1),
            &ResourceMergePolicy::add_all(),
        );
        map.add(
            ResourceId::new("nat20_core", "Bonus Action"),
            flat_resource(1, 1),
            &ResourceMergePolicy::add_all(),
        );
        let ids: Vec<_> = map.iter().map(|(id, _)| id).collect();
        assert!(ids.contains(&&ResourceId::new("nat20_core", "Action")));
        assert!(ids.contains(&&ResourceId::new("nat20_core", "Bonus Action")));
    }

    #[test]
    fn resource_map_can_afford_flat() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Ki Point"),
            flat_resource(3, 3),
            &ResourceMergePolicy::add_all(),
        );

        let mut cost = ResourceAmountMap::new();
        cost.map.insert(
            ResourceId::new("nat20_core", "Ki Point"),
            ResourceAmount::Flat(2),
        );

        assert!(map.can_afford_all(&cost).is_ok());
    }

    #[test]
    fn resource_map_can_afford_flat_insufficient() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Ki Point"),
            flat_resource(1, 3),
            &ResourceMergePolicy::add_all(),
        );

        let mut cost = ResourceAmountMap::new();
        cost.map.insert(
            ResourceId::new("nat20_core", "Ki Point"),
            ResourceAmount::Flat(2),
        );

        assert!(!map.can_afford_all(&cost).is_ok());
    }

    #[test]
    fn resource_map_can_afford_tier() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Spell Slot"),
            tiered_resource(&[(1, 2, 2), (2, 1, 1)]),
            &ResourceMergePolicy::add_all(),
        );

        let mut cost = ResourceAmountMap::new();
        cost.map.insert(
            ResourceId::new("nat20_core", "Spell Slot"),
            ResourceAmount::Tiered { tier: 1, amount: 2 },
        );

        assert!(map.can_afford_all(&cost).is_ok());
    }

    #[test]
    fn resource_map_can_afford_tier_insufficient() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Spell Slot"),
            tiered_resource(&[(1, 1, 2), (2, 1, 1)]),
            &ResourceMergePolicy::add_all(),
        );

        let mut cost = ResourceAmountMap::new();
        cost.map.insert(
            ResourceId::new("nat20_core", "Spell Slot"),
            ResourceAmount::Tiered { tier: 1, amount: 2 },
        );

        assert!(!map.can_afford_all(&cost).is_ok());
    }

    #[test]
    fn resource_map_spend_flat_success() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Ki Point"),
            flat_resource(3, 3),
            &ResourceMergePolicy::add_all(),
        );

        let mut cost = ResourceAmountMap::new();
        cost.map.insert(
            ResourceId::new("nat20_core", "Ki Point"),
            ResourceAmount::Flat(2),
        );

        assert!(map.spend_all(&cost).is_ok());
        let res = map.get(&ResourceId::new("nat20_core", "Ki Point")).unwrap();
        assert_eq!(res.current_uses()[0], ResourceAmount::Flat(1));
    }

    #[test]
    fn resource_map_spend_flat_insufficient() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Ki Point"),
            flat_resource(1, 3),
            &ResourceMergePolicy::add_all(),
        );

        let mut cost = ResourceAmountMap::new();
        cost.map.insert(
            ResourceId::new("nat20_core", "Ki Point"),
            ResourceAmount::Flat(2),
        );

        let err = map.spend_all(&cost).unwrap_err();
        assert!(err.len() == 1);
        match err[0].clone() {
            ResourceError::InsufficientResource {
                id,
                needed,
                available,
            } => {
                assert_eq!(id, ResourceId::new("nat20_core", "Ki Point"));
                assert_eq!(needed, ResourceAmount::Flat(2));
                assert_eq!(available, ResourceAmount::Flat(1));
            }
            _ => panic!("Unexpected error variant"),
        }
    }

    #[test]
    fn resource_map_spend_tier_success() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Spell Slot"),
            tiered_resource(&[(1, 2, 2), (2, 1, 1)]),
            &ResourceMergePolicy::add_all(),
        );

        let mut cost = ResourceAmountMap::new();
        cost.map.insert(
            ResourceId::new("nat20_core", "Spell Slot"),
            ResourceAmount::Tiered { tier: 1, amount: 2 },
        );

        assert!(map.spend_all(&cost).is_ok());
        let res = map
            .get(&ResourceId::new("nat20_core", "Spell Slot"))
            .unwrap();
        let uses = res.current_uses();
        assert_eq!(
            uses,
            vec![
                ResourceAmount::Tiered { tier: 1, amount: 0 },
                ResourceAmount::Tiered { tier: 2, amount: 1 }
            ]
        );
    }

    #[test]
    fn resource_map_can_afford_mixed_resources() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Ki Point"),
            flat_resource(3, 3),
            &ResourceMergePolicy::add_all(),
        );
        map.add(
            ResourceId::new("nat20_core", "Spell Slot"),
            tiered_resource(&[(1, 2, 2), (2, 1, 1)]),
            &ResourceMergePolicy::add_all(),
        );

        let mut cost = ResourceAmountMap::new();
        cost.map.insert(
            ResourceId::new("nat20_core", "Ki Point"),
            ResourceAmount::Flat(2),
        );
        cost.map.insert(
            ResourceId::new("nat20_core", "Spell Slot"),
            ResourceAmount::Tiered { tier: 1, amount: 2 },
        );

        assert!(map.can_afford_all(&cost).is_ok());
    }

    #[test]
    fn resource_map_spend_multiple_resources() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Ki Point"),
            flat_resource(3, 3),
            &ResourceMergePolicy::add_all(),
        );
        map.add(
            ResourceId::new("nat20_core", "Rage"),
            flat_resource(2, 2),
            &ResourceMergePolicy::add_all(),
        );

        let mut cost = ResourceAmountMap::new();
        cost.map.insert(
            ResourceId::new("nat20_core", "Ki Point"),
            ResourceAmount::Flat(2),
        );
        cost.map.insert(
            ResourceId::new("nat20_core", "Rage"),
            ResourceAmount::Flat(1),
        );

        assert!(map.spend_all(&cost).is_ok());
        let ki = map.get(&ResourceId::new("nat20_core", "Ki Point")).unwrap();
        let rage = map.get(&ResourceId::new("nat20_core", "Rage")).unwrap();
        assert_eq!(ki.current_uses()[0], ResourceAmount::Flat(1));
        assert_eq!(rage.current_uses()[0], ResourceAmount::Flat(1));
    }

    #[test]
    fn resource_map_spend_mixed_resources() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Ki Point"),
            flat_resource(3, 3),
            &ResourceMergePolicy::add_all(),
        );
        map.add(
            ResourceId::new("nat20_core", "Spell Slot"),
            tiered_resource(&[(1, 2, 2), (2, 1, 1)]),
            &ResourceMergePolicy::add_all(),
        );

        let mut cost = ResourceAmountMap::new();
        cost.map.insert(
            ResourceId::new("nat20_core", "Ki Point"),
            ResourceAmount::Flat(2),
        );
        cost.map.insert(
            ResourceId::new("nat20_core", "Spell Slot"),
            ResourceAmount::Tiered { tier: 1, amount: 2 },
        );

        assert!(map.spend_all(&cost).is_ok());
        let ki = map.get(&ResourceId::new("nat20_core", "Ki Point")).unwrap();
        let spell_slot = map
            .get(&ResourceId::new("nat20_core", "Spell Slot"))
            .unwrap();
        assert_eq!(ki.current_uses()[0], ResourceAmount::Flat(1));
        assert_eq!(
            spell_slot.current_uses(),
            vec![
                ResourceAmount::Tiered { tier: 1, amount: 0 },
                ResourceAmount::Tiered { tier: 2, amount: 1 }
            ]
        );
    }

    #[test]
    fn resource_map_replace_tiered_resource() {
        let mut map = ResourceMap::new();
        let policy = ResourceMergePolicy {
            current_uses: ResourceMergeOperation::Skip,
            max_uses: ResourceMergeOperation::Replace,
            tier: ResourceMergeOperation::Replace,
        };

        map.add(
            ResourceId::new("nat20_core", "Spell Slot"),
            tiered_resource(&[(1, 1, 1)]),
            &policy,
        );
        assert_eq!(
            map.get(&ResourceId::new("nat20_core", "Spell Slot"))
                .unwrap()
                .current_uses(),
            vec![ResourceAmount::Tiered { tier: 1, amount: 1 }]
        );

        map.add(
            ResourceId::new("nat20_core", "Spell Slot"),
            tiered_resource(&[(1, 0, 0), (2, 1, 1)]),
            &policy,
        );
        assert_eq!(
            map.get(&ResourceId::new("nat20_core", "Spell Slot"))
                .unwrap()
                .current_uses(),
            vec![ResourceAmount::Tiered { tier: 2, amount: 1 }]
        );
    }
}
