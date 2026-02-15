use std::{
    collections::{BTreeMap, HashMap},
    fmt::{Debug, Display},
    hash::Hash,
    ops::{Add, AddAssign, Sub, SubAssign},
    str::FromStr,
};

use serde::{Deserialize, Deserializer, Serialize};

use crate::{
    components::{
        id::{IdProvider, ResourceId},
        modifier::ModifierSource,
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

impl Into<String> for RechargeRule {
    fn into(self) -> String {
        match self {
            RechargeRule::Turn => "turn".to_string(),
            RechargeRule::Rest(RestKind::Short) => "short_rest".to_string(),
            RechargeRule::Rest(RestKind::Long) => "long_rest".to_string(),
            RechargeRule::Daily => "daily".to_string(),
            RechargeRule::Never => "never".to_string(),
        }
    }
}

impl RechargeRule {
    /// Checks if this recharge rule is recharged by another rule.
    /// For example, `ShortRest` is also recharged by `LongRest`.
    ///
    /// `Never` is (as the name suggests) never recharged.
    pub fn is_recharged_by(&self, other: &RechargeRule) -> bool {
        if self == &RechargeRule::Never {
            return false;
        }
        match (self, other) {
            (RechargeRule::Rest(self_rest), RechargeRule::Rest(other_rest)) => {
                return other_rest >= self_rest;
            }
            _ => {}
        }
        *other >= *self
    }
}

impl Display for RechargeRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct ResourceBudget {
    pub current_uses: u8,
    pub max_uses: u8,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResourceBudgetError {
    InsufficientResources { needed: u8, available: u8 },
    ZeroMaxUses,
    NegativeMaxUses { reduction: u8, max_uses: u8 },
    CurrentUsesAboveMax { current_uses: u8, max_uses: u8 },
}

impl ResourceBudget {
    pub fn new(current_uses: u8, max_uses: u8) -> Result<Self, ResourceBudgetError> {
        if max_uses == 0 {
            return Err(ResourceBudgetError::ZeroMaxUses);
        }
        if current_uses > max_uses {
            return Err(ResourceBudgetError::CurrentUsesAboveMax {
                current_uses,
                max_uses,
            });
        }
        Ok(Self {
            current_uses,
            max_uses,
        })
    }

    pub fn with_max_uses(max_uses: u8) -> Result<Self, ResourceBudgetError> {
        Self::new(max_uses, max_uses)
    }

    pub fn can_afford(&self, amount: u8) -> bool {
        self.current_uses >= amount
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
        self.current_uses == 0
    }

    pub fn add_uses(&mut self, amount: u8) -> Result<(), ResourceBudgetError> {
        self.max_uses += amount;
        self.current_uses += amount;
        Ok(())
    }

    pub fn remove_uses(&mut self, amount: u8) -> Result<(), ResourceBudgetError> {
        if amount > self.max_uses {
            return Err(ResourceBudgetError::NegativeMaxUses {
                reduction: amount,
                max_uses: self.max_uses,
            });
        }
        self.max_uses -= amount;
        if self.current_uses > self.max_uses {
            self.current_uses = self.max_uses;
        }
        Ok(())
    }

    pub fn set_current_uses(&mut self, current_uses: u8) -> Result<(), ResourceBudgetError> {
        if current_uses > self.max_uses {
            return Err(ResourceBudgetError::CurrentUsesAboveMax {
                current_uses,
                max_uses: self.max_uses,
            });
        }
        self.current_uses = current_uses;
        Ok(())
    }

    pub fn set_max_uses(&mut self, max_uses: u8) -> Result<(), ResourceBudgetError> {
        if max_uses == 0 {
            return Err(ResourceBudgetError::ZeroMaxUses);
        }
        if max_uses < self.current_uses {
            self.current_uses = max_uses;
        }
        self.max_uses = max_uses;
        Ok(())
    }

    pub fn recharge_full(&mut self) {
        self.current_uses = self.max_uses;
    }

    // TODO: return type is just for the macro impl_resource_amount_router
    pub fn restore(&mut self, amount: u8) -> Result<(), ResourceBudgetError> {
        self.current_uses += amount;
        if self.current_uses > self.max_uses {
            self.current_uses = self.max_uses;
        }
        Ok(())
    }
}

impl Display for ResourceBudget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.current_uses, self.max_uses)
    }
}

impl FromStr for ResourceBudget {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split('/').collect();
        if parts.len() == 1 {
            let max_uses = parts[0]
                .parse::<u8>()
                .map_err(|_| format!("Invalid ResourceBudget format: {}", s))?;
            let result = ResourceBudget::with_max_uses(max_uses);
            match result {
                Ok(budget) => Ok(budget),
                Err(e) => Err(format!("Error creating ResourceBudget: {:?}", e)),
            }
        } else if parts.len() == 2 {
            let current_uses = parts[0]
                .parse::<u8>()
                .map_err(|_| format!("Invalid ResourceBudget format: {}", s))?;
            let max_uses = parts[1]
                .parse::<u8>()
                .map_err(|_| format!("Invalid ResourceBudget format: {}", s))?;
            let result = ResourceBudget::new(current_uses, max_uses);
            match result {
                Ok(budget) => Ok(budget),
                Err(e) => Err(format!("Error creating ResourceBudget: {:?}", e)),
            }
        } else {
            return Err(format!("Invalid ResourceBudget format: {}", s));
        }
    }
}

impl TryFrom<String> for ResourceBudget {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<ResourceBudget> for String {
    fn from(spec: ResourceBudget) -> Self {
        spec.to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ResourceBudgetKind {
    Flat(ResourceBudget),
    Tiered(BTreeMap<u8, ResourceBudget>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResourceError {
    BudgetError(ResourceBudgetError),
    MistmatchAmountAndKind {
        amount: ResourceAmount,
        kind: ResourceBudgetKind,
    },
    InvalidResourceKind(String),
    InvalidTier {
        tier: u8,
    },
    InsufficientResource {
        id: ResourceId,
        needed: ResourceAmount,
        available: ResourceAmount,
    },
}

macro_rules! impl_resource_amount_router {
    ($( $fn_name:ident => $inner:ident ),+ $(,)?) => {
        $(
            pub fn $fn_name(&mut self, __arg: &ResourceAmount) -> Result<(), ResourceError> {
                match (&mut *self, __arg) {
                    (ResourceBudgetKind::Flat(budget), ResourceAmount::Flat(amt)) => {
                        match budget.$inner(*amt) {
                            Ok(()) => Ok(()),
                            Err(e) => Err(ResourceError::BudgetError(e)),
                        }
                    }
                    (ResourceBudgetKind::Tiered(budgets), ResourceAmount::Tiered { tier, amount }) => {
                        if let Some(budget) = budgets.get_mut(tier) {
                            match budget.$inner(*amount) {
                                Ok(()) => Ok(()),
                                Err(e) => Err(ResourceError::BudgetError(e)),
                            }
                        } else {
                            Err(ResourceError::InvalidTier {
                                tier: *tier,
                            })
                        }
                    }
                    _ => Err(ResourceError::MistmatchAmountAndKind {
                        amount: __arg.clone(),
                        kind: self.clone(),
                    }),
                }
            }
        )+
    };
}

impl ResourceBudgetKind {
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

    fn can_afford(&self, cost: &ResourceAmount) -> bool {
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

    impl_resource_amount_router! {
        spend => spend,
        restore => restore,
        add_uses => add_uses,
        remove_uses => remove_uses,
        set_current_uses => set_current_uses,
        set_max_uses => set_max_uses,
    }

    pub fn max_uses(&self) -> Vec<ResourceAmount> {
        match self {
            ResourceBudgetKind::Flat(budget) => vec![ResourceAmount::Flat(budget.max_uses)],
            ResourceBudgetKind::Tiered(budgets) => budgets
                .iter()
                .map(|(tier, budget)| ResourceAmount::Tiered {
                    tier: *tier,
                    amount: budget.max_uses,
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
}

impl From<ResourceAmount> for ResourceBudgetKind {
    fn from(amount: ResourceAmount) -> Self {
        match amount {
            ResourceAmount::Flat(max_uses) => {
                ResourceBudgetKind::Flat(ResourceBudget::with_max_uses(max_uses).unwrap())
            }
            ResourceAmount::Tiered { tier, amount } => {
                let mut map = BTreeMap::new();
                map.insert(tier, ResourceBudget::with_max_uses(amount).unwrap());
                ResourceBudgetKind::Tiered(map)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ResourceDefinitionKind {
    Flat,
    Tiered,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Resource {
    pub id: ResourceId,
    pub kind: ResourceDefinitionKind,
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
        #[derive(Deserialize)]
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

pub type ResourceAmountMap = HashMap<ResourceId, ResourceAmount>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceMap {
    resources: HashMap<ResourceId, ResourceBudgetKind>,
    /// Some effects might prevent certain resources from being used
    disabled_resources: HashMap<ResourceId, ModifierSource>,
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
        set_current_uses: bool,
    ) {
        self.resources
            .entry(resource.clone())
            .and_modify(|existing| match (existing, &budget) {
                (
                    ResourceBudgetKind::Flat(existing_budget),
                    ResourceBudgetKind::Flat(new_budget),
                ) => {
                    existing_budget.set_max_uses(new_budget.max_uses).unwrap();
                    if set_current_uses {
                        existing_budget
                            .set_current_uses(new_budget.current_uses)
                            .unwrap();
                    }
                }

                (
                    ResourceBudgetKind::Tiered(existing_budgets),
                    ResourceBudgetKind::Tiered(new_budgets),
                ) => {
                    for (tier, new_budget) in new_budgets {
                        existing_budgets
                            .entry(*tier)
                            .and_modify(|existing_budget| {
                                existing_budget.set_max_uses(new_budget.max_uses).unwrap();
                                if set_current_uses {
                                    existing_budget
                                        .set_current_uses(new_budget.current_uses)
                                        .unwrap();
                                }
                            })
                            .or_insert(new_budget.clone());
                    }
                }

                _ => {
                    panic!("Mismatched resource kinds for resource id {}", resource);
                }
            })
            .or_insert(budget);
    }

    pub fn add_uses(
        &mut self,
        resource: &ResourceId,
        amount: &ResourceAmount,
    ) -> Result<(), ResourceError> {
        if let Some(budget) = self.resources.get_mut(resource) {
            budget.add_uses(amount)
        } else {
            Err(ResourceError::InvalidResourceKind(format!(
                "Resource with id {} not found",
                resource
            )))
        }
    }

    pub fn remove_uses(
        &mut self,
        resource: &ResourceId,
        amount: &ResourceAmount,
    ) -> Result<(), ResourceError> {
        if let Some(budget) = self.resources.get_mut(resource) {
            budget.remove_uses(amount)
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

    pub fn can_afford_all(&self, cost: &ResourceAmountMap) -> (bool, Option<ResourceId>) {
        for (res_id, res_cost) in cost {
            if !self.can_afford(res_id, res_cost) {
                return (false, Some(res_id.clone()));
            }
        }

        (true, None)
    }

    pub fn spend(&mut self, id: &ResourceId, cost: &ResourceAmount) -> Result<(), ResourceError> {
        if let Some(resource) = self.resources.get_mut(id)
            && !self.disabled_resources.contains_key(id)
        {
            resource.spend(cost)
        } else {
            Err(ResourceError::InvalidResourceKind(format!(
                "Resource with id {} not found",
                id
            )))
        }
    }

    pub fn spend_all(&mut self, cost: &ResourceAmountMap) -> Result<(), ResourceError> {
        let (can_afford, lacking_id) = self.can_afford_all(cost);
        if !can_afford {
            let resource_id = lacking_id.unwrap();
            return Err(ResourceError::InsufficientResource {
                id: resource_id.clone(),
                needed: cost.get(&resource_id).unwrap().clone(),
                available: self
                    .get(&resource_id)
                    .unwrap()
                    .current_uses()
                    .first()
                    .unwrap()
                    .clone(),
            });
        }

        for (id, res_cost) in cost {
            self.spend(id, res_cost)?;
        }

        Ok(())
    }

    pub fn restore(
        &mut self,
        id: &ResourceId,
        restoration: &ResourceAmount,
    ) -> Result<(), ResourceError> {
        if let Some(resource) = self.resources.get_mut(id) {
            resource.restore(restoration)
        } else {
            Err(ResourceError::InvalidResourceKind(format!(
                "Resource with id {} not found",
                id
            )))
        }
    }

    pub fn restore_all(&mut self, restoration: &ResourceAmountMap) -> Result<(), ResourceError> {
        for (id, res_restoration) in restoration {
            self.restore(id, res_restoration)?;
        }

        Ok(())
    }

    pub fn is_resource_disabled(&self, id: &ResourceId) -> Option<&ModifierSource> {
        self.disabled_resources.get(id)
    }

    pub fn disable_resource(&mut self, id: &ResourceId, source: ModifierSource) {
        self.disabled_resources.insert(id.clone(), source);
    }

    pub fn enable_resource(&mut self, id: &ResourceId) {
        self.disabled_resources.remove(id);
    }
}

impl Default for ResourceMap {
    fn default() -> Self {
        let mut map = ResourceMap::new();
        map.resources = HashMap::from([
            (
                ResourceId::new("nat20_core", "resource.action").clone(),
                ResourceBudgetKind::Flat(ResourceBudget::new(1, 1).unwrap()),
            ),
            (
                ResourceId::new("nat20_core", "resource.bonus_action").clone(),
                ResourceBudgetKind::Flat(ResourceBudget::new(1, 1).unwrap()),
            ),
            (
                ResourceId::new("nat20_core", "resource.reaction").clone(),
                ResourceBudgetKind::Flat(ResourceBudget::new(1, 1).unwrap()),
            ),
        ]);
        map
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn flat_resource(current: u8, max: u8) -> ResourceBudgetKind {
        ResourceBudgetKind::Flat(ResourceBudget::new(current, max).unwrap())
    }

    fn tiered_resource(tiers: &[(u8, u8, u8)]) -> ResourceBudgetKind {
        let mut map = BTreeMap::new();
        for (tier, current, max) in tiers {
            map.insert(*tier, ResourceBudget::new(*current, *max).unwrap());
        }
        ResourceBudgetKind::Tiered(map)
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
        let res = flat_resource(0, 1);
        assert!(res.is_empty());
    }

    #[test]
    fn flat_add_uses() {
        let mut res = flat_resource(2, 2);
        res.add_uses(&ResourceAmount::Flat(3));
        assert_eq!(res.max_uses()[0], ResourceAmount::Flat(5));
        assert_eq!(res.current_uses()[0], ResourceAmount::Flat(5));
    }

    #[test]
    fn flat_remove_uses_success() {
        let mut res = flat_resource(4, 4);
        assert!(res.remove_uses(&ResourceAmount::Flat(2)).is_ok());
        assert_eq!(res.max_uses()[0], ResourceAmount::Flat(2));
        assert_eq!(res.current_uses()[0], ResourceAmount::Flat(2));
    }

    #[test]
    fn flat_remove_uses_too_many() {
        let mut res = flat_resource(1, 1);
        let err = res.remove_uses(&ResourceAmount::Flat(2)).unwrap_err();
        match err {
            ResourceError::BudgetError(ResourceBudgetError::NegativeMaxUses {
                reduction,
                max_uses,
            }) => {
                assert_eq!(reduction, 2);
                assert_eq!(max_uses, 1);
            }
            _ => panic!("Unexpected error variant"),
        }
    }

    #[test]
    fn flat_set_current_uses() {
        let mut res = flat_resource(3, 3);
        assert!(res.set_current_uses(&ResourceAmount::Flat(2)).is_ok());
        assert_eq!(res.current_uses()[0], ResourceAmount::Flat(2));
    }

    #[test]
    fn flat_set_max_uses() {
        let mut res = flat_resource(3, 3);
        assert!(res.set_max_uses(&ResourceAmount::Flat(4)).is_ok());
        assert_eq!(res.max_uses()[0], ResourceAmount::Flat(4));
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
    fn tiered_add_uses() {
        let mut res = tiered_resource(&[(1, 2, 2), (2, 1, 1)]);
        res.add_uses(&ResourceAmount::Tiered { tier: 1, amount: 1 });
        let uses = res.max_uses();
        assert_eq!(
            uses,
            vec![
                ResourceAmount::Tiered { tier: 1, amount: 3 },
                ResourceAmount::Tiered { tier: 2, amount: 1 }
            ]
        );
    }

    #[test]
    fn tiered_remove_uses_success() {
        let mut res = tiered_resource(&[(1, 2, 2), (2, 1, 1)]);
        assert!(
            res.remove_uses(&ResourceAmount::Tiered { tier: 1, amount: 1 })
                .is_ok()
        );
        let uses = res.max_uses();
        assert_eq!(
            uses,
            vec![
                ResourceAmount::Tiered { tier: 1, amount: 1 },
                ResourceAmount::Tiered { tier: 2, amount: 1 }
            ]
        );
    }

    #[test]
    fn tiered_remove_uses_too_many() {
        let mut res = tiered_resource(&[(1, 2, 2)]);
        let err = res
            .remove_uses(&ResourceAmount::Tiered { tier: 1, amount: 3 })
            .unwrap_err();
        match err {
            ResourceError::BudgetError(ResourceBudgetError::NegativeMaxUses {
                reduction,
                max_uses,
            }) => {
                assert_eq!(reduction, 3);
                assert_eq!(max_uses, 2);
            }
            _ => panic!("Unexpected error variant"),
        }
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
        let res = tiered_resource(&[(1, 0, 2), (2, 0, 1)]);
        assert!(res.is_empty());
    }

    #[test]
    fn resource_map_add_and_get() {
        let mut map = ResourceMap::new();
        let res = flat_resource(1, 1);
        map.add(ResourceId::new("nat20_core", "Action"), res.clone(), false);
        let got = map.get(&ResourceId::new("nat20_core", "Action")).unwrap();
        assert_eq!(got.max_uses(), vec![ResourceAmount::Flat(1)]);
    }

    #[test]
    fn resource_map_iter() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Action"),
            flat_resource(1, 1),
            false,
        );
        map.add(
            ResourceId::new("nat20_core", "Bonus Action"),
            flat_resource(1, 1),
            false,
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
            false,
        );

        let mut cost = ResourceAmountMap::new();
        cost.insert(
            ResourceId::new("nat20_core", "Ki Point"),
            ResourceAmount::Flat(2),
        );

        assert!(map.can_afford_all(&cost).0);
    }

    #[test]
    fn resource_map_can_afford_flat_insufficient() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Ki Point"),
            flat_resource(1, 3),
            false,
        );

        let mut cost = ResourceAmountMap::new();
        cost.insert(
            ResourceId::new("nat20_core", "Ki Point"),
            ResourceAmount::Flat(2),
        );

        assert!(!map.can_afford_all(&cost).0);
    }

    #[test]
    fn resource_map_can_afford_tier() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Spell Slot"),
            tiered_resource(&[(1, 2, 2), (2, 1, 1)]),
            false,
        );

        let mut cost = ResourceAmountMap::new();
        cost.insert(
            ResourceId::new("nat20_core", "Spell Slot"),
            ResourceAmount::Tiered { tier: 1, amount: 2 },
        );

        assert!(map.can_afford_all(&cost).0);
    }

    #[test]
    fn resource_map_can_afford_tier_insufficient() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Spell Slot"),
            tiered_resource(&[(1, 1, 2), (2, 1, 1)]),
            false,
        );

        let mut cost = ResourceAmountMap::new();
        cost.insert(
            ResourceId::new("nat20_core", "Spell Slot"),
            ResourceAmount::Tiered { tier: 1, amount: 2 },
        );

        assert!(!map.can_afford_all(&cost).0);
    }

    #[test]
    fn resource_map_spend_flat_success() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Ki Point"),
            flat_resource(3, 3),
            false,
        );

        let mut cost = ResourceAmountMap::new();
        cost.insert(
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
            false,
        );

        let mut cost = ResourceAmountMap::new();
        cost.insert(
            ResourceId::new("nat20_core", "Ki Point"),
            ResourceAmount::Flat(2),
        );

        let err = map.spend_all(&cost).unwrap_err();
        match err {
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
            false,
        );

        let mut cost = ResourceAmountMap::new();
        cost.insert(
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
            false,
        );
        map.add(
            ResourceId::new("nat20_core", "Spell Slot"),
            tiered_resource(&[(1, 2, 2), (2, 1, 1)]),
            false,
        );

        let mut cost = ResourceAmountMap::new();
        cost.insert(
            ResourceId::new("nat20_core", "Ki Point"),
            ResourceAmount::Flat(2),
        );
        cost.insert(
            ResourceId::new("nat20_core", "Spell Slot"),
            ResourceAmount::Tiered { tier: 1, amount: 2 },
        );

        assert!(map.can_afford_all(&cost).0);
    }

    #[test]
    fn resource_map_spend_multiple_resources() {
        let mut map = ResourceMap::new();
        map.add(
            ResourceId::new("nat20_core", "Ki Point"),
            flat_resource(3, 3),
            false,
        );
        map.add(
            ResourceId::new("nat20_core", "Rage"),
            flat_resource(2, 2),
            false,
        );

        let mut cost = ResourceAmountMap::new();
        cost.insert(
            ResourceId::new("nat20_core", "Ki Point"),
            ResourceAmount::Flat(2),
        );
        cost.insert(
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
            false,
        );
        map.add(
            ResourceId::new("nat20_core", "Spell Slot"),
            tiered_resource(&[(1, 2, 2), (2, 1, 1)]),
            false,
        );

        let mut cost = ResourceAmountMap::new();
        cost.insert(
            ResourceId::new("nat20_core", "Ki Point"),
            ResourceAmount::Flat(2),
        );
        cost.insert(
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
}
