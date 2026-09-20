use std::collections::BTreeMap;

use imgui::{InputTextFlags, TreeNodeFlags};
use nat20_core::components::scratchpad::{ScratchValue, Scratchable, Scratchpad};

use crate::{
    render::ui::utils::{ImguiRenderableMut, ImguiRenderableMutWithContext},
    state::{self},
};

impl ImguiRenderableMutWithContext<&str> for ScratchValue {
    fn render_mut_with_context(&mut self, ui: &imgui::Ui, label: &str) {
        match self {
            ScratchValue::Bool(v) => ui.checkbox(label, v),
            ScratchValue::I32(v) => ui.input_scalar(label, v).build(),
            ScratchValue::F32(v) => ui.input_scalar(label, v).build(),
            ScratchValue::String(v) => ui.input_text(label, v).build(),
        };
    }
}

/// Pure view node for rendering; stores child folders + *full keys* of leaves.
/// No references, so no borrow headaches.
#[derive(Default)]
struct ViewNode {
    children: BTreeMap<String, ViewNode>,
    leaves: Vec<String>, // keys that terminate here
}

impl ViewNode {
    pub fn new<'a, I>(keys: I) -> Self
    where
        I: Iterator<Item = &'a str>,
    {
        Self::new_filtered(keys, |_| true)
    }

    /// Build a pruned tree containing only keys that match `pred`, plus their ancestor folders.
    fn new_filtered<'a, I, F>(keys: I, mut pred: F) -> Self
    where
        I: Iterator<Item = &'a str>,
        F: FnMut(&str) -> bool,
    {
        let mut root = ViewNode::default();
        for full in keys {
            if !pred(full) {
                continue;
            }
            let mut node = &mut root;
            let mut parts = full.split('.').peekable();
            while let Some(seg) = parts.next() {
                if parts.peek().is_some() {
                    node = node.children.entry(seg.to_string()).or_default();
                } else {
                    node.leaves.push(full.to_string());
                }
            }
        }
        root
    }
}

// root_path is "" for the root; we build child paths like "render/ui/imgui"
fn render_view_tree(
    ui: &imgui::Ui,
    node: &ViewNode,
    settings: &mut Scratchpad,
    title: &str,
    root_path: &str,
    open_all: bool, // true when filtering
) {
    let flags = if open_all {
        TreeNodeFlags::DEFAULT_OPEN
    } else {
        TreeNodeFlags::empty()
    };

    if title.is_empty() {
        for (name, child) in &node.children {
            let next = if root_path.is_empty() {
                name.clone()
            } else {
                format!("{}/{}", root_path, name)
            };
            render_view_tree(ui, child, settings, name, &next, open_all);
        }
        for key in &node.leaves {
            let _id = ui.push_id(key);
            if let Some(s) = settings.get_scratch_value_mut(key) {
                // Optional: highlight leaf label when search matches
                let label = leaf_label(key);
                s.render_mut_with_context(ui, label);
            }
        }
        return;
    }

    let _id = ui.push_id(root_path);
    ui.tree_node_config(title).flags(flags).build(|| {
        for (name, child) in &node.children {
            let next = format!("{}/{}", root_path, name);
            render_view_tree(ui, child, settings, name, &next, open_all);
        }
        for key in &node.leaves {
            let _lid = ui.push_id(key);
            if let Some(s) = settings.get_scratch_value_mut(key) {
                s.render_mut_with_context(ui, leaf_label(key));
            }
        }
    });
}

pub struct GuiSettings {
    /// The underlying scratchpad storing all settings as key-value pairs.
    /// Not the intended use of this component, but it works nicely here :)
    settings: Scratchpad,
    view_tree: ViewNode,
    search: String,
}

impl GuiSettings {
    pub fn new(settings: BTreeMap<String, ScratchValue>) -> Self {
        let view_tree = ViewNode::new(settings.keys().map(String::as_str));
        Self {
            settings: Scratchpad::new(settings),
            view_tree,
            search: String::new(),
        }
    }

    /// Borrow as the requested type, if the variant matches.
    pub fn get<T: Scratchable>(&self, key: &str) -> &T {
        if !self.settings.contains_key(key) {
            panic!("setting '{}' does not exist", key);
        }
        self.settings
            .get(key)
            .unwrap_or_else(|| panic!("setting '{}' is not of expected type", key))
    }

    /// Mutably borrow as the requested type, if the variant matches.
    pub fn get_mut<T: Scratchable>(&mut self, key: &str) -> &mut T {
        if !self.settings.contains_key(key) {
            panic!("setting '{}' does not exist", key);
        }
        self.settings
            .get_mut(key)
            .unwrap_or_else(|| panic!("setting '{}' is not of expected type", key))
    }

    /// Set/overwrite the value with the appropriate enum variant.
    pub fn set<T: Scratchable>(&mut self, key: &str, value: T) {
        self.settings.insert(key.to_string(), value);
        // (Optional) if you allow inserting new keys here, rebuild the tree:
        // self.view_tree = ViewNode::new(self.settings.keys().map(String::as_str));
    }
}

impl ImguiRenderableMut for GuiSettings {
    fn render_mut(&mut self, ui: &imgui::Ui) {
        // --- Search bar ---
        let width_token = ui.push_item_width(100.0);
        ui.input_text("Search", &mut self.search)
            .flags(InputTextFlags::AUTO_SELECT_ALL)
            .build();
        width_token.end();

        ui.same_line();
        if ui.button("Clear") {
            self.search.clear();
        }

        ui.separator();

        // Case-insensitive matcher: match on full key OR leaf label
        let query = self.search.trim().to_lowercase();
        let filtering = !query.is_empty();
        let matcher = |full: &str| {
            if query.is_empty() {
                return true;
            }
            let leaf = leaf_label(full);
            full.to_lowercase().contains(&query) || leaf.to_lowercase().contains(&query)
        };

        // Build the (possibly filtered) tree for this frame
        let tree = if filtering {
            &ViewNode::new_filtered(self.settings.keys().map(String::as_str), matcher)
        } else {
            // use the cached unfiltered tree
            &self.view_tree
        };

        // Render with tree nodes; when filtering, default-open everything
        render_view_tree(ui, tree, &mut self.settings, "", "", filtering);
    }
}

impl Default for GuiSettings {
    fn default() -> Self {
        Self::new(BTreeMap::from([
            (
                state::parameters::RENDER_IMGUI_ABOUT.to_string(),
                ScratchValue::Bool(false),
            ),
            (
                state::parameters::RENDER_IMGUI_METRICS.to_string(),
                ScratchValue::Bool(false),
            ),
            (
                state::parameters::RENDER_IMGUI_USER_GUIDE.to_string(),
                ScratchValue::Bool(false),
            ),
            (
                state::parameters::RENDER_IMGUI_DEMO.to_string(),
                ScratchValue::Bool(false),
            ),
            (
                state::parameters::RENDER_NAVIGATION_DEBUG.to_string(),
                ScratchValue::Bool(false),
            ),
            (
                state::parameters::RENDER_NAVIGATION_NAVMESH.to_string(),
                ScratchValue::Bool(false),
            ),
            (
                state::parameters::RENDER_CAMERA_DEBUG.to_string(),
                ScratchValue::Bool(false),
            ),
            (
                state::parameters::RENDER_GRID.to_string(),
                ScratchValue::Bool(true),
            ),
            (
                state::parameters::RENDER_LINE_OF_SIGHT_DEBUG.to_string(),
                ScratchValue::Bool(false),
            ),
            (
                state::parameters::RENDER_GAME_STATE_DEBUG.to_string(),
                ScratchValue::Bool(false),
            ),
            (
                state::parameters::UPDATE_GAME_STATE.to_string(),
                ScratchValue::Bool(true),
            ),
        ]))
    }
}

/// Helper: last segment of a key for a nice leaf label
fn leaf_label(key: &str) -> &str {
    key.rsplit('.').next().unwrap_or(key)
}
