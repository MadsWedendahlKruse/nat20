use std::{collections::BTreeMap, sync::LazyLock};

use imgui::TreeNodeFlags;
use nat20_core::{
    engine::{game_state::GameState, geometry::WorldPath},
    systems,
};
use rerecast::ConfigBuilder;

use crate::{
    render::{
        common::{colors::Color, utils::RenderableMutWithContext},
        ui::utils::ImguiRenderableMut,
    },
    state::{self, gui_state::GuiState},
    windows::anchor::{self},
};

pub struct NavigationDebugWindow {
    // TODO: This could be useful later to show what changed
    pub initial_config: ConfigBuilder,
    pub navmesh_config: ConfigBuilder,
    pub path_start: [f32; 3],
    pub path_end: [f32; 3],
    pub path: Option<WorldPath>,
    pub render_start: bool,
    pub render_end: bool,
    pub render_path: bool,
}

impl NavigationDebugWindow {
    pub fn new(initial_config: &ConfigBuilder) -> Self {
        Self {
            initial_config: initial_config.clone(),
            navmesh_config: initial_config.clone(),
            path_start: [3.0, 0.0, -8.0],
            path_end: [0.0, 2.0, 7.0],
            path: None,
            render_start: true,
            render_end: true,
            render_path: true,
        }
    }
}

impl RenderableMutWithContext<&mut GameState> for NavigationDebugWindow {
    fn render_mut_with_context(
        &mut self,
        ui: &imgui::Ui,
        gui_state: &mut GuiState,
        game_state: &mut GameState,
    ) {
        let mut nav_debug_open = *gui_state
            .settings
            .get::<bool>(state::parameters::RENDER_NAVIGATION_DEBUG);

        if !nav_debug_open {
            return;
        }

        gui_state.window_manager.render_window(
            ui,
            "Navigation Debug",
            &anchor::CENTER_RIGHT,
            [0.0, 500.0],
            &mut nav_debug_open,
            || {
                if ui.collapsing_header("Navmesh", TreeNodeFlags::DEFAULT_OPEN) {
                    let mut render_navmesh = *gui_state
                        .settings
                        .get::<bool>(state::parameters::RENDER_NAVIGATION_NAVMESH);

                    if ui.checkbox("Render Navmesh", &mut render_navmesh) {
                        // Clicking the checkbox updates the value, so no need to
                        // invert it when setting it back
                        gui_state.settings.set::<bool>(
                            state::parameters::RENDER_NAVIGATION_NAVMESH,
                            render_navmesh,
                        );
                    }

                    if ui.button("Rebuild Navmesh") {
                        let config = self.navmesh_config.clone().build();
                        game_state.geometry.rebuild_navmesh(&config);
                        gui_state.mesh_cache.remove("navmesh");
                    }

                    ui.separator_with_text("Configuration");
                    self.navmesh_config.render_mut(ui);
                }

                if ui.collapsing_header("Pathfinding", TreeNodeFlags::DEFAULT_OPEN) {
                    ui.checkbox("Render Start", &mut self.render_start);
                    ui.checkbox("Render End", &mut self.render_end);

                    let width_token = ui.push_item_width(200.0);
                    ui.input_float3("Start", &mut self.path_start).build();
                    ui.input_float3("End", &mut self.path_end).build();
                    width_token.end();

                    // TODO: These are impossible to see lol
                    if self.render_start {
                        gui_state
                            .line_renderer
                            .add_circle(self.path_start, 0.2, [0.2, 1.0, 0.2]);
                    }
                    if self.render_end {
                        gui_state
                            .line_renderer
                            .add_circle(self.path_end, 0.2, [1.0, 0.2, 0.2]);
                    }

                    ui.checkbox("Render Path", &mut self.render_path);

                    if ui.button("Find Path") {
                        self.path = systems::geometry::path_point_point(
                            &game_state.geometry,
                            self.path_start.into(),
                            self.path_end.into(),
                        )
                    }

                    if self.render_path {
                        if let Some(path) = &self.path {
                            gui_state.line_renderer.add_path(path, Color::White);

                            if ui.collapsing_header("Path Points", TreeNodeFlags::empty()) {
                                ui.text(format!("{:#?}", path));
                            }
                        } else {
                            ui.text("No path found");
                        }
                    }
                }

                if ui.collapsing_header("Polyanya", TreeNodeFlags::empty()) {
                    let polyanya_mesh = &mut game_state.geometry.polyanya_mesh;
                    ui.input_scalar("Search Delta", &mut polyanya_mesh.search_delta)
                        .build();
                    ui.input_scalar("Search Steps", &mut polyanya_mesh.search_steps)
                        .build();
                    for (i, layer) in game_state.geometry.polyanya_mesh.layers.iter().enumerate() {
                        ui.tree_node_config(format!("Layer {}", i)).build(|| {
                            // ui.text(format!("Polygons: {}", layer.polygons.len()));
                            // ui.text(format!("Nodes: {}", layer.nodes.len()));
                        });
                    }
                }
            },
        );

        gui_state
            .settings
            .set::<bool>(state::parameters::RENDER_NAVIGATION_DEBUG, nav_debug_open);
    }
}

impl ImguiRenderableMut for ConfigBuilder {
    fn render_mut(&mut self, ui: &imgui::Ui) {
        let parameters_f32 = [
            ("cell_size_fraction", &mut self.cell_size_fraction),
            ("cell_height_fraction", &mut self.cell_height_fraction),
            ("agent_height", &mut self.agent_height),
            ("agent_radius", &mut self.agent_radius),
            ("walkable_climb", &mut self.walkable_climb),
            ("walkable_slope_angle", &mut self.walkable_slope_angle),
            (
                "max_simplification_error",
                &mut self.max_simplification_error,
            ),
            ("detail_sample_dist", &mut self.detail_sample_dist),
            ("detail_sample_max_error", &mut self.detail_sample_max_error),
        ];

        let parameters_u16 = [
            ("min_region_size", &mut self.min_region_size),
            ("merge_region_size", &mut self.merge_region_size),
            ("edge_max_len_factor", &mut self.edge_max_len_factor),
            (
                "max_vertices_per_polygon",
                &mut self.max_vertices_per_polygon,
            ),
        ];

        let width_token = ui.push_item_width(60.0);

        for (label, value) in parameters_f32 {
            ui.input_scalar(label, value)
                .always_overwrite(true)
                .allow_tab_input(true)
                .auto_select_all(true)
                .build();
            if ui.is_item_hovered() {
                if let Some(doc) = CONFIG_PARAMETERS_DOCUMENTATION.get(label) {
                    ui.tooltip_text(doc);
                }
            }
        }

        for (label, value) in parameters_u16 {
            ui.input_scalar(label, value)
                .always_overwrite(true)
                .allow_tab_input(true)
                .auto_select_all(true)
                .build();
            if ui.is_item_hovered() {
                if let Some(doc) = CONFIG_PARAMETERS_DOCUMENTATION.get(label) {
                    ui.tooltip_text(doc);
                }
            }
        }

        width_token.end();
    }
}

static CONFIG_PARAMETERS_DOCUMENTATION: LazyLock<BTreeMap<&str, String>> = LazyLock::new(|| {
    BTreeMap::from([
        ("cell_size_fraction", rust_doc_to_str(
            "/// How many cells should fit in the [`Self::agent_radius`] on the xz-plane to use for fields. `[Limit: > 0]`.
            ///
            /// The voxelization cell size defines the voxel size along both axes of the ground plane: x and z in Recast.
            /// The resulting value is derived from the character radius r. For example, setting `cell_size_fraction` to 2 will result in the
            /// cell size being r/2, where r is [`Self::agent_radius`].
            ///
            /// A recommended starting value for cell_size is either 2 or 3.
            /// Larger values of cell_size will increase rasterization resolution and navmesh detail, but total generation time will increase exponentially.
            /// In outdoor environments, 2 is often good enough. For indoor scenes with tight spaces you might want the extra precision,
            /// so a value of 3 or higher may give better results.
            ///
            /// The initial instinct is to reduce this value to something very high to maximize the detail of the generated navmesh.
            /// This quickly becomes a case of diminishing returns, however. Beyond a certain point there's usually not much perceptable difference
            /// in the generated navmesh, but huge increases in generation time.
            /// This hinders your ability to quickly iterate on level designs and provides little benefit.
            /// The general recommendation here is to use as small a value for cell_size as you can get away with.
            ///
            /// [`Self::cell_size_fraction`] and [`Self::cell_height_fraction`] define voxel/grid/cell size. So their values have significant side effects on all parameters defined in voxel units.
            ///
            /// The maximum value for this parameter depends on the platform's floating point accuracy,
            /// with the practical maximum usually such that [`Self::agent_radius`] / [`Self::cell_size_fraction`] = 0.05.")),
        ("cell_height_fraction", rust_doc_to_str(
            "/// How many cells should fit in the [`Self::agent_height`] on the y-axis to use for fields. `[Limit: > 0]`
            ///
            /// The voxelization cell height is defined separately in order to allow for greater precision in height tests.
            /// A good starting point for [`Self::cell_height_fraction`] is twice the size of [`Self::cell_size_fraction`].
            /// Higher [`Self::cell_height_fraction`] values ensure that the navmesh properly connects areas that are only separated by a small curb or ditch.
            /// If small holes are generated in your navmesh around where there are discontinuities in height (for example, stairs or curbs),
            /// you may want to increase the cell height value to increase the vertical rasterization precision of rerecast.
            ///
            /// [`Self::cell_size_fraction`] and [`Self::cell_height_fraction`] define voxel/grid/cell size. So their values have significant side effects on all parameters defined in voxel units.
            ///
            /// The minimum value for this parameter depends on the platform's floating point accuracy, with the practical minimum usually usually such that [`Self::agent_radius`] / [`Self::cell_height_fraction`] = 0.05.")),
        ("agent_height", rust_doc_to_str(
            "/// The height of the agent. `[Limit: > 0] [Units: wu]`
            ///
            /// It's often a good idea to add a little bit of padding to the height. For example,
            /// an agent that is 1.8 world units tall might want to set this value to 2.0 units.")),
        ("agent_radius", rust_doc_to_str(
            "/// The radius of the agent. `[Limit: > 0] [Units: wu]`")),
        ("walkable_climb", rust_doc_to_str(
            "/// Maximum ledge height that is considered to still be traversable. `[Limit: >=0] [Units: wu]`
            ///
            /// The walkable_climb value defines the maximum height of ledges and steps that the agent can walk up.
            ///
            /// Allows the mesh to flow over low lying obstructions such as curbs and up/down stairways.
            /// The value is usually set to how far up/down an agent can step.")),
        ("walkable_slope_angle", rust_doc_to_str(
            "/// The maximum slope that is considered walkable. `[Limits: 0 <= value < 0.5*π] [Units: Radians]`
            ///
            /// The parameter walkable_slope_angle is to filter out areas of the world where the ground slope
            /// would be too steep for an agent to traverse.
            /// This value is defined as a maximum angle in degrees that the surface normal of a polygon can differ from the world's up vector.
            /// This value must be within the range `[0, 90.0.to_radians()]`.
            ///
            /// The practical upper limit for this parameter is usually around `85.0.to_radians()`.")),
        ("max_simplification_error", rust_doc_to_str(
            "/// The maximum distance a simplified contour's border edges should deviate
            /// the original raw contour. `[Limit: >=0] [Units: vx]`
            ///
            /// When the rasterized areas are converted back to a vectorized representation,
            /// the max_simplification_error describes how loosely the simplification is done.
            /// The simplification process uses the Ramer–Douglas-Peucker algorithm, and this value describes the max deviation in voxels.
            ///
            /// Good values for max_simplification_error are in the range `[1.1, 1.5]`.
            /// A value of 1.3 is a good starting point and usually yields good results.
            /// If the value is less than 1.1, some sawtoothing starts to appear at the generated edges.
            /// If the value is more than 1.5, the mesh simplification starts to cut some corners it shouldn't.
            ///
            /// The effect of this parameter only applies to the xz-plane.")),
        ("detail_sample_dist", rust_doc_to_str(
            "/// Sets the sampling distance to use when generating the detail mesh.
            /// (For height detail only.) `[Limits: 0 or >= 0.9] [Units: wu]`
            ///
            /// When this value is below 0.9, it will be clamped to 0.0.")),
        ("detail_sample_max_error", rust_doc_to_str(
            "/// The maximum distance the detail mesh surface should deviate from heightfield
         /// data. (For height detail only.) `[Limit: >=0] [Units: wu]`")),
        ("min_region_size", rust_doc_to_str(
            "/// The minimum number of cells allowed to form isolated island areas along one horizontal axis. `[Limit: >=0] [Units: vx]`
            ///
            /// Watershed partitioning is really prone to noise in the input distance field.
            /// In order to get nicer areas, the areas are merged and small disconnected areas are removed after the water shed partitioning.
            /// The parameter [`Self::min_region_size`] describes the minimum isolated region size that is still kept.
            /// A region is removed if the number of voxels in the region is less than the square of [`Self::min_region_size`].
            ///
            /// Any regions that are smaller than this area will be marked as unwalkable.
            /// This is useful in removing useless regions that can sometimes form on geometry such as table tops, box tops, etc.")),
        ("merge_region_size", rust_doc_to_str(
            "/// Any regions with a span count smaller than the square of this value will, if possible,
            /// be merged with larger regions. `[Limit: >=0] [Units: vx]`
            ///
            /// The triangulation process works best with small, localized voxel regions.
            /// The parameter [`Self::merge_region_size`] controls the maximum voxel area of a region that is allowed to be merged with another region.
            /// If you see small patches missing here and there, you could lower the [`Self::min_region_size`] value.")),
        ("edge_max_len_factor", rust_doc_to_str(
            "/// The maximum allowed length for contour edges along the border of the mesh in terms of [`Self::agent_radius`]. `[Limit: >=0]`
            ///
            /// In certain cases, long outer edges may decrease the quality of the resulting triangulation, creating very long thin triangles.
            /// This can sometimes be remedied by limiting the maximum edge length, causing the problematic long edges to be broken up into smaller segments.
            ///
            /// The parameter [`Self::edge_max_len_factor`] defines the maximum edge length and is defined in terms of world units.
            /// A good value for [`Self::edge_max_len_factor`] is something like 8.
            /// A good way to adjust this value is to first set it really high and see if your data creates long edges.
            /// If it does, decrease [`Self::edge_max_len_factor`] until you find the largest value which improves the resulting tesselation.
            ///
            /// Extra vertices will be inserted as needed to keep contour edges below this length.
            /// A value of zero effectively disables this feature.")),
        ("max_vertices_per_polygon", rust_doc_to_str(
            "/// The maximum number of vertices allowed for polygons generated during the
            /// contour to polygon conversion process. `[Limit: >= 3]`")),
    ])
});

fn rust_doc_to_str(doc: &str) -> String {
    doc.lines()
        .map(|line| line.trim().trim_start_matches('/'))
        .collect::<Vec<&str>>()
        .join("\n")
}
