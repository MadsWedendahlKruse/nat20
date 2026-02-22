use std::collections::{BTreeMap, HashMap};

use glow::HasContext;
use hecs::Entity;
use imgui_glow_renderer::AutoRenderer;
use nat20_core::systems::{geometry::RaycastResult, movement::PathResult};
use parry3d::na::Vector3;
use winit::window::Window;

use crate::{
    render::world::{
        camera::OrbitCamera,
        frame_uniforms::FrameUniforms,
        grid::GridRenderer,
        line::LineRenderer,
        mesh::{Mesh, MeshRenderMode},
        program::BasicProgram,
    },
    state::settings::GuiSettings,
    windows::anchor::WindowManager,
};

pub struct GuiState {
    pub ig_renderer: AutoRenderer,
    pub frame_uniforms: FrameUniforms,
    pub program: BasicProgram,
    pub camera: OrbitCamera,

    /// I'm not entirely sure where the best place to put these two, so for now
    /// they can live in here :^)
    pub line_renderer: LineRenderer,
    pub grid_renderer: GridRenderer,

    /// GUI settings, mostly used to configure various rendering options.
    pub settings: GuiSettings,

    /// Manages the positioning of anchored windows.
    pub window_manager: WindowManager,

    /// Store the latest computed path for each entity. This is mostly used for
    /// visualization/debugging purposes.
    pub path_cache: HashMap<Entity, PathResult>,

    // TODO: Everything involving the mesh cache seems like a mess right now.
    pub mesh_cache: BTreeMap<String, Mesh>,

    /// The result of a raycast from the cursor into the 3D world.
    /// This is updated every frame, and can be used by various UI components
    /// to determine what the cursor is pointing at.
    ///
    /// To avoid multiple UI components using the same raycast result, this is
    /// an `Option`. UI components that use it should `.take()` it, so that other
    /// components know it has already been used.
    ///
    /// Note that this will also be `None` if the cursor didn't hit anything in
    /// the 3D world.
    pub cursor_ray_result: Option<RaycastResult>,

    /// The entity currently selected. This can be used for various purposes, so
    /// it lives here in the GUI state :^)
    pub selected_entity: Option<Entity>,

    /// The render mode for each creature entity. Mostly used for highlighting
    /// creatures within an AoE, etc.
    pub creature_render_mode: HashMap<Entity, MeshRenderMode>,
}

impl GuiState {
    pub fn new(gl: glow::Context, imgui_context: &mut imgui::Context) -> Self {
        unsafe {
            gl.enable(glow::DEPTH_TEST);
            gl.blend_func(glow::SRC_ALPHA, glow::ONE_MINUS_SRC_ALPHA);
            gl.enable(glow::BLEND);
        }

        let ig_renderer =
            AutoRenderer::new(gl, imgui_context).expect("failed to create imgui OpenGL renderer");
        let frame_uniforms = FrameUniforms::new(ig_renderer.gl_context(), 0);
        let program = BasicProgram::new(
            ig_renderer.gl_context(),
            include_str!("../render/world/shaders/basic.vert"),
            include_str!("../render/world/shaders/basic.frag"),
        );

        let line_renderer = LineRenderer::new(
            ig_renderer.gl_context(),
            include_str!("../render/world/shaders/line.vert"),
            include_str!("../render/world/shaders/line.frag"),
        );
        let grid_renderer = GridRenderer::new(
            ig_renderer.gl_context(),
            50,
            1.0,
            10,
            include_str!("../render/world/shaders/grid.vert"),
            include_str!("../render/world/shaders/grid.frag"),
        );

        Self {
            ig_renderer,
            frame_uniforms,
            program,
            line_renderer,
            grid_renderer,
            camera: OrbitCamera::new(),
            settings: GuiSettings::default(),
            window_manager: WindowManager::new(),
            path_cache: HashMap::new(),
            mesh_cache: BTreeMap::new(),
            cursor_ray_result: None,
            selected_entity: None,
            creature_render_mode: HashMap::default(),
        }
    }

    pub fn gl_context(&self) -> &glow::Context {
        self.ig_renderer.gl_context()
    }

    pub fn new_frame(&mut self, window: &Window) {
        let gl = self.ig_renderer.gl_context();
        unsafe {
            gl.clear_color(0.05, 0.05, 0.1, 1.0);
            gl.clear(glow::COLOR_BUFFER_BIT | glow::DEPTH_BUFFER_BIT);
        }

        let size = window.inner_size();
        unsafe {
            gl.viewport(0, 0, size.width as i32, size.height as i32);
            gl.clear_color(0.05, 0.05, 0.1, 1.0);
            gl.clear(glow::COLOR_BUFFER_BIT | glow::DEPTH_BUFFER_BIT);
        }

        let view = self.camera.view();
        let proj = self.camera.proj(size.width, size.height);
        let light_dir = Vector3::new(-0.5, -1.0, -0.8);
        self.frame_uniforms.update(gl, view, proj, light_dir);

        self.window_manager.new_frame();

        self.creature_render_mode = HashMap::default()
    }
}
