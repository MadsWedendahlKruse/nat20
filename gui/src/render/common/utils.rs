use crate::state::gui_state::GuiState;

/// A trait for things that can render themselves.
///
/// As opposed to `ImguiRenderable`, which is only for rendering ImGui widgets
/// in the UI, this can be used for rendering anything, including 3D objects in
/// the world.
pub trait Renderable {
    fn render(&self, ui: &imgui::Ui, gui_state: &mut GuiState);
}

pub trait RenderableMut {
    fn render_mut(&mut self, ui: &imgui::Ui, gui_state: &mut GuiState);
}

pub trait RenderableWithContext<C> {
    fn render_with_context(&self, ui: &imgui::Ui, gui_state: &mut GuiState, context: C);
}

pub trait RenderableMutWithContext<C> {
    fn render_mut_with_context(&mut self, ui: &imgui::Ui, gui_state: &mut GuiState, context: C);
}
