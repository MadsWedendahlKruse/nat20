use nat20_core::{
    engine::game_state::GameState,
    systems::{self, geometry::RaycastFilter},
};
use parry3d::{
    na::{Isometry3, Perspective3, Point3, Vector3},
    query::Ray,
};
use winit::{
    event::{MouseButton, MouseScrollDelta, WindowEvent},
    keyboard::PhysicalKey,
};

use crate::{
    render::ui::utils::ImguiRenderableMutWithContext,
    windows::anchor::{self, AUTO_RESIZE, WindowManager},
};

// Initial camera parameters
static TARGET_X: f32 = 0.0;
static TARGET_Y: f32 = 0.0;
static TARGET_Z: f32 = 0.0;
static DISTANCE: f32 = 30.0;
static YAW: f32 = (-135.0f32).to_radians();
static PITCH: f32 = (-45.0f32).to_radians();

pub struct OrbitCamera {
    pub target: Point3<f32>,
    pub radius: f32, // distance from target
    pub yaw: f32,    // radians, around Y
    pub pitch: f32,  // radians, up/down
    // controls
    rotate_sens: f32, // radians per pixel
    pan_sens: f32,    // world units per pixel (scaled by radius)
    zoom_sens: f32,   // scalar per wheel tick
    // state
    mmb_down: bool,
    shift_down: bool,

    last_cursor: Option<(f32, f32)>,
    last_viewport: Option<(f32, f32)>,
    last_proj: Option<Perspective3<f32>>,
}

impl OrbitCamera {
    pub fn new() -> Self {
        Self {
            target: Point3::new(TARGET_X, TARGET_Y, TARGET_Z),
            radius: DISTANCE,
            yaw: YAW,
            pitch: PITCH,
            rotate_sens: 0.005,
            pan_sens: 0.0015,
            zoom_sens: 1.1,
            mmb_down: false,
            shift_down: false,
            last_cursor: None,
            last_viewport: None,
            last_proj: None,
        }
    }

    pub fn view(&self) -> Isometry3<f32> {
        let dir = Self::spherical_dir(self.yaw, self.pitch);
        let eye = self.target - dir * self.radius;
        Isometry3::look_at_rh(&eye, &self.target, &Vector3::y())
    }

    pub fn proj(&mut self, width: u32, height: u32) -> &Perspective3<f32> {
        let (width, height) = (width as f32, height as f32);
        if self.last_viewport != Some((width, height)) {
            self.last_viewport = Some((width, height));
            self.last_proj = Some(Perspective3::new(
                (width.max(1.0)) / (height.max(1.0)),
                (45.0f32).to_radians(),
                0.1,
                500.0,
            ));
        }
        self.last_proj.as_ref().unwrap()
    }

    /// World-space camera position (eye)
    pub fn eye(&self) -> Point3<f32> {
        let dir = Self::spherical_dir(self.yaw, self.pitch);
        self.target - dir * self.radius
    }

    pub fn ray_from_cursor(&self) -> Option<Ray> {
        if self.last_cursor.is_none() || self.last_viewport.is_none() {
            return None;
        }

        let (mouse_px, mouse_py) = self.last_cursor.unwrap();
        let (viewport_w, viewport_h) = self.last_viewport.unwrap();
        let proj = self.last_proj.as_ref().unwrap();

        // NDC in OpenGL: x,y ∈ [-1,1], y-up (flip because pixels have y-down)
        let x_ndc = (2.0 * mouse_px / viewport_w) - 1.0;
        let y_ndc = 1.0 - (2.0 * mouse_py / viewport_h);

        // Camera-space ray dir (OpenGL RH: forward is -Z)
        let fovy = proj.fovy();
        let aspect = proj.aspect();
        let tan = (fovy * 0.5).tan();
        let dir_cam = Vector3::new(x_ndc * tan * aspect, y_ndc * tan, -1.0).normalize();

        // World-space: rotate by camera orientation, origin at camera eye
        let cam_iso = self.view().inverse(); // camera (world) pose
        let origin = cam_iso.translation.vector;
        let dir = cam_iso.rotation * dir_cam;

        Some(Ray::new(origin.into(), dir.normalize()))
    }

    fn spherical_dir(yaw: f32, pitch: f32) -> Vector3<f32> {
        let cp = pitch.clamp(-1.5533, 1.5533); // ~±89°
        let cy = yaw.cos();
        let sy = yaw.sin();
        let cpv = cp.cos();
        let sp = cp.sin();
        // right-handed: +Z forward; we want camera looking towards target
        Vector3::new(cy * cpv, sp, sy * cpv)
    }

    pub fn world_to_screen(&self, world_pos: &Point3<f32>) -> Option<(f32, f32)> {
        if self.last_viewport.is_none() || self.last_proj.is_none() {
            return None;
        }
        let (viewport_w, viewport_h) = self.last_viewport.unwrap();

        let proj = self.last_proj.as_ref().unwrap();
        let view = self.view();
        let vp_matrix = proj.as_matrix() * view.to_homogeneous();

        let wp = world_pos.to_homogeneous();
        let cp = vp_matrix * wp; // clip space
        if cp.w <= 0.0 {
            return None; // behind camera
        }
        let ndc = cp.xyz() / cp.w; // normalized device coords

        // NDC to window coords
        let x = (ndc.x + 1.0) * 0.5 * viewport_w;
        let y = (1.0 - ndc.y) * 0.5 * viewport_h; // flip Y for pixels
        Some((x, y))
    }

    pub fn handle_event(&mut self, event: &WindowEvent, imgui_wants_mouse: bool) {
        match event {
            WindowEvent::MouseInput { state, button, .. } => {
                let down = *state == winit::event::ElementState::Pressed;
                if button == &MouseButton::Middle {
                    self.mmb_down = down
                }
            }

            WindowEvent::KeyboardInput { event, .. } => match event.physical_key {
                PhysicalKey::Code(key_code) => {
                    if key_code == winit::keyboard::KeyCode::ShiftLeft
                        || key_code == winit::keyboard::KeyCode::ShiftRight
                    {
                        self.shift_down = event.state == winit::event::ElementState::Pressed;
                    }
                }
                PhysicalKey::Unidentified(_) => {}
            },

            WindowEvent::CursorMoved { position, .. } => {
                let (position_x, position_y) = (position.x as f32, position.y as f32);
                if imgui_wants_mouse {
                    self.last_cursor = Some((position_x, position_y));
                    return;
                }

                if let Some((lx, ly)) = self.last_cursor {
                    let dx = position_x - lx;
                    let dy = position_y - ly;

                    if self.mmb_down {
                        // PAN when MMB (and Shift)
                        if self.shift_down {
                            // pan along camera right/up
                            let dir = Self::spherical_dir(self.yaw, self.pitch);
                            let right = Vector3::new(dir.z, 0.0, -dir.x).normalize(); // Y-up right
                            let up = Vector3::y();
                            let scale = self.radius * self.pan_sens;
                            self.target += right * dx * scale;
                            self.target += up * dy * scale;
                        } else {
                            // ORBIT when MMB
                            self.yaw += dx * self.rotate_sens;
                            self.pitch -= dy * self.rotate_sens;
                            self.pitch = self.pitch.clamp(-1.53, 1.53);
                        }
                    }
                }
                self.last_cursor = Some((position_x, position_y));
            }

            WindowEvent::MouseWheel { delta, .. } => {
                if imgui_wants_mouse {
                    return;
                }
                match delta {
                    MouseScrollDelta::LineDelta(_, y) => {
                        self.radius = (self.radius / self.zoom_sens.powf(*y)).clamp(0.5, 200.0);
                    }
                    MouseScrollDelta::PixelDelta(p) => {
                        let y = p.y as f32 / 60.0;
                        self.radius = (self.radius / self.zoom_sens.powf(y)).clamp(0.5, 200.0);
                    }
                }
            }

            WindowEvent::Focused(false) => {
                self.mmb_down = false;
            }
            _ => {}
        }
    }
}

impl ImguiRenderableMutWithContext<(&GameState, &mut bool, &mut WindowManager)> for OrbitCamera {
    fn render_mut_with_context(
        &mut self,
        ui: &imgui::Ui,
        (game_state, opened, window_manager): (&GameState, &mut bool, &mut WindowManager),
    ) {
        window_manager.render_window(
            ui,
            "Camera",
            &anchor::TOP_RIGHT,
            AUTO_RESIZE,
            opened,
            || {
                if ui.button("Reset") {
                    *self = Self::new();
                }

                ui.slider("Target X", -100.0, 100.0, &mut self.target.x);
                ui.slider("Target Y", -100.0, 100.0, &mut self.target.y);
                ui.slider("Target Z", -100.0, 100.0, &mut self.target.z);

                ui.separator();

                ui.slider("Distance", 0.5, 200.0, &mut self.radius);
                ui.slider(
                    "Yaw",
                    -std::f32::consts::PI,
                    std::f32::consts::PI,
                    &mut self.yaw,
                );
                ui.slider("Pitch", -1.5533, 1.5533, &mut self.pitch);

                ui.separator();

                ui.text(format!(
                    "Eye: ({:.2}, {:.2}, {:.2})",
                    self.eye().x,
                    self.eye().y,
                    self.eye().z
                ));

                if ui.collapsing_header("Ray from cursor", imgui::TreeNodeFlags::DEFAULT_OPEN) {
                    if let Some(ray) = self.ray_from_cursor() {
                        ui.text(format!(
                            "Cursor ray direction: ({:.2}, {:.2}, {:.2})",
                            ray.dir.x, ray.dir.y, ray.dir.z
                        ));
                        ui.text(format!(
                            "Hit: {:#?}",
                            systems::geometry::raycast_with_toi(
                                &game_state.world,
                                &game_state.geometry,
                                &ray,
                                1000.0,
                                &RaycastFilter::All
                            )
                        ));
                    } else {
                        ui.text("(no cursor ray)");
                    }
                }
            },
        );
    }
}
