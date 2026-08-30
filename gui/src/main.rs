use std::{
    num::NonZeroU32,
    time::{Instant, SystemTime, UNIX_EPOCH},
};

mod render;
mod state;
mod utils;
mod windows;

use glow::HasContext;
use glutin::surface::GlSurface;
use tracing_subscriber::{
    EnvFilter,
    fmt::{format::Writer, time::FormatTime},
};

use crate::{
    render::ui::utils::ImguiRenderableMut, state::gui_state::GuiState,
    windows::main_menu::MainMenuWindow,
};

fn main() {
    init_logging();

    let (event_loop, window, surface, context) = utils::create_window("Hello, triangle!", None);
    let (mut winit_platform, mut imgui_context) = utils::imgui_init(&window);
    let gl = utils::glow_context(&context);

    let mut gui_state = GuiState::new(gl, &mut imgui_context);
    let mut show_settings = false;

    let mut last_frame = Instant::now();

    let mut main_menu = MainMenuWindow::new();

    #[allow(deprecated)]
    event_loop
        .run(move |event, window_target| match event {
            winit::event::Event::NewEvents(_) => {
                let now = Instant::now();
                imgui_context
                    .io_mut()
                    .update_delta_time(now.duration_since(last_frame));
                last_frame = now;
            }

            winit::event::Event::AboutToWait => {
                winit_platform
                    .prepare_frame(imgui_context.io_mut(), &window)
                    .unwrap();

                window.request_redraw();
            }

            winit::event::Event::WindowEvent {
                event: winit::event::WindowEvent::RedrawRequested,
                ..
            } => {
                let ui = imgui_context.frame();

                gui_state.new_frame(&window);

                let prev_show_settings = show_settings;

                if ui.is_key_pressed(imgui::Key::Escape) {
                    show_settings = !show_settings;
                }

                if show_settings {
                    ui.window("Settings")
                        .position([0.0, 0.0], imgui::Condition::FirstUseEver)
                        .size([0.0, 300.0], imgui::Condition::Always)
                        .opened(&mut show_settings)
                        .focus_on_appearing(true)
                        .build(|| {
                            if !prev_show_settings {
                                ui.set_keyboard_focus_here();
                            }
                            gui_state.settings.render_mut(ui);
                        });
                }

                main_menu.render(ui, &mut gui_state);

                render_imgui_windows(&mut gui_state, ui);

                winit_platform.prepare_render(ui, &window);
                let draw_data = imgui_context.render();

                gui_state
                    .ig_renderer
                    .render(draw_data)
                    .expect("error rendering imgui");

                surface
                    .swap_buffers(&context)
                    .expect("Failed to swap buffers");
            }

            winit::event::Event::WindowEvent {
                event: winit::event::WindowEvent::CloseRequested,
                ..
            } => {
                window_target.exit();
            }

            winit::event::Event::WindowEvent {
                event: winit::event::WindowEvent::Resized(new_size),
                ..
            } => {
                if new_size.width > 0 && new_size.height > 0 {
                    surface.resize(
                        &context,
                        NonZeroU32::new(new_size.width).unwrap(),
                        NonZeroU32::new(new_size.height).unwrap(),
                    );
                    unsafe {
                        gui_state.gl_context().viewport(
                            0,
                            0,
                            new_size.width as i32,
                            new_size.height as i32,
                        );
                    }
                }
                winit_platform.handle_event(imgui_context.io_mut(), &window, &event);
            }

            // winit::event::Event::WindowEvent {
            //     event: winit::event::WindowEvent::ScaleFactorChanged { new_inner_size, .. },
            //     ..
            // } => {
            //     // new_inner_size is &mut; read and resize the surface + viewport
            //     let (w, h) = (new_inner_size.width, new_inner_size.height);
            //     if w > 0 && h > 0 {
            //         surface.resize(
            //             &context,
            //             NonZeroU32::new(w).unwrap(),
            //             NonZeroU32::new(h).unwrap(),
            //         );
            //         unsafe {
            //             gl.viewport(0, 0, w as i32, h as i32);
            //         }
            //     }
            //     winit_platform.handle_event(imgui_context.io_mut(), &window, &event);
            // }
            winit::event::Event::WindowEvent {
                event: ref window_event,
                ..
            } => {
                let wants_mouse = imgui_context.io().want_capture_mouse;
                gui_state.camera.handle_event(window_event, wants_mouse);
                winit_platform.handle_event(imgui_context.io_mut(), &window, &event);
            }

            winit::event::Event::LoopExiting => {}

            event => {
                winit_platform.handle_event(imgui_context.io_mut(), &window, &event);
            }
        })
        .expect("EventLoop error");
}

fn render_imgui_windows(gui_state: &mut GuiState, ui: &mut imgui::Ui) {
    let show_demo = gui_state
        .settings
        .get_mut::<bool>(state::parameters::RENDER_IMGUI_DEMO);
    if *show_demo {
        ui.show_demo_window(show_demo);
    }

    let show_about = gui_state
        .settings
        .get_mut::<bool>(state::parameters::RENDER_IMGUI_ABOUT);
    if *show_about {
        ui.show_about_window(show_about);
    }

    let show_metrics = gui_state
        .settings
        .get_mut::<bool>(state::parameters::RENDER_IMGUI_METRICS);
    if *show_metrics {
        ui.show_metrics_window(show_metrics);
    }

    let show_user_guide = gui_state
        .settings
        .get_mut::<bool>(state::parameters::RENDER_IMGUI_USER_GUIDE);
    if *show_user_guide {
        ui.show_user_guide();
    }
}

const DEFAULT_LOG_LEVEL: &str = "info";

fn init_logging() {
    let mut log_level = DEFAULT_LOG_LEVEL;
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        log_level = &args[1];
    }
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new(log_level));

    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_target(true) // module path == "class name"
        .with_level(true)
        .with_timer(FixedMillisUtcTime)
        .with_ansi(true)
        .init();
}

pub struct FixedMillisUtcTime;

impl FormatTime for FixedMillisUtcTime {
    fn format_time(&self, writer: &mut Writer<'_>) -> std::fmt::Result {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards");

        let seconds = now.as_secs();
        let millis = now.subsec_millis();

        // Convert seconds to UTC date/time
        let datetime = chrono::DateTime::<chrono::Utc>::from(
            UNIX_EPOCH + std::time::Duration::from_secs(seconds),
        );

        write!(
            writer,
            "{}.{:03}Z",
            datetime.format("%Y-%m-%dT%H:%M:%S"),
            millis
        )
    }
}
