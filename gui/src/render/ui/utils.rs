use std::fmt::Display;

pub trait ImguiRenderable {
    fn render(&self, ui: &imgui::Ui);
}

pub trait ImguiRenderableMut {
    fn render_mut(&mut self, ui: &imgui::Ui);
}

pub trait ImguiRenderableWithContext<C> {
    fn render_with_context(&self, ui: &imgui::Ui, context: C);
}

pub trait ImguiRenderableMutWithContext<C> {
    fn render_mut_with_context(&mut self, ui: &imgui::Ui, context: C);
}

#[macro_export]
macro_rules! table_with_columns {
    ($ui:expr, $table_name:expr, $( $col_name:expr ),+ $(,)? ) => {{
    use imgui::TableColumnSetup;
    use imgui::TableColumnFlags;
    use imgui::Id;
    use imgui::TableFlags;

    fn table_column(name: &str) -> TableColumnSetup<&str> {
        TableColumnSetup {
            name,
            flags: TableColumnFlags::NO_HIDE,
            init_width_or_weight: 0.0,
            user_id: Id::default(),
        }
    }

    let columns = [ $( table_column($col_name) ),+ ];
    $ui.begin_table_header_with_flags(
        $table_name,
        columns,
        TableFlags::SIZING_FIXED_FIT
        | TableFlags::BORDERS
        | TableFlags::ROW_BG
        | TableFlags::NO_HOST_EXTEND_X,
    )
    }};
}

pub fn roman_numeral(level: u8) -> &'static str {
    match level {
        1 => "I",
        2 => "II",
        3 => "III",
        4 => "IV",
        5 => "V",
        6 => "VI",
        7 => "VII",
        8 => "VIII",
        9 => "IX",
        _ => "?",
    }
}

// TODO: Look into using imgui::Selectable instead
pub static SELECTED_BUTTON_COLOR: [f32; 4] = [0.25, 0.6, 1.0, 1.0];

pub fn render_button_selectable(
    ui: &imgui::Ui,
    label: String,
    button_size: [f32; 2],
    selected: bool,
) -> bool {
    let style_color = if selected {
        ui.push_style_color(imgui::StyleColor::Button, SELECTED_BUTTON_COLOR)
    } else {
        ui.push_style_color(
            imgui::StyleColor::Button,
            ui.style_color(imgui::StyleColor::Button),
        )
    };

    let clicked = ui.button_with_size(label, button_size);

    style_color.pop();

    clicked
}

pub fn labels_max_width<I, S>(ui: &imgui::Ui, labels: I) -> f32
where
    I: Clone + IntoIterator<Item = S>,
    S: AsRef<str>,
{
    labels
        .into_iter()
        .map(|label| ui.calc_text_size(label.as_ref())[0])
        .fold(0.0, f32::max)
}

static DEFAULT_PADDING: [f32; 2] = [20.0, 5.0];

pub fn render_button_with_padding(ui: &imgui::Ui, label: &str, padding: [f32; 2]) -> bool {
    let text_size = ui.calc_text_size(label);
    let size = [
        text_size[0] + padding[0] * 2.0,
        text_size[1] + padding[1] * 2.0,
    ];
    ui.button_with_size(label, size)
}

pub fn render_uniform_buttons<I, S>(ui: &imgui::Ui, labels: I) -> Option<usize>
where
    I: Clone + IntoIterator<Item = S>,
    S: AsRef<str>,
{
    render_uniform_buttons_with_padding(ui, labels, DEFAULT_PADDING)
}

/// Renders a vertical list of same-width buttons with centered text.
/// Returns the index of the clicked button, if any.
pub fn render_uniform_buttons_with_padding<I, S>(
    ui: &imgui::Ui,
    labels: I,
    padding: [f32; 2],
) -> Option<usize>
where
    I: Clone + IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let iter = labels.clone().into_iter();

    // Pass 1: find max width
    let max_width = iter
        .map(|label| ui.calc_text_size(label.as_ref())[0])
        .fold(0.0, f32::max)
        + padding[0] * 2.0;

    let mut clicked_index = None;

    // Pass 2: render
    let style = ui.push_style_var(imgui::StyleVar::ButtonTextAlign([0.5, 0.5]));
    for (i, label) in labels.into_iter().enumerate() {
        let height = ui.calc_text_size(label.as_ref())[1] + padding[1] * 2.0;
        if ui.button_with_size(label.as_ref(), [max_width, height]) {
            clicked_index = Some(i);
        }
    }
    style.pop();

    clicked_index
}

#[macro_export]
macro_rules! buttons {
    ($($label:expr => $action:expr),+ $(,)?) => {
        vec![$(($label, Box::new($action) as _)),+]
    };
}

pub fn render_button_disabled_conditionally(
    ui: &imgui::Ui,
    label: &str,
    size: [f32; 2],
    condition: bool,
    tooltip: &str,
) -> bool {
    let disabled_token = ui.begin_disabled(condition);

    let clicked = ui.button_with_size(label, size);

    disabled_token.end();

    if ui.is_item_hovered_with_flags(imgui::HoveredFlags::ALLOW_WHEN_DISABLED)
        && condition
        && !tooltip.is_empty()
    {
        ui.tooltip_text(tooltip);
    }

    clicked
}

pub fn render_empty_button(ui: &imgui::Ui, label: &str) {
    let disabled = ui.begin_disabled(true);
    let color = ui.push_style_color(imgui::StyleColor::Button, [0.3, 0.3, 0.3, 1.0]);
    ui.button(label);
    color.pop();
    disabled.end();
}

pub fn render_window_at_cursor<R, F: FnOnce() -> R>(
    ui: &imgui::Ui,
    name: &str,
    always_auto_resize: bool,
    build_fn: F,
) {
    let cursor_pos = ui.io().mouse_pos;
    ui.window(name)
        .position(cursor_pos, imgui::Condition::Once)
        .always_auto_resize(always_auto_resize)
        .build(build_fn);
}

pub fn interpolate_color(color1: [f32; 4], color2: [f32; 4], factor: f32) -> [f32; 4] {
    [
        color2[0] + (color1[0] - color2[0]) * factor,
        color2[1] + (color1[1] - color2[1]) * factor,
        color2[2] + (color1[2] - color2[2]) * factor,
        color2[3] + (color1[3] - color2[3]) * factor,
    ]
}

pub struct ProgressBarColor {
    pub color_full: [f32; 4],
    pub color_empty: [f32; 4],
    pub color_full_bg: [f32; 4],
    pub color_empty_bg: [f32; 4],
}

pub fn render_progress_bar<T: Display>(
    ui: &imgui::Ui,
    current: T,
    max: T,
    temp_bonus: Option<T>,
    fraction: f32,
    width: f32,
    label: &str,
    units: Option<&str>,
    progress_bar_color: Option<ProgressBarColor>,
) {
    let mut text = format!("{} / {}", current, max);
    if let Some(temp) = temp_bonus {
        text = format!("{} (+{})", text, temp);
    }
    if let Some(units) = units {
        text = format!("{} {}", text, units);
    }
    // Reserve vertical space for the taller widget (the progress bar)
    let line_height = ui.text_line_height_with_spacing();
    let bar_height = line_height;
    let y_offset = (bar_height - line_height) * 0.5;
    ui.dummy([0.0, y_offset.max(0.0)]); // move down a little if needed

    // "HP" label
    ui.text(label);
    ui.same_line();

    // Style colors (interpolated based on health fraction)
    let (foreground, background) = if let Some(progress_bar_color) = progress_bar_color {
        let (color, color_bg) = {
            (
                interpolate_color(
                    progress_bar_color.color_full,
                    progress_bar_color.color_empty,
                    fraction,
                ),
                interpolate_color(
                    progress_bar_color.color_full_bg,
                    progress_bar_color.color_empty_bg,
                    fraction,
                ),
            )
        };
        let foreground = ui.push_style_color(imgui::StyleColor::PlotHistogram, color);
        let background = ui.push_style_color(imgui::StyleColor::FrameBg, color_bg);

        (Some(foreground), Some(background))
    } else {
        (None, None)
    };

    imgui::ProgressBar::new(fraction)
        .size([width, bar_height])
        .overlay_text(&text)
        .build(ui);

    // Pop the style colors
    if let Some(background) = background
        && let Some(foreground) = foreground
    {
        background.pop();
        foreground.pop();
    }
}

static COLOR_EMPTY: [f32; 4] = [0.4, 0.4, 0.4, 1.0];
static COLOR_FULL: [f32; 4] = [1.0, 1.0, 1.0, 1.0];

// TODO: Call this something else
pub fn render_capacity_meter(ui: &imgui::Ui, label: &str, current: usize, max: usize) {
    for i in 0..max {
        if i > 0 {
            ui.same_line();
        }
        let color = if i >= current {
            COLOR_EMPTY
        } else {
            COLOR_FULL
        };
        // TODO: Consider using primitives instead of buttons (ui.get_window_draw_list())
        // need to figure out how to do the positioning then
        let color_token = ui.push_style_color(imgui::StyleColor::Button, color);
        ui.button_with_size(format!("##{}{}/{}", label, current, max), [15.0, 15.0]);
        color_token.pop();
    }
}

pub fn sign(value: &i32) -> &'static str {
    if *value >= 0 { "+" } else { "-" }
}

pub fn signed_value(value: &i32) -> String {
    format!("{}{}", sign(value), value.abs())
}
