use std::collections::{BTreeMap, HashMap};

pub static AUTO_RESIZE: [f32; 2] = [0.0, 0.0];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HorizontalAnchor {
    Left,
    Center,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VerticalAnchor {
    Top,
    Center,
    Bottom,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WindowAnchor {
    pub horizontal: HorizontalAnchor,
    pub vertical: VerticalAnchor,
}

impl WindowAnchor {
    pub fn new(horizontal: HorizontalAnchor, vertical: VerticalAnchor) -> Self {
        Self {
            horizontal,
            vertical,
        }
    }
}

pub static TOP_LEFT: WindowAnchor = WindowAnchor {
    horizontal: HorizontalAnchor::Left,
    vertical: VerticalAnchor::Top,
};

pub static TOP_CENTER: WindowAnchor = WindowAnchor {
    horizontal: HorizontalAnchor::Center,
    vertical: VerticalAnchor::Top,
};

pub static TOP_RIGHT: WindowAnchor = WindowAnchor {
    horizontal: HorizontalAnchor::Right,
    vertical: VerticalAnchor::Top,
};

pub static CENTER_LEFT: WindowAnchor = WindowAnchor {
    horizontal: HorizontalAnchor::Left,
    vertical: VerticalAnchor::Center,
};

pub static CENTER: WindowAnchor = WindowAnchor {
    horizontal: HorizontalAnchor::Center,
    vertical: VerticalAnchor::Center,
};

pub static CENTER_RIGHT: WindowAnchor = WindowAnchor {
    horizontal: HorizontalAnchor::Right,
    vertical: VerticalAnchor::Center,
};

pub static BOTTOM_LEFT: WindowAnchor = WindowAnchor {
    horizontal: HorizontalAnchor::Left,
    vertical: VerticalAnchor::Bottom,
};

pub static BOTTOM_CENTER: WindowAnchor = WindowAnchor {
    horizontal: HorizontalAnchor::Center,
    vertical: VerticalAnchor::Bottom,
};

pub static BOTTOM_RIGHT: WindowAnchor = WindowAnchor {
    horizontal: HorizontalAnchor::Right,
    vertical: VerticalAnchor::Bottom,
};

#[derive(Debug, Clone)]
pub struct AnchoredWindow {
    pub label: String,
    pub anchor: WindowAnchor,
    pub prev_size: [f32; 2],
    pub rendered_prev_frame: bool,
}

impl AnchoredWindow {
    pub fn new(label: String, anchor: WindowAnchor) -> Self {
        Self {
            label,
            anchor,
            prev_size: [0.0, 0.0],
            rendered_prev_frame: false,
        }
    }

    pub fn update_size(&mut self, size: [f32; 2]) {
        self.prev_size = size;
    }
}

#[derive(Debug, Clone)]
pub struct WindowManager {
    pub windows_by_label: BTreeMap<String, AnchoredWindow>,
    pub windos_by_anchor: HashMap<WindowAnchor, Vec<String>>,
    rendered_this_frame: BTreeMap<String, bool>,
}

impl WindowManager {
    pub fn new() -> Self {
        Self {
            windows_by_label: BTreeMap::new(),
            windos_by_anchor: HashMap::new(),
            rendered_this_frame: BTreeMap::new(),
        }
    }

    pub fn add(&mut self, window: AnchoredWindow) {
        let window_label = window.label.clone();
        self.windos_by_anchor
            .entry(window.anchor)
            .or_default()
            .push(window_label);
        self.windows_by_label.insert(window.label.clone(), window);
    }

    fn add_if_missing(&mut self, label: &str, anchor: &WindowAnchor) {
        if !self.windows_by_label.contains_key(label) {
            let window = AnchoredWindow::new(label.to_string(), *anchor);
            self.add(window);
        }
    }

    fn window_position(&self, label: &str, ui: &imgui::Ui) -> Option<[f32; 2]> {
        let window = self.windows_by_label.get(label)?;

        let viewport = ui.io().display_size;
        let size = window.prev_size;

        // Take into account that there can be multiple windows with the same anchor
        let anchor_windows = self.windos_by_anchor.get(&window.anchor)?;
        // Initial anchor position
        let (mut x, mut y) = (
            match window.anchor.horizontal {
                HorizontalAnchor::Left => 0.0,
                HorizontalAnchor::Center => (viewport[0] - size[0]) / 2.0,
                HorizontalAnchor::Right => viewport[0] - size[0],
            },
            match window.anchor.vertical {
                VerticalAnchor::Top => 0.0,
                VerticalAnchor::Center => (viewport[1] - size[1]) / 2.0,
                VerticalAnchor::Bottom => viewport[1] - size[1],
            },
        );
        // Offset by previous windows with the same anchor
        for window_label in anchor_windows {
            if window_label == label {
                break;
            }

            if let Some(prev_window) = self.windows_by_label.get(window_label)
                && prev_window.rendered_prev_frame
            {
                // Windows on the sides offset horizontally towards center
                // Windows on top/bottom offset vertically towards center
                match window.anchor.horizontal {
                    HorizontalAnchor::Left => {
                        x += prev_window.prev_size[0];
                        continue;
                    }
                    HorizontalAnchor::Right => {
                        x -= prev_window.prev_size[0];
                        continue;
                    }
                    HorizontalAnchor::Center => {}
                }

                match window.anchor.vertical {
                    VerticalAnchor::Top => {
                        y += prev_window.prev_size[1];
                        continue;
                    }
                    VerticalAnchor::Bottom => {
                        y -= prev_window.prev_size[1];
                        continue;
                    }
                    VerticalAnchor::Center => {}
                }
            }
        }

        Some([x, y])
    }

    pub fn render_window<R, F: FnOnce() -> R>(
        &mut self,
        ui: &imgui::Ui,
        label: &str,
        anchor: &WindowAnchor,
        size: [f32; 2],
        opened: &mut bool,
        build_fn: F,
    ) {
        if !*opened {
            return;
        }

        self.add_if_missing(label, anchor);

        let position = self.window_position(label, ui).unwrap_or([0.0, 0.0]);

        let mut actual_size = None;

        ui.window(label)
            .position(position, imgui::Condition::Always)
            .size(size, imgui::Condition::Always)
            .movable(false)
            .opened(opened)
            .build(|| {
                actual_size = Some(ui.window_size());
                build_fn();
            });

        if let Some(window) = self.windows_by_label.get_mut(label)
            && let Some(size) = actual_size
        {
            window.update_size(size);
            self.rendered_this_frame.insert(label.to_string(), true);
        }
    }

    pub fn new_frame(&mut self) {
        for window in self.windows_by_label.values_mut() {
            window.rendered_prev_frame = self
                .rendered_this_frame
                .get(&window.label)
                .cloned()
                .unwrap_or(false);
        }
        self.rendered_this_frame.clear();
    }
}
