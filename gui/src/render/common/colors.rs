#[derive(Debug, Clone, Copy)]
pub enum Color {
    Black,
    Blue,
    Cyan,
    Gray,
    Green,
    LightGray,
    Magenta,
    Orange,
    Purple,
    Red,
    White,
    Yellow,
}

impl Color {
    pub fn with_alpha(self, alpha: f32) -> [f32; 4] {
        let [r, g, b]: [f32; 3] = self.into();
        [r, g, b, alpha]
    }
}

impl Into<[f32; 4]> for Color {
    fn into(self) -> [f32; 4] {
        match self {
            Color::Black => [0.0, 0.0, 0.0, 1.0],
            Color::Blue => [0.0, 0.0, 1.0, 1.0],
            Color::Cyan => [0.0, 1.0, 1.0, 1.0],
            Color::Gray => [0.5, 0.5, 0.5, 1.0],
            Color::Green => [0.0, 1.0, 0.0, 1.0],
            Color::LightGray => [0.75, 0.75, 0.75, 1.0],
            Color::Magenta => [1.0, 0.0, 1.0, 1.0],
            Color::Red => [1.0, 0.0, 0.0, 1.0],
            Color::White => [1.0, 1.0, 1.0, 1.0],
            Color::Orange => [1.0, 0.5, 0.0, 1.0],
            Color::Purple => [0.5, 0.0, 1.0, 1.0],
            Color::Yellow => [1.0, 1.0, 0.0, 1.0],
        }
    }
}

impl Into<[f32; 3]> for Color {
    fn into(self) -> [f32; 3] {
        let [r, g, b, _a]: [f32; 4] = self.into();
        [r, g, b]
    }
}
