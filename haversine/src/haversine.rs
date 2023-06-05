use std::fmt::{Debug, Formatter};

pub struct CoordinatePair {
    pub x0: f64,
    pub y0: f64,
    pub x1: f64,
    pub y1: f64,
}

impl Debug for CoordinatePair {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "x0: {} ({:?}), y0: {} ({:?}), x1: {} ({:?}), y1: {} ({:?})",
            self.x0,
            self.x0.to_be_bytes(),
            self.y0,
            self.y0.to_be_bytes(),
            self.x1,
            self.x1.to_be_bytes(),
            self.y1,
            self.y1.to_be_bytes()
        ))
    }
}

pub struct HaversineData {
    pub pairs: Vec<CoordinatePair>,
}
