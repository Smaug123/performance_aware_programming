#[derive(Debug)]
pub struct CoordinatePair {
    pub x0: f64,
    pub y0: f64,
    pub x1: f64,
    pub y1: f64,
}

pub struct HaversineData {
    pub pairs: Vec<CoordinatePair>,
}
