use serde::Serialize;

#[derive(Serialize)]
pub struct CoordinatePair {
    pub x0: f64,
    pub y0: f64,
    pub x1: f64,
    pub y1: f64,
}

#[derive(Serialize)]
pub struct HaversineData {
    pub pairs: Vec<CoordinatePair>,
}
