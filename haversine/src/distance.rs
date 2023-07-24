use crate::haversine::CoordinatePair;

fn square(x: f64) -> f64 {
    x * x
}

#[must_use]
pub fn naive(point: &CoordinatePair, earth_radius: f64) -> f64 {
    let lat1 = point.y0;
    let lat2 = point.y1;
    let lon1 = point.x0;
    let lon2 = point.x1;

    let lat_deg = (lat2 - lat1).to_radians();
    let lon_deg = (lon2 - lon1).to_radians();
    let lat1 = lat1.to_radians();
    let lat2 = lat2.to_radians();

    let a = square(f64::sin(lat_deg / 2.0))
        + f64::cos(lat1) * f64::cos(lat2) * square(f64::sin(lon_deg / 2.0));
    let c = 2.0 * f64::asin(f64::sqrt(a));

    earth_radius * c
}
