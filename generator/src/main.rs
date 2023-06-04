use byteorder::{ByteOrder, LittleEndian};
use clap::{builder::PossibleValue, Parser, ValueEnum};
use haversine::haversine::{CoordinatePair, HaversineData};
use haversine::{distance, earth};
use rand::distributions::Distribution;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::str::FromStr;

#[derive(Clone, Debug)]
enum SelectionAlgorithm {
    Cluster,
    Uniform,
}

impl Display for SelectionAlgorithm {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Cluster => f.write_str("cluster"),
            Self::Uniform => f.write_str("uniform"),
        }
    }
}

impl FromStr for SelectionAlgorithm {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_str() {
            "cluster" => Ok(SelectionAlgorithm::Cluster),
            "uniform" => Ok(SelectionAlgorithm::Uniform),
            _ => Err(()),
        }
    }
}

impl ValueEnum for SelectionAlgorithm {
    fn value_variants<'a>() -> &'a [Self] {
        &[SelectionAlgorithm::Cluster, SelectionAlgorithm::Uniform]
    }
    fn to_possible_value(&self) -> Option<PossibleValue> {
        match self {
            SelectionAlgorithm::Cluster => Some(PossibleValue::new("cluster")),
            SelectionAlgorithm::Uniform => Some(PossibleValue::new("uniform")),
        }
    }
}

#[derive(Parser, Debug)]
struct Args {
    #[arg(value_name = "RANDOM_SEED", required = true)]
    seed: u64,
    #[arg(value_name = "COUNT", required = true)]
    count: usize,
    #[arg(default_value_t = SelectionAlgorithm::Cluster, long)]
    algorithm: SelectionAlgorithm,
}

fn write_json(data: &HaversineData, json_filename: &str) {
    let output_file = File::create(json_filename).unwrap();
    let mut writer = BufWriter::new(output_file);

    serde_json::to_writer(&mut writer, &data).unwrap();
    writer.flush().unwrap();
}

fn write_answer(data: &HaversineData, binary_filename: &str) {
    let output_file = File::create(binary_filename).unwrap();
    let mut writer = BufWriter::new(output_file);

    let mut expected_average: f64 = 0.0;

    let mut buf = [0u8; 8];
    for (count, point) in data.pairs.iter().enumerate() {
        let distance = distance::naive(point, earth::RADIUS);
        LittleEndian::write_f64(&mut buf, distance);

        let written = writer.write(&buf).unwrap();
        if written < buf.len() {
            panic!("Failed to write everything")
        }

        expected_average = ((1.0 - (1.0 / (count as f64 + 1.0))) * expected_average)
            + (distance / (count as f64 + 1.0));
    }

    // sic!
    println!("Expected sum: {}", expected_average);

    LittleEndian::write_f64(&mut buf, expected_average);
    let written = writer.write(&buf).unwrap();
    if written < buf.len() {
        panic!("Failed to write everything")
    }

    writer.flush().unwrap();
}

const CLUSTER_COUNT: usize = 20;

/// Returns the smaller one then the larger one.
fn sample_two<R, D>(range: D, rng: &mut R) -> (f64, f64)
where
    R: Rng,
    D: Distribution<f64>,
{
    let x1 = range.sample(rng);
    let x2 = range.sample(rng);

    if x1 < x2 {
        (x1, x2)
    } else {
        (x2, x1)
    }
}

fn sample_point<R, D1, D2>(rng: &mut R, x_range: D1, y_range: D2) -> CoordinatePair
where
    R: Rng,
    D1: Distribution<f64>,
    D2: Distribution<f64>,
{
    CoordinatePair {
        x0: x_range.sample(rng),
        y0: y_range.sample(rng),
        x1: x_range.sample(rng),
        y1: y_range.sample(rng),
    }
}

fn main() {
    let args = Args::parse();
    println!("Method: {}", args.algorithm);
    println!("Random seed: {}", args.seed);
    println!("Pair count: {}", args.count);

    let mut v = Vec::with_capacity(args.count);
    let mut rng = StdRng::seed_from_u64(args.seed);

    let x_range = rand::distributions::Uniform::from(-180.0..180.0);
    let y_range = rand::distributions::Uniform::from(-90.0..90.0);

    match args.algorithm {
        SelectionAlgorithm::Uniform => {
            for _ in 0usize..args.count {
                let point = sample_point(&mut rng, x_range, y_range);
                v.push(point);
            }
        }
        SelectionAlgorithm::Cluster => {
            if args.count % CLUSTER_COUNT != 0 {
                panic!(
                    "Number of points {} is not a multiple of cluster count {CLUSTER_COUNT}",
                    args.count
                );
            }

            for _ in 0..CLUSTER_COUNT {
                let (min_x, max_x) = sample_two(x_range, &mut rng);
                let (min_y, max_y) = sample_two(y_range, &mut rng);

                let x_range = rand::distributions::Uniform::from(min_x..max_x);
                let y_range = rand::distributions::Uniform::from(min_y..max_y);

                for _ in 0..args.count / CLUSTER_COUNT {
                    let point = sample_point(&mut rng, x_range, y_range);
                    v.push(point);
                }
            }
        }
    }

    let data = HaversineData { pairs: v };
    let json_filename = format!("data_{}_flex.json", args.count);
    write_json(&data, &json_filename);

    let answer_filename = format!("data_{}_haveranswer.f64", args.count);
    write_answer(&data, &answer_filename);
}
