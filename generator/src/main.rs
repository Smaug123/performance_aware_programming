use byteorder::{ByteOrder, LittleEndian};
use clap::{builder::PossibleValue, Parser, ValueEnum};
use haversine::distance;
use haversine::haversine::{CoordinatePair, HaversineData};
use rand::distributions::Distribution;
use rand::rngs::StdRng;
use rand::SeedableRng;
use serde_json::Serializer;
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

    let mut expected_sum: f64 = 0.0;

    let mut buf = [0u8; 8];
    for (count, point) in data.pairs.iter().enumerate() {
        let distance = distance::naive(point);
        LittleEndian::write_f64(&mut buf, distance);

        let written = writer.write(&buf).unwrap();
        if written < buf.len() {
            panic!("Failed to write everything")
        }

        expected_sum = expected_sum * (count as f64 / ((count + 1) as f64))
            + (distance / ((count + 1) as f64));
    }

    println!("Expected sum: {}", expected_sum);

    LittleEndian::write_f64(&mut buf, expected_sum);
    let written = writer.write(&buf).unwrap();
    if written < buf.len() {
        panic!("Failed to write everything")
    }

    writer.flush().unwrap();
}

fn main() {
    let args = Args::parse();
    println!("Method: {}", args.algorithm);
    println!("Random seed: {}", args.seed);
    println!("Pair count: {}", args.count);

    let mut v = Vec::with_capacity(args.count);
    let mut rng = StdRng::seed_from_u64(args.seed);

    match args.algorithm {
        SelectionAlgorithm::Uniform => {
            for count in 0usize..args.count {
                let x_range = rand::distributions::Uniform::from(-180.0..180.0);
                let y_range = rand::distributions::Uniform::from(-90.0..90.0);
                let point = CoordinatePair {
                    x0: x_range.sample(&mut rng),
                    y0: y_range.sample(&mut rng),
                    x1: x_range.sample(&mut rng),
                    y1: y_range.sample(&mut rng),
                };

                v.push(point);
            }
        }
        SelectionAlgorithm::Cluster => {
            for count in 0usize..args.count {
                let x_range = rand::distributions::Uniform::from(-180.0..180.0);
                let y_range = rand::distributions::Uniform::from(-90.0..90.0);
                let point = CoordinatePair {
                    x0: x_range.sample(&mut rng),
                    y0: y_range.sample(&mut rng),
                    x1: x_range.sample(&mut rng),
                    y1: y_range.sample(&mut rng),
                };

                v.push(point);
            }
        }
    }

    let data = HaversineData { pairs: v };
    let json_filename = format!("data_{}_flex.json", args.count);
    write_json(&data, &json_filename);

    let answer_filename = format!("data_{}_haveranswer.f64", args.count);
    write_answer(&data, &answer_filename);
}
