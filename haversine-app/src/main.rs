use byteorder::{ByteOrder, LittleEndian};
use clap::Parser;
use haversine::haversine::CoordinatePair;
use haversine::{distance, earth};
use json::json_object::JsonValue;
use std::fs::File;
use std::io::{BufReader, Read};
use utf8_read::Reader;

#[derive(Parser, Debug)]
struct Args {
    #[arg(value_name = "INPUT_JSON", required = true)]
    input_json: String,
    #[arg(value_name = "EXPECTED_F64", required = true)]
    expected: String,
}

fn read_answer(binary_filename: &str) -> (Vec<f64>, f64) {
    let mut file = File::open(binary_filename).unwrap();
    let file_size = file.metadata().unwrap().len();
    if file_size % 8 != 0 {
        panic!(
            "Malformed input file of size {} is not a multiple of 8",
            file_size
        )
    }
    let num_floats = file_size / 8;
    assert_ne!(num_floats, 0, "Empty file");
    let num_bytes = num_floats - 1;

    let mut data = Vec::with_capacity(num_bytes as usize);
    let mut buf = [0u8, 8];

    for _ in 0..num_floats {
        let bytes_read = file.read(&mut buf).unwrap();
        if bytes_read < 8 {
            panic!("Not enough bytes read")
        }

        data.push(LittleEndian::read_f64(&buf));
    }

    let bytes_read = file.read(&mut buf).unwrap();
    if bytes_read < 8 {
        panic!("Not enough bytes read")
    }

    (data, LittleEndian::read_f64(&buf))
}

fn read_json(json_filename: &str) -> Vec<CoordinatePair> {
    let file = File::open(json_filename).unwrap();
    let reader = BufReader::new(file);
    let mut decoder = Reader::new(reader);
    match JsonValue::parse(&mut decoder.into_iter().map(|x| x.unwrap())).unwrap() {
        (JsonValue::Object(o), None) => {
            let a = o.values.get("pairs").unwrap().as_array();
            a.iter()
                .map(|o| {
                    let o = o.as_object();
                    CoordinatePair {
                        x0: o.get("x0").unwrap().as_number(),
                        y0: o.get("y0").unwrap().as_number(),
                        x1: o.get("x1").unwrap().as_number(),
                        y1: o.get("y1").unwrap().as_number(),
                    }
                })
                .collect()
        }
        other => {
            panic!("Expected a JSON object, got: {:?}", other)
        }
    }
}

fn haversine_sum(v: &[CoordinatePair]) -> f64 {
    let mut answer = 0.0_f64;
    for pair in v {
        answer += distance::naive(pair, earth::RADIUS);
    }
    answer
}

fn main() {
    let args = Args::parse();
    let input = read_json(&args.input_json);
    let (_expected_values, expected_sum) = read_answer(&args.expected);
    println!("Pair count: {}", input.len());

    println!("Validation:");
    println!("Reference sum: {}", expected_sum);
    let actual_sum = haversine_sum(&input);
    println!("Difference: {}", f64::abs(expected_sum - actual_sum));
}
