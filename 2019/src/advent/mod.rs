use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Lines;

pub mod stars;

pub fn input_lines(day: u8) -> Lines<BufReader<File>> {
    let filename = format!("input/{:02}.input", day);
    match File::open(filename) {
        Err(why) => panic!("File not found: {}", why),
        Ok(file) => BufReader::new(file).lines()
    }
}
