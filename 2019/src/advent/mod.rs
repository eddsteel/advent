use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Lines;
use std::str::FromStr;
use std::fmt::Debug;

pub mod stars;

pub fn input_lines(day: u8) -> Lines<BufReader<File>> {
    let filename = format!("input/{:02}.input", day);
    match File::open(filename) {
        Err(why) => panic!("File not found: {}", why),
        Ok(file) => BufReader::new(file).lines()
    }
}

pub fn fold_input_values<A, V>(day: u8, initial: A, fold: fn(A, V) -> A) -> A
where V: FromStr, <V as FromStr>::Err: Debug
{
    input_lines(day).fold(initial, |a, l| {
        fold(a, l.unwrap().parse::<V>().expect("Bad input!"))
    })
}
