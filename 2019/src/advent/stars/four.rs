use itertools::Itertools;
use crate::advent::read_input;

type Criterion = fn(u32) -> bool;
type Criteria = Vec<Criterion>;

fn any_char_pairs(string: String, compare: fn((char, char)) -> bool) -> bool {
    string.chars().zip(string.chars().skip(1)).any(compare)
}

fn dig(c: char) -> u32 {
    c.to_digit(10).expect("Wanted a number!")
}

const rule1: Criterion = { |i| i.to_string().len() == 6 };
const rule2: Criterion = { |i| any_char_pairs(i.to_string(), |(x, y)| x == y) };
const rule3: Criterion = { |i| !any_char_pairs(i.to_string(), |(x, y)| dig(x) > dig(y)) };

// there is one group that is only 2 digits
//
const rule4: Criterion = { |i|
    i.to_string().chars().sorted().group_by(|x| *x).into_iter().any(|x| x.1.into_iter().count() == 2)
};

fn rules() -> Criteria {
    vec![rule1, rule2, rule3]
}

fn qualifies(input: u32) -> bool {
    rules().iter().all(|rule| rule(input))
}

fn strict_qualifies(input: u32) -> bool {
    qualifies(input) && rule4(input)
}

pub fn stars() {
    let input = read_input(4);
    let mut itr = input.split("-");
    let min: u32 = itr.next().unwrap().parse().expect("required a number");
    let max: u32 = itr.next().unwrap().parse().expect("required a number");

    println!("{}", (min..max).filter(|&i| qualifies(i)).count());
    println!("{}", (min..max).filter(|&i| strict_qualifies(i)).count());
}

#[test]
fn test_examples() {
    assert!(qualifies(111111));
    assert!(!qualifies(123789));
    assert!(!qualifies(223450));    
}
