use crate::advent;

// i32 div rounds towards zero.
fn fuel(mass: i32) -> i32 {
    std::cmp::max(0, mass / 3 - 2)
}

fn agg_fuel(mass: i32) -> i32 {
    fn inner(mass: i32, total: i32) -> i32 {
        let fuel_amt = fuel(mass);
        if fuel_amt == 0 {
            total + fuel_amt
        } else {
            inner(fuel_amt, total + fuel_amt)
        }
    }

    inner(mass, 0)
}

#[test]
fn test_examples() {
    assert_eq!(fuel(12), 2);
    assert_eq!(fuel(14), 2);
    assert_eq!(fuel(1969), 654);
    assert_eq!(fuel(100756), 33583);

    assert_eq!(agg_fuel(14), 2);
    assert_eq!(agg_fuel(1969), 966);
    assert_eq!(agg_fuel(100756), 50346);
}

pub fn star1() {
    let answer = advent::fold_input_values(1, 0, |total, mass| {
        total + fuel(mass)
    });

    println!("{}", answer);
}

pub fn star2() {
    let answer = advent::fold_input_values(1, 0, |total, mass| {
        total + agg_fuel(mass)
    });

    println!("{}", answer);
}
