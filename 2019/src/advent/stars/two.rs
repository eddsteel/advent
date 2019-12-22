use crate::advent;
use crate::advent::opcode_computer::Computer;

pub fn star1() {
    let line = advent::input_lines(2).next().expect("No input");
    let mut computer = line.unwrap().parse::<Computer>().expect("Bad input");
    computer.init(12, 2);    
    computer.run();

    println!("{}", computer.get_val(0))
}

pub fn star2() {
    let target = 19690720;
    let line = advent::input_lines(2).next().expect("No input").unwrap();
    for noun in 0..99 {
        for verb in 0..99 {
            let mut computer = line.parse::<Computer>().expect("Bad input");
            computer.init(noun, verb);
            computer.run();
            if computer.get_val(0) == target {
                println!("{}{}", noun, verb);
                return;
            }
        }
    }
}

