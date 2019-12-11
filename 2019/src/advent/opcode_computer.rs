use std::convert::TryInto;
use std::num::ParseIntError;
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq)]
pub struct Computer {
    memory: Vec<u32>,
    instruction_counter: usize,
    running: bool
}

#[derive(Debug, PartialEq, Eq)]
pub enum OpCode {
    Add(usize, usize, usize),
    Mult(usize, usize, usize),
    Halt,
    CatchFire
}


impl FromStr for Computer {
    type Err = ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let memory: Vec<u32> = s.split(',').map(|s| s.parse().expect("fail")).collect();
        Ok(Computer::new(memory))
    }
}

impl Computer {
    pub fn new(memory: Vec<u32>) -> Computer {
        Computer { memory: memory, instruction_counter: 0, running: true }
    }

    pub fn init(&mut self, val1: u32, val2: u32) {
        self.set_val(1, val1);
        self.set_val(2, val2);
    }

    pub fn run(&mut self) {
        while self.running {
            self.step();
        }
    }

    pub fn get_val(&self, location: usize) -> u32 {
        self.memory[location]
    }

    fn get_ref(&self, location: usize) -> usize {
        self.memory[location].try_into().unwrap()
    }

    fn set_val(&mut self, location: usize, value: u32) {
        self.memory[location] = value;
    }

    fn current_val(&self) -> u32 {
        self.memory[self.instruction_counter]
    }

    fn current_op(&self) -> OpCode {
        match self.current_val() {
            1 => {
                OpCode::Add(
                    self.get_ref(self.instruction_counter + 1),
                    self.get_ref(self.instruction_counter + 2),
                    self.get_ref(self.instruction_counter + 3)
                )
            },
            2 => {
                OpCode::Mult(
                    self.get_ref(self.instruction_counter + 1),
                    self.get_ref(self.instruction_counter + 2),
                    self.get_ref(self.instruction_counter + 3)
                )
            }
            99 => OpCode::Halt,
            _ => OpCode::CatchFire
        }
    }

    pub fn step(&mut self) {
        if self.running != false {
            match self.current_op() {
                OpCode::Add(ref1, ref2, dst) => {
                    let r1 = self.get_val(ref1);
                    let r2 = self.get_val(ref2);
                    self.set_val(dst, r1 + r2);
                    self.instruction_counter = self.instruction_counter + 4;
                },
                OpCode::Mult(ref1, ref2, dst) => {
                    let r1 = self.get_val(ref1);
                    let r2 = self.get_val(ref2);
                    self.set_val(dst, r1 * r2);
                    self.instruction_counter = self.instruction_counter + 4;
                },
                OpCode::Halt => self.running = false,
                OpCode::CatchFire => panic!("Caught fire!")                    
            };
        }
    }
}

fn run(comp: &mut Computer) -> &mut Computer {
    comp.run();
    comp
}

#[test]
fn test_program() {
    assert_eq!(Computer::new(vec![1, 0, 0, 0, 99]).current_val(), 1);
    assert_eq!(Computer::new(vec![1, 0, 0, 0, 99]).current_op(), OpCode::Add(0, 0, 0))
}

#[test]
fn test_examples_star2() {
    assert_eq!(run(&mut Computer::new(vec![1, 0, 0, 0, 99])).memory,
               Computer::new(vec![2, 0, 0, 0, 99]).memory);
    assert_eq!(run(&mut Computer::new(vec![2,3,0,3,99])).memory,
               Computer::new(vec![2,3,0,6,99]).memory);
    assert_eq!(run(&mut Computer::new(vec![2,4,4,5,99,0])).memory,
               Computer::new(vec![2,4,4,5,99,9801]).memory);
    assert_eq!(run(&mut Computer::new(vec![1,1,1,4,99,5,6,0,99])).memory,
               Computer::new(vec![30,1,1,4,2,5,6,0,99]).memory);
}
