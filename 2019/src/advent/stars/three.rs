use std::str::FromStr;
use std::fmt;
use std::result::Result;
use std::collections::HashSet;
use std::collections::HashMap;
use crate::advent::input_lines;

#[derive(Default, Debug)]
struct State {
    points: HashSet<Point>,
    costs: HashMap<Point, u32>,
    crosses: HashSet<Point>
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[c {:?}]", self.costs)
    }
}

impl State {
    fn new() -> State {
        State {
            points: HashSet::default(),
            costs: HashMap::default(),
            crosses: HashSet::default()
        }
    }

    fn check_all(&mut self, tpoints: Vec<TPoint>) {
        let points : HashSet<Point> = tpoints.iter().map(|TPoint(p, _)| *p).collect();
        for p in points.intersection(&self.points) {
            self.crosses.insert(p.clone());
        }
        for tp in tpoints {
            let TPoint(point, cost) = tp;
            self.points.insert(point);
            if cost > 0 {
                self.costs
                    .entry(point)
                    .and_modify(|i| *i += cost)
                    .or_insert(cost);
            }
        }
    }

    fn closest_cross(&self) -> i16 {
        self.crosses.iter().map(|c| manhattan_distance0(*c)).min().unwrap_or(0)
    }

    fn cheapest_cross(&self) -> u32 {        
        self.crosses.iter().map(|c| *self.costs.get(c).unwrap()).min().unwrap_or(0)        
    }
}

enum Direction {
    U, D, L, R
}

impl FromStr for Direction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "U" => Ok(Direction::U),
            "D" => Ok(Direction::D),
            "L" => Ok(Direction::L),
            "R" => Ok(Direction::R),
            _ => Err(())
        }
    }
}

struct Move {
    direction: Direction,
    distance: i16,
}

impl Move {
    fn parse(str: String) -> Vec<Move> {
        str.split(",").map(|w| w.parse().unwrap()).collect()
    }
}

impl FromStr for Move {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let d: Direction = s[0..1].parse().unwrap();
        let i: i16 = s[1..].parse().unwrap();

        Ok(Move { direction: d, distance: i})
    }
}

#[derive(PartialEq, PartialOrd, Debug, Hash, Eq, Clone, Copy)]
// x, y
struct Point(i16, i16);

#[derive(PartialEq, PartialOrd, Debug, Hash, Eq, Clone, Copy)]
struct TPoint(Point, u32);

fn origin() -> Point { Point(0, 0) }

fn manhattan_distance0(p: Point) -> i16 {
    p.0.abs() + p.1.abs()
}

#[test]
fn test_manhattan_distance() {
    assert_eq!(manhattan_distance0(Point(5, 5)), 10);
}

fn trace(start: TPoint, mv: Move) -> Vec<TPoint> {
    let TPoint(Point(x, y), t) = start;
    match mv.direction {
        Direction::U =>
            (0..mv.distance).enumerate().map(|(u, i)| TPoint(Point(x, y + i + 1), 1 + t + (u as u32))).collect(),

        Direction::D =>
            (0..mv.distance).enumerate().map(|(u, i)| TPoint(Point(x, y - i - 1), 1 + t + (u as u32))).collect(),

        Direction::L =>
            (0..mv.distance).enumerate().map(|(u, i)| TPoint(Point(x - i - 1, y), 1 + t + (u as u32))).collect(),

        Direction::R =>
            (0..mv.distance).enumerate().map(|(u, i)| TPoint(Point(x + i + 1, y), 1 + t + (u as u32))).collect()
    }
}

fn trace_wire(line: Vec<Move>) -> Vec<TPoint> {
    let mut start = TPoint(origin(), 0);
    let mut all: Vec<TPoint> = vec![];

    for mv in line {
        let mut new = trace(start, mv);
        start = new.last().unwrap().clone();
        all.append(&mut new);
    }
    all
}

#[test]
fn test_trace() {
    assert_eq!(trace(TPoint(origin(), 0), Move { direction: Direction::U, distance: 2}),
               vec![TPoint(Point(0, 1), 1), TPoint(Point(0, 2), 2)]);
}

fn load_wires(wire1: String, wire2: String) -> State {
    let mut state = State::default();
    let wire1: Vec<Move> = Move::parse(wire1);
    let wire2: Vec<Move> = Move::parse(wire2);
    state.check_all(trace_wire(wire1));
    state.check_all(trace_wire(wire2));
    
    state
}

#[test]
fn test_examples() {
    let ex1 = load_wires("R8,U5,L5,D3".to_string(), "U7,R6,D4,L4".to_string());
    assert_eq!(ex1.closest_cross(), 6);
    println!("{}", ex1);
    assert_eq!(ex1.cheapest_cross(), 30);

    let ex2 = load_wires("R75,D30,R83,U83,L12,D49,R71,U7,L72".to_string(),
                         "U62,R66,U55,R34,D71,R55,D58,R83".to_string());
    assert_eq!(ex2.closest_cross(), 159);
    assert_eq!(ex2.cheapest_cross(), 610);

    let ex3 = load_wires("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".to_string(),
                         "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".to_string());
    assert_eq!(ex3.closest_cross(), 135);
    assert_eq!(ex3.cheapest_cross(), 410);
}


pub fn stars() {
    let mut lines = input_lines(3);
    let line1 = lines.next().unwrap();
    let line2 = lines.next().unwrap();

    let state = load_wires(line1.unwrap(), line2.unwrap());
    println!("{}", state.closest_cross());   
    println!("{}", state.cheapest_cross());
}
